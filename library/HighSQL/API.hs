module HighSQL.API where

import HighSQL.Prelude hiding (read, Read, write, Write, Error)
import qualified Data.Pool as Pool
import qualified HighSQL.Backend as Backend
import qualified HighSQL.Row as Row
import qualified ListT


-- * Pool
-------------------------

-- |
-- A pool of connections to the database.
newtype Pool b = 
  Pool (Pool.Pool (Backend.Connection b))

-- |
-- Pool initization settings.
data Settings =
  Settings {
    -- | 
    -- The number of stripes (distinct sub-pools) to maintain. 
    -- The smallest acceptable value is 1.
    striping1 :: Word32,
    -- |
    -- The maximum number of connections to keep open per a pool stripe. 
    -- The smallest acceptable value is 1.
    -- Requests for connections will block if this limit is reached 
    -- on a single stripe, 
    -- even if other stripes have idle connections available.
    striping2 :: Word32,
    -- |
    -- The amount of time for which an unused connection is kept open. 
    -- The smallest acceptable value is 0.5 seconds.
    connectionTimeout :: NominalDiffTime
  }

-- |
-- Initialize a pool given a backend and settings 
-- and run an IO computation with it, 
-- while automating the resource management.
withPool :: Backend.Backend b => b -> Settings -> (Pool b -> IO a) -> IO a
withPool b s =
  bracket acquire release
  where
    acquire = 
      do
        pool <-
          Pool.createPool 
            (Backend.connect b) (Backend.disconnect) 
            (striping1 s) (connectionTimeout s) (striping2 s)
        return (Pool pool)
    release (Pool pool) =
      Pool.purgePool pool

-- * Error
-------------------------

-- |
-- The only exception type that this API can raise.
data Error =
  -- |
  -- Cannot connect to a server 
  -- or the connection got interrupted.
  Disconnected Text |
  -- |
  -- Attempt to parse a statement execution result into an incompatible type.
  -- Indicates either a mismatching schema or an incorrect query.
  ResultParsingError ByteString TypeRep
  deriving (Show, Typeable)

instance Exception Error




-- * Transaction
-------------------------

-- |
-- A transaction with a level @l@,
-- running on an anonymous state-thread @s@ 
-- and gaining a result @r@.
newtype Transaction b l s r =
  Transaction (ReaderT (Backend.Connection b) IO r)
  deriving (Functor, Applicative, Monad)

-- |
-- Execute a transaction using a connections pool.
-- Do it in the atomic mode if the first flag is true
-- and in the write mode if the second one is true.
-- 
-- * Automatically retries the transaction in case of a
-- 'Backend.TransactionConflict' exception.
-- 
-- * Rethrows all the other exceptions.
transaction :: (Backend.Backend b) => Bool -> Bool -> Pool b -> (forall s. Transaction b l s r) -> IO r
transaction a w (Pool p) (Transaction t) = 
  do 
    e <- try $ Pool.withResource p $ loop
    case e of
      Left (Backend.Disconnected t) ->
        throwIO (Disconnected t)
      Left (Backend.TransactionConflict) ->
        $bug "Unexpected TransactionConflict"
      Right r ->
        return r
  where
    loop c =
      do
        Backend.beginTransaction a w c
        e <- try $ runReaderT t c
        case e of
          Left Backend.TransactionConflict ->
            do
              Backend.finishTransaction False c
              loop c
          Left e ->
            do
              Backend.finishTransaction False c
              throwIO e
          Right r -> 
            do
              Backend.finishTransaction True c
              return r


-- ** Levels
-------------------------

data Read

-- -- |
-- -- Execute a transaction on a connections pool.
-- -- 
-- -- Requires minimal locking from the database,
-- -- however you can only execute the \"SELECT\" statements in it. 
-- -- The API ensures of that on the type-level.
-- read :: Pool -> (forall s. Transaction Read s r) -> IO r
-- read = transaction False


data Write

-- -- |
-- -- Execute a transaction on a connections pool.
-- -- 
-- -- Allows to execute the \"SELECT\", \"UPDATE\", \"INSERT\" 
-- -- and \"DELETE\" statements.
-- -- However, compared to 'read', this transaction requires the database to choose 
-- -- a more resource-demanding locking strategy.
-- write :: Pool -> (forall s. Transaction Write s r) -> IO r
-- write = transaction True


data Admin

-- -- |
-- -- Execute a transaction on a connections pool.
-- -- 
-- -- Same as 'write', but allows you to perform any kind of statements,
-- -- including \"CREATE\", \"DROP\" and \"ALTER\".
-- admin :: Pool -> (forall s. Transaction Admin s r) -> IO r
-- admin = transaction True


-- ** Privileges
-------------------------

-- | 
-- Produce a results stream from the statement.
select :: 
  forall b l s r. 
  (SelectPrivilege l, Row.Row b r, Backend.Backend b, Typeable r) => 
  Statement b -> ResultsStream s (Transaction b l s) r
select (bs, vl) = 
  do
    (w, s) <- 
      lift $ Transaction $ do
        connection <- ask
        liftIO $ do
          Backend.executeStreaming bs vl Nothing connection
    l <- ResultsStream $ hoist (Transaction . liftIO) $ replicateM w s
    maybe throwParsingError return $ Row.parseResults l
  where
    throwParsingError =
      ResultsStream $ lift $ Transaction $ liftIO $ throwIO $ 
        ResultParsingError bs (typeOf (undefined :: r))

-- |
-- \"SELECT\"
class SelectPrivilege l

instance SelectPrivilege Read
instance SelectPrivilege Write
instance SelectPrivilege Admin


-- |
-- \"UPDATE\", \"INSERT\", \"DELETE\"
class UpdatePrivilege l

instance UpdatePrivilege Write
instance UpdatePrivilege Admin

-- |
-- Execute and count the amount of affected rows.
update :: (UpdatePrivilege l, Backend.Backend b) => Statement b -> Transaction b l s Integer
update (bs, vl) =
  Transaction $ do
    connection <- ask
    liftIO $ do
      Backend.executeCountingEffects bs vl connection

-- |
-- Execute and return the possibly auto-incremented number.
insert :: (UpdatePrivilege l, Backend.Backend b, Backend.Mapping b Integer) => Statement b -> Transaction b l s (Maybe Integer)
insert (bs, vl) =
  Transaction $ do
    connection <- ask
    liftIO $ do
      (w, l) <- Backend.executeStreaming bs vl (Just 1) connection
      case w of
        1 -> do
          traverse (maybe throwParsingError return . Row.parseResults . pure) =<< ListT.head l
        _ -> $bug "Unexpected result"
  where
    throwParsingError =
      liftIO $ throwIO $ ResultParsingError bs (typeOf (undefined :: Integer))


-- |
-- \"CREATE\", \"ALTER\", \"DROP\", \"TRUNCATE\"
class CreatePrivilege l 

instance CreatePrivilege Admin

create :: (CreatePrivilege l, Backend.Backend b) => Statement b -> Transaction b l s ()
create (bs, vl) =
  Transaction $ do
    connection <- ask
    liftIO $ do
      Backend.execute bs vl connection


-- * Statement
-------------------------

type Statement b =
  (ByteString, [Backend.StatementArgument b])

mkStatement :: forall b. ByteString -> [Value b] -> Statement b
mkStatement sql values =
  (,) sql (map renderValue values)
  where
    renderValue (Value v) = 
      Backend.renderValue v :: Backend.StatementArgument b

data Value b =
  forall v. (Backend.Mapping b v) => Value !v


-- * Results Stream
-------------------------

-- |
-- A stream of results, 
-- which fetches only those that you reach.
-- 
-- It is implemented as a wrapper around 'ListT.ListT',
-- hence all the utility functions of the list transformer API 
-- are applicable to this type.
-- 
-- It uses the same trick as 'ST' to become impossible to be 
-- executed outside of its transaction.
-- Hence you can only access it while remaining in a transaction,
-- and, when the transaction finishes,
-- all the acquired resources get automatically released.
newtype ResultsStream s m r =
  ResultsStream (ListT.ListT m r)
  deriving (Functor, Applicative, Alternative, Monad, MonadTrans, MonadPlus, 
            Monoid, ListT.ListMonad)

instance ListT.ListTrans (ResultsStream s) where
  uncons = 
    unsafeCoerce 
      (ListT.uncons :: ListT.ListT m r -> m (Maybe (r, ListT.ListT m r)))
