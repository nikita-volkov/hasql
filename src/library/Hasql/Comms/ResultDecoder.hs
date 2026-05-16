module Hasql.Comms.ResultDecoder
  ( ResultDecoder,

    -- * Relations
    Handler,
    toHandler,
    fromHandler,

    -- * Extractors
    columnOids,

    -- * Constructors

    -- ** Basic
    ok,
    pipelineSync,
    rowsAffected,
    checkExecStatus,

    -- ** Higher-level decoders
    maybe,
    single,
    vector,
    foldl,
    foldr,

    -- ** Refinement
    refine,

    -- * Errors
    Error (..),
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Hasql.Comms.RowDecoder qualified as RowDecoder
import Hasql.Driver.Interface qualified as Interface
import Hasql.Platform.Prelude hiding (foldl, foldr, maybe)
import Hasql.Platform.Prelude qualified as Prelude

-- | Result consumption context, for consuming a single result from a sequence of results returned by the server.
newtype ResultDecoder a
  = ResultDecoder
      { runResultDecoder ::
          forall result.
          Interface.ResultDriver result ->
          result ->
          IO (Either Error a)
      }

-- * Instances

instance Functor ResultDecoder where
  {-# INLINE fmap #-}
  fmap f (ResultDecoder g) = ResultDecoder \rd result ->
    fmap (fmap f) (g rd result)

instance Applicative ResultDecoder where
  {-# INLINE pure #-}
  pure a = ResultDecoder \_ _ -> pure (Right a)
  {-# INLINE (<*>) #-}
  ResultDecoder gf <*> ResultDecoder ga = ResultDecoder \rd result -> do
    ef <- gf rd result
    ea <- ga rd result
    pure (ef <*> ea)

instance Monad ResultDecoder where
  {-# INLINE (>>=) #-}
  ResultDecoder ga >>= f = ResultDecoder \rd result -> do
    ea <- ga rd result
    case ea of
      Left err -> pure (Left err)
      Right a -> runResultDecoder (f a) rd result

instance MonadError Error ResultDecoder where
  {-# INLINE throwError #-}
  throwError e = ResultDecoder \_ _ -> pure (Left e)
  {-# INLINE catchError #-}
  catchError (ResultDecoder g) h = ResultDecoder \rd result -> do
    e <- g rd result
    case e of
      Left err -> runResultDecoder (h err) rd result
      Right a -> pure (Right a)

instance Filterable ResultDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

-- * Relations

-- ** Handler

type Handler a = forall result. Interface.ResultDriver result -> result -> IO (Either Error a)

toHandler :: ResultDecoder a -> Interface.ResultDriver result -> result -> IO (Either Error a)
toHandler (ResultDecoder handler) = handler

fromHandler :: (forall result. Interface.ResultDriver result -> result -> IO (Either Error a)) -> ResultDecoder a
fromHandler = ResultDecoder

-- * Construction

{-# INLINE ok #-}
ok :: ResultDecoder ()
ok = checkExecStatus [Interface.CommandOk, Interface.TuplesOk]

{-# INLINE pipelineSync #-}
pipelineSync :: ResultDecoder ()
pipelineSync = checkExecStatus [Interface.PipelineSync]

{-# INLINE rowsAffected #-}
rowsAffected :: ResultDecoder Int64
rowsAffected = do
  checkExecStatus [Interface.CommandOk]
  ResultDecoder \rd result -> do
    cmdTuplesReader <$> Interface.rdCmdTuples rd result
  where
    cmdTuplesReader =
      notNothing >=> notEmpty >=> decimal
      where
        notNothing =
          Prelude.maybe (Left (UnexpectedResult "No bytes")) Right
        notEmpty bytes =
          if ByteString.null bytes
            then Left (UnexpectedResult "Empty bytes")
            else Right bytes
        decimal bytes =
          first
            ( \m ->
                UnexpectedResult
                  ("Decimal parsing failure: " <> fromString m)
            )
            ( Attoparsec.parseOnly
                (Attoparsec.decimal <* Attoparsec.endOfInput)
                bytes
            )

{-# INLINE checkExecStatus #-}
checkExecStatus :: [Interface.ExecStatus] -> ResultDecoder ()
checkExecStatus expectedList = do
  status <- ResultDecoder \rd result -> Right <$> Interface.rdResultStatus rd result
  unless (elem status expectedList) $ do
    case status of
      Interface.BadResponse -> serverError
      Interface.NonfatalError -> serverError
      Interface.FatalError -> serverError
      Interface.EmptyQuery -> return ()
      _ ->
        throwError
          ( UnexpectedResult
              ("Unexpected result status: " <> fromString (show status) <> ". Expecting one of the following: " <> fromString (show expectedList))
          )

{-# INLINE serverError #-}
serverError :: ResultDecoder ()
serverError =
  ResultDecoder \rd result -> do
    code <-
      fold <$> Interface.rdResultErrorField rd result Interface.DiagSqlstate
    message <-
      fold <$> Interface.rdResultErrorField rd result Interface.DiagMessagePrimary
    detail <-
      Interface.rdResultErrorField rd result Interface.DiagMessageDetail
    hint <-
      Interface.rdResultErrorField rd result Interface.DiagMessageHint
    position <-
      parsePosition <$> Interface.rdResultErrorField rd result Interface.DiagStatementPosition
    pure $ Left $ ServerError code message detail hint position
  where
    parsePosition = \case
      Nothing -> Nothing
      Just pos ->
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) pos of
          Right pos -> Just pos
          _ -> Nothing

-- | Get the OIDs of all columns in the current result.
{-# INLINE columnOids #-}
columnOids :: ResultDecoder [Word32]
columnOids = ResultDecoder \rd result -> do
  count <- Interface.rdNfields rd result
  oids <- forM [0 .. count - 1] $ \colIndex ->
    Interface.rdFtype rd result colIndex
  pure (Right oids)

-- * Higher-level decoders

{-# INLINE checkCompatibility #-}
checkCompatibility :: RowDecoder.RowDecoder a -> ResultDecoder ()
checkCompatibility rowDec =
  let oids = RowDecoder.toExpectedOids rowDec
   in ResultDecoder \rd result -> do
        maxCols <- Interface.rdNfields rd result
        if length oids == maxCols
          then
            let go [] _ = pure (Right ())
                go (Nothing : rest) colIndex = go rest (succ colIndex)
                go (Just expectedOid : rest) colIndex = do
                  actualOid <- Interface.rdFtype rd result colIndex
                  if actualOid == expectedOid
                    then go rest (succ colIndex)
                    else
                      pure
                        ( Left
                            ( DecoderTypeMismatch
                                colIndex
                                expectedOid
                                actualOid
                            )
                        )
             in go oids 0
          else pure (Left (UnexpectedColumnCount (length oids) maxCols))

{-# INLINE maybe #-}
maybe :: RowDecoder.RowDecoder a -> ResultDecoder (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Interface.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \rd result -> do
        maxRows <- Interface.rdNtuples rd result
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            r <-
              RowDecoder.toDecoder rowDec rd result 0
                <&> first (RowError 0)
            pure (fmap Just r)
          _ -> return (Left (UnexpectedRowCount maxRows))

{-# INLINE single #-}
single :: RowDecoder.RowDecoder a -> ResultDecoder a
single rowDec =
  do
    checkExecStatus [Interface.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \rd result -> do
        maxRows <- Interface.rdNtuples rd result
        case maxRows of
          1 -> do
            RowDecoder.toDecoder rowDec rd result 0
              <&> first (RowError 0)
          _ -> return (Left (UnexpectedRowCount maxRows))

{-# INLINE vector #-}
vector :: RowDecoder.RowDecoder a -> ResultDecoder (Vector a)
vector rowDec =
  do
    checkExecStatus [Interface.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \rd result -> do
        maxRows <- Interface.rdNtuples rd result
        mvector <- MutableVector.unsafeNew maxRows
        failureRef <- newIORef Nothing
        forMFromZero_ maxRows $ \rowIndex -> do
          rowResult <- RowDecoder.toDecoder rowDec rd result rowIndex
          case rowResult of
            Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
            Right !x -> MutableVector.unsafeWrite mvector rowIndex x
        readIORef failureRef >>= \case
          Nothing -> Right <$> Vector.unsafeFreeze mvector
          Just x -> pure (Left x)

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Interface.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \rd result ->
        {-# SCC "traversal" #-}
        do
          maxRows <- Interface.rdNtuples rd result
          accRef <- newIORef init
          failureRef <- newIORef Nothing
          forMFromZero_ maxRows $ \rowIndex -> do
            rowResult <- RowDecoder.toDecoder rowDec rd result rowIndex
            case rowResult of
              Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
              Right !x -> modifyIORef' accRef (\acc -> step acc x)
          readIORef failureRef >>= \case
            Nothing -> Right <$> readIORef accRef
            Just x -> pure (Left x)

{-# INLINE foldr #-}
foldr :: (b -> a -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Interface.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \rd result -> do
        maxRows <- Interface.rdNtuples rd result
        accRef <- newIORef init
        failureRef <- newIORef Nothing
        forMToZero_ maxRows $ \rowIndex -> do
          rowResult <- RowDecoder.toDecoder rowDec rd result rowIndex
          case rowResult of
            Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
            Right !x -> modifyIORef accRef (\acc -> step x acc)
        readIORef failureRef >>= \case
          Nothing -> Right <$> readIORef accRef
          Just x -> pure (Left x)

-- * Refinement

refine :: (a -> Either Text b) -> ResultDecoder a -> ResultDecoder b
refine refiner (ResultDecoder reader) = ResultDecoder
  $ \rd result -> do
    resultEither <- reader rd result
    return $ resultEither >>= first UnexpectedResult . refiner

-- * Errors

-- |
-- An error with a command result.
data Error
  = -- | An error reported by the DB.
    ServerError
      -- | __Code__. The SQLSTATE code for the error. It's recommended to use
      -- <http://hackage.haskell.org/package/postgresql-error-codes
      -- the "postgresql-error-codes" package> to work with those.
      ByteString
      -- | __Message__. The primary human-readable error message(typically one
      -- line). Always present.
      ByteString
      -- | __Details__. An optional secondary error message carrying more
      -- detail about the problem. Might run to multiple lines.
      (Maybe ByteString)
      -- | __Hint__. An optional suggestion on what to do about the problem.
      -- This is intended to differ from detail in that it offers advice
      -- (potentially inappropriate) rather than hard facts. Might run to
      -- multiple lines.
      (Maybe ByteString)
      -- | __Position__. Error cursor position as an index into the original
      -- statement string. Positions are measured in characters not bytes.
      (Maybe Int)
  | -- |
    -- The database returned an unexpected result.
    -- Indicates an improper statement or a schema mismatch.
    UnexpectedResult Text
  | -- |
    -- An unexpected amount of rows.
    UnexpectedRowCount Int
  | -- |
    -- An unexpected amount of columns in the result.
    UnexpectedColumnCount
      -- | Expected amount of columns.
      Int
      -- | Actual amount of columns.
      Int
  | -- |
    -- Appears when the decoder's expected type doesn't match the actual column type.
    -- Reports the expected OID and the actual OID from the result.
    DecoderTypeMismatch
      -- | Column index.
      Int
      -- | Expected OID.
      Word32
      -- | Actual OID.
      Word32
  | -- | An error in a specific row, reported by a row decoder.
    RowError
      -- | Row index.
      Int
      -- | Underlying error.
      RowDecoder.Error
  deriving (Show, Eq)
