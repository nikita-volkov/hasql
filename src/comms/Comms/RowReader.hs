-- | Lower level context focused on just the actual decoding of values. No metadata involved.
module Comms.RowReader
  ( RowReader,
    nullableColumn,
    nonNullableColumn,

    -- * Errors
    Error (..),
    CellError (..),

    -- * Relations
    toHandler,
  )
where

import Platform.Prelude
import Pq qualified

data Error
  = CellError
      -- | Column index, 0-based.
      Int
      -- | OID of the column type as reported by Postgres.
      Pq.Oid
      -- | Underlying error.
      CellError
  deriving stock (Eq, Show)

data CellError
  = DecodingCellError Text
  | UnexpectedNullCellError
  deriving stock (Eq, Show)

newtype RowReader a
  = RowReader (StateT Pq.Column (ReaderT Env (ExceptT Error IO)) a)
  deriving
    (Functor, Applicative)
    via (StateT Pq.Column (ReaderT Env (ExceptT Error IO)))

data Env
  = Env
      Pq.Result
      Pq.Row

-- * Functions

{-# INLINE toHandler #-}
toHandler :: RowReader a -> Pq.Result -> Pq.Row -> IO (Either Error a)
toHandler (RowReader f) result row =
  let env = Env result row
   in runExceptT (runReaderT (evalStateT f 0) env)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: (Maybe a -> Maybe b) -> (ByteString -> Either Text a) -> RowReader b
column processNullable valueDec = RowReader do
  col <- get
  Env result row <- ask
  let colInt = Pq.colToInt col
  put (succ col)

  valueMaybe <- liftIO ({-# SCC "getvalue'" #-} Pq.getvalue' result row col)

  valueMaybe <- case valueMaybe of
    Nothing -> pure Nothing
    Just v -> do
      oid <- liftIO (Pq.ftype result col)
      case {-# SCC "decode" #-} valueDec v of
        Left err -> throwError (CellError colInt oid (DecodingCellError err))
        Right decoded -> pure (Just decoded)

  case processNullable valueMaybe of
    Nothing -> do
      oid <- liftIO (Pq.ftype result col)
      throwError (CellError colInt oid UnexpectedNullCellError)
    Just decoded -> pure decoded

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: (ByteString -> Either Text a) -> RowReader (Maybe a)
nullableColumn = column Just

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: (ByteString -> Either Text a) -> RowReader a
nonNullableColumn = column id
