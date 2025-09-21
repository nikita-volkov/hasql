-- | Lower level context focused on just the actual decoding of values. No metadata involved.
module Hipq.ResultRowDecoder
  ( ResultRowDecoder,
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
      -- | Underlying error.
      CellError
  deriving stock (Eq, Show)

data CellError
  = DecodingCellError Text
  | UnexpectedNullCellError
  deriving stock (Eq, Show)

newtype ResultRowDecoder a
  = ResultRowDecoder (StateT Pq.Column (ReaderT Env (ExceptT Error IO)) a)
  deriving
    (Functor, Applicative)
    via (StateT Pq.Column (ReaderT Env (ExceptT Error IO)))

data Env
  = Env
      Pq.Result
      Pq.Row

-- * Functions

{-# INLINE toHandler #-}
toHandler :: ResultRowDecoder a -> Pq.Result -> Pq.Row -> IO (Either Error a)
toHandler (ResultRowDecoder f) result row =
  let env = Env result row
   in runExceptT (runReaderT (evalStateT f 0) env)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: (Maybe a -> Maybe b) -> (ByteString -> Either Text a) -> ResultRowDecoder b
column processNullable valueDec = ResultRowDecoder do
  col <- get
  Env result row <- ask
  let colInt = Pq.colToInt col
  put (succ col)

  valueMaybe <- liftIO ({-# SCC "getvalue'" #-} Pq.getvalue' result row col)

  valueMaybe <- case valueMaybe of
    Nothing -> pure Nothing
    Just v ->
      case {-# SCC "decode" #-} valueDec v of
        Left err -> throwError (CellError colInt (DecodingCellError err))
        Right decoded -> pure (Just decoded)

  case processNullable valueMaybe of
    Nothing -> throwError (CellError colInt UnexpectedNullCellError)
    Just decoded -> pure decoded

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: (ByteString -> Either Text a) -> ResultRowDecoder (Maybe a)
nullableColumn = column Just

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: (ByteString -> Either Text a) -> ResultRowDecoder a
nonNullableColumn = column id
