-- | Lower level context focused on just the actual decoding of values. No metadata involved.
module Core.Contexts.RowDecoder.RowDecoder
  ( RowDecoder,
    toHandler,
    nullableColumn,
    nonNullableColumn,
  )
where

import Core.Contexts.ValueDecoder qualified as ValueDecoder
import Core.Errors
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import Pq qualified

newtype RowDecoder a
  = RowDecoder (StateT Pq.Column (ReaderT Env (ExceptT ResultError IO)) a)
  deriving
    (Functor, Applicative)
    via (StateT Pq.Column (ReaderT Env (ExceptT ResultError IO)))

data Env
  = Env
      Pq.Result
      Pq.Row
      Pq.Column

-- * Functions

{-# INLINE toHandler #-}
toHandler :: RowDecoder a -> Pq.Row -> Pq.Column -> Pq.Result -> IO (Either ResultError a)
toHandler (RowDecoder f) row columnsAmount result =
  let env = Env result row columnsAmount
   in runExceptT (runReaderT (evalStateT f 0) env)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: ValueDecoder.ValueDecoder a -> (Maybe a -> Either RowError b) -> RowDecoder b
column valueDec processNullable = RowDecoder do
  col <- get
  Env result row columnsAmount <- ask
  let colInt = Pq.colToInt col
  let packRowError err = RowError (Pq.rowToInt row) colInt err
  put (succ col)

  when (col >= columnsAmount) (throwError (UnexpectedAmountOfColumns (colInt + 1) (Pq.colToInt columnsAmount)))

  valueMaybe <- liftIO ({-# SCC "getvalue'" #-} Pq.getvalue' result row col)

  valueMaybe <- case valueMaybe of
    Nothing -> pure Nothing
    Just v ->
      case {-# SCC "decode" #-} A.valueParser (ValueDecoder.toHandler valueDec) v of
        Left err -> throwError (packRowError (ValueError err))
        Right decoded -> pure (Just decoded)

  case processNullable valueMaybe of
    Left err -> throwError (packRowError err)
    Right decoded -> pure decoded

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder (Maybe a)
nullableColumn valueDec = column valueDec Right

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullableColumn valueDec = column valueDec process
  where
    process Nothing = Left UnexpectedNull
    process (Just v) = Right v
