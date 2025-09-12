-- | Lower level context focused on just the actual decoding of values. No metadata involved.
module Core.Contexts.RowDecoder.RowDecoder
  ( RowDecoder,
    run,
    value,
    nonNullValue,
  )
where

import Core.Contexts.ValueDecoder qualified as ValueDecoder
import Core.Errors
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import Pq qualified

newtype RowDecoder a
  = RowDecoder (StateT Pq.Column (ReaderT Env (ExceptT ResultError IO)) a)
  deriving (Functor, Applicative, Monad) via (StateT Pq.Column (ReaderT Env (ExceptT ResultError IO)))

data Env
  = Env
      Pq.Result
      Pq.Row
      Pq.Column
      Bool

-- * Functions

{-# INLINE run #-}
run :: RowDecoder a -> Bool -> Pq.Row -> Pq.Column -> Pq.Result -> IO (Either ResultError a)
run (RowDecoder f) integerDatetimes row columnsAmount result =
  let env = Env result row columnsAmount integerDatetimes
   in runExceptT (runReaderT (evalStateT f 0) env)

{-# INLINE rowError #-}
rowError :: RowError -> RowDecoder a
rowError x = RowDecoder do
  col <- get
  Env _ row _ _ <- ask
  throwError (RowError (Pq.rowToInt row) (Pq.colToInt (pred col)) x)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: ValueDecoder.ValueDecoder a -> RowDecoder (Maybe a)
value valueDec = RowDecoder do
  col <- get
  Env result row columnsAmount integerDatetimes <- ask
  let packRowError err = RowError (Pq.rowToInt row) (Pq.colToInt col) err
  put (succ col)
  if col < columnsAmount
    then do
      valueMaybe <- liftIO ({-# SCC "getvalue'" #-} Pq.getvalue' result row col)
      case valueMaybe of
        Nothing -> pure Nothing
        Just v -> do
          let decoded = first (packRowError . ValueError) ({-# SCC "decode" #-} A.valueParser (ValueDecoder.run valueDec integerDatetimes) v)
          either throwError (pure . Just) decoded
    else throwError (packRowError EndOfInput)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (rowError UnexpectedNull) pure
