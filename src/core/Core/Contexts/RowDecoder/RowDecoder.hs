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
  = RowDecoder (Env -> IO (Either ResultError a))
  deriving (Functor, Applicative, Monad) via (ReaderT Env (ExceptT ResultError IO))

data Env
  = Env
      Pq.Result
      Pq.Row
      Pq.Column
      Bool
      (IORef Pq.Column)

-- * Functions

{-# INLINE run #-}
run :: RowDecoder a -> Bool -> Pq.Row -> Pq.Column -> Pq.Result -> IO (Either ResultError a)
run (RowDecoder f) integerDatetimes row columnsAmount result = do
  columnRef <- newIORef 0
  f (Env result row columnsAmount integerDatetimes columnRef)

{-# INLINE rowError #-}
rowError :: RowError -> RowDecoder a
rowError x = RowDecoder \(Env _ row _ _ columnRef) -> do
  col <- readIORef columnRef
  pure (Left (RowError (Pq.rowToInt row) (Pq.colToInt (pred col)) x))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: ValueDecoder.ValueDecoder a -> RowDecoder (Maybe a)
value valueDec =
  {-# SCC "value" #-}
  RowDecoder \(Env result row columnsAmount integerDatetimes columnRef) -> do
    col <- readIORef columnRef
    let packRowError err = RowError (Pq.rowToInt row) (Pq.colToInt col) err
    writeIORef columnRef (succ col)
    if col < columnsAmount
      then do
        valueMaybe <- {-# SCC "getvalue'" #-} Pq.getvalue' result row col
        pure case valueMaybe of
          Nothing -> Right Nothing
          Just value ->
            fmap Just
              $ first (packRowError . ValueError)
              $ {-# SCC "decode" #-} A.valueParser (ValueDecoder.run valueDec integerDatetimes) value
      else pure (Left (packRowError EndOfInput))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (rowError UnexpectedNull) pure
