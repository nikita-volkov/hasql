module Hasql.Contexts.RowDecoder
  ( RowDecoder,
    run,
    error,
    value,
    nonNullValue,
  )
where

import Hasql.Contexts.ValueDecoder qualified as ValueDecoder
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (error)
import PostgreSQL.Binary.Decoding qualified as A

newtype RowDecoder a = RowDecoder (Env -> IO (Either RowError a))
  deriving (Functor, Applicative, Monad) via (ReaderT Env (ExceptT RowError IO))

instance MonadFail RowDecoder where
  fail = error . ValueError . fromString

data Env
  = Env Pq.Result Pq.Row Pq.Column Bool (IORef Pq.Column)

-- * Functions

{-# INLINE run #-}
run :: RowDecoder a -> Pq.Result -> Pq.Row -> Pq.Column -> Bool -> IO (Either (Int, RowError) a)
run (RowDecoder f) result row columnsAmount integerDatetimes = do
  columnRef <- newIORef 0
  let env = Env result row columnsAmount integerDatetimes columnRef
  f env >>= \case
    Left e -> do
      Pq.Col col <- readIORef columnRef
      -- -1 because succ is applied before the error is returned
      pure $ Left (fromIntegral col - 1, e)
    Right x -> pure $ Right x

{-# INLINE error #-}
error :: RowError -> RowDecoder a
error x = RowDecoder (const (pure (Left x)))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: ValueDecoder.ValueDecoder a -> RowDecoder (Maybe a)
value valueDec =
  {-# SCC "value" #-}
  RowDecoder \(Env result row columnsAmount integerDatetimes columnRef) ->
    do
      col <- readIORef columnRef
      writeIORef columnRef (succ col)
      if col < columnsAmount
        then do
          valueMaybe <- {-# SCC "getvalue'" #-} Pq.getvalue' result row col
          pure case valueMaybe of
            Nothing -> Right Nothing
            Just value ->
              fmap Just
                $ first ValueError
                $ {-# SCC "decode" #-} A.valueParser (ValueDecoder.run valueDec integerDatetimes) value
        else pure (Left EndOfInput)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (error UnexpectedNull) pure
