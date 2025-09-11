module Hasql.Decoders.Row
  ( Row,
    run,
    error,
    value,
    nonNullValue,
  )
where

import Hasql.Decoders.Value qualified as Value
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (error)
import PostgreSQL.Binary.Decoding qualified as A

newtype Row a = Row (Env -> IO (Either RowError a))
  deriving (Functor, Applicative, Monad) via (ReaderT Env (ExceptT RowError IO))

instance MonadFail Row where
  fail = error . ValueError . fromString

data Env
  = Env Pq.Result Pq.Row Pq.Column Bool (IORef Pq.Column)

-- * Functions

{-# INLINE run #-}
run :: Row a -> Pq.Result -> Pq.Row -> Pq.Column -> Bool -> IO (Either (Int, RowError) a)
run (Row f) result row columnsAmount integerDatetimes = do
  columnRef <- newIORef 0
  let env = Env result row columnsAmount integerDatetimes columnRef
  f env >>= \case
    Left e -> do
      Pq.Col col <- readIORef columnRef
      -- -1 because succ is applied before the error is returned
      pure $ Left (fromIntegral col - 1, e)
    Right x -> pure $ Right x

{-# INLINE error #-}
error :: RowError -> Row a
error x = Row (const (pure (Left x)))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDec =
  {-# SCC "value" #-}
  Row \(Env result row columnsAmount integerDatetimes columnRef) ->
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
                $ {-# SCC "decode" #-} A.valueParser (Value.run valueDec integerDatetimes) value
        else pure (Left EndOfInput)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (error UnexpectedNull) pure
