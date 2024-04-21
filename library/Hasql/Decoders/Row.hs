module Hasql.Decoders.Row where

import Database.PostgreSQL.LibPQ qualified as LibPQ
import Hasql.Decoders.Value qualified as Value
import Hasql.Errors
import Hasql.Prelude hiding (error)
import PostgreSQL.Binary.Decoding qualified as A

newtype Row a
  = Row (ReaderT Env (ExceptT ColumnError IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadFail Row where
  fail = error . ValueColumnError . fromString

data Env
  = Env !LibPQ.Result !LibPQ.Row !LibPQ.Column !Bool !(IORef LibPQ.Column)

-- * Functions

{-# INLINE run #-}
run :: Row a -> (LibPQ.Result, LibPQ.Row, LibPQ.Column, Bool) -> IO (Either RowError a)
run (Row impl) (result, row, columnsAmount, integerDatetimes) =
  do
    columnRef <- newIORef 0
    runExceptT (runReaderT impl (Env result row columnsAmount integerDatetimes columnRef)) >>= \case
      Left e -> do
        LibPQ.Col col <- readIORef columnRef
        -- -1 because succ is applied before the error is returned
        pure $ Left (ColumnRowError (fromIntegral col - 1) e)
      Right x -> pure $ Right x

{-# INLINE error #-}
error :: ColumnError -> Row a
error x =
  Row (ReaderT (const (ExceptT (pure (Left x)))))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDec =
  {-# SCC "value" #-}
  Row
    $ ReaderT
    $ \(Env result row columnsAmount integerDatetimes columnRef) -> ExceptT $ do
      col <- readIORef columnRef
      writeIORef columnRef (succ col)
      if col < columnsAmount
        then do
          valueMaybe <- {-# SCC "getvalue'" #-} LibPQ.getvalue' result row col
          pure
            $ case valueMaybe of
              Nothing ->
                Right Nothing
              Just value ->
                fmap Just
                  $ mapLeft ValueColumnError
                  $ {-# SCC "decode" #-} A.valueParser (Value.run valueDec integerDatetimes) value
        else pure (Left EndOfInputColumnError)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (error UnexpectedNullColumnError) pure
