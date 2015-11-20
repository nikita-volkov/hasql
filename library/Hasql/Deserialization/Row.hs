module Hasql.Deserialization.Row where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Hasql.Deserialization.Value as Value


newtype Row a =
  Row (ReaderT Env (EitherT Error IO) a)
  deriving (Functor, Applicative, Monad)

data Env =
  Env !LibPQ.Result !LibPQ.Row !LibPQ.Column !Bool !(IORef LibPQ.Column)

data Error =
  EndOfInput |
  UnexpectedNull |
  ValueError !Text
  deriving (Show)


-- * Functions
-------------------------

{-# INLINE run #-}
run :: Row a -> (LibPQ.Result, LibPQ.Row, LibPQ.Column, Bool) -> IO (Either Error a)
run (Row impl) (result, row, columnsAmount, integerDatetimes) =
  do
    columnRef <- newIORef 0
    runEitherT (runReaderT impl (Env result row columnsAmount integerDatetimes columnRef))

{-# INLINE error #-}
error :: Error -> Row a
error x =
  Row (ReaderT (const (EitherT (pure (Left x)))))

-- |
-- Next value, decoded using the provided value deserializer.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDes =
  {-# SCC "value" #-} 
  Row $ ReaderT $ \(Env result row columnsAmount integerDatetimes columnRef) -> EitherT $ do
    col <- readIORef columnRef
    writeIORef columnRef (succ col)
    if col < columnsAmount
      then do
        valueMaybe <- LibPQ.getvalue result row col
        pure $ 
          case valueMaybe of
            Nothing -> Right Nothing
            Just value -> fmap Just $ mapLeft ValueError $ Decoder.run (Value.run valueDes integerDatetimes) value
      else pure (Left EndOfInput)

-- |
-- Next value, decoded using the provided value deserializer.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDes =
  {-# SCC "nonNullValue" #-}
  Row $ ReaderT $ \(Env result row columnsAmount integerDatetimes columnRef) -> EitherT $ do
    col <- readIORef columnRef
    writeIORef columnRef (succ col)
    if col < columnsAmount
      then do
        valueMaybe <- LibPQ.getvalue result row col
        pure $ 
          case valueMaybe of
            Nothing -> Left UnexpectedNull
            Just value -> mapLeft ValueError $ Decoder.run (Value.run valueDes integerDatetimes) value
      else pure (Left EndOfInput)
