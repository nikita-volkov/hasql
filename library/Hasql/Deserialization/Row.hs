module Hasql.Deserialization.Row where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Hasql.Deserialization.Value as Value


newtype Row a =
  Row
    (EitherT Error 
      (ReaderT 
        (LibPQ.Result, LibPQ.Row, LibPQ.Column, Bool) 
        (StateT LibPQ.Column IO)) 
      a)
  deriving (Functor, Applicative, Monad)

data Error =
  EndOfInput |
  UnexpectedNull |
  ValueError !Text
  deriving (Show)


{-# INLINE run #-}
run :: Row a -> (LibPQ.Result, LibPQ.Row, LibPQ.Column, Bool) -> IO (Either Error a)
run (Row m) env =
  flip evalStateT 0 (flip runReaderT env (runEitherT m))


{-# INLINE error #-}
error :: Error -> Row a
error x =
  Row (EitherT (return (Left x)))

-- |
-- Next value, decoded using the provided value deserializer.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDes =
  {-# SCC "value" #-} 
  Row $ EitherT $ ReaderT $ \(result, row, maxCol, integerDatetimes) -> StateT $ \col ->
    if col < maxCol
      then
        flip fmap (LibPQ.getvalue result row col) $ \x ->
          (traverse (mapLeft ValueError . Decoder.run (Value.run valueDes integerDatetimes)) x,
           succ col)
      else return (Left EndOfInput, col)

-- |
-- Next value, decoded using the provided value deserializer.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDes =
  {-# SCC "nonNullValue" #-} 
  value valueDes >>= maybe (error UnexpectedNull) pure
