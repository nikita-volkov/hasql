module Hasql.Private.Decoders.Row where

import Hasql.Private.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Hasql.Private.Decoders.Value as Value


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
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDec =
  {-# SCC "value" #-} 
  Row $ ReaderT $ \(Env result row columnsAmount integerDatetimes columnRef) -> EitherT $ do
    col <- readIORef columnRef
    writeIORef columnRef (succ col)
    if col < columnsAmount
      then do
        valueMaybe <- {-# SCC "getvalue'" #-} LibPQ.getvalue' result row col
        pure $ 
          case valueMaybe of
            Nothing ->
              Right Nothing
            Just value ->
              fmap Just $ mapLeft ValueError $
              {-# SCC "decode" #-} Decoder.run (Value.run valueDec integerDatetimes) value
      else pure (Left EndOfInput)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-} 
  value valueDec >>= maybe (error UnexpectedNull) pure
