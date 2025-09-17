module Core.Contexts.Recv
  ( Recv,
    RecvError (..),
    HandlesResult (..),
    singleResult,
    toHandler,
  )
where

import Core.Errors
import Platform.Prelude
import Pq qualified

data RecvError
  = ResultRecvError
      -- | Offset of the result in the series.
      Int
      -- | Underlying error.
      ResultError
  | NoResultsRecvError
      -- | Details about the error. Possibly empty.
      (Maybe ByteString)
  | TooManyResultsRecvError
      -- | Expected count.
      Int

newtype Recv a
  = Recv (Pq.Connection -> IO (Either RecvError a))
  deriving stock (Functor)

instance Applicative Recv where
  {-# INLINE pure #-}
  pure x = Recv \_ -> pure (Right x)
  {-# INLINE (<*>) #-}
  Recv recv1 <*> Recv recv2 =
    Recv \cs -> do
      ef <- recv1 cs
      eg <- recv2 cs
      pure (ef <*> eg)

class (Functor f) => HandlesResult f where
  handleResult :: f a -> Pq.Result -> IO (Either ResultError a)

toHandler :: Recv a -> Pq.Connection -> IO (Either RecvError a)
toHandler (Recv recv) = recv

singleResult :: (HandlesResult f) => f a -> Recv a
singleResult handler = Recv \connection -> runExceptT do
  result <- ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> do
        errorMessage <- Pq.errorMessage connection
        pure (Left (NoResultsRecvError errorMessage))
      Just result -> pure (Right result)
  result <- ExceptT do
    result <- handleResult handler result
    pure (first (ResultRecvError 0) result)
  ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> pure (Right result)
      Just _ -> pure (Left (TooManyResultsRecvError 1))
  pure result
