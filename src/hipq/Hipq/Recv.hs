module Hipq.Recv
  ( Recv,
    singleResult,
    toHandler,
    Error (..),
  )
where

import Hipq.ResultDecoder qualified as ResultDecoder
import Platform.Prelude
import Pq qualified

newtype Recv context a
  = Recv (Pq.Connection -> IO (Either (Error context) a))
  deriving stock (Functor)

instance Applicative (Recv context) where
  {-# INLINE pure #-}
  pure x = Recv \_ -> pure (Right x)
  {-# INLINE (<*>) #-}
  Recv recv1 <*> Recv recv2 =
    Recv \cs -> do
      ef <- recv1 cs
      eg <- recv2 cs
      pure (ef <*> eg)

toHandler :: Recv context a -> Pq.Connection -> IO (Either (Error context) a)
toHandler (Recv recv) = recv

-- | Exactly one result.
singleResult :: context -> ResultDecoder.ResultDecoder a -> Recv context a
singleResult context handler = Recv \connection -> runExceptT do
  result <- ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> do
        errorMessage <- Pq.errorMessage connection
        pure (Left (NoResultsError context errorMessage))
      Just result -> pure (Right result)
  ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> pure (Right result)
      Just _ -> pure (Left (TooManyResultsError context 1))
  result <- ExceptT do
    result <- ResultDecoder.toHandler handler result
    pure (first (ResultError context 0) result)
  pure result

-- * Errors

data Error context
  = ResultError
      context
      -- | Offset of the result in the series.
      Int
      -- | Underlying error.
      ResultDecoder.Error
  | NoResultsError
      context
      -- | Details about the error. Possibly empty.
      (Maybe ByteString)
  | TooManyResultsError
      context
      -- | Expected count.
      Int
  deriving stock (Show, Eq)
