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

newtype Recv a
  = Recv (Pq.Connection -> IO (Either Error a))
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

toHandler :: Recv a -> Pq.Connection -> IO (Either Error a)
toHandler (Recv recv) = recv

-- | Exactly one result.
singleResult :: ResultDecoder.ResultDecoder a -> Recv a
singleResult handler = Recv \connection -> runExceptT do
  result <- ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> do
        errorMessage <- Pq.errorMessage connection
        pure (Left (NoResultsError errorMessage))
      Just result -> pure (Right result)
  result <- ExceptT do
    result <- ResultDecoder.toHandler handler result
    pure (first (ResultError 0) result)
  ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> pure (Right result)
      Just _ -> pure (Left (TooManyResultsError 1))
  pure result

-- * Errors

data Error
  = ResultError
      -- | Offset of the result in the series.
      Int
      -- | Underlying error.
      ResultDecoder.Error
  | NoResultsError
      -- | Details about the error. Possibly empty.
      (Maybe ByteString)
  | TooManyResultsError
      -- | Expected count.
      Int
  deriving stock (Show, Eq)
