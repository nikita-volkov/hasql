module Hasql.Comms.Recv
  ( Recv,
    singleResult,
    allResults,
    toHandler,
    Error (..),
  )
where

import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Platform.Prelude
import Pqi qualified

newtype Recv context a
  = Recv (forall conn. (Pqi.IsConnection conn) => conn -> IO (Either (Error context) a))

instance Functor (Recv context) where
  fmap f (Recv g) = Recv $ \conn -> fmap (fmap f) (g conn)

instance Applicative (Recv context) where
  {-# INLINE pure #-}
  pure x = Recv \_ -> pure (Right x)
  {-# INLINE (<*>) #-}
  Recv recv1 <*> Recv recv2 =
    Recv \cs -> do
      ef <- recv1 cs
      eg <- recv2 cs
      pure (ef <*> eg)

instance Bifunctor Recv where
  {-# INLINE bimap #-}
  bimap f g (Recv recv) = Recv (fmap (bimap (fmap f) g) . recv)

toHandler :: (Pqi.IsConnection conn) => Recv context a -> conn -> IO (Either (Error context) a)
toHandler (Recv recv) = recv

-- | Exactly one result.
singleResult :: context -> ResultDecoder.ResultDecoder a -> Recv context a
singleResult context handler = Recv \connection -> runExceptT do
  result <- ExceptT do
    mResult <- Pqi.getResult connection
    case mResult of
      Nothing -> do
        errorMessage <- Pqi.errorMessage connection
        pure (Left (NoResultsError context errorMessage))
      Just result -> pure (Right result)
  ExceptT do
    mExtra <- Pqi.getResult connection
    case mExtra of
      Nothing -> pure (Right ())
      Just _ -> pure (Left (TooManyResultsError context 1))
  decResult <- ExceptT do
    decoded <- ResultDecoder.toHandler handler result
    pure (first (ResultError context 0) decoded)
  pure decResult

-- | Consume all results from a multi-statement query (e.g., scripts).
-- Each result is decoded using the provided handler.
-- This is useful for scripts that may contain multiple statements,
-- where each statement produces a result that needs to be validated.
-- All results are consumed even if an error occurs, to leave the connection
-- in a clean state.
allResults :: context -> ResultDecoder.ResultDecoder a -> Recv context ()
allResults context handler = Recv \connection -> do
  let loop resultIndex maybeError = do
        mResult <- Pqi.getResult connection
        case mResult of
          Nothing -> pure maybeError
          Just result -> do
            decodedResult <- ResultDecoder.toHandler handler result
            case decodedResult of
              Left err ->
                -- Continue consuming results even after error to clean up connection
                loop (resultIndex + 1) (Just (ResultError context resultIndex err))
              Right _ ->
                loop (resultIndex + 1) maybeError
  errorOrUnit <- loop 0 Nothing
  pure (maybe (Right ()) Left errorOrUnit)

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
  deriving stock (Show, Eq, Functor)

instance Comonad Error where
  {-# INLINE extract #-}
  extract = \case
    ResultError context _ _ -> context
    NoResultsError context _ -> context
    TooManyResultsError context _ -> context

  {-# INLINE duplicate #-}
  duplicate e = case e of
    ResultError _ resultIndex resultError -> ResultError e resultIndex resultError
    NoResultsError _ details -> NoResultsError e details
    TooManyResultsError _ expectedCount -> TooManyResultsError e expectedCount
