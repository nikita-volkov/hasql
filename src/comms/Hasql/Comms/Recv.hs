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
import Pqi qualified as Pq

-- | An action that receives and decodes results from a connection.
--
-- The @tag@ type parameter is a value attached at construction,
-- which the error carries if receiving or decoding fails.
newtype Recv tag a
  = Recv (Pq.Connection -> IO (Either (Error tag) a))
  deriving stock (Functor)

instance Applicative (Recv tag) where
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

toHandler :: Recv tag a -> Pq.Connection -> IO (Either (Error tag) a)
toHandler (Recv recv) = recv

-- | Exactly one result.
singleResult :: tag -> ResultDecoder.ResultDecoder a -> Recv tag a
singleResult tag handler = Recv \connection -> runExceptT do
  result <- ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> do
        errorMessage <- Pq.errorMessage connection
        pure (Left (NoResultsError tag errorMessage))
      Just result -> pure (Right result)
  ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> pure (Right result)
      Just _ -> do
        -- Unreachable today: 'singleResult' backs 'queryParams' and
        -- 'queryPrepared', which go through the extended protocol where
        -- Postgres rejects multi-statement SQL outright, and 'Roundtrip.query',
        -- whose only caller is 'Hasql.Comms.Session.runCommand' with "ABORT"
        -- and "DEALLOCATE ALL" - each a single statement. Draining anyway
        -- keeps this branch correct on its own terms rather than relying on
        -- that non-local argument to hold forever.
        drainRemaining connection
        pure (Left (TooManyResultsError tag 1))
  result <- ExceptT do
    result <- ResultDecoder.toHandler handler result
    pure (first (ResultError tag 0) result)
  pure result
  where
    drainRemaining connection =
      Pq.getResult connection >>= \case
        Nothing -> pure ()
        Just _ -> drainRemaining connection

-- | Consume all results from a multi-statement query (e.g., scripts).
-- Each result is decoded using the provided handler.
-- This is useful for scripts that may contain multiple statements,
-- where each statement produces a result that needs to be validated.
-- All results are consumed even if an error occurs, to leave the connection
-- in a clean state.
allResults :: tag -> ResultDecoder.ResultDecoder a -> Recv tag ()
allResults tag handler = Recv \connection -> do
  let loop resultIndex maybeError = do
        result <- Pq.getResult connection
        case result of
          Nothing -> pure maybeError
          Just result -> do
            decodedResult <- ResultDecoder.toHandler handler result
            case decodedResult of
              Left err ->
                -- Continue consuming results even after error to clean up connection
                loop (resultIndex + 1) (Just (ResultError tag resultIndex err))
              Right _ ->
                loop (resultIndex + 1) maybeError
  errorOrUnit <- loop 0 Nothing
  pure (maybe (Right ()) Left errorOrUnit)

-- * Errors

-- | Error of receiving results, carrying the tag of the action that caused it.
data Error tag
  = ResultError
      tag
      -- | Offset of the result in the series.
      Int
      -- | Underlying error.
      ResultDecoder.Error
  | NoResultsError
      tag
      -- | Details about the error. Possibly empty.
      (Maybe ByteString)
  | TooManyResultsError
      tag
      -- | Expected count.
      Int
  deriving stock (Show, Eq, Functor)

instance Comonad Error where
  {-# INLINE extract #-}
  extract = \case
    ResultError tag _ _ -> tag
    NoResultsError tag _ -> tag
    TooManyResultsError tag _ -> tag

  {-# INLINE duplicate #-}
  duplicate e = case e of
    ResultError _ resultIndex resultError -> ResultError e resultIndex resultError
    NoResultsError _ details -> NoResultsError e details
    TooManyResultsError _ expectedCount -> TooManyResultsError e expectedCount
