module Hasql.Comms.Recv
  ( Recv,
    singleResult,
    allResults,
    toHandler,
    Error (..),
  )
where

import Data.ByteString qualified as ByteString
import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Platform.Prelude
import Pqi qualified as Pq

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
      pure case (ef, eg) of
        -- A lost connection explains every other error in the same roundtrip:
        -- whatever else failed, it failed because the connection died.
        -- It must not be hidden behind an earlier statement's ordinary error.
        (Left e@ConnectionLostError {}, _) -> Left e
        (_, Left e@ConnectionLostError {}) -> Left e
        _ -> ef <*> eg

instance Bifunctor Recv where
  {-# INLINE bimap #-}
  bimap f g (Recv recv) = Recv (fmap (bimap (fmap f) g) . recv)

toHandler :: Recv context a -> Pq.Connection -> IO (Either (Error context) a)
toHandler (Recv recv) = recv

-- | Exactly one result.
--
-- After the results have been consumed the connection status is checked.
-- If the connection is bad, the outcome is 'ConnectionLostError' regardless
-- of what the results contained. See 'ConnectionLostError' for the reason.
singleResult :: context -> ResultDecoder.ResultDecoder a -> Recv context a
singleResult context handler = Recv \connection -> do
  firstResult <- Pq.getResult connection
  -- Only look for a second result when there was a first one:
  -- in the pipeline mode a 'Nothing' separates the results of consecutive
  -- queries, so asking again would consume the next query's result.
  secondResult <- case firstResult of
    Nothing -> pure Nothing
    Just _ -> Pq.getResult connection
  connectionStatus <- Pq.status connection
  case connectionStatus of
    Pq.ConnectionBad -> do
      details <- lostConnectionDetails connection (catMaybes [firstResult, secondResult])
      pure (Left (ConnectionLostError context details))
    _ -> case (firstResult, secondResult) of
      (Nothing, _) -> do
        errorMessage <- Pq.errorMessage connection
        pure (Left (NoResultsError context errorMessage))
      (Just _, Just _) ->
        pure (Left (TooManyResultsError context 1))
      (Just result, Nothing) -> do
        decoded <- ResultDecoder.toHandler handler result
        pure (first (ResultError context 0) decoded)

-- | Consume all results from a multi-statement query (e.g., scripts).
-- Each result is decoded using the provided handler.
-- This is useful for scripts that may contain multiple statements,
-- where each statement produces a result that needs to be validated.
-- All results are consumed even if an error occurs, to leave the connection
-- in a clean state.
allResults :: context -> ResultDecoder.ResultDecoder a -> Recv context ()
allResults context handler = Recv \connection -> do
  let loop resultIndex maybeError = do
        result <- Pq.getResult connection
        case result of
          Nothing -> pure maybeError
          Just result -> do
            decodedResult <- ResultDecoder.toHandler handler result
            case decodedResult of
              Left err -> do
                -- Continue consuming results even after error to clean up connection
                message <- Pq.resultErrorMessage result
                loop (resultIndex + 1) (maybeError <|> Just (ResultError context resultIndex err, message))
              Right _ ->
                loop (resultIndex + 1) maybeError
  errorOrUnit <- loop 0 Nothing
  connectionStatus <- Pq.status connection
  case connectionStatus of
    Pq.ConnectionBad -> do
      details <- lostConnectionDetailsFrom connection (errorOrUnit >>= snd)
      pure (Left (ConnectionLostError context details))
    _ ->
      pure (maybe (Right ()) (Left . fst) errorOrUnit)

-- | Details for a lost connection: the first non-empty message among the given results,
-- or else the connection's own error message.
lostConnectionDetails :: Pq.Connection -> [Pq.Result] -> IO (Maybe ByteString)
lostConnectionDetails connection results = do
  messages <- traverse Pq.resultErrorMessage results
  lostConnectionDetailsFrom connection (listToMaybe (catMaybes messages))

lostConnectionDetailsFrom :: Pq.Connection -> Maybe ByteString -> IO (Maybe ByteString)
lostConnectionDetailsFrom connection resultMessage =
  case resultMessage of
    Just message | not (ByteString.null message) -> pure (Just message)
    _ -> Pq.errorMessage connection

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
  | -- | The connection was bad by the time the results had been consumed.
    --
    -- Decided by the connection status alone, not by the content of the results.
    -- That is the only reliable signal, because what libpq reports about a dead
    -- connection varies:
    --
    -- * When the socket closes silently, libpq makes up a fatal result with no
    --   SQLSTATE, indistinguishable from a server error by its shape.
    -- * When the server announces its termination (@57P01@, @57P02@), the genuine
    --   fatal result comes first and the connection is only marked bad when the
    --   EOF that follows it is read, i.e. after that result has been returned.
    --
    -- Takes precedence over every other error. See [#341](https://github.com/nikita-volkov/hasql/issues/341).
    ConnectionLostError
      context
      -- | Details about the error.
      -- The error message of the failing result if there is one,
      -- otherwise the error message of the connection.
      (Maybe ByteString)
  deriving stock (Show, Eq, Functor)

instance Comonad Error where
  {-# INLINE extract #-}
  extract = \case
    ResultError context _ _ -> context
    NoResultsError context _ -> context
    TooManyResultsError context _ -> context
    ConnectionLostError context _ -> context

  {-# INLINE duplicate #-}
  duplicate e = case e of
    ResultError _ resultIndex resultError -> ResultError e resultIndex resultError
    NoResultsError _ details -> NoResultsError e details
    TooManyResultsError _ expectedCount -> TooManyResultsError e expectedCount
    ConnectionLostError _ details -> ConnectionLostError e details
