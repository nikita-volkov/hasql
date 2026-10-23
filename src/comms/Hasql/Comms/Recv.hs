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
        -- In the pipeline mode the server's last words, e.g. a FATAL with
        -- 57P01, can arrive as an earlier result while the connection still
        -- looks fine, and only a later result finds it bad. Appended to the
        -- details, so that they are not dropped along with the earlier error.
        (Left (ResultError _ _ resultError), Left (ConnectionLostError context details)) ->
          Left (ConnectionLostError context (details <> serverErrorDetails resultError))
        (_, Left e@ConnectionLostError {}) -> Left e
        _ -> ef <*> eg
    where
      -- Text of an error the server reported, in the shape of 'resultErrorDetails'.
      -- 'Nothing' for errors that did not come from the server.
      serverErrorDetails = \case
        ResultDecoder.ServerError code message detail hint _ ->
          Just
            ( mconcat
                [ message <> "\n",
                  foldMap (\text -> "DETAIL:  " <> text <> "\n") detail,
                  foldMap (\text -> "HINT:  " <> text <> "\n") hint,
                  if ByteString.null code then "" else "SQLSTATE: " <> code <> "\n"
                ]
            )
        _ -> Nothing

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
  where
    -- The first non-empty message among the results with its SQLSTATE,
    -- or else the connection's own error message.
    lostConnectionDetails connection results = do
      details <- traverse resultErrorDetails results
      lostConnectionDetailsFrom connection (find (not . ByteString.null) (catMaybes details))

-- | Consume all results from a multi-statement query (e.g., scripts).
-- Each result is decoded using the provided handler.
-- This is useful for scripts that may contain multiple statements,
-- where each statement produces a result that needs to be validated.
-- All results are consumed even if an error occurs, to leave the connection
-- in a clean state.
--
-- The first error is the one reported, as in the '<*>' of 'Recv': it is the
-- cause, and whatever fails after it is a consequence. The server stops a
-- script at its first error, so a later one can only come from libpq itself,
-- e.g. the result it makes up when the connection is lost.
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
                details <- resultErrorDetails result
                loop (resultIndex + 1) (maybeError <|> Just (ResultError context resultIndex err, details))
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

lostConnectionDetailsFrom :: Pq.Connection -> Maybe ByteString -> IO (Maybe ByteString)
lostConnectionDetailsFrom connection resultDetails =
  case resultDetails of
    Just details | not (ByteString.null details) -> pure (Just details)
    _ -> Pq.errorMessage connection

-- | The error message of a result, followed by its SQLSTATE if it has one.
--
-- At the default verbosity neither libpq nor pqi-native puts the SQLSTATE in
-- the message. A lost connection is reported by its text alone, so without
-- this a code the server announced its termination with (@57P01@, @57P02@)
-- would be dropped.
resultErrorDetails :: Pq.Result -> IO (Maybe ByteString)
resultErrorDetails result = do
  maybeMessage <- Pq.resultErrorMessage result
  maybeSqlState <- Pq.resultErrorField result Pq.DiagSqlstate
  pure case (maybeMessage, maybeSqlState) of
    (Just message, Just sqlState)
      | not (ByteString.null message) ->
          Just (terminateLine message <> "SQLSTATE: " <> sqlState <> "\n")
    _ -> maybeMessage
  where
    terminateLine line =
      if ByteString.isSuffixOf "\n" line then line else line <> "\n"

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
      -- The error message of the failing result followed by its SQLSTATE if there is one,
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
