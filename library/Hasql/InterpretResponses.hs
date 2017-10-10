module Hasql.InterpretResponses where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ParseDataRow as A
import qualified Data.Vector as B


newtype InterpretResponses result =
  InterpretResponses (Response -> IO Response -> (Response -> IO ()) -> IO (Either Error result))

instance Functor InterpretResponses where
  fmap mapping (InterpretResponses io) =
    InterpretResponses (\a b c -> (fmap . fmap) mapping (io a b c))

instance Applicative InterpretResponses where
  pure x =
    InterpretResponses (\_ _ _ -> pure (Right x))
  (<*>) (InterpretResponses leftIO) (InterpretResponses rightIO) =
    InterpretResponses $ \firstResponse fetchResponse discardResponse -> do
      leftEither <- leftIO firstResponse fetchResponse discardResponse
      case leftEither of
        Left error -> return (Left error)
        Right leftResult -> do
          rightFirstResponse <- fetchResponse
          rightEither <- rightIO rightFirstResponse fetchResponse discardResponse
          return (fmap leftResult rightEither)

instance Monad InterpretResponses where
  return = pure
  (>>=) (InterpretResponses leftIO) rightK =
    InterpretResponses $ \firstResponse fetchResponse discardResponse ->
    do
      leftEither <- leftIO firstResponse fetchResponse discardResponse
      case leftEither of
        Left error -> return (Left error)
        Right leftResult -> case rightK leftResult of
          InterpretResponses rightIO -> do
            rightFirstResponse <- fetchResponse
            rightIO rightFirstResponse fetchResponse discardResponse


matchResponse :: (Response -> Maybe (Either Error result)) -> InterpretResponses result
matchResponse match =
  InterpretResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse firstResponse
      where
        processResponse response =
          trace ("InterpretResponses/matchResponse/processResponse: \ESC[1m" <> show response <> "\ESC[0m") $
          case match response of
            Just result -> return result
            Nothing -> case response of
              ErrorResponse status message -> return (Left (BackendError status message))
              _ -> do
                discardResponse response
                nextResponse <- fetchResponse
                processResponse response

foldRows :: FoldM IO row result -> A.ParseDataRow row -> InterpretResponses (result, Int)
foldRows (FoldM foldStep foldStart foldEnd) (A.ParseDataRow rowLength vectorFn) =
  InterpretResponses def
  where
    def firstResponse fetchResponse discardResponse =
      do
        initialState <- foldStart
        processResponse initialState firstResponse
      where
        processResponse !state response =
          trace ("InterpretResponses/foldRows/processResponse: \ESC[1m" <> show response <> "\ESC[0m") $
          case response of
            DataRowResponse values ->
              if B.length values == rowLength
                then case vectorFn values 0 of
                  Left error -> return (Left (DecodingError error))
                  Right row -> do
                    nextState <- foldStep state row
                    nextResponse <- fetchResponse
                    processResponse nextState nextResponse
                else return (Left (DecodingError (fromString
                  (showString "Invalid amount of columns: "
                    (shows (B.length values)
                      (showString ", expecting "
                        (show rowLength)))))))
            CommandCompleteResponse amount ->
              do
                result <- foldEnd state
                return (Right (result, amount))
            ErrorResponse state message ->
              return (Left (BackendError state message))
            EmptyQueryResponse ->
              do
                result <- foldEnd state
                return (Right (result, 0))
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponse state nextResponse

singleRow :: A.ParseDataRow row -> InterpretResponses row
singleRow (A.ParseDataRow rowLength vectorFn) =
  InterpretResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponseWithoutRow firstResponse
      where
        processResponseWithoutRow response =
          trace ("InterpretResponses/singleRow/processResponse: \ESC[1m" <> show response <> "\ESC[0m") $
          case response of
            DataRowResponse values ->
              if B.length values == rowLength
                then case vectorFn values 0 of
                  Left error -> return (Left (DecodingError error))
                  Right row -> do
                    nextResponse <- fetchResponse
                    processResponseWithRow row nextResponse
                else return (Left (DecodingError (fromString
                  (showString "Invalid amount of columns: "
                    (shows (B.length values)
                      (showString ", expecting "
                        (show rowLength)))))))
            CommandCompleteResponse _ ->
              return (Left (DecodingError "Not a single row"))
            ErrorResponse state message ->
              return (Left (BackendError state message))
            EmptyQueryResponse ->
              return (Left (DecodingError "Empty query"))
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponseWithoutRow nextResponse
        processResponseWithRow row response =
          case response of
            DataRowResponse _ ->
              do
                nextResponse <- fetchResponse
                processResponseWithRow row nextResponse
            CommandCompleteResponse _ ->
              return (Right row)
            ErrorResponse state message ->
              return (Left (BackendError state message))
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponseWithRow row nextResponse

rowsAffected :: InterpretResponses Int
rowsAffected =
  InterpretResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse firstResponse
      where
        processResponse =
          \case
            CommandCompleteResponse amount -> return (Right amount)
            DataRowResponse _ -> fetchResponse >>= processResponse
            ErrorResponse state message -> return (Left (BackendError state message))
            EmptyQueryResponse -> return (Right 0)
            otherResponse -> do
              discardResponse otherResponse
              nextResponse <- fetchResponse
              processResponse nextResponse

authenticationStatus :: InterpretResponses AuthenticationStatus
authenticationStatus =
  matchResponse $ \case
    AuthenticationResponse status -> Just (Right status)
    _ -> Nothing

parameterStatus :: (ByteString -> ByteString -> result) -> InterpretResponses result
parameterStatus result =
  matchResponse $ \case
    ParameterStatusResponse name value -> Just (Right (result name value))
    _ -> Nothing

parameters :: InterpretResponses Bool
parameters =
  InterpretResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse (Left (ProtocolError "Missing the \"integer_datetimes\" setting")) firstResponse
      where
        processResponse !state =
          \case
            ParameterStatusResponse name value -> do
              nextResponse <- fetchResponse
              case name of
                "integer_datetimes" -> case value of
                  "on" -> processResponse (Right True) nextResponse
                  "off" -> processResponse (Right False) nextResponse
                  _ -> processResponse (Left (ProtocolError ("Unexpected value of the \"integer_datetimes\" setting: " <> (fromString . show) value))) nextResponse
                _ -> processResponse state nextResponse
            ReadyForQueryResponse _ -> return state
            otherResponse -> do
              discardResponse otherResponse
              nextResponse <- fetchResponse
              processResponse state nextResponse

authenticationResult :: InterpretResponses AuthenticationResult
authenticationResult =
  do
    authenticationStatusResult <- authenticationStatus
    case authenticationStatusResult of
      NeedClearTextPasswordAuthenticationStatus -> return (NeedClearTextPasswordAuthenticationResult)
      NeedMD5PasswordAuthenticationStatus salt -> return (NeedMD5PasswordAuthenticationResult salt)
      OkAuthenticationStatus -> OkAuthenticationResult <$> parameters

parseComplete :: InterpretResponses ()
parseComplete =
  matchResponse $ \case
    ParseCompleteResponse -> Just (Right ())
    _ -> Nothing

bindComplete :: InterpretResponses ()
bindComplete =
  matchResponse $ \case
    BindCompleteResponse -> Just (Right ())
    _ -> Nothing

readyForQuery :: InterpretResponses TransactionStatus
readyForQuery =
  matchResponse $ \case
    ReadyForQueryResponse transactionStatus -> Just (Right transactionStatus)
    _ -> Nothing
