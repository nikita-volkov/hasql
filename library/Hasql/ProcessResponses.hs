module Hasql.ProcessResponses where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ParseDataRow as A
import qualified Data.Vector as B


newtype ProcessResponses result =
  ProcessResponses (Response -> IO Response -> (Response -> IO ()) -> IO result)

instance Functor ProcessResponses where
  fmap mapping (ProcessResponses io) =
    ProcessResponses (\a b c -> fmap mapping (io a b c))

instance Applicative ProcessResponses where
  pure x =
    ProcessResponses (\_ _ _ -> pure x)
  (<*>) (ProcessResponses leftIO) (ProcessResponses rightIO) =
    ProcessResponses $ \firstResponse fetchResponse discardResponse ->
    leftIO firstResponse fetchResponse discardResponse <*> rightIO firstResponse fetchResponse discardResponse

instance Monad ProcessResponses where
  return = pure
  (>>=) (ProcessResponses leftIO) rightK =
    ProcessResponses $ \firstResponse fetchResponse discardResponse ->
    do
      leftResult <- leftIO firstResponse fetchResponse discardResponse
      case rightK leftResult of
        ProcessResponses rightIO -> rightIO firstResponse fetchResponse discardResponse


matchResponse :: (Response -> Maybe result) -> ProcessResponses result
matchResponse match =
  ProcessResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse firstResponse
      where
        processResponse response =
          case match response of
            Just result -> return result
            Nothing -> do
              discardResponse response
              nextResponse <- fetchResponse
              processResponse response

foldRows :: A.ParseDataRow row -> FoldM IO row result -> ProcessResponses (Either Text (result, Int))
foldRows (A.ParseDataRow rowLength vectorFn) (FoldM foldStep foldStart foldEnd) =
  ProcessResponses def
  where
    def firstResponse fetchResponse discardResponse =
      do
        initialState <- foldStart
        processResponse initialState firstResponse
      where
        processResponse !state =
          \case
            DataRowResponse values ->
              if B.length values == rowLength
                then case vectorFn values 0 of
                  Left error -> return (Left error)
                  Right row -> do
                    nextState <- foldStep state row
                    nextResponse <- fetchResponse
                    processResponse nextState nextResponse
                else return (Left (fromString
                  (showString "Invalid amount of columns: "
                    (shows (B.length values)
                      (showString ", expecting "
                        (show rowLength))))))
            CommandCompleteResponse amount ->
              do
                result <- foldEnd state
                return (Right (result, amount))
            EmptyQueryResponse ->
              do
                result <- foldEnd state
                return (Right (result, 0))
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponse state nextResponse

singleRow :: A.ParseDataRow row -> ProcessResponses (Either Text row)
singleRow (A.ParseDataRow rowLength vectorFn) =
  ProcessResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponseWithoutRow firstResponse
      where
        processResponseWithoutRow =
          \case
            DataRowResponse values ->
              if B.length values == rowLength
                then case vectorFn values 0 of
                  Left error -> return (Left error)
                  Right row -> do
                    nextResponse <- fetchResponse
                    processResponseWithRow row nextResponse
                else return (Left (fromString
                  (showString "Invalid amount of columns: "
                    (shows (B.length values)
                      (showString ", expecting "
                        (show rowLength))))))
            CommandCompleteResponse _ ->
              return (Left "Not a single row")
            EmptyQueryResponse ->
              return (Left "Empty query")
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponseWithoutRow nextResponse
        processResponseWithRow row =
          \case
            DataRowResponse _ ->
              do
                nextResponse <- fetchResponse
                processResponseWithRow row nextResponse
            CommandCompleteResponse _ ->
              return (Right row)
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponseWithRow row nextResponse

rowsAffected :: ProcessResponses Int
rowsAffected =
  ProcessResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse firstResponse
      where
        processResponse =
          \case
            CommandCompleteResponse amount -> return amount
            DataRowResponse _ -> fetchResponse >>= processResponse
            EmptyQueryResponse -> return 0
            otherResponse -> do
              discardResponse otherResponse
              nextResponse <- fetchResponse
              processResponse nextResponse

authenticationStatus :: ProcessResponses AuthenticationStatus
authenticationStatus =
  matchResponse $ \case
    AuthenticationResponse status -> Just status
    _ -> Nothing

parameterStatus :: (ByteString -> ByteString -> result) -> ProcessResponses result
parameterStatus result =
  matchResponse $ \case
    ParameterStatusResponse name value -> Just (result name value)
    _ -> Nothing

parameters :: ProcessResponses (Either Text Bool)
parameters =
  ProcessResponses def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse (Left "Missing the \"integer_datetimes\" setting") firstResponse
      where
        processResponse !state =
          \case
            ParameterStatusResponse name value -> do
              nextResponse <- fetchResponse
              case name of
                "integer_datetimes" -> case value of
                  "on" -> processResponse (Right True) nextResponse
                  "off" -> processResponse (Right False) nextResponse
                  _ -> processResponse (Left ("Unexpected value of the \"integer_datetimes\" setting: " <> (fromString . show) value)) nextResponse
                _ -> processResponse state nextResponse
            ReadyForQueryResponse _ -> return state
            otherResponse -> do
              discardResponse otherResponse
              nextResponse <- fetchResponse
              processResponse state nextResponse

authenticationResult :: ProcessResponses (Either Text AuthenticationResult)
authenticationResult =
  do
    authenticationStatusResult <- authenticationStatus
    case authenticationStatusResult of
      NeedClearTextPasswordAuthenticationStatus -> return (Right NeedClearTextPasswordAuthenticationResult)
      NeedMD5PasswordAuthenticationStatus salt -> return (Right (NeedMD5PasswordAuthenticationResult salt))
      OkAuthenticationStatus -> fmap OkAuthenticationResult <$> parameters
