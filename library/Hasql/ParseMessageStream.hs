module Hasql.ParseMessageStream where

import Hasql.Prelude hiding (error)
import Hasql.Model hiding (Error(..))
import Control.Monad.Free.Church
import qualified Hasql.ParseMessage as A
import qualified Hasql.ParseDataRow as F
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified Hasql.Protocol.Model as C
import qualified Hasql.Choosing as B
import qualified BinaryParser as D


{-|
A specification of how to parse a stream of messages.
-}
newtype ParseMessageStream result =
  ParseMessageStream (F (Compose A.ParseMessage (Either Text)) result)
  deriving (Functor, Applicative, Monad)

raiseError :: Text -> ParseMessageStream result
raiseError error =
  ParseMessageStream (liftF (Compose (pure (Left error))))

parseMessage :: A.ParseMessage result -> ParseMessageStream result
parseMessage parseMessage =
  ParseMessageStream (liftF (Compose (fmap Right parseMessage)))

orParseMessage :: A.ParseMessage leftResult -> ParseMessageStream rightResult -> ParseMessageStream (Either leftResult rightResult)
orParseMessage pm (ParseMessageStream (F freeFn)) =
  ParseMessageStream $ F $ \pure free ->
  freeFn (pure . Right) $ \(Compose rightPm) ->
  free (Compose (rightPm <|> fmap (Right . pure . Left) pm))

error :: ParseMessageStream ErrorMessage
error =
  parseMessage A.error

errorCont :: (ByteString -> ByteString -> result) -> ParseMessageStream result
errorCont message =
  parseMessage (A.errorCont message)

commandComplete :: ParseMessageStream Int
commandComplete =
  parseMessage A.commandComplete

row :: F.ParseDataRow row -> ParseMessageStream row
row pdr =
  join (parseMessage (row <|> end))
  where
    row =
      (\row -> parseMessage (A.commandCompleteWithoutAmount <|> A.emptyQuery) $> row) <$> A.dataRow pdr
    end =
      raiseError "Not a single row" <$ (A.commandCompleteWithoutAmount <|> A.emptyQuery)

rowWithCount :: F.ParseDataRow row -> ParseMessageStream (row, Int)
rowWithCount pdr =
  join (parseMessage (row <|> end))
  where
    row =
      (\row -> parseMessage ((row,) <$> A.commandComplete <|> (row, 0) <$ A.emptyQuery)) <$> A.dataRow pdr
    end =
      raiseError "Not a single row" <$ (A.commandCompleteWithoutAmount <|> A.emptyQuery)

rows :: F.ParseDataRow row -> Fold row result -> ParseMessageStream result
rows parseDataRow (Fold foldStep foldStart foldEnd) =
  fold foldStart
  where
    fold !state =
      join (parseMessage (step <|> end))
      where
        step = fold . foldStep state <$> A.dataRow parseDataRow
        end = pure (foldEnd state) <$ (A.commandCompleteWithoutAmount <|> A.emptyQuery)

rowsWithCount :: F.ParseDataRow row -> Fold row result -> ParseMessageStream (result, Int)
rowsWithCount parseDataRow (Fold foldStep foldStart foldEnd) =
  fold foldStart
  where
    fold !state =
      join (parseMessage (step <|> end))
      where
        step = fold . foldStep state <$> A.dataRow parseDataRow
        end = (\x -> pure (foldEnd state, x)) <$> (A.commandComplete <|> 0 <$ A.emptyQuery)

rowsAffected :: ParseMessageStream Int
rowsAffected =
  parseMessage (A.commandComplete <|> 0 <$ A.emptyQuery)

parseComplete :: ParseMessageStream ()
parseComplete =
  parseMessage A.parseComplete

bindComplete :: ParseMessageStream ()
bindComplete =
  parseMessage A.bindComplete

readyForQuery :: ParseMessageStream ()
readyForQuery =
  parseMessage A.readyForQuery

authentication :: ParseMessageStream AuthenticationResult
authentication =
  do
    response <- parseMessage A.authentication
    case response of
      C.OkAuthenticationMessage -> fmap OkAuthenticationResult params
      C.ClearTextPasswordAuthenticationMessage -> return NeedClearTextPasswordAuthenticationResult
      C.MD5PasswordAuthenticationMessage salt -> return (NeedMD5PasswordAuthenticationResult salt)

params :: ParseMessageStream Bool
params =
  iterate (Left "Missing the \"integer_datetimes\" setting")
  where
    iterate !state =
      join (parseMessage (param <|> readyForQuery))
      where
        param =
          flip fmap A.parameterStatus $ \(name, value) ->
          case name of
            "integer_datetimes" -> case value of
              "on" -> iterate (Right True)
              "off" -> iterate (Right False)
              _ -> iterate (Left ("Unexpected value of the \"integer_datetimes\" setting: " <> (fromString . show) value))
            _ -> iterate state
        readyForQuery =
          A.readyForQuery $>
          case state of
            Left error -> raiseError error
            Right result -> return result
