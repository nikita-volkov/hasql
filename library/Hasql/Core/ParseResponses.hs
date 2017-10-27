module Hasql.Core.ParseResponses where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.ParseDataRow as A
import qualified Hasql.Core.ParseResponse as F
import qualified Data.Vector as B
import qualified Hasql.Core.Protocol.Parse.Responses as E
import qualified Ptr.Parse as C
import qualified Ptr.ByteString as D


newtype ParseResponses output =
  ParseResponses (forall result. Word8 -> (Text -> result) -> (ParseResponses output -> result) -> (output -> result) -> C.Parse result -> C.Parse result)

deriving instance Functor ParseResponses

instance Applicative ParseResponses where
  pure = return
  (<*>) = ap

instance Monad ParseResponses where
  return x =
    ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
    pure (outputResult x) 
  (>>=) (ParseResponses left) rightK =
    ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
    left type_ errorResult
      (\ parseResponses -> recurResult (parseResponses >>= rightK))
      (\ leftOutput -> recurResult (rightK leftOutput))
      alternative

foldRows :: Fold row output -> A.ParseDataRow row -> ParseResponses (output, Int)
foldRows (Fold step start end) pdr =
  iterate start
  where
    iterate !state =
      ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
      if
        | G.dataRow type_ ->
          flip fmap (E.dataRowBody pdr) $ \ !row ->
          recurResult (iterate (step state row))
        | G.commandComplete type_ ->
          flip fmap (E.commandCompleteBody) $ \ !amount ->
          outputResult (end state, amount)
        | G.emptyQuery type_ ->
          return (outputResult (end state, 0))
        | True ->
          alternative

singleRow :: A.ParseDataRow row -> ParseResponses row
singleRow pdr =
  iterate (Left "Not a single row")
  where
    iterate !state =
      ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
      if
        | G.dataRow type_ ->
          flip fmap (E.dataRowBody pdr) $ \ !row ->
          recurResult (iterate (Right row))
        | G.commandComplete type_ ->
          return (either errorResult outputResult state)
        | G.emptyQuery type_ ->
          return (either errorResult outputResult state)
        | True ->
          alternative

rowsAffected :: ParseResponses Int
rowsAffected =
  ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
  if
    | G.commandComplete type_ ->
      flip fmap (E.commandCompleteBody) $ \ !amount ->
      outputResult amount
    | G.emptyQuery type_ ->
      return (outputResult 0)
    | True ->
      alternative

authenticationStatus :: ParseResponses AuthenticationStatus
authenticationStatus =
  ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
  if G.authentication type_
    then fmap outputResult E.authenticationBody
    else alternative

parameters :: ParseResponses Bool
parameters =
  iterate (Left "Missing the \"integer_datetimes\" setting")
  where
    iterate !state =
      ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
      if
        | G.parameterStatus type_ ->
          E.parameterStatusBody $ \ name value ->
          if name == "integer_datetimes"
            then case value of
              "on" -> recurResult (iterate (Right True))
              "off" -> recurResult (iterate (Right False))
              _ -> recurResult (iterate (Left ("Unexpected value of the \"integer_datetimes\" setting: " <> (fromString . show) value)))
            else recurResult (iterate state)
        | G.readyForQuery type_ ->
          return (either errorResult outputResult state)
        | otherwise ->
          alternative

authenticationResult :: ParseResponses AuthenticationResult
authenticationResult =
  do
    authenticationStatusResult <- authenticationStatus
    case authenticationStatusResult of
      NeedClearTextPasswordAuthenticationStatus -> return (NeedClearTextPasswordAuthenticationResult)
      NeedMD5PasswordAuthenticationStatus salt -> return (NeedMD5PasswordAuthenticationResult salt)
      OkAuthenticationStatus -> OkAuthenticationResult <$> parameters

parseComplete :: ParseResponses ()
parseComplete =
  ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
  if G.parseComplete type_
    then return (outputResult ())
    else alternative

bindComplete :: ParseResponses ()
bindComplete =
  ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
  if G.bindComplete type_
    then return (outputResult ())
    else alternative

readyForQuery :: ParseResponses ()
readyForQuery =
  ParseResponses $ \ type_ errorResult recurResult outputResult alternative ->
  if G.readyForQuery type_
    then return (outputResult ())
    else alternative
