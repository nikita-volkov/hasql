module Hasql.Protocol.MessagePayloadInterpretation where

import Hasql.Prelude
import Hasql.Protocol.Model
import qualified BinaryParser as A
import qualified Hasql.Protocol.Decoding as D


dataRow ::
  A.BinaryParser row ->
  (row -> result) {-^ Row handler -} ->
  (Text -> result) {-^ Row parsing error handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
dataRow rowParser rowHandler rowParsingErrorHandler protocolErrorHandler messageBytes =
  either rowParsingErrorHandler rowHandler $
  A.run (rowParser <* A.endOfInput) messageBytes

commandComplete ::
  (Int -> result) {-^ Rows affected handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
commandComplete resultHandler protocolErrorHandler messageBytes =
  either (protocolErrorHandler . mappend "CommandComplete parsing error: ") resultHandler $
  A.run D.commandCompleteMessageAffectedRows messageBytes

errorResponse ::
  (ByteString -> ByteString -> result) {-^ Backend error handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
errorResponse backendErrorHandler protocolErrorHandler messageBytes =
  case A.run D.errorMessage messageBytes of
    Right (Error code message) ->
      backendErrorHandler code message
    Left parsingError ->
      protocolErrorHandler ("ErrorResponse parsing error: " <> parsingError)

parameterStatus ::
  (ByteString -> ByteString -> result) {-^ Parameter key-value handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
parameterStatus parameterStatusHandler protocolErrorHandler =
  either (protocolErrorHandler . mappend "ParameterStatus parsing error: ") id .
  A.run (D.parameterStatusMessagePayloadKeyValue parameterStatusHandler)

authentication ::
  result {-^ Ok handler -} ->
  result {-^ ClearTextPassword handler -} ->
  (ByteString -> result) {-^ MD5 with salt handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
authentication okHandler clearTextPasswordHandler md5PasswordHandler protocolErrorHandler messageBytes =
  case A.run D.authenticationMessage messageBytes of
    Right authenticationMessage ->
      case authenticationMessage of
        OkAuthenticationMessage ->
          okHandler
        ClearTextPasswordAuthenticationMessage ->
          clearTextPasswordHandler
        MD5PasswordAuthenticationMessage salt ->
          md5PasswordHandler salt
    Left parsingError ->
      protocolErrorHandler ("AuthenticationMessage parsing error: " <> parsingError)
