module Hasql.Protocol.MessagePayloadInterpretation where

import Hasql.Prelude
import Hasql.Protocol.Model
import qualified BinaryParser as A
import qualified Hasql.Protocol.Decoding as D


{-# INLINE dataRow #-}
dataRow ::
  A.BinaryParser row ->
  (row -> result) {-^ Row handler -} ->
  (Text -> result) {-^ Row parsing error handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
dataRow rowParser rowHandler rowParsingErrorHandler protocolErrorHandler messageBytes =
  {-# SCC "dataRow" #-} 
  either rowParsingErrorHandler rowHandler $
  A.run (rowParser <* A.endOfInput) messageBytes

{-# INLINE commandComplete #-}
commandComplete ::
  (Int -> result) {-^ Rows affected handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
commandComplete resultHandler protocolErrorHandler messageBytes =
  {-# SCC "commandComplete" #-} 
  either (protocolErrorHandler . mappend "CommandComplete parsing error: ") resultHandler $
  A.run D.commandCompleteMessageAffectedRows messageBytes

{-# INLINE error #-}
error ::
  (ByteString -> ByteString -> result) {-^ Backend error handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
error backendErrorHandler protocolErrorHandler messageBytes =
  case A.run (D.errorMessage backendErrorHandler) messageBytes of
    Right handler ->
      handler
    Left parsingError ->
      protocolErrorHandler ("ErrorResponse parsing error: " <> parsingError)

{-# INLINE parameterStatus #-}
parameterStatus ::
  (ByteString -> ByteString -> result) {-^ Parameter key-value handler -} ->
  (Text -> result) {-^ Protocol error handler -} ->
  ByteString -> result
parameterStatus parameterStatusHandler protocolErrorHandler =
  either (protocolErrorHandler . mappend "ParameterStatus parsing error: ") id .
  A.run (D.parameterStatusMessagePayloadKeyValue parameterStatusHandler)

{-# INLINE authentication #-}
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
