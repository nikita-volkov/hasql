{-|
https://www.postgresql.org/docs/9.6/static/protocol-message-formats.html
-}
module Hasql.Core.Protocol.Encoding where

import Hasql.Prelude
import Hasql.Core.Protocol.Model
import ByteString.StrictBuilder
import qualified Data.Vector as A
import qualified Crypto.Hash as B


-- * Constants
-------------------------

{-# NOINLINE nullByte #-}
nullByte =
  word8 0

{-# NOINLINE nullSize #-}
nullSize =
  int32BE (-1)

-- *
-------------------------

array elementBuilder vector =
  word16BE (fromIntegral (A.length vector)) <>
  foldMap elementBuilder vector

sizedPayload payload =
  word32BE (fromIntegral (builderLength payload) + 4) <> payload

startUpMessage majorProtocolVersion minorProtocolVersion username databaseMaybe runtimeParameters =
  sizedPayload (protocolVersion majorProtocolVersion minorProtocolVersion <> parameters <> nullByte)
  where
    parameters =
      parameter "user" username <>
      foldMap database databaseMaybe <>
      foldMap (uncurry parameter) runtimeParameters
      where
        parameter name value =
          nullTerminatedString name <> nullTerminatedString value
        database database =
          parameter "database" database

{-# NOINLINE syncMessage #-}
syncMessage =
  asciiChar 'S' <> word32BE 4

{-# NOINLINE terminateMessage #-}
terminateMessage =
  asciiChar 'X' <> word32BE 4

protocolVersion major minor =
  word16BE major <> word16BE minor

nullTerminatedString string =
  bytes string <> nullByte

{-# NOINLINE passwordMessageIdentifier #-}
passwordMessageIdentifier =
  asciiChar 'p'

passwordMessage payload =
  passwordMessageIdentifier <> sizedPayload (payload <> nullByte)

clearTextPasswordMessage password =
  passwordMessage (bytes password)

md5PasswordMessage username password salt =
  passwordMessage ("md5" <> bytes (md5HexBytes (md5HexBytes (password <> username) <> salt)))
  where
    md5HexBytes =
      fromString . show . B.hashWith B.MD5

{-# NOINLINE parseMessageIdentifier #-}
parseMessageIdentifier =
  asciiChar 'P'

{-|
Parse (F)
Byte1('P')
Identifies the message as a Parse command.

Int32
Length of message contents in bytes, including self.

String
The name of the destination prepared statement (an empty string selects the unnamed prepared statement).

String
The query string to be parsed.

Int16
The number of parameter data types specified (can be zero). Note that this is not an indication of the number of parameters that might appear in the query string, only the number that the frontend wants to prespecify types for.

Then, for each parameter, there is the following:

Int32
Specifies the object ID of the parameter data type. Placing a zero here is equivalent to leaving the type unspecified.
-}
{-# INLINE parseMessage #-}
parseMessage preparedStatementName query oids =
  parseMessageIdentifier <> sizedPayload payload
  where
    payload =
      nullTerminatedString preparedStatementName <>
      nullTerminatedString query <>
      array word32BE oids

{-# NOINLINE bindMessageIdentifier #-}
bindMessageIdentifier =
  asciiChar 'B'

{-|
Bind (F)
Byte1('B')
Identifies the message as a Bind command.

Int32
Length of message contents in bytes, including self.

String
The name of the destination portal (an empty string selects the unnamed portal).

String
The name of the source prepared statement (an empty string selects the unnamed prepared statement).

Int16
The number of parameter format codes that follow (denoted C below). This can be zero to indicate that there are no parameters or that the parameters all use the default format (text); or one, in which case the specified format code is applied to all parameters; or it can equal the actual number of parameters.

Int16[C]
The parameter format codes. Each must presently be zero (text) or one (binary).

Int16
The number of parameter values that follow (possibly zero). This must match the number of parameters needed by the query.

Next, the following pair of fields appear for each parameter:

Int32
The length of the parameter value, in bytes (this count does not include itself). Can be zero. As a special case, -1 indicates a NULL parameter value. No value bytes follow in the NULL case.

Byten
The value of the parameter, in the format indicated by the associated format code. n is the above length.

After the last parameter, the following fields appear:

Int16
The number of result-column format codes that follow (denoted R below). This can be zero to indicate that there are no result columns or that the result columns should all use the default format (text); or one, in which case the specified format code is applied to all result columns (if any); or it can equal the actual number of result columns of the query.

Int16[R]
The result-column format codes. Each must presently be zero (text) or one (binary).
-}      
binaryFormatBindMessage portalName preparedStatementName parameters =
  bindMessageIdentifier <> sizedPayload payload
  where
    payload =
      nullTerminatedString portalName <>
      nullTerminatedString preparedStatementName <>
      uniformBinaryFormatCodes <>
      array nullableSizedValue parameters <>
      uniformBinaryFormatCodes

binaryFormatBindMessageWithEncodedParams portalName preparedStatementName paramsAmount encodedParams =
  bindMessageIdentifier <> sizedPayload payload
  where
    payload =
      nullTerminatedString portalName <>
      nullTerminatedString preparedStatementName <>
      uniformBinaryFormatCodes <>
      word16BE paramsAmount <>
      encodedParams <>
      uniformBinaryFormatCodes

{-# NOINLINE uniformBinaryFormatCodes #-}
uniformBinaryFormatCodes =
  word16BE 1 <> word16BE 1

sizedValue value =
  word32BE (fromIntegral (builderLength value)) <> value

nullableSizedValue =
  maybe nullSize sizedValue

{-|
Execute (F)
Byte1('E')
Identifies the message as an Execute command.

Int32
Length of message contents in bytes, including self.

String
The name of the portal to execute (an empty string selects the unnamed portal).

Int32
Maximum number of rows to return, if portal contains a query that returns rows (ignored otherwise). Zero denotes "no limit".
-}
unlimitedExecuteMessage portalName =
  executeMessageIdentifier <> sizedPayload payload
  where
    payload =
      nullTerminatedString portalName <>
      word32BE 0

{-# NOINLINE executeMessageIdentifier #-}
executeMessageIdentifier =
  asciiChar 'E'

