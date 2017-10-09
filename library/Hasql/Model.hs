module Hasql.Model where

import Hasql.Prelude hiding (State, peek)


data Response =
  DataRowResponse !(Vector (Maybe ByteString)) |
  CommandCompleteResponse !Int |
  ReadyForQueryResponse !TransactionStatus |
  ParseCompleteResponse |
  EmptyQueryResponse |
  NotificationResponse !Word32 !ByteString !ByteString |
  ErrorResponse !ByteString !ByteString |
  AuthenticationResponse !AuthenticationStatus |
  ParameterStatusResponse !ByteString !ByteString
  deriving (Show)

data AuthenticationStatus =
  NeedClearTextPasswordAuthenticationStatus |
  NeedMD5PasswordAuthenticationStatus !ByteString |
  OkAuthenticationStatus
  deriving (Show)

data TransactionStatus =
  IdleTransactionStatus |
  ActiveTransactionStatus |
  FailedTransactionStatus
  deriving (Show)

data AuthenticationResult =
  NeedClearTextPasswordAuthenticationResult |
  NeedMD5PasswordAuthenticationResult !ByteString |
  OkAuthenticationResult !Bool

data Notification = Notification !Word32 !ByteString !ByteString deriving (Show)

data Error =
  {-| 
  An erroneous result received from the DB.
  The components are:

  * The SQLSTATE code for the error. The SQLSTATE code identifies the type of error that has occurred; 
  it can be used by front-end applications to perform specific operations (such as error handling) 
  in response to a particular database error. 
  For a list of the possible SQLSTATE codes, see Appendix A.
  This field is not localizable, and is always present.

  * The primary human-readable error message (typically one line). Always present.
  -}
  BackendError !ByteString !ByteString |
  {-|
  Can happen as a result of an incorrect decoder being applied.
  -}
  DecodingError !Text |
  {-|
  Problems with the connection.
  -}
  TransportError !Text |
  {-|
  An unexpected or broken data packet received from the server.
  Can happen as a result of 
  the server sending an unsupported message or
  something interfering
  in the communication channel.
  This error type is highly unlikely.
  -}
  ProtocolError !Text
  deriving (Show, Eq)
