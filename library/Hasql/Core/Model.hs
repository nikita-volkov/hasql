module Hasql.Core.Model where

import Hasql.Prelude hiding (State, peek)


{-|
An internal request to the dispatcher.
-}
data Request
  -- ParseRequest !ByteString !ByteString !(Vector Word32) !(Either Error () -> IO ()) |
  -- BindRequest !ByteString !ByteString !D.Encoding !(Either Error () -> IO ()) |
  -- ExecuteRequest 

data ResultProcessor =
  RowsResultProcessor !(ByteString -> IO ()) !(IO ()) |
  RowsAffectedResultProcessor !(Int -> IO ())

data Message = Message !Word8 !ByteString

data Notification = Notification !Word32 !ByteString !ByteString

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
data ErrorMessage = ErrorMessage !ByteString !ByteString

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
  deriving (Show)
