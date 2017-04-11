module Hasql.Client.Model where

import Hasql.Prelude


data Error =
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

data BackendSettings =
  {-|
  * Integer datetimes
  -}
  BackendSettings Bool
