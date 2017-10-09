module Hasql.MessageTypeNames where

import Hasql.Prelude


string :: Word8 -> String
string =
  \case
    82 -> "Authentication"
    75 -> "KeyData"
    50 -> "BindComplete"
    51 -> "CloseComplete"
    67 -> "CommandComplete"
    100 -> "CopyOutData"
    99 -> "CopyOutDone"
    71 -> "CopyInResponse"
    72 -> "CopyOut"
    87 -> "CopyBoth"
    68 -> "DataRow"
    73 -> "EmptyQuery"
    69 -> "Error"
    86 -> "FunctionCall"
    110 -> "NoData"
    78 -> "Notice"
    65 -> "Notification"
    116 -> "ParameterDescription"
    83 -> "ParameterStatus"
    49 -> "ParseComplete"
    115 -> "PortalSuspended"
    90 -> "ReadyForQuery"
    84 -> "RowDescription"
    x -> showString "Unknown message type: " (show x)
