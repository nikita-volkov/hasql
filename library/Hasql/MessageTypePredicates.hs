module Hasql.MessageTypePredicates where

import Hasql.Prelude


authentication = (== 82) :: Word8 -> Bool
keyData = (== 75) :: Word8 -> Bool
bindComplete = (== 50) :: Word8 -> Bool
closeComplete = (== 51) :: Word8 -> Bool
commandComplete = (== 67) :: Word8 -> Bool
copyOutData = (== 100) :: Word8 -> Bool
copyOutDone = (== 99) :: Word8 -> Bool
copyInResponse = (== 71) :: Word8 -> Bool
copyOut = (== 72) :: Word8 -> Bool
copyBoth = (== 87) :: Word8 -> Bool
dataRow = (== 68) :: Word8 -> Bool
emptyQuery = (== 73) :: Word8 -> Bool
error = (== 69) :: Word8 -> Bool
functionCall = (== 86) :: Word8 -> Bool
noData = (== 110) :: Word8 -> Bool
notice = (== 78) :: Word8 -> Bool
notification = (== 65) :: Word8 -> Bool
parameterDescription = (== 116) :: Word8 -> Bool
parameterStatus = (== 83) :: Word8 -> Bool
parseComplete = (== 49) :: Word8 -> Bool
portalSuspended = (== 115) :: Word8 -> Bool
readyForQuery = (== 90) :: Word8 -> Bool
rowDescription = (== 84) :: Word8 -> Bool
