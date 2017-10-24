module Hasql.Core.Protocol.Peek where

import Hasql.Prelude hiding (take)
import Hasql.Core.Model
import Ptr.Peek
import qualified Hasql.Core.Protocol.Take as G
import qualified Ptr.Take as F
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as A
import qualified Data.Vector as D
import qualified Hasql.Core.MessageTypePredicates as C
import qualified Hasql.Core.NoticeFieldTypes as E



-- * Postgres Protocol-specific Primitives
-------------------------

{-# INLINE messageTypeAndLength #-}
messageTypeAndLength :: (Word8 -> Int -> a) -> Peek a
messageTypeAndLength cont =
  cont <$> word8 <*> payloadLength

{-# INLINE payloadLength #-}
payloadLength :: Peek Int
payloadLength =
  subtract 4 . fromIntegral <$> beWord32

{-# INLINE messageTypeAndPayload #-}
messageTypeAndPayload :: (Word8 -> F.Take a) -> Peek (Peek (Maybe a))
messageTypeAndPayload take_ =
  messageTypeAndLength $ \(!messageType) (!length) ->
  take length (take_ messageType)


-- * Responses
-------------------------

{-# INLINE response #-}
response :: Peek (Peek (Maybe (Maybe (Either Text Response))))
response =
  {-# SCC "response" #-} 
  messageTypeAndPayload G.responseBody
