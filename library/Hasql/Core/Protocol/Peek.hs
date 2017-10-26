module Hasql.Core.Protocol.Peek where

import Hasql.Prelude hiding (take)
import Hasql.Core.Model
import Ptr.Peek
import qualified Hasql.Core.Protocol.Parse.Responses as G
import qualified Ptr.Parse as F
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
messageTypeAndPayload :: (Text -> a) -> (Word8 -> F.Parse a) -> Peek (Peek a)
messageTypeAndPayload error parse_ =
  messageTypeAndLength $ \ !messageType !length ->
  parse length (parse_ messageType) (\ _ -> error "Not enough data") error


-- * Responses
-------------------------

{-# INLINE response #-}
response :: (Text -> x) -> (Maybe Response -> x) -> Peek (Peek x)
response error response =
  {-# SCC "response" #-} 
  messageTypeAndPayload error (fmap response . G.responseBody)
