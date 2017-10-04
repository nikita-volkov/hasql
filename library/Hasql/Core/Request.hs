module Hasql.Core.Request where

import Hasql.Prelude
import Hasql.Core.Model
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified Hasql.Core.ParseMessageStream as A
import qualified Hasql.Protocol.Encoding as K


{-|
A builder of concatenated outgoing messages and
a parser of the stream of incoming messages.
-}
data Request result =
  Request !B.Builder !(A.ParseMessageStream result)

instance Functor Request where
  {-# INLINE fmap #-}
  fmap mapping (Request builder parse) =
    Request builder (fmap mapping parse)

instance Applicative Request where
  {-# INLINE pure #-}
  pure =
    Request mempty . return
  {-# INLINE (<*>) #-}
  (<*>) (Request leftBuilder leftParse) (Request rightBuilder rightParse) =
    Request (leftBuilder <> rightBuilder) (leftParse <*> rightParse)

{-# INLINE parse #-}
parse :: ByteString -> ByteString -> Vector Word32 -> Request ()
parse preparedStatementName query oids =
  Request builder parse
  where
    builder = K.parseMessage preparedStatementName query oids
    parse = A.parseComplete

{-# INLINE bind #-}
bind :: ByteString -> ByteString -> Vector (Maybe B.Builder) -> Request ()
bind portalName preparedStatementName parameters =
  Request builder parse
  where
    builder = K.binaryFormatBindMessage portalName preparedStatementName parameters
    parse = A.bindComplete

{-# INLINE execute #-}
execute :: ByteString -> A.ParseMessageStream result -> Request result
execute portalName parse =
  Request builder parse
  where
    builder = K.unlimitedExecuteMessage portalName

{-# INLINE sync #-}
sync :: Request ()
sync =
  Request K.syncMessage A.readyForQuery
