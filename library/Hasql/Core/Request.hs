module Hasql.Core.Request where

import Hasql.Prelude
import Hasql.Core.Model
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified Hasql.Core.ParseMessageStream as A
import qualified Hasql.Core.ParseMessage as E
import qualified Hasql.Protocol.Encoding as K
import qualified Hasql.Protocol.Model as C
import qualified Data.Vector as G


{-|
A builder of concatenated outgoing messages and
a parser of the stream of incoming messages.
-}
data Request result =
  Request !B.Builder !(ExceptT Text A.ParseMessageStream result)

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
    parse = lift A.parseComplete

{-# INLINE bind #-}
bind :: ByteString -> ByteString -> Vector (Maybe B.Builder) -> Request ()
bind portalName preparedStatementName parameters =
  Request builder parse
  where
    builder = K.binaryFormatBindMessage portalName preparedStatementName parameters
    parse = lift A.bindComplete

{-# INLINE bindEncoded #-}
bindEncoded :: ByteString -> ByteString -> Int -> B.Builder -> Request ()
bindEncoded portalName preparedStatementName paramsAmount paramsBuilder =
  Request builder parse
  where
    builder = K.binaryFormatBindMessageWithEncodedParams portalName preparedStatementName (fromIntegral paramsAmount) paramsBuilder
    parse = lift A.bindComplete

{-# INLINE execute #-}
execute :: ByteString -> A.ParseMessageStream (Either Text result) -> Request result
execute portalName parse =
  Request builder (ExceptT parse)
  where
    builder = K.unlimitedExecuteMessage portalName

{-# INLINE sync #-}
sync :: Request ()
sync =
  Request K.syncMessage (lift A.readyForQuery)

{-# INLINE startUp #-}
startUp :: ByteString -> Maybe ByteString -> [(ByteString, ByteString)] -> Request (Either ErrorMessage AuthenticationResult)
startUp username databaseMaybe runtimeParameters =
  Request 
    (K.startUpMessage 3 0 username databaseMaybe runtimeParameters)
    (ExceptT A.authentication)

{-# INLINE clearTextPassword #-}
clearTextPassword :: ByteString -> Request (Either ErrorMessage AuthenticationResult)
clearTextPassword password =
  Request
    (K.clearTextPasswordMessage password)
    (ExceptT A.authentication)

{-# INLINE md5Password #-}
md5Password :: ByteString -> ByteString -> ByteString -> Request (Either ErrorMessage AuthenticationResult)
md5Password username password salt =
  Request
    (K.md5PasswordMessage username password salt)
    (ExceptT A.authentication)

{-# INLINE unparsedStatement #-}
unparsedStatement :: ByteString -> ByteString -> Vector Word32 -> B.Builder -> A.ParseMessageStream (Either Text result) -> Request result
unparsedStatement name template oidVec bytesBuilder parseMessageStream =
  parse name template oidVec *>
  parsedStatement name template (G.length oidVec) bytesBuilder parseMessageStream

{-# INLINE parsedStatement #-}
parsedStatement :: ByteString -> ByteString -> Int -> B.Builder -> A.ParseMessageStream (Either Text result) -> Request result
parsedStatement name template paramsAmount bytesBuilder parseMessageStream =
  bindEncoded "" name paramsAmount bytesBuilder *>
  execute "" parseMessageStream
