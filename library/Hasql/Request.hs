module Hasql.Request where

import Hasql.Prelude
import Hasql.Model hiding (Error(..))
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified Hasql.ParseMessageStream as A
import qualified Hasql.ParseMessage as E
import qualified Hasql.Protocol.Encoding as K
import qualified Hasql.Protocol.Model as C
import qualified Data.Vector as G


{-|
A builder of concatenated outgoing messages and
a parser of the stream of incoming messages.
-}
data Request result =
  Request !B.Builder !(ExceptT Error A.ParseMessageStream result)

data Error =
  BackendError !ByteString !ByteString

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

{-# INLINE simple #-}
simple :: B.Builder -> A.ParseMessageStream result -> Request result
simple builder pms =
  Request builder (ExceptT (A.orParseMessage (E.errorCont BackendError) pms))

{-# INLINE parse #-}
parse :: ByteString -> ByteString -> Vector Word32 -> Request ()
parse preparedStatementName query oids =
  simple (K.parseMessage preparedStatementName query oids) A.parseComplete

{-# INLINE bind #-}
bind :: ByteString -> ByteString -> Vector (Maybe B.Builder) -> Request ()
bind portalName preparedStatementName parameters =
  simple (K.binaryFormatBindMessage portalName preparedStatementName parameters) A.bindComplete

{-# INLINE bindEncoded #-}
bindEncoded :: ByteString -> ByteString -> Int -> B.Builder -> Request ()
bindEncoded portalName preparedStatementName paramsAmount paramsBuilder =
  simple
    (K.binaryFormatBindMessageWithEncodedParams portalName preparedStatementName (fromIntegral paramsAmount) paramsBuilder)
    A.bindComplete

{-# INLINE execute #-}
execute :: ByteString -> A.ParseMessageStream result -> Request result
execute portalName pms =
  simple (K.unlimitedExecuteMessage portalName) pms

{-# INLINE sync #-}
sync :: Request ()
sync =
  simple K.syncMessage A.readyForQuery

{-# INLINE startUp #-}
startUp :: ByteString -> Maybe ByteString -> [(ByteString, ByteString)] -> Request AuthenticationResult
startUp username databaseMaybe runtimeParameters =
  simple 
    (K.startUpMessage 3 0 username databaseMaybe runtimeParameters)
    (A.authentication)

{-# INLINE clearTextPassword #-}
clearTextPassword :: ByteString -> Request AuthenticationResult
clearTextPassword password =
  simple
    (K.clearTextPasswordMessage password)
    (A.authentication)

{-# INLINE md5Password #-}
md5Password :: ByteString -> ByteString -> ByteString -> Request AuthenticationResult
md5Password username password salt =
  simple
    (K.md5PasswordMessage username password salt)
    (A.authentication)

{-# INLINE unparsedStatement #-}
unparsedStatement :: ByteString -> ByteString -> Vector Word32 -> B.Builder -> A.ParseMessageStream result -> Request result
unparsedStatement name template oidVec bytesBuilder parseMessageStream =
  parse name template oidVec *>
  parsedStatement name template (G.length oidVec) bytesBuilder parseMessageStream

{-# INLINE parsedStatement #-}
parsedStatement :: ByteString -> ByteString -> Int -> B.Builder -> A.ParseMessageStream result -> Request result
parsedStatement name template paramsAmount bytesBuilder parseMessageStream =
  bindEncoded "" name paramsAmount bytesBuilder *>
  execute "" parseMessageStream
