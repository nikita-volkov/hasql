module Hasql.Core.ParseResponse where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.ParseDataRow as A
import qualified Data.Vector as B
import qualified Hasql.Core.Protocol.Parse.Responses as E
import qualified Ptr.Parse as C
import qualified Ptr.ByteString as D

newtype ParseResponse output =
  ParseResponse (forall result. Word8 -> C.Parse result -> (output -> C.Parse result) -> C.Parse result)

deriving instance Functor ParseResponse

instance Applicative ParseResponse where
  pure x =
    ParseResponse $ \ _ _ succeed -> succeed x
  (<*>) (ParseResponse left) (ParseResponse right) =
    ParseResponse $ \ type_ fail succeed ->
    left type_ fail
      (\ leftOutput ->
        right type_ fail
          (\ rightOutput -> succeed (leftOutput rightOutput)))

instance Alternative ParseResponse where
  empty =
    ParseResponse $ \ _ fail _ -> fail
  (<|>) (ParseResponse left) (ParseResponse right) =
    ParseResponse $ \ type_ fail succeed ->
    left type_ (right type_ fail succeed) succeed

rowOrEnd :: (row -> output) -> (Int -> output) -> A.ParseDataRow row -> ParseResponse output
rowOrEnd rowOutput amountOutput pdr =
  ParseResponse $ \ type_ fail succeed ->
  if
    | G.dataRow type_ -> E.dataRowBody pdr >>= succeed . rowOutput
    | G.commandComplete type_ -> E.commandCompleteBody >>= succeed . amountOutput
    | G.emptyQuery type_ -> succeed (amountOutput 0)
    | otherwise -> fail

error :: (ByteString -> ByteString -> error) -> ParseResponse error
error errorResult =
  ParseResponse $ \ type_ fail succeed ->
  if G.error type_
    then E.errorResponseBody errorResult >>= succeed
    else fail

notification :: (Word32 -> ByteString -> ByteString -> notification) -> ParseResponse notification
notification notificationResult =
  ParseResponse $ \ type_ fail succeed ->
  if G.notification type_
    then E.notificationBody notificationResult >>= succeed
    else fail

any :: ParseResponse ()
any =
  ParseResponse $ \ _ _ succeed -> succeed ()
