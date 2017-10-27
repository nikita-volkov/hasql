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
  ParseResponse (Word8 -> forall a. a -> (C.Parse output -> a) -> a)

deriving instance Functor ParseResponse

instance Applicative ParseResponse where
  pure x =
    ParseResponse (\ _ _ parse -> parse (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (ParseResponse left) (ParseResponse right) =
    ParseResponse $ \ type_ yield parse ->
    left type_ yield $ \ leftParse ->
    right type_ yield $ \ rightParse ->
    parse (leftParse <*> rightParse)

instance Alternative ParseResponse where
  empty =
    ParseResponse (\ _ yield _ -> yield)
  {-# INLINE (<|>) #-}
  (<|>) (ParseResponse left) (ParseResponse right) =
    ParseResponse $ \ type_ yield parse ->
    left type_ (right type_ yield parse) parse

{-# INLINE predicateAndParser #-}
predicateAndParser :: (Word8 -> Bool) -> C.Parse result -> ParseResponse result
predicateAndParser predicate parser =
  ParseResponse $ \ type_ yield parse ->
  if predicate type_
    then parse parser
    else yield

{-# INLINE rowOrEnd #-}
rowOrEnd :: A.ParseDataRow output -> (Int -> output) -> ParseResponse output
rowOrEnd pdr amountOutput =
  ParseResponse $ \ type_ yield parse ->
  if
    | G.dataRow type_ -> parse (E.dataRowBody pdr)
    | G.commandComplete type_ -> parse (fmap amountOutput E.commandCompleteBody)
    | G.emptyQuery type_ -> parse (pure (amountOutput 0))
    | otherwise -> yield

{-# INLINE rowsAffected #-}
rowsAffected :: ParseResponse Int
rowsAffected =
  ParseResponse $ \ type_ yield parse ->
  if
    | G.commandComplete type_ -> parse E.commandCompleteBody
    | G.emptyQuery type_ -> parse (pure 0)
    | otherwise -> yield

{-# INLINE error #-}
error :: (ByteString -> ByteString -> error) -> ParseResponse error
error errorResult =
  predicateAndParser G.error (E.errorResponseBody errorResult)

{-# INLINE authenticationStatus #-}
authenticationStatus :: ParseResponse AuthenticationStatus
authenticationStatus =
  predicateAndParser G.authentication E.authenticationBody

{-# INLINE parameterStatus #-}
parameterStatus :: (ByteString -> ByteString -> result) -> ParseResponse result
parameterStatus result =
  predicateAndParser G.parameterStatus (E.parameterStatusBody result)

{-# INLINE readyForQuery #-}
readyForQuery :: ParseResponse ()
readyForQuery =
  predicateAndParser G.readyForQuery (pure ())

{-# INLINE parseComplete #-}
parseComplete :: ParseResponse ()
parseComplete =
  predicateAndParser G.parseComplete (pure ())

{-# INLINE bindComplete #-}
bindComplete :: ParseResponse ()
bindComplete =
  predicateAndParser G.bindComplete (pure ())

{-# INLINE notification #-}
notification :: (Word32 -> ByteString -> ByteString -> notification) -> ParseResponse notification
notification notificationResult =
  ParseResponse $ \ type_ yield parse ->
  if G.notification type_
    then parse (E.notificationBody notificationResult)
    else yield

{-# INLINE any #-}
any :: ParseResponse ()
any =
  ParseResponse $ \ _ _ parse -> parse (pure ())
