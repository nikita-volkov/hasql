module Hasql.Codecs.RequestingOid
  ( RequestingOid,
    toUnknownTypes,
    toBase,
    requestAndHandle,
    lift,
    hoist,
    lookup,
    lookingUp,
    hoistLookingUp,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasql.Codecs.RequestingOid.LookingUp qualified as LookingUp
import Hasql.Platform.Prelude hiding (lift, lookup)
import PostgreSQL.Binary.Decoding qualified as Binary

type RequestingOid =
  LookingUp.LookingUp
    (Maybe Text, Text)
    (Word32, Word32)

toUnknownTypes ::
  RequestingOid a ->
  HashSet (Maybe Text, Text)
toUnknownTypes (LookingUp.LookingUp unknownTypes _) =
  fromList unknownTypes

toBase ::
  RequestingOid a ->
  HashMap (Maybe Text, Text) (Word32, Word32) ->
  a
toBase (LookingUp.LookingUp _unknownTypes decoder) oidCache =
  decoder \key ->
    HashMap.lookup key oidCache
      & fromMaybe (0, 0)

requestAndHandle ::
  [(Maybe Text, Text)] ->
  (((Maybe Text, Text) -> (Word32, Word32)) -> a) ->
  RequestingOid a
requestAndHandle keys fn = LookingUp.LookingUp keys fn

lift :: a -> RequestingOid a
lift = LookingUp.lift

hoist :: (a -> b) -> RequestingOid a -> RequestingOid b
hoist fn (LookingUp.LookingUp keys use) = LookingUp.LookingUp keys (fn . use)

lookup :: (Maybe Text, Text) -> RequestingOid (Word32, Word32)
lookup = LookingUp.lookup

lookingUp :: (Maybe Text, Text) -> ((Word32, Word32) -> a) -> RequestingOid a
lookingUp = LookingUp.lookingUp

hoistLookingUp :: (Maybe Text, Text) -> ((Word32, Word32) -> a -> b) -> RequestingOid a -> RequestingOid b
hoistLookingUp = LookingUp.hoistLookingUp
