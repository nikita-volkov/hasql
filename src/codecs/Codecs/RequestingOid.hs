module Codecs.RequestingOid where

import Data.HashMap.Strict qualified as HashMap
import Platform.LookingUp qualified as LookingUp
import Platform.Prelude

type RequestingOid =
  LookingUp
    (Maybe Text, Text)
    (Word32, Word32)

toUnknownTypes ::
  RequestingOid f a ->
  HashSet (Maybe Text, Text)
toUnknownTypes (LookingUp unknownTypes _) =
  fromList unknownTypes

toBase ::
  RequestingOid f a ->
  HashMap (Maybe Text, Text) (Word32, Word32) ->
  f a
toBase (LookingUp _unknownTypes decoder) oidCache =
  decoder \key ->
    HashMap.lookup key oidCache
      & fromMaybe (0, 0)

lift :: f a -> RequestingOid f a
lift = LookingUp.lift

hoist :: (f a -> g b) -> RequestingOid f a -> RequestingOid g b
hoist = LookingUp.hoist
