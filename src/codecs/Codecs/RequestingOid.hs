module Codecs.RequestingOid
  ( RequestingOid,
    toUnknownTypes,
    toBase,
    lift,
    hoist,
    lookup,
    lookingUp,
    hoistLookingUp,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Platform.LookingUp qualified as LookingUp
import Platform.Prelude hiding (lift, lookup)

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

lookup :: (Applicative f) => (Maybe Text, Text) -> RequestingOid f (Word32, Word32)
lookup = LookingUp.lookup

lookingUp :: (Applicative f) => (Maybe Text, Text) -> ((Word32, Word32) -> f a) -> RequestingOid f a
lookingUp = LookingUp.lookingUp

hoistLookingUp :: (Applicative f) => (Maybe Text, Text) -> ((Word32, Word32) -> f a -> g b) -> RequestingOid f a -> RequestingOid g b
hoistLookingUp = LookingUp.hoistLookingUp
