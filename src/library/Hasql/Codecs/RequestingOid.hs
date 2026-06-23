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

import Hasql.Codecs.RequestingOid.LookingUp qualified as LookingUp
import Hasql.Codecs.Vocab qualified as Vocab
import Hasql.Codecs.Vocab.OidCache qualified as Vocab.OidCache
import Hasql.Codecs.Vocab.TypeInfo qualified as Vocab.TypeInfo
import Hasql.Platform.Prelude hiding (lift, lookup)

type RequestingOid =
  LookingUp.LookingUp
    Vocab.QualifiedTypeName
    Vocab.TypeInfo.TypeInfo

{-# INLINE toUnknownTypes #-}
toUnknownTypes ::
  RequestingOid a ->
  HashSet Vocab.QualifiedTypeName
toUnknownTypes (LookingUp.LookingUp unknownTypes _) =
  fromList unknownTypes

{-# INLINE toBase #-}
toBase ::
  RequestingOid a ->
  Vocab.OidCache ->
  a
toBase (LookingUp.LookingUp _unknownTypes decoder) oidCache =
  decoder \key ->
    Vocab.OidCache.lookupTypeInfo key oidCache
      & fromMaybe (Vocab.TypeInfo.TypeInfo 0 0)

{-# INLINE requestAndHandle #-}
requestAndHandle ::
  [Vocab.QualifiedTypeName] ->
  ((Vocab.QualifiedTypeName -> Vocab.TypeInfo.TypeInfo) -> a) ->
  RequestingOid a
requestAndHandle keys fn = LookingUp.LookingUp keys fn

{-# INLINE lift #-}
lift :: a -> RequestingOid a
lift = LookingUp.lift

{-# INLINE hoist #-}
hoist :: (a -> b) -> RequestingOid a -> RequestingOid b
hoist fn (LookingUp.LookingUp keys use) = LookingUp.LookingUp keys (fn . use)

{-# INLINE lookup #-}
lookup :: Vocab.QualifiedTypeName -> RequestingOid Vocab.TypeInfo.TypeInfo
lookup = LookingUp.lookup

{-# INLINE lookingUp #-}
lookingUp :: Vocab.QualifiedTypeName -> (Vocab.TypeInfo.TypeInfo -> a) -> RequestingOid a
lookingUp = LookingUp.lookingUp

{-# INLINE hoistLookingUp #-}
hoistLookingUp :: Vocab.QualifiedTypeName -> (Vocab.TypeInfo.TypeInfo -> a -> b) -> RequestingOid a -> RequestingOid b
hoistLookingUp = LookingUp.hoistLookingUp
