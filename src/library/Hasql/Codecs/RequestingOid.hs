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
import Hasql.Kernel qualified as Kernel
import Hasql.Kernel.TypeInfo qualified as Kernel.TypeInfo
import Hasql.Platform.Prelude hiding (lift, lookup)

type RequestingOid =
  LookingUp.LookingUp
    Kernel.QualifiedTypeName
    Kernel.TypeInfo.TypeInfo

{-# INLINE toUnknownTypes #-}
toUnknownTypes ::
  RequestingOid a ->
  HashSet Kernel.QualifiedTypeName
toUnknownTypes (LookingUp.LookingUp unknownTypes _) =
  fromList unknownTypes

{-# INLINE toBase #-}
toBase ::
  RequestingOid a ->
  HashMap Kernel.QualifiedTypeName Kernel.TypeInfo.TypeInfo ->
  a
toBase (LookingUp.LookingUp _unknownTypes decoder) oidCache =
  decoder \key ->
    HashMap.lookup key oidCache
      & fromMaybe (Kernel.TypeInfo.TypeInfo 0 0)

{-# INLINE requestAndHandle #-}
requestAndHandle ::
  [Kernel.QualifiedTypeName] ->
  ((Kernel.QualifiedTypeName -> Kernel.TypeInfo.TypeInfo) -> a) ->
  RequestingOid a
requestAndHandle keys fn = LookingUp.LookingUp keys fn

{-# INLINE lift #-}
lift :: a -> RequestingOid a
lift = LookingUp.lift

{-# INLINE hoist #-}
hoist :: (a -> b) -> RequestingOid a -> RequestingOid b
hoist fn (LookingUp.LookingUp keys use) = LookingUp.LookingUp keys (fn . use)

{-# INLINE lookup #-}
lookup :: Kernel.QualifiedTypeName -> RequestingOid Kernel.TypeInfo.TypeInfo
lookup = LookingUp.lookup

{-# INLINE lookingUp #-}
lookingUp :: Kernel.QualifiedTypeName -> (Kernel.TypeInfo.TypeInfo -> a) -> RequestingOid a
lookingUp = LookingUp.lookingUp

{-# INLINE hoistLookingUp #-}
hoistLookingUp :: Kernel.QualifiedTypeName -> (Kernel.TypeInfo.TypeInfo -> a -> b) -> RequestingOid a -> RequestingOid b
hoistLookingUp = LookingUp.hoistLookingUp
