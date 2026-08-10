module Hasql.CodecsCore.RequestingOid
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

import Hasql.CodecsCore qualified as CodecsCore
import Hasql.CodecsCore.RequestingOid.LookingUp qualified as LookingUp
import Hasql.CodecsCore.TypeInfo qualified as CodecsCore.TypeInfo
import Hasql.Platform.Prelude hiding (lift, lookup)

type RequestingOid =
  LookingUp.LookingUp
    CodecsCore.QualifiedTypeName
    CodecsCore.TypeInfo.TypeInfo

{-# INLINE toUnknownTypes #-}
toUnknownTypes ::
  RequestingOid a ->
  HashSet CodecsCore.QualifiedTypeName
toUnknownTypes (LookingUp.LookingUp unknownTypes _) =
  fromList unknownTypes

{-# INLINE toBase #-}
toBase ::
  RequestingOid a ->
  (CodecsCore.QualifiedTypeName -> CodecsCore.TypeInfo.TypeInfo) ->
  a
toBase (LookingUp.LookingUp _unknownTypes decoder) resolve =
  decoder resolve

{-# INLINE requestAndHandle #-}
requestAndHandle ::
  [CodecsCore.QualifiedTypeName] ->
  ((CodecsCore.QualifiedTypeName -> CodecsCore.TypeInfo.TypeInfo) -> a) ->
  RequestingOid a
requestAndHandle keys fn = LookingUp.LookingUp keys fn

{-# INLINE lift #-}
lift :: a -> RequestingOid a
lift = LookingUp.lift

{-# INLINE hoist #-}
hoist :: (a -> b) -> RequestingOid a -> RequestingOid b
hoist fn (LookingUp.LookingUp keys use) = LookingUp.LookingUp keys (fn . use)

{-# INLINE lookup #-}
lookup :: CodecsCore.QualifiedTypeName -> RequestingOid CodecsCore.TypeInfo.TypeInfo
lookup = LookingUp.lookup

{-# INLINE lookingUp #-}
lookingUp :: CodecsCore.QualifiedTypeName -> (CodecsCore.TypeInfo.TypeInfo -> a) -> RequestingOid a
lookingUp = LookingUp.lookingUp

{-# INLINE hoistLookingUp #-}
hoistLookingUp :: CodecsCore.QualifiedTypeName -> (CodecsCore.TypeInfo.TypeInfo -> a -> b) -> RequestingOid a -> RequestingOid b
hoistLookingUp = LookingUp.hoistLookingUp
