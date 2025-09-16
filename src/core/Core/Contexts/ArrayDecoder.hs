module Core.Contexts.ArrayDecoder where

import Core.PostgresTypeInfo qualified as PTI
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A

data ArrayDecoder a
  = ArrayDecoder 
      -- | Type name for the array element
      Text
      -- | Statically known OID for the element type.
      (Maybe PTI.OID)
      -- | Decoding function
      (A.Array a)
  deriving (Functor)

{-# INLINE run #-}
run :: ArrayDecoder a -> A.Value a
run (ArrayDecoder _ _ imp) =
  A.array imp

{-# INLINE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> ArrayDecoder a -> ArrayDecoder b
dimension replicateM (ArrayDecoder typeName typeOID imp) =
  ArrayDecoder typeName typeOID $ A.dimensionArray replicateM imp

{-# INLINE value #-}
value :: A.Value a -> ArrayDecoder (Maybe a)
value decoder' =
  ArrayDecoder "unknown" Nothing $ A.nullableValueArray decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: A.Value a -> ArrayDecoder a
nonNullValue decoder' =
  ArrayDecoder "unknown" Nothing $ A.valueArray decoder'

-- | Get the type name for the array based on element type name
{-# INLINE toTypeName #-}
toTypeName :: ArrayDecoder a -> Text
toTypeName (ArrayDecoder elementTypeName _ _) = elementTypeName <> "[]"

-- | Get the array OID based on element type OID
{-# INLINE toOid #-}
toOid :: ArrayDecoder a -> Maybe PTI.OID
toOid (ArrayDecoder _ Nothing _) = Nothing
toOid (ArrayDecoder _ (Just elementOID) _) = 
  PTI.lookupArrayOidFromElement elementOID
