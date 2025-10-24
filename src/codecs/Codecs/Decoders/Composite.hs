module Codecs.Decoders.Composite where

import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Platform.LookingUp qualified as LookingUp
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (RequestingOid.RequestingOid Binary.Composite a)
  deriving newtype
    (Functor, Applicative)

toValueDecoder :: Composite a -> RequestingOid.RequestingOid Binary.Value a
toValueDecoder (Composite imp) =
  RequestingOid.hoist Binary.composite imp

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just expectedOid ->
        -- Static OID known, create decoder with OID check
        Composite (RequestingOid.hoist (checkedValueComposite expectedOid) (Value.toDecoder valueDecoder))
      Nothing ->
        -- Dynamic OID, need to look it up and check
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
            innerDecoder = Value.toDecoder valueDecoder
            unknownTypes = HashSet.insert (schema, typeName) (RequestingOid.toUnknownTypes innerDecoder)
         in Composite $
              LookingUp
                (toList unknownTypes)
                ( \lookupFn ->
                    let oidCache = HashMap.fromList [(key, lookupFn key) | key <- toList unknownTypes]
                        (expectedOid, _arrayOid) = lookupFn (schema, typeName)
                        binaryDecoder = RequestingOid.toBase innerDecoder oidCache
                     in checkedValueComposite expectedOid binaryDecoder
                )
  NullableOrNot.Nullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just expectedOid ->
        -- Static OID known, create decoder with OID check
        Composite (RequestingOid.hoist (checkedNullableValueComposite expectedOid) (Value.toDecoder valueDecoder))
      Nothing ->
        -- Dynamic OID, need to look it up and check
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
            innerDecoder = Value.toDecoder valueDecoder
            unknownTypes = HashSet.insert (schema, typeName) (RequestingOid.toUnknownTypes innerDecoder)
         in Composite $
              LookingUp
                (toList unknownTypes)
                ( \lookupFn ->
                    let oidCache = HashMap.fromList [(key, lookupFn key) | key <- toList unknownTypes]
                        (expectedOid, _arrayOid) = lookupFn (schema, typeName)
                        binaryDecoder = RequestingOid.toBase innerDecoder oidCache
                     in checkedNullableValueComposite expectedOid binaryDecoder
                )

-- |
-- Value composite decoder with OID checking.
checkedValueComposite :: Word32 -> Binary.Value a -> Binary.Composite a
checkedValueComposite expectedOid decoder =
  Binary.Composite \actualOidWord32 lenWord32 bytesPtr nullBool ->
    if actualOidWord32 == expectedOid
      then Binary.valueComposite decoder actualOidWord32 lenWord32 bytesPtr nullBool
      else Left ("Composite field OID mismatch. Expected: " <> fromString (show expectedOid) <> ", got: " <> fromString (show actualOidWord32))

-- |
-- Nullable value composite decoder with OID checking.
checkedNullableValueComposite :: Word32 -> Binary.Value a -> Binary.Composite (Maybe a)
checkedNullableValueComposite expectedOid decoder =
  Binary.Composite \actualOidWord32 lenWord32 bytesPtr nullBool ->
    if actualOidWord32 == expectedOid
      then Binary.nullableValueComposite decoder actualOidWord32 lenWord32 bytesPtr nullBool
      else Left ("Composite field OID mismatch. Expected: " <> fromString (show expectedOid) <> ", got: " <> fromString (show actualOidWord32))
