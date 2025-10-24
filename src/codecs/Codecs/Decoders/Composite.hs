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
      Just _expectedOid ->
        -- Static OID known - TODO: implement OID checking
        -- For now, use the unchecked version
        Composite (RequestingOid.hoist Binary.valueComposite (Value.toDecoder valueDecoder))
      Nothing ->
        -- Dynamic OID - TODO: implement OID checking
        -- For now, use the unchecked version
        Composite (RequestingOid.hoist Binary.valueComposite (Value.toDecoder valueDecoder))
  NullableOrNot.Nullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just _expectedOid ->
        -- Static OID known - TODO: implement OID checking
        -- For now, use the unchecked version
        Composite (RequestingOid.hoist Binary.nullableValueComposite (Value.toDecoder valueDecoder))
      Nothing ->
        -- Dynamic OID - TODO: implement OID checking
        -- For now, use the unchecked version
        Composite (RequestingOid.hoist Binary.nullableValueComposite (Value.toDecoder valueDecoder))
