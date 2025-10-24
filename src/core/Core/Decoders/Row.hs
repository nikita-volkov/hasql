module Core.Decoders.Row where

import Codecs.Decoders
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hipq.RowDecoder qualified
import Platform.LookingUp qualified as LookingUp
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Decoder of an individual row,
-- which gets composed of column value decoders.
-- E.g.:
--
-- @
-- x :: 'Row' (Maybe Int64, Text, TimeOfDay)
-- x = (,,) '<$>' ('column' . 'nullable') 'int8' '<*>' ('column' . 'nonNullable') 'text' '<*>' ('column' . 'nonNullable') 'time'
-- @
newtype Row a
  = Row (RequestingOid.RequestingOid Hipq.RowDecoder.RowDecoder a)
  deriving newtype
    (Functor, Applicative)

toDecoder ::
  Row a ->
  RequestingOid.RequestingOid Hipq.RowDecoder.RowDecoder a
toDecoder (Row f) = f

-- |
-- Lift an individual value decoder to a composable row decoder.
{-# INLINEABLE column #-}
column :: NullableOrNot Value a -> Row a
column = \case
  Nullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just oid ->
        -- Static OID known, use it directly
        Row
          ( RequestingOid.hoist
              (Hipq.RowDecoder.nullableColumn (Just oid) . Binary.valueParser)
              (Value.toDecoder valueDecoder)
          )
      Nothing ->
        -- Dynamic OID, need to look it up
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
            innerDecoder = Value.toDecoder valueDecoder
            unknownTypes = HashSet.insert (schema, typeName) (RequestingOid.toUnknownTypes innerDecoder)
         in Row $
              LookingUp
                (toList unknownTypes)
                ( \lookupFn ->
                    let oidCache = HashMap.fromList [(key, lookupFn key) | key <- toList unknownTypes]
                        (baseOid, _arrayOid) = lookupFn (schema, typeName)
                        binaryDecoder = RequestingOid.toBase innerDecoder oidCache
                     in Hipq.RowDecoder.nullableColumn (Just baseOid) (Binary.valueParser binaryDecoder)
                )
  NonNullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just oid ->
        -- Static OID known, use it directly
        Row
          ( RequestingOid.hoist
              (Hipq.RowDecoder.nonNullableColumn (Just oid) . Binary.valueParser)
              (Value.toDecoder valueDecoder)
          )
      Nothing ->
        -- Dynamic OID, need to look it up
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
            innerDecoder = Value.toDecoder valueDecoder
            unknownTypes = HashSet.insert (schema, typeName) (RequestingOid.toUnknownTypes innerDecoder)
         in Row $
              LookingUp
                (toList unknownTypes)
                ( \lookupFn ->
                    let oidCache = HashMap.fromList [(key, lookupFn key) | key <- toList unknownTypes]
                        (baseOid, _arrayOid) = lookupFn (schema, typeName)
                        binaryDecoder = RequestingOid.toBase innerDecoder oidCache
                     in Hipq.RowDecoder.nonNullableColumn (Just baseOid) (Binary.valueParser binaryDecoder)
                )
