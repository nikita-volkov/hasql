module Core.Decoders.Row where

import Codecs.Decoders
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Data.Text qualified as Text
import Hipq.RowDecoder qualified
import Platform.LookingUp qualified
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
    let staticOid = Value.toBaseOid valueDecoder
     in case staticOid of
          Just _ ->
            -- Static OID known, use it directly
            Row
              ( RequestingOid.hoist
                  (Hipq.RowDecoder.nullableColumn staticOid . Binary.valueParser)
                  (Value.toDecoder valueDecoder)
              )
          Nothing ->
            -- Dynamic OID, need to look it up
            let schema = Value.toSchema valueDecoder
                typeName = Value.toTypeName valueDecoder
                valueDecoder' = Value.toDecoder valueDecoder
                -- For arrays, we need to look up the element type and use its array OID
                (lookupKey, useArrayOid) = parseTypeName typeName
             in case valueDecoder' of
                  Platform.LookingUp.LookingUp keys use ->
                    Row $ Platform.LookingUp.LookingUp ((schema, lookupKey) : keys) $ \lookupFn ->
                      let (baseOid, arrayOid) = lookupFn (schema, lookupKey)
                          oid = if useArrayOid then arrayOid else baseOid
                          binaryDecoder = use lookupFn
                       in Hipq.RowDecoder.nullableColumn (Just oid) (Binary.valueParser binaryDecoder)
  NonNullable valueDecoder ->
    let staticOid = Value.toBaseOid valueDecoder
     in case staticOid of
          Just _ ->
            -- Static OID known, use it directly
            Row
              ( RequestingOid.hoist
                  (Hipq.RowDecoder.nonNullableColumn staticOid . Binary.valueParser)
                  (Value.toDecoder valueDecoder)
              )
          Nothing ->
            -- Dynamic OID, need to look it up
            let schema = Value.toSchema valueDecoder
                typeName = Value.toTypeName valueDecoder
                valueDecoder' = Value.toDecoder valueDecoder
                -- For arrays, we need to look up the element type and use its array OID
                (lookupKey, useArrayOid) = parseTypeName typeName
             in case valueDecoder' of
                  Platform.LookingUp.LookingUp keys use ->
                    Row $ Platform.LookingUp.LookingUp ((schema, lookupKey) : keys) $ \lookupFn ->
                      let (baseOid, arrayOid) = lookupFn (schema, lookupKey)
                          oid = if useArrayOid then arrayOid else baseOid
                          binaryDecoder = use lookupFn
                       in Hipq.RowDecoder.nonNullableColumn (Just oid) (Binary.valueParser binaryDecoder)

-- | Parse a type name to determine if it's an array and extract the base type name.
-- Returns (base type name, whether to use array OID).
-- For "mytype", returns ("mytype", False)
-- For "mytype[]", returns ("mytype", True)
-- For "mytype[][]", returns ("mytype[]", True) - use the array of array
parseTypeName :: Text -> (Text, Bool)
parseTypeName typeName =
  case Text.stripSuffix "[]" typeName of
    Just baseType -> (baseType, True)
    Nothing -> (typeName, False)
