module Codecs.Decoders.Composite where

import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Platform.LookingUp qualified as LookingUp
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
-- This wraps field decoders that will be composed into a complete composite decoder.
data Composite a
  = Composite
      -- Expected field OIDs (Nothing if OID not statically known)
      [Maybe Word32]
      -- Field decoder function wrapped for RequestingOid compatibility
      (RequestingOid.RequestingOid FieldsDecoder a)
  deriving stock (Functor)

-- Newtype wrapper for field decoder function to make it work with RequestingOid
-- This has kind (* -> *) as required by RequestingOid
newtype FieldsDecoder a = FieldsDecoder {runFieldsDecoder :: [ByteString] -> Either Text a}
  deriving stock (Functor)

-- Note: This Applicative instance doesn't split fields - that's handled by the Composite Applicative
instance Applicative FieldsDecoder where
  pure a = FieldsDecoder (\_fields -> Right a)
  FieldsDecoder f <*> FieldsDecoder a = FieldsDecoder \fields -> f fields <*> a fields

instance Applicative Composite where
  pure a = Composite [] (RequestingOid.lift (FieldsDecoder (\_fields -> Right a)))
  Composite lOids lDec <*> Composite rOids rDec =
    let combinedOids = lOids <> rOids
        -- Manually combine RequestingOids by merging their unknown types and combining decoders
        RequestingOid.LookingUp lKeys lFn = lDec
        RequestingOid.LookingUp rKeys rFn = rDec
        combinedDec = RequestingOid.LookingUp
          (lKeys <> rKeys)
          (\lookup ->
            let FieldsDecoder lDecoder = lFn lookup
                FieldsDecoder rDecoder = rFn lookup
             in FieldsDecoder \fieldData ->
                  let (lFields, rFields) = splitAt (length lOids) fieldData
                   in lDecoder lFields <*> rDecoder rFields)
     in Composite combinedOids combinedDec

toValueDecoder :: Composite a -> RequestingOid.RequestingOid Binary.Value a
toValueDecoder (Composite expectedOids decoder) =
  RequestingOid.hoist (customCompositeParser expectedOids) decoder
  where
    -- Create a custom parser that reads the composite binary format and checks OIDs
    customCompositeParser :: [Maybe Word32] -> FieldsDecoder a -> Binary.Value a
    customCompositeParser oids (FieldsDecoder fieldDecoder) =
      Binary.fn \bytes -> do
        -- Parse composite format: int32 numFields, then for each field: int32 OID, int32 length, bytes data
        fields <- parseCompositeStructure bytes oids
        fieldDecoder fields

-- Parse the PostgreSQL composite binary format
parseCompositeStructure :: ByteString -> [Maybe Word32] -> Either Text [ByteString]
parseCompositeStructure bytes expectedOids = do
  -- Read number of fields
  when (BS.length bytes < 4) $
    Left "Composite data too short to contain field count"
  let numFields = readInt32BE (BS.take 4 bytes)
  when (numFields /= fromIntegral (length expectedOids)) $
    Left $ "Composite field count mismatch. Expected " <> fromString (show (length expectedOids)) <> " fields, got " <> fromString (show numFields)
  
  -- Parse each field
  parseFields (BS.drop 4 bytes) expectedOids 0

parseFields :: ByteString -> [Maybe Word32] -> Int -> Either Text [ByteString]
parseFields _ [] _ = Right []
parseFields bytes (expectedOid : rest) fieldIndex = do
  when (BS.length bytes < 8) $
    Left $ "Composite data too short for field " <> fromString (show fieldIndex)
  
  let actualOid = readInt32BE (BS.take 4 bytes)
      fieldLen = readInt32BE (BS.take 4 (BS.drop 4 bytes))
  
  -- Check OID if we have an expected value
  case expectedOid of
    Just expected | fromIntegral actualOid /= expected ->
      Left $ "Composite field " <> fromString (show fieldIndex) <> " OID mismatch. Expected " <> fromString (show expected) <> ", got " <> fromString (show actualOid)
    _ -> pure ()
  
  -- Handle NULL field (length = -1)
  if fieldLen == -1
    then do
      rest' <- parseFields (BS.drop 8 bytes) rest (fieldIndex + 1)
      Right (BS.empty : rest')
    else do
      when (BS.length bytes < 8 + fromIntegral fieldLen) $
        Left $ "Composite data too short for field " <> fromString (show fieldIndex) <> " data"
      let fieldData = BS.take (fromIntegral fieldLen) (BS.drop 8 bytes)
          remaining = BS.drop (8 + fromIntegral fieldLen) bytes
      rest' <- parseFields remaining rest (fieldIndex + 1)
      Right (fieldData : rest')

-- Read a big-endian int32 from ByteString
readInt32BE :: ByteString -> Int32
readInt32BE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Int32
      b1 = fromIntegral (BS.index bs 1) :: Int32
      b2 = fromIntegral (BS.index bs 2) :: Int32
      b3 = fromIntegral (BS.index bs 3) :: Int32
   in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just expectedOid ->
        -- Static OID known
        Composite
          [Just expectedOid]
          ( RequestingOid.hoist
              ( \valueParser -> FieldsDecoder \fieldData -> case fieldData of
                  [bytes] -> Binary.valueParser valueParser bytes
                  _ -> Left "Expected exactly one field"
              )
              (Value.toDecoder valueDecoder)
          )
      Nothing ->
        -- Dynamic OID, need to look it up
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
            innerDecoder = Value.toDecoder valueDecoder
            unknownTypes = HashSet.insert (schema, typeName) (RequestingOid.toUnknownTypes innerDecoder)
         in Composite
              [Nothing] -- Will be resolved when OID cache is available
              $ LookingUp
                (toList unknownTypes)
                ( \lookupFn -> FieldsDecoder \fieldData -> case fieldData of
                    [bytes] ->
                      let oidCache = HashMap.fromList [(key, lookupFn key) | key <- toList unknownTypes]
                          binaryDecoder = RequestingOid.toBase innerDecoder oidCache
                       in Binary.valueParser binaryDecoder bytes
                    _ -> Left "Expected exactly one field"
                )
  NullableOrNot.Nullable valueDecoder ->
    case Value.toBaseOid valueDecoder of
      Just expectedOid ->
        -- Static OID known
        Composite
          [Just expectedOid]
          ( RequestingOid.hoist
              ( \valueParser -> FieldsDecoder \fieldData -> case fieldData of
                  [bytes]
                    | BS.null bytes -> Right Nothing
                    | otherwise -> Just <$> Binary.valueParser valueParser bytes
                  _ -> Left "Expected exactly one field"
              )
              (Value.toDecoder valueDecoder)
          )
      Nothing ->
        -- Dynamic OID, need to look it up
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
            innerDecoder = Value.toDecoder valueDecoder
            unknownTypes = HashSet.insert (schema, typeName) (RequestingOid.toUnknownTypes innerDecoder)
         in Composite
              [Nothing] -- Will be resolved when OID cache is available
              $ LookingUp
                (toList unknownTypes)
                ( \lookupFn -> FieldsDecoder \fieldData -> case fieldData of
                    [bytes]
                      | BS.null bytes -> Right Nothing
                      | otherwise ->
                          let oidCache = HashMap.fromList [(key, lookupFn key) | key <- toList unknownTypes]
                              binaryDecoder = RequestingOid.toBase innerDecoder oidCache
                           in Just <$> Binary.valueParser binaryDecoder bytes
                    _ -> Left "Expected exactly one field"
                )
