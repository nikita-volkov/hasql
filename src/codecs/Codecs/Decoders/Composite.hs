module Codecs.Decoders.Composite where

import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Data.ByteString qualified as ByteString
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary
import TextBuilder qualified

-- |
-- Composable decoder of composite values (rows, records).
-- 
-- This is a parser for individual fields within a composite type.
-- Unlike the postgresql-binary version, this implementation checks OIDs
-- to ensure type safety.
newtype Composite a
  = Composite (Int -> RequestingOid.RequestingOid Binary.Value a)
  deriving (Functor)

instance Applicative Composite where
  pure x = Composite (\_ -> pure x)
  Composite ff <*> Composite fx = Composite \fieldIndex -> do
    f <- ff fieldIndex
    x <- fx (fieldIndex + 1)
    pure (f x)

-- |
-- Convert a Composite decoder to a Value decoder.
-- This adds the composite header parsing (number of fields).
toValueDecoder :: Composite a -> RequestingOid.RequestingOid Binary.Value a
toValueDecoder (Composite decoder) =
  RequestingOid.hoist addCompositeHeader (decoder 0)
  where
    addCompositeHeader valueDecoder =
      Binary.fn \bytes ->
        case Binary.valueParser parser bytes of
          Left err -> Left err
          Right val -> Right val
      where
        parser = do
          -- Read number of fields (we don't validate this, just skip it)
          _numFields <- Binary.int :: Binary.Value Int32
          -- Now parse the actual value
          valueDecoder

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable imp ->
    Composite \_fieldIndex ->
      case Value.toOid imp of
        Just expectedOid ->
          RequestingOid.hoist (parseNonNullFieldWithOidCheck expectedOid) (Value.toDecoder imp)
        Nothing ->
          RequestingOid.hoistLookingUp
            (Value.toSchema imp, Value.toTypeName imp)
            (\lookupResult -> parseNonNullFieldWithOidCheck (chooseLookedUpOid imp lookupResult))
            (Value.toDecoder imp)
  NullableOrNot.Nullable imp ->
    Composite \_fieldIndex ->
      case Value.toOid imp of
        Just expectedOid ->
          RequestingOid.hoist (parseNullableFieldWithOidCheck expectedOid) (Value.toDecoder imp)
        Nothing ->
          RequestingOid.hoistLookingUp
            (Value.toSchema imp, Value.toTypeName imp)
            (\lookupResult -> parseNullableFieldWithOidCheck (chooseLookedUpOid imp lookupResult))
            (Value.toDecoder imp)
  where
    chooseLookedUpOid valueDecoder (elementOid, arrayOid) =
      if Value.toDimensionality valueDecoder > 0
        then arrayOid
        else elementOid

-- |
-- Parse a non-nullable composite field with OID checking.
parseNonNullFieldWithOidCheck :: Word32 -> Binary.Value a -> Binary.Value a
parseNonNullFieldWithOidCheck expectedOid valueDecoder =
  Binary.fn \bytes ->
    case Binary.valueParser parser bytes of
      Left err -> Left err
      Right val -> Right val
  where
    parser = do
      -- Read and check OID
      actualOid <- Binary.int :: Binary.Value Word32
      when (actualOid /= expectedOid) do
        fail
          $ "OID mismatch: expected "
          <> show expectedOid
          <> " but got "
          <> show actualOid
      
      -- Read size
      size <- Binary.int :: Binary.Value Int32
      
      -- Parse data based on size
      case size of
        (-1) -> fail "Unexpected NULL"
        n -> isolate (fromIntegral n) valueDecoder
    
    -- Helper to parse a limited number of bytes
    isolate n decoder =
      Binary.fn \allBytes ->
        let (bytes, _rest) = ByteString.splitAt n allBytes
         in Binary.valueParser decoder bytes

-- |
-- Parse a nullable composite field with OID checking.
parseNullableFieldWithOidCheck :: Word32 -> Binary.Value a -> Binary.Value (Maybe a)
parseNullableFieldWithOidCheck expectedOid valueDecoder =
  Binary.fn \bytes ->
    case Binary.valueParser parser bytes of
      Left err -> Left err
      Right val -> Right val
  where
    parser = do
      -- Read and check OID
      actualOid <- Binary.int :: Binary.Value Word32
      when (actualOid /= expectedOid) do
        fail
          $ "OID mismatch: expected "
          <> show expectedOid
          <> " but got "
          <> show actualOid
      
      -- Read size
      size <- Binary.int :: Binary.Value Int32
      
      -- Parse data based on size
      case size of
        (-1) -> pure Nothing
        n -> Just <$> isolate (fromIntegral n) valueDecoder
    
    -- Helper to parse a limited number of bytes
    isolate n decoder =
      Binary.fn \allBytes ->
        let (bytes, _rest) = ByteString.splitAt n allBytes
         in Binary.valueParser decoder bytes
