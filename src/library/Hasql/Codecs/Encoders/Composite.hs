module Hasql.Codecs.Encoders.Composite where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Encoders.Value qualified as Value
import Hasql.Platform.Prelude hiding (bool)
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- |
-- Composite or row-types encoder.
data Composite a
  = Composite
      -- | Names of types that are not known statically and must be looked up at runtime collected from the nested composite and array encoders.
      (HashSet (Maybe Text, Text))
      -- | Serialization function given the dictionary of resolved OIDs.
      (HashMap (Maybe Text, Text) (Word32, Word32) -> a -> Binary.Composite)
      -- | Render function for error messages.
      (a -> [TextBuilder.TextBuilder])

instance Contravariant Composite where
  contramap f (Composite unknownTypes encode print) =
    Composite unknownTypes (\oidCache -> encode oidCache . f) (print . f)

instance Divisible Composite where
  divide f (Composite unknownTypesL encodeL printL) (Composite unknownTypesR encodeR printR) =
    Composite
      (unknownTypesL <> unknownTypesR)
      (\oidCache val -> case f val of (lVal, rVal) -> encodeL oidCache lVal <> encodeR oidCache rVal)
      (\val -> case f val of (lVal, rVal) -> printL lVal <> printR rVal)
  conquer = mempty

instance Semigroup (Composite a) where
  Composite unknownTypesL encodeL printL <> Composite unknownTypesR encodeR printR =
    Composite
      (unknownTypesL <> unknownTypesR)
      (\oidCache val -> encodeL oidCache val <> encodeR oidCache val)
      (\val -> printL val <> printR val)

instance Monoid (Composite a) where
  mempty = Composite mempty mempty mempty

-- | Single field of a row-type.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable (Value.Value schemaName typeName scalarOid arrayOid dimensionality unknownTypes encode print) ->
    let staticOid = if dimensionality == 0 then scalarOid else arrayOid
     in case staticOid of
          Just oid ->
            Composite
              unknownTypes
              (\oidCache val -> Binary.field oid (encode oidCache val))
              (\val -> [print val])
          Nothing ->
            Composite
              (HashSet.insert (schemaName, typeName) unknownTypes)
              ( \oidCache val ->
                  let oid = if dimensionality == 0 then maybe 0 fst (HashMap.lookup (schemaName, typeName) oidCache) else maybe 0 snd (HashMap.lookup (schemaName, typeName) oidCache)
                   in Binary.field oid (encode oidCache val)
              )
              (\val -> [print val])
  NullableOrNot.Nullable (Value.Value schemaName typeName scalarOid arrayOid dimensionality unknownTypes encode print) ->
    let staticOid = if dimensionality == 0 then scalarOid else arrayOid
     in case staticOid of
          Just oid ->
            Composite
              unknownTypes
              ( \oidCache -> \case
                  Nothing -> Binary.nullField oid
                  Just val -> Binary.field oid (encode oidCache val)
              )
              ( \case
                  Nothing -> ["NULL"]
                  Just val -> [print val]
              )
          Nothing ->
            Composite
              (HashSet.insert (schemaName, typeName) unknownTypes)
              ( \oidCache -> \case
                  Nothing ->
                    let oid = if dimensionality == 0 then maybe 0 fst (HashMap.lookup (schemaName, typeName) oidCache) else maybe 0 snd (HashMap.lookup (schemaName, typeName) oidCache)
                     in Binary.nullField oid
                  Just val ->
                    let oid = if dimensionality == 0 then maybe 0 fst (HashMap.lookup (schemaName, typeName) oidCache) else maybe 0 snd (HashMap.lookup (schemaName, typeName) oidCache)
                     in Binary.field oid (encode oidCache val)
              )
              ( \case
                  Nothing -> ["NULL"]
                  Just val -> [print val]
              )
