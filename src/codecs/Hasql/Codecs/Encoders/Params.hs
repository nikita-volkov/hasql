module Hasql.Codecs.Encoders.Params
  ( Params,
    noParams,
    param,
    toColumnsMetadata,
    toUnknownTypes,
    toSerializer,
    toPrinter,
  )
where

import Data.HashSet qualified as HashSet
import Data.Vector qualified as Vector
import Hasql.Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Encoders.Value qualified as Value
import Hasql.Codecs.Vocab qualified as Vocab
import Hasql.Codecs.Vocab.OidCache qualified as Vocab.OidCache
import Hasql.Codecs.Vocab.ParamMeta (ParamMeta (..))
import Hasql.Codecs.Vocab.QualifiedTypeName qualified as Vocab.QualifiedTypeName
import Hasql.Codecs.Vocab.TypeRef qualified as Vocab.TypeRef
import Hasql.Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- | Frozen per-parameter metadata: type reference, dimensionality, text-format flag.
toColumnsMetadata :: Params a -> Vector ParamMeta
toColumnsMetadata (Params _ _ columnsMetadata _ _) = freezeColumnsMetadata columnsMetadata
  where
    freezeColumnsMetadata =
      Vector.fromList . toList

toUnknownTypes :: Params a -> HashSet Vocab.QualifiedTypeName
toUnknownTypes (Params _ unknownTypes _ _ _) =
  unknownTypes

-- | Serialise params to encoded wire values given a resolved OID cache.
toSerializer :: Params a -> Vocab.OidCache -> a -> [Maybe ByteString]
toSerializer (Params _ _ _ serializer _) = serializer

-- | Render params in human-readable form (for error reporting).
toPrinter :: Params a -> a -> [Text]
toPrinter (Params _ _ _ _ printer) = toList . printer

-- |
-- Encoder of some representation of a parameters product.
--
-- Has instances of 'Contravariant', 'Divisible' and 'Monoid',
-- which you can use to compose multiple parameters together.
-- E.g.,
--
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   ('fst' '>$<' 'param' ('nonNullable' 'int8')) '<>'
--   ('snd' '>$<' 'param' ('nullable' 'text'))
-- @
--
-- As a general solution for tuples of any arity, instead of 'fst' and 'snd',
-- consider the functions of the @contrazip@ family
-- from the "contravariant-extras" package.
-- E.g., here's how you can achieve the same as the above:
--
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   'contrazip2' ('param' ('nonNullable' 'int8')) ('param' ('nullable' 'text'))
-- @
--
-- Here's how you can implement encoders for custom composite types:
--
-- @
-- data Person = Person { name :: Text, gender :: Gender, age :: Int }
--
-- data Gender = Male | Female
--
-- personParams :: 'Params' Person
-- personParams =
--   (name '>$<' 'param' ('nonNullable' 'text')) '<>'
--   (gender '>$<' 'param' ('nonNullable' genderValue)) '<>'
--   ('fromIntegral' . age '>$<' 'param' ('nonNullable' 'int8'))
--
-- genderValue :: 'Value.Value' Gender
-- genderValue = 'enum' Nothing (Just "gender") genderText where
--   genderText gender = case gender of
--     Male -> "male"
--     Female -> "female"
-- @
data Params a = Params
  { size :: Int,
    unknownTypes :: HashSet Vocab.QualifiedTypeName,
    -- | (Type reference, dimensionality, Text Format) for each parameter.
    columnsMetadata :: DList ParamMeta,
    serializer :: Vocab.OidCache -> a -> [Maybe ByteString],
    printer :: a -> DList Text
  }

instance Contravariant Params where
  contramap fn (Params size unknownTypes columnsMetadata oldSerializer oldPrinter) = Params {..}
    where
      serializer oidCache = oldSerializer oidCache . fn
      printer = oldPrinter . fn

instance Divisible Params where
  divide
    divisor
    (Params leftSize leftUnknownTypes leftColumnsMetadata leftSerializer leftPrinter)
    (Params rightSize rightUnknownTypes rightColumnsMetadata rightSerializer rightPrinter) =
      Params
        { size = leftSize + rightSize,
          unknownTypes = leftUnknownTypes <> rightUnknownTypes,
          columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
          serializer = \oidCache input -> case divisor input of
            (leftInput, rightInput) -> leftSerializer oidCache leftInput <> rightSerializer oidCache rightInput,
          printer = \input -> case divisor input of
            (leftInput, rightInput) -> leftPrinter leftInput <> rightPrinter rightInput
        }
  conquer =
    Params
      { size = 0,
        unknownTypes = mempty,
        columnsMetadata = mempty,
        serializer = mempty,
        printer = mempty
      }

instance Semigroup (Params a) where
  Params leftSize leftUnknownTypes leftColumnsMetadata leftSerializer leftPrinter <> Params rightSize rightUnknownTypes rightColumnsMetadata rightSerializer rightPrinter =
    Params
      { size = leftSize + rightSize,
        unknownTypes = leftUnknownTypes <> rightUnknownTypes,
        columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
        serializer = \oidCache input -> leftSerializer oidCache input <> rightSerializer oidCache input,
        printer = \input -> leftPrinter input <> rightPrinter input
      }

instance Monoid (Params a) where
  mempty = conquer

value :: Value.Value a -> Params a
value (Value.Value schemaName typeName scalarOid arrayOid dimensionality textFormat unknownTypes serialize print) =
  let staticOid = if dimensionality == 0 then scalarOid else arrayOid
      serializer oidCache = pure . Just . Binary.encodingBytes . serialize (Vocab.OidCache.toHashMap oidCache)
      printer = pure . TextBuilder.toText . print
      size = 1
   in case staticOid of
        Just oid ->
          Params
            { size,
              unknownTypes,
              columnsMetadata = pure (ParamMeta (Vocab.TypeRef.KnownOid oid) dimensionality textFormat),
              serializer,
              printer
            }
        Nothing ->
          Params
            { size,
              unknownTypes = HashSet.insert (Vocab.QualifiedTypeName.QualifiedTypeName schemaName typeName) unknownTypes,
              columnsMetadata = pure (ParamMeta (Vocab.TypeRef.NamedType (Vocab.QualifiedTypeName.QualifiedTypeName schemaName typeName)) dimensionality textFormat),
              serializer,
              printer
            }

nullableValue :: Value.Value a -> Params (Maybe a)
nullableValue (Value.Value schemaName typeName scalarOid arrayOid dimensionality textFormat unknownTypes serialize print) =
  let staticOid = if dimensionality == 0 then scalarOid else arrayOid
      serializer oidCache = pure . fmap (Binary.encodingBytes . serialize (Vocab.OidCache.toHashMap oidCache))
      printer = pure . maybe "null" (TextBuilder.toText . print)
      size = 1
   in case staticOid of
        Just oid ->
          Params
            { size,
              unknownTypes,
              columnsMetadata = pure (ParamMeta (Vocab.TypeRef.KnownOid oid) dimensionality textFormat),
              serializer,
              printer
            }
        Nothing ->
          Params
            { size,
              unknownTypes = HashSet.insert (Vocab.QualifiedTypeName.QualifiedTypeName schemaName typeName) unknownTypes,
              columnsMetadata = pure (ParamMeta (Vocab.TypeRef.NamedType (Vocab.QualifiedTypeName.QualifiedTypeName schemaName typeName)) dimensionality textFormat),
              serializer,
              printer
            }

-- |
-- No parameters. Same as `mempty` and `conquered`.
noParams :: Params ()
noParams = mempty

-- |
-- Lift a single parameter encoder, with its nullability specified,
-- associating it with a single placeholder.
param :: NullableOrNot.NullableOrNot Value.Value a -> Params a
param = \case
  NullableOrNot.NonNullable valueEnc -> value valueEnc
  NullableOrNot.Nullable valueEnc -> nullableValue valueEnc
