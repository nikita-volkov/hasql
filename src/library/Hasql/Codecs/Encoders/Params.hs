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

import Data.Vector qualified as Vector
import Hasql.Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Encoders.Value qualified as Value
import Hasql.CodecsCore qualified as CodecsCore
import Hasql.CodecsCore.QualifiedTypeName qualified as CodecsCore.QualifiedTypeName
import Hasql.CodecsCore.TypeRef qualified as CodecsCore.TypeRef
import Hasql.CodecsCore.TypeShape (TypeShape (..))
import Hasql.Platform.Prelude
import Hasql.ToBeResolved qualified as ToBeResolved
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- | Frozen per-parameter type shapes: type reference, dimensionality, text-format flag.
toColumnsMetadata :: Params a -> Vector TypeShape
toColumnsMetadata (Params _ _ columnsMetadata _) = freezeColumnsMetadata columnsMetadata
  where
    freezeColumnsMetadata =
      Vector.fromList . toList

toUnknownTypes :: Params a -> HashSet CodecsCore.QualifiedTypeName
toUnknownTypes (Params _ (ToBeResolved.ToBeResolved unknownTypes _) _ _) =
  fromList unknownTypes

-- | Serialise params to encoded wire values given a resolver of type names to their OIDs.
toSerializer :: Params a -> (CodecsCore.QualifiedTypeName -> CodecsCore.TypeInfo) -> a -> [Maybe ByteString]
toSerializer (Params _ (ToBeResolved.ToBeResolved _ serializer) _ _) resolve = serializer resolve

-- | Render params in human-readable form (for error reporting).
toPrinter :: Params a -> a -> [Text]
toPrinter (Params _ _ _ printer) = toList . printer

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
    -- | Serialization function, deferring the names of types that must be looked up at runtime.
    request :: ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName CodecsCore.TypeInfo (a -> [Maybe ByteString]),
    -- | Type shape for each parameter.
    columnsMetadata :: DList TypeShape,
    printer :: a -> DList Text
  }

instance Contravariant Params where
  contramap fn (Params size request columnsMetadata printer) =
    Params size (fmap (. fn) request) columnsMetadata (printer . fn)

instance Divisible Params where
  divide
    divisor
    (Params leftSize leftRequest leftColumnsMetadata leftPrinter)
    (Params rightSize rightRequest rightColumnsMetadata rightPrinter) =
      Params
        { size = leftSize + rightSize,
          request =
            liftA2
              ( \leftSerializer rightSerializer input -> case divisor input of
                  (leftInput, rightInput) -> leftSerializer leftInput <> rightSerializer rightInput
              )
              leftRequest
              rightRequest,
          columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
          printer = \input -> case divisor input of
            (leftInput, rightInput) -> leftPrinter leftInput <> rightPrinter rightInput
        }
  conquer =
    Params
      { size = 0,
        request = pure mempty,
        columnsMetadata = mempty,
        printer = mempty
      }

instance Semigroup (Params a) where
  Params leftSize leftRequest leftColumnsMetadata leftPrinter <> Params rightSize rightRequest rightColumnsMetadata rightPrinter =
    Params
      { size = leftSize + rightSize,
        request = liftA2 (\leftSerializer rightSerializer input -> leftSerializer input <> rightSerializer input) leftRequest rightRequest,
        columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
        printer = \input -> leftPrinter input <> rightPrinter input
      }

instance Monoid (Params a) where
  mempty = conquer

value :: Value.Value a -> Params a
value (Value.Value schemaName typeName scalarOid arrayOid dimensionality textFormat serialize print) =
  let staticOid = if dimensionality == 0 then scalarOid else arrayOid
      toRequest = fmap (\encode -> pure . Just . Binary.encodingBytes . encode)
      printer = pure . TextBuilder.toText . print
      size = 1
   in case staticOid of
        Just oid ->
          Params
            { size,
              request = toRequest serialize,
              columnsMetadata = pure (TypeShape (CodecsCore.TypeRef.KnownOid oid) dimensionality textFormat),
              printer
            }
        Nothing ->
          let key = CodecsCore.QualifiedTypeName.QualifiedTypeName schemaName typeName
           in Params
                { size,
                  request = toRequest (ToBeResolved.lookup key *> serialize),
                  columnsMetadata = pure (TypeShape (CodecsCore.TypeRef.NamedType key) dimensionality textFormat),
                  printer
                }

nullableValue :: Value.Value a -> Params (Maybe a)
nullableValue (Value.Value schemaName typeName scalarOid arrayOid dimensionality textFormat serialize print) =
  let staticOid = if dimensionality == 0 then scalarOid else arrayOid
      toRequest = fmap (\encode -> pure . fmap (Binary.encodingBytes . encode))
      printer = pure . maybe "null" (TextBuilder.toText . print)
      size = 1
   in case staticOid of
        Just oid ->
          Params
            { size,
              request = toRequest serialize,
              columnsMetadata = pure (TypeShape (CodecsCore.TypeRef.KnownOid oid) dimensionality textFormat),
              printer
            }
        Nothing ->
          let key = CodecsCore.QualifiedTypeName.QualifiedTypeName schemaName typeName
           in Params
                { size,
                  request = toRequest (ToBeResolved.lookup key *> serialize),
                  columnsMetadata = pure (TypeShape (CodecsCore.TypeRef.NamedType key) dimensionality textFormat),
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
