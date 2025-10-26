module Codecs.Encoders.Params where

import Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Codecs.Encoders.Value qualified as Value
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

renderReadable :: Params a -> a -> [Text]
renderReadable (Params _ _ _ _ printer) params =
  printer params
    & toList

compilePreparedStatementData ::
  Params a ->
  HashMap (Maybe Text, Text) (Word32, Word32) ->
  a ->
  ([Word32], [Maybe ByteString])
compilePreparedStatementData (Params _ _ columnsMetadata serializer _) oidCache input =
  (oidList, valueList)
  where
    oidList =
      columnsMetadata
        & toList
        & fmap
          ( \case
              (Left name, dimensionality) ->
                case HashMap.lookup name oidCache of
                  Just (baseOid, arrayOid) ->
                    if dimensionality == 0 then baseOid else arrayOid
                  Nothing ->
                    0
              (Right oid, _) ->
                oid
          )

    valueList =
      serializer oidCache input
        & toList

compileUnpreparedStatementData ::
  Params a ->
  HashMap (Maybe Text, Text) (Word32, Word32) ->
  a ->
  [Maybe (Word32, ByteString)]
compileUnpreparedStatementData (Params _ _ columnsMetadata serializer _) oidCache input =
  zipWith
    ( \(nameOrOid, dimensionality) encoding ->
        let oid = case nameOrOid of
              Left name -> case HashMap.lookup name oidCache of
                Just (baseOid, arrayOid) ->
                  if dimensionality == 0 then baseOid else arrayOid
                Nothing -> 0
              Right oid -> oid
         in (,) <$> Just oid <*> encoding
    )
    (toList columnsMetadata)
    (toList (serializer oidCache input))

toUnknownTypes :: Params a -> HashSet (Maybe Text, Text)
toUnknownTypes (Params _ unknownTypes _ _ _) =
  unknownTypes

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
-- from the \"contravariant-extras\" package.
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
-- genderValue = 'enum' genderText 'text' where
--   genderText gender = case gender of
--     Male -> "male"
--     Female -> "female"
-- @
data Params a = Params
  { size :: Int,
    unknownTypes :: HashSet (Maybe Text, Text),
    -- | (Name or OID, dimensionality) for each parameter.
    columnsMetadata :: DList (Either (Maybe Text, Text) Word32, Word),
    serializer :: HashMap (Maybe Text, Text) (Word32, Word32) -> a -> DList (Maybe ByteString),
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
value (Value.Value schemaName typeName scalarOid arrayOid dimensionality unknownTypes serialize print) =
  let staticOid = if dimensionality == 0 then scalarOid else arrayOid
      serializer oidCache = pure . Just . Binary.encodingBytes . serialize oidCache
      printer = pure . TextBuilder.toText . print
      size = 1
   in case staticOid of
        Just oid ->
          Params
            { size,
              unknownTypes,
              columnsMetadata = pure (Right oid, dimensionality),
              serializer,
              printer
            }
        Nothing ->
          Params
            { size,
              unknownTypes = HashSet.insert (schemaName, typeName) unknownTypes,
              columnsMetadata = pure (Left (schemaName, typeName), dimensionality),
              serializer,
              printer
            }

nullableValue :: Value.Value a -> Params (Maybe a)
nullableValue (Value.Value schemaName typeName scalarOid arrayOid dimensionality unknownTypes serialize print) =
  let staticOid = if dimensionality == 0 then scalarOid else arrayOid
      serializer oidCache = pure . fmap (Binary.encodingBytes . serialize oidCache)
      printer = pure . maybe "null" (TextBuilder.toText . print)
      size = 1
   in case staticOid of
        Just oid ->
          Params
            { size,
              unknownTypes,
              columnsMetadata = pure (Right oid, dimensionality),
              serializer,
              printer
            }
        Nothing ->
          Params
            { size,
              unknownTypes = HashSet.insert (schemaName, typeName) unknownTypes,
              columnsMetadata = pure (Left (schemaName, typeName), dimensionality),
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
