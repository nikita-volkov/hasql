module Codecs.Encoders.Params where

import Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Codecs.Encoders.Value qualified as Value
import Codecs.TypeInfo qualified as TypeInfo
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

renderReadable :: Params a -> a -> [Text]
renderReadable (Params _ _ _ printer) params =
  printer params
    & toList

compilePreparedStatementData :: Params a -> a -> ([Word32], [Maybe (ByteString, Bool)])
compilePreparedStatementData (Params _ columnsMetadata serializer _) input =
  (oidList, valueAndFormatList)
  where
    (oidList, formatList) =
      columnsMetadata & toList & unzip
    valueAndFormatList =
      serializer input
        & toList
        & zipWith (\format encoding -> (,format) <$> encoding) formatList

compileUnpreparedStatementData :: Params a -> a -> [Maybe (Word32, ByteString, Bool)]
compileUnpreparedStatementData (Params _ columnsMetadata serializer _) input =
  zipWith
    ( \(oid, format) encoding ->
        (,,) <$> pure oid <*> encoding <*> pure format
    )
    (toList columnsMetadata)
    (toList (serializer input))

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
    -- | (OID, Format) for each parameter.
    columnsMetadata :: DList (Word32, Bool),
    serializer :: a -> DList (Maybe ByteString),
    printer :: a -> DList Text
  }

instance Contravariant Params where
  contramap fn (Params size columnsMetadata oldSerializer oldPrinter) = Params {..}
    where
      serializer = oldSerializer . fn
      printer = oldPrinter . fn

instance Divisible Params where
  divide
    divisor
    (Params leftSize leftColumnsMetadata leftSerializer leftPrinter)
    (Params rightSize rightColumnsMetadata rightSerializer rightPrinter) =
      Params
        { size = leftSize + rightSize,
          columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
          serializer = \input -> case divisor input of
            (leftInput, rightInput) -> leftSerializer leftInput <> rightSerializer rightInput,
          printer = \input -> case divisor input of
            (leftInput, rightInput) -> leftPrinter leftInput <> rightPrinter rightInput
        }
  conquer =
    Params
      { size = 0,
        columnsMetadata = mempty,
        serializer = mempty,
        printer = mempty
      }

instance Semigroup (Params a) where
  Params leftSize leftColumnsMetadata leftSerializer leftPrinter <> Params rightSize rightColumnsMetadata rightSerializer rightPrinter =
    Params
      { size = leftSize + rightSize,
        columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
        serializer = \input -> leftSerializer input <> rightSerializer input,
        printer = \input -> leftPrinter input <> rightPrinter input
      }

instance Monoid (Params a) where
  mempty = conquer

value :: Value.Value a -> Params a
value (Value.Value _ textFormat (Just valueOid) _ serialize print) =
  Params
    { size = 1,
      columnsMetadata = pure (valueOid, textFormat),
      serializer = pure . Just . Binary.encodingBytes . serialize,
      printer = pure . TextBuilder.toText . print
    }
value (Value.Value _ textFormat Nothing _ serialize print) =
  Params
    { size = 1,
      columnsMetadata = pure (TypeInfo.toBaseOid TypeInfo.unknown, textFormat),
      serializer = pure . Just . Binary.encodingBytes . serialize,
      printer = pure . TextBuilder.toText . print
    }

nullableValue :: Value.Value a -> Params (Maybe a)
nullableValue (Value.Value _ textFormat (Just valueOid) _ serialize print) =
  Params
    { size = 1,
      columnsMetadata = pure (valueOid, textFormat),
      serializer = pure . fmap (Binary.encodingBytes . serialize),
      printer = pure . maybe "null" (TextBuilder.toText . print)
    }
nullableValue (Value.Value _ textFormat Nothing _ serialize print) =
  Params
    { size = 1,
      columnsMetadata = pure (TypeInfo.toBaseOid TypeInfo.unknown, textFormat),
      serializer = pure . fmap (Binary.encodingBytes . serialize),
      printer = pure . maybe "null" (TextBuilder.toText . print)
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
