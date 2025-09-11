module Hasql.Contexts.ParamsEncoder where

import Hasql.Contexts.ValueEncoder qualified as C
import Hasql.LibPq14 qualified as A
import Hasql.PostgresTypeInfo qualified as D
import Hasql.Prelude
import PostgreSQL.Binary.Encoding qualified as B
import TextBuilder qualified as E

renderReadable :: ParamsEncoder a -> a -> [Text]
renderReadable (ParamsEncoder _ _ _ printer) params =
  printer params
    & toList

compilePreparedStatementData :: ParamsEncoder a -> Bool -> a -> ([A.Oid], [Maybe (ByteString, A.Format)])
compilePreparedStatementData (ParamsEncoder _ columnsMetadata serializer _) integerDatetimes input =
  (oidList, valueAndFormatList)
  where
    (oidList, formatList) =
      columnsMetadata & toList & unzip
    valueAndFormatList =
      serializer integerDatetimes input
        & toList
        & zipWith (\format encoding -> (,format) <$> encoding) formatList

compileUnpreparedStatementData :: ParamsEncoder a -> Bool -> a -> [Maybe (A.Oid, ByteString, A.Format)]
compileUnpreparedStatementData (ParamsEncoder _ columnsMetadata serializer _) integerDatetimes input =
  zipWith
    ( \(oid, format) encoding ->
        (,,) <$> pure oid <*> encoding <*> pure format
    )
    (toList columnsMetadata)
    (toList (serializer integerDatetimes input))

-- |
-- Encoder of some representation of a parameters product.
data ParamsEncoder a = ParamsEncoder
  { size :: Int,
    columnsMetadata :: DList (A.Oid, A.Format),
    serializer :: Bool -> a -> DList (Maybe ByteString),
    printer :: a -> DList Text
  }

instance Contravariant ParamsEncoder where
  contramap fn (ParamsEncoder size columnsMetadata oldSerializer oldPrinter) = ParamsEncoder {..}
    where
      serializer idt = oldSerializer idt . fn
      printer = oldPrinter . fn

instance Divisible ParamsEncoder where
  divide
    divisor
    (ParamsEncoder leftSize leftColumnsMetadata leftSerializer leftPrinter)
    (ParamsEncoder rightSize rightColumnsMetadata rightSerializer rightPrinter) =
      ParamsEncoder
        { size = leftSize + rightSize,
          columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
          serializer = \idt input -> case divisor input of
            (leftInput, rightInput) -> leftSerializer idt leftInput <> rightSerializer idt rightInput,
          printer = \input -> case divisor input of
            (leftInput, rightInput) -> leftPrinter leftInput <> rightPrinter rightInput
        }
  conquer =
    ParamsEncoder
      { size = 0,
        columnsMetadata = mempty,
        serializer = mempty,
        printer = mempty
      }

instance Semigroup (ParamsEncoder a) where
  ParamsEncoder leftSize leftColumnsMetadata leftSerializer leftPrinter <> ParamsEncoder rightSize rightColumnsMetadata rightSerializer rightPrinter =
    ParamsEncoder
      { size = leftSize + rightSize,
        columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
        serializer = \idt input -> leftSerializer idt input <> rightSerializer idt input,
        printer = \input -> leftPrinter input <> rightPrinter input
      }

instance Monoid (ParamsEncoder a) where
  mempty = conquer

value :: C.ValueEncoder a -> ParamsEncoder a
value (C.ValueEncoder _ (Just valueOID) _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = \idt -> pure . Just . B.encodingBytes . serialize idt,
      printer = pure . E.toText . print
    }
  where
    D.OID _ pqOid format = valueOID
value (C.ValueEncoder _ Nothing _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = \idt -> pure . Just . B.encodingBytes . serialize idt,
      printer = pure . E.toText . print
    }
  where
    D.OID _ pqOid format = D.ptiOID D.binaryUnknown

nullableValue :: C.ValueEncoder a -> ParamsEncoder (Maybe a)
nullableValue (C.ValueEncoder _ (Just valueOID) _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = \idt -> pure . fmap (B.encodingBytes . serialize idt),
      printer = pure . maybe "null" (E.toText . print)
    }
  where
    D.OID _ pqOid format = valueOID
nullableValue (C.ValueEncoder _ Nothing _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = \idt -> pure . fmap (B.encodingBytes . serialize idt),
      printer = pure . maybe "null" (E.toText . print)
    }
  where
    D.OID _ pqOid format = D.ptiOID D.binaryUnknown
