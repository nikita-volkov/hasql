module Codecs.ParamsEncoder where

import Codecs.PostgresTypeInfo qualified as D
import Codecs.ValueEncoder qualified as C
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as B
import Pq qualified as A
import TextBuilder qualified as E

renderReadable :: ParamsEncoder a -> a -> [Text]
renderReadable (ParamsEncoder _ _ _ printer) params =
  printer params
    & toList

compilePreparedStatementData :: ParamsEncoder a -> a -> ([A.Oid], [Maybe (ByteString, A.Format)])
compilePreparedStatementData (ParamsEncoder _ columnsMetadata serializer _) input =
  (oidList, valueAndFormatList)
  where
    (oidList, formatList) =
      columnsMetadata & toList & unzip
    valueAndFormatList =
      serializer input
        & toList
        & zipWith (\format encoding -> (,format) <$> encoding) formatList

compileUnpreparedStatementData :: ParamsEncoder a -> a -> [Maybe (A.Oid, ByteString, A.Format)]
compileUnpreparedStatementData (ParamsEncoder _ columnsMetadata serializer _) input =
  zipWith
    ( \(oid, format) encoding ->
        (,,) <$> pure oid <*> encoding <*> pure format
    )
    (toList columnsMetadata)
    (toList (serializer input))

-- |
-- Encoder of some representation of a parameters product.
data ParamsEncoder a = ParamsEncoder
  { size :: Int,
    columnsMetadata :: DList (A.Oid, A.Format),
    serializer :: a -> DList (Maybe ByteString),
    printer :: a -> DList Text
  }

instance Contravariant ParamsEncoder where
  contramap fn (ParamsEncoder size columnsMetadata oldSerializer oldPrinter) = ParamsEncoder {..}
    where
      serializer = oldSerializer . fn
      printer = oldPrinter . fn

instance Divisible ParamsEncoder where
  divide
    divisor
    (ParamsEncoder leftSize leftColumnsMetadata leftSerializer leftPrinter)
    (ParamsEncoder rightSize rightColumnsMetadata rightSerializer rightPrinter) =
      ParamsEncoder
        { size = leftSize + rightSize,
          columnsMetadata = leftColumnsMetadata <> rightColumnsMetadata,
          serializer = \input -> case divisor input of
            (leftInput, rightInput) -> leftSerializer leftInput <> rightSerializer rightInput,
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
        serializer = \input -> leftSerializer input <> rightSerializer input,
        printer = \input -> leftPrinter input <> rightPrinter input
      }

instance Monoid (ParamsEncoder a) where
  mempty = conquer

value :: C.ValueEncoder a -> ParamsEncoder a
value (C.ValueEncoder _ (Just valueOID) _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = pure . Just . B.encodingBytes . serialize,
      printer = pure . E.toText . print
    }
  where
    D.OID _ pqOid format = valueOID
value (C.ValueEncoder _ Nothing _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = pure . Just . B.encodingBytes . serialize,
      printer = pure . E.toText . print
    }
  where
    D.OID _ pqOid format = D.ptiOID D.binaryUnknown

nullableValue :: C.ValueEncoder a -> ParamsEncoder (Maybe a)
nullableValue (C.ValueEncoder _ (Just valueOID) _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = pure . fmap (B.encodingBytes . serialize),
      printer = pure . maybe "null" (E.toText . print)
    }
  where
    D.OID _ pqOid format = valueOID
nullableValue (C.ValueEncoder _ Nothing _ serialize print) =
  ParamsEncoder
    { size = 1,
      columnsMetadata = pure (pqOid, format),
      serializer = pure . fmap (B.encodingBytes . serialize),
      printer = pure . maybe "null" (E.toText . print)
    }
  where
    D.OID _ pqOid format = D.ptiOID D.binaryUnknown

class Wraps f where
  wrap :: ParamsEncoder a -> f a
  unwrap :: f a -> ParamsEncoder a
