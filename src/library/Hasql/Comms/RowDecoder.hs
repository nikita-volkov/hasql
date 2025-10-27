module Hasql.Comms.RowDecoder
  ( RowDecoder,
    nullableColumn,
    nonNullableColumn,

    -- * Relations

    -- ** Expected OIDs
    toExpectedOids,

    -- ** Decoder
    Decoder,
    toDecoder,

    -- * Errors
    Error,
  )
where

import Hasql.Comms.RowReader qualified as RowReader
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

-- * RowDecoder

data RowDecoder a
  = RowDecoder
      [Maybe Pq.Oid]
      (RowReader.RowReader a)
  deriving stock (Functor)

instance Applicative RowDecoder where
  pure a = RowDecoder [] (pure a)
  RowDecoder lOids lDec <*> RowDecoder rOids rDec =
    RowDecoder (lOids <> rOids) (lDec <*> rDec)

-- * Functions

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: Maybe Word32 -> (ByteString -> Either Text a) -> RowDecoder (Maybe a)
nullableColumn oid decoder =
  RowDecoder
    [Pq.Oid . fromIntegral <$> oid]
    (RowReader.nullableColumn decoder)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: Maybe Word32 -> (ByteString -> Either Text a) -> RowDecoder a
nonNullableColumn oid decoder =
  RowDecoder
    [Pq.Oid . fromIntegral <$> oid]
    (RowReader.nonNullableColumn decoder)

-- * Relations

-- ** Expected OIDs

toExpectedOids :: RowDecoder a -> [Maybe Pq.Oid]
toExpectedOids (RowDecoder oids _) = oids

-- ** Decoder

type Decoder a = Pq.Result -> Pq.Row -> IO (Either Error a)

toDecoder :: RowDecoder a -> Decoder a
toDecoder (RowDecoder _ dec) result row =
  RowReader.toHandler dec result row

-- * Errors

type Error = RowReader.Error
