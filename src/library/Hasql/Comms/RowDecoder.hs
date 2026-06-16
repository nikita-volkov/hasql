module Hasql.Comms.RowDecoder
  ( RowDecoder,
    nullableColumn,
    nonNullableColumn,

    -- * Relations

    -- ** Expected OIDs
    toExpectedOids,

    -- ** Row reader
    toRowReader,

    -- * Errors
    Error,
  )
where

import Hasql.Comms.RowReader qualified as RowReader
import Hasql.Platform.Prelude

-- * RowDecoder

data RowDecoder a
  = RowDecoder
      [Maybe Word32]
      (RowReader.RowReader a)
  deriving stock (Functor)

instance Applicative RowDecoder where
  pure a = RowDecoder [] (pure a)
  RowDecoder lOids lDec <*> RowDecoder rOids rDec =
    RowDecoder (lOids <> rOids) (lDec <*> rDec)

instance Filterable RowDecoder where
  mapMaybe fn (RowDecoder oids dec) =
    RowDecoder oids (mapMaybe fn dec)

-- * Functions

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: Maybe Word32 -> (ByteString -> Either Text a) -> RowDecoder (Maybe a)
nullableColumn oid decoder =
  RowDecoder
    [oid]
    (RowReader.nullableColumn decoder)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: Maybe Word32 -> (ByteString -> Either Text a) -> RowDecoder a
nonNullableColumn oid decoder =
  RowDecoder
    [oid]
    (RowReader.nonNullableColumn decoder)

-- * Relations

-- ** Expected OIDs

toExpectedOids :: RowDecoder a -> [Maybe Word32]
toExpectedOids (RowDecoder oids _) = oids

-- ** Row reader

toRowReader :: RowDecoder a -> RowReader.RowReader a
toRowReader (RowDecoder _ dec) = dec

-- * Errors

type Error = RowReader.Error
