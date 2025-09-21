module Hipq.ResultRowMapping
  ( ResultRowMapping,
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

import Hipq.ResultRowDecoder qualified as ResultRowDecoder
import Platform.Prelude
import Pq qualified

-- * ResultRowMapping

data ResultRowMapping a
  = ResultRowMapping
      [Maybe Pq.Oid]
      (ResultRowDecoder.ResultRowDecoder a)
  deriving stock (Functor)

instance Applicative ResultRowMapping where
  pure a = ResultRowMapping [] (pure a)
  ResultRowMapping lOids lDec <*> ResultRowMapping rOids rDec =
    ResultRowMapping (lOids <> rOids) (lDec <*> rDec)

-- * Functions

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: Maybe Word32 -> (ByteString -> Either Text a) -> ResultRowMapping (Maybe a)
nullableColumn oid decoder =
  ResultRowMapping
    [Pq.Oid . fromIntegral <$> oid]
    (ResultRowDecoder.nullableColumn decoder)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: Maybe Word32 -> (ByteString -> Either Text a) -> ResultRowMapping a
nonNullableColumn oid decoder =
  ResultRowMapping
    [Pq.Oid . fromIntegral <$> oid]
    (ResultRowDecoder.nonNullableColumn decoder)

-- * Relations

-- ** Expected OIDs

toExpectedOids :: ResultRowMapping a -> [Maybe Pq.Oid]
toExpectedOids (ResultRowMapping oids _) = oids

-- ** Decoder

type Decoder a = Pq.Result -> Pq.Row -> IO (Either Error a)

toDecoder :: ResultRowMapping a -> Decoder a
toDecoder (ResultRowMapping _ dec) result row =
  ResultRowDecoder.toHandler dec result row

-- * Errors

type Error = ResultRowDecoder.Error
