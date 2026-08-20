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
    withDecoded,

    -- * Errors
    Error,
  )
where

import Hasql.Comms.RowReader qualified as RowReader
import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- * RowDecoder

data RowDecoder a
  = RowDecoder
      [Maybe Word32]
      (RowReader.RowReader a)
  deriving stock (Functor)

instance Applicative RowDecoder where
  {-# INLINE pure #-}
  pure a = RowDecoder [] (pure a)
  {-# INLINE (<*>) #-}
  RowDecoder lOids lDec <*> RowDecoder rOids rDec =
    RowDecoder (lOids <> rOids) (lDec <*> rDec)

instance Filterable RowDecoder where
  {-# INLINE mapMaybe #-}
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

-- ** Decoder

type Decoder a = Pq.Result -> Int32 -> IO (Either Error a)

{-# INLINE toDecoder #-}
toDecoder :: RowDecoder a -> Decoder a
toDecoder (RowDecoder _ dec) result row =
  RowReader.toHandler dec result row

-- |
-- Decode one row and hand the result to one of two continuations.
--
-- 'toDecoder' collects the outcome into an @Either@ first, which costs an
-- allocation per row for a value that is looked at once and dropped. Callers
-- that only branch on it -- the vector and fold result decoders -- pass their
-- two branches in here instead.
{-# INLINE withDecoded #-}
withDecoded ::
  RowDecoder a ->
  Pq.Result ->
  Int32 ->
  (Error -> IO r) ->
  (a -> IO r) ->
  IO r
withDecoded (RowDecoder _ dec) result row onError onSuccess =
  RowReader.withRow dec result row 0 onError (\_col a -> onSuccess a)

-- * Errors

type Error = RowReader.Error
