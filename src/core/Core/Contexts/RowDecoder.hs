module Core.Contexts.RowDecoder
  ( RowDecoder,
    nullableColumn,
    nonNullableColumn,

    -- * Relations

    -- ** Expected OIDs
    toExpectedOids,

    -- ** Handler
    Handler,
    toHandler,
  )
where

import Core.Contexts.RowDecoder.RowDecoder qualified as RowDecoder
import Core.Contexts.ValueDecoder qualified as ValueDecoder
import Core.Errors
import Platform.Prelude hiding (error)
import Pq qualified

data RowDecoder a
  = RowDecoder
      [Maybe Pq.Oid]
      (RowDecoder.RowDecoder a)
  deriving stock (Functor)

instance Applicative RowDecoder where
  pure a = RowDecoder [] (pure a)
  RowDecoder lOids lDec <*> RowDecoder rOids rDec =
    RowDecoder (lOids <> rOids) (lDec <*> rDec)

-- * Functions

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder (Maybe a)
nullableColumn valueDec =
  RowDecoder
    [ValueDecoder.toExpectedOid valueDec]
    (RowDecoder.value valueDec)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullableColumn valueDec =
  RowDecoder
    [ValueDecoder.toExpectedOid valueDec]
    (RowDecoder.nonNullValue valueDec)

-- * Relations

-- ** Expected OIDs

toExpectedOids :: RowDecoder a -> [Maybe Pq.Oid]
toExpectedOids (RowDecoder oids _) = oids

-- ** Handler

type Handler a = Bool -> Pq.Row -> Pq.Column -> Pq.Result -> IO (Either ResultError a)

toHandler :: RowDecoder a -> Handler a
toHandler (RowDecoder _ dec) =
  RowDecoder.run dec
