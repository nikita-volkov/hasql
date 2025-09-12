module Core.Contexts.RowDecoder
  ( RowDecoder,
    nullableColumn,
    nonNullableColumn,

    -- * Relations

    -- ** Expected OIDs
    toExpectedOids,

    -- ** Decoder
    Decoder,
    toDecoder,

    -- ** Compatiblity check
    toCompatibilityCheck,
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
    (RowDecoder.nullableColumn valueDec)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullableColumn valueDec =
  RowDecoder
    [ValueDecoder.toExpectedOid valueDec]
    (RowDecoder.nonNullableColumn valueDec)

-- * Relations

-- ** Expected OIDs

toExpectedOids :: RowDecoder a -> [Maybe Pq.Oid]
toExpectedOids (RowDecoder oids _) = oids

-- ** Decoder

type Decoder a = Bool -> Pq.Row -> Pq.Column -> Pq.Result -> IO (Either ResultError a)

toDecoder :: RowDecoder a -> Decoder a
toDecoder =
  RowDecoder.toHandler . toInnerRowDecoder

-- ** Compatiblity check

toCompatibilityCheck :: RowDecoder a -> Pq.Result -> IO (Either ResultError ())
toCompatibilityCheck (RowDecoder oids _) result = do
  maxCols <- Pq.nfields result
  if length oids == Pq.colToInt maxCols
    then go oids 0
    else pure (Left (UnexpectedAmountOfColumns (length oids) (Pq.colToInt maxCols)))
  where
    go [] _ = pure (Right ())
    go (Nothing : rest) colIndex = go rest (succ colIndex)
    go (Just expectedOid : rest) colIndex = do
      actualOid <- Pq.ftype result (Pq.toColumn colIndex)
      if actualOid == expectedOid
        then go rest (succ colIndex)
        else
          pure
            ( Left
                ( DecoderTypeMismatch
                    colIndex
                    (Pq.oidToWord32 expectedOid)
                    (Pq.oidToWord32 actualOid)
                )
            )

-- ** Inner decoder

toInnerRowDecoder :: RowDecoder a -> RowDecoder.RowDecoder a
toInnerRowDecoder (RowDecoder _ dec) = dec
