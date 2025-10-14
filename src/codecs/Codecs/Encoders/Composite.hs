module Codecs.Encoders.Composite where

import Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Codecs.Encoders.Value qualified as Value
import Codecs.TypeInfo qualified as TypeInfo
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- |
-- Composite or row-types encoder.
data Composite a
  = Composite
      (a -> Binary.Composite)
      (a -> [TextBuilder.TextBuilder])

instance Contravariant Composite where
  contramap f (Composite encode print) =
    Composite (encode . f) (print . f)

instance Divisible Composite where
  divide f (Composite encodeL printL) (Composite encodeR printR) =
    Composite
      (\val -> case f val of (lVal, rVal) -> encodeL lVal <> encodeR rVal)
      (\val -> case f val of (lVal, rVal) -> printL lVal <> printR rVal)
  conquer = mempty

instance Semigroup (Composite a) where
  Composite encodeL printL <> Composite encodeR printR =
    Composite
      (\val -> encodeL val <> encodeR val)
      (\val -> printL val <> printR val)

instance Monoid (Composite a) where
  mempty = Composite mempty mempty

-- | Single field of a row-type.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable (Value.Value _ _ (Just elementOid) _ encode print) ->
    Composite
      (\val -> Binary.field elementOid (encode val))
      (\val -> [print val])
  NullableOrNot.NonNullable (Value.Value _ _ Nothing _ encode print) ->
    Composite
      (\val -> Binary.field (TypeInfo.toBaseOid TypeInfo.unknown) (encode val))
      (\val -> [print val])
  NullableOrNot.Nullable (Value.Value _ _ (Just elementOid) _ encode print) ->
    Composite
      ( \val -> case val of
          Nothing -> Binary.nullField elementOid
          Just val -> Binary.field elementOid (encode val)
      )
      ( \val ->
          case val of
            Nothing -> ["NULL"]
            Just val -> [print val]
      )
  NullableOrNot.Nullable (Value.Value _ _ Nothing _ encode print) ->
    Composite
      ( \val -> case val of
          Nothing -> Binary.nullField (TypeInfo.toBaseOid TypeInfo.unknown)
          Just val -> Binary.field (TypeInfo.toBaseOid TypeInfo.unknown) (encode val)
      )
      ( \val ->
          case val of
            Nothing -> ["NULL"]
            Just val -> [print val]
      )
