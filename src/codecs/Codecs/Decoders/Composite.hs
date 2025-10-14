module Codecs.Decoders.Composite where

import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (Binary.Composite a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

{-# INLINE run #-}
run :: Composite a -> Binary.Value a
run (Composite imp) =
  Binary.composite imp

{-# INLINE value #-}
value :: Binary.Value a -> Composite (Maybe a)
value decoder' =
  Composite $ Binary.nullableValueComposite decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: Binary.Value a -> Composite a
nonNullValue decoder' =
  Composite $ Binary.valueComposite decoder'

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable imp -> nonNullValue (Value.toHandler imp)
  NullableOrNot.Nullable imp -> value (Value.toHandler imp)
