module Codecs.Decoders.Composite where

import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (HashMap (Maybe Text, Text) (Word32, Word32) -> Binary.Composite a)
  deriving
    (Functor, Applicative, Monad, MonadFail)
    via (ReaderT (HashMap (Maybe Text, Text) (Word32, Word32)) Binary.Composite)

{-# INLINE run #-}
run :: Composite a -> HashMap (Maybe Text, Text) (Word32, Word32) -> Binary.Value a
run (Composite imp) oidCache =
  Binary.composite (imp oidCache)

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable imp ->
    Composite \oidCache -> Binary.valueComposite (Value.toHandler imp oidCache)
  NullableOrNot.Nullable imp ->
    Composite \oidCache -> Binary.nullableValueComposite (Value.toHandler imp oidCache)
