module Hasql.Codecs.Encoders.Composite where

import Hasql.Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Encoders.Value qualified as Value
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.CodecsVocab.TypeInfo qualified as CodecsVocab.TypeInfo
import Hasql.Platform.Prelude hiding (bool)
import Hasql.ToBeResolved qualified as ToBeResolved
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- |
-- Composite or row-types encoder.
data Composite a
  = Composite
      -- | Serialization function, deferring the names of types that must be looked up at runtime.
      (ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo (a -> Binary.Composite))
      -- | Render function for error messages.
      (a -> [TextBuilder.TextBuilder])

instance Contravariant Composite where
  contramap f (Composite request print) =
    Composite (fmap (. f) request) (print . f)

instance Divisible Composite where
  divide f (Composite requestL printL) (Composite requestR printR) =
    Composite
      ( liftA2
          (\encodeL encodeR val -> case f val of (lVal, rVal) -> encodeL lVal <> encodeR rVal)
          requestL
          requestR
      )
      (\val -> case f val of (lVal, rVal) -> printL lVal <> printR rVal)
  conquer = mempty

instance Semigroup (Composite a) where
  Composite requestL printL <> Composite requestR printR =
    Composite
      (liftA2 (\encodeL encodeR val -> encodeL val <> encodeR val) requestL requestR)
      (\val -> printL val <> printR val)

instance Monoid (Composite a) where
  mempty = Composite (pure mempty) mempty

-- | Single field of a row-type.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable (Value.Value schemaName typeName scalarOid arrayOid dimensionality _ serialize print) ->
    let staticOid = if dimensionality == 0 then scalarOid else arrayOid
        toField oid encode = \val -> Binary.field oid (encode val)
     in case staticOid of
          Just oid ->
            Composite (fmap (toField oid) serialize) (\val -> [print val])
          Nothing ->
            Composite
              ( (\typeInfo -> toField (if dimensionality == 0 then CodecsVocab.TypeInfo.toBaseOid typeInfo else CodecsVocab.TypeInfo.toArrayOid typeInfo))
                  <$> ToBeResolved.lookup (CodecsVocab.QualifiedTypeName.QualifiedTypeName schemaName typeName)
                  <*> serialize
              )
              (\val -> [print val])
  NullableOrNot.Nullable (Value.Value schemaName typeName scalarOid arrayOid dimensionality _ serialize print) ->
    let staticOid = if dimensionality == 0 then scalarOid else arrayOid
        toField oid encode = maybe (Binary.nullField oid) (Binary.field oid . encode)
     in case staticOid of
          Just oid ->
            Composite (fmap (toField oid) serialize) (maybe ["NULL"] (\val -> [print val]))
          Nothing ->
            Composite
              ( (\typeInfo -> toField (if dimensionality == 0 then CodecsVocab.TypeInfo.toBaseOid typeInfo else CodecsVocab.TypeInfo.toArrayOid typeInfo))
                  <$> ToBeResolved.lookup (CodecsVocab.QualifiedTypeName.QualifiedTypeName schemaName typeName)
                  <*> serialize
              )
              (maybe ["NULL"] (\val -> [print val]))
