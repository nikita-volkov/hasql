module Codecs.Encoders.Array where

import Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Codecs.Encoders.Value qualified as Value
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified as TextBuilder

-- |
-- Generic array encoder.
--
-- Here's an example of its usage:
--
-- @
-- someParamsEncoder :: 'Params' [[Int64]]
-- someParamsEncoder = 'param' ('nonNullable' ('array' ('dimension' 'foldl'' ('dimension' 'foldl'' ('element' ('nonNullable' 'int8'))))))
-- @
--
-- Please note that the PostgreSQL @IN@ keyword does not accept an array, but rather a syntactical list of
-- values, thus this encoder is not suited for that. Use a @value = ANY($1)@ condition instead.
data Array a
  = Array
      -- | Schema name, if any.
      (Maybe Text)
      -- | Type name.
      Text
      -- | Text format?
      Bool
      -- | Dimensionality. If 0 then it is not an array, but a scalar value.
      Word
      -- | OID of the element type.
      (Maybe Word32)
      -- | OID of the array type.
      (Maybe Word32)
      -- | Names of types that are not known statically and must be looked up at runtime collected from the nested composite and array encoders.
      (HashSet (Maybe Text, Text))
      -- | Serialization function given the dictionary of resolved OIDs.
      (HashMap (Maybe Text, Text) (Word32, Word32) -> a -> Binary.Array)
      -- | Render function for error messages.
      (a -> TextBuilder.TextBuilder)

instance Contravariant Array where
  contramap fn (Array schemaName typeName textFormat dimensionality valueOid arrayOid unknownTypes elEncoder elRenderer) =
    Array schemaName typeName textFormat dimensionality valueOid arrayOid unknownTypes (\oidCache -> elEncoder oidCache . fn) (elRenderer . fn)

-- |
-- Lifts a 'Value.Value' encoder into an 'Array' encoder.
element :: NullableOrNot.NullableOrNot Value.Value a -> Array a
element = \case
  NullableOrNot.NonNullable (Value.Value schemaName typeName textFormat dimensionality scalarOid arrayOid unknownTypes serialize print) ->
    Array
      schemaName
      typeName
      textFormat
      dimensionality
      scalarOid
      arrayOid
      unknownTypes
      (\oidCache -> Binary.encodingArray . serialize oidCache)
      print
  NullableOrNot.Nullable (Value.Value schemaName typeName textFormat dimensionality scalarOid arrayOid unknownTypes serialize print) ->
    let maybeSerialize oidCache =
          maybe Binary.nullArray (Binary.encodingArray . serialize oidCache)
        maybePrint =
          maybe (TextBuilder.string "null") print
     in Array
          schemaName
          typeName
          textFormat
          dimensionality
          scalarOid
          arrayOid
          unknownTypes
          maybeSerialize
          maybePrint

-- |
-- Encoder of an array dimension,
-- which thus provides support for multidimensional arrays.
--
-- Accepts:
--
-- * An implementation of the left-fold operation,
-- such as @Data.Foldable.'foldl''@,
-- which determines the input value.
--
-- * A component encoder, which can be either another 'dimension' or 'element'.
{-# INLINE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
dimension fold (Array schemaName typeName textFormat dimensionality valueOid arrayOid unknownTypes elEncoder elRenderer) =
  let encoder oidCache =
        Binary.dimensionArray fold (elEncoder oidCache)
      renderer els =
        let folded =
              let step builder el =
                    if TextBuilder.isEmpty builder
                      then TextBuilder.char '[' <> elRenderer el
                      else builder <> TextBuilder.string ", " <> elRenderer el
               in fold step mempty els
         in if TextBuilder.isEmpty folded
              then TextBuilder.string "[]"
              else folded <> TextBuilder.char ']'
   in Array schemaName typeName textFormat (succ dimensionality) valueOid arrayOid unknownTypes encoder renderer
