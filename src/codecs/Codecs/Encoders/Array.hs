module Codecs.Encoders.Array where

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
  = Array Word32 Word32 (a -> Binary.Array) (a -> TextBuilder.TextBuilder)

instance Contravariant Array where
  contramap fn (Array valueOid arrayOid encoder renderer) =
    Array valueOid arrayOid (encoder . fn) (renderer . fn)

{-# INLINE value #-}
value :: Word32 -> Word32 -> (a -> Binary.Encoding) -> (a -> TextBuilder.TextBuilder) -> Array a
value valueOid arrayOid encoder =
  Array valueOid arrayOid (Binary.encodingArray . encoder)

{-# INLINE nullableValue #-}
nullableValue :: Word32 -> Word32 -> (a -> Binary.Encoding) -> (a -> TextBuilder.TextBuilder) -> Array (Maybe a)
nullableValue valueOid arrayOid encoder renderer =
  let maybeEncoder =
        maybe Binary.nullArray (Binary.encodingArray . encoder)
      maybeRenderer =
        maybe (TextBuilder.string "null") renderer
   in Array valueOid arrayOid maybeEncoder maybeRenderer

{-# INLINE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
dimension fold (Array valueOid arrayOid elEncoder elRenderer) =
  let encoder =
        Binary.dimensionArray fold elEncoder
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
   in Array valueOid arrayOid encoder renderer
