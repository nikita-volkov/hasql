module Core.Decoders.Result where

import Codecs.RequestingOid qualified as RequestingOid
import Comms.ResultDecoder qualified as ResultDecoder
import Core.Decoders.Row (Row (..))
import Core.Decoders.Row qualified as Row
import Platform.Prelude

-- |
-- Decoder of a query result.
newtype Result a
  = Result (RequestingOid.RequestingOid ResultDecoder.ResultDecoder a)
  deriving newtype
    (Functor, Applicative, Filterable)

unwrap :: Result a -> RequestingOid.RequestingOid ResultDecoder.ResultDecoder a
unwrap (Result decoder) = decoder

-- * Construction

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
{-# INLINEABLE noResult #-}
noResult :: Result ()
noResult =
  Result (RequestingOid.lift ResultDecoder.ok)

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
{-# INLINEABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  Result (RequestingOid.lift ResultDecoder.rowsAffected)

-- |
-- Exactly one row.
-- Will raise the 'Errors.UnexpectedAmountOfRows' error if it's any other.
{-# INLINEABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow decoder =
  Result (RequestingOid.hoist ResultDecoder.single (Row.toDecoder decoder))

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result decoder) =
  Result (RequestingOid.hoist (ResultDecoder.refine refiner) decoder)

-- ** Multi-row traversers

-- |
-- Foldl multiple rows.
{-# INLINEABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init decoder =
  Result
    (RequestingOid.hoist (ResultDecoder.foldl step init) (Row.toDecoder decoder))

-- |
-- Foldr multiple rows.
{-# INLINEABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init decoder =
  Result
    (RequestingOid.hoist (ResultDecoder.foldr step init) (Row.toDecoder decoder))

-- ** Specialized multi-row results

-- |
-- Maybe one row or none.
{-# INLINEABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe decoder =
  Result
    (RequestingOid.hoist ResultDecoder.maybe (Row.toDecoder decoder))

-- |
-- Zero or more rows packed into the vector.
--
-- It's recommended to prefer this function to 'rowList',
-- since it performs notably better.
{-# INLINEABLE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector decoder =
  Result
    (RequestingOid.hoist ResultDecoder.vector (Row.toDecoder decoder))

-- |
-- Zero or more rows packed into the list.
{-# INLINEABLE rowList #-}
rowList :: Row a -> Result [a]
rowList =
  foldrRows strictCons []
