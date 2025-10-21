module Core.Decoders.Result where

import Codecs.Encoders.Params qualified as Params
import Core.Decoders.Row (Row (..))
import Core.Decoders.Row qualified as Row
import Core.Errors qualified as Errors
import Core.PqProcedures.SelectTypeInfo qualified as PqProcedures.SelectTypeInfo
import Core.Structures.OidCache qualified as OidCache
import Core.Structures.StatementCache qualified as StatementCache
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hipq.ResultDecoder qualified as ResultDecoder
import Hipq.Roundtrip qualified
import Platform.Prelude
import Pq qualified

data Result a
  = Result
      (HashSet (Maybe Text, Text))
      ( HashMap (Maybe Text, Text) (Word32, Word32) ->
        ResultDecoder.ResultDecoder a
      )

deriving instance Functor Result

instance Applicative Result where
  pure a = Result HashSet.empty (\_oidCache -> pure a)
  Result lUnknownTypes lDec <*> Result rUnknownTypes rDec =
    Result
      (lUnknownTypes <> rUnknownTypes)
      ( \oidCache ->
          lDec oidCache <*> rDec oidCache
      )

instance Filterable Result where
  {-# INLINE mapMaybe #-}
  mapMaybe fn (Result unknownTypes decoder) =
    Result unknownTypes (mapMaybe fn . decoder)

toUnknownTypes :: Result a -> HashSet (Maybe Text, Text)
toUnknownTypes (Result unknownTypes _) = unknownTypes

toDecoder ::
  Result a ->
  HashMap (Maybe Text, Text) (Word32, Word32) ->
  ResultDecoder.ResultDecoder a
toDecoder (Result _unknownTypes decoder) oidCache =
  decoder oidCache

-- * Construction

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
{-# INLINEABLE noResult #-}
noResult :: Result ()
noResult =
  Result mempty (const ResultDecoder.ok)

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
{-# INLINEABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  Result mempty (const ResultDecoder.rowsAffected)

-- |
-- Exactly one row.
-- Will raise the 'Errors.UnexpectedAmountOfRows' error if it's any other.
{-# INLINEABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row unknownTypes decoder) =
  Result unknownTypes (ResultDecoder.single . decoder)

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result unknownTypes resultDecoder) =
  Result unknownTypes (ResultDecoder.refine refiner . resultDecoder)

-- ** Multi-row traversers

-- |
-- Foldl multiple rows.
{-# INLINEABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init (Row unknownTypes row) =
  Result unknownTypes \oidCache -> ResultDecoder.foldl step init (row oidCache)

-- |
-- Foldr multiple rows.
{-# INLINEABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row unknownTypes row) =
  Result unknownTypes \oidCache -> ResultDecoder.foldr step init (row oidCache)

-- ** Specialized multi-row results

-- |
-- Maybe one row or none.
{-# INLINEABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe (Row unknownTypes row) =
  Result unknownTypes \oidCache -> ResultDecoder.maybe (row oidCache)

-- |
-- Zero or more rows packed into the vector.
--
-- It's recommended to prefer this function to 'rowList',
-- since it performs notably better.
{-# INLINEABLE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector (Row unknownTypes row) =
  Result unknownTypes \oidCache -> ResultDecoder.vector (row oidCache)

-- |
-- Zero or more rows packed into the list.
{-# INLINEABLE rowList #-}
rowList :: Row a -> Result [a]
rowList =
  foldrRows strictCons []
