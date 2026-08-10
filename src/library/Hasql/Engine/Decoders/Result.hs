module Hasql.Engine.Decoders.Result where

import Hasql.CodecsCore.QualifiedTypeName qualified as CodecsCore.QualifiedTypeName
import Hasql.CodecsCore.TypeInfo qualified as CodecsCore.TypeInfo
import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Engine.Decoders.Row (Row (..))
import Hasql.Engine.Decoders.Row qualified as Row
import Hasql.Platform.Prelude
import Hasql.ToBeResolved qualified as ToBeResolved

-- |
-- Decoder of a query result.
newtype Result a
  = Result (ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo (ResultDecoder.ResultDecoder a))
  deriving
    (Functor, Applicative, Filterable)
    via (Compose (ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo) ResultDecoder.ResultDecoder)

-- | Names of types that must be looked up at runtime before the decoder can run.
toUnknownTypes :: Result a -> HashSet CodecsCore.QualifiedTypeName.QualifiedTypeName
toUnknownTypes (Result (ToBeResolved.ToBeResolved unknownTypes _)) = fromList unknownTypes

-- | Resolve the decoder given a resolver of type names to their OIDs.
toBase :: Result a -> (CodecsCore.QualifiedTypeName.QualifiedTypeName -> CodecsCore.TypeInfo.TypeInfo) -> ResultDecoder.ResultDecoder a
toBase (Result (ToBeResolved.ToBeResolved _ decoder)) = decoder

-- * Construction

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
{-# INLINE noResult #-}
noResult :: Result ()
noResult =
  Result (ToBeResolved.lift ResultDecoder.ok)

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
{-# INLINE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  Result (ToBeResolved.lift ResultDecoder.rowsAffected)

-- |
-- Exactly one row.
-- Will raise the 'Hasql.Errors.UnexpectedRowCountStatementError' error if it's any other.
{-# INLINE singleRow #-}
singleRow :: Row a -> Result a
singleRow decoder =
  Result (fmap ResultDecoder.single (Row.toDecoder decoder))

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result decoder) =
  Result (fmap (ResultDecoder.refine refiner) decoder)

-- ** Multi-row traversers

-- |
-- Foldl multiple rows.
{-# INLINE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init decoder =
  Result
    (fmap (ResultDecoder.foldl step init) (Row.toDecoder decoder))

-- |
-- Foldr multiple rows.
{-# INLINE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init decoder =
  Result
    (fmap (ResultDecoder.foldr step init) (Row.toDecoder decoder))

-- ** Specialized multi-row results

-- |
-- Maybe one row or none.
{-# INLINE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe decoder =
  Result
    (fmap ResultDecoder.maybe (Row.toDecoder decoder))

-- |
-- Zero or more rows packed into the vector.
--
-- It's recommended to prefer this function to 'rowList',
-- since it performs notably better.
{-# INLINE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector decoder =
  Result
    (fmap ResultDecoder.vector (Row.toDecoder decoder))

-- |
-- Zero or more rows packed into the list.
{-# INLINE rowList #-}
rowList :: Row a -> Result [a]
rowList =
  foldrRows strictCons []
