-- | Lower level context focused on just the actual decoding of values. No metadata involved.
module Hasql.Comms.RowReader
  ( RowReader,
    nullableColumn,
    nonNullableColumn,

    -- * Errors
    Error (..),
    CellError (..),

    -- * Relations
    toHandler,
  )
where

import Hasql.Platform.Prelude
import Pqi qualified

data Error
  = CellError
      -- | Column index, 0-based.
      Int
      -- | OID of the column type as reported by Postgres.
      Word32
      -- | Underlying error.
      CellError
  | RefinementError Text
  deriving stock (Eq, Show)

data CellError
  = DecodingCellError Text
  | UnexpectedNullCellError
  deriving stock (Eq, Show)

newtype RowReader a
  = RowReader (forall r. (Pqi.IsResult r) => r -> Int32 -> Int32 -> IO (Either Error (a, Int32)))

-- * Instances

instance Functor RowReader where
  {-# INLINE fmap #-}
  fmap f (RowReader g) = RowReader $ \r row col ->
    fmap (fmap (\(a, col') -> (f a, col'))) (g r row col)

instance Applicative RowReader where
  {-# INLINE pure #-}
  pure a = RowReader $ \_ _ col -> pure (Right (a, col))
  {-# INLINE (<*>) #-}
  RowReader ff <*> RowReader fa = RowReader $ \r row col -> do
    efc <- ff r row col
    case efc of
      Left err -> pure (Left err)
      Right (f, col') -> do
        eac <- fa r row col'
        pure (fmap (\(a, col'') -> (f a, col'')) eac)

instance Filterable RowReader where
  {-# INLINE mapMaybe #-}
  mapMaybe fn (RowReader run) = RowReader $ \r row col -> do
    res <- run r row col
    case res of
      Left err -> pure (Left err)
      Right (a, col') -> case fn a of
        Just b -> pure (Right (b, col'))
        Nothing -> pure (Left (RefinementError "Filtration failed"))

-- * Functions

{-# INLINE toHandler #-}
toHandler :: (Pqi.IsResult result) => RowReader a -> result -> Int32 -> IO (Either Error a)
toHandler (RowReader f) result row =
  fmap (fmap fst) (f result row 0)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: (Maybe a -> Maybe b) -> (ByteString -> Either Text a) -> RowReader b
column processNullable valueDec = RowReader $ \r row col -> do
  let colInt = fromIntegral col :: Int
  valueMaybe <- {-# SCC "getvalue'" #-} Pqi.getvalue' r row col
  decodedMaybe <- case valueMaybe of
    Nothing -> pure (Right Nothing)
    Just v -> do
      oid <- Pqi.ftype r col
      pure $ case {-# SCC "decode" #-} valueDec v of
        Left err -> Left (CellError colInt oid (DecodingCellError err))
        Right decoded -> Right (Just decoded)
  case decodedMaybe of
    Left err -> pure (Left err)
    Right valueMaybe' -> case processNullable valueMaybe' of
      Nothing -> do
        oid <- Pqi.ftype r col
        pure (Left (CellError colInt oid UnexpectedNullCellError))
      Just decoded -> pure (Right (decoded, col + 1))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: (ByteString -> Either Text a) -> RowReader (Maybe a)
nullableColumn = column Just

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: (ByteString -> Either Text a) -> RowReader a
nonNullableColumn = column id
