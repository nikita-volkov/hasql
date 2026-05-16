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

import Hasql.Driver.Interface qualified as Interface
import Hasql.Platform.Prelude

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
  = RowReader
      { runRowReader ::
          forall result.
          Interface.ResultDriver result ->
          result ->
          -- | Row index.
          Int ->
          -- | Column index (current state).
          Int ->
          IO (Either Error (a, Int))
      }

-- * Instances

instance Functor RowReader where
  {-# INLINE fmap #-}
  fmap f (RowReader g) = RowReader \rd result row col ->
    fmap (fmap (first f)) (g rd result row col)

instance Applicative RowReader where
  {-# INLINE pure #-}
  pure a = RowReader \_ _ _ col -> pure (Right (a, col))
  {-# INLINE (<*>) #-}
  RowReader gf <*> RowReader ga = RowReader \rd result row col -> do
    ef <- gf rd result row col
    case ef of
      Left err -> pure (Left err)
      Right (f, col') -> do
        ea <- ga rd result row col'
        case ea of
          Left err -> pure (Left err)
          Right (a, col'') -> pure (Right (f a, col''))

instance Filterable RowReader where
  {-# INLINE mapMaybe #-}
  mapMaybe fn (RowReader g) = RowReader \rd result row col -> do
    e <- g rd result row col
    case e of
      Left err -> pure (Left err)
      Right (a, col') -> case fn a of
        Just b -> pure (Right (b, col'))
        Nothing -> pure (Left (RefinementError "Filtration failed"))

-- * Functions

{-# INLINE toHandler #-}
toHandler :: RowReader a -> Interface.ResultDriver result -> result -> Int -> IO (Either Error a)
toHandler (RowReader f) rd result row =
  fmap (fmap fst) (f rd result row 0)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: (Maybe a -> Maybe b) -> (ByteString -> Either Text a) -> RowReader b
column processNullable valueDec = RowReader \rd result row col -> runExceptT do
  valueMaybe <- liftIO ({-# SCC "getvalue'" #-} Interface.rdGetValue rd result row col)
  valueMaybe' <- case valueMaybe of
    Nothing -> pure Nothing
    Just v ->
      liftIO (Interface.rdFtype rd result col) >>= \oid ->
        case {-# SCC "decode" #-} valueDec v of
          Left err -> throwError (CellError col oid (DecodingCellError err))
          Right decoded -> pure (Just decoded)
  case processNullable valueMaybe' of
    Nothing -> do
      oid <- liftIO (Interface.rdFtype rd result col)
      throwError (CellError col oid UnexpectedNullCellError)
    Just decoded -> pure (decoded, col + 1)

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
