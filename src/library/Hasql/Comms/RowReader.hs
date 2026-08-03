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
import Hasql.Pq qualified as Pq

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

-- | Outcome of running a 'RowReader' starting from a given column: either it
-- failed, or it produced a value along with the column position to resume
-- decoding from.
data Outcome a
  = Failure Error
  | Success a Pq.Column

-- | A row reader is a closure threading the current column position
-- explicitly through its arguments and return value, instead of through a
-- monad transformer stack. Composing readers with '(<*>)' therefore costs
-- nothing beyond the direct 'IO' calls each one performs.
newtype RowReader a
  = RowReader (Pq.Result -> Pq.Row -> Pq.Column -> IO (Outcome a))

instance Functor RowReader where
  {-# INLINE fmap #-}
  fmap f (RowReader run) = RowReader \result row col -> do
    outcome <- run result row col
    pure case outcome of
      Failure err -> Failure err
      Success a col' -> Success (f a) col'

instance Applicative RowReader where
  {-# INLINE pure #-}
  pure a = RowReader \_ _ col -> pure (Success a col)
  {-# INLINE (<*>) #-}
  RowReader runF <*> RowReader runA = RowReader \result row col -> do
    outcomeF <- runF result row col
    case outcomeF of
      Failure err -> pure (Failure err)
      Success f col' -> do
        outcomeA <- runA result row col'
        pure case outcomeA of
          Failure err -> Failure err
          Success a col'' -> Success (f a) col''

instance Filterable RowReader where
  {-# INLINE mapMaybe #-}
  mapMaybe fn (RowReader run) = RowReader \result row col -> do
    outcome <- run result row col
    pure case outcome of
      Failure err -> Failure err
      Success a col' -> case fn a of
        Just refined -> Success refined col'
        Nothing -> Failure (RefinementError "Filtration failed")

-- * Functions

{-# INLINE toHandler #-}
toHandler :: RowReader a -> Pq.Result -> Pq.Row -> IO (Either Error a)
toHandler (RowReader run) result row = do
  outcome <- run result row 0
  pure case outcome of
    Failure err -> Left err
    Success a _ -> Right a

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: (Maybe a -> Maybe b) -> (ByteString -> Either Text a) -> RowReader b
column processNullable valueDec = RowReader \result row col -> do
  rawMaybe <- {-# SCC "getvalue'" #-} Pq.getvalue' result row col

  decodedMaybe <- case rawMaybe of
    Nothing -> pure (Right Nothing)
    Just v -> case {-# SCC "decode" #-} valueDec v of
      Left err -> do
        oid <- Pq.oidToWord32 <$> Pq.ftype result col
        pure (Left (CellError (Pq.colToInt col) oid (DecodingCellError err)))
      Right decoded -> pure (Right (Just decoded))

  case decodedMaybe of
    Left err -> pure (Failure err)
    Right valueMaybe -> case processNullable valueMaybe of
      Nothing -> do
        oid <- Pq.oidToWord32 <$> Pq.ftype result col
        pure (Failure (CellError (Pq.colToInt col) oid UnexpectedNullCellError))
      Just decoded -> pure (Success decoded (succ col))

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
