-- | Lower level context focused on just the actual decoding of values. No metadata involved.
--
-- The reader is in continuation-passing style rather than a
-- @StateT \/ ReaderT \/ ExceptT@ stack. The stack is the more readable
-- formulation, but its 'Applicative' allocates for every column of every row:
-- @StateT@'s bind returns a @(value, columnIndex)@ pair and @ExceptT@'s wraps
-- that pair in an 'Either', so a three-column row decoder built with @<*>@
-- allocates three pairs, three 'Either's and the partial applications of the
-- constructor being filled -- all of it dead the instant the row is consumed.
--
-- In CPS there is nothing to allocate: the column index is an argument rather
-- than state threaded through a pair, and failure is a continuation rather than
-- an 'Either' to inspect. With the 'Functor' \/ 'Applicative' methods inlined, a
-- decoder whose shape is known at its definition site collapses into
-- straight-line code that reads each column and hands the finished value to the
-- success continuation.
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
import Pqi qualified as Pq

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

-- | Decoder of one row, as a function awaiting its continuations.
--
-- The success continuation takes the index of the next unread column along with
-- the decoded value, which is what replaces the state in the transformer
-- formulation.
newtype RowReader a
  = RowReader
      ( forall r.
        Pq.Result ->
        -- Row index.
        Int32 ->
        -- Index of the next column to read.
        Int32 ->
        -- Failure.
        (Error -> IO r) ->
        -- Success, given the next unread column and the value.
        (Int32 -> a -> IO r) ->
        IO r
      )

-- * Instances

instance Functor RowReader where
  {-# INLINE fmap #-}
  fmap fn (RowReader run) =
    RowReader \result row col onError onSuccess ->
      run result row col onError \col' a -> onSuccess col' (fn a)

instance Applicative RowReader where
  {-# INLINE pure #-}
  pure a =
    RowReader \_result _row col _onError onSuccess -> onSuccess col a
  {-# INLINE (<*>) #-}
  RowReader runFn <*> RowReader runArg =
    RowReader \result row col onError onSuccess ->
      runFn result row col onError \col' fn ->
        runArg result row col' onError \col'' a ->
          onSuccess col'' (fn a)

instance Filterable RowReader where
  {-# INLINE mapMaybe #-}
  mapMaybe fn (RowReader run) =
    RowReader \result row col onError onSuccess ->
      run result row col onError \col' a ->
        case fn a of
          Just refined -> onSuccess col' refined
          Nothing -> onError (RefinementError "Filtration failed")

-- * Functions

-- |
-- Run the reader over one row, collecting the outcome into an 'Either'.
--
-- Kept for callers that want the row as a value. The one in the hot path
-- (@ResultDecoder.vector@) uses the 'RowReader' directly instead, so that the
-- decoded row is handed straight to its consumer without an 'Either' in
-- between.
{-# INLINE toHandler #-}
toHandler :: RowReader a -> Pq.Result -> Int32 -> IO (Either Error a)
toHandler (RowReader run) result row =
  run result row 0 (pure . Left) (\_col a -> pure (Right a))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE column #-}
column :: (Maybe a -> Maybe b) -> (ByteString -> Either Text a) -> RowReader b
column processNullable valueDec =
  RowReader \result row col onError onSuccess ->
    let colInt = fromIntegral col
        col' = succ col

        unexpectedNull = do
          oid <- Pq.ftype result col
          onError (CellError colInt oid UnexpectedNullCellError)

        emit valueMaybe =
          case processNullable valueMaybe of
            Just decoded -> onSuccess col' decoded
            Nothing -> unexpectedNull
     in do
          valueMaybe <- {-# SCC "getvalue'" #-} Pq.getvalue' result row col
          case valueMaybe of
            Nothing -> emit Nothing
            Just v ->
              case {-# SCC "decode" #-} valueDec v of
                Left err -> do
                  oid <- Pq.ftype result col
                  onError (CellError colInt oid (DecodingCellError err))
                Right decoded -> emit (Just decoded)

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
