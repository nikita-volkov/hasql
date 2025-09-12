module Core.Contexts.RowDecoder
  ( RowDecoder,

    -- * Constructors
    nullableColumn,
    nonNullableColumn,

    -- * Relations

    -- ** Handler
    Handler,
    toHandler,
  )
where

import Core.Contexts.ValueDecoder qualified as ValueDecoder
import Core.Errors
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import Pq qualified

newtype RowDecoder a
  = RowDecoder (Env -> IO (Either ResultError a))
  deriving (Functor, Applicative, Monad) via (ReaderT Env (ExceptT ResultError IO))

instance MonadFail RowDecoder where
  fail = rowError . ValueError . fromString

data Env
  = Env
      Pq.Result
      Pq.Row
      Pq.Column
      Bool
      (IORef Pq.Column)

-- * Functions

{-# INLINE rowError #-}
rowError :: RowError -> RowDecoder a
rowError x = RowDecoder \(Env _ row _ _ columnRef) -> do
  col <- readIORef columnRef
  pure (Left (RowError (Pq.rowToInt row) (Pq.colToInt (pred col)) x))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nullableColumn #-}
nullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder (Maybe a)
nullableColumn valueDec =
  {-# SCC "nullableColumn" #-}
  RowDecoder \(Env result row columnsAmount integerDatetimes columnRef) -> do
    col <- readIORef columnRef
    let packRowError err = RowError (Pq.rowToInt row) (Pq.colToInt col) err
    writeIORef columnRef (succ col)
    if col < columnsAmount
      then do
        valueMaybe <- {-# SCC "getvalue'" #-} Pq.getvalue' result row col
        pure case valueMaybe of
          Nothing -> Right Nothing
          Just value ->
            fmap Just
              $ first (packRowError . ValueError)
              $ {-# SCC "decode" #-} A.valueParser (ValueDecoder.toHandler valueDec integerDatetimes) value
      else pure (Left (packRowError EndOfInput))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullableColumn #-}
nonNullableColumn :: ValueDecoder.ValueDecoder a -> RowDecoder a
nonNullableColumn valueDec =
  {-# SCC "nonNullableColumn" #-}
  nullableColumn valueDec >>= maybe (rowError UnexpectedNull) pure

-- * Relations

-- ** Handler

type Handler a = Bool -> Pq.Row -> Pq.Column -> Pq.Result -> IO (Either ResultError a)

toHandler :: RowDecoder a -> Handler a
toHandler (RowDecoder run) integerDatetimes row columnsAmount result = do
  columnRef <- newIORef 0
  run (Env result row columnsAmount integerDatetimes columnRef)
