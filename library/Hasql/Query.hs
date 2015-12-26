module Hasql.Query
(
  Query(..),
  -- * Execution
  ResultsError(..),
  ResultError(..),
  RowError(..),
  run,
)
where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.PreparedStatementRegistry as PreparedStatementRegistry
import qualified Hasql.Decoders.Results as ResultsDecoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders.Params as ParamsEncoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Settings as Settings
import qualified Hasql.IO as IO
import qualified Hasql.Connection.Impl as Connection


-- |
-- An error of the result-decoder.
data ResultsError =
  -- |
  -- An error on the client-side,
  -- with a message generated by the \"libpq\" library.
  -- Usually indicates problems with connection.
  ClientError !(Maybe ByteString) |
  -- |
  -- Decoder error details.
  ResultError !ResultError
  deriving (Show, Eq)

-- |
-- Decoder error details.
data ResultError =
  -- |
  -- An error reported by the DB.
  -- Consists of the following: Code, message, details, hint.
  --
  -- * __Code__.
  -- The SQLSTATE code for the error.
  -- It's recommended to use
  -- <http://hackage.haskell.org/package/postgresql-error-codes the "postgresql-error-codes" package>
  -- to work with those.
  --
  -- * __Message__.
  -- The primary human-readable error message (typically one line). Always present.
  --
  -- * __Details__.
  -- An optional secondary error message carrying more detail about the problem.
  -- Might run to multiple lines.
  --
  -- * __Hint__.
  -- An optional suggestion on what to do about the problem.
  -- This is intended to differ from detail in that it offers advice (potentially inappropriate)
  -- rather than hard facts.
  -- Might run to multiple lines.
  ServerError !ByteString !ByteString !(Maybe ByteString) !(Maybe ByteString) |
  -- |
  -- The database returned an unexpected result.
  -- Indicates an improper statement or a schema mismatch.
  UnexpectedResult !Text |
  -- |
  -- An error of the row reader, preceded by the index of the row.
  RowError !Int !RowError |
  -- |
  -- An unexpected amount of rows.
  UnexpectedAmountOfRows !Int
  deriving (Show, Eq)

-- |
-- An error during the decoding of a specific row.
data RowError =
  -- |
  -- Appears on the attempt to parse more columns than there are in the result.
  EndOfInput |
  -- |
  -- Appears on the attempt to parse a @NULL@ as some value.
  UnexpectedNull |
  -- |
  -- Appears when a wrong value parser is used.
  -- Comes with the error details.
  ValueError !Text
  deriving (Show, Eq)


-- |
-- A specification of a strictly single-statement query, which can be parameterized and prepared.
--
-- Consists of the following:
--
-- * SQL template,
-- * params encoder,
-- * result decoder,
-- * a flag, determining whether it should be prepared.
--
-- The SQL template must be formatted according to Postgres' standard,
-- with any non-ASCII characters of the template encoded using UTF-8.
-- According to the format,
-- parameters must be referred to using the positional notation, as in the following:
-- @$1@, @$2@, @$3@ and etc.
-- Those references must be used to refer to the values of the 'Encoders.Params' encoder.
--
-- Following is an example of the declaration of a prepared statement with its associated codecs.
--
-- @
-- selectSum :: Hasql.'Hasql.Query' (Int64, Int64) Int64
-- selectSum =
--   Hasql.'Hasql.Query' sql encoder decoder True
--   where
--     sql =
--       "select ($1 + $2)"
--     encoder =
--       'contramap' 'fst' (Hasql.Encoders.'Hasql.Encoders.value' Hasql.Encoders.'Hasql.Encoders.int8') '<>'
--       'contramap' 'snd' (Hasql.Encoders.'Hasql.Encoders.value' Hasql.Encoders.'Hasql.Encoders.int8')
--     decoder =
--       Hasql.Decoders.'Hasql.Decoders.singleRow' (Hasql.Decoders.'Hasql.Decoders.value' Hasql.Decoders.'Hasql.Decoders.int8')
-- @
--
-- The statement above accepts a product of two parameters of type 'Int64'
-- and produces a single result of type 'Int64'.
--
data Query a b =
  Query !ByteString !(Encoders.Params a) !(Decoders.Result b) !Bool
  deriving (Functor)

instance Profunctor Query where
  {-# INLINE lmap #-}
  lmap f (Query p1 p2 p3 p4) =
    Query p1 (contramap f p2) p3 p4
  {-# INLINE rmap #-}
  rmap f (Query p1 p2 p3 p4) =
    Query p1 p2 (fmap f p3) p4
  {-# INLINE dimap #-}
  dimap f1 f2 (Query p1 p2 p3 p4) =
    Query p1 (contramap f1 p2) (fmap f2 p3) p4

-- |
-- Execute the query, producing either a deserialization failure or a successful result.
run :: Query a b -> a -> Connection.Connection -> IO (Either ResultsError b)
run (Query template encoder decoder preparable) params (Connection.Connection pqConnectionRef integerDatetimes registry) =
  {-# SCC "query" #-}
  Connection.withConnectionRef pqConnectionRef $ \pqConnection ->
    fmap (mapLeft coerceResultsError) $ runEitherT $ do
      EitherT $ IO.sendParametricQuery pqConnection integerDatetimes registry template (coerceEncoder encoder) preparable params
      EitherT $ IO.getResults pqConnection integerDatetimes (coerceDecoder decoder)

-- |
-- WARNING: We need to take special care that the structure of
-- the "ResultsDecoders.Error" type in the public API is an exact copy of
-- "Error", since we're using coercion.
coerceResultsError :: ResultsDecoders.Error -> ResultsError
coerceResultsError =
  unsafeCoerce

coerceDecoder :: Decoders.Result a -> ResultsDecoders.Results a
coerceDecoder =
  unsafeCoerce

coerceEncoder :: Encoders.Params a -> ParamsEncoders.Params a
coerceEncoder =
  unsafeCoerce
