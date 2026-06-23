module Hasql.Engine.Statement
  ( Statement (..),
    preparable,
    unpreparable,
    refineResult,
    toSql,
  )
where

import Data.Text.Encoding qualified as TextEncoding
import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Engine.Decoders.Result qualified as Decoders.Result
import Hasql.Kernel qualified as Kernel
import Hasql.Kernel.TypeInfo qualified as Kernel.TypeInfo
import Hasql.Platform.Prelude

-- |
-- Specification of a strictly single-statement query, which can be parameterized and prepared.
-- It encapsulates the mapping of parameters and results in association with an SQL template.
--
-- Following is an example of a declaration of a prepared statement with its associated codecs.
--
-- @
-- selectSum :: 'Statement' (Int64, Int64) Int64
-- selectSum =
--   'preparable' sql encoder decoder
--   where
--     sql =
--       \"select ($1 + $2)\"
--     encoder =
--       ('fst' '>$<' Encoders.'Hasql.Encoders.param' (Encoders.'Hasql.Encoders.nonNullable' Encoders.'Hasql.Encoders.int8')) '<>'
--       ('snd' '>$<' Encoders.'Hasql.Encoders.param' (Encoders.'Hasql.Encoders.nonNullable' Encoders.'Hasql.Encoders.int8'))
--     decoder =
--       Decoders.'Hasql.Decoders.singleRow' (Decoders.'Hasql.Decoders.column' (Decoders.'Hasql.Decoders.nonNullable' Decoders.'Hasql.Decoders.int8'))
-- @
--
-- The statement above accepts a product of two parameters of type 'Int64'
-- and produces a single result of type 'Int64'.
data Statement params result
  = Statement
  { -- | SQL template pre-encoded as UTF-8 for execution.
    sql :: ByteString,
    -- | Frozen per-parameter metadata: type reference, dimensionality, text-format flag.
    -- Produced once at construction from the Params DList and reused across executions.
    columnsMetadata :: Vector Params.ParamMeta,
    -- | Serialise params to encoded wire values given a resolved OID cache.
    serializer :: HashMap Kernel.QualifiedTypeName Kernel.TypeInfo.TypeInfo -> params -> [Maybe ByteString],
    -- | Render params in human-readable form (for error reporting).
    printer :: params -> [Text],
    -- | Union of encoder and decoder unknown types, resolved once at construction.
    unknownTypes :: HashSet Kernel.QualifiedTypeName,
    -- | Unwrapped result decoder (RequestingOid layer already peeled from Result).
    decoder :: RequestingOid.RequestingOid (ResultDecoder.ResultDecoder result),
    -- | Whether this statement may be prepared on the server.
    isPrepared :: Bool
  }

-- |
-- Construct a preparable statement.
--
-- Use this for statements that will be executed multiple times with different parameters.
-- Preparable statements are cached by PostgreSQL, which avoids reconstructing the execution plan each time.
--
-- Suitable for applications with a limited amount of queries that don't generate SQL dynamically.
preparable ::
  -- | SQL template with parameters in positional notation (@$1@, @$2@, etc.)
  Text ->
  -- | Parameters encoder
  Encoders.Params params ->
  -- | Result decoder
  Decoders.Result result ->
  Statement params result
preparable sqlText encoder resultDecoder =
  Statement
    { sql = TextEncoding.encodeUtf8 sqlText,
      columnsMetadata = Params.toColumnsMetadata encoder,
      serializer = Params.serializer encoder,
      printer = toList . Params.printer encoder,
      unknownTypes = Params.toUnknownTypes encoder <> RequestingOid.toUnknownTypes rawDecoder,
      decoder = rawDecoder,
      isPrepared = True
    }
  where
    rawDecoder = Decoders.Result.unwrap resultDecoder

-- |
-- Construct an unpreparable statement.
--
-- Use this for statements that are dynamically generated or executed only once.
-- Unpreparable statements are not cached by PostgreSQL.
--
-- Suitable for dynamic SQL or one-off queries.
unpreparable ::
  -- | SQL template with parameters in positional notation (@$1@, @$2@, etc.)
  Text ->
  -- | Parameters encoder
  Encoders.Params params ->
  -- | Result decoder
  Decoders.Result result ->
  Statement params result
unpreparable sqlText encoder resultDecoder =
  Statement
    { sql = TextEncoding.encodeUtf8 sqlText,
      columnsMetadata = Params.toColumnsMetadata encoder,
      serializer = Params.serializer encoder,
      printer = toList . Params.printer encoder,
      unknownTypes = Params.toUnknownTypes encoder <> RequestingOid.toUnknownTypes rawDecoder,
      decoder = rawDecoder,
      isPrepared = False
    }
  where
    rawDecoder = Decoders.Result.unwrap resultDecoder

instance Functor (Statement params) where
  {-# INLINE fmap #-}
  fmap f stmt = stmt {decoder = fmap (fmap f) (decoder stmt)}

instance Filterable (Statement params) where
  {-# INLINE mapMaybe #-}
  mapMaybe filtrator stmt = stmt {decoder = fmap (mapMaybe filtrator) (decoder stmt)}

instance Profunctor Statement where
  {-# INLINE dimap #-}
  dimap f1 f2 stmt =
    stmt
      { serializer = \oidCache -> serializer stmt oidCache . f1,
        printer = printer stmt . f1,
        decoder = fmap (fmap f2) (decoder stmt)
      }

-- |
-- Refine the result of a statement,
-- causing the running session to fail with the 'Hasql.Errors.UnexpectedResultStatementError' error in case of a refinement failure.
--
-- This function is especially useful for refining the results of statements produced with
-- <http://hackage.haskell.org/package/hasql-th the \"hasql-th\" library>.
refineResult :: (a -> Either Text b) -> Statement params a -> Statement params b
refineResult refiner stmt = stmt {decoder = fmap (ResultDecoder.refine refiner) (decoder stmt)}

-- | Extract the SQL template from a statement.
toSql :: Statement params result -> Text
toSql stmt = TextEncoding.decodeUtf8Lenient (sql stmt)
