module Hasql.Engine.Statement
  ( Statement (..),
    statement,
    refineResult,
    toSql,
    compileStatementData,
    inlineOids,
  )
where

import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Hasql.Codecs.Encoders qualified as Encoders
import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Codecs.Vocab qualified as Vocab
import Hasql.Codecs.Vocab.OidCache qualified as Vocab.OidCache
import Hasql.Codecs.Vocab.ParamMeta (ParamMeta (..))
import Hasql.Codecs.Vocab.TypeRef qualified as Vocab.TypeRef
import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Engine.Decoders.Result qualified as Decoders
import Hasql.Engine.Decoders.Result qualified as Decoders.Result
import Hasql.Platform.Prelude

-- |
-- Specification of a strictly single-statement query, which can be parameterized and prepared.
-- It encapsulates the mapping of parameters and results in association with an SQL template.
--
-- Following is an example of a declaration of a statement with its associated codecs.
--
-- @
-- selectSum :: 'Statement' (Int64, Int64) Int64
-- selectSum =
--   'statement' sql encoder decoder
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
    columnsMetadata :: Vector ParamMeta,
    -- | Serialise params to encoded wire values given a resolved OID cache.
    serializer :: Vocab.OidCache -> params -> [Maybe ByteString],
    -- | Render params in human-readable form (for error reporting).
    printer :: params -> [Text],
    -- | Union of encoder and decoder unknown types, resolved once at construction.
    unknownTypes :: HashSet Vocab.QualifiedTypeName,
    -- | Unwrapped result decoder (RequestingOid layer already peeled from Result).
    decoder :: RequestingOid.RequestingOid (ResultDecoder.ResultDecoder result)
  }

-- |
-- Construct a statement.
--
-- Whether it ends up prepared on the server is not your decision to make: the
-- driver observes how often each statement is actually executed on each
-- connection and prepares the ones that earn it, deallocating the ones that
-- fall out of use. See 'Hasql.Connection.Settings.statementCacheSize' and
-- 'Hasql.Connection.Settings.prepareThreshold' for the connection-level
-- controls over that.
--
-- If a specific statement suffers from PostgreSQL choosing a generic plan for
-- it, set @plan_cache_mode = force_custom_plan@ for it in the database, which
-- is both the correct fix and finer-grained than anything this library could
-- offer.
statement ::
  -- | SQL template with parameters in positional notation (@$1@, @$2@, etc.)
  Text ->
  -- | Parameters encoder
  Encoders.Params params ->
  -- | Result decoder
  Decoders.Result result ->
  Statement params result
statement sqlText encoder resultDecoder =
  Statement
    { sql = TextEncoding.encodeUtf8 sqlText,
      columnsMetadata = Params.toColumnsMetadata encoder,
      serializer = Params.toSerializer encoder,
      printer = Params.toPrinter encoder,
      unknownTypes = Params.toUnknownTypes encoder <> RequestingOid.toUnknownTypes rawDecoder,
      decoder = rawDecoder
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
toSql stmt = decodeUtf8Lenient (sql stmt)

-- | Resolve the parameter OIDs and encode the parameter values.
--
-- Both execution paths need both halves — the prepared one to declare the
-- parameter types at @PARSE@ time and to identify the statement in the cache,
-- the unprepared one to inline the types into the query itself — so this is
-- computed once per execution regardless of how the statement is executed.
compileStatementData ::
  Statement params result ->
  Vocab.OidCache ->
  params ->
  ([Word32], [Maybe (ByteString, Bool)])
compileStatementData stmt oidCache params =
  unzip
    $ zipWith
      (\(ParamMeta typeRef dim fmt) encoding -> (resolveOid typeRef dim, fmap (,fmt) encoding))
      (Vector.toList (columnsMetadata stmt))
      (serializer stmt oidCache params)
  where
    resolveOid (Vocab.TypeRef.NamedType name) dim =
      case Vocab.OidCache.lookupTypeNameScalar name oidCache of
        Just oid -> if dim == 0 then oid else fromMaybe 0 (Vocab.OidCache.lookupTypeNameArray name oidCache)
        Nothing -> 0
    resolveOid (Vocab.TypeRef.KnownOid oid) _ = oid

-- | Fold the resolved OIDs into the encoded values, as the unprepared
-- execution path needs them: it declares each parameter's type inline instead
-- of having declared them at @PARSE@ time.
inlineOids :: [oid] -> [Maybe (value, format)] -> [Maybe (oid, value, format)]
inlineOids =
  zipWith (\oid -> fmap (\(value, format) -> (oid, value, format)))
