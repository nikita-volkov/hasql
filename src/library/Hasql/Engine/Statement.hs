module Hasql.Engine.Statement
  ( Statement (..),
    preparable,
    unpreparable,
    refineResult,
    toSql,
    compilePreparedStatementData,
    compileUnpreparedStatementData,
  )
where

import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Hasql.Codecs.Encoders qualified as Encoders
import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.CodecsVocab qualified as CodecsVocab
import Hasql.CodecsVocab.TypeInfo qualified as CodecsVocab.TypeInfo
import Hasql.CodecsVocab.TypeRef qualified as CodecsVocab.TypeRef
import Hasql.CodecsVocab.TypeShape (TypeShape (..))
import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Engine.Decoders.Result qualified as Decoders
import Hasql.Engine.Decoders.Result qualified as Decoders.Result
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
    -- | Frozen per-parameter type shapes.
    -- Produced once at construction from the Params DList and reused across executions.
    columnsMetadata :: Vector TypeShape,
    -- | Serialise params to encoded wire values given a resolver of type names to their OIDs.
    serializer :: (CodecsVocab.QualifiedTypeName -> CodecsVocab.TypeInfo) -> params -> [Maybe ByteString],
    -- | Render params in human-readable form (for error reporting).
    printer :: params -> [Text],
    -- | Union of encoder and decoder unknown types, resolved once at construction.
    unknownTypes :: HashSet CodecsVocab.QualifiedTypeName,
    -- | Result decoder, given a resolver of type names to their OIDs.
    decoder :: (CodecsVocab.QualifiedTypeName -> CodecsVocab.TypeInfo) -> ResultDecoder.ResultDecoder result,
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
      serializer = Params.toSerializer encoder,
      printer = Params.toPrinter encoder,
      unknownTypes = Params.toUnknownTypes encoder <> Decoders.Result.toUnknownTypes resultDecoder,
      decoder = Decoders.Result.toBase resultDecoder,
      isPrepared = True
    }

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
      serializer = Params.toSerializer encoder,
      printer = Params.toPrinter encoder,
      unknownTypes = Params.toUnknownTypes encoder <> Decoders.Result.toUnknownTypes resultDecoder,
      decoder = Decoders.Result.toBase resultDecoder,
      isPrepared = False
    }

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
      { serializer = \resolve -> serializer stmt resolve . f1,
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

-- | Compile prepared-statement data: resolve OIDs and pair encoded values with their format flags.
compilePreparedStatementData ::
  Statement params result ->
  (CodecsVocab.QualifiedTypeName -> CodecsVocab.TypeInfo) ->
  params ->
  ([Word32], [Maybe (ByteString, Bool)])
compilePreparedStatementData stmt resolve params =
  unzip
    $ zipWith
      (\(TypeShape typeRef dim fmt) encoding -> (resolveOid resolve typeRef dim, fmap (,fmt) encoding))
      (Vector.toList (columnsMetadata stmt))
      (serializer stmt resolve params)

-- | Compile unprepared-statement data: resolve OIDs inline with encoded values.
compileUnpreparedStatementData ::
  Statement params result ->
  (CodecsVocab.QualifiedTypeName -> CodecsVocab.TypeInfo) ->
  params ->
  [Maybe (Word32, ByteString, Bool)]
compileUnpreparedStatementData stmt resolve params =
  zipWith
    (\(TypeShape typeRef dim fmt) encoding -> (,,) <$> Just (resolveOid resolve typeRef dim) <*> encoding <*> Just fmt)
    (Vector.toList (columnsMetadata stmt))
    (serializer stmt resolve params)

-- | Resolve a param's wire OID given the dictionary of resolved type names.
resolveOid :: (CodecsVocab.QualifiedTypeName -> CodecsVocab.TypeInfo) -> CodecsVocab.TypeRef.TypeRef -> Word -> Word32
resolveOid resolve (CodecsVocab.TypeRef.NamedType name) dim =
  (if dim == 0 then CodecsVocab.TypeInfo.toBaseOid else CodecsVocab.TypeInfo.toArrayOid) (resolve name)
resolveOid _ (CodecsVocab.TypeRef.KnownOid oid) _ = oid
