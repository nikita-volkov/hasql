module Hasql.Session
  ( Session.Session,
    Session.pipeline,
    script,
    statement,
    statementLegacy,
    Session.onLibpqConnection,
  )
where

import Hasql.Engine.Contexts.Session qualified as Session
import Hasql.Engine.Statement qualified as Statement
import Hasql.Platform.Prelude

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
script :: Text -> Session.Session ()
script sql = Session.script (encodeUtf8 sql)

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session.Session result
statement params (Statement.Statement sql encoder decoder preparable) =
  Session.statement
    (encodeUtf8 sql)
    encoder
    decoder
    preparable
    params

-- |
-- Execute a statement by providing parameters to it,
-- using the legacy pipeline-wrapped path.
--
-- This is the pre-1.10 implementation retained for compatibility.
-- It routes every query through pipeline mode, which for a single statement
-- adds per-call overhead (entering and exiting pipeline mode, a pipeline sync,
-- and async-exception masking) without saving any network roundtrips.
--
-- Prefer the new 'statement' for direct serial execution,
-- or 'pipeline' when batching multiple statements.
--
-- __Warning:__ This operation is deprecated and will be removed before the
-- next release.
statementLegacy :: params -> Statement.Statement params result -> Session.Session result
statementLegacy params (Statement.Statement sql encoder decoder preparable) =
  Session.statementLegacy
    (encodeUtf8 sql)
    encoder
    decoder
    preparable
    params
