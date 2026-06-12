module Hasql.Session
  ( Session.Session,
    Session.pipeline,
    script,
    statement,
    statementSerial,
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
-- Execute a statement using a direct serial path, bypassing pipeline mode.
--
-- This is an opt-in alternative to 'statement'. 'statement' routes every query
-- through pipeline mode, which for a single statement adds per-call overhead
-- (entering and exiting pipeline mode, a pipeline sync, and async-exception
-- masking) without saving any network roundtrips. 'statementSerial' issues the
-- statement directly instead.
--
-- The first execution of a preparable statement costs an extra network roundtrip
-- (a separate @PARSE@), after which steady-state execution is a single roundtrip.
--
-- To batch multiple statements into fewer roundtrips, use 'pipeline' instead.
statementSerial :: params -> Statement.Statement params result -> Session.Session result
statementSerial params (Statement.Statement sql encoder decoder preparable) =
  Session.statementSerial
    (encodeUtf8 sql)
    encoder
    decoder
    preparable
    params
