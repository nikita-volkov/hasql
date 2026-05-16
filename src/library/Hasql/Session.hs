module Hasql.Session
  ( Session,
    pipeline,
    script,
    statement,
    onLibpqConnection,
  )
where

import Hasql.Engine.Contexts.Session qualified as Session
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.Statement qualified as Statement
import Hasql.Pipeline qualified as Pipeline
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

-- | A session bound to the libpq connection type.
type Session a = Session.Session Pq.Connection a

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline a -> Session a
pipeline = Session.pipeline

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
script :: Text -> Session ()
script sql = Session.script (encodeUtf8 sql)

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session result
statement params (Statement.Statement sql encoder decoder preparable) =
  Session.statement
    (encodeUtf8 sql)
    encoder
    decoder
    preparable
    params

-- |
-- Execute an operation on the raw libpq connection possibly producing an error and updating the connection.
-- This is a low-level escape hatch for custom integrations.
--
-- You can supply a new connection in the result to replace it in the running Hasql connection.
-- The responsibility to close the old libpq connection is on you.
-- Otherwise, just return the same connection you've received.
--
-- Producing a 'Left' value will cause the session to fail with the given error.
-- Regardless of success or failure, the connection will be replaced with the one you return.
--
-- Throwing exceptions is okay. It will lead to the connection getting reset.
onLibpqConnection ::
  (Pq.Connection -> IO (Either Errors.SessionError a, Pq.Connection)) ->
  Session a
onLibpqConnection = Session.onConnection
