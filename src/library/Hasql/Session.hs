module Hasql.Session
  ( Session.Session,
    Session.pipeline,
    script,
    statement,
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
