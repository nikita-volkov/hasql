module Hasql.Session
  ( Session.Session,
    Session.pipeline,
    Session.script,
    statement,
    Session.onLibpqConnection,

    -- * Execution
    run,
  )
where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.Session qualified as Session
import Core.UsageError
import Hasql.Connection qualified as Connection
import Hasql.Statement qualified as Statement
import Hipq.ResultDecoder qualified as ResultDecoder
import Platform.Prelude

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection.
{-# DEPRECATED run "Use @Hasql.Connection.'Hasql.Connection.use'@ instead" #-}
run :: Session.Session a -> Connection.Connection -> IO (Either UsageError a)
run session connection = Connection.use connection session

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session.Session result
statement params (Statement.Statement sql paramsEncoder decoder preparable) =
  Session.statement
    sql
    (ParamsEncoder.unwrap paramsEncoder)
    (ResultDecoder.unwrap decoder)
    preparable
    params
