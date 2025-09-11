module Hasql.Session
  ( Session.Session,
    Session.pipeline,
    Session.sql,
    statement,
    Session.onLibpqConnection,

    -- * Execution
    run,

    -- * Errors
    module Core.Errors,
  )
where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.ResultsDecoder qualified as ResultsDecoder
import Core.Contexts.Session qualified as Session
import Core.Errors
import Hasql.Connection qualified as Connection
import Hasql.Statement qualified as Statement
import Platform.Prelude

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection.
{-# DEPRECATED run "Use @Hasql.Connection.'Hasql.Connection.use'@ instead" #-}
run :: Session.Session a -> Connection.Connection -> IO (Either SessionError a)
run session connection = Connection.use connection session

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session.Session result
statement
  params
  ( Statement.Statement
      sql
      (ParamsEncoder.unwrap -> paramsEncoder)
      (ResultsDecoder.unwrap -> decoder)
      preparable
    ) =
    Session.statement sql paramsEncoder decoder preparable params
