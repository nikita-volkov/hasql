module Hasql.Session
  ( Session,
    sql,
    statement,
    pipeline,

    -- * Execution
    run,

    -- * Errors
    module Hasql.Errors,
  )
where

import Hasql.Connection.Core qualified as Connection
import Hasql.Errors
import Hasql.Prelude
import Hasql.Session.Core hiding (run)

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection.
run :: Session a -> Connection.Connection -> IO (Either SessionError a)
run session connection = Connection.use connection session
