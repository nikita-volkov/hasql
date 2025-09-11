module Hasql.TestingKit.TestingDsl
  ( -- * Errors
    Error (..),
    Session.SessionError (..),
    Session.CommandError (..),
    Session.ResultError (..),
    Session.RowError (..),

    -- * Abstractions
    Session.Session,
    Pipeline.Pipeline,
    Statement.Statement (..),

    -- * Execution
    runSessionOnLocalDb,
    runPipelineOnLocalDb,
    runSessionWithSettings,
    runPipelineWithSettings,
    runStatementInSession,
    runPipelineInSession,
  )
where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Constants qualified as Constants
import Hasql.TestingKit.Preludes.Base

data Error
  = ConnectionError (Connection.ConnectionError)
  | SessionError (Session.SessionError)
  deriving (Show, Eq)

runSessionOnLocalDb :: Session.Session a -> IO (Either Error a)
runSessionOnLocalDb = runSessionWithSettings Constants.localConnectionSettings

runPipelineOnLocalDb :: Pipeline.Pipeline a -> IO (Either Error a)
runPipelineOnLocalDb =
  runSessionOnLocalDb . Session.pipeline

runSessionWithSettings :: [Connection.Setting.Setting] -> Session.Session a -> IO (Either Error a)
runSessionWithSettings settings session =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ fmap (first ConnectionError) $ Connection.acquire settings
    use connection =
      ExceptT
        $ fmap (first SessionError)
        $ Connection.use connection session
    release connection =
      lift $ Connection.release connection

runPipelineWithSettings :: [Connection.Setting.Setting] -> Pipeline.Pipeline a -> IO (Either Error a)
runPipelineWithSettings settings =
  runSessionWithSettings settings . Session.pipeline

runStatementInSession :: Statement.Statement a b -> a -> Session.Session b
runStatementInSession statement params =
  Session.statement params statement

runPipelineInSession :: Pipeline.Pipeline a -> Session.Session a
runPipelineInSession =
  Session.pipeline
