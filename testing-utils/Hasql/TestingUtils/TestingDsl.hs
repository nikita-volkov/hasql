module Hasql.TestingUtils.TestingDsl
  ( Session.Session,
    Error (..),
    Session.SessionError (..),
    Session.CommandError (..),
    Pipeline.Pipeline,
    Statement.Statement (..),
    runSessionOnLocalDb,
    runPipelineOnLocalDb,
    runStatementInSession,
    runPipelineInSession,
  )
where

import Hasql.Connection qualified as Connection
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingUtils.Constants qualified as Constants
import Prelude

data Error
  = ConnectionError (Connection.ConnectionError)
  | SessionError (Session.SessionError)
  deriving (Show, Eq)

runSessionOnLocalDb :: Session.Session a -> IO (Either Error a)
runSessionOnLocalDb session =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ fmap (mapLeft ConnectionError) $ Connection.acquire Constants.localConnectionSettings
    use connection =
      ExceptT
        $ fmap (mapLeft SessionError)
        $ Session.run session connection
    release connection =
      lift $ Connection.release connection

runPipelineOnLocalDb :: Pipeline.Pipeline a -> IO (Either Error a)
runPipelineOnLocalDb =
  runSessionOnLocalDb . Session.pipeline

runStatementInSession :: Statement.Statement a b -> a -> Session.Session b
runStatementInSession statement params =
  Session.statement params statement

runPipelineInSession :: Pipeline.Pipeline a -> Session.Session a
runPipelineInSession =
  Session.pipeline
