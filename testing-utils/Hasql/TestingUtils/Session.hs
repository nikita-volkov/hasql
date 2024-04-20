module Hasql.TestingUtils.Session
  ( Session.Session,
    SessionError (..),
    Session.QueryError (..),
    Session.CommandError (..),
    runSession,
    runStatementInSession,
  )
where

import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Prelude

data SessionError
  = ConnectionError (Connection.ConnectionError)
  | SessionError (Session.QueryError)
  deriving (Show, Eq)

runSession :: Session.Session a -> IO (Either SessionError a)
runSession session =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ fmap (mapLeft ConnectionError) $ Connection.acquire settings
      where
        settings =
          Connection.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = "postgres"
            database = "postgres"
    use connection =
      ExceptT
        $ fmap (mapLeft SessionError)
        $ Session.run session connection
    release connection =
      lift $ Connection.release connection

runStatementInSession :: Statement.Statement a b -> a -> Session.Session b
runStatementInSession statement params =
  Session.statement params statement
