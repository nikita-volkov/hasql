module Main.DSL
  ( Session,
    SessionError (..),
    session,
    Hasql.Session.statement,
    Hasql.Session.sql,
  )
where

import Hasql.Connection qualified as HC
import Hasql.Session qualified
import Main.Prelude

type Session =
  Hasql.Session.Session

data SessionError
  = ConnectionError (HC.ConnectionError)
  | SessionError (Hasql.Session.QueryError)
  deriving (Show, Eq)

session :: Session a -> IO (Either SessionError a)
session session =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ fmap (mapLeft ConnectionError) $ HC.acquire settings
      where
        settings =
          HC.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = "postgres"
            database = "postgres"
    use connection =
      ExceptT
        $ fmap (mapLeft SessionError)
        $ Hasql.Session.run session connection
    release connection =
      lift $ HC.release connection
