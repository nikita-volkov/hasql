module Main.DSL
(
  Session,
  SessionError(..),
  session,
  Hasql.Session.statement,
  Hasql.Session.sql,
)
where

import Main.Prelude
import qualified Hasql.Connection as HC
import qualified Hasql.Statement as HQ
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.Session


type Session =
  Hasql.Session.Session

data SessionError =
  ConnectionError (HC.ConnectionError) |
  SessionError (Hasql.Session.QueryError)
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
            password = ""
            database = "postgres"
    use connection =
      ExceptT $
      fmap (mapLeft SessionError) $
      Hasql.Session.run session connection
    release connection =
      lift $ HC.release connection
