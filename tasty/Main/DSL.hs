module Main.DSL where

import Main.Prelude
import qualified Hasql.IO as IO
import qualified Hasql.Query as HQ
import qualified Hasql.Settings as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.Session


newtype Session a =
  Session (ReaderT IO.Connection (EitherT IO.SessionError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data SessionError =
  ConnectionError (IO.ConnectionError) |
  SessionError (IO.SessionError)
  deriving (Show, Eq)

session :: Session a -> IO (Either SessionError a)
session (Session impl) =
  runEitherT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      EitherT $ fmap (mapLeft ConnectionError) $ IO.acquireConnection settings
      where
        settings =
          HS.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = ""
            database = "postgres"
    use connection =
      bimapEitherT SessionError id $
      runReaderT impl connection
    release connection =
      lift $ IO.releaseConnection connection

query :: a -> HQ.Query a b -> Session b
query params query =
  Session $ ReaderT $ \connection -> EitherT $ IO.session connection $ Hasql.Session.query params query
