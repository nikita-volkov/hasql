module Main.DSL where

import Main.Prelude
import qualified Hasql.Connection as HC
import qualified Hasql.Query as HQ
import qualified Hasql.Settings as HS
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD


newtype Session a =
  Session (ReaderT HC.Connection (EitherT HQ.ResultsError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data SessionError =
  ConnectionError (HC.ConnectionError) |
  ResultsError (HQ.ResultsError)
  deriving (Show, Eq)

session :: Session a -> IO (Either SessionError a)
session (Session impl) =
  runEitherT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      EitherT $ fmap (mapLeft ConnectionError) $ HC.connect settings
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
      bimapEitherT ResultsError id $
      runReaderT impl connection
    release connection =
      lift $ HC.disconnect connection

query :: a -> HQ.Query a b -> Session b
query params query =
  Session $ ReaderT $ EitherT . HQ.run query params
