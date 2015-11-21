module Main.DSL where

import Main.Prelude
import qualified Hasql as H
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD


newtype Session a =
  Session (ReaderT H.Connection (EitherT H.ResultsError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data SessionError =
  ConnectionError (H.ConnectionError) |
  ResultsError (H.ResultsError)
  deriving (Show, Eq)

session :: Session a -> IO (Either SessionError a)
session (Session impl) =
  runEitherT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      EitherT $ fmap (mapLeft ConnectionError) $ H.connect settings
      where
        settings =
          H.settings host port user password database
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
      lift $ H.disconnect connection

query :: a -> H.Query a b -> Session b
query params query =
  Session $ ReaderT $ \connection -> EitherT $ H.query connection query params
