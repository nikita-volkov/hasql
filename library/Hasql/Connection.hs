module Hasql.Connection
(
  Connection,
  D.Error(..),
  acquire,
  use,
  release,
)
where

import Hasql.Prelude
import qualified Hasql.Client.Communicator as A
import qualified Hasql.Client.Socket as B
import qualified Hasql.Client.Model as D
import qualified Hasql.Connection.Session.Session as C
import qualified Hasql.PreparedStatementRegistry as E


data Connection =
  Connection {
    use :: forall result. C.Session result -> IO (Either D.Error result)
    ,
    release :: IO ()
  }

acquire host portMaybe username passwordMaybe databaseMaybe =
  do
    runExceptT $ do
      socket <-
        let
          port =
            fromMaybe 5432 portMaybe
          in ExceptT $ fmap (first D.TransportError) $ B.connectToHostAndPort host port
      communicator <- lift (A.acquire socket)
      backendSettings <- ExceptT $ join $ A.startUp communicator username passwordMaybe databaseMaybe []
      preparedStatementRegistry <- lift E.new
      let
        use :: forall result. C.Session result -> IO (Either D.Error result)
        use (C.Session io) =
          io (C.Env communicator backendSettings preparedStatementRegistry)
        release =
          do
            A.release communicator
            B.close socket
        in return (Connection use release)
