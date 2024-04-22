module Hasql.Session.Core where

import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Result qualified as Decoders.Result
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.IO qualified as IO
import Hasql.Pipeline.Core qualified as Pipeline
import Hasql.Prelude
import Hasql.Statement qualified as Statement

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (ReaderT Connection.Connection (ExceptT SessionError IO) a)
  deriving (Functor, Applicative, Monad, MonadError SessionError, MonadIO, MonadReader Connection.Connection)

-- |
-- Executes a bunch of commands on the provided connection.
run :: Session a -> Connection.Connection -> IO (Either SessionError a)
run (Session impl) connection =
  runExceptT
    $ runReaderT impl connection

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  Session
    $ ReaderT
    $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
      ExceptT
        $ fmap (mapLeft (QuerySessionError sql []))
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendNonparametricStatement pqConnection sql
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          return $ r1 *> r2
  where
    decoder =
      Decoders.Results.single Decoders.Result.noResult

-- |
-- Parameters and a specification of a parametric single-statement query to apply them to.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Session
    $ ReaderT
    $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
      ExceptT
        $ fmap (mapLeft (QuerySessionError template (Encoders.Params.renderReadable paramsEncoder input)))
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder preparable input
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          return $ r1 *> r2

pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline =
  Session $ ReaderT \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
    ExceptT $ withMVar pqConnectionRef \pqConnection ->
      Pipeline.run pipeline pqConnection registry integerDatetimes
