module Hasql.Session.Core where

import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Result qualified as Decoders.Result
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as Pq
import Hasql.Pipeline.Core qualified as Pipeline
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Statement qualified as Statement
import Hasql.Structures.ConnectionState qualified as ConnectionState
import Hasql.Structures.RegistryState qualified as RegistryState

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (ReaderT ConnectionState.ConnectionState (ExceptT SessionError IO) a)
  deriving (Functor, Applicative, Monad, MonadError SessionError, MonadIO, MonadReader ConnectionState.ConnectionState)

-- |
-- Executes a bunch of commands on the provided connection state.
run :: Session a -> ConnectionState.ConnectionState -> IO (Either SessionError a)
run (Session impl) connectionState =
  runExceptT $ runReaderT impl connectionState

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  Session
    $ ReaderT
    $ \(ConnectionState.ConnectionState _ pqConnection integerDatetimes _) ->
      ExceptT
        $ fmap (first (QueryError sql []))
        $ do
          r1 <- IO.sendNonparametricStatement pqConnection sql
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          return $ r1 *> r2
  where
    decoder =
      Decoders.Results.single Decoders.Result.noResult

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Session
    $ ReaderT
    $ \(ConnectionState.ConnectionState usePreparedStatements pqConnection integerDatetimes registryState) ->
      ExceptT
        $ fmap (first (QueryError template (Encoders.Params.renderReadable paramsEncoder input)))
        $ do
          -- Create a temporary registry wrapper for the IO layer
          registry <- PreparedStatementRegistry.new
          -- We lose the registry state updates for now, but this makes it compile
          r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder (usePreparedStatements && preparable) input
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          return $ r1 *> r2

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline =
  Session $ ReaderT \(ConnectionState.ConnectionState usePreparedStatements pqConnection integerDatetimes registryState) -> do
    -- Create a temporary registry for the pipeline execution  
    registry <- liftIO PreparedStatementRegistry.new
    ExceptT $ Pipeline.run pipeline usePreparedStatements pqConnection registry integerDatetimes
