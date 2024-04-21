module Hasql.Session.Core where

import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.Result qualified as Decoders.Result
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.IO qualified as IO
import Hasql.Prelude
import Hasql.Statement qualified as Statement

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (ReaderT Connection.Connection (ExceptT QueryError IO) a)
  deriving (Functor, Applicative, Monad, MonadError QueryError, MonadIO, MonadReader Connection.Connection)

-- |
-- Executes a bunch of commands on the provided connection.
run :: Session a -> Connection.Connection -> IO (Either QueryError a)
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
        $ fmap (mapLeft (QueryError sql []))
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
statement input (Statement.Statement template (Encoders.Params paramsEncoder@(Encoders.Params.Params _ _ _ printer)) decoder preparable) =
  Session
    $ ReaderT
    $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
      ExceptT
        $ fmap (mapLeft (QueryError template inputReps))
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder preparable input
          r2 <- IO.getResults pqConnection integerDatetimes (unsafeCoerce decoder)
          return $ r1 *> r2
  where
    inputReps =
      printer input
        & toList
