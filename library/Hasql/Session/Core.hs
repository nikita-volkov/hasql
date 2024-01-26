module Hasql.Session.Core where

import qualified Hasql.Connection.Core as Connection
import qualified Hasql.Decoders.Result as Decoders.Result
import qualified Hasql.Decoders.Results as Decoders.Results
import qualified Hasql.Encoders.All as Encoders
import qualified Hasql.Encoders.Params as Encoders.Params
import Hasql.Errors
import qualified Hasql.IO as IO
import Hasql.Prelude
import qualified Hasql.Statement as Statement

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
statement input (Statement.Statement template (Encoders.Params paramsEncoder) decoder preparable) =
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
      let Encoders.Params.Params (Op encoderOp) = paramsEncoder
          step (_, _, _, rendering) acc =
            rendering : acc
       in foldr step [] (encoderOp input)
