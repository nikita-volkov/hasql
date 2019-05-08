module Hasql.Private.Session
where

import Hasql.Private.Prelude
import Hasql.Private.Errors
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Private.Decoders.Results as Decoders.Results
import qualified Hasql.Private.Decoders.Result as Decoders.Result
import qualified Hasql.Private.Encoders.Params as Encoders.Params
import qualified Hasql.Private.Settings as Settings
import qualified Hasql.Private.IO as IO
import qualified Hasql.Statement as Statement
import qualified Hasql.Private.Connection as Connection


-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a =
  Session (ReaderT Connection.Connection (ExceptT QueryError IO) a)
  deriving (Functor, Applicative, Monad, MonadError QueryError, MonadIO)

-- |
-- Executes a bunch of commands on the provided connection.
run :: Session a -> Connection.Connection -> IO (Either QueryError a)
run (Session impl) connection =
  runExceptT $
  runReaderT impl connection

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  Session $ ReaderT $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
    ExceptT $ fmap (mapLeft (QueryError sql [])) $ withMVar pqConnectionRef $ \pqConnection -> do
      r1 <- IO.sendNonparametricStatement pqConnection sql
      r2 <- IO.getResults pqConnection integerDatetimes decoder
      return $ r1 *> r2
  where
    decoder =
      Decoders.Results.single Decoders.Result.unit

-- |
-- Parameters and a specification of a parametric single-statement query to apply them to.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement template encoder decoder preparable) =
  Session $ ReaderT $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
    ExceptT $ fmap (mapLeft (QueryError template inputReps)) $ withMVar pqConnectionRef $ \pqConnection -> do
      r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template (unsafeCoerce encoder) preparable input
      r2 <- IO.getResults pqConnection integerDatetimes (unsafeCoerce decoder)
      return $ r1 *> r2
  where
    inputReps =
      let
        Encoders.Params.Params (Op encoderOp) = (unsafeCoerce encoder)
        step (_, _, _, rendering) acc =
          rendering : acc
        in foldr step [] (encoderOp input)

-- |
-- Parameters and a specification of a multi parametric single-statement query to apply them to.
multiParamStatement :: [params] -> Statement.MultiParamStatement params result -> Session result
multiParamStatement input (Statement.MultiParamStatement template encoders decoder preparable) =
  Session $ ReaderT $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
    ExceptT $ fmap (mapLeft (QueryError template inputReps)) $ withMVar pqConnectionRef $ \pqConnection -> do
      r1 <- IO.sendMultiParametricStatement pqConnection integerDatetimes registry template coercedEncoders preparable input
      r2 <- IO.getResults pqConnection integerDatetimes (unsafeCoerce decoder)
      return $ r1 *> r2
  where
    coercedEncoders = unsafeCoerce <$> encoders
    deconstruct (Encoders.Params.Params (Op encoderOp)) = encoderOp
    encoderOps = deconstruct <$> coercedEncoders
    step (_, _, _, rendering) acc = rendering : acc
    inputReps = concat $ zipWith inputRep encoderOps input
    inputRep e i = foldr step [] (e i)
