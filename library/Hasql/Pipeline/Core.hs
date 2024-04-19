module Hasql.Pipeline.Core where

import Database.PostgreSQL.LibPQ qualified as Pq
import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.IO qualified as IO
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Statement qualified as Statement

run :: Pipeline a -> Connection.Connection -> IO (Either QueryError a)
run (Pipeline send recv) (Connection.Connection pqConnectionRef integerDatetimes registry) =
  withMVar pqConnectionRef \pqConnection -> do
    Pq.enterPipelineMode pqConnection
    sendResult <- send pqConnection integerDatetimes registry
    Pq.pipelineSync pqConnection
    recvResult <- recv pqConnection integerDatetimes
    Pq.exitPipelineMode pqConnection
    pure (sendResult *> recvResult)

data Pipeline a
  = Pipeline
      -- | Send commands.
      (Pq.Connection -> Bool -> PreparedStatementRegistry.PreparedStatementRegistry -> IO (Either QueryError ()))
      -- | Receive results.
      (Pq.Connection -> Bool -> IO (Either QueryError a))
  deriving (Functor)

instance Applicative Pipeline where
  pure a =
    Pipeline send recv
    where
      send _ _ _ =
        pure (Right ())
      recv _ _ =
        pure (Right a)

  Pipeline lSend lRecv <*> Pipeline rSend rRecv =
    Pipeline send recv
    where
      send pqConn idt pReg = do
        lSendRes <- lSend pqConn idt pReg
        rSendRes <- rSend pqConn idt pReg
        pure (lSendRes *> rSendRes)
      recv pqConn idt = do
        lRecvRes <- lRecv pqConn idt
        rRecvRes <- rRecv pqConn idt
        pure (lRecvRes <*> rRecvRes)

statement :: params -> Statement.Statement params result -> Pipeline result
statement params (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Pipeline send recv
  where
    send pqConnection integerDatetimes registry =
      mapLeft commandToQueryError
        <$> IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder preparable params

    recv pqConnection integerDatetimes =
      mapLeft commandToQueryError
        <$> IO.getResults pqConnection integerDatetimes decoder

    commandToQueryError =
      QueryError template (Encoders.Params.renderReadable paramsEncoder params)
