module Hasql.Pipeline.Core where

import Database.PostgreSQL.LibPQ qualified as Pq
import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.IO qualified as IO
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Statement qualified as Statement
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

run :: Pipeline a -> Connection.Connection -> IO (Either QueryError a)
run (Pipeline send recv) (Connection.Connection pqConnectionRef integerDatetimes registry) = do
  hSetBuffering stdout NoBuffering
  withMVar pqConnectionRef \pqConnection -> do
    putStrLn "enterPipelineMode"
    runCommandFailing pqConnection $ Pq.enterPipelineMode pqConnection
    putStrLn "send"
    sendResult <- send pqConnection integerDatetimes registry
    putStrLn "pipelineSync"
    runCommandFailing pqConnection $ Pq.pipelineSync pqConnection
    putStrLn "recv"
    recvResult <- recv pqConnection integerDatetimes
    putStrLn "exitPipelineMode"
    handleEither =<< Decoders.Results.run Decoders.Results.dropRemainders pqConnection integerDatetimes
    putStrLn "exitPipelineMode"
    runCommandFailing pqConnection $ Pq.exitPipelineMode pqConnection
    putStrLn "return"
    pure (sendResult *> recvResult)
  where
    runCommandFailing :: Pq.Connection -> IO Bool -> IO ()
    runCommandFailing pqConn runCmd =
      IO.checkedSend pqConn runCmd >>= handleEither
    handleEither = \case
      Right a -> pure a
      Left err -> fail $ show err

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
        -- <$> Decoders.Results.run decoder (integerDatetimes, pqConnection)
        <$> IO.getResults pqConnection integerDatetimes decoder

    commandToQueryError =
      QueryError template (Encoders.Params.renderReadable paramsEncoder params)
