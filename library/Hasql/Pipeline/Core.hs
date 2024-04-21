{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-binds #-}

module Hasql.Pipeline.Core where

import Database.PostgreSQL.LibPQ qualified as Pq
import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Result qualified as Decoders.Result
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
run (Pipeline send) (Connection.Connection pqConnectionRef integerDatetimes registry) = do
  hSetBuffering stdout NoBuffering
  withMVar pqConnectionRef \pqConnection -> do
    putStrLn "enterPipelineMode"
    runCommandFailing pqConnection $ Pq.enterPipelineMode pqConnection
    putStrLn "send"
    sendResult <- send pqConnection registry integerDatetimes
    case sendResult of
      Left err -> do
        pure (Left err)
      Right recv -> do
        putStrLn "pipelineSync"
        runCommandFailing pqConnection $ Pq.pipelineSync pqConnection
        putStrLn "recv"
        recvResult <- recv
        putStrLn "dropRemainders"
        handleEither =<< Decoders.Results.run Decoders.Results.dropRemainders pqConnection integerDatetimes
        putStrLn "exitPipelineMode"
        runCommandFailing pqConnection $ Pq.exitPipelineMode pqConnection
        putStrLn "return"
        pure recvResult
  where
    runCommandFailing :: Pq.Connection -> IO Bool -> IO ()
    runCommandFailing pqConn runCmd =
      IO.checkedSend pqConn runCmd >>= handleEither
    handleEither = \case
      Right a -> pure a
      Left err -> fail $ show err

newtype Pipeline a
  = Pipeline
      ( Pq.Connection ->
        PreparedStatementRegistry.PreparedStatementRegistry ->
        Bool ->
        IO (Either QueryError (IO (Either QueryError a)))
      )
  deriving (Functor)

instance Applicative Pipeline where
  pure a =
    Pipeline (\_ _ _ -> pure (Right (pure (Right a))))

  Pipeline lSend <*> Pipeline rSend =
    Pipeline \conn reg integerDatetimes ->
      lSend conn reg integerDatetimes >>= \case
        Left sendErr ->
          pure (Left sendErr)
        Right lRecv ->
          rSend conn reg integerDatetimes <&> \case
            Left sendErr ->
              Left sendErr
            Right rRecv ->
              Right (liftA2 (<*>) lRecv rRecv)

statement :: params -> Statement.Statement params result -> Pipeline result
statement params (Statement.Statement sql (Encoders.Params encoder) (Decoders.Result decoder) preparable) =
  Pipeline run
  where
    run connection registry integerDatetimes =
      if preparable
        then runPrepared
        else runUnprepared
      where
        runPrepared = runExceptT do
          (key, keyRecv) <- ExceptT resolvePreparedStatementKey
          queryRecv <- ExceptT (sendQuery key)
          pure (keyRecv *> queryRecv)
          where
            (oidList, valueAndFormatList) =
              Encoders.Params.compilePreparedStatementData encoder integerDatetimes params

            resolvePreparedStatementKey =
              PreparedStatementRegistry.update localKey onNewRemoteKey onOldRemoteKey registry
              where
                localKey =
                  PreparedStatementRegistry.LocalKey sql oidList
                onNewRemoteKey key =
                  do
                    sent <- Pq.sendPrepare connection key sql (mfilter (not . null) (Just oidList))
                    if sent
                      then pure (True, Right (key, recv))
                      else (False,) . Left . commandToQueryError . ClientError <$> Pq.errorMessage connection
                  where
                    recv :: IO (Either QueryError ())
                    recv = do
                      Pq.getResult connection >>= \case
                        Nothing ->
                          Left . commandToQueryError . ClientError <$> Pq.errorMessage connection
                        Just result ->
                          mapLeft (commandToQueryError . ResultError)
                            <$> Decoders.Result.run Decoders.Result.noResult integerDatetimes result
                onOldRemoteKey key =
                  pure (Right (key, pure (Right ())))

            sendQuery key =
              Pq.sendQueryPrepared connection key valueAndFormatList Pq.Binary >>= \case
                False -> Left . commandToQueryError . ClientError <$> Pq.errorMessage connection
                True -> pure (Right recv)
              where
                recv =
                  fmap (mapLeft commandToQueryError)
                    $ (<*)
                    <$> Decoders.Results.run decoder connection integerDatetimes
                    <*> Decoders.Results.run Decoders.Results.dropRemainders connection integerDatetimes

        runUnprepared =
          Pq.sendQueryParams connection sql (Encoders.Params.compileUnpreparedStatementData encoder integerDatetimes params) Pq.Binary >>= \case
            False -> Left . commandToQueryError . ClientError <$> Pq.errorMessage connection
            True -> pure (Right recv)
          where
            recv =
              fmap (mapLeft commandToQueryError)
                $ (<*)
                <$> Decoders.Results.run decoder connection integerDatetimes
                <*> Decoders.Results.run Decoders.Results.dropRemainders connection integerDatetimes

    commandToQueryError =
      QueryError sql (Encoders.Params.renderReadable encoder params)
