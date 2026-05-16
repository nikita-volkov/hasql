module Hasql.LibpqDriver (libpqDriver) where

import Database.PostgreSQL.LibPQ qualified as Pq
import Hasql.Driver.Interface qualified as Interface
import Hasql.Pq qualified as Pq2
import Hasql.Pq.Mappings qualified as Mappings
import Prelude

libpqDriver :: Interface.Driver Pq.Connection Pq.Result
libpqDriver =
  Interface.Driver
    { Interface.driverConnect = Pq.connectdb,
      Interface.driverConnectOk = \conn -> (== Pq.ConnectionOk) <$> Pq.status conn,
      Interface.driverDisconnect = Pq.finish,
      Interface.driverErrorMessage = Pq.errorMessage,
      Interface.driverTransactionStatus = \conn -> mapTransactionStatus <$> Pq.transactionStatus conn,
      Interface.driverPipelineStatus = \conn -> mapPipelineStatus <$> Pq2.pipelineStatus conn,
      Interface.driverCancel = \conn -> do
        mCancel <- Pq.getCancel conn
        case mCancel of
          Nothing -> pure (Right ())
          Just cancel -> Pq.cancel cancel,
      Interface.driverServerVersion = Pq.serverVersion,
      Interface.driverExec = \conn sql -> Pq.exec conn sql >> pure (),
      Interface.driverSendQuery = Pq.sendQuery,
      Interface.driverSendQueryParams = \conn sql params ->
        Pq.sendQueryParams
          conn
          sql
          (fmap (fmap (\(oid, bytes, textFmt) -> (Pq.Oid (fromIntegral oid), bytes, if textFmt then Pq.Text else Pq.Binary))) params)
          Pq.Binary,
      Interface.driverSendPrepare = \conn name sql oids ->
        Pq.sendPrepare conn name sql (Just (fmap (Pq.Oid . fromIntegral) oids)),
      Interface.driverSendQueryPrepared = \conn name params ->
        Pq.sendQueryPrepared
          conn
          name
          (fmap (fmap (\(bytes, textFmt) -> (bytes, if textFmt then Pq.Text else Pq.Binary))) params)
          Pq.Binary,
      Interface.driverEnterPipelineMode = Pq2.enterPipelineMode,
      Interface.driverExitPipelineMode = Pq2.exitPipelineMode,
      Interface.driverPipelineSync = Pq2.pipelineSync,
      Interface.driverSendFlushRequest = Pq2.sendFlushRequest,
      Interface.driverGetResult = Pq.getResult,
      Interface.driverResult = libpqResultDriver
    }

libpqResultDriver :: Interface.ResultDriver Pq.Result
libpqResultDriver =
  Interface.ResultDriver
    { Interface.rdResultStatus = \r -> mapExecStatus <$> Pq2.resultStatus r,
      Interface.rdCmdTuples = Pq.cmdTuples,
      Interface.rdResultErrorField = \r field -> Pq.resultErrorField r (mapDiagField field),
      Interface.rdNtuples = \r -> Pq2.rowToInt <$> Pq.ntuples r,
      Interface.rdNfields = \r -> Pq2.colToInt <$> Pq.nfields r,
      Interface.rdFtype = \r col -> Pq2.oidToWord32 <$> Pq.ftype r (Pq.Col (fromIntegral col)),
      Interface.rdGetValue = \r row col ->
        Pq.getvalue' r (Pq.Row (fromIntegral row)) (Pq.Col (fromIntegral col))
    }

mapExecStatus :: Mappings.ExecStatus -> Interface.ExecStatus
mapExecStatus = \case
  Mappings.EmptyQuery -> Interface.EmptyQuery
  Mappings.CommandOk -> Interface.CommandOk
  Mappings.TuplesOk -> Interface.TuplesOk
  Mappings.CopyOut -> Interface.CopyOut
  Mappings.CopyIn -> Interface.CopyIn
  Mappings.CopyBoth -> Interface.CopyBoth
  Mappings.BadResponse -> Interface.BadResponse
  Mappings.NonfatalError -> Interface.NonfatalError
  Mappings.FatalError -> Interface.FatalError
  Mappings.SingleTuple -> Interface.SingleTuple
  Mappings.PipelineSync -> Interface.PipelineSync
  Mappings.PipelineAbort -> Interface.PipelineAbort

mapPipelineStatus :: Mappings.PipelineStatus -> Interface.PipelineStatus
mapPipelineStatus = \case
  Mappings.PipelineOn -> Interface.PipelineOn
  Mappings.PipelineOff -> Interface.PipelineOff
  Mappings.PipelineAborted -> Interface.PipelineAborted

mapTransactionStatus :: Pq.TransactionStatus -> Interface.TransactionStatus
mapTransactionStatus = \case
  Pq.TransIdle -> Interface.TransIdle
  Pq.TransActive -> Interface.TransActive
  Pq.TransInTrans -> Interface.TransInTrans
  Pq.TransInError -> Interface.TransInError
  Pq.TransUnknown -> Interface.TransUnknown

mapDiagField :: Interface.DiagField -> Pq.FieldCode
mapDiagField = \case
  Interface.DiagSqlstate -> Pq.DiagSqlstate
  Interface.DiagMessagePrimary -> Pq.DiagMessagePrimary
  Interface.DiagMessageDetail -> Pq.DiagMessageDetail
  Interface.DiagMessageHint -> Pq.DiagMessageHint
  Interface.DiagStatementPosition -> Pq.DiagStatementPosition
