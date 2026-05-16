module Hasql.Driver.Interface
  ( ExecStatus (..),
    PipelineStatus (..),
    TransactionStatus (..),
    DiagField (..),
    ResultDriver (..),
    Driver (..),
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word32)

data ExecStatus
  = EmptyQuery
  | CommandOk
  | TuplesOk
  | CopyOut
  | CopyIn
  | CopyBoth
  | BadResponse
  | NonfatalError
  | FatalError
  | SingleTuple
  | PipelineSync
  | PipelineAbort
  deriving (Eq, Show)

data PipelineStatus
  = PipelineOn
  | PipelineOff
  | PipelineAborted
  deriving (Eq, Show)

data TransactionStatus
  = TransIdle
  | TransInTrans
  | TransActive
  | TransInError
  | TransUnknown
  deriving (Eq, Show)

data DiagField
  = DiagSqlstate
  | DiagMessagePrimary
  | DiagMessageDetail
  | DiagMessageHint
  | DiagStatementPosition
  deriving (Eq, Show, Enum, Bounded)

data ResultDriver result = ResultDriver
  { rdResultStatus :: result -> IO ExecStatus,
    rdCmdTuples :: result -> IO (Maybe ByteString),
    rdResultErrorField :: result -> DiagField -> IO (Maybe ByteString),
    rdNtuples :: result -> IO Int,
    rdNfields :: result -> IO Int,
    rdFtype :: result -> Int -> IO Word32,
    rdGetValue :: result -> Int -> Int -> IO (Maybe ByteString)
  }

data Driver conn result = Driver
  { driverConnect :: ByteString -> IO conn,
    driverConnectOk :: conn -> IO Bool,
    driverDisconnect :: conn -> IO (),
    driverErrorMessage :: conn -> IO (Maybe ByteString),
    driverTransactionStatus :: conn -> IO TransactionStatus,
    driverPipelineStatus :: conn -> IO PipelineStatus,
    driverCancel :: conn -> IO (Either ByteString ()),
    driverServerVersion :: conn -> IO Int,
    driverExec :: conn -> ByteString -> IO (),
    driverSendQuery :: conn -> ByteString -> IO Bool,
    driverSendQueryParams :: conn -> ByteString -> [Maybe (Word32, ByteString, Bool)] -> IO Bool,
    driverSendPrepare :: conn -> ByteString -> ByteString -> [Word32] -> IO Bool,
    driverSendQueryPrepared :: conn -> ByteString -> [Maybe (ByteString, Bool)] -> IO Bool,
    driverEnterPipelineMode :: conn -> IO Bool,
    driverExitPipelineMode :: conn -> IO Bool,
    driverPipelineSync :: conn -> IO Bool,
    driverSendFlushRequest :: conn -> IO Bool,
    driverGetResult :: conn -> IO (Maybe result),
    driverResult :: ResultDriver result
  }
