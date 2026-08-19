module Pure.Connection.AcquireSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Pqi qualified as Pq
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "acquire" do
    it "finishes the pq connection when the status check fails" do
      finishedRef <- newIORef False
      let adapter =
            fakeAdapter
              ( fakeConnection
                  finishedRef
                  Pq.ConnectionBad
                  (Just "could not connect to server: Connection refused")
                  180000
              )
      result <- Connection.acquire adapter mempty
      case result of
        Left (Errors.NetworkingAcquireError _) -> pure ()
        Left err -> expectationFailure ("Expected NetworkingAcquireError, got: " <> show err)
        Right _ -> expectationFailure "Expected connection to fail, but it succeeded"
      readIORef finishedRef `shouldReturn` True

    it "finishes the pq connection when the server version check fails" do
      finishedRef <- newIORef False
      let adapter =
            fakeAdapter
              ( fakeConnection
                  finishedRef
                  Pq.ConnectionOk
                  Nothing
                  80000
              )
      result <- Connection.acquire adapter mempty
      case result of
        Left (Errors.CompatibilityAcquireError _) -> pure ()
        Left err -> expectationFailure ("Expected CompatibilityAcquireError, got: " <> show err)
        Right _ -> expectationFailure "Expected connection to fail, but it succeeded"
      readIORef finishedRef `shouldReturn` True

    it "does not finish the pq connection when acquisition succeeds" do
      finishedRef <- newIORef False
      let adapter =
            fakeAdapter
              ( (fakeConnection finishedRef Pq.ConnectionOk Nothing 180000)
                  { Pq.exec = \_ -> pure (Just (fakeResult Pq.CommandOk Nothing))
                  }
              )
      result <- Connection.acquire adapter mempty
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure ("Expected acquire to succeed, got: " <> show err)
      readIORef finishedRef `shouldReturn` False

    it "finishes the pq connection when the status check throws an exception" do
      finishedRef <- newIORef False
      let brokenConnection =
            (fakeConnection finishedRef Pq.ConnectionOk Nothing 180000)
              { Pq.status = throwIO (userError "boom")
              }
          adapter = fakeAdapter brokenConnection
      result <- try @IOException (Connection.acquire adapter mempty)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected acquire to propagate the exception"
      readIORef finishedRef `shouldReturn` True

    it "fails and finishes the pq connection when the session-init exec returns no result" do
      finishedRef <- newIORef False
      let brokenConnection =
            (fakeConnection finishedRef Pq.ConnectionOk Nothing 180000)
              { Pq.exec = \_ -> pure Nothing,
                Pq.errorMessage = pure (Just "server closed the connection")
              }
          adapter = fakeAdapter brokenConnection
      result <- Connection.acquire adapter mempty
      case result of
        Left (Errors.OtherAcquireError _) -> pure ()
        Left err -> expectationFailure ("Expected OtherAcquireError, got: " <> show err)
        Right _ -> expectationFailure "Expected connection to fail, but it succeeded"
      readIORef finishedRef `shouldReturn` True

    it "fails and finishes the pq connection when the session-init exec reports a non-CommandOk status" do
      finishedRef <- newIORef False
      let failedResult =
            fakeResult Pq.FatalError (Just "permission denied for SET")
          brokenConnection =
            (fakeConnection finishedRef Pq.ConnectionOk Nothing 180000)
              { Pq.exec = \_ -> pure (Just failedResult)
              }
          adapter = fakeAdapter brokenConnection
      result <- Connection.acquire adapter mempty
      case result of
        Left (Errors.OtherAcquireError msg) -> msg `shouldBe` "permission denied for SET"
        Left err -> expectationFailure ("Expected OtherAcquireError, got: " <> show err)
        Right _ -> expectationFailure "Expected connection to fail, but it succeeded"
      readIORef finishedRef `shouldReturn` True

    it "succeeds when the session-init exec reports a CommandOk status" do
      finishedRef <- newIORef False
      let okResult =
            fakeResult Pq.CommandOk Nothing
          adapter =
            fakeAdapter
              ( (fakeConnection finishedRef Pq.ConnectionOk Nothing 180000)
                  { Pq.exec = \_ -> pure (Just okResult)
                  }
              )
      result <- Connection.acquire adapter mempty
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure ("Expected acquire to succeed, got: " <> show err)
      readIORef finishedRef `shouldReturn` False

fakeAdapter :: Pq.Connection -> Pq.Adapter
fakeAdapter connection =
  Pq.Adapter
    { name = "fake",
      connectdb = \_ -> pure connection,
      connectStart = \_ -> unimplemented "connectStart",
      newNullConnection = unimplementedIO "newNullConnection",
      unescapeBytea = \_ -> unimplemented "unescapeBytea",
      resStatus = \_ -> unimplemented "resStatus"
    }

fakeConnection :: IORef Bool -> Pq.ConnStatus -> Maybe ByteString -> Int -> Pq.Connection
fakeConnection finishedRef connStatus connErrorMessage connServerVersion =
  Pq.Connection
    { connectPoll = unimplementedIO "connectPoll",
      isNullConnection = False,
      finish = writeIORef finishedRef True,
      reset = unimplementedIO "reset",
      resetStart = unimplementedIO "resetStart",
      resetPoll = unimplementedIO "resetPoll",
      db = unimplementedIO "db",
      user = unimplementedIO "user",
      pass = unimplementedIO "pass",
      host = unimplementedIO "host",
      port = unimplementedIO "port",
      options = unimplementedIO "options",
      status = pure connStatus,
      transactionStatus = unimplementedIO "transactionStatus",
      parameterStatus = \_ -> unimplemented "parameterStatus",
      protocolVersion = unimplementedIO "protocolVersion",
      serverVersion = pure connServerVersion,
      errorMessage = pure connErrorMessage,
      socket = unimplementedIO "socket",
      backendPID = unimplementedIO "backendPID",
      connectionNeedsPassword = unimplementedIO "connectionNeedsPassword",
      connectionUsedPassword = unimplementedIO "connectionUsedPassword",
      exec = \_ -> pure Nothing,
      execParams = \_ _ _ -> unimplemented "execParams",
      prepare = \_ _ _ -> unimplemented "prepare",
      execPrepared = \_ _ _ -> unimplemented "execPrepared",
      describePrepared = \_ -> unimplemented "describePrepared",
      describePortal = \_ -> unimplemented "describePortal",
      escapeStringConn = \_ -> unimplemented "escapeStringConn",
      escapeByteaConn = \_ -> unimplemented "escapeByteaConn",
      escapeIdentifier = \_ -> unimplemented "escapeIdentifier",
      sendQuery = \_ -> unimplemented "sendQuery",
      sendQueryParams = \_ _ _ -> unimplemented "sendQueryParams",
      sendPrepare = \_ _ _ -> unimplemented "sendPrepare",
      sendQueryPrepared = \_ _ _ -> unimplemented "sendQueryPrepared",
      sendDescribePrepared = \_ -> unimplemented "sendDescribePrepared",
      sendDescribePortal = \_ -> unimplemented "sendDescribePortal",
      getResult = unimplementedIO "getResult",
      consumeInput = unimplementedIO "consumeInput",
      isBusy = unimplementedIO "isBusy",
      setnonblocking = \_ -> unimplemented "setnonblocking",
      isnonblocking = unimplementedIO "isnonblocking",
      setSingleRowMode = unimplementedIO "setSingleRowMode",
      flush = unimplementedIO "flush",
      pipelineStatus = unimplementedIO "pipelineStatus",
      enterPipelineMode = unimplementedIO "enterPipelineMode",
      exitPipelineMode = unimplementedIO "exitPipelineMode",
      pipelineSync = unimplementedIO "pipelineSync",
      sendFlushRequest = unimplementedIO "sendFlushRequest",
      getCancel = unimplementedIO "getCancel",
      notifies = unimplementedIO "notifies",
      disableNoticeReporting = unimplementedIO "disableNoticeReporting",
      enableNoticeReporting = unimplementedIO "enableNoticeReporting",
      getNotice = unimplementedIO "getNotice",
      putCopyData = \_ -> unimplemented "putCopyData",
      putCopyEnd = \_ -> unimplemented "putCopyEnd",
      getCopyData = \_ -> unimplemented "getCopyData",
      loCreat = unimplementedIO "loCreat",
      loCreate = \_ -> unimplemented "loCreate",
      loImport = \_ -> unimplemented "loImport",
      loImportWithOid = \_ _ -> unimplemented "loImportWithOid",
      loExport = \_ _ -> unimplemented "loExport",
      loOpen = \_ _ -> unimplemented "loOpen",
      loWrite = \_ _ -> unimplemented "loWrite",
      loRead = \_ _ -> unimplemented "loRead",
      loSeek = \_ _ _ -> unimplemented "loSeek",
      loTell = \_ -> unimplemented "loTell",
      loTruncate = \_ _ -> unimplemented "loTruncate",
      loClose = \_ -> unimplemented "loClose",
      loUnlink = \_ -> unimplemented "loUnlink",
      clientEncoding = unimplementedIO "clientEncoding",
      setClientEncoding = \_ -> unimplemented "setClientEncoding",
      setErrorVerbosity = \_ -> unimplemented "setErrorVerbosity"
    }

fakeResult :: Pq.ExecStatus -> Maybe ByteString -> Pq.Result
fakeResult status errorMessage =
  Pq.Result
    { resultStatus = pure status,
      resultErrorMessage = pure errorMessage,
      resultErrorField = \_ -> unimplemented "resultErrorField",
      unsafeFreeResult = unimplementedIO "unsafeFreeResult",
      ntuples = unimplementedIO "ntuples",
      nfields = unimplementedIO "nfields",
      fname = \_ -> unimplemented "fname",
      fnumber = \_ -> unimplemented "fnumber",
      ftable = \_ -> unimplemented "ftable",
      ftablecol = \_ -> unimplemented "ftablecol",
      fformat = \_ -> unimplemented "fformat",
      ftype = \_ -> unimplemented "ftype",
      fmod = \_ -> unimplemented "fmod",
      fsize = \_ -> unimplemented "fsize",
      getvalue = \_ _ -> unimplemented "getvalue",
      getvalue' = \_ _ -> unimplemented "getvalue'",
      getisnull = \_ _ -> unimplemented "getisnull",
      getlength = \_ _ -> unimplemented "getlength",
      nparams = unimplementedIO "nparams",
      paramtype = \_ -> unimplemented "paramtype",
      cmdStatus = unimplementedIO "cmdStatus",
      cmdTuples = unimplementedIO "cmdTuples"
    }

-- | Marks a t'Pq.Connection'\/t'Pq.Adapter' field this test double doesn't
-- implement. Forcing it fails the test with a clear message instead of a
-- pattern-match error, pointing at exactly which unexpected call happened.
unimplemented :: String -> a
unimplemented fieldName =
  error ("Pure.Connection.AcquireSpec: fake connection field not implemented: " <> fieldName)

-- | Like 'unimplemented', but for @IO@-typed fields. Since @pqi@'s records
-- use @StrictData@, a bare 'unimplemented' in such a field would blow up the
-- moment the record is constructed, not when the field is actually run.
-- Wrapping it in 'pure' defers evaluation to the point of use.
unimplementedIO :: String -> IO a
unimplementedIO fieldName =
  pure (unimplemented fieldName)
