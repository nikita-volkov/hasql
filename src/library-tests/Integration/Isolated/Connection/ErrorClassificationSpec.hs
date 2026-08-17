module Integration.Isolated.Connection.ErrorClassificationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Data.List qualified as List
import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Errors qualified as Errors
import Pqi qualified
import Prelude
import System.Environment qualified as Environment
import System.Process qualified as Process
import Test.Hspec

-- | Reproductions for <https://github.com/nikita-volkov/hasql/issues/329>.
--
-- Each test below documents the classifier's /current/ (buggy) behaviour, so
-- that fixing the issue means flipping the assertions here, not deleting them.
spec :: SpecWith Pqi.Adapter
spec = describe "Issue 329: ConnectionError classification" do
  describe "Locale dependence (claim a)" do
    it "stops recognizing a plainly transient networking failure once the client locale is non-English" \adapter -> do
      -- `LC_ALL` is process-global, and once libpq has translated a message
      -- under it, the C locale it caches internally does not revert on
      -- `unsetEnv` for the rest of the process. So this can't just
      -- `setEnv`/`unsetEnv` around a single `acquire` call in-process without
      -- permanently flipping every other test in this suite to French too
      -- (confirmed by trying exactly that: it did). Instead, the one call
      -- that needs the French locale is made in a genuinely separate child
      -- process, which can't leak anything back into this one.
      childMode <- Environment.lookupEnv childModeEnvVar
      case childMode of
        Just _ ->
          if Pqi.name adapter /= "pqi-ffi"
            then pure () -- Only pqi-ffi calls libpq; nothing to report for pqi-native.
            else do
              let settings = Settings.hostAndPort "nonexistent.invalid.host" 5432
              result <- Connection.acquire adapter settings
              case result of
                Left (Errors.OtherConnectionError _) -> putStrLn childResultMarkerOther
                Left err -> putStrLn (childResultMarkerUnexpected <> show err)
                Right conn -> Connection.release conn >> putStrLn childResultMarkerUnexpectedSuccess
        Nothing ->
          if Pqi.name adapter /= "pqi-ffi"
            then pendingWith "libpq's message translation is specific to the pqi-ffi adapter; pqi-native never calls libpq"
            else do
              exePath <- Environment.getExecutablePath
              parentEnv <- Environment.getEnvironment
              output <-
                Process.readCreateProcess
                  ( Process.proc
                      exePath
                      ["--match", "Issue 329: ConnectionError classification/Locale dependence"]
                  )
                    { Process.env =
                        Just
                          ( (childModeEnvVar, "1")
                              : ("LC_ALL", "fr_FR.UTF-8")
                              : filter ((/= childModeEnvVar) . fst) parentEnv
                          )
                    }
                  ""
              -- This is the bug: under English (the default test locale), the very same
              -- failure is classified as NetworkingConnectionError (see "is reported for
              -- invalid host" above). Under French, libpq's translated message text no
              -- longer contains any of the English substrings the classifier looks for,
              -- so it falls through to OtherConnectionError, and `isTransient` silently
              -- flips from True to False for an identical underlying failure.
              unless (childResultMarkerOther `List.isInfixOf` output) do
                expectationFailure ("Expected the child process to report OtherConnectionError (the locale-mangled message defeating classification). Child output:\n" <> output)

  describe "Miscategorised pattern (claim c)" do
    it "classifies a missing Unix-socket directory as NetworkingConnectionError, though it is a permanent misconfiguration" \adapter ->
      if Pqi.name adapter /= "pqi-ffi"
        then pendingWith "pqi-native only supports TCP connections; it has no Unix-domain-socket code path to misclassify"
        else do
          let settings = Settings.host "/tmp/hasql-issue-329-nonexistent-socket-dir"
          result <- Connection.acquire adapter settings
          case result of
            -- "no such file or directory" is in the networkingErrors list, so this
            -- (permanent) misconfiguration is reported as transient.
            Left (Errors.NetworkingConnectionError _) -> pure ()
            Left err -> expectationFailure ("Expected the current (buggy) NetworkingConnectionError classification, but got: " <> show err)
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail"

  describe "Missing retryable pattern (claim b)" do
    it "classifies \"sorry, too many clients already\" as OtherConnectionError, though it is pure backpressure" \adapter -> do
      runMaxConnectionsOneContainer \(host, port) -> do
        let settings =
              mconcat
                [ Settings.hostAndPort host port,
                  Settings.user "postgres",
                  Settings.dbname "postgres"
                ]
        -- Also doubles as the readiness wait: the container takes a moment to
        -- start accepting connections after `docker run` returns.
        hogging <- retryAcquire adapter settings 100
        resultOrIOException <- try @IOException (Connection.acquire adapter settings)
        Connection.release hogging
        case resultOrIOException of
          -- Separate finding, not part of #329: on this rejection pqi-native
          -- throws an uncaught IOException from mid-handshake instead of
          -- returning a classified Left, because `establish` only wraps the
          -- initial `Transport.connect` in a handler, not the `handshake`
          -- that follows it (src/library/Pqi/Native/Connection.hs:317-327).
          Left ioException ->
            pendingWith ("pqi-native raised an uncaught IOException instead of a classified error (separate bug, unrelated to #329's classifier): " <> show ioException)
          Right result -> case result of
            -- None of the patterns in `networkingErrors` match "sorry, too
            -- many clients already", so this transient, load-shedding
            -- failure is reported as permanent.
            Left (Errors.OtherConnectionError msg) ->
              Text.toLower msg `shouldSatisfy` Text.isInfixOf "too many clients"
            Left err -> expectationFailure ("Expected the current (buggy) OtherConnectionError classification, but got: " <> show err)
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected the second connection to fail with max_connections=1 already taken"

-- | Retry 'Connection.acquire' until it succeeds, used to wait out both
-- Docker/Postgres startup and (incidentally) the "database system is
-- starting up" window this issue is also about.
-- | Env var used to tell a re-exec'd child instance of this test binary to
-- run the locale test's `acquire` call directly, instead of spawning yet
-- another child.
childModeEnvVar :: String
childModeEnvVar = "HASQL_ISSUE_329_LOCALE_CHILD"

childResultMarkerOther :: String
childResultMarkerOther = "HASQL-ISSUE-329-RESULT: OtherConnectionError"

childResultMarkerUnexpected :: String
childResultMarkerUnexpected = "HASQL-ISSUE-329-RESULT: unexpected error: "

childResultMarkerUnexpectedSuccess :: String
childResultMarkerUnexpectedSuccess = "HASQL-ISSUE-329-RESULT: unexpected success"

retryAcquire :: Pqi.Adapter -> Settings.Settings -> Int -> IO Connection.Connection
retryAcquire adapter settings attemptsLeft = do
  -- pqi-native can throw a bare IOException instead of returning a classified
  -- Left while the server is still starting up (a separate bug; see the
  -- "Missing retryable pattern" test below), so retry on that too.
  resultOrIOException <- try @IOException (Connection.acquire adapter settings)
  case resultOrIOException of
    Right (Right conn) -> pure conn
    Right (Left err) -> retryOrGiveUp (show err)
    Left ioException -> retryOrGiveUp (show ioException)
  where
    retryOrGiveUp errDescription
      | attemptsLeft > 1 = threadDelay 300000 >> retryAcquire adapter settings (attemptsLeft - 1)
      | otherwise = expectationFailure ("Container never became ready to accept connections: " <> errDescription) *> error "unreachable"

-- | A Postgres container started with @max_connections=1@, so that a second
-- concurrent connection is guaranteed to be rejected with "sorry, too many
-- clients already".
--
-- Uses raw @docker@ invocations rather than the @testcontainers@ package:
-- its 'TestContainers.setCmd' appends the command to @docker start@ instead
-- of @docker create@, which Docker rejects outright.
runMaxConnectionsOneContainer :: ((Text, Word16) -> IO ()) -> IO ()
runMaxConnectionsOneContainer action =
  bracket
    ( Text.strip . Text.pack
        <$> Process.readProcess
          "docker"
          [ "run",
            "-d",
            "--rm",
            "-e",
            "POSTGRES_HOST_AUTH_METHOD=trust",
            "-p",
            "127.0.0.1::5432",
            "postgres:18",
            "postgres",
            "-c",
            "max_connections=1",
            "-c",
            "superuser_reserved_connections=0"
          ]
          ""
    )
    (\containerId -> Process.readProcess "docker" ["stop", Text.unpack containerId] "" >>= \_ -> pure ())
    ( \containerId -> do
        portMapping <- Process.readProcess "docker" ["port", Text.unpack containerId, "5432/tcp"] ""
        case Text.splitOn ":" (Text.strip (Text.pack portMapping)) of
          [_, portText]
            | [(port, "")] <- reads (Text.unpack portText) ->
                action ("127.0.0.1", port)
          _ -> expectationFailure ("Could not parse `docker port` output: " <> portMapping)
    )
