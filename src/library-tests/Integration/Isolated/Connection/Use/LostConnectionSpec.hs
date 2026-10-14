-- |
-- Contract of 'Connection.use' when the connection is gone by the time an
-- operation receives its results:
--
-- > Connection.use returns Left (ConnectionSessionError details), with non-empty details.
--
-- It applies whatever the server sent before closing, including a SQLSTATE,
-- and to every operation that receives results: statements (unpreparable,
-- preparable on first use and from the statement cache), pipelines, scripts
-- and the type-info lookup that precedes a statement with named types.
--
-- A dead connection must not be reported as a 'Errors.StatementSessionError'
-- or 'Errors.ScriptSessionError', because pools drop a connection only on
-- 'Errors.ConnectionSessionError' and otherwise keep handing out the dead one.
--
-- The connection dies *between* two sessions, so the next session's send still
-- succeeds (the bytes land in the kernel buffer) and the failure only shows up
-- on receive. A reset (RST) would instead make the send fail, which is a
-- different, already correctly handled path, so the silent-close fixture
-- closes gracefully (FIN).
--
-- Isolated, because the crash-shutdown fixture makes the server terminate
-- every connection it has, so the spec needs a server of its own and runs its
-- cells one at a time.
--
-- Reproduces <https://github.com/nikita-volkov/hasql/issues/341>.
module Integration.Isolated.Connection.Use.LostConnectionSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.ByteString
import Pqi qualified
import Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

spec :: SpecWith Pqi.Adapter
spec = do
  byDistro "postgres:9"
  byDistro "postgres:18"
  where
    byDistro tagName =
      describe
        (toList tagName)
        ( aroundAllWith
            ( \action adapter ->
                TestcontainersPostgresql.run
                  TestcontainersPostgresql.Config
                    { tagName,
                      auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
                      forwardLogs = False
                    }
                  (\(host, port) -> action (adapter, host, port))
            )
            byWay
        )
    byWay =
      for_ ways \Way {name, withKillableConnection, announcedSqlState} ->
        describe name do
          for_ operations \Operation {name, warmUp, session} ->
            it name \config ->
              withKillableConnection config \connection kill -> do
                warmUpResult <- Connection.use connection warmUp
                warmUpResult `shouldBe` Right ()
                kill
                outcome <- useWithinBounds connection session
                shouldBeConnectionLost announcedSqlState outcome

          -- The first use has received whatever the server announced, so the
          -- further one has no SQLSTATE to report.
          it "Reports a further use of the same handle as a lost connection too" \config ->
            withKillableConnection config \connection kill -> do
              kill
              _ <- useWithinBounds connection (void (Session.statement () selectOne))
              outcome <- useWithinBounds connection (void (Session.statement () selectOne))
              shouldBeConnectionLost Nothing outcome

-- * Expectation

-- | What a call to 'Connection.use' did.
--
-- Besides returning, it can break its contract of never throwing and never
-- blocking, which a lost connection has been seen to provoke. Those are
-- recorded as outcomes, so that they fail the test with a message instead of
-- escaping it or hanging the run.
data Outcome
  = -- | It returned.
    ReturnedOutcome (Either Errors.SessionError ())
  | -- | It threw, which the public API promises never to do.
    ThrewOutcome SomeException
  | -- | It did not return within the bounds of the test.
    HungOutcome
  deriving stock (Show)

-- | Runs 'Connection.use' and captures whatever it does.
useWithinBounds :: Connection.Connection -> Session.Session () -> IO Outcome
useWithinBounds connection session =
  fmap
    (either ThrewOutcome (maybe HungOutcome ReturnedOutcome))
    (try @SomeException (timeout 5_000_000 (Connection.use connection session)))

-- | Fails with the full actual outcome, so that a red run records the
-- misclassification it observed.
--
-- When given the SQLSTATE the server announced the termination with, also
-- requires the details to mention it, as that is the only place it survives.
shouldBeConnectionLost :: Maybe Text -> Outcome -> Expectation
shouldBeConnectionLost announcedSqlState = \case
  ReturnedOutcome (Left (Errors.ConnectionSessionError details))
    | not (Text.null details),
      all (`Text.isInfixOf` details) announcedSqlState ->
        pure ()
  actual ->
    expectationFailure
      ( mconcat
          [ "Expected: ReturnedOutcome (Left (ConnectionSessionError <non-empty details",
            foldMap ((" mentioning " <>) . Text.unpack) announcedSqlState,
            ">))\n",
            "     Got: ",
            show actual
          ]
      )

-- * Operations

-- | A session that is run as the victim's next one after the connection died.
data Operation = Operation
  { name :: String,
    -- | Runs while the connection is still healthy, before it is killed.
    warmUp :: Session.Session (),
    -- | The session under test.
    session :: Session.Session ()
  }

operations :: [Operation]
operations =
  [ Operation
      { name = "Unpreparable statement",
        warmUp = pure (),
        session = void (Session.statement () selectOneUnpreparable)
      },
    Operation
      { name = "Preparable statement, first use",
        warmUp = pure (),
        session = void (Session.statement () selectOne)
      },
    Operation
      { name = "Preparable statement, already in the statement cache",
        warmUp = void (Session.statement () selectOne),
        session = void (Session.statement () selectOne)
      },
    Operation
      { name = "Pipeline of two statements",
        warmUp = pure (),
        session =
          void
            ( Session.pipeline
                ( (,)
                    <$> Pipeline.statement () selectOne
                    <*> Pipeline.statement () selectOne
                )
            )
      },
    Operation
      { name = "Script",
        warmUp = pure (),
        session = Session.script "select 1"
      },
    Operation
      { name = "Statement with a named type, whose OID is looked up first",
        warmUp = pure (),
        -- The type needs not to exist: the lookup roundtrip fails before
        -- anything checks for it.
        session = void (Session.statement "lost_connection_spec_enum" selectNamedType)
      }
  ]

selectOne :: Statement.Statement () Int32
selectOne =
  Statement.preparable
    "select 1"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

selectOneUnpreparable :: Statement.Statement () Int32
selectOneUnpreparable =
  Statement.unpreparable
    "select 1"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

selectNamedType :: Statement.Statement Text Text
selectNamedType =
  Statement.preparable
    "select $1 :: lost_connection_spec_enum"
    (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing "lost_connection_spec_enum" id)))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing "lost_connection_spec_enum" Just))))

-- * Ways of dying

-- | A way for the connection to die between two sessions.
data Way = Way
  { name :: String,
    -- | Provides a healthy connection together with an action that kills it
    -- and returns only after the kill has landed.
    withKillableConnection ::
      Scripts.ScopeParams ->
      (Connection.Connection -> IO () -> IO ()) ->
      IO (),
    -- | The SQLSTATE the server announces the termination with in an error
    -- result, if it does.
    announcedSqlState :: Maybe Text
  }

ways :: [Way]
ways =
  [ Way
      { name = "Silent close",
        withKillableConnection = \(adapter, host, port) action ->
          withSeveringProxy host port \proxyPort sever _ ->
            withVictimConnection (adapter, "127.0.0.1", proxyPort) \connection ->
              action connection sever,
        announcedSqlState = Nothing
      },
    Way
      { name = "Server-announced termination",
        withKillableConnection = \config@(adapter, host, port) action ->
          -- The victim goes through the proxy, which here is merely a buffer.
          -- Postgres closes its socket for good when it terminates the
          -- backend, so the victim's next write would be answered with a
          -- reset, which races with the read of the FATAL message and fails
          -- the send of a multi-write operation. The proxy keeps its client
          -- side open, so the victim deterministically gets the FATAL message
          -- followed by a plain FIN.
          withSeveringProxy host port \proxyPort _ _ ->
            withVictimConnection (adapter, "127.0.0.1", proxyPort) \victim -> do
              pid <- run victim (Session.statement () selectBackendPid)
              action victim (terminateBackend config pid),
        -- admin_shutdown, which pg_terminate_backend makes the backend send.
        announcedSqlState = Just "57P01"
      },
    Way
      { name = "Crash shutdown",
        -- The shape the issue was reported with, from an AWS Aurora failover:
        -- the server sends a warning with crash_shutdown (57P02) and closes.
        -- Stock Postgres does exactly that to every backend when another one
        -- crashes. A warning is a notice, not a result, so it never reaches
        -- the receive, and the client is left with a plain EOF.
        --
        -- Goes through the proxy for the same reason as the termination.
        withKillableConnection = \config@(adapter, host, port) action ->
          finally
            ( withSeveringProxy host port \proxyPort _ awaitUpstreamClosed ->
                withVictimConnection (adapter, "127.0.0.1", proxyPort) \victim ->
                  action victim do
                    crashAnotherBackend config
                    awaitUpstreamClosed
            )
            (awaitRecovery config),
        announcedSqlState = Nothing
      }
  ]

-- | Acquires the connection that is going to be killed.
--
-- Does not use 'Scripts.onPreparableConnection', because its release
-- would block forever if a lost connection makes 'Connection.use' leave the
-- handle unusable. The release is bounded instead, and the dead connection is
-- left for the garbage collector.
withVictimConnection :: Scripts.ScopeParams -> (Connection.Connection -> IO a) -> IO a
withVictimConnection config =
  bracket (Scripts.acquire False config) \connection ->
    void (timeout 1_000_000 (Connection.release connection))

run :: Connection.Connection -> Session.Session a -> IO a
run connection session =
  Connection.use connection session >>= \case
    Left err -> fail ("Unexpected session error in fixture: " <> show err)
    Right result -> pure result

selectBackendPid :: Statement.Statement () Int32
selectBackendPid =
  Statement.preparable
    "select pg_backend_pid()"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

-- | Terminates the backend from a second connection and waits until it has
-- vanished from @pg_stat_activity@, so that the kill has landed before the
-- victim runs anything.
--
-- Uses the one-argument @pg_terminate_backend@, because the two-argument one
-- does not exist on the Postgres 9 the shared hook also runs.
terminateBackend :: Scripts.ScopeParams -> Int32 -> IO ()
terminateBackend config pid =
  Scripts.onPreparableConnection config \killer -> do
    terminated <- run killer (Session.statement pid terminate)
    unless terminated do
      fail ("pg_terminate_backend returned false for pid " <> show pid)
    waitUntilGone killer (200 :: Int)
  where
    terminate =
      Statement.preparable
        "select pg_terminate_backend($1)"
        (Encoders.param (Encoders.nonNullable Encoders.int4))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    countActive =
      Statement.preparable
        "select count(*) :: int8 from pg_stat_activity where pid = $1"
        (Encoders.param (Encoders.nonNullable Encoders.int4))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
    waitUntilGone killer attemptsLeft = do
      count <- run killer (Session.statement pid countActive)
      if count == 0
        then pure ()
        else
          if attemptsLeft <= 0
            then fail ("Backend " <> show pid <> " is still in pg_stat_activity after it was terminated")
            else do
              threadDelay 25_000
              waitUntilGone killer (attemptsLeft - 1)

-- | Makes the server go through a crash restart, by killing a backend of a
-- second connection with SIGKILL.
--
-- The postmaster then terminates every other backend with SIGQUIT, and each of
-- them sends its client a warning with crash_shutdown (57P02) before it exits.
-- The killed backend is the one running the @COPY@, which is the parent of
-- the shell the program runs in.
crashAnotherBackend :: Scripts.ScopeParams -> IO ()
crashAnotherBackend config =
  bracket (Scripts.acquire False config) Connection.release \crasher ->
    -- Fails, as the backend running it dies.
    void (Connection.use crasher (Session.script "copy (select 1) to program 'kill -KILL $PPID'"))

-- | Waits until the server accepts connections again after a crash restart,
-- so that the next test finds it usable.
awaitRecovery :: Scripts.ScopeParams -> IO ()
awaitRecovery config =
  loop (300 :: Int)
  where
    loop attemptsLeft =
      try @IOException (Scripts.acquire False config) >>= \case
        Right connection -> Connection.release connection
        Left err ->
          if attemptsLeft <= 0
            then throwIO err
            else do
              threadDelay 100_000
              loop (attemptsLeft - 1)

-- | A TCP proxy that sits between the connection under test and the database.
--
-- It accepts exactly one client and forwards bytes in both directions.
--
-- Its client-facing socket stays open until the scope ends, so whatever the
-- client sends after the database side has gone is swallowed by the kernel
-- buffer instead of being answered with a reset, and the client reads the
-- database's last words followed by a plain FIN.
--
-- The provided @sever@ action closes the database side without the database
-- saying anything: the forwarder passes the EOF on to the client as a FIN, and
-- @sever@ returns once the forwarders have stopped.
--
-- The provided @awaitUpstreamClosed@ action returns once the database has
-- closed its side and everything it sent before that has been passed on to
-- the client.
withSeveringProxy ::
  -- | Database host.
  Text ->
  -- | Database port.
  Word16 ->
  -- | Receives the proxy's port, the @sever@ action and the
  -- @awaitUpstreamClosed@ action.
  (Word16 -> IO () -> IO () -> IO a) ->
  IO a
withSeveringProxy upstreamHost upstreamPort action = do
  upstreamAddress <- do
    addresses <-
      Socket.getAddrInfo
        (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
        (Just (Text.unpack upstreamHost))
        (Just (show upstreamPort))
    case addresses of
      address : _ -> pure address
      [] -> fail ("Cannot resolve " <> Text.unpack upstreamHost)
  opened <- newIORef []
  forwarders <- newIORef []
  severance <- newEmptyMVar
  downstreamStopped <- newEmptyMVar
  let open socketAction = do
        socket <- socketAction
        atomicModifyIORef' opened \sockets -> (socket : sockets, ())
        pure socket
      -- Registers the thread before an exception can interrupt the forking
      -- one, so that the teardown never misses a forwarder.
      fork io = mask_ do
        thread <- forkIOWithUnmask \unmask -> unmask io
        atomicModifyIORef' forwarders \threads -> (thread : threads, ())
        pure thread
  listener <- open (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
  Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen listener 1
  proxyPort <- Socket.socketPort listener
  acceptor <- forkIO do
    (client, _) <- Socket.accept listener
    atomicModifyIORef' opened \sockets -> (client : sockets, ())
    upstream <-
      open
        ( Socket.socket
            (Socket.addrFamily upstreamAddress)
            Socket.Stream
            Socket.defaultProtocol
        )
    Socket.connect upstream (Socket.addrAddress upstreamAddress)
    clientToUpstream <- fork (forward client upstream)
    _ <- fork (forward upstream client `finally` putMVar downstreamStopped ())
    putMVar severance do
      Socket.shutdown upstream Socket.ShutdownBoth
      readMVar downstreamStopped
      killThread clientToUpstream
  let sever = join (readMVar severance)
      awaitUpstreamClosed = readMVar downstreamStopped
      -- Stops every thread before closing any socket. A closed descriptor is
      -- reused by the next socket any concurrent test opens, and a forwarder
      -- still holding it would read from or shut down that socket instead.
      teardown = do
        killThread acceptor
        traverse_ killThread =<< readIORef forwarders
        sockets <- readIORef opened
        for_ sockets \socket -> Socket.close socket `catch` \(_ :: IOException) -> pure ()
  action (fromIntegral proxyPort) sever awaitUpstreamClosed `finally` teardown
  where
    -- Copies bytes until the source reaches EOF or fails, then passes that on
    -- as a FIN, so that the peer never waits for bytes that cannot come.
    forward :: Socket.Socket -> Socket.Socket -> IO ()
    forward source destination =
      loop `catch` \(_ :: IOException) -> finish
      where
        finish =
          Socket.shutdown destination Socket.ShutdownSend
            `catch` \(_ :: IOException) -> pure ()
        loop = do
          chunk <- Socket.ByteString.recv source 16_384
          if ByteString.null chunk
            then finish
            else do
              Socket.ByteString.sendAll destination chunk
              loop
