-- | Regression tests for
-- <https://github.com/nikita-volkov/hasql/issues/323>: 'Hasql.Connection.use'
-- discarding OID-cache entries learned during an interrupted session.
--
-- Runs in "Isolated" (one dedicated container per test) rather than
-- "Sharing", because every case needs to take an exclusive lock on @pg_type@
-- for a moment, which would stall every other test sharing the container.
module Integration.Isolated.Connection.Use.OidCacheRetentionSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Pqi qualified
import Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

spec :: SpecWith Pqi.Adapter
spec = do
  it "Keeps a type's cached OID usable after a later statement in the same session throws" \adapter ->
    withSubjectAndLocker adapter \subject locker -> do
      -- Create the type outside of any transaction the failing session will
      -- roll back, so it's still around afterwards.
      createEnum subject "completed_step_enum"

      -- Statement A resolves and caches the type's OID. Statement B then
      -- throws a genuine exception, interrupting the session.
      interrupt subject do
        _ <- Session.statement () (selectEnum "completed_step_enum")
        liftIO (throwIO (userError "Simulated failure"))

      withPgTypeLocked subject locker do
        result <- Connection.use subject (Session.statement () (selectEnum "completed_step_enum"))
        result `shouldBe` Right "a"

  it "Keeps a type's cached OID usable when the statement that resolved it is the one interrupted" \adapter ->
    withSubjectAndLocker adapter \subject locker -> do
      createEnum subject "interrupted_step_enum"

      -- A single statement, which resolves the type's OID in a round trip of
      -- its own and only then issues the query that gets interrupted. The
      -- driver has the entry in hand by the time the exception lands, but
      -- 'Session.statement' hands its updated state back only on the way out
      -- of a successful round trip, so no completed step ever carries it.
      interrupted <-
        timeout
          200_000
          (Connection.use subject (Session.statement () (selectEnumSlowly "interrupted_step_enum")))
      interrupted `shouldBe` Nothing

      withPgTypeLocked subject locker do
        result <- Connection.use subject (Session.statement () (selectEnum "interrupted_step_enum"))
        result `shouldBe` Right "a"

  it "Keeps the cached OIDs of every step that completed before the throw" \adapter ->
    withSubjectAndLocker adapter \subject locker -> do
      createEnum subject "first_enum"
      createEnum subject "second_enum"

      -- Composed with explicit '>>=' rather than a do-block: with
      -- `ApplicativeDo` on, three mutually independent steps are free to
      -- desugar through '<*>', and this case is specifically about a throw
      -- nested inside an inner '>>=', where the state of the innermost step
      -- to complete has to survive being carried back out through the outer
      -- ones rather than being replaced by their older state.
      interrupt
        subject
        ( Session.statement () (selectEnum "first_enum")
            >>= \first ->
              Session.statement () (selectEnum "second_enum")
                >>= \second ->
                  liftIO (throwIO (userError (show (first, second))))
        )

      withPgTypeLocked subject locker do
        firstResult <- Connection.use subject (Session.statement () (selectEnum "first_enum"))
        firstResult `shouldBe` Right "a"
        secondResult <- Connection.use subject (Session.statement () (selectEnum "second_enum"))
        secondResult `shouldBe` Right "a"

-- | Start a dedicated container and hand the body two connections to it: a
-- @subject@, whose OID cache is what every case here is about, and a
-- @locker@, used to hold the exclusive @pg_type@ lock.
withSubjectAndLocker ::
  Pqi.Adapter ->
  (Connection.Connection -> Connection.Connection -> IO ()) ->
  IO ()
withSubjectAndLocker adapter body =
  TestcontainersPostgresql.run
    TestcontainersPostgresql.Config
      { tagName = "postgres:18",
        auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
        forwardLogs = False
      }
    \(host, port) -> do
      let settings =
            mconcat
              [ Settings.hostAndPort host port,
                Settings.user "postgres",
                Settings.password "postgres",
                Settings.dbname "postgres"
              ]
          acquire = do
            result <- Connection.acquire adapter settings
            case result of
              Left err -> fail ("Connection failed: " <> show err)
              Right connection -> pure connection
      bracket acquire Connection.release \subject ->
        bracket acquire Connection.release \locker ->
          body subject locker

-- | Create a single-value enum type of the given name.
createEnum :: Connection.Connection -> Text -> IO ()
createEnum connection name = do
  result <-
    Connection.use connection (Session.script (mconcat ["create type ", name, " as enum ('a')"]))
  result `shouldBe` Right ()

-- | Select a value of the named enum type, which forces that type's OID to be
-- resolved and cached before the query can be issued.
selectEnum :: Text -> Statement.Statement () Text
selectEnum name =
  Statement.preparable
    (mconcat ["select 'a' :: ", name])
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing name Just))))

-- | Like 'selectEnum', but the server sleeps before answering, leaving a
-- window in which the round trip can be interrupted from the client side.
-- The OID lookup that precedes the round trip has already completed by then.
selectEnumSlowly :: Text -> Statement.Statement () Text
selectEnumSlowly name =
  Statement.preparable
    (mconcat ["select 'a' :: ", name, " from pg_sleep(1)"])
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing name Just))))

-- | Run a session expected to be interrupted by a thrown exception, asserting
-- that the exception does propagate out of 'Connection.use'.
interrupt :: Connection.Connection -> Session.Session a -> IO ()
interrupt connection session = do
  result <- try @SomeException (Connection.use connection session)
  case result of
    Left _ -> pure ()
    Right _ -> expectationFailure "Expected the interrupting exception to propagate out of `use`"

-- | Take an exclusive lock on @pg_type@ from @locker@ and run @body@ while it
-- is held.
--
-- Any attempt by @subject@ to resolve a type's OID would need to read
-- @pg_type@ and would block on this lock; only a @subject@ whose OID cache
-- already holds the entry can proceed regardless. Re-parsing a statement does
-- not block on it, so a reset prepared-statement cache is not what any of
-- these cases end up measuring.
--
-- A short server-side @statement_timeout@ (rather than a client-side timeout
-- around 'Connection.use') is what bounds that block: a blocked libpq round
-- trip is a blocked foreign call, which a client-side timeout can't reliably
-- interrupt.
withPgTypeLocked :: Connection.Connection -> Connection.Connection -> IO a -> IO a
withPgTypeLocked subject locker body = do
  timeoutResult <- Connection.use subject (Session.script "set statement_timeout = '2s'")
  timeoutResult `shouldBe` Right ()
  bracket_
    ( do
        lockResult <-
          Connection.use locker (Session.script "begin; lock table pg_type in access exclusive mode")
        lockResult `shouldBe` Right ()
    )
    (void (Connection.use locker (Session.script "rollback")))
    body
