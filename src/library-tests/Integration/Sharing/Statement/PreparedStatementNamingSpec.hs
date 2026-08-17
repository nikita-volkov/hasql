-- |
-- The names hasql prepares statements under, as the server sees them.
--
-- The unit tests in @Hasql.ConnectionState.StatementCacheSpec@ pin the naming
-- function itself. This pins the property that motivates it: the name is a
-- pure function of the statement, so it is the same on every connection.
-- PgBouncer's transaction pooling depends on that — it is how it recognises
-- that a statement is already prepared on whichever server connection a
-- transaction lands on — and a driver naming statements per connection keeps
-- working fine against a bare Postgres, so nothing else here would notice the
-- property regressing.
module Integration.Sharing.Statement.PreparedStatementNamingSpec (spec) where

import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec =
  describe "Prepared statement naming" do
    it "gives the same statement the same name on different connections" \config -> do
      firstNames <- Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection (Session.statement () selectFortyTwo)
        result `shouldBe` Right 42
        selectNames connection
      secondNames <- Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection (Session.statement () selectFortyTwo)
        result `shouldBe` Right 42
        selectNames connection
      secondNames `shouldBe` firstNames

    it "gives different names to the same SQL under different parameter types" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        int4Result <- Connection.use connection (Session.statement 1 selectInt4Param)
        int4Result `shouldBe` Right 1
        textResult <- Connection.use connection (Session.statement "a" selectTextParam)
        textResult `shouldBe` Right "a"
        names <- selectNames connection
        -- Two server-side plans, because the parameter OIDs are part of the
        -- statement's identity. Collapsing them onto one name is a 42P05.
        length names `shouldBe` 2

    it "names statements with an identifier-safe prefix" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        _ <- Connection.use connection (Session.statement () selectFortyTwo)
        names <- selectNames connection
        -- Unquoted-legal and greppable in pg_prepared_statements and in a
        -- pooler's logs, and it cannot collide with a user's own PREPARE.
        all (Text.isPrefixOf "hasql_") names `shouldBe` True

selectNames :: Connection.Connection -> IO [Text]
selectNames connection = do
  result <-
    Connection.use connection
      $ Execution.sessionByParams Statements.SelectPreparedStatementNames
  case result of
    Right names -> pure names
    Left err -> fail ("Failed to read prepared statement names: " <> show err)

selectFortyTwo :: Statement.Statement () Int32
selectFortyTwo =
  Statement.preparable
    "select 42"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

selectInt4Param :: Statement.Statement Int32 Int32
selectInt4Param =
  Statement.preparable
    "select $1"
    (Encoders.param (Encoders.nonNullable Encoders.int4))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

selectTextParam :: Statement.Statement Text Text
selectTextParam =
  Statement.preparable
    "select $1"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
