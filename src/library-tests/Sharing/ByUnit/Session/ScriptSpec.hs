module Sharing.ByUnit.Session.ScriptSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  it "returns ServerSessionError on syntax errors" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      result <- Connection.use connection (Session.script "THIS IS INVALID SQL")
      case result of
        Left (Errors.ScriptSessionError _ _) -> pure ()
        _ -> expectationFailure $ "Expected ScriptSessionError with ExecutionScriptError, got: " <> show result

  it "handles multi-statement DDL scripts with comments" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      tableName <- Scripts.generateSymname
      let sql =
            (mconcat . map (<> "\n"))
              [ "create table \"" <> tableName <> "_genre\" (",
                "  \"id\" int4 not null primary key,",
                "  \"name\" text not null unique",
                ");",
                "",
                "create table \"" <> tableName <> "_artist\" (",
                "  \"id\" int4 not null primary key,",
                "  \"name\" text not null",
                ");",
                "",
                "create table \"" <> tableName <> "_album\" (",
                "  \"id\" int4 not null primary key,",
                "  -- Album name.",
                "  \"name\" text not null,",
                "  -- The date the album was first released.",
                "  \"released\" date null",
                ");",
                "",
                "create table \"" <> tableName <> "_album_genre\" (",
                "  \"album\" int4 not null references \"" <> tableName <> "_album\",",
                "  \"genre\" int4 not null references \"" <> tableName <> "_genre\"",
                ");",
                "",
                "create table \"" <> tableName <> "_album_artist\" (",
                "  \"album\" int4 not null references \"" <> tableName <> "_album\",",
                "  \"artist\" int4 not null references \"" <> tableName <> "_artist\",",
                "  -- Whether it is the primary artist",
                "  \"primary\" bool not null,",
                "  primary key (\"album\", \"artist\")",
                ");"
              ]
      result <- Connection.use connection (Session.script sql)
      case result of
        Right () -> pure ()
        Left err -> expectationFailure $ "Expected success, got: " <> show err
