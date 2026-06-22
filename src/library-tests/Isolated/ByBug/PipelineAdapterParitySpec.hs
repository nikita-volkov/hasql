-- |
-- Reproduces the benchmark scenarios in which the native adapter radically
-- outperforms the FFI one on pipelining, running the very same sessions on
-- both adapters against a single shared server and asserting that they
-- produce identical and correct results.
--
-- The point is to rule out that the native adapter's pipelining speed is an
-- artifact of it returning truncated or otherwise incorrect results.
module Isolated.ByBug.PipelineAdapterParitySpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as Decoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Pqi.Ffi qualified as Pqi.Ffi
import Pqi.Native qualified as Pqi.Native
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

spec :: Spec
spec =
  describe "On a shared server" do
    aroundAll withBothAdapters do
      describe "manySmallResults" do
        it "Pipeline matches sequential and both adapters agree" \adapters -> do
          let expected = replicate 100 (1, 2)
          checkParity adapters expected sessionWithManySmallResults
          checkParity adapters expected sessionWithManySmallResultsViaPipeline

      describe "manyLargeResults" do
        it "Pipeline matches sequential and both adapters agree" \adapters -> do
          let expected = replicate 100 largeResultRows
          checkParity adapters expected sessionWithManyLargeResults
          checkParity adapters expected sessionWithManyLargeResultsViaPipeline

-- * Harness

-- | Both adapters connected to the same server.
data Adapters = Adapters
  { ffi :: Connection.Connection,
    native :: Connection.Connection
  }

withBothAdapters :: (Adapters -> IO ()) -> IO ()
withBothAdapters action =
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
      bracket
        ( Adapters
            <$> (Connection.acquire (Proxy @Pqi.Ffi.Connection) settings >>= either (fail . show) pure)
            <*> (Connection.acquire (Proxy @Pqi.Native.Connection) settings >>= either (fail . show) pure)
        )
        (\Adapters {ffi, native} -> Connection.release ffi *> Connection.release native)
        action

-- | Run the same session on both adapters and assert that each yields the
-- expected result and that the two adapters agree with each other.
checkParity :: (Eq a, Show a) => Adapters -> a -> Session.Session a -> IO ()
checkParity Adapters {ffi, native} expected session = do
  ffiResult <- Connection.use ffi session >>= either (fail . show) pure
  nativeResult <- Connection.use native session >>= either (fail . show) pure
  ffiResult `shouldBe` expected
  nativeResult `shouldBe` expected
  ffiResult `shouldBe` nativeResult

-- * Sessions (mirrors of @src/benchmarks/Main.hs@)

sessionWithManySmallResults :: Session.Session [(Int32, Int32)]
sessionWithManySmallResults =
  replicateM 100 (Session.statement () statementWithSingleRow)

sessionWithManySmallResultsViaPipeline :: Session.Session [(Int32, Int32)]
sessionWithManySmallResultsViaPipeline =
  Session.pipeline (replicateM 100 (Pipeline.statement () statementWithSingleRow))

sessionWithManyLargeResults :: Session.Session [[(Int32, Int32)]]
sessionWithManyLargeResults =
  replicateM 100 (Session.statement () statementWithManyRows)

sessionWithManyLargeResultsViaPipeline :: Session.Session [[(Int32, Int32)]]
sessionWithManyLargeResultsViaPipeline =
  Session.pipeline (replicateM 100 (Pipeline.statement () statementWithManyRows))

-- | The rows produced by 'statementWithManyRows'.
largeResultRows :: [(Int32, Int32)]
largeResultRows =
  zip [0 .. 1000] [1000 .. 2000]

-- * Statements

statementWithSingleRow :: Statement.Statement () (Int32, Int32)
statementWithSingleRow =
  Statement.preparable "SELECT 1, 2" mempty (Decoders.singleRow row)
  where
    row =
      (,)
        <$> (Decoders.column . Decoders.nonNullable) Decoders.int4
        <*> (Decoders.column . Decoders.nonNullable) Decoders.int4

statementWithManyRows :: Statement.Statement () [(Int32, Int32)]
statementWithManyRows =
  Statement.preparable
    "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    mempty
    (Decoders.rowList row)
  where
    row =
      (,)
        <$> (Decoders.column . Decoders.nonNullable) Decoders.int4
        <*> (Decoders.column . Decoders.nonNullable) Decoders.int4
