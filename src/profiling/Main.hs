module Main where

import Control.Exception
import Data.Vector qualified as F
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as D
import Hasql.Session qualified as B
import Hasql.Statement qualified as Statement
import TestcontainersPostgresql qualified
import Prelude

main :: IO ()
main =
  withConnection \connection -> do
    traceEventIO "START Session"
    result <- Connection.use connection sessionWithManySmallResults
    traceEventIO "STOP Session"
    case result of
      Left err ->
        putStrLn ("Error: " <> show err)
      Right vector -> do
        putStrLn ("Received " <> show (F.length vector) <> " rows")
    return ()

-- * Sessions

sessionWithManySmallParameters :: Vector (Int64, Int64) -> B.Session ()
sessionWithManySmallParameters =
  error "TODO: sessionWithManySmallParameters"

sessionWithSingleLargeResultInVector :: B.Session (Vector (Int64, Int64))
sessionWithSingleLargeResultInVector =
  B.statement () statementWithManyRowsInVector

sessionWithSingleLargeResultInList :: B.Session [(Int64, Int64)]
sessionWithSingleLargeResultInList =
  B.statement () statementWithManyRowsInList

sessionWithManySmallResults :: B.Session (Vector (Int64, Int64))
sessionWithManySmallResults =
  F.replicateM 1000 (B.statement () statementWithSingleRow)

-- * Statements

statementWithManyParameters :: Statement.Statement (Vector (Int64, Int64)) ()
statementWithManyParameters =
  error "TODO: statementWithManyParameters"

statementWithSingleRow :: Statement.Statement () (Int64, Int64)
statementWithSingleRow =
  Statement.preparable template encoder decoder
  where
    template =
      "SELECT 1 :: int8, 2 :: int8"
    encoder =
      conquer
    decoder =
      D.singleRow row
      where
        row =
          tuple <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.int8
          where
            tuple !a !b =
              (a, b)

statementWithManyRows :: (D.Row (Int64, Int64) -> D.Result result) -> Statement.Statement () result
statementWithManyRows decoder =
  Statement.preparable template encoder (decoder rowDecoder)
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    encoder =
      conquer
    rowDecoder =
      tuple <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.int8
      where
        tuple !a !b =
          (a, b)

statementWithManyRowsInVector :: Statement.Statement () (Vector (Int64, Int64))
statementWithManyRowsInVector =
  statementWithManyRows D.rowVector

statementWithManyRowsInList :: Statement.Statement () [(Int64, Int64)]
statementWithManyRowsInList =
  statementWithManyRows D.rowList

-- * Testcontainers

withConnection :: (Connection.Connection -> IO ()) -> IO ()
withConnection = withConnectionByTagName "postgres:17"

withConnectionByTagName :: Text -> (Connection.Connection -> IO ()) -> IO ()
withConnectionByTagName tagName action = withConnectionSettings tagName \settings -> do
  connection <- Connection.acquire settings
  case connection of
    Left err -> fail ("Connection failed: " <> show err)
    Right conn -> finally (action conn) (Connection.release conn)

withConnectionSettings :: Text -> (Settings.Settings -> IO ()) -> IO ()
withConnectionSettings tagName action = do
  TestcontainersPostgresql.run
    TestcontainersPostgresql.Config
      { tagName,
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
      action settings
