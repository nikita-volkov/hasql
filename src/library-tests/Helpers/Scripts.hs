module Helpers.Scripts where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import System.Random.Stateful qualified as Random
import TextBuilder qualified
import Prelude

-- |
-- Parameters provided by the scope.
-- Host and port of a running isolated postgres server.
type ScopeParams = (Text, Word16)

-- |
-- Connection that prepares every statement on its first execution.
--
-- Most specs care about exercising the prepared path rather than about the
-- admission policy, and the threshold would otherwise leave them executing
-- everything unprepared.
onPreparingConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onPreparingConnection = onConnection (Settings.prepareThreshold 1)

-- |
-- Connection that never prepares anything.
onNonPreparingConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onNonPreparingConnection = onConnection (Settings.statementCacheSize 0)

-- |
-- Connection left on the library's defaults.
onDefaultConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onDefaultConnection = onConnection mempty

onConnection :: Settings.Settings -> ScopeParams -> (Connection.Connection -> IO a) -> IO a
onConnection extraSettings (host, port) =
  bracket
    ( do
        let settings =
              mconcat
                [ Settings.hostAndPort host (fromIntegral port),
                  Settings.user "postgres",
                  Settings.password "postgres",
                  Settings.dbname "postgres",
                  extraSettings
                ]
        res <- Connection.acquire settings
        case res of
          Left err -> fail ("Connection failed: " <> show err)
          Right conn -> pure conn
    )
    Connection.release

-- | Generate a unique name of the following pattern:
--
-- > <prefix><uniqueNum1><infix><uniqueNum2><suffix>
generateName :: Text -> Text -> Text -> IO Text
generateName prefix infix_ suffix = do
  uniqueNum1 <- Random.uniformWord64 Random.globalStdGen
  uniqueNum2 <- Random.uniformWord64 Random.globalStdGen
  pure
    $ TextBuilder.toText
    $ mconcat
    $ [ TextBuilder.text prefix,
        TextBuilder.decimal uniqueNum1,
        TextBuilder.text infix_,
        TextBuilder.decimal uniqueNum2,
        TextBuilder.text suffix
      ]

generateVarname :: IO Text
generateVarname = do
  generateName "testing.v" "v" ""

generateSymname :: IO Text
generateSymname =
  generateName "test_" "_" ""
