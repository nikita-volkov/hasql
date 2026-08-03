module Helpers.Scripts where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Pqi qualified
import Prelude
import System.Random.Stateful qualified as Random
import TextBuilder qualified

-- |
-- Parameters provided by the scope.
-- Adapter, host and port of a running isolated postgres server.
type ScopeParams = (Pqi.Adapter, Text, Word16)

onPreparableConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onPreparableConnection = onConnection False

onUnpreparableConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onUnpreparableConnection = onConnection True

onConnection :: Bool -> ScopeParams -> (Connection.Connection -> IO a) -> IO a
onConnection unpreparable (adapter, host, port) =
  bracket
    ( do
        let settings =
              mconcat
                [ Settings.hostAndPort host (fromIntegral port),
                  Settings.user "postgres",
                  Settings.password "postgres",
                  Settings.dbname "postgres",
                  Settings.noPreparedStatements unpreparable
                ]
        res <- Connection.acquire adapter settings
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
