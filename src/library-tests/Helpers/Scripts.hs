module Helpers.Scripts where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Errors qualified as Errors
import System.Random.Stateful qualified as Random
import TextBuilder qualified
import Prelude

type Acquire = Settings.Settings -> IO (Either Errors.ConnectionError Connection.Connection)

-- |
-- Parameters provided by the scope.
-- Acquire function and host/port of a running isolated postgres server.
type ScopeParams = (Acquire, Text, Word16)

onPreparableConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onPreparableConnection = onConnection False

onUnpreparableConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onUnpreparableConnection = onConnection True

onConnection :: Bool -> ScopeParams -> (Connection.Connection -> IO a) -> IO a
onConnection unpreparable (acquire, host, port) =
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
        res <- acquire settings
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
