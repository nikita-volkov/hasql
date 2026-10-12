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
onConnection unpreparable scope =
  bracket (acquire unpreparable scope) Connection.release

-- | Acquire a connection, failing the test if that is not possible.
--
-- The caller is responsible for releasing it. Prefer 'onConnection', which
-- does so, unless the test needs to control the release itself.
acquire :: Bool -> ScopeParams -> IO Connection.Connection
acquire unpreparable (adapter, host, port) = do
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
