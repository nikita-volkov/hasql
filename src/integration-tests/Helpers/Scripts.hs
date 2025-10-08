module Helpers.Scripts where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Connection.Setting.Connection qualified as Connection.Setting.Connection
import Hasql.Connection.Setting.Connection.Param qualified as Connection.Setting.Connection.Param
import System.Random.Stateful qualified as Random
import TextBuilder qualified
import Prelude

-- |
-- Parameters provided by the scope.
-- Host and port of a running isolated postgres server.
type ScopeParams = (Text, Word16)

onPreparableConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onPreparableConnection = onConnection False

onUnpreparableConnection :: ScopeParams -> (Connection.Connection -> IO a) -> IO a
onUnpreparableConnection = onConnection True

onConnection :: Bool -> ScopeParams -> (Connection.Connection -> IO a) -> IO a
onConnection unpreparable (host, port) =
  bracket
    ( do
        res <-
          Connection.acquire
            [ Connection.Setting.connection
                ( Connection.Setting.Connection.params
                    [ Connection.Setting.Connection.Param.host host,
                      Connection.Setting.Connection.Param.port (fromIntegral port),
                      Connection.Setting.Connection.Param.user "postgres",
                      Connection.Setting.Connection.Param.password "postgres",
                      Connection.Setting.Connection.Param.dbname "postgres"
                    ]
                ),
              Connection.Setting.noPreparedStatements unpreparable
            ]
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
