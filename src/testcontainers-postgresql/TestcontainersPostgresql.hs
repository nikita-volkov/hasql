module TestcontainersPostgresql where

import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Word (Word16)
import TestContainers qualified
import TestContainers.Hspec qualified
import Prelude

hook ::
  Text ->
  Text ->
  Text ->
  Bool ->
  ((Text, Word16) -> IO ()) ->
  IO ()
hook tagName user password forwardLogs =
  TestContainers.Hspec.withContainers do
    container <-
      TestContainers.run
        ( TestContainers.fromTag tagName
            & TestContainers.containerRequest
            & TestContainers.setExpose [5432]
            & TestContainers.setWaitingFor
              ( mconcat
                  [ TestContainers.waitForLogLine TestContainers.Stderr (Text.Lazy.isInfixOf "database system is ready to accept connections"),
                    TestContainers.waitUntilMappedPortReachable 5432
                  ]
              )
            & TestContainers.setEnv
              [ ("POSTGRES_USER", user),
                ("POSTGRES_PASSWORD", password)
              ]
            & (if forwardLogs then TestContainers.withFollowLogs TestContainers.consoleLogConsumer else id)
        )
    let (host, portInt) = TestContainers.containerAddress container 5432
    pure (host, fromIntegral portInt)
