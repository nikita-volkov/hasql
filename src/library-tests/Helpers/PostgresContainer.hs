module Helpers.PostgresContainer (run) where

import Control.Exception qualified as Exception
import Data.Text qualified as Text
import Prelude
import TestContainers qualified as Docker
import TestcontainersPostgresql qualified

-- |
-- Like 'TestcontainersPostgresql.run', but retries container startup a few
-- times when Docker fails to bind the automatically assigned host port.
--
-- Docker picks the host port at container creation but only reserves it at
-- container start, so when multiple containers get created around the same
-- time (as happens across our isolated tests), two of them can race for the
-- same port and fail with \"address already in use\". Retrying with a fresh
-- container sidesteps the race.
run :: TestcontainersPostgresql.Config -> ((Text, Word16) -> IO ()) -> IO ()
run config action =
  go (3 :: Int)
  where
    go retriesLeft =
      Exception.catch
        (TestcontainersPostgresql.run config action)
        \exception ->
          if retriesLeft > 0 && isPortConflict exception
            then go (retriesLeft - 1)
            else Exception.throwIO exception

    isPortConflict Docker.DockerException {stderr} =
      "address already in use" `Text.isInfixOf` stderr
