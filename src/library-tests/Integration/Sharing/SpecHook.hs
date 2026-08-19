-- Docs: https://hspec.github.io/hspec-discover.html
module Integration.Sharing.SpecHook where

import Helpers.PostgresContainer qualified as PostgresContainer
import Helpers.Scripts qualified as Scripts
import Pqi qualified
import Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

type HookedSpec = SpecWith Scripts.ScopeParams

hook :: HookedSpec -> SpecWith Pqi.Adapter
hook hookedSpec = do
  byDistro "postgres:9"
  byDistro "postgres:18"
  where
    byDistro tagName =
      describe (toList tagName)
        $ aroundAllWith
          ( \action adapter ->
              PostgresContainer.run
                TestcontainersPostgresql.Config
                  { tagName,
                    auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
                    forwardLogs = False
                  }
                (\(host, port) -> action (adapter, host, port))
          )
          (parallel hookedSpec)
