-- Docs: https://hspec.github.io/hspec-discover.html
module Integration.Sharing.SpecHook where

import Pqi qualified
import Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

type HookedSpec = SpecWith (Pqi.Adapter, Text, Word16)

hook :: HookedSpec -> SpecWith Pqi.Adapter
hook hookedSpec =
  parallel do
    byDistro "postgres:9"
    byDistro "postgres:18"
  where
    byDistro tagName =
      describe (toList tagName)
        $ aroundAllWith
          ( \action adapter ->
              TestcontainersPostgresql.run
                TestcontainersPostgresql.Config
                  { tagName,
                    auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
                    forwardLogs = False
                  }
                (\(host, port) -> action (adapter, host, port))
          )
          hookedSpec
