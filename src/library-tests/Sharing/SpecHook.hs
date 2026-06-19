-- Docs: https://hspec.github.io/hspec-discover.html
module Sharing.SpecHook where

import Hasql.Connection qualified as Connection
import Helpers.Scripts qualified as Scripts
import Pqi.Ffi qualified as Pqi.Ffi
import Pqi.Native qualified as Pqi.Native
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

type HookedSpec = SpecWith Scripts.ScopeParams

hook :: HookedSpec -> Spec
hook hookedSpec = parallel do
  byDistro "postgres:9"
  byDistro "postgres:18"
  where
    byDistro tagName =
      describe (toList tagName) do
        aroundAll
          ( TestcontainersPostgresql.run
              TestcontainersPostgresql.Config
                { tagName,
                  auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
                  forwardLogs = False
                }
          )
          do
            forM_ adapters \(adapterName, acquire) ->
              describe adapterName
                $ aroundWith (\action (host, port) -> action (acquire, host, port)) (parallel hookedSpec)
    adapters =
      [ ("pqi-ffi", Connection.acquire (Proxy @Pqi.Ffi.Connection)),
        ("pqi-native", Connection.acquire (Proxy @Pqi.Native.Connection))
      ]
