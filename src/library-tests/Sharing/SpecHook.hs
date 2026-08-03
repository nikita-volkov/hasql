-- Docs: https://hspec.github.io/hspec-discover.html
module Sharing.SpecHook where

import Helpers.Adapters qualified as Adapters
import Pqi qualified
import Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

type HookedSpec = SpecWith (Pqi.Adapter, Text, Word16)

hook :: HookedSpec -> Spec
hook hookedSpec =
  parallel
    $ Adapters.byAdapter \adapter -> do
      byDistro adapter "postgres:9"
      byDistro adapter "postgres:18"
  where
    byDistro adapter tagName =
      describe (toList tagName) do
        aroundAll
          ( TestcontainersPostgresql.run
              TestcontainersPostgresql.Config
                { tagName,
                  auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
                  forwardLogs = False
                }
          )
          (mapSubject (\(host, port) -> (adapter, host, port)) hookedSpec)
