-- Docs: https://hspec.github.io/hspec-discover.html
module Hasql.Comms.SpecHook where

import Hasql.Platform.Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

type HookedSpec = SpecWith (Text, Word16)

hook :: HookedSpec -> Spec
hook hookedSpec = parallel do
  byDistro "postgres:10"
  byDistro "postgres:17"
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
          (parallel hookedSpec)
