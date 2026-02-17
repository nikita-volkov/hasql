-- Docs: https://hspec.github.io/hspec-discover.html
module Sharing.SpecHook where

import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

type HookedSpec = SpecWith (Text, Word16)

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
          (parallel hookedSpec)
