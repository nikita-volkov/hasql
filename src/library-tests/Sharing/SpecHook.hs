-- Docs: https://hspec.github.io/hspec-discover.html
module Sharing.SpecHook where

import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

type HookedSpec = SpecWith (Text, Word16)

hook :: HookedSpec -> Spec
hook hookedSpec = parallel do
  byDistro "postgres:10" TestcontainersPostgresql.Distro10
  byDistro "postgres:17" TestcontainersPostgresql.Distro17
  where
    byDistro name distro =
      describe name do
        aroundAll
          ( \handler ->
              TestcontainersPostgresql.run
                TestcontainersPostgresql.Config
                  { forwardLogs = False,
                    distro,
                    auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres"
                  }
                ( \(host, portInt) ->
                    handler (host, fromIntegral portInt)
                )
          )
          (parallel hookedSpec)
