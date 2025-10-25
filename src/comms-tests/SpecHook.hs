-- Docs: https://hspec.github.io/hspec-discover.html
module SpecHook where

import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

type HookedSpec = SpecWith (Text, Word16)

hook :: HookedSpec -> Spec
hook hookedSpec = parallel do
  byDistro "postgres:10"
  byDistro "postgres:17"
  where
    byDistro tagName =
      describe (toList tagName) do
        aroundAll
          ( TestcontainersPostgresql.hook
              tagName
              "postgres"
              "postgres"
              False
          )
          (parallel hookedSpec)
