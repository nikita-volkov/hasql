-- Docs: https://hspec.github.io/hspec-discover.html
module Sharing.SpecHook where

import Hasql.Connection qualified as Connection
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

hook :: SpecWith Connection.Connection -> Spec
hook =
  Testcontainers.aroundSpecWithConnection False . parallel
