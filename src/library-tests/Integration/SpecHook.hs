module Integration.SpecHook (hook) where

import Helpers.Adapters qualified as Adapters
import Pqi qualified
import Prelude
import Test.Hspec

hook :: SpecWith Pqi.Adapter -> Spec
hook hookedSpec =
  Adapters.byAdapter \adapter ->
    mapSubject (const adapter) hookedSpec
