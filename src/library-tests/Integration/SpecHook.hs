module Integration.SpecHook (hook) where

import Helpers.Adapters qualified as Adapters
import Pqi qualified
import Test.Hspec

hook :: SpecWith Pqi.Adapter -> Spec
hook = Adapters.hook
