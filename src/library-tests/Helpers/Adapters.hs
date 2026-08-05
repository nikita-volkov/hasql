module Helpers.Adapters
  ( adapters,
    byAdapter,
    hook,
  )
where

import Pqi qualified
import Pqi.Ffi qualified
import Pqi.Native qualified
import Prelude
import Test.Hspec

adapters :: [Pqi.Adapter]
adapters =
  [ Pqi.Ffi.adapter,
    Pqi.Native.adapter
  ]

-- | Run the given spec-building function once per available Pqi adapter,
-- nesting each run under a @describe@ named after the adapter.
byAdapter :: (Pqi.Adapter -> Spec) -> Spec
byAdapter f =
  for_ adapters \adapter ->
    describe (toList (Pqi.name adapter)) (f adapter)

hook :: SpecWith Pqi.Adapter -> Spec
hook hookedSpec =
  byAdapter \adapter ->
    mapSubject (const adapter) hookedSpec
