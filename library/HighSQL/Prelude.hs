module HighSQL.Prelude
( 
  module Exports,
  bug,
  bottom,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (left, right, tryJust, bool, isRight, isLeft)

-- mtl-prelude
-------------------------
import MTLPrelude as Exports hiding (shift)

-- mmorph
-------------------------
import Control.Monad.Morph as Exports

-- safe
-------------------------
import Safe as Exports

-- list-t
-------------------------
import ListT as Exports (ListT)

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable)

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- time
-------------------------
import Data.Time as Exports

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH


bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"high-sql\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]
