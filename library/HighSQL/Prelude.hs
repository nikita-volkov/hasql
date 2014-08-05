module HighSQL.Prelude
( 
  module Exports,
  bug,
  bottom,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (left, right, tryJust, bool)

-- mtl-prelude
-------------------------
import MTLPrelude as Exports hiding (shift)

-- list-t
-------------------------
import ListT as Exports (ListT)

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- time
-------------------------
import Data.Time as Exports

-- unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)
import Data.HashSet as Exports (HashSet)

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable)

-- errors
-------------------------
import Control.Error as Exports

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
