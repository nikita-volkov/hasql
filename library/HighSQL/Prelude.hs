module HighSQL.Prelude
( 
  module Exports,
  bug,
  bottom,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports

-- mtl-prelude
-------------------------
import MTLPrelude as Exports hiding (shift)

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH


bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"stream\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]
