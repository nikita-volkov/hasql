{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hasql (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hasql"
version :: Version
version = Version [1,9,1,2] []

synopsis :: String
synopsis = "Fast PostgreSQL driver with a flexible mapping API"
copyright :: String
copyright = "(c) 2014, Nikita Volkov"
homepage :: String
homepage = "https://github.com/nikita-volkov/hasql"
