module Hasql.PreparedStatementRegistry.Map
  ( -- * Pure registry operations
    RegistryState,
    empty,
    lookup,
    insert,
    reset,

    -- * Key type
    LocalKey (..),
  )
where

import Hasql.Structures.RegistryState
