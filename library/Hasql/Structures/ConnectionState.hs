module Hasql.Structures.ConnectionState where

import Hasql.LibPq14 qualified as LibPQ
import Hasql.Prelude
import Hasql.Structures.RegistryState qualified as RegistryState

-- |
-- Connection state containing the core connection data.
data ConnectionState
  = ConnectionState
      -- | Whether prepared statements are allowed.
      !Bool
      -- | Lower level libpq connection.
      !LibPQ.Connection
      -- | Integer datetimes.
      !Bool
      -- | Prepared statement registry.
      !RegistryState.RegistryState