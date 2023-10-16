-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection
  ( Connection,
    ConnectionError,
    acquire,
    release,
    Settings,
    settings,
    withLibPQConnection,
  )
where

import Hasql.Private.Connection
import Hasql.Private.Settings
