module Hasql.Connection.Config where

import Platform.Prelude

data Config
  = Config
  { -- | Pre-rendered connection string.
    connectionString :: ByteString,
    noPreparedStatements :: Bool
  }
  deriving stock (Eq)

-- | For values that can be compiled to 'Config'.
class Constructs a where
  construct :: a -> Config
