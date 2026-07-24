module Hasql.Connection.Config where

import Hasql.Platform.Prelude

data Config
  = Config
  { -- | Pre-rendered connection string.
    connectionString :: ByteString,
    -- | Maximum amount of statements kept prepared on the connection.
    statementCacheSize :: Int,
    -- | Amount of executions after which a statement gets prepared.
    prepareThreshold :: Int
  }
  deriving stock (Eq)

-- | Default maximum amount of statements kept prepared on a connection.
defaultStatementCacheSize :: Int
defaultStatementCacheSize = 1024

-- | Default amount of executions after which a statement gets prepared.
--
-- Deliberately lower than the 5 used by pgJDBC and Npgsql: the cache is
-- per-connection and pools recycle connections, so a high threshold risks
-- short-lived connections never preparing anything. At 2 a hot statement pays
-- exactly one unprepared execution per connection, while one-shot dynamic SQL
-- is still never prepared.
defaultPrepareThreshold :: Int
defaultPrepareThreshold = 2

-- | For values that can be compiled to 'Config'.
class Constructs a where
  construct :: a -> Config
