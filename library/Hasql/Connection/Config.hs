module Hasql.Connection.Config where

import Hasql.Prelude

data Config = Config
  { connectionString :: ByteString,
    usePreparedStatements :: Bool
  }

class Updates a where
  update :: a -> Config -> Config

nil :: Config
nil =
  Config
    { connectionString = "",
      usePreparedStatements = True
    }

fromUpdates :: (Updates a) => [a] -> Config
fromUpdates = foldl' (flip update) nil
