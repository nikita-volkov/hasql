module Hasql.PreparedStatementRegistry
(
  PreparedStatementRegistry,
  new,
  lookup,
  register,
)
where

import Hasql.Prelude hiding (lookup)
import qualified Data.HashTable.IO as Hashtables


data PreparedStatementRegistry =
  PreparedStatementRegistry !(Hashtables.BasicHashTable LocalKey ByteString) !(IORef Word)

{-# INLINABLE new #-}
new :: IO PreparedStatementRegistry
new =
  PreparedStatementRegistry <$> Hashtables.new <*> newIORef 0

{-# INLINABLE lookup #-}
lookup :: ByteString -> [Word32] -> PreparedStatementRegistry -> IO (Maybe ByteString)
lookup template oids (PreparedStatementRegistry table counter) =
  Hashtables.lookup table (LocalKey template oids)

{-# INLINABLE register #-}
register :: ByteString -> [Word32] -> PreparedStatementRegistry -> IO ByteString
register template oids (PreparedStatementRegistry table counter) =
  do
    n <- readIORef counter
    writeIORef counter (succ n)
    let remoteKey = fromString (show n)
    Hashtables.insert table (LocalKey template oids) remoteKey
    return remoteKey

-- |
-- Local statement key.
data LocalKey =
  LocalKey !ByteString ![Word32]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template types) =
    hashWithSalt salt template
