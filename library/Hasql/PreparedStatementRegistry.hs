module Hasql.PreparedStatementRegistry
(
  Registry,
  nil,
  lookupOrRegister,
)
where

import Hasql.Prelude hiding (lookup)
import qualified Data.HashMap.Strict as A
import qualified ByteString.StrictBuilder as B


-- |
-- Local statement key.
data LocalKey =
  LocalKey !ByteString !(Vector Word32)
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template types) =
    hashWithSalt salt template


data Registry =
  Registry !(A.HashMap LocalKey ByteString) !Word

{-# INLINE nil #-}
nil :: Registry
nil =
  Registry A.empty 0

{-# INLINABLE lookupOrRegister #-}
lookupOrRegister :: ByteString -> Vector Word32 -> Registry -> (Either ByteString ByteString, Registry)
lookupOrRegister template oids (Registry hashMap counter) =
  case A.lookup localKey hashMap of
    Just remoteKey ->
      (Right remoteKey, Registry hashMap counter)
    Nothing ->
      (Left remoteKey, Registry newHashMap (succ counter))
      where
        remoteKey =
          B.builderBytes (B.asciiIntegral counter)
        newHashMap =
          A.insert localKey remoteKey hashMap
  where
    localKey =
      LocalKey template oids
