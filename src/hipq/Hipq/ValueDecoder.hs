-- | Simple value decoder abstraction to replace postgresql-binary dependency
module Hipq.ValueDecoder where

import Platform.Prelude

-- | A function type that can decode a ByteString value into a Haskell value.
-- The Bool parameter indicates whether to use integer timestamps (always True for PostgreSQL 10+).
type ValueDecoder a = Bool -> ByteString -> Either Text a

-- | Parse a ByteString using a ValueDecoder.
-- This replaces postgresql-binary's valueParser function.
{-# INLINE parse #-}
parse :: ValueDecoder a -> ByteString -> Either Text a
parse decoder = decoder True  -- Always use integer timestamps for PostgreSQL 10+

-- | Convert a simple ByteString parser to a ValueDecoder.
-- This is useful for interfacing with postgresql-binary parsers.
{-# INLINE fromParser #-}
fromParser :: (ByteString -> Either Text a) -> ValueDecoder a
fromParser parser _useIntegerTimestamps = parser