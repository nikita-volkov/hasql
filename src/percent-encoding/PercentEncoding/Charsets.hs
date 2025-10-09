module PercentEncoding.Charsets where

import Data.CharSet
import Platform.Prelude hiding (fromList)

-- | Characters which don't need to be percent-encoded in URLs.
{-# NOINLINE passthrough #-}
passthrough :: CharSet
passthrough =
  mconcat
    [ lowerAsciiAlpha,
      upperAsciiAlpha,
      digit,
      fromList ['-', '.', '_', '~']
    ]

lowerAsciiAlpha :: CharSet
lowerAsciiAlpha = fromDistinctAscList ['a' .. 'z']

upperAsciiAlpha :: CharSet
upperAsciiAlpha = fromDistinctAscList ['A' .. 'Z']

digit :: CharSet
digit = fromDistinctAscList ['0' .. '9']
