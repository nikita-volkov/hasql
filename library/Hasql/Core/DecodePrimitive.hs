module Hasql.Core.DecodePrimitive where

import Hasql.Prelude hiding (bool)
import Hasql.Core.Model
import qualified Ptr.Parse as A
import qualified Hasql.Core.Protocol.Parse.Values as B


newtype DecodePrimitive primitive =
  DecodePrimitive (ReaderT Bool A.Parse primitive)
  deriving (Functor)


-- * Helpers
-------------------------

{-# INLINE nonDateTime #-}
nonDateTime :: A.Parse primitive -> DecodePrimitive primitive
nonDateTime parser =
  DecodePrimitive (ReaderT (const parser))

{-# INLINE dateTime #-}
dateTime :: A.Parse primitive -> A.Parse primitive -> DecodePrimitive primitive
dateTime intParser floatParser =
  DecodePrimitive (ReaderT (\case False -> floatParser; True -> intParser))

-- * Numbers
-------------------------

{-# INLINE bool #-}
bool :: DecodePrimitive Bool
bool =
  nonDateTime B.bool

{-# INLINE int4 #-}
int4 :: DecodePrimitive Int32
int4 =
  nonDateTime B.int4

{-# INLINE int8 #-}
int8 :: DecodePrimitive Int64
int8 =
  nonDateTime B.int8

-- * Blobs
-------------------------

{-# INLINE text #-}
text :: DecodePrimitive Text
text =
  nonDateTime B.text

{-# INLINE bytea #-}
bytea :: DecodePrimitive ByteString
bytea =
  nonDateTime B.bytea

-- * Time
-------------------------

{-# INLINE timestamptz #-}
timestamptz :: DecodePrimitive UTCTime
timestamptz =
  dateTime B.intTimestamptz B.floatTimestamptz
