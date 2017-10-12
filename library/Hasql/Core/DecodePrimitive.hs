module Hasql.Core.DecodePrimitive where

import Hasql.Prelude hiding (bool)
import Hasql.Core.Model
import qualified BinaryParser as A
import qualified PostgreSQL.Binary.Decoding as B


newtype DecodePrimitive primitive =
  DecodePrimitive (ReaderT Bool A.BinaryParser primitive)
  deriving (Functor)


-- * Helpers
-------------------------

{-# INLINE nonDateTime #-}
nonDateTime :: A.BinaryParser primitive -> DecodePrimitive primitive
nonDateTime parser =
  DecodePrimitive (ReaderT (const parser))

{-# INLINE dateTime #-}
dateTime :: A.BinaryParser primitive -> A.BinaryParser primitive -> DecodePrimitive primitive
dateTime intParser floatParser =
  DecodePrimitive (ReaderT (\case False -> floatParser; True -> intParser))

-- * Numbers
-------------------------

{-# INLINE bool #-}
bool :: DecodePrimitive Bool
bool =
  nonDateTime B.bool

{-# INLINE int8 #-}
int8 :: DecodePrimitive Int64
int8 =
  nonDateTime B.int

-- * Blobs
-------------------------

{-# INLINE text #-}
text :: DecodePrimitive Text
text =
  nonDateTime B.text_strict

{-# INLINE bytea #-}
bytea :: DecodePrimitive ByteString
bytea =
  nonDateTime B.bytea_strict

-- * Time
-------------------------

{-# INLINE timestamptz #-}
timestamptz :: DecodePrimitive UTCTime
timestamptz =
  dateTime B.timestamptz_int B.timestamptz_float
