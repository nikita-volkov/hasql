module Hasql.Core.Protocol.Parse.Values where

import Hasql.Prelude hiding (fail)
import Ptr.Parse
import Hasql.Core.Protocol.Parse.Primitives


{-# INLINE int4 #-}
int4 :: Parse Int32
int4 =
  fmap fromIntegral beWord32

{-# INLINE int8 #-}
int8 :: Parse Int64
int8 =
  fmap fromIntegral beWord64

{-# INLINE float4 #-}
float4 :: Parse Float
float4 =
  unsafeCoerce beWord32

{-# INLINE float8 #-}
float8 :: Parse Double
float8 =
  unsafeCoerce beWord64

{-# INLINE bool #-}
bool :: Parse Bool
bool =
  fmap (== 1) word8

{-# INLINE text #-}
text :: Parse Text
text =
  $(todo "text")

{-# INLINE bytea #-}
bytea :: Parse ByteString
bytea =
  $(todo "bytea")

{-# INLINE intTimestamptz #-}
intTimestamptz :: Parse UTCTime
intTimestamptz =
  $(todo "intTimestamptz")

{-# INLINE floatTimestamptz #-}
floatTimestamptz :: Parse UTCTime
floatTimestamptz =
  $(todo "floatTimestamptz")
