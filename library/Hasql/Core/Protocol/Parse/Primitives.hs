module Hasql.Core.Protocol.Parse.Primitives where

import Hasql.Prelude hiding (fail)
import Hasql.Core.Model
import Ptr.Parse


-- * Primitives
-------------------------

{-|
Int32
The length of the column value, in bytes (this count does not include itself). Can be zero. As a special case, -1 indicates a NULL column value. No value bytes follow in the NULL case.

Byten
The value of the column, in the format indicated by the associated format code. n is the above length.
-}
{-# INLINE sized #-}
sized :: Parse result -> Parse result -> Parse result
sized nullParse sizedParse =
  {-# SCC "sized" #-} 
  do
    size <- fromIntegral <$> beWord32
    if size == -1
      then nullParse
      else sizedParse

{-|
Int32
The length of the column value, in bytes (this count does not include itself). Can be zero. As a special case, -1 indicates a NULL column value. No value bytes follow in the NULL case.

Byten
The value of the column, in the format indicated by the associated format code. n is the above length.
-}
{-# INLINE sizedBytes #-}
sizedBytes :: Parse (Maybe ByteString)
sizedBytes =
  {-# SCC "sizedBytes" #-} 
  do
    size <- fromIntegral <$> beWord32
    if size == -1
      then return Nothing
      else do
        !bytes_ <- bytes size
        return (Just bytes_)


-- * Components
-------------------------

{-# INLINE nullableDataRowColumn #-}
nullableDataRowColumn :: Parse column -> Parse (Maybe column)
nullableDataRowColumn parseColumn =
  {-# SCC "nullableDataRowColumn" #-} 
  sized (return Nothing) (fmap Just parseColumn)

{-# INLINE nonNullDataRowColumn #-}
nonNullDataRowColumn :: Parse column -> Parse column
nonNullDataRowColumn parseColumn =
  {-# SCC "nonNullDataRowColumn" #-} 
  sized (fail "Unexpected Null column value") parseColumn

