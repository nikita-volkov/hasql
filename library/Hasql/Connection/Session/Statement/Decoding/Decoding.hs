module Hasql.Connection.Session.Statement.Decoding.Decoding where

import Hasql.Prelude
import qualified BinaryParser as A
import qualified PostgreSQL.Binary.Decoding as B
import qualified Hasql.OID.Primitive as C
import qualified Hasql.OID.Array as D


data Value result =
  Value (Word32 -> Maybe Text) (A.BinaryParser result) (A.BinaryParser result)

deriving instance Functor Value

{-# INLINE primitive #-}
primitive :: Word32 -> Text -> A.BinaryParser result -> A.BinaryParser result -> Value result
primitive oid name parser1 parser2 =
  Value validator parser1 parser2
  where
    validator actualOID =
      if oid == actualOID
        then Nothing
        else Just ("Type is not " <> name)

{-# INLINE int8 #-}
int8 :: Value Int64
int8 =
  primitive C.int8 "int8" B.int B.int

{-# INLINE text #-}
text :: Value Text
text =
  primitive C.text "text" B.text_strict B.text_strict
