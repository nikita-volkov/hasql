module Hasql.Core.Statement where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.EncodeParams as A
import qualified Hasql.Core.DecodeResult as B
import qualified Hasql.Core.ParseResponses as C
import qualified ByteString.StrictBuilder as D
import qualified VectorBuilder.Vector as O


data Statement params result =
  Statement
    ByteString
    (Vector Word32) (params -> D.Builder) (params -> D.Builder)
    (C.ParseResponses result) (C.ParseResponses result)
    Bool

deriving instance Functor (Statement params)

instance Profunctor Statement where
  {-# INLINE lmap #-}
  lmap fn (Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 interpretResponses1 interpretResponses2 prepared) =
    Statement template paramOIDs (paramBytesBuilder1 . fn) (paramBytesBuilder2 . fn) interpretResponses1 interpretResponses2 prepared
  {-# INLINE rmap #-}
  rmap fn (Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 interpretResponses1 interpretResponses2 prepared) =
    Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 (fn <$> interpretResponses1) (fn <$> interpretResponses2) prepared
  {-# INLINE dimap #-}
  dimap lfn rfn (Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 interpretResponses1 interpretResponses2 prepared) =
    Statement template paramOIDs (paramBytesBuilder1 . lfn) (paramBytesBuilder2 . lfn) (rfn <$> interpretResponses1) (rfn <$> interpretResponses2) prepared

prepared :: ByteString -> A.EncodeParams params -> B.DecodeResult result -> Statement params result
prepared template (A.EncodeParams oidVecBuilder builder1 builder2) (B.DecodeResult (ReaderT interpretResponses)) =
  Statement template (O.build oidVecBuilder) builder1 builder2 (interpretResponses True) (interpretResponses False) True

unprepared :: ByteString -> A.EncodeParams params -> B.DecodeResult result -> Statement params result
unprepared template (A.EncodeParams oidVecBuilder builder1 builder2) (B.DecodeResult (ReaderT interpretResponses)) =
  Statement template (O.build oidVecBuilder) builder1 builder2 (interpretResponses True) (interpretResponses False) False
