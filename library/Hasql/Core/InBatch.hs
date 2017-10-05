module Hasql.Core.InBatch where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Request as C
import qualified Hasql.PreparedStatementRegistry as D
import qualified Hasql.Core.ParseMessageStream as A
import qualified ByteString.StrictBuilder as E
import qualified Hasql.Core.EncodedParams as B
import qualified Hasql.Core.DecodeResult as F
import qualified VectorBuilder.Vector as O
import qualified Data.Vector as G


newtype InBatch result =
  InBatch (Bool -> D.Registry -> (C.Request result, D.Registry))

deriving instance Functor InBatch

instance Applicative InBatch where
  {-# INLINE pure #-}
  pure x =
    InBatch (\_ psr -> (pure x, psr))
  {-# INLINABLE (<*>) #-}
  (<*>) (InBatch left) (InBatch right) =
    InBatch (\idt psr -> case left idt psr of
      (leftRequest, leftPsr) -> case right idt leftPsr of
        (rightRequest, rightPsr) -> (leftRequest <*> rightRequest, rightPsr))

preparedStatement :: ByteString -> B.EncodedParams -> F.DecodeResult result -> InBatch result
preparedStatement template (B.EncodedParams oidVecBuilder bytesBuilder) (F.DecodeResult parseMessageStream) =
  InBatch $ \idt psr ->
  case D.lookupOrRegister template oidVec psr of
    (newOrOldName, newPsr) -> 
      let 
        request =
          case newOrOldName of
            Left name -> C.unparsedStatement name template oidVec (bytesBuilder idt) (parseMessageStream idt)
            Right name -> C.parsedStatement name template (G.length oidVec) (bytesBuilder idt) (parseMessageStream idt)
        in (request, newPsr)
  where
    oidVec = O.build oidVecBuilder

unpreparedStatement :: ByteString -> B.EncodedParams -> F.DecodeResult result -> InBatch result
unpreparedStatement template (B.EncodedParams oidVecBuilder bytesBuilder) (F.DecodeResult parseMessageStream) =
  InBatch $ \idt psr ->
  case C.unparsedStatement "" template oidVec (bytesBuilder idt) (parseMessageStream idt) of
    request -> (request, psr)
  where
    oidVec = O.build oidVecBuilder
