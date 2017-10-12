module Hasql.Core.Batch where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Statement as A
import qualified Hasql.Core.Request as C
import qualified Hasql.Core.PreparedStatementRegistry as D
import qualified ByteString.StrictBuilder as E
import qualified Hasql.Core.DecodeResult as F
import qualified VectorBuilder.Vector as O
import qualified Data.Vector as G


newtype Batch result =
  Batch (Bool -> D.Registry -> (C.Request result, D.Registry))

deriving instance Functor Batch

instance Applicative Batch where
  {-# INLINE pure #-}
  pure x =
    Batch (\_ psr -> (pure x, psr))
  {-# INLINABLE (<*>) #-}
  (<*>) (Batch left) (Batch right) =
    Batch (\idt psr -> case left idt psr of
      (leftRequest, leftPsr) -> case right idt leftPsr of
        (rightRequest, rightPsr) -> (leftRequest <*> rightRequest, rightPsr))

statement :: A.Statement params result -> params -> Batch result
statement (A.Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 interpretResponses1 interpretResponses2 prepared) params =
  Batch $ \idt psr ->
  if prepared
    then case D.lookupOrRegister template paramOIDs psr of
      (newOrOldName, newPsr) ->
        let
          request =
            case newOrOldName of
              Left name ->
                C.unparsedStatement name template paramOIDs
                  (bool paramBytesBuilder2 paramBytesBuilder1 idt params)
                  (bool interpretResponses2 interpretResponses1 idt)
              Right name ->
                C.parsedStatement name template (G.length paramOIDs)
                  (bool paramBytesBuilder2 paramBytesBuilder1 idt params)
                  (bool interpretResponses2 interpretResponses1 idt)
          in (request, newPsr)
    else
      let
        request =
          C.unparsedStatement "" template paramOIDs
            (bool paramBytesBuilder2 paramBytesBuilder1 idt params)
            (bool interpretResponses2 interpretResponses1 idt)
        in (request, psr)
