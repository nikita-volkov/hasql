module Hasql.Core.ParseResponses where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.ParseDataRow as A
import qualified Hasql.Core.ParseResponse as F
import qualified Data.Vector as B
import qualified Hasql.Core.Protocol.Parse.Responses as E
import qualified Ptr.Parse as C
import qualified Ptr.ByteString as D


newtype ParseResponses output =
  ParseResponses (forall result. Word8 -> (ParseResponses output -> result) -> (output -> result) -> C.Parse result -> C.Parse result)

deriving instance Functor ParseResponses

instance Applicative ParseResponses where
  pure = return
  (<*>) = ap

instance Monad ParseResponses where
  return x =
    ParseResponses $ \ type_ recurResult outputResult alternative ->
    pure (outputResult x) 
  (>>=) (ParseResponses left) rightK =
    ParseResponses $ \ type_ recurResult outputResult alternative ->
    left type_
      (\ parseResponses -> recurResult (parseResponses >>= rightK))
      (\ leftOutput -> recurResult (rightK leftOutput))
      alternative

foldRows :: Fold row output -> A.ParseDataRow row -> ParseResponses (output, Int)
foldRows (Fold step start end) pdr =
  loop start
  where
    loop !state =
      ParseResponses $ \ type_ recurResult outputResult alternative ->
      if
        | G.dataRow type_ ->
          flip fmap (E.dataRowBody pdr) $ \ !row ->
          recurResult (loop (step state row))
        | G.commandComplete type_ ->
          flip fmap (E.commandCompleteBody) $ \ !amount ->
          outputResult (end state, amount)
        | G.emptyQuery type_ ->
          return (outputResult (end state, 0))
        | True ->
          alternative
