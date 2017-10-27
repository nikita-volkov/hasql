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

foldRows :: Fold row output -> A.ParseDataRow row -> ParseResponses (output, Int)
foldRows (Fold step start end) pdr =
  loop start
  where
    loop !state =
      ParseResponses $ \ type_ pdrResult outputResult parseAlternative ->
      if
        | G.dataRow type_ ->
          flip fmap (E.dataRowBody pdr) $ \ !row ->
          pdrResult (loop (step state row))
        | G.commandComplete type_ ->
          flip fmap (E.commandCompleteBody) $ \ !amount ->
          outputResult (end state, amount)
        | G.emptyQuery type_ ->
          return (outputResult (end state, 0))
        | True ->
          parseAlternative
