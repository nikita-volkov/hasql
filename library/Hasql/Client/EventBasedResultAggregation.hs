module Hasql.Client.EventBasedResultAggregation where

import Hasql.Prelude
import Hasql.Client.Model
import qualified BinaryParser as A
import qualified Control.Concurrent.Chan.Unagi as E


data Event =
  DataRowEvent ByteString |
  FinishEvent |
  ErrorEvent Error

{-|
Produces a result-computing action and an event enqueing action.
-}
{-# INLINE rowsReduction #-}
rowsReduction :: A.BinaryParser row -> FoldM IO row reduction -> IO (IO (Either Error reduction), Event -> IO ())
rowsReduction rowParser (FoldM progress enter exit) =
  do
    (eventInChan, eventOutChan) <- E.newChan
    let
      interpretNextEvent accumulator =
        {-# SCC "rowsReduction/interpretNextEvent" #-} 
        E.readChan eventOutChan >>= \case
          DataRowEvent bytes ->
            do
              traceEventIO "START EventBasedResultAggregation/rowsReduction/DataRow"
              case A.run (rowParser <* A.endOfInput) bytes of
                Right parsedRow ->
                  do
                    newAccumulator <- progress accumulator parsedRow
                    traceEventIO "STOP EventBasedResultAggregation/rowsReduction/DataRow"
                    interpretNextEvent newAccumulator
                Left rowParsingError ->
                  do
                    traceEventIO "STOP EventBasedResultAggregation/rowsReduction/DataRow"
                    return (Left (DecodingError ("Row: " <> rowParsingError)))
          FinishEvent ->
            fmap Right (exit accumulator)
          ErrorEvent error ->
            return (Left error)
      enqueueEvent =
        E.writeChan eventInChan
      in return (interpretNextEvent =<< enter, enqueueEvent)
