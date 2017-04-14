module Hasql.Client.EventBasedResultAggregation where

import Hasql.Prelude
import Hasql.Client.Model
import qualified BinaryParser as A


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
    eventQueue <- newTQueueIO
    let
      interpretNextEvent accumulator =
        {-# SCC "rowsReduction/interpretNextEvent" #-} 
        atomically (readTQueue eventQueue) >>= \case
          DataRowEvent bytes ->
            do
              case A.run (rowParser <* A.endOfInput) bytes of
                Right parsedRow ->
                  do
                    newAccumulator <- progress accumulator parsedRow
                    interpretNextEvent newAccumulator
                Left rowParsingError ->
                  do
                    return (Left (DecodingError ("Row: " <> rowParsingError)))
          FinishEvent ->
            fmap Right (exit accumulator)
          ErrorEvent error ->
            return (Left error)
      enqueueEvent event =
        atomically (writeTQueue eventQueue event)
      in return (interpretNextEvent =<< enter, enqueueEvent)
