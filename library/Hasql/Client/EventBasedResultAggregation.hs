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
rowsReduction :: A.BinaryParser row -> FoldM IO row reduction -> IO (IO (Either Error reduction), Event -> IO ())
rowsReduction rowParser (FoldM progress enter exit) =
  do
    (eventInChan, eventOutChan) <- E.newChan
    let
      interpretNextEvent accumulator =
        E.readChan eventOutChan >>= \case
          DataRowEvent bytes ->
            case A.run (rowParser <* A.endOfInput) bytes of
              Right parsedRow ->
                do
                  newAccumulator <- progress accumulator parsedRow
                  interpretNextEvent newAccumulator
              Left rowParsingError ->
                return (Left (DecodingError ("Row: " <> rowParsingError)))
          FinishEvent ->
            fmap Right (exit accumulator)
          ErrorEvent error ->
            return (Left error)
      enqueueEvent =
        E.writeChan eventInChan
      in return (interpretNextEvent =<< enter, enqueueEvent)
