module Hasql.Loops.Receiver where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.Socket as A
import qualified Data.ByteString as B
import qualified Scanner as C
import qualified Hasql.Scanner as D


{-# INLINABLE loop #-}
loop :: A.Socket -> (Response -> IO ()) -> (Text -> IO ()) -> IO ()
loop socket sendResponse reportError =
  processScannerResult (C.More (C.scan D.response))
  where
    processScannerResult =
      \case
        C.More consume -> do
          traceM ("Receiving more bytes")
          receivingResult <- A.receive socket (shiftL 2 12)
          case receivingResult of
            Right bytes ->
              trace ("Received " <> show (B.length bytes)) $
              if B.null bytes
                then reportError "Connection interrupted"
                else processScannerResult (consume bytes)
            Left msg ->
              reportError msg
        C.Done remainders responseMaybe -> do
          traceM ("Parsed a response: \27[1m" <> show responseMaybe <> "\27[0m, remainders amount: " <> show (B.length remainders))
          traverse_ sendResponse responseMaybe
          if B.null remainders
            then processScannerResult (C.More (C.scan D.response))
            else processScannerResult (C.scan D.response remainders)
        C.Fail remainders message -> do
          reportError (fromString message)
