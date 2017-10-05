module Hasql.Core.Connection where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Dispatcher as A
import qualified Hasql.Socket as B
import qualified Hasql.Core.Request as C
import qualified Hasql.PreparedStatementRegistry as D
import qualified Hasql.Core.InBatch as E


data Connection =
  Connection !B.Socket !(IO ()) !(forall what. C.Request what -> IO (Either Error what)) !(IORef D.Registry) !Bool

open :: IO Connection
open =
  $(todo "")

inBatch :: Connection -> E.InBatch result -> IO (Either Error result)
inBatch (Connection _ _ performRequest psrRef idt) (E.InBatch inBatchFn) =
  do
    request <- atomicModifyIORef' psrRef (swap . inBatchFn idt)
    performRequest (request <* C.sync)

close :: Connection -> IO ()
close (Connection socket stopDispatch _ _ _) =
  stopDispatch >> B.close socket
