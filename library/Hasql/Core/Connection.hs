module Hasql.Core.Connection where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Dispatcher as A
import qualified Hasql.Socket as B
import qualified Hasql.Core.Request as C
import qualified Hasql.PreparedStatementRegistry as D
import qualified Hasql.Core.Query as E
import qualified Hasql.Core.Interact as G


data Connection =
  {-
  We use 'MVar' over registry insteadOf 'atomicModifyIORef' to protect
  from race conditions, when one thread might initiate a query over a prepared statement,
  which hasn't yet been committed by another thread, when that other thread has already updated
  the registry.
  -}
  Connection !B.Socket !A.Dispatcher !(MVar D.Registry) !Bool

open :: B.ConnectionSettings -> ByteString -> ByteString -> Maybe ByteString -> (Either Error Notification -> IO ()) -> IO (Either Error Connection)
open transportSettings username password databaseMaybe sendErrorOrNotification =
  do
    connectionResult <- B.connect transportSettings
    case connectionResult of
      Left message -> return (Left (TransportError message))
      Right socket -> do
        dispatcher <- A.start socket sendErrorOrNotification
        handshakeResult <- A.interact dispatcher (G.handshake username password databaseMaybe [])
        case handshakeResult of
          Left error -> return (Left error)
          Right errorOrIdt -> case errorOrIdt of
            Left error -> return (Left (TransportError error))
            Right idt -> do
              psrVar <- newMVar D.nil
              return (Right (Connection socket dispatcher psrVar idt))

query :: Connection -> E.Query result -> IO (Either Error result)
query (Connection _ dispatcher psrVar idt) (E.Query queryFn) =
  do
    psr <- takeMVar psrVar
    case queryFn idt psr of
      (request, newPsr) -> do
        result <- A.performRequest dispatcher (request <* C.sync)
        putMVar psrVar newPsr
        return result

close :: Connection -> IO ()
close (Connection socket dispatcher _ _) =
  A.stop dispatcher >> B.close socket
