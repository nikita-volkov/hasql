module Hasql.Connection
(
  Connection,
  B.ConnectionSettings(..),
  Error(..),
  Notification(..),
  open,
  session,
  batch,
  close,
)
where

import Hasql.Prelude hiding (interact)
import Hasql.Core.Model
import qualified Hasql.Core.Dispatcher as A
import qualified Hasql.Core.Socket as B
import qualified Hasql.Core.Request as C
import qualified Hasql.Core.PreparedStatementRegistry as D
import qualified Hasql.Core.Batch as E
import qualified Hasql.Core.Session as F
import qualified Hasql.Core.UnauthenticatedSession as G


data Connection =
  {-
  We use 'MVar' over registry insteadOf 'atomicModifyIORef' to protect
  from race conditions, when one thread might initiate a query over a prepared statement,
  which hasn't yet been committed by another thread, when that other thread has already updated
  the registry.
  -}
  Connection !B.Socket !A.Dispatcher !(MVar D.Registry) !Bool

open :: B.ConnectionSettings -> ByteString -> ByteString -> Maybe ByteString -> (Notification -> IO ()) -> IO (Either Error Connection)
open transportSettings username password databaseMaybe sendNotification =
  do
    connectionResult <- B.connect transportSettings
    case connectionResult of
      Left message -> return (Left (TransportError message))
      Right socket -> do
        dispatcher <- A.start socket sendNotification
        handshakeResult <- A.interact dispatcher (G.handshake username password databaseMaybe [])
        case handshakeResult of
          Left error -> return (Left error)
          Right errorOrIdt -> case errorOrIdt of
            Left error -> return (Left (TransportError error))
            Right idt -> do
              psrVar <- newMVar D.nil
              return (Right (Connection socket dispatcher psrVar idt))

session :: Connection -> F.Session result -> IO (Either Error result)
session connection (F.Session free) =
  runExceptT (iterM (\x -> join (ExceptT (batch connection x))) free)

batch :: Connection -> E.Batch result -> IO (Either Error result)
batch (Connection _ dispatcher psrVar idt) (E.Batch batchFn) =
  do
    psr <- takeMVar psrVar
    case batchFn idt psr of
      (request, newPsr) -> do
        result <- A.performRequest dispatcher (request <* C.sync)
        putMVar psrVar $ case result of
          Left (BackendError _ _) -> psr
          _ -> newPsr
        return result

close :: Connection -> IO ()
close (Connection socket dispatcher _ _) =
  A.stop dispatcher >> B.close socket
