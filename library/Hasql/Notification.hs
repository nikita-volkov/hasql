{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hasql.Notification
-- Copyright   :  (c) 2011-2015 Leon P Smith
--                (c) 2012 Joey Adams
-- License     :  MIT
--
-- Maintainer  :  Nikita Volkov <nikita.y.volkov@mail.ru>
--
-- Support for receiving asynchronous notifications via PostgreSQL's
-- Listen/Notify mechanism.  See
-- <http://www.postgresql.org/docs/9.4/static/sql-notify.html> for more
-- information.
--
-- Note that on Windows,  @getNotification@ currently uses a polling loop
-- of 1 second to check for more notifications,  due to some inadequacies
-- in GHC's IO implementation and interface on that platform.   See GHC
-- issue #7353 for more information.  While this workaround is less than
-- ideal,  notifications are still better than polling the database directly.
-- Notifications do not create any extra work for the backend,  and are
-- likely cheaper on the client side as well.
--
-- <http://hackage.haskell.org/trac/ghc/ticket/7353>
--
-----------------------------------------------------------------------------

module Hasql.Notification
     ( Notification(..)
     , getNotification
     , getNotificationNonBlocking
     , getBackendPID
     ) where

import           Hasql.Prelude
import           Hasql.Connection.Impl

import           Control.Exception ( throwIO )
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.LibPQ as PQ
import           GHC.IO.Exception  ( IOError(..) )
import           System.Posix.Types ( CPid )

#if defined(mingw32_HOST_OS)
import           Control.Concurrent ( threadDelay )
#elif !MIN_VERSION_base(4,7,0)
import           Control.Concurrent ( threadWaitRead )
#else
import           GHC.Conc           ( atomically )
import           Control.Concurrent ( threadWaitReadSTM )
#endif

data Notification = Notification
   { notificationPid     :: !CPid
   , notificationChannel :: !B.ByteString
   , notificationData    :: !B.ByteString
   }

convertNotice :: PQ.Notify -> Notification
convertNotice PQ.Notify{..}
    = Notification { notificationPid     = notifyBePid
                   , notificationChannel = notifyRelname
                   , notificationData    = notifyExtra   }

-- | Returns a single notification.   If no notifications are available,
--   'getNotification' blocks until one arrives.
--
--   It is safe to call 'getNotification' on a connection that is concurrently
--   being used for other purposes,   note however that PostgreSQL does not
--   deliver notifications while a connection is inside a transaction.

getNotification :: Connection -> IO (Either IOError Notification)
getNotification conn = join $ withConnection conn fetch
  where
    funcName = "Hasql.Notification.getNotification"

    fetch c = do
        PQ.notifies c >>= \case
          Just msg -> return (return $! (Right $! convertNotice msg))
          Nothing -> do
              PQ.socket c >>= \case
                Nothing  -> return (return (Left fdError))
#if defined(mingw32_HOST_OS)
                -- threadWaitRead doesn't work for sockets on Windows, so just
                -- poll for input every second (PQconsumeInput is non-blocking).
                --
                -- We could call select(), but FFI calls can't be interrupted
                -- with async exceptions, whereas threadDelay can.
                Just _fd -> do
                  return (threadDelay 1000000 >> loop)
#elif !MIN_VERSION_base(4,7,0)
                Just fd  -> do
                  return $ try (threadWaitRead fd) >>= \case
                              Left  err -> return (Left (setLoc err))
                              Right _   -> loop
#else
                Just fd  -> do
                  (waitRead, _) <- threadWaitReadSTM fd
                  return $ try (atomically waitRead) >>= \case
                              Left  err -> return (Left (setLoc err))
                              Right _   -> loop

#endif

    loop = join $ withConnection conn $ \c -> do
             PQ.consumeInput c
             fetch c

    setLoc :: IOError -> IOError
    setLoc err = err { ioe_location = funcName }

    fdError :: IOError
    fdError = IOError {
                     ioe_handle      = Nothing,
                     ioe_type        = ResourceVanished,
                     ioe_location    = funcName,
                     ioe_description = "failed to fetch file descriptor (did the connection time out?)",
                     ioe_errno       = Nothing,
                     ioe_filename    = Nothing
                   }

-- | Non-blocking variant of 'getNotification'.   Returns a single notification,
-- if available.   If no notifications are available,  returns 'Nothing'.

getNotificationNonBlocking :: Connection -> IO (Maybe Notification)
getNotificationNonBlocking conn =
    withConnection conn $ \c -> do
        mmsg <- PQ.notifies c
        case mmsg of
          Just msg -> return $! Just $! convertNotice msg
          Nothing -> do
              _ <- PQ.consumeInput c
              mmsg' <- PQ.notifies c
              case mmsg' of
                Just msg -> return $! Just $! convertNotice msg
                Nothing  -> return Nothing

-- | Returns the process 'CPid' of the backend server process
-- handling this connection.
--
-- The backend PID is useful for debugging purposes and for comparison
-- to NOTIFY messages (which include the PID of the notifying backend
-- process). Note that the PID belongs to a process executing on the
-- database server host, not the local host!

getBackendPID :: Connection -> IO CPid
getBackendPID conn = withConnection conn PQ.backendPID
