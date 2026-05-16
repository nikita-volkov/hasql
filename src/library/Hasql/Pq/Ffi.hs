{-# LANGUAGE CApiFFI #-}

module Hasql.Pq.Ffi where

import Database.PostgreSQL.LibPQ.Internal
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Prelude (Bool, IO)

foreign import capi "libpq-fe.h PQresultStatus"
  resultStatus :: Ptr () -> IO CInt

foreign import capi "libpq-fe.h PQpipelineStatus"
  pipelineStatus :: Ptr PGconn -> IO CInt

foreign import capi "libpq-fe.h PQenterPipelineMode"
  enterPipelineMode :: Ptr PGconn -> IO CInt

foreign import capi "libpq-fe.h PQexitPipelineMode"
  exitPipelineMode :: Ptr PGconn -> IO CInt

foreign import capi "libpq-fe.h PQpipelineSync"
  pipelineSync :: Ptr PGconn -> IO CInt

foreign import capi "libpq-fe.h PQsendFlushRequest"
  sendFlushRequest :: Ptr PGconn -> IO CInt
