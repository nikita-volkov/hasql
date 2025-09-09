module Hasql.OidCache where

import Hasql.Connection.Core qualified as Connection
import Hasql.LibPq14 qualified as LibPQ
import Hasql.PostgresTypeInfo qualified as PTI
import Hasql.Prelude
import Data.HashTable.IO qualified as HashTable
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEnc
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Text.Read (readMaybe)

-- |
-- Look up a type OID by name in the cache.
-- If not found, query the database and cache the result.
lookupTypeOid :: Connection.Connection -> Text -> IO (Maybe PTI.OID)
lookupTypeOid (Connection.Connection _ pqConnectionRef _ _ oidCache) typeName = do
  -- First check the cache
  cachedOid <- HashTable.lookup oidCache typeName
  case cachedOid of
    Just oid -> pure (Just oid)
    Nothing -> do
      -- Query the database for the type OID
      withMVar pqConnectionRef $ \pqConnection -> do
        queryResult <- queryTypeOid pqConnection typeName
        case queryResult of
          Just oid -> do
            -- Cache the result
            HashTable.insert oidCache typeName oid
            pure (Just oid)
          Nothing -> pure Nothing

-- |
-- Query PostgreSQL's pg_type table to get the OID for a type name.
queryTypeOid :: LibPQ.Connection -> Text -> IO (Maybe PTI.OID)
queryTypeOid pqConnection typeName = do
  -- Use simple exec since we're not parameterizing anything complex
  let query = "SELECT oid FROM pg_type WHERE typname = '" <> Text.replace "'" "''" typeName <> "'"
  let queryBytes = TextEnc.encodeUtf8 query
  
  maybeResult <- LibPQ.exec pqConnection queryBytes
  case maybeResult of
    Nothing -> pure Nothing
    Just result -> do
      status <- LibPQ.resultStatus result
      case status of
        LibPQ.TuplesOk -> do
          ntuples <- LibPQ.ntuples result
          if ntuples > 0
            then do
              maybeOidBytes <- LibPQ.getvalue result 0 0
              case maybeOidBytes of
                Just oidBytes -> do
                  -- Parse the OID as text since we're using text format
                  case readMaybe (BS8.unpack oidBytes) of
                    Just oidWord32 -> pure (Just (PTI.mkOID LibPQ.Binary oidWord32))
                    Nothing -> pure Nothing
                Nothing -> pure Nothing
            else pure Nothing
        _ -> pure Nothing

-- |
-- Look up an enum type OID by name.
lookupEnumOid :: Connection.Connection -> Text -> IO (Maybe PTI.OID)
lookupEnumOid conn typeName = lookupTypeOid conn typeName

-- |
-- Look up a composite type OID by name.
lookupCompositeOid :: Connection.Connection -> Text -> IO (Maybe PTI.OID)
lookupCompositeOid conn typeName = lookupTypeOid conn typeName