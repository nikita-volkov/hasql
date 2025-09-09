module Hasql.Session.Types where

import Hasql.Session.Core qualified as Session
import Hasql.Connection.Core qualified as Connection
import Hasql.OidCache qualified as OidCache
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as SessionError
import Hasql.Prelude
import Data.Text.Encoding qualified as Text
import PostgreSQL.Binary.Decoding qualified as A
import PostgreSQL.Binary.Encoding qualified as B
import TextBuilder qualified as C

-- |
-- Look up an enum type and create a decoder for it.
-- The type OID is looked up from the database and cached for future use.
enumDecoder :: Text -> (Text -> Maybe a) -> Session.Session (Decoders.Value a)
enumDecoder typeName mapping = do
  connection <- ask
  maybeOid <- liftIO $ OidCache.lookupEnumOid connection typeName
  case maybeOid of
    Just _oid -> pure $ Decoders.enum mapping -- For now, just use the standard enum decoder
    Nothing -> throwError $ SessionError.QueryError ("Unknown type: " <> Text.encodeUtf8 typeName) [] (SessionError.ClientError (Just "Type not found in pg_type"))

-- |
-- Look up an enum type and create an encoder for it.
-- The type OID is looked up from the database and cached for future use.
enumEncoder :: Text -> (a -> Text) -> Session.Session (Encoders.Value a)
enumEncoder typeName mapping = do
  connection <- ask
  maybeOid <- liftIO $ OidCache.lookupEnumOid connection typeName
  case maybeOid of
    Just _oid -> pure $ Encoders.enum mapping -- For now, just use the standard enum encoder  
    Nothing -> throwError $ SessionError.QueryError ("Unknown type: " <> Text.encodeUtf8 typeName) [] (SessionError.ClientError (Just "Type not found in pg_type"))

-- |
-- Look up a composite type and create a decoder for it.
-- The type OID is looked up from the database and cached for future use.
compositeDecoder :: Text -> Decoders.Composite a -> Session.Session (Decoders.Value a)
compositeDecoder typeName composite = do
  connection <- ask
  maybeOid <- liftIO $ OidCache.lookupCompositeOid connection typeName
  case maybeOid of
    Just _oid -> pure $ Decoders.composite composite -- For now, just use the standard composite decoder
    Nothing -> throwError $ SessionError.QueryError ("Unknown type: " <> Text.encodeUtf8 typeName) [] (SessionError.ClientError (Just "Type not found in pg_type"))

-- |
-- Look up a composite type and create an encoder for it.
-- The type OID is looked up from the database and cached for future use.
compositeEncoder :: Text -> Encoders.Composite a -> Session.Session (Encoders.Value a)
compositeEncoder typeName composite = do
  connection <- ask
  maybeOid <- liftIO $ OidCache.lookupCompositeOid connection typeName
  case maybeOid of
    Just _oid -> pure $ Encoders.composite composite -- For now, just use the standard composite encoder
    Nothing -> throwError $ SessionError.QueryError ("Unknown type: " <> Text.encodeUtf8 typeName) [] (SessionError.ClientError (Just "Type not found in pg_type"))