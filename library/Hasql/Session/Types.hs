-- |
-- Session-level type codec functions with dynamic OID lookup.
--
-- This module provides functions for creating encoders and decoders for custom
-- PostgreSQL types (enums and composites) by name. The type OIDs are looked up
-- dynamically from the database's @pg_type@ system table and cached per connection.
--
-- __Example usage:__
--
-- @
-- data Color = Red | Green | Blue
--
-- colorToText :: Color -> Text
-- colorToText Red = "red"
-- colorToText Green = "green"  
-- colorToText Blue = "blue"
--
-- textToColor :: Text -> Maybe Color
-- textToColor "red" = Just Red
-- textToColor "green" = Just Green
-- textToColor "blue" = Just Blue
-- textToColor _ = Nothing
--
-- mySession :: Session (Maybe Color)
-- mySession = do
--   -- Create an encoder for the custom 'color_enum' type
--   colorEncoder <- enumEncoder "color_enum" colorToText
--   
--   -- Create a decoder for the same type
--   colorDecoder <- enumDecoder "color_enum" textToColor
--   
--   -- Use them in a statement
--   let statement = Statement "SELECT $1::color_enum" 
--                    (param (nonNullable colorEncoder))
--                    (singleRow (column (nullable colorDecoder)))
--                    True
--   statement Red myStatement
-- @
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