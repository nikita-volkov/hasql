module Main.OidCacheTest where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.OidCache qualified as OidCache
import Hasql.PostgresTypeInfo qualified as PTI
import Hasql.Session qualified as Session
import Hasql.Session.Types qualified as SessionTypes
import Hasql.Statement qualified as Statement
import Main.Prelude
import Main.Statements qualified as Statements

-- |
-- Test basic OID cache functionality
testOidCache :: IO Bool
testOidCache = do
  connectionResult <- Connection.acquire []
  case connectionResult of
    Left err -> do
      putStrLn $ "Failed to connect: " <> show err
      pure False
    Right connection -> do
      result <- Session.run testSession connection
      Connection.release connection
      case result of
        Left err -> do
          putStrLn $ "Session error: " <> show err
          pure False
        Right success -> pure success

testSession :: Session.Session Bool
testSession = do
  connection <- ask

  -- Test looking up a built-in type
  oidResult <- liftIO $ OidCache.lookupTypeOid connection "int4"
  case oidResult of
    Just oid -> liftIO $ putStrLn $ "Found int4 OID: " <> show (PTI.oidWord32 oid)
    Nothing -> liftIO $ putStrLn "Failed to find int4 OID"

  -- Test the enum decoder with a built-in type
  enumDecoder <- SessionTypes.enumDecoder "bool" $ \case
    "t" -> Just True
    "f" -> Just False
    _ -> Nothing

  liftIO $ putStrLn "Successfully created enum decoder for bool type"

  -- Test looking up a non-existent type
  nonExistentResult <- liftIO $ OidCache.lookupTypeOid connection "non_existent_type"
  case nonExistentResult of
    Just _ -> do
      liftIO $ putStrLn "ERROR: Found OID for non-existent type"
      pure False
    Nothing -> do
      liftIO $ putStrLn "Correctly returned Nothing for non-existent type"
      pure True
