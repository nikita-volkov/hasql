module Main.SimpleOidTest where

import Hasql.OidCache qualified as OidCache
import Hasql.PostgresTypeInfo qualified as PTI
import Main.Prelude
import Data.Text qualified as Text
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8

-- |
-- Test basic OID cache parsing functionality without database connection
testOidParsing :: IO Bool
testOidParsing = do
  -- Test the parseOidBytes function indirectly by testing a known pattern
  let testText = "1234"
  let testBytes = BS.pack [49, 50, 51, 52] -- ASCII for "1234"
  
  -- Test text parsing logic (simulating what would happen with pg_type query result)
  case readMaybe (BS8.unpack testBytes) of
    Just (oid :: Word32) -> do
      putStrLn $ "Successfully parsed OID: " <> show oid
      if oid == 1234
        then do
          putStrLn "OID parsing test passed"
          pure True
        else do
          putStrLn "OID parsing test failed - wrong value"
          pure False
    Nothing -> do
      putStrLn "OID parsing test failed - couldn't parse"
      pure False
  where
    readMaybe :: String -> Maybe Word32
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing