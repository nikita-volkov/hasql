module ConnectionString.Types.Gens where

import Data.Map.Strict qualified as Map
import Platform.Prelude
import Test.QuickCheck

-- | Generate valid hostname (domain or IP)
genHostname :: Gen Text
genHostname =
  oneof
    [ genDomain,
      genIpAddress
    ]

-- | Generate a simple domain name
genDomain :: Gen Text
genDomain = do
  parts <- listOf1 genDomainPart
  pure (fromString (intercalate "." parts))
  where
    genDomainPart = listOf1 (elements (['a' .. 'z'] <> ['0' .. '9'] <> ['-']))

-- | Generate a simple IPv4 address
genIpAddress :: Gen Text
genIpAddress = do
  a <- elements [0 .. 255 :: Int]
  b <- elements [0 .. 255 :: Int]
  c <- elements [0 .. 255 :: Int]
  d <- elements [0 .. 255 :: Int]
  pure (fromString (show a <> "." <> show b <> "." <> show c <> "." <> show d))

-- | Generate Maybe Text with safe characters (excluding special URI chars)
genMaybeText :: Int -> Gen (Maybe Text)
genMaybeText size =
  oneof
    [ pure Nothing,
      Just <$> genSafeText size
    ]

-- | Generate text with safe characters (letters, numbers, basic punctuation)
genSafeText :: Int -> Gen Text
genSafeText size = do
  len <- elements [1 .. max 1 (size `div` 2)]
  chars <- vectorOf len genSafeChar
  pure (fromString chars)
  where
    genSafeChar = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_', '-', '.'])

-- | Generate parameter map
genParams :: Int -> Gen (Map.Map Text Text)
genParams size = do
  len <- elements [0 .. max 0 (size `div` 4)]
  pairs <- vectorOf len genParamPair
  pure (Map.fromList pairs)
  where
    genParamPair = do
      key <- genSafeText (size `div` 2)
      value <- genSafeText (size `div` 2)
      pure (key, value)
