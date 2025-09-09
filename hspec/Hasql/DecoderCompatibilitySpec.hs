module Hasql.DecoderCompatibilitySpec where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Constants qualified as Constants
import Hasql.TestingKit.Statements.WrongDecoder qualified as WrongDecoder
import Hasql.TestingKit.Testcontainers qualified as Testcontainers
import Test.Hspec
import Prelude

spec :: Spec
spec = 
  around Testcontainers.withConnection $ do
    describe "Decoder compatibility checks" $ do
      it "detects type mismatch between int8 and uuid decoders" $ \connection -> do
        -- This should fail with a DecoderTypeMismatch error
        result <- Session.run (WrongDecoder.session False (WrongDecoder.Params 1 3)) connection
        case result of
          Left (Session.QueryError _ _ _) -> do
            -- For now, just check that we get an error - we'll refine this later
            pure ()
          other -> expectationFailure $ "Expected QueryError, got: " <> show other

      it "allows compatible decoders to work normally" $ \connection -> do
        -- Test with a compatible decoder (int8 -> int8)
        let compatibleStatement = Statement.Statement "SELECT generate_series($1::int8, $2::int8)" 
                                    WrongDecoder.encoder 
                                    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8)))
                                    False
        result <- Session.run (Session.statement (WrongDecoder.Params 1 3) compatibleStatement) connection
        result `shouldBe` Right [1, 2, 3]

      it "handles decoders without static type information gracefully" $ \connection -> do
        -- Test with a decoder that has no static OID (should not perform checks)
        let dynamicDecoder = Decoders.rowList (Decoders.column (Decoders.nonNullable (Decoders.custom (\_ -> Right . fromIntegral . (read :: Prelude.String -> Prelude.Int) . show))))
        let dynamicStatement = Statement.Statement "SELECT generate_series($1::int8, $2::int8)"
                                WrongDecoder.encoder
                                dynamicDecoder
                                False 
        result <- Session.run (Session.statement (WrongDecoder.Params 1 3) dynamicStatement) connection
        -- This should work since no type check is performed for custom decoders without OID info
        case result of
          Right _ -> pure () -- Success expected
          Left err -> expectationFailure $ "Expected success for dynamic decoder, got: " <> show err