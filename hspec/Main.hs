import BasePrelude
import Test.Hspec
import qualified Hasql as H
import qualified Hasql.Backend as HB


data X
instance HB.Backend X where
  data StatementArgument X = 
    StatementArgument String
    deriving (Eq, Show)
instance HB.Mapping X Char where
  renderValue = StatementArgument . show


main = 
  hspec $ do
    context "Quasi quoter" $ do
      it "generates a proper statement" $ do
        (flip shouldBe)
          (" SELECT ? ", [HB.renderValue 'a'], True)
          ([H.q| SELECT ? |] 'a' :: HB.Statement X)
