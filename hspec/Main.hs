import BasePrelude
import Test.Hspec
import qualified Hasql as H
import qualified Hasql.Backend as HB
import qualified Data.Vector as V


data X

data instance HB.StmtParam X = 
  StmtParam String 
  deriving (Eq, Show)

deriving instance Show (HB.Stmt X)
deriving instance Eq (HB.Stmt X)

instance HB.CxValue X Char where
  encodeValue = StmtParam . show

main = 
  hspec $ do
    context "Quasi quoter" $ do
      it "generates a proper statement" $ do
        (flip shouldBe)
          (HB.Stmt " SELECT (? + ?) " (V.fromList [HB.encodeValue 'a', HB.encodeValue 'b']) True)
          ([H.q| SELECT (? + ?) |] 'a' 'b' :: HB.Stmt X)
