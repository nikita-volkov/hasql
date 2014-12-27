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
          (HB.Stmt "SELECT (? + ?)" (V.fromList [HB.encodeValue 'a', HB.encodeValue 'b']) True)
          ([H.stmt| SELECT (? + ?) |] 'a' 'b' :: HB.Stmt X)
      it "does not drop quotes" $ do
        let 
          HB.Stmt t _ _ =
            [H.stmt| SELECT "a", 'b' |]
        (flip shouldBe)
          "SELECT \"a\", 'b'"
          t
      it "cleans whitespace" $ do
        let 
          HB.Stmt t _ _ =
            [H.stmt| CREATE TABLE data (
                       field1    DECIMAL NOT NULL,
                       field2    BIGINT  NOT NULL,
                       PRIMARY KEY (field1)
                     ) |]
        (flip shouldBe)
          "CREATE TABLE data ( field1 DECIMAL NOT NULL, field2 BIGINT NOT NULL, PRIMARY KEY (field1) )"
          t
