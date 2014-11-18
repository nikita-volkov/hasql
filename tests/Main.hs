{-# OPTIONS_GHC -F -pgmF htfpp #-}
import BasePrelude
import Test.Framework
import qualified Hasql as H
import qualified Hasql.Backend as HB

main = 
  htfMain $ htf_thisModulesTests

data X
instance HB.Backend X where
  data StatementArgument X = 
    StatementArgument String
    deriving (Eq, Show)
instance HB.Mapping X Char where
  renderValue = StatementArgument . show


test_quasiQuoterGeneratesAProperStatement =
  assertEqual 
    (" SELECT ? ", [HB.renderValue 'a'], True)
    ([H.q| SELECT ? |] 'a' :: HB.Statement X)
