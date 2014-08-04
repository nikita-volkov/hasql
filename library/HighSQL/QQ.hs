module HighSQL.QQ where

import HighSQL.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified HighSQL.QQ.Parser as Parser
import qualified HighSQL.Transaction as Transaction
import qualified Database.HDBC as HDBC


qq :: QuasiQuoter
qq = 
  QuasiQuoter
    parseExp
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")


parseExp :: String -> Q Exp
parseExp s =
  do
    (k, n) <- 
      either (fail . (showString "Parsing failure: ")) return (Parser.parse (fromString s))
    return $ fmapLamE (AppE (VarE (conName k))) (statementF s n)
  where
    conName =
      \case
        Parser.Select -> 'Transaction.select
        Parser.Update -> 'Transaction.update
        Parser.Create -> 'Transaction.create

-- |
-- An expression of an arbitrary arg-length function, 
-- which produces a "Transaction.Statement".
statementF :: String -> Int -> Exp
statementF s n =
  LamE pats exp
  where
    vars = map (mkName . ('_' :) . show) [1 .. n]
    pats = map VarP vars
    exp  = AppE (AppE (ConE 'Transaction.Statement) (LitE (StringL s))) (ListE exps)
      where
        exps = map (AppE (VarE 'HDBC.toSql) . VarE) vars

fmapLamE :: (Exp -> Exp) -> Exp -> Exp
fmapLamE f =
  \case
    LamE pats exp -> LamE pats (f exp)
