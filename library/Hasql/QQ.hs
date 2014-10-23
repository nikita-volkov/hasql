module Hasql.QQ where

import Hasql.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Hasql.QQ.Parser as Parser
import qualified Hasql.Backend as Backend


-- |
-- Produces a lambda-expression, 
-- which takes as many parameters as there are placeholders in the quoted text
-- and results in an expression of type 'Backend.Statement'. 
-- 
-- E.g.:
-- 
-- >selectFive :: Statement b
-- >selectFive = [q|SELECT (? + ?)|] 2 3
-- 
q :: QuasiQuoter
q = 
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
    return $ statementF s n
  where

-- |
-- An expression of a function with an arbitrary arity, 
-- which produces a "Backend.Statement".
statementF :: String -> Int -> Exp
statementF s n =
  LamE pats exp
  where
    vars = map (mkName . ('_' :) . show) [1 .. n]
    pats = map VarP vars
    exp  = AppE (AppE (ConE '(,)) (LitE (StringL s))) (ListE exps)
      where
        exps = map (AppE (VarE 'Backend.renderValue) . VarE) vars

