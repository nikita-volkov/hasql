module HighSQL.QQ where

import HighSQL.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified HighSQL.QQ.Parser as Parser
import qualified HighSQL.API as API
import qualified HighSQL.Backend as Backend
import qualified HighSQL.Conversion as Conversion


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
        Parser.Select   -> 'API.select
        Parser.Update   -> 'API.update
        Parser.Insert   -> 'API.insert
        Parser.Delete   -> 'API.update
        Parser.Create   -> 'API.create
        Parser.Alter    -> 'API.create
        Parser.Drop     -> 'API.create
        Parser.Truncate -> 'API.create

-- |
-- An expression of an arbitrary arg-length function, 
-- which produces a "API.Statement".
statementF :: String -> Int -> Exp
statementF s n =
  LamE pats exp
  where
    vars = map (mkName . ('_' :) . show) [1 .. n]
    pats = map VarP vars
    exp  = AppE (AppE (ConE 'API.Statement) (LitE (StringL s))) (ListE exps)
      where
        exps = map (AppE (VarE 'Conversion.toValue) . VarE) vars

fmapLamE :: (Exp -> Exp) -> Exp -> Exp
fmapLamE f =
  \case
    LamE pats exp -> LamE pats (f exp)
