module Hasql.QQ where

import Hasql.Prelude
import Hasql.TH
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import qualified Data.Text as Text
import qualified Hasql.QQ.Parser as Parser
import qualified Hasql.Backend as Bknd


-- |
-- Produces a lambda-expression, 
-- which takes as many parameters as there are placeholders in the quoted text
-- and results in a 'Bknd.Stmt'. 
-- 
-- E.g.:
-- 
-- >selectSum :: Int -> Int -> Stmt c
-- >selectSum = [stmt|SELECT (? + ?)|]
-- 
-- It also allows to directly refer to free variables like so:
-- 
-- >selectSum :: Int -> Int -> Stmt c
-- >selectSum a b = [stmt|SELECT ($a + $b)|]
stmt :: QuasiQuoter
stmt = 
  QuasiQuoter
    (parseExp)
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")
  where
    parseExp =
      fmap (uncurry statementF) .
      either (fail . showString "Parsing failure: ") return .
      Parser.parse .
      fromString
    statementF t params =
      LamE
        (map VarP argNames)
        (purify [|Bknd.Stmt $(pure statementE) $(pure argsE) True|])
      where
        (varNames, argNames) =
          (\(a, b) -> (reverse a, reverse b)) $ 
          flip execState ([], []) $ forM_ params $ \case
            Parser.ParamName n ->
              modify $ \(a, b) -> (mkName (Text.unpack n) : a, b)
            Parser.OrderedPlaceholder ->
              modify $ \(a, b) -> 
                let n = mkName $ '_' : show (length b + 1)
                    in (n : a, n : b)
            Parser.IndexedPlaceholder i ->
              fail "Indexed placeholders are not supported"
        statementE = 
          LitE (StringL (Text.unpack t))
        argsE =
          vectorE $
          map (\x -> purify [| Bknd.encodeValue $(varE x) |]) $
          varNames
