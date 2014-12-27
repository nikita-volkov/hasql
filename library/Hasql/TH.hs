-- |
-- TH utils.
module Hasql.TH where

import Hasql.Prelude
import Language.Haskell.TH
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector


applicativeE :: Exp -> [Exp] -> Exp
applicativeE head =
  \case
    [] -> error "Empty expressions list"
    exps ->
      reduce $ 
        head : VarE '(<$>) : intersperse (VarE '(<*>)) exps
      where
        reduce =
          \case
            e : o : t -> UInfixE e o (reduce t)
            e : [] -> e
            _ -> error $ "Unexpected queue size. Exps: " <> show exps

purify :: Q a -> a
purify = unsafePerformIO . runQ

-- |
-- Produce a lambda expression of a given arity,
-- which efficiently constructs a vector of a size equal to the arity.
vectorLamE :: Int -> Exp
vectorLamE arity =
  LamE (map VarP argNames) body
  where
    argNames = 
      map (mkName . ('_' :) . show) [1 .. arity]
    body =
      vectorE $ map VarE argNames

vectorE :: [Exp] -> Exp
vectorE cellExps =
  if null cellExps
    then
      VarE 'Vector.empty
    else
      AppE (VarE 'runST) $ DoE $ 
        pure vectorDeclarationStmt <> cellAssignmentStmts <> pure freezingStmt
  where
    vectorVarName =
      mkName "v"
    vectorDeclarationStmt =
      (BindS 
        (VarP vectorVarName) 
        (AppE 
          (VarE 'MVector.unsafeNew) 
          (LitE (IntegerL (fromIntegral (length cellExps))))))
    cellAssignmentStmts =
      map (NoBindS . uncurry cellAssignmentExp) $ zip [0..] cellExps
      where
        cellAssignmentExp index exp =
          (AppE
            (AppE
              (AppE
                (VarE 'MVector.unsafeWrite)
                (VarE vectorVarName))
              (LitE (IntegerL (fromIntegral index))))
            (exp))
    freezingStmt =
      (NoBindS
        (AppE
          (VarE 'Vector.unsafeFreeze)
          (VarE vectorVarName)))

