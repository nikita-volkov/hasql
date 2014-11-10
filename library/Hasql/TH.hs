-- |
-- TH utils.
module Hasql.TH where

import Hasql.Prelude
import Language.Haskell.TH


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
            _ -> $bug $ "Unexpected queue size. Exps: " <> show exps

purify :: Q a -> a
purify = unsafePerformIO . runQ
