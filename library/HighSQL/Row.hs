{-# LANGUAGE UndecidableInstances #-}
module HighSQL.Row where

import HighSQL.Prelude
import Language.Haskell.TH
import qualified Data.Text as Text
import qualified HighSQL.Backend as Backend


class Row b r where
  parseResults :: [Backend.Result b] -> Maybe r

instance Row b () where
  parseResults = \case [] -> Just (); _ -> Nothing

instance Backend.Mapping b v => Row b v where
  parseResults = join . fmap (Backend.parseResult :: Backend.Result b -> Maybe v) . headMay

-- Generate tuple instaces using Template Haskell:
let
  inst :: Int -> Dec
  inst arity =
    InstanceD constraints head [fromRowDec]
    where
      varNames =
        [1 .. arity] >>= \i -> return (mkName ('_' : show i))
      varTypes =
        map VarT varNames
      backendType =
        VarT (mkName "b")
      constraints =
        map (\t -> ClassP ''Backend.Mapping [backendType, t]) varTypes
      head =
        AppT (AppT (ConT ''Row) backendType) (foldl AppT (TupleT arity) varTypes)
      fromRowDec =
        FunD 'parseResults [c1, c2]
        where
          c1 = 
            Clause [ListP (map VarP varNames)] (NormalB e) []
            where
              e =
                foldQueue queue
                where
                  con = ConE (tupleDataName arity)
                  queue =
                    (con :) $
                    (VarE '(<$>) :) $
                    intersperse (VarE '(<*>)) $
                    map (AppE (VarE 'Backend.parseResult) . VarE) varNames
                  foldQueue =
                    \case
                      e : o : t -> UInfixE e o (foldQueue t)
                      e : [] -> e
                      _ -> $bug "Unexpected queue size"
          c2 =
            Clause [WildP] (NormalB (ConE 'Nothing)) []
  in 
    mapM (return . inst) [2 .. 24]
