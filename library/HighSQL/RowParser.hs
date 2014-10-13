{-# LANGUAGE UndecidableInstances #-}
module HighSQL.RowParser where

import HighSQL.Prelude
import Language.Haskell.TH
import qualified Data.Text as Text
import qualified HighSQL.Backend as Backend


class RowParser b r where
  parse :: [Backend.Result b] -> Maybe r

instance RowParser b () where
  parse = \case [] -> Just (); _ -> Nothing

instance Backend.Mapping b v => RowParser b v where
  parse = join . fmap (Backend.parseResult :: Backend.Result b -> Maybe v) . headMay

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
        AppT (AppT (ConT ''RowParser) backendType) (foldl AppT (TupleT arity) varTypes)
      fromRowDec =
        FunD 'parse [c1, c2]
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
