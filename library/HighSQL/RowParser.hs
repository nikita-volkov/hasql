{-# LANGUAGE UndecidableInstances #-}
module HighSQL.RowParser where

import HighSQL.Prelude
import Language.Haskell.TH
import qualified Data.Text as Text
import qualified HighSQL.Backend as Backend


class RowParser b r where
  parse :: [Backend.Result b] -> Either Text r

instance RowParser b () where
  parse = \case [] -> Right (); _ -> Left $ "Row is not empty"

instance Backend.Mapping b v => RowParser b v where
  parse l = do
    h <- maybe (Left $ "Empty row") Right $ headMay l
    Backend.parseResult h

-- Generate tuple instaces using Template Haskell:
let
  inst :: Int -> Dec
  inst arity =
    InstanceD constraints head [parseDec]
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
      parseDec =
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
            Clause [WildP] (NormalB (AppE (ConE 'Left) (LitE (StringL m)))) []
            where
              m = "Not enough items in the row"
  in 
    mapM (return . inst) [2 .. 24]
