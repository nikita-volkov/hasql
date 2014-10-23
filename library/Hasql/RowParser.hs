module Hasql.RowParser where

import Hasql.Prelude
import Language.Haskell.TH
import qualified Data.Text as Text
import qualified Hasql.Backend as Backend


class RowParser b r where
  parseRow :: [Backend.Result b] -> Either Text r

instance RowParser b () where
  parseRow = \case [] -> Right (); _ -> Left $ "Row is not empty"

instance Backend.Mapping b v => RowParser b (Identity v) where
  parseRow l = do
    h <- maybe (Left $ "Empty row") Right $ headMay l
    Identity <$> Backend.parseResult h

-- Generate tuple instaces using Template Haskell:
let
  inst :: Int -> Dec
  inst arity =
    InstanceD constraints head [parseRowDec]
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
      parseRowDec =
        FunD 'parseRow [c1, c2]
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
