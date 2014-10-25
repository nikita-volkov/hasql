module Hasql.RowParser where

import Hasql.Prelude
import Language.Haskell.TH
import qualified Hasql.Backend as Backend
import qualified Data.Vector as Vector


class RowParser b r where
  parseRow :: Vector.Vector (Backend.Result b) -> Either Text r

instance RowParser b () where
  parseRow row = 
    if Vector.null row
      then Right ()
      else $bug "Not an empty row"

instance Backend.Mapping b v => RowParser b (Identity v) where
  parseRow row = do
    Identity <$> Backend.parseResult (Vector.unsafeHead row)

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
        FunD 'parseRow [Clause [VarP n] (NormalB e) []]
        where
          n = mkName "row"
          e =
            foldQueue queue
            where
              lookups = do
                i <- [0 .. pred arity]
                return $ purify $
                  [|
                    Backend.parseResult $ 
                    (Vector.unsafeIndex) $(varE n) $(litE (IntegerL $ fromIntegral i)) 
                  |]
              queue =
                (ConE (tupleDataName arity) :) $
                (VarE '(<$>) :) $
                intersperse (VarE '(<*>)) $
                lookups
              foldQueue =
                \case
                  e : o : t -> UInfixE e o (foldQueue t)
                  e : [] -> e
                  _ -> $bug "Unexpected queue size"
      purify = unsafePerformIO . runQ
  in 
    mapM (return . inst) [2 .. 24]
