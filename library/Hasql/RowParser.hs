module Hasql.RowParser where

import Hasql.Prelude
import Language.Haskell.TH
import qualified Hasql.Backend as Backend
import qualified Data.Vector as Vector
import qualified Hasql.TH as THUtil


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
return $ flip map [2 .. 24] $ \arity ->
  let 
    varNames =
      [1 .. arity] >>= \i -> return (mkName ('v' : show i))
    varTypes =
      map VarT varNames
    connectionType =
      VarT (mkName "b")
    constraints =
      map (\t -> ClassP ''Backend.Mapping [connectionType, t]) varTypes
    head =
      AppT (AppT (ConT ''RowParser) connectionType) (foldl AppT (TupleT arity) varTypes)
    parseRowDec =
      FunD 'parseRow [Clause [VarP rowVarName] (NormalB e) []]
      where
        rowVarName = mkName "row"
        e =
          THUtil.applicativeE (ConE (tupleDataName arity)) lookups
          where
            lookups = do
              i <- [0 .. pred arity]
              return $ THUtil.purify $
                [|
                  Backend.parseResult
                    (Vector.unsafeIndex $(varE rowVarName) $(litE (IntegerL $ fromIntegral i)) )
                |]
    in InstanceD constraints head [parseRowDec]

