module Hasql.CxRow where

import Hasql.Prelude
import Language.Haskell.TH
import qualified Hasql.Backend as Bknd
import qualified Data.Vector as Vector
import qualified Hasql.TH as THUtil


-- |
-- This class is only intended to be used with the supplied instances,
-- which should be enough to cover all use cases.
class CxRow c r where
  parseRow :: Bknd.ResultRow c -> Either Text r

instance CxRow c () where
  parseRow row = 
    if Vector.null row
      then Right ()
      else Left "Not an empty row"

instance Bknd.CxValue c v => CxRow c (Identity v) where
  parseRow row = do
    Identity <$> Bknd.decodeValue (Vector.unsafeHead row)

-- Generate tuple instaces using Template Haskell:
return $ flip map [2 .. 24] $ \arity ->
  let 
    varNames =
      [1 .. arity] >>= \i -> return (mkName ('v' : show i))
    varTypes =
      map VarT varNames
    connectionType =
      VarT (mkName "c")
    constraints =
      map (\t -> ClassP ''Bknd.CxValue [connectionType, t]) varTypes
    head =
      AppT (AppT (ConT ''CxRow) connectionType) (foldl AppT (TupleT arity) varTypes)
    parseRowDec =
      FunD 'parseRow [Clause [VarP rowVarName] (NormalB e) []]
      where
        rowVarName = mkName "row"
        e =
          THUtil.purify $
            [|
              let actualLength = Vector.length $(varE rowVarName)
                  expectedLength = $(litE (IntegerL $ fromIntegral arity))
                  in if actualLength == expectedLength
                    then $(pure $ THUtil.applicativeE (ConE (tupleDataName arity)) lookups)
                    else Left $ fromString $ ($ "") $
                           showString "Inappropriate row length: " . shows actualLength .
                           showString ", expecting: " . shows expectedLength . 
                           showString " instead"
            |]
          where
            lookups = do
              i <- [0 .. pred arity]
              return $ THUtil.purify $
                [|
                  Bknd.decodeValue
                    (Vector.unsafeIndex $(varE rowVarName) $(litE (IntegerL $ fromIntegral i)) )
                |]
    in InstanceD constraints head [parseRowDec]

