-- |
-- An open API for implementation of specific backend drivers.
module HighSQL.Row where

import HighSQL.Prelude
import Language.Haskell.TH
import qualified Data.Text as Text
import qualified HighSQL.Backend as Backend


class Row r where
  fromRow :: [Backend.Value] -> Maybe r

-- Generate tuple instaces using Template Haskell:
$(
  let
    inst :: Int -> Dec
    inst arity =
      InstanceD constraints head [fromRowDec]
      where
        varNames =
          [1 .. arity] >>= \i -> return (mkName ('_' : show i))
        varTypes =
          map VarT varNames
        constraints =
          map (ClassP ''Backend.ValueConversion . pure) varTypes
        head =
          AppT (ConT ''Row) (foldl AppT (TupleT arity) varTypes)
        fromRowDec =
          FunD 'fromRow [c1, c2]
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
                      map (AppE (VarE 'Backend.fromValue) . VarE) varNames
                    foldQueue =
                      \case
                        e : o : t -> UInfixE e o (foldQueue t)
                        e : [] -> e
                        _ -> $bug "Unexpected queue size"
            c2 =
              Clause [WildP] (NormalB (ConE 'Nothing)) []
    in 
      mapM (return . inst) [2 .. 24]
  )

-- Generate single-type instaces using Template Haskell:
$(
  let
    inst :: Name -> Dec
    inst n =
      InstanceD [] (AppT (ConT ''Row) (ConT n)) [fromRowDec]
      where
        fromRowDec =
          FunD 'fromRow [c1, c2]
          where
            c1 = 
              Clause [ListP [VarP v]] (NormalB e) []
              where
                v = mkName "a"
                e = AppE (VarE 'Backend.fromValue) (VarE v)
            c2 =
              Clause [WildP] (NormalB (ConE 'Nothing)) []
    in 
      mapM (return . inst)
        [''NominalDiffTime, ''UTCTime, ''ZonedTime, ''TimeOfDay, ''LocalTime, 
         ''Day, ''Rational, ''Double, ''Bool, ''Char, ''Integer, ''Int64, 
         ''Int32, ''Word64, ''Word32, ''ByteString, ''Text]
  )

instance Row () where
  fromRow = \case [] -> Just (); _ -> Nothing

instance Row String where
  fromRow = fmap Text.unpack . fromRow

instance Row Word where
  fromRow = fmap (fromIntegral :: Word64 -> Word) . fromRow

instance Row Int where
  fromRow = fmap (fromIntegral :: Int64 -> Int) . fromRow
