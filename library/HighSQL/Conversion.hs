-- |
-- An open API for implementation of specific backend drivers.
module HighSQL.Conversion where

import HighSQL.Prelude
import Language.Haskell.TH
import qualified Data.Text as Text
import qualified HighSQL.Backend as Backend


-- * Value
-------------------------

class Value a where
  toValue :: a -> Backend.Value
  fromValue :: Backend.Value -> Maybe a

-- Generate standard instances using Template Haskell:
let
  inst :: Name -> Name -> Dec
  inst t c = 
    InstanceD [] (AppT (ConT ''Value) (ConT t)) [d1, d2]
    where
      d1 =
        FunD 'toValue [Clause [] (NormalB (ConE c)) []]
      d2 = 
        FunD 'fromValue [c1, c2]
        where
          c1 = 
            Clause [p] (NormalB e) []
            where
              p = ConP c [VarP v]
              v = mkName "a"
              e = AppE (ConE 'Just) (VarE v)
          c2 = 
            Clause [WildP] (NormalB (ConE 'Nothing)) []
  in 
    mapM (return . uncurry inst) 
      [
        (''NominalDiffTime, 'Backend.Value_NominalDiffTime),
        (''UTCTime, 'Backend.Value_UTCTime),
        (''ZonedTime, 'Backend.Value_ZonedTime),
        (''TimeOfDay, 'Backend.Value_TimeOfDay),
        (''LocalTime, 'Backend.Value_LocalTime),
        (''Day, 'Backend.Value_Day),
        (''Rational, 'Backend.Value_Rational),
        (''Double, 'Backend.Value_Double),
        (''Bool, 'Backend.Value_Bool),
        (''Char, 'Backend.Value_Char),
        (''Integer, 'Backend.Value_Integer),
        (''Int64, 'Backend.Value_Int64),
        (''Int32, 'Backend.Value_Int32),
        (''Word64, 'Backend.Value_Word64),
        (''Word32, 'Backend.Value_Word32),
        (''ByteString, 'Backend.Value_ByteString),
        (''Text, 'Backend.Value_Text)
      ]

instance Value String where
  toValue = Backend.Value_Text . Text.pack
  fromValue = \case Backend.Value_Text a -> Just (Text.unpack a); _ -> Nothing

instance Value Word where
  toValue = Backend.Value_Word64 . fromIntegral
  fromValue = \case Backend.Value_Word64 a -> Just (fromIntegral a); _ -> Nothing

instance Value Int where
  toValue = Backend.Value_Int64 . fromIntegral
  fromValue = \case Backend.Value_Int64 a -> Just (fromIntegral a); _ -> Nothing

instance Value a => Value (Maybe a) where
  toValue = Backend.Value_Maybe . fmap toValue
  fromValue = \case Backend.Value_Maybe a -> traverse fromValue a; _ -> Nothing


-- * Row
-------------------------

class Row r where
  fromRow :: [Backend.Value] -> Maybe r

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
      constraints =
        map (ClassP ''Value . pure) varTypes
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
                    map (AppE (VarE 'fromValue) . VarE) varNames
                  foldQueue =
                    \case
                      e : o : t -> UInfixE e o (foldQueue t)
                      e : [] -> e
                      _ -> $bug "Unexpected queue size"
          c2 =
            Clause [WildP] (NormalB (ConE 'Nothing)) []
  in 
    mapM (return . inst) [2 .. 24]

-- Generate single-type instaces using Template Haskell:
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
              e = AppE (VarE 'fromValue) (VarE v)
          c2 =
            Clause [WildP] (NormalB (ConE 'Nothing)) []
  in 
    mapM (return . inst)
      [''NominalDiffTime, ''UTCTime, ''ZonedTime, ''TimeOfDay, ''LocalTime, 
       ''Day, ''Rational, ''Double, ''Bool, ''Char, ''Integer, ''Int64, 
       ''Int32, ''Word64, ''Word32, ''ByteString, ''Text]

instance Row () where
  fromRow = \case [] -> Just (); _ -> Nothing

instance Row String where
  fromRow = fmap Text.unpack . fromRow

instance Row Word where
  fromRow = fmap (fromIntegral :: Word64 -> Word) . fromRow

instance Row Int where
  fromRow = fmap (fromIntegral :: Int64 -> Int) . fromRow
