{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hasql.CxRow (CxRow(..)) where

import Hasql.Prelude
import Language.Haskell.TH
import qualified Hasql.Backend as Bknd
import qualified Data.Vector as Vector
import qualified Hasql.TH as THUtil
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as TL
import GHC.Generics
import GHC.TypeLits

-- deja vu ...
type family Fields f where
  Fields (f :*: g)  = Fields f + Fields g
  Fields (M1 i c f) = Fields f
  Fields (K1 i c)   = 1

-- |
-- This class is intended to be used via the provided tuple instances or
-- derived instances via "GHC.Generics".
class CxRow c r where
  parseRow :: Bknd.ResultRow c -> Either Text r

  default parseRow
    :: (Generic r, GCxRow c (Rep r), KnownNat (Fields (Rep r)))
    => Bknd.ResultRow c -> Either Text r
  parseRow row =
    if elen == Vector.length row
    then to . snd <$> gparseRow 0 row
    else Left . TL.toStrict . TB.toLazyText $
         "Inappropriate row length: " <> TB.decimal (Vector.length row) <>
         ", expecting: " <> TB.decimal elen <> " instead."
   where
    elen :: Int
    elen = fromInteger (natVal (Proxy :: Proxy (Fields (Rep r))))

instance CxRow c () where
  parseRow row = 
    if Vector.null row
      then Right ()
      else Left "Not an empty row"

instance Bknd.CxValue c v => CxRow c (Identity v) where
  parseRow row = do
    Identity <$> Bknd.decodeValue (Vector.unsafeHead row)

class GCxRow c f where
  gparseRow :: Int -> Bknd.ResultRow c -> Either Text (Int, f a)

instance (GCxRow c f, GCxRow c g) => GCxRow c (f :*: g) where
  gparseRow pos row = do
    (pos'  , l) <- gparseRow pos  row
    (pos'' , r) <- gparseRow pos' row
    return (pos'', l :*: r)

instance Bknd.CxValue c a => GCxRow c (K1 i a) where
  gparseRow pos row = do
    v <- Bknd.decodeValue (Vector.unsafeIndex row pos)
    Right (pos+1, K1 v)

instance GCxRow c p => GCxRow c (M1 i a p) where
  gparseRow pos row = second M1 <$> gparseRow pos row

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
      map (\t -> THUtil.classP ''Bknd.CxValue [connectionType, t]) varTypes
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

