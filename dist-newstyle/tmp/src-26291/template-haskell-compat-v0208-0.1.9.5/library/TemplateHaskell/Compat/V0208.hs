{-# LANGUAGE CPP #-}

module TemplateHaskell.Compat.V0208 where

import Language.Haskell.TH hiding (conP)
import TemplateHaskell.Compat.V0208.Prelude

classP :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP n tl =
  foldl AppT (ConT n) tl
#else
classP =
  ClassP
#endif

instanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)  
instanceD =
  InstanceD Nothing
#else
instanceD =
  InstanceD
#endif

dataD :: Cxt -> Name -> [UnitTyVarBndr] -> [Con] -> [Name] -> Dec
#if MIN_VERSION_template_haskell(2,21,0)
dataD cxt name varBndrs cons derivingNames =
  DataD cxt name preparedVarBndrs Nothing cons (pure (DerivClause Nothing (map ConT derivingNames)))
  where
    preparedVarBndrs :: [TyVarBndr BndrVis]
    preparedVarBndrs =
      fmap (fmap (const BndrReq)) varBndrs
#elif MIN_VERSION_template_haskell(2,12,0)
dataD cxt name varBndrs cons derivingNames =
  DataD cxt name varBndrs Nothing cons (pure (DerivClause Nothing (map ConT derivingNames)))
#elif MIN_VERSION_template_haskell(2,11,0)
dataD cxt name varBndrs cons derivingNames =
  DataD cxt name varBndrs Nothing cons (map ConT derivingNames)
#else
dataD cxt name varBndrs cons derivingNames =
  DataD cxt name varBndrs cons derivingNames
#endif

notStrict :: Strict
#if MIN_VERSION_template_haskell(2,11,0)
notStrict =
  Bang NoSourceUnpackedness NoSourceStrictness
#else
notStrict =
  NotStrict
#endif

tupE :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
tupE = \ case
  [a] -> a
  a -> TupE (map Just a)
#else
tupE = TupE
#endif

flaglessPlainTV :: Name -> UnitTyVarBndr
#if MIN_VERSION_template_haskell(2,17,0)
flaglessPlainTV name = PlainTV name ()
#else
flaglessPlainTV = PlainTV
#endif

specifiedPlainTV :: Name -> SpecificityTyVarBndr
#if MIN_VERSION_template_haskell(2,17,0)
specifiedPlainTV = flip PlainTV SpecifiedSpec
#else
specifiedPlainTV = PlainTV
#endif

#if MIN_VERSION_template_haskell(2,17,0)
type SpecificityTyVarBndr = TyVarBndr Specificity
#else
type SpecificityTyVarBndr = TyVarBndr
#endif

#if MIN_VERSION_template_haskell(2,17,0)
type UnitTyVarBndr = TyVarBndr ()
#else
type UnitTyVarBndr = TyVarBndr
#endif

doE :: [Stmt] -> Exp
#if MIN_VERSION_template_haskell(2,17,0)
doE = DoE Nothing
#else
doE = DoE
#endif

#if MIN_VERSION_template_haskell(2,17,0)
tyVarBndrKind :: TyVarBndr flag -> Maybe Kind
tyVarBndrKind = \ case
  KindedTV _ _ a -> Just a
  _ -> Nothing
#else
tyVarBndrKind :: TyVarBndr -> Maybe Kind
tyVarBndrKind = \ case
  KindedTV _ a -> Just a
  _ -> Nothing
#endif

#if MIN_VERSION_template_haskell(2,18,0)
conP :: Name -> [Pat] -> Pat
conP name pats = ConP name [] pats
#else
conP :: Name -> [Pat] -> Pat
conP = ConP
#endif

{-# DEPRECATED conp "Use 'conP'" #-}
conp :: Name -> [Pat] -> Pat
conp = conP
