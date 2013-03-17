{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Default.TH (deriveDefault) where

import Control.Applicative
import Data.Default
import Data.List
import Language.Haskell.TH

createInstance :: Name -> [Name] -> Name -> [Type] -> Q Dec
createInstance typeConstructorName typeVariables constructorName constructorArgumentTypes = do
	reqs <- constraints typeConstructorName constructorArgumentTypes
	return $ InstanceD reqs
		(AppT (ConT ''Default) (foldl' (\x y -> AppT x (VarT y)) (ConT typeConstructorName) typeVariables))
		[FunD 'def [Clause [] (NormalB (foldl' (\x _ -> AppE x (VarE 'def)) (ConE constructorName) constructorArgumentTypes)) []]]

constraints :: Name -> [Type] -> Q [Pred]
constraints tcn ts = nub . concat <$> mapM (constraint tcn) ts

constraint :: Name -> Type -> Q [Pred]
constraint tcn t@(VarT n)       = return [ClassP ''Default [t]]
constraint tcn t@(ConT n)       = return [ClassP ''Default [t]]
#if MIN_VERSION_template_haskell(2,8,0)
constraint tcn   (SigT t StarT) = constraint tcn t
#else
constraint tcn   (SigT t StarK) = constraint tcn t
#endif
constraint tcn t@(AppT _ _)     = case normalize t of
	(ArrowT, [_, r])        -> constraint tcn r
	(ListT, [t])            -> return []
	(TupleT n, ts)          -> constraints tcn ts
	(ConT n, ts) | n == tcn -> return [] -- don't demand a Default instance while building the Default instance
	_ -> return [ClassP ''Default [t]]
constraint tcn t = fail $ "I got surprised by the type " ++ pprint t ++ " as a constructor argument while trying to derive a Default instance for " ++ pprint tcn

normalize = normalize' [] where
	normalize' acc (AppT t t') = normalize' (t':acc) t
	normalize' acc t = (t, reverse acc)

instanceQ :: Name -> [TyVarBndr] -> Name -> [Type] -> Q [Dec]
instanceQ t vs c as = return <$> createInstance t (map name vs) c as

name :: TyVarBndr -> Name
name (PlainTV n) = n
name (KindedTV n k) = n

deriveDefault :: Name -> Q [Dec]
deriveDefault n = do
	info <- reify n
	case info of
		TyConI (DataD _ qn tvars (con:_) _) -> case con of
			NormalC  conName ts -> instanceQ qn tvars conName (map snd ts)
			RecC     conName ts -> instanceQ qn tvars conName (map (\(v,s,t) -> t) ts)
			InfixC t conName t' -> instanceQ qn tvars conName (map snd [t, t'])
			_ -> fail $ "Dunno how to derive Default instances for existential types"
		TyConI (DataD _ _ _ [] _) -> fail $ "Really? You want to derive a Default instance for an uninhabited type?"
		_ -> fail $ "Couldn't derive a Default instance; didn't know what to do with " ++ pprint info
