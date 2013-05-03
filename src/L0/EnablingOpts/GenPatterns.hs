{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.GenPatterns (  
                         foldlPattern
                       , buildExpPattern 
                    )
  where

import Data.Data
import Data.Generics

import L0.AbSyn
 
 
------------------------------------------
------------- foldlPattern ---------------
------------------------------------------
foldlPattern :: TypeBox tf =>   (a -> Exp tf    -> a) -> 
                                (a -> Lambda tf -> a) -> 
                                a -> Exp tf -> a
foldlPattern _ _ ne (Literal _)   = ne
foldlPattern _ _ ne (Var     _)   = ne
foldlPattern f _ ne (Negate e _ _)= f ne e
foldlPattern f _ ne (Not    e _  )= f ne e
foldlPattern f _ ne (Copy   e _  )= f ne e

foldlPattern f _ ne (TupLit els _)     = foldl f ne els
foldlPattern f _ ne (ArrayLit els _ _) = foldl f ne els
foldlPattern f _ ne (BinOp _ e1 e2 _ _)= foldl f ne [e1, e2]
foldlPattern f _ ne (And e1 e2 _)      = foldl f ne [e1, e2]
foldlPattern f _ ne (Or  e1 e2 _)      = foldl f ne [e1, e2]
foldlPattern f _ ne (If e1 e2 e3 _ _)  = foldl f ne [e1, e2, e3]
foldlPattern f _ ne (Apply _ es _ _)   = foldl f ne es

foldlPattern f _ ne (DoLoop _ mergeexp _ n lbody letexp _)  = 
    foldl f ne (mergeexp : (n : lbody : letexp : []))
foldlPattern f _ ne (LetWith _ _ inds el body _) = foldl f ne (el : body : inds)
foldlPattern f _ ne (LetPat  _ e body _)   = foldl f ne [e, body]
foldlPattern f _ ne (Index _ inds _ _ _ )  = foldl f ne inds

foldlPattern f _ ne (Iota      e _    )    = f ne e
foldlPattern f _ ne (Size      e _    )    = f ne e
foldlPattern f _ ne (Transpose e _    )    = f ne e
foldlPattern f _ ne (Unzip     e   _ _)    = f ne e
foldlPattern f _ ne (Zip exptps _)         = foldl f ne (fst (unzip exptps))
foldlPattern f _ ne (Replicate e1 e2 _)  = foldl f ne [e1, e2]
foldlPattern f _ ne (Reshape es e _)       = foldl f ne (e:es)
foldlPattern f doLam ne (Map lam e _ _ _)      = foldl f (doLam ne lam) (e:getLambdaExps lam)
foldlPattern f doLam ne (Mapall lam e _ _ _)   = foldl f (doLam ne lam) (e:getLambdaExps lam)
foldlPattern f doLam ne (Reduce lam e1 e2 _ _) = foldl f (doLam ne lam) (e1:e2:getLambdaExps lam)
foldlPattern f doLam ne (Scan lam e1 e2 _ _)   = foldl f (doLam ne lam) (e1:e2:getLambdaExps lam)
foldlPattern f doLam ne (Filter lam e _ _)     = foldl f (doLam ne lam) (e:getLambdaExps lam)
foldlPattern f doLam ne (Redomap lam1 lam2 e1 e2 _ _ _) = 
    foldl   f (doLam (doLam ne lam1) lam2)  
            (e1:e2: getLambdaExps lam1 ++ getLambdaExps lam2 )
foldlPattern f _ ne (Split e1 e2 _ _)      = foldl f ne [e1, e2]
foldlPattern f _ ne (Concat e1 e2 _ _)     = foldl f ne [e1, e2]
-- soac2 implem (Cosmin) --
foldlPattern f doLam ne (Map2 lam e _ _ _)      = foldl f (doLam ne lam) (e++getLambdaExps lam)
foldlPattern f doLam ne (Mapall2 lam e _ _ _)   = foldl f (doLam ne lam) (e++getLambdaExps lam)
foldlPattern f doLam ne (Reduce2 lam e1 e2 _ _) = foldl f (doLam ne lam) (e1:e2++getLambdaExps lam)
foldlPattern f doLam ne (Scan2 lam e1 e2 _ _)   = foldl f (doLam ne lam) (e1:e2++getLambdaExps lam)
foldlPattern f doLam ne (Filter2 lam e _ _)     = foldl f (doLam ne lam) (e++getLambdaExps lam)
foldlPattern f doLam ne (Redomap2 lam1 lam2 e1 e2 _ _ _) = 
    foldl   f (doLam (doLam ne lam1) lam2)  
            (e1 : e2 ++ getLambdaExps lam1 ++ getLambdaExps lam2 )

 
getLambdaExps :: TypeBox tf => Lambda tf -> [Exp tf]
getLambdaExps (AnonymFun _ body   _ _) = [body]
getLambdaExps (CurryFun  _ params _ _) = params

-----------------------------------------------
------------- buildExpPattern   ---------------
-----------------------------------------------
buildExpPattern :: TypeBox tf => (Exp tf -> Exp tf) -> Exp tf -> Exp tf
buildExpPattern f e = gmapT (mkT f
                             `extT` buildLambda f
                             `extT` map f
                             `extT` map (buildExpPair f)) e

buildExpPair :: TypeBox tf => (Exp tf -> Exp tf) -> (Exp tf, tf) -> (Exp tf, tf)
buildExpPair f (e,t) = (f e,t)

buildLambda :: TypeBox tf => (Exp tf -> Exp tf) -> Lambda tf -> Lambda tf
buildLambda f (AnonymFun tps body  tp pos) = AnonymFun tps     (f body  ) tp pos
buildLambda f (CurryFun  nm params tp pos) = CurryFun  nm  (map f params) tp pos
