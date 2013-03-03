{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.GenPatterns (  
                         foldlPattern
                       , buildExpPattern 
                    )
  where

import L0.AbSyn
 
 
------------------------------------------
------------- foldlPattern ---------------
------------------------------------------
foldlPattern :: TypeBox tf =>   (a -> Exp tf    -> a) -> 
                                (a -> Lambda tf -> a) -> 
                                a -> Exp tf -> a
foldlPattern _ _ ne (Read _ _)    = ne
foldlPattern _ _ ne (Literal _)   = ne
foldlPattern _ _ ne (Var     _)   = ne
foldlPattern f _ ne (Write e _ _) = f ne e
foldlPattern _ _ ne (New _ _)    = ne
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

foldlPattern f _ ne (LetWith _ e inds el body _) = foldl f ne (e : el : body : inds)
foldlPattern f _ ne (LetPat  _ e body _)   = foldl f ne [e, body]
foldlPattern f _ ne (DoLoop _ n body _ _)  = foldl f ne [n, body]
foldlPattern f _ ne (Index _ inds _ _ _ )  = foldl f ne inds

foldlPattern f _ ne (Iota      e _    )    = f ne e
foldlPattern f _ ne (Size      e _    )    = f ne e
foldlPattern f _ ne (Transpose e _ _ _)    = f ne e
foldlPattern f _ ne (Unzip     e   _ _)    = f ne e
foldlPattern f _ ne (Zip exptps _)         = foldl f ne (fst (unzip exptps))
foldlPattern f _ ne (Replicate e1 e2 _)  = foldl f ne [e1, e2]
foldlPattern f _ ne (Reshape es e _ _ _)   = foldl f ne (e:es)
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

getLambdaExps :: TypeBox tf => Lambda tf -> [Exp tf]
getLambdaExps (AnonymFun _ body   _ _) = [body]
getLambdaExps (CurryFun  _ params _ _) = params

-----------------------------------------------
------------- buildExpPattern   ---------------
-----------------------------------------------
buildExpPattern :: TypeBox tf => (Exp tf -> Exp tf) -> Exp tf -> Exp tf

buildExpPattern _ e@(Read  _ _)     = e
buildExpPattern _ e@(New  _ _)      = e
buildExpPattern _ e@(Literal _)     = e
buildExpPattern _ e@(Var     _)     = e
buildExpPattern f (Write  e tp pos) = Write  (f e) tp pos 
buildExpPattern f (Negate e tp pos) = Negate (f e) tp pos
buildExpPattern f (Not    e    pos) = Not    (f e)    pos
buildExpPattern f (Copy   e    pos) = Copy   (f e)    pos

buildExpPattern f (TupLit       els      pos) = TupLit    (map f els)      pos
buildExpPattern f (ArrayLit     els   tp pos) = ArrayLit  (map f els)   tp pos
buildExpPattern f (Apply     nm els   tp pos) = Apply nm  (map f els)   tp pos
buildExpPattern f (BinOp    bop e1 e2 tp pos) = BinOp bop (f e1) (f e2) tp pos
buildExpPattern f (And          e1 e2    pos) = And       (f e1) (f e2)    pos
buildExpPattern f (Or           e1 e2    pos) = Or        (f e1) (f e2)    pos
buildExpPattern f (If        e1 e2 e3 tp pos) = If (f e1) (f e2) (f e3) tp pos

buildExpPattern f (LetWith ii e inds el body pos) = LetWith ii (f e) (map f inds) (f el) (f body) pos
buildExpPattern f (LetPat  ii e         body pos) = LetPat  ii (f e) (f body) pos
buildExpPattern f (DoLoop  i  n body merges  pos) = DoLoop  i  (f n) (f body) merges  pos
buildExpPattern f (Index nm inds tp1 tp2     pos) = Index nm (map f inds) tp1 tp2     pos 

buildExpPattern f (Iota      e          pos)  = Iota (f e) pos
buildExpPattern f (Size      e          pos)  = Size (f e) pos
buildExpPattern f (Transpose e  tp1 tp2 pos)  = Transpose (f e) tp1 tp2 pos
buildExpPattern f (Unzip     e      tps pos)  = Unzip (f e) tps pos
buildExpPattern f (Zip       exptps     pos)  = 
    let (exps,tps) = unzip exptps 
        exptps'    = zip (map f exps) tps
    in  Zip exptps' pos
buildExpPattern f (Replicate e1 e2      pos)  = Replicate (f e1) (f e2) pos
buildExpPattern f (Reshape es e tp1 tp2 pos)  = Reshape (map f es) (f e) tp1 tp2 pos
buildExpPattern f (Map    lam e tp1 tp2 pos)  = Map    (buildLambda f lam) (f e) tp1 tp2 pos
buildExpPattern f (Mapall lam e tp1 tp2 pos)  = Mapall (buildLambda f lam) (f e) tp1 tp2 pos
buildExpPattern f (Reduce lam e1 e2 tp  pos)  = Reduce (buildLambda f lam) (f e1) (f e2) tp pos
buildExpPattern f (Scan   lam e1 e2 tp  pos)  = Scan   (buildLambda f lam) (f e1) (f e2) tp pos
buildExpPattern f (Filter lam e     tp  pos)  = Filter (buildLambda f lam) (f e) tp pos
buildExpPattern f (Split      e1 e2 tp  pos)  = Split  (f e1) (f e2) tp pos
buildExpPattern f (Concat     e1 e2 tp  pos)  = Concat (f e1) (f e2) tp pos

buildExpPattern f (Redomap lam1 lam2 e1 e2 tp1 tp2 pos) = 
    let (lam1',lam2') = (buildLambda f lam1, buildLambda f lam2)  
    in  Redomap lam1' lam2' (f e1) (f e2) tp1 tp2 pos 

buildLambda :: TypeBox tf => (Exp tf -> Exp tf) -> Lambda tf -> Lambda tf
buildLambda f (AnonymFun tps body  tp pos) = AnonymFun tps     (f body  ) tp pos
buildLambda f (CurryFun  nm params tp pos) = CurryFun  nm  (map f params) tp pos
