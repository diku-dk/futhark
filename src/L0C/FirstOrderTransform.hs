-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive
-- - it's certainly worth considering when we can express such
-- transformations in-place.  This module should be run very late in
-- the compilation pipeline, ideally just before the code generator.
module L0C.FirstOrderTransform
  ( transformProg )
  where

import Control.Applicative
import Control.Monad.State

import Data.Loc

import L0C.L0
import L0C.FreshNames

type TransformM = State VNameSource

-- | Return a new, fresh name, with the given string being part of the
-- name.
new :: String -> TransformM VName
new = state . newVName

-- | Perform the first-order transformation on an L0 program.
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runTransformM $ mapM transformFunDec $ progFunctions prog
  where runTransformM m = evalState m $ newNameSourceForProg prog

transformFunDec :: FunDec -> TransformM FunDec
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- transformExp body
  return (fname, rettype, params, body', loc)

-- We sometimes have to allocate new arrays up front.  This is a bit
-- tricky, as in case the new array is multidimensional, we need to
-- compute the first element in order to get the proper size.  For
-- example, in the case for map, we evaluate the function on the first
-- element of the array and use that to construct the array for the
-- rest.  If the input array is empty, we simply return an empty
-- output array.
transformExp :: Exp -> TransformM Exp

transformExp mape@(Map fun e intype loc) =
  newLet "inarr" e $ \inarr inarrv -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    newLet "n" (Size 0 inarrv loc) $ \_ nv -> do
      let zero = Literal (IntVal 0) loc
          index0 = Index inarr [zero] intype loc
          indexi = Index inarr [iv] intype loc
      funcall0 <- transformLambda fun [index0]
      funcall <- transformLambda fun [indexi]
      newLet "outarr" (Replicate nv funcall0 loc) $ \outarr outarrv -> do
        let branch = If (BinOp Less zero nv (Elem Bool) loc) letbody
                     (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
                     loc
            letbody = DoLoop (Id outarr) outarrv i nv loopbody outarrv loc
            loopbody = LetWith outarr outarr [iv] funcall outarrv loc
        return branch

transformExp (Reduce fun accexp arrexp intype loc) =
  newReduction loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index arr [iv] intype loc
    funcall <- transformLambda fun [accv, indexi]
    let loop = DoLoop (Id acc) accv i (Size 0 arrv loc) loopbody accv loc
        loopbody = LetPat (Id acc) funcall accv loc
    return loop

transformExp (Scan fun accexp arrexp intype loc) =
  newReduction loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index arr [iv] intype loc
    funcall <- transformLambda fun [accv, indexi]
    let loop = DoLoop (TupId [Id acc, Id arr] loc)
               (TupLit [accv, arrv] loc) i (Size 0 arrv loc) loopbody arrv loc
        loopbody = LetWith arr arr [iv] funcall (TupLit [indexi, arrv] loc) loc
    return loop

transformExp (Filter fun arrexp rowtype loc) =
  newLet "arr" arrexp $ \arr arrv ->
    newLet "n" (Size 0 arrv loc) $ \_ nv -> do
      let checkempty nonempty = If (BinOp Equal nv (intval 0) (Elem Bool) loc)
                                (Literal (emptyArray rowtype) loc) nonempty
                                loc
      (x, xv) <- newVar loc "x" rowtype
      (i, iv) <- newVar loc "i" (Elem Int)
      fun' <- transformLambda fun [xv]
      let branch = If fun' (intval 1) (intval 0) loc
          indexin0 = Index arr [intval 0] rowtype loc
          indexin = Index arr [iv] rowtype loc
      mape <- transformExp $
              Map (AnonymFun [toParam x] branch (Elem Int) loc) arrv rowtype loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ AnonymFun [toParam a, toParam b]
                 (BinOp Plus av bv (Elem Int) loc) (Elem Int) loc
      scan <- transformExp $ Scan plus (intval 0) mape (Elem Int) loc
      newLet "ia" scan $ \ia _ -> do
        let indexia ind = Index ia [ind] (Elem Int) loc
            indexiaend = indexia (BinOp Minus nv (intval 1) (Elem Int) loc)
            indexi = indexia iv
            indexim1 = indexia (BinOp Minus iv (intval 1) (Elem Int) loc)
        newLet "res" (Replicate indexiaend indexin0 loc) $ \res resv -> do
          let loop = DoLoop (Id res) resv i nv loopbody resv loc
              loopbody = If (Or (BinOp Equal iv (intval 0) (Elem Bool) loc)
                                (And (BinOp Less (intval 0) iv (Elem Bool) loc)
                                     (BinOp Equal indexi indexim1 (Elem Bool) loc) loc)
                             loc)
                         resv update loc
              update = LetWith res res [BinOp Minus indexi (intval 1) (Elem Int) loc]
                       indexin resv loc
          return $ checkempty loop
        where intval x = Literal (IntVal x) loc

transformExp (Redomap redfun mapfun accexp arrexp _ loc) =
  newReduction loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index arr [iv] (stripArray 1 $ typeOf arrexp) loc
    mapfuncall <- transformLambda mapfun [indexi]
    redfuncall <- transformLambda redfun [accv, mapfuncall]
    let loop = DoLoop (Id acc) accv i (Size 0 arrv loc) loopbody accv loc
        loopbody = LetWith acc acc [] redfuncall accv loc
    return loop

transformExp mape@(Map2 fun arrs _ loc) = do
  let zero = Literal (IntVal 0) loc
  newLets "inarr" arrs $ \inarrs _ -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    newLet "n" (size inarrs) $ \_ nv -> do
      funcall0 <- tuple <$> transformLambda fun (index inarrs zero)
      funcall <- tuple <$> transformLambda fun (index inarrs iv)
      newResultArray nv funcall0 $ \outarr outarrv -> do
        loopbody <- letwith outarr iv funcall outarrv
        let branch = If (BinOp Less zero nv (Elem Bool) loc)
                     letbody
                     (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
                     loc
            letbody = DoLoop (TupId (map Id outarr) loc) outarrv i nv loopbody
                      outarrv loc
        return branch

transformExp (Reduce2 fun accexp arrexps _ loc) =
  newReduction2 loc arrexps accexp $ \(arr, _) (acc, accv) (i, iv) -> do
    funcall <- transformLambda fun (map Var acc ++ index arr iv)
    return $ DoLoop (TupId (map Id acc) loc) accv i (size arr)
             funcall accv loc

transformExp (Scan2 fun accexp arrexps _ loc) =
  newReduction2 loc arrexps accexp $ \(arr, arrv) (acc, _) (i, iv) -> do
    funcall <- transformLambda fun $ map Var acc ++ index arr iv
    loopbody <- letwith arr iv funcall $
                TupLit (index arr iv++map Var arr) loc
    let loop = DoLoop (TupId (map Id $ acc ++ arr) loc)
               (TupLit (map Var $ acc ++ arr) loc)
               i (size arr) loopbody arrv loc
    return loop

transformExp filtere@(Filter2 fun arrexps loc) =
  newLets "arr" arrexps $ \arr _ ->
    newLet "n" (size arr) $ \_ nv -> do
      let checkempty nonempty =
            If (BinOp Equal nv (intval 0) (Elem Bool) loc)
            (Literal (blankValue $ typeOf filtere) loc) nonempty
            loc
          rowtypes = map (rowType . identType) arr
      (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
      (i, iv) <- newVar loc "i" $ Elem Int
      fun' <- transformLambda fun $ map Var xs
      let branch = If fun' (intval 1) (intval 0) loc
          indexin0 = index arr $ intval 0
          indexin = index arr iv
      mape <- transformExp $
              Map2 (AnonymFun (map toParam xs) branch (Elem Int) loc) (map Var arr)
              rowtypes loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ AnonymFun [toParam a, toParam b] (BinOp Plus av bv (Elem Int) loc) (Elem Int) loc
      scan <- newTupLet "mape" mape $ \_ mape' ->
                transformExp $ Scan2 plus [intval 0] [mape'] [Elem Int] loc
      newTupLet "ia" scan $ \ia _ -> do
        let indexia ind = Index ia [ind] (Elem Int) loc
            indexiaend = indexia (sub1 nv)
            indexi = indexia iv
            indexim1 = indexia (sub1 iv)
        newResultArray indexiaend (TupLit indexin0 loc) $ \res resv -> do
          update <- letwith res (sub1 indexi) (TupLit indexin loc) resv
          let loop = DoLoop (TupId (map Id res) loc) resv i nv loopbody resv loc
              loopbody = If (Or (BinOp Equal iv (intval 0) (Elem Bool) loc)
                                (And (BinOp Less (intval 0) iv (Elem Bool) loc)
                                     (BinOp Equal indexi indexim1 (Elem Bool) loc) loc)
                             loc)
                         resv update loc
          return $ checkempty loop
  where intval x = Literal (IntVal x) loc
        sub1 e = BinOp Minus e (intval 1) (Elem Int) loc

transformExp (Redomap2 redfun mapfun accexps arrexps _ loc) =
  newReduction2 loc arrexps accexps $ \(arr, _) (acc, accv) (i, iv) -> do
    mapfuncall <- transformLambda mapfun $ index arr iv
    let loop loopbody = DoLoop (TupId (map Id acc) loc) accv i (size arr) loopbody accv loc
    case typeOf mapfuncall of
      Elem (Tuple ts) -> do
        names <- mapM (liftM fst . newVar loc "mapres") ts
        redfuncall <- transformLambda redfun $ map Var $ acc ++ names
        return $ loop $ LetPat (TupId (map Id names) loc) mapfuncall redfuncall loc
      t -> do
        name <- fst <$> newVar loc "mapres" t
        redfuncall <- transformLambda redfun $ map Var $ acc ++ [name]
        return $ loop $ LetPat (Id name) mapfuncall redfuncall loc

transformExp e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = transformExp
                    }

newReduction :: SrcLoc -> Exp -> Exp
             -> ((Ident, Exp) -> (Ident, Exp) -> (Ident, Exp) -> TransformM Exp)
             -> TransformM Exp
newReduction loc arrexp accexp body =
  newLet "arr" arrexp $ \arr arrv ->
    newLet "acc" accexp $ \acc accv -> do
      (i, iv) <- newVar loc "i" (Elem Int)
      body (arr, arrv) (acc, accv) (i, iv)

newReduction2 :: SrcLoc -> [Exp] -> [Exp]
             -> (([Ident], Exp) -> ([Ident], Exp) -> (Ident, Exp) -> TransformM Exp)
             -> TransformM Exp
newReduction2 loc arrexps accexps body = do
  (i, iv) <- newVar loc "i" (Elem Int)
  newLets "arr" arrexps $ \arr arrv -> do
    let ets = map typeOf accexps
    (names, namevs) <- unzip <$> mapM (newVar loc "acc") ets
    let binder inner = LetPat (TupId (map Id names) loc) (TupLit accexps loc) inner loc
    binder <$> body (arr, arrv) (names, TupLit namevs loc) (i, iv)

newLet :: String -> Exp -> (Ident -> Exp -> TransformM Exp)
       -> TransformM Exp
newLet name e body = do
  e' <- liftM maybeCopy $ transformExp e
  (x,xv) <- newVar loc name (typeOf e')
  let xlet inner = LetPat (Id x) e' inner loc
  xlet <$> body x xv
  where loc = srclocOf e

newTupLet :: String -> Exp -> (Ident -> Exp -> TransformM Exp)
       -> TransformM Exp
newTupLet name e body = do
  e' <- liftM maybeCopy $ transformExp e
  case typeOf e' of
    Elem (Tuple [t]) -> do
      (x,xv) <- newVar loc name t
      let xlet inner = LetPat (TupId [Id x] loc) e' inner loc
      xlet <$> body x xv
        where loc = srclocOf e
    _ -> newLet name e body

newLets :: String -> [Exp] -> ([Ident] -> Exp -> TransformM Exp)
        -> TransformM Exp
newLets k es body = newLets' es []
  where newLets' [] names       =
          body (reverse names) $ TupLit (map Var $ reverse names) noLoc
        newLets' (e:rest) names =
          newLet k e $ \name _ -> newLets' rest (name:names)

newVar :: SrcLoc -> String -> Type -> TransformM (Ident, Exp)
newVar loc name tp = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: Exp -> Exp
maybeCopy e
  | unique (typeOf e) || basicType (typeOf e)  = e
  | otherwise = Copy e $ srclocOf e

index :: [Ident] -> Exp -> [Exp]
index arrs i = flip map arrs $ \arr ->
               Index arr [i] (stripArray 1 $ identType arr) $ srclocOf i

newResultArray :: Exp -> Exp -> ([Ident] -> Exp -> TransformM Exp) -> TransformM Exp
newResultArray sizeexp valueexp body =
  case typeOf valueexp of
    Elem (Tuple ets) -> do
      (names, namevs) <- unzip <$> mapM (newVar loc "outarr_v") ets
      let bnd inner = LetPat (TupId (map Id names) loc ) valueexp inner loc
          arrexp = [ maybeCopy $ Replicate sizeexp namev loc
                     | namev <- namevs ]
      bnd <$> newLets "outarr" arrexp body
    _ -> do let rep = maybeCopy $ Replicate sizeexp valueexp loc
            (name, namev) <- newVar loc "outarr" $ typeOf rep
            let bnd inner = LetPat (Id name) rep inner loc
            bnd <$> body [name] namev
  where loc = srclocOf valueexp

letwith :: [Ident] -> Exp -> Exp -> Exp -> TransformM Exp
letwith ks i v body =
  case typeOf v of
    Elem (Tuple ets) -> do
      names <- mapM (liftM fst . newVar loc "tup") ets
      let comb inner (k, name) = LetWith k k [i] (Var name) inner loc
      return $ LetPat (TupId (map Id names) loc) v
               (foldl comb body $ zip ks names) loc
    _ -> do let comb inner k = LetWith k k [i] v inner loc
            return $ foldl comb body ks
  where loc = srclocOf body

tuple :: Exp -> Exp
tuple e = case typeOf e of Elem (Tuple _) -> e
                           _              -> TupLit [e] $ srclocOf e

size :: [Ident] -> Exp
size [] = Literal (IntVal 0) noLoc
size (k:_) = Size 0 (Var k) $ srclocOf k

transformLambda :: Lambda -> [Exp] -> TransformM Exp
transformLambda (AnonymFun params body _ loc) args = do
  body' <- transformExp body
  return $ foldl bind body' $ zip (map fromParam params) args
  where bind e (Ident pname ptype _, arg) = LetPat (Id $ Ident pname ptype loc) arg e loc
transformLambda (CurryFun fname curryargs rettype loc) args = do
  curryargs' <- mapM transformExp curryargs
  return $ Apply fname [(arg, Observe) | arg <- curryargs'++args] rettype loc
