module L0.FirstOrderTransform
  ( transformProg )
  where

import Control.Monad.State

import Data.Generics
import Data.Loc

import L0.AbSyn
import L0.FreshNames

type TransformM = State NameSource

-- | Return a new, fresh name, with the given string being part of the
-- name.
new :: String -> TransformM String
new = state . newName

transformProg :: Prog Type -> Prog Type
transformProg prog = runTransformM $ mapM transformFunDec prog
  where runTransformM m = evalState m $ newNameSourceForProg prog

transformFunDec :: FunDec Type -> TransformM (FunDec Type)
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- everywhereM (mkM transformExp) body
  return (fname, rettype, params, body', loc)

transformExp :: Exp Type -> TransformM (Exp Type)
transformExp (Map fun e intype outtype loc)
  | intype == outtype = do
  (i, iv) <- newVar "i" (Int loc) loc
  (arr, arrv, arrlet) <- newLet e "arr"
  let index = Index arr [iv] intype intype loc
  funcall <- transformLambda fun [index]
  let letbody = DoLoop i (Size arrv loc) loopbody [arr] loc
      loopbody = LetWith arr arr [iv] funcall arrv loc
  return $ arrlet letbody
  | otherwise = do
  -- We have to allocate a new array up front, as for-loops cannot
  -- change the type of the array.
  (inarr, inarrv, inarrlet) <- newLet e "inarr"
  (i, iv) <- newVar "i" (Int loc) loc
  (_, nv, nlet) <- newLet (Size inarrv loc) "n"
  let outarrt = Array outtype Nothing loc
      zero = Literal $ IntVal 0 loc
      index0 = Index inarr [zero] intype intype loc
      index = Index inarr [iv] intype intype loc
  funcall0 <- transformLambda fun [index0]
  funcall <- transformLambda fun [index]
  (outarr, outarrv, outarrlet) <- newLet (Replicate nv funcall0 loc) "outarr"
  let branch = If (BinOp Less zero nv (Bool loc) loc)
               (outarrlet letbody)
               (Literal (arrayVal [] outtype loc)) outarrt loc
      letbody = DoLoop i nv loopbody [outarr] loc
      loopbody = LetWith outarr outarr [iv] funcall outarrv loc
  return $ inarrlet $ nlet branch
transformExp (Reduce fun accexp arrexp intype loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype intype loc
  funcall <- transformLambda fun [accv, index]
  let loop = DoLoop i (Size arrv loc) loopbody [acc] loc
      loopbody = LetWith acc acc [] funcall accv loc
  return $ redlet loop
transformExp (Scan fun accexp arrexp intype loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype intype loc
  funcall <- transformLambda fun [accv, index]
  let looplet = LetPat (TupId [Id acc, Id arr] loc)
                loop arrv loc
      loop = DoLoop i (Size arrv loc) loopbody [acc, arr] loc
      loopbody = LetWith arr arr [iv] funcall (TupLit [index, arrv] loc) loc
  return $ redlet looplet
transformExp (Filter fun arrexp elty loc) = do
  (arr, arrv, arrlet) <- newLet arrexp "arr"
  (_, nv, nlet) <- newLet (Size arrv loc) "n"
  let checkempty nonempty = If (BinOp Equal nv (intval 0) bool loc)
                            (Literal $ emptyArray elty loc) nonempty
                            (expType arrexp) loc
  (x, xv) <- newVar "x" elty loc
  (i, iv) <- newVar "i" int loc
  fun' <- transformLambda fun [xv]
  let branch = If fun' (intval 1) (intval 0) int loc
      indexin0 = Index arr [intval 0] elty elty loc
      indexin = Index arr [iv] elty elty loc
  mape <- transformExp $ Map (AnonymFun [x] branch int loc) arrv elty int loc
  plus <- do
    (a,av) <- newVar "a" int loc
    (b,bv) <- newVar "b" int loc
    return $ AnonymFun [a, b] (BinOp Plus av bv int loc) int loc
  scan <- transformExp $ Scan plus (intval 0) mape int loc
  (ia, _, ialet) <- newLet scan "ia"
  let indexia ind = Index ia [ind] int int loc
      indexiaend = indexia (BinOp Minus nv (intval 1) int loc)
      indexi = indexia iv
      indexim1 = indexia (BinOp Minus iv (intval 1) int loc)
  (res, resv, reslet) <- newLet (Replicate indexiaend indexin0 loc) "res"
  let loop = DoLoop i nv loopbody [res] loc
      loopbody = If (Or (BinOp Equal iv (intval 0) bool loc)
                        (And (BinOp Less (intval 0) iv bool loc)
                             (BinOp Equal indexi indexim1 bool loc) loc)
                     loc)
                 resv update (expType arrexp) loc
      update = LetWith res res [BinOp Minus indexi (intval 1) int loc] indexin resv loc
  return $ arrlet $ nlet $ checkempty $ ialet $ reslet loop
  where int = Int loc
        bool = Bool loc
        intval x = Literal (IntVal x loc)
transformExp (Mapall fun arrexp _ outtype loc) = transformExp =<< toMap arrexp
  where toMap e = case expType e of
                    Array et _ _ -> do
                      (x,xv) <- newVar "x" et loc
                      body <- toMap xv
                      let ot = arrayType (arrayDims et) outtype
                      return $ Map (AnonymFun [x] body ot loc) e et ot loc
                    _ -> transformLambda fun [e]
transformExp (Redomap redfun mapfun accexp arrexp intype _ loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype intype loc
  mapfuncall <- transformLambda mapfun [index]
  redfuncall <- transformLambda redfun [accv, mapfuncall]
  let loop = DoLoop i (Size arrv loc) loopbody [acc, arr] loc
      loopbody = LetWith arr arr [iv] redfuncall (TupLit [accv, arrv] loc) loc
  return $ redlet loop
transformExp e = return e

newReduction :: SrcLoc -> Exp Type -> Exp Type
             -> TransformM ((Ident Type, Exp Type),
                            (Ident Type, Exp Type),
                            (Ident Type, Exp Type),
                            Exp Type -> Exp Type)
newReduction loc arrexp accexp = do
  (arr, arrv, arrlet) <- newLet arrexp "arr"
  (acc, accv, acclet) <- newLet accexp "acc"
  (i, iv) <- newVar "i" (Int loc) loc
  return ((arr, arrv), (acc, accv), (i, iv), acclet . arrlet)

newLet :: Exp Type -> String -> TransformM (Ident Type, Exp Type, Exp Type -> Exp Type)
newLet e name = do
  (x,xv) <- newVar name (expType e) loc
  let xlet body = LetPat (Id x) e body loc
  return (x, xv, xlet)
  where loc = srclocOf e

newVar :: String -> Type -> SrcLoc -> TransformM (Ident Type, Exp Type)
newVar name tp loc = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)

transformLambda :: Lambda Type -> [Exp Type] -> TransformM (Exp Type)
transformLambda (AnonymFun params body _ loc) args = do
  body' <- transformExp body
  return $ foldl bind body' $ zip params args
  where bind e (Ident pname ptype _, arg) = LetPat (Id $ Ident pname ptype loc) arg e loc
transformLambda (CurryFun fname curryargs rettype loc) args = do
  curryargs' <- mapM transformExp curryargs
  return $ Apply fname (curryargs'++args) rettype loc
