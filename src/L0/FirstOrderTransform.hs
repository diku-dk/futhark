module L0.FirstOrderTransform
  ( transformProg )
  where

import Control.Monad.State

import Data.Loc

import L0.AbSyn
import L0.Traversals
import L0.FreshNames

type TransformM = State NameSource

-- | Return a new, fresh name, with the given string being part of the
-- name.
new :: String -> TransformM Name
new = state . newName . nameFromString

transformProg :: Prog Type -> Prog Type
transformProg prog = runTransformM $ mapM transformFunDec prog
  where runTransformM m = evalState m $ newNameSourceForProg prog

transformFunDec :: FunDec Type -> TransformM (FunDec Type)
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- transformExp body
  return (fname, rettype, params, body', loc)

transformExp :: Exp Type -> TransformM (Exp Type)
transformExp mape@(Map fun e intype loc) = do
  -- We have to allocate a new array up front.  This is a bit tricky,
  -- as in case the new array is an array-of-arrays, we need to
  -- compute the first element in order to get the proper size.  We
  -- evaluate the function on the first element of the array and use
  -- that to construct the array for the rest.  If the input array is
  -- empty, we simply return an empty output array.
  (inarr, inarrv, inarrlet) <- newLet e "inarr"
  (i, iv) <- newVar "i" (Elem Int) loc
  (_, nv, nlet) <- newLet (Size inarrv loc) "n"
  let zero = Literal (IntVal 0) loc
      index0 = Index inarr [zero] intype loc
      index = Index inarr [iv] intype loc
  funcall0 <- transformLambda fun [index0]
  funcall <- transformLambda fun [index]
  (outarr, outarrv, outarrlet) <- newLet (Replicate nv funcall0 loc) "outarr"
  let branch = If (BinOp Less zero nv (Elem Bool) loc)
               (outarrlet letbody)
               (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
               (typeOf mape) loc
      letbody = DoLoop (Id outarr) outarrv i nv loopbody outarrv loc
      loopbody = LetWith outarr outarr [iv] funcall outarrv loc
  return $ inarrlet $ nlet branch
transformExp (Reduce fun accexp arrexp intype loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype loc
  funcall <- transformLambda fun [accv, index]
  let loop = DoLoop (Id acc) accv i (Size arrv loc) loopbody accv loc
      loopbody = LetPat (Id acc) funcall accv loc
  return $ redlet loop
transformExp (Scan fun accexp arrexp intype loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype loc
  funcall <- transformLambda fun [accv, index]
  let loop = DoLoop (TupId [Id acc, Id arr] loc) (TupLit [accv, arrv] loc) i (Size arrv loc) loopbody arrv loc
      loopbody = LetWith arr arr [iv] funcall (TupLit [index, arrv] loc) loc
  return $ redlet loop
transformExp (Filter fun arrexp elty loc) = do
  (arr, arrv, arrlet) <- newLet arrexp "arr"
  (_, nv, nlet) <- newLet (Size arrv loc) "n"
  let checkempty nonempty = If (BinOp Equal nv (intval 0) (Elem Bool) loc)
                            (Literal (emptyArray elty) loc) nonempty
                            (typeOf arrexp) loc
  (x, xv) <- newVar "x" elty loc
  (i, iv) <- newVar "i" (Elem Int) loc
  fun' <- transformLambda fun [xv]
  let branch = If fun' (intval 1) (intval 0) (Elem Int) loc
      indexin0 = Index arr [intval 0] elty loc
      indexin = Index arr [iv] elty loc
  mape <- transformExp $ Map (AnonymFun [x] branch (Elem Int) loc) arrv elty loc
  plus <- do
    (a,av) <- newVar "a" (Elem Int) loc
    (b,bv) <- newVar "b" (Elem Int) loc
    return $ AnonymFun [a, b] (BinOp Plus av bv (Elem Int) loc) (Elem Int) loc
  scan <- transformExp $ Scan plus (intval 0) mape (Elem Int) loc
  (ia, _, ialet) <- newLet scan "ia"
  let indexia ind = Index ia [ind] (Elem Int) loc
      indexiaend = indexia (BinOp Minus nv (intval 1) (Elem Int) loc)
      indexi = indexia iv
      indexim1 = indexia (BinOp Minus iv (intval 1) (Elem Int) loc)
  (res, resv, reslet) <- newLet (Replicate indexiaend indexin0 loc) "res"
  let loop = DoLoop (Id res) resv i nv loopbody resv loc
      loopbody = If (Or (BinOp Equal iv (intval 0) (Elem Bool) loc)
                        (And (BinOp Less (intval 0) iv (Elem Bool) loc)
                             (BinOp Equal indexi indexim1 (Elem Bool) loc) loc)
                     loc)
                 resv update (typeOf arrexp) loc
      update = LetWith res res [BinOp Minus indexi (intval 1) (Elem Int) loc] indexin resv loc
  return $ arrlet $ nlet $ checkempty $ ialet $ reslet loop
  where intval x = Literal (IntVal x) loc
transformExp (Mapall fun arrexp loc) = transformExp =<< toMap arrexp
  where toMap e = case peelArray 1 $ typeOf e of
                    Just et -> do
                      (x,xv) <- newVar "x" et loc
                      body <- toMap xv
                      let ot = arrayType (arrayDims et) (typeOf fun) Nonunique
                      return $ Map (AnonymFun [x] body ot loc) e et loc
                    _ -> transformLambda fun [e]
transformExp (Redomap redfun mapfun accexp arrexp intype _ loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype loc
  mapfuncall <- transformLambda mapfun [index]
  redfuncall <- transformLambda redfun [accv, mapfuncall]
  let loop = DoLoop (Id acc) accv i (Size arrv loc) loopbody accv loc
      loopbody = LetWith acc acc [] redfuncall accv loc
  return $ redlet loop
transformExp e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = transformExp
                    }

newReduction :: SrcLoc -> Exp Type -> Exp Type
             -> TransformM ((Ident Type, Exp Type),
                            (Ident Type, Exp Type),
                            (Ident Type, Exp Type),
                            Exp Type -> Exp Type)
newReduction loc arrexp accexp = do
  (arr, arrv, arrlet) <- newLet arrexp "arr"
  (acc, accv, acclet) <- newLet accexp "acc"
  (i, iv) <- newVar "i" (Elem Int) loc
  return ((arr, arrv), (acc, accv), (i, iv), acclet . arrlet)

newLet :: Exp Type -> String -> TransformM (Ident Type, Exp Type, Exp Type -> Exp Type)
newLet e name = do
  e' <- liftM maybeCopy $ transformExp e
  (x,xv) <- newVar name (typeOf e') loc
  let xlet body = LetPat (Id x) e' body loc
  return (x, xv, xlet)
  where loc = srclocOf e


newVar :: String -> Type -> SrcLoc -> TransformM (Ident Type, Exp Type)
newVar name tp loc = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: Exp Type -> Exp Type
maybeCopy e
  | unique e || basicType (typeOf e)  = e
  | otherwise = Copy e $ srclocOf e

transformLambda :: Lambda Type -> [Exp Type] -> TransformM (Exp Type)
transformLambda (AnonymFun params body _ loc) args = do
  body' <- transformExp body
  return $ foldl bind body' $ zip params args
  where bind e (Ident pname ptype _, arg) = LetPat (Id $ Ident pname ptype loc) arg e loc
transformLambda (CurryFun fname curryargs rettype loc) args = do
  curryargs' <- mapM transformExp curryargs
  return $ Apply fname (curryargs'++args) rettype loc
