module L0.FirstOrderTransform
  ( transformProg )
  where

import Control.Monad.State

import Data.Generics
import Data.Loc

import L0.AbSyn

data TrState = TrState {
    stCounter :: Int
  , newFunctions :: [FunDec Type]
  }

newTrState :: TrState
newTrState = TrState {
               stCounter = 0
             , newFunctions = []
             }

type TransformM = State TrState

-- | Return a new, fresh name, with the given string being part of the
-- name.
new :: String -> TransformM String
new k = do i <- gets stCounter
           modify $ \s -> s { stCounter = i + 1 }
           return $ k ++ "_" ++ show i

runTransformM :: TransformM a -> a
runTransformM m = evalState m newTrState

transformProg :: Prog Type -> Prog Type
transformProg = runTransformM . mapM transformFunDec

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
      loopbody = LetWith arr arrv [iv] funcall arrv loc
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
  (outarr, outarrv, outarrlet) <- newLet (Replicate nv funcall0 outtype loc) "outarr"
  let branch = If (BinOp Less zero nv (Bool loc) loc)
               (outarrlet letbody)
               (Literal (arrayVal [] outtype loc)) outarrt loc
      letbody = DoLoop i nv loopbody [outarr] loc
      loopbody = LetWith outarr outarrv [iv] funcall outarrv loc
  return $ inarrlet $ nlet branch
transformExp (Reduce fun accexp arrexp intype loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype intype loc
  funcall <- transformLambda fun [accv, index]
  let loop = DoLoop i (Size arrv loc) loopbody [acc] loc
      loopbody = LetWith acc accv [] funcall accv loc
  return $ redlet loop
transformExp e@(Scan fun accexp arrexp intype loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype intype loc
  funcall <- transformLambda fun [accv, index]
  let looplet = LetPat (TupId [Id arr (expType e) loc,
                               Id acc (expType accexp) loc] loc)
                loop arrv loc
      loop = DoLoop i (Size arrv loc) loopbody [acc, arr] loc
      loopbody = LetWith arr arrv [iv] funcall (TupLit [accv, arrv] loc) loc
  return $ redlet looplet
transformExp (Mapall fun arrexp _ outtype loc) = transformExp =<< toMap arrexp
  where toMap e = case expType e of
                    Array et _ _ -> do
                      (x,xv) <- newVar "x" et loc
                      body <- toMap xv
                      let ot = arrayType (arrayDims et) outtype
                      return $ Map (AnonymFun [(x, et)] body ot loc) e et ot loc
                    _ -> transformLambda fun [e]
transformExp (Redomap redfun mapfun accexp arrexp intype _ loc) = do
  ((arr, arrv), (acc, accv), (i, iv), redlet) <- newReduction loc arrexp accexp
  let index = Index arr [iv] intype intype loc
  mapfuncall <- transformLambda mapfun [index]
  redfuncall <- transformLambda redfun [accv, mapfuncall]
  let loop = DoLoop i (Size arrv loc) loopbody [acc, arr] loc
      loopbody = LetWith arr arrv [iv] redfuncall (TupLit [accv, arrv] loc) loc
  return $ redlet loop
transformExp e = return e

newReduction :: Loc -> Exp Type -> Exp Type
             -> TransformM ((String, Exp Type),
                            (String, Exp Type),
                            (String, Exp Type),
                            Exp Type -> Exp Type)
newReduction loc arrexp accexp = do
  (arr, arrv, arrlet) <- newLet arrexp "arr"
  (acc, accv, acclet) <- newLet accexp "acc"
  (i, iv) <- newVar "i" (Int loc) loc
  return ((arr, arrv), (acc, accv), (i, iv), acclet . arrlet)

newLet :: Exp Type -> String -> TransformM (String, Exp Type, Exp Type -> Exp Type)
newLet e name = do
  x <- new name
  let xv = Var x (expType e) loc
      xlet body = LetPat (Id x (expType e) loc) e body loc
  return (x, xv, xlet)
  where loc = locOf e

newVar :: String -> Type -> Loc -> TransformM (String, Exp Type)
newVar name tp loc = do
  x <- new name
  return (x, Var x tp loc)

transformLambda :: Lambda Type -> [Exp Type] -> TransformM (Exp Type)
transformLambda (AnonymFun params body _ loc) args = do
  body' <- transformExp body
  return $ foldl bind body' $ zip params args
  where bind e ((pname, ptype), arg) = LetPat (Id pname ptype loc) arg e loc
transformLambda (CurryFun fname curryargs rettype loc) args = do
  curryargs' <- mapM transformExp curryargs
  return $ Apply fname (curryargs'++args) rettype loc
