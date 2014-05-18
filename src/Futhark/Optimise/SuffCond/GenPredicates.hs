-- | This module exports functionality for splitting a function into
-- predicate and value function.
module Futhark.Optimise.SuffCond.GenPredicates
  ( genPredicate
  )
  where

import Control.Applicative

import Data.Loc
import Data.Maybe
import Data.Monoid

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Tools

predicateFunctionName :: Name -> Name
predicateFunctionName fname = fname <> nameFromString "_pred"

genPredicate :: MonadFreshNames m =>
                FunDec -> m (FunDec, FunDec)
genPredicate (fname,rettype,params,body,loc) = do
  pred_ident <- newIdent "pred" (Basic Bool) loc
  cert_ident <- newIdent "pred_cert" (Basic Cert) loc
  (pred_params, bnds) <- nonuniqueParams params
  (pred_body, Body val_bnds val_res) <- splitFunBody cert_ident body
  let pred_args = [ (Var $ fromParam arg, Observe) | arg <- params ]
      pred_bnd = Let [pred_ident] $ Apply predFname pred_args [Basic Bool] loc
      cert_bnd = Let [cert_ident] $ Assert (Var pred_ident) loc
      val_fun = (fname, rettype, params,
                 Body (pred_bnd:cert_bnd:val_bnds) val_res, loc)
      pred_fun = (predFname, [Basic Bool], pred_params,
                  bnds `insertBindings` pred_body, loc)
  return (pred_fun, val_fun)
  where predFname = predicateFunctionName fname

splitFunBody :: MonadFreshNames m => Ident -> Body -> m (Body, Body)
splitFunBody cert_ident body = do
  (pred_body, val_body) <- splitBody cert_ident body
  let onlyCert res = Body [] $ case resultSubExps res of
        []  -> res  -- Does this ever happen?
        c:_ -> res { resultSubExps = [c] }
  return (mapResult onlyCert pred_body,
          val_body)

splitBody :: MonadFreshNames m => Ident -> Body -> m (Body, Body)
splitBody cert_ident (Body bnds valres) = do
  (pred_bnds, val_bnds, preds) <- unzip3 <$> mapM (splitBinding cert_ident) bnds
  (conjoined_preds, conj_bnds) <-
    runBinder'' $ letSubExp "conjoined_preds" =<<
    foldBinOp LogAnd (constant True loc) (catMaybes preds) (Basic Bool)
  let predbody = Body (concat pred_bnds <> conj_bnds) $
                 valres { resultSubExps =
                             conjoined_preds : resultSubExps valres
                        }
      valbody = Body val_bnds valres
  return (predbody, valbody)
  where loc = srclocOf valres

splitBinding :: MonadFreshNames m => Ident -> Binding -> m ([Binding], Binding, Maybe SubExp)

splitBinding cert_ident bnd@(Let pat (Assert se _)) =
  return ([bnd],
          Let pat $ SubExp (Var cert_ident),
          Just se)

splitBinding cert_ident bnd@(Let pat (Map cs fun args loc)) = do
  (predbody, valfun, ok) <- splitMap cert_ident cs fun args loc
  return (predbody ++ [bnd],
          Let pat $ Map cs valfun args loc,
          ok)

splitBinding cert_ident bnd@(Let pat (Filter cs fun args ressize loc)) = do
  (predbnds, valfun, ok) <- splitMap cert_ident cs fun args loc
  return (predbnds ++ [bnd],
          Let pat $ Filter cs valfun args ressize loc,
          ok)

splitBinding cert_ident (Let pat (DoLoop respat merge i bound body loc)) = do
  (predbody, valbody) <- splitBody cert_ident body
  ok <- newIdent "loop_ok" (Basic Bool) loc
  let predloop = DoLoop (ok:respat)
                 ((ok,constant True loc):merge) i bound predbody loc
      valloop = DoLoop respat merge i bound valbody loc
  return ([Let (ok:pat) predloop],
          Let pat valloop,
          Just $ Var ok)

splitBinding cert_ident (Let pat (If cond tbranch fbranch t loc)) = do
  (tbranch_pred, tbranch_val) <- splitBody cert_ident tbranch
  (fbranch_pred, fbranch_val) <- splitBody cert_ident fbranch
  ok <- newIdent "if_ok" (Basic Bool) loc
  return ([Let (ok:pat) $ If cond tbranch_pred fbranch_pred (Basic Bool:t) loc],
          Let pat $ If cond tbranch_val fbranch_val t loc,
          Just $ Var ok)

splitBinding _ bnd = return ([bnd], bnd, Nothing)

splitMap :: MonadFreshNames m =>
            Ident -> [Ident] -> Lambda -> [SubExp] -> SrcLoc
         -> m ([Binding], Lambda, Maybe SubExp)
splitMap cert_ident cs fun args loc = do
  (predfun, valfun) <- splitMapLambda cert_ident fun
  andchecks <- newIdent "checks" (Basic Bool) loc
  andfun <- binOpLambda LogAnd (Basic Bool) loc
  innerfun <- predConjFun predfun
  let andbnd = Let [andchecks] $
               Redomap cs andfun innerfun [constant True loc] args loc
  return ([andbnd],
          valfun,
          Just $ Var andchecks)
  where predConjFun predfun = do
          acc <- newIdent "acc" (Basic Bool) loc
          res <- newIdent "res" (Basic Bool) loc
          let Body predbnds (Result _ [se] _) = lambdaBody predfun -- XXX
              andbnd = Let [res] $ BinOp LogAnd (Var acc) se (Basic Bool) loc
              body = Body (predbnds++[andbnd]) $ Result [] [Var res] loc
          return Lambda { lambdaSrcLoc = srclocOf predfun
                        , lambdaParams = toParam acc : lambdaParams predfun
                        , lambdaReturnType = [Basic Bool]
                        , lambdaBody = body
                        }

splitMapLambda :: MonadFreshNames m => Ident -> Lambda -> m (Lambda, Lambda)
splitMapLambda cert_ident lam = do
  (Body predbnds predres, valbody) <- splitFunBody cert_ident $ lambdaBody lam
  (pred_params, cpybnds) <- nonuniqueParams $ lambdaParams lam
  let predbody = Body (cpybnds <> predbnds) predres
      predlam = lam { lambdaBody = predbody
                    , lambdaReturnType = [Basic Bool]
                    , lambdaParams = pred_params
                    }
      vallam = lam { lambdaBody = valbody }
  return (predlam, vallam)
