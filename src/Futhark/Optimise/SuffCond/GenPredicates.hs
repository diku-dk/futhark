{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.Analysis.DataDependencies
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools

type Blacklist = Names

data GenEnv = GenEnv Ident Dependencies Blacklist

type GenM = ReaderT GenEnv (State VNameSource)

runGenM :: MonadFreshNames m => GenEnv -> GenM a -> m a
runGenM env m = modifyNameSource $ runState (runReaderT m env)

instance MonadFreshNames GenM where
  getNameSource = get
  putNameSource = put

banning :: Names -> GenM a -> GenM a
banning = local . banning'
  where banning' names (GenEnv cert deps blacklist) =
          GenEnv cert deps $ blacklist <> names

predicateFunctionName :: Name -> Name
predicateFunctionName fname = fname <> nameFromString "_pred"

genPredicate :: MonadFreshNames m => FunDec -> m (FunDec, FunDec)
genPredicate (fname,rettype,params,body,loc) = do
  pred_ident <- newIdent "pred" (Basic Bool) loc
  cert_ident <- newIdent "pred_cert" (Basic Cert) loc
  (pred_params, bnds) <- nonuniqueParams params
  let env = GenEnv cert_ident (dataDependencies body) mempty
  (pred_body, Body val_bnds val_res) <- runGenM env $ splitFunBody body
  let pred_args = [ (Var $ fromParam arg, Observe) | arg <- params ]
      pred_bnd = Let [pred_ident] () $ Apply predFname pred_args [Basic Bool] loc
      cert_bnd = Let [cert_ident] () $ Assert (Var pred_ident) loc
      val_fun = (fname, rettype, params,
                 Body (pred_bnd:cert_bnd:val_bnds) val_res, loc)
      pred_fun = (predFname, [Basic Bool], pred_params,
                  bnds `insertBindings` pred_body, loc)
  return (pred_fun, val_fun)
  where predFname = predicateFunctionName fname

splitFunBody :: Body -> GenM (Body, Body)
splitFunBody body = do
  (pred_body, val_body) <- splitBody body
  let onlyCert res = Body [] $ case reverse $ resultSubExps res of
        []  -> res  -- Does this ever happen?
        c:_ -> res { resultSubExps = [c] }
  return (mapResult onlyCert pred_body,
          val_body)

splitBody :: Body -> GenM (Body, Body)
splitBody (Body bnds valres) = do
  (pred_bnds, val_bnds, preds) <- unzip3 <$> mapM splitBinding bnds
  (conjoined_preds, conj_bnds) <-
    runBinder'' $ letSubExp "conjPreds" =<<
    foldBinOp LogAnd (constant True loc) (catMaybes preds) (Basic Bool)
  let predbody = Body (concat pred_bnds <> conj_bnds) $
                 valres { resultSubExps =
                             resultSubExps valres ++ [conjoined_preds]
                        }
      valbody = Body val_bnds valres
  return (predbody, valbody)
  where loc = srclocOf valres

splitBinding :: Binding -> GenM ([Binding], Binding, Maybe SubExp)

splitBinding bnd@(Let pat () (Assert (Var v) _)) = do
  GenEnv cert_ident deps blacklist <- ask
  let forbidden =
        not $ HS.null $ maybe HS.empty (HS.intersection blacklist) $
        HM.lookup (identName v) deps
  return $ if forbidden then ([bnd], bnd, Nothing)
           else ([bnd],
                 Let pat () $ SubExp (Var cert_ident),
                 Just $ Var v)

splitBinding bnd@(Let pat () (Map cs fun args loc)) = do
  (predbody, valfun, ok) <- splitMap cs fun args loc
  return (predbody ++ [bnd],
          Let pat () $ Map cs valfun args loc,
          ok)

splitBinding bnd@(Let pat () (Filter cs fun args loc)) = do
  (predbnds, valfun, ok) <- splitMap cs fun args loc
  return (predbnds ++ [bnd],
          Let pat () $ Filter cs valfun args loc,
          ok)

splitBinding bnd@(Let pat () (Reduce cs fun args loc)) = do
  (predbody, valfun, ok) <- splitReduce cs fun args loc
  return (predbody ++ [bnd],
          Let pat () $ Reduce cs valfun args loc,
          ok)

splitBinding bnd@(Let pat () (Scan cs fun args loc)) = do
  (predbody, valfun, ok) <- splitReduce cs fun args loc
  return (predbody ++ [bnd],
          Let pat () $ Scan cs valfun args loc,
          ok)

splitBinding bnd@(Let pat () (Redomap cs outerfun innerfun acc arr loc)) = do
  (predbody, valfun, ok) <- splitRedomap cs innerfun acc arr loc
  return (predbody ++ [bnd],
          Let pat () $ Redomap cs outerfun valfun acc arr loc,
          ok)

splitBinding (Let pat () (DoLoop respat merge i bound body loc)) = do
  (predbody, valbody) <- splitBody body
  ok <- newIdent "loop_ok" (Basic Bool) loc
  predbody' <- conjoinLoopBody ok predbody
  let predloop = DoLoop (respat++[ok])
                 (merge++[(ok,constant True loc)]) i bound
                 predbody' loc
      valloop = DoLoop respat merge i bound valbody loc
  return ([Let (pat++[ok]) () predloop],
          Let pat () valloop,
          Just $ Var ok)
  where
    conjoinLoopBody ok (Body bnds res) = do
      ok' <- newIdent "loop_ok_res" (Basic Bool) loc
      case reverse $ resultSubExps res of
        []   -> fail "conjoinLoopBody: null loop"
        x:xs ->
          let res' = res { resultSubExps = reverse $ Var ok':xs }
              bnds' = bnds ++
                      [Let [ok'] () $ BinOp LogAnd x (Var ok) (Basic Bool) loc]
          in return $ Body bnds' res'

splitBinding (Let pat () (If cond tbranch fbranch t loc)) = do
  (tbranch_pred, tbranch_val) <- splitBody tbranch
  (fbranch_pred, fbranch_val) <- splitBody fbranch
  ok <- newIdent "if_ok" (Basic Bool) loc
  return ([Let (pat++[ok]) () $ If cond tbranch_pred fbranch_pred (t++[Basic Bool]) loc],
          Let pat () $ If cond tbranch_val fbranch_val t loc,
          Just $ Var ok)

splitBinding bnd = return ([bnd], bnd, Nothing)

splitMap :: [Ident] -> Lambda -> [SubExp] -> SrcLoc
         -> GenM ([Binding], Lambda, Maybe SubExp)
splitMap cs fun args loc = do
  (predfun, valfun) <- splitMapLambda fun
  (andbnd, andcheck) <- allTrue cs predfun args loc
  return ([andbnd],
          valfun,
          Just andcheck)

splitReduce :: [Ident] -> Lambda -> [(SubExp,SubExp)] -> SrcLoc
            -> GenM ([Binding], Lambda, Maybe SubExp)
splitReduce cs fun args loc = do
  (predfun, valfun) <- splitFoldLambda fun $ map fst args
  (andbnd, andcheck) <- allTrue cs predfun (map snd args) loc
  return ([andbnd],
          valfun,
          Just andcheck)

splitRedomap :: [Ident] -> Lambda -> [SubExp] -> [SubExp] -> SrcLoc
             -> GenM ([Binding], Lambda, Maybe SubExp)
splitRedomap cs fun acc arr loc = do
  (predfun, valfun) <- splitFoldLambda fun acc
  (andbnd, andcheck) <- allTrue cs predfun arr loc
  return ([andbnd],
          valfun,
          Just andcheck)

splitMapLambda :: Lambda -> GenM (Lambda, Lambda)
splitMapLambda lam = do
  (Body predbnds predres, valbody) <- splitFunBody $ lambdaBody lam
  (pred_params, cpybnds) <- nonuniqueParams $ lambdaParams lam
  let predbody = Body (cpybnds <> predbnds) predres
      predlam = lam { lambdaBody = predbody
                    , lambdaReturnType = [Basic Bool]
                    , lambdaParams = pred_params
                    }
      vallam = lam { lambdaBody = valbody }
  return (predlam, vallam)

splitFoldLambda :: Lambda -> [SubExp] -> GenM (Lambda, Lambda)
splitFoldLambda lam acc = do
  (Body predbnds predres, valbody) <-
    banning (HS.fromList $ map identName accParams) $
    splitFunBody $ lambdaBody lam
  (pred_params, cpybnds) <- nonuniqueParams arrParams
  let predbody = Body (accbnds <> cpybnds <> predbnds) predres
      predlam = lam { lambdaBody = predbody
                    , lambdaReturnType = [Basic Bool]
                    , lambdaParams = pred_params
                    }
      vallam = lam { lambdaBody = valbody }
  return (predlam, vallam)
  where (accParams,arrParams) = splitAt (length acc) $ lambdaParams lam
        accbnds = [ Let [fromParam p] () $ SubExp e
                  | (p,e) <- zip accParams acc ]

allTrue :: Certificates -> Lambda -> [SubExp] -> SrcLoc
        -> GenM (Binding, SubExp)
allTrue cs predfun args loc = do
  andchecks <- newIdent "allTrue" (Basic Bool) loc
  andfun <- binOpLambda LogAnd (Basic Bool) loc
  innerfun <- predConjFun
  let andbnd = Let [andchecks] () $
               Redomap cs andfun innerfun [constant True loc] args loc
  return (andbnd,
          Var andchecks)
  where predConjFun = do
          acc <- newIdent "acc" (Basic Bool) loc
          res <- newIdent "res" (Basic Bool) loc
          let Body predbnds (Result _ [se] _) = lambdaBody predfun -- XXX
              andbnd = Let [res] () $ BinOp LogAnd (Var acc) se (Basic Bool) loc
              body = Body (predbnds++[andbnd]) $ Result [] [Var res] loc
          return Lambda { lambdaSrcLoc = srclocOf predfun
                        , lambdaParams = toParam acc : lambdaParams predfun
                        , lambdaReturnType = [Basic Bool]
                        , lambdaBody = body
                        }
