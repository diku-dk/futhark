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

import Prelude

import Futhark.Analysis.DataDependencies
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools

type Blacklist = Names

data GenEnv = GenEnv Ident Dependencies Blacklist

type GenM = ReaderT GenEnv (State VNameSource)

runGenM :: MonadFreshNames m => GenEnv -> GenM a -> m a
runGenM env m = modifyNameSource $ runState (runReaderT m env)

banning :: Names -> GenM a -> GenM a
banning = local . banning'
  where banning' names (GenEnv cert deps blacklist) =
          GenEnv cert deps $ blacklist <> names

predicateFunctionName :: Name -> Name
predicateFunctionName fname = fname <> nameFromString "_pred"

genPredicate :: MonadFreshNames m => FunDec -> m (FunDec, FunDec)
genPredicate (FunDec fname rettype params body) = do
  pred_ident <- newIdent "pred" $ Basic Bool
  cert_ident <- newIdent "pred_cert" $ Basic Cert
  ((pred_params, bnds),_) <- runBinderEmptyEnv $ nonuniqueParams $ map fparamIdent params
  let env = GenEnv cert_ident (dataDependencies body) mempty
  (pred_body, Body _ val_bnds val_res) <- runGenM env $ splitFunBody body
  let mkFParam = flip FParam ()
      pred_args = [ (Var arg, Observe) | arg <- map fparamName params ]
      pred_bnd = mkLet' [] [pred_ident] $
                 Apply predFname pred_args $ basicRetType Bool
      cert_bnd = mkLet' [] [cert_ident] $
                 PrimOp $ Assert (Var $ identName pred_ident) noLoc
      val_fun = FunDec fname rettype params
                (mkBody (pred_bnd:cert_bnd:val_bnds) val_res)
      pred_fun = FunDec predFname (basicRetType Bool)
                 (map mkFParam pred_params)
                 (bnds `insertBindings` pred_body)
  return (pred_fun, val_fun)
  where predFname = predicateFunctionName fname

splitFunBody :: Body -> GenM (Body, Body)
splitFunBody body = do
  (pred_body, val_body) <- splitBody body
  let onlyCert res = mkBody [] $ case reverse $ resultSubExps res of
        []  -> res  -- Does this ever happen?
        c:_ -> res { resultSubExps = [c] }
  return (mapResult onlyCert pred_body,
          val_body)

splitBody :: Body -> GenM (Body, Body)
splitBody (Body _ bnds valres) = do
  (pred_bnds, val_bnds, preds) <- unzip3 <$> mapM splitBinding bnds
  (conjoined_preds, conj_bnds) <-
    runBinderEmptyEnv $ letSubExp "conjPreds" =<<
    foldBinOp LogAnd (constant True) (catMaybes preds) Bool
  let predbody = mkBody (concat pred_bnds <> conj_bnds) $
                 valres { resultSubExps =
                             resultSubExps valres ++ [conjoined_preds]
                        }
      valbody = mkBody val_bnds valres
  return (predbody, valbody)

splitBinding :: Binding -> GenM ([Binding], Binding, Maybe SubExp)

splitBinding bnd@(Let pat _ (PrimOp (Assert (Var v) _))) = do
  GenEnv cert_ident deps blacklist <- ask
  let forbidden =
        not $ HS.null $ maybe HS.empty (HS.intersection blacklist) $
        HM.lookup v deps
  return $ if forbidden then ([bnd], bnd, Nothing)
           else ([bnd],
                 mkLet' [] (patternIdents pat) $
                 PrimOp $ SubExp (Var $ identName cert_ident),
                 Just $ Var v)

splitBinding bnd@(Let pat _ (LoopOp (Map cs fun args))) = do
  (predbody, valfun, ok) <- splitMap cs fun args
  return (predbody ++ [bnd],
          mkLet' [] (patternIdents pat) $
          LoopOp $ Map cs valfun args,
          ok)

splitBinding bnd@(Let pat _ (LoopOp (Reduce cs fun args))) = do
  (predbody, valfun, ok) <- splitReduce cs fun args
  return (predbody ++ [bnd],
          mkLet' [] (patternIdents pat) $
          LoopOp $ Reduce cs valfun args,
          ok)

splitBinding bnd@(Let pat _ (LoopOp (Scan cs fun args))) = do
  (predbody, valfun, ok) <- splitReduce cs fun args
  return (predbody ++ [bnd],
          mkLet' [] (patternIdents pat) $
          LoopOp $ Scan cs valfun args,
          ok)

splitBinding bnd@(Let pat _ (LoopOp (Redomap cs outerfun innerfun acc arr))) = do
  (predbody, valfun, ok) <- splitRedomap cs innerfun acc arr
  return (predbody ++ [bnd],
          mkLet' [] (patternIdents pat) $
          LoopOp $ Redomap cs outerfun valfun acc arr,
          ok)

splitBinding (Let pat _ (LoopOp (DoLoop respat merge form body))) = do
  (predbody, valbody) <- splitBody body
  ok <- newIdent "loop_ok" (Basic Bool)
  predbody' <- conjoinLoopBody (identName ok) predbody
  let predloop = LoopOp $ DoLoop (respat++[identName ok])
                 (merge++[(FParam ok (),constant True)]) form
                 predbody'
      valloop = LoopOp $ DoLoop respat merge form valbody
  return ([mkLet' [] (idents<>[ok]) predloop],
          mkLet' [] idents valloop,
          Just $ Var $ identName ok)
  where
    idents = patternIdents pat
    conjoinLoopBody ok (Body _ bnds res) = do
      ok' <- newIdent "loop_ok_res" (Basic Bool)
      case reverse $ resultSubExps res of
        []   -> fail "conjoinLoopBody: null loop"
        x:xs ->
          let res' = res { resultSubExps = reverse $ Var (identName ok'):xs }
              bnds' = bnds ++
                      [mkLet' [] [ok'] $ PrimOp $ BinOp LogAnd x (Var ok) Bool]
          in return $ mkBody bnds' res'

splitBinding (Let pat _ (If cond tbranch fbranch t)) = do
  (tbranch_pred, tbranch_val) <- splitBody tbranch
  (fbranch_pred, fbranch_val) <- splitBody fbranch
  ok <- newIdent "if_ok"  $ Basic Bool
  return ([mkLet' [] (idents<>[ok]) $
           If cond tbranch_pred fbranch_pred
           (t<>[Basic Bool])],
          mkLet' [] idents $ If cond tbranch_val fbranch_val t,
          Just $ Var $ identName ok)
  where idents = patternIdents pat

splitBinding bnd = return ([bnd], bnd, Nothing)

splitMap :: Certificates -> Lambda -> [VName]
         -> GenM ([Binding], Lambda, Maybe SubExp)
splitMap cs fun args = do
  (predfun, valfun) <- splitMapLambda fun
  (andbnd, andcheck) <- allTrue cs predfun args
  return ([andbnd],
          valfun,
          Just andcheck)

splitReduce :: Certificates -> Lambda -> [(SubExp,VName)]
            -> GenM ([Binding], Lambda, Maybe SubExp)
splitReduce cs fun args = do
  (predfun, valfun) <- splitFoldLambda fun $ map fst args
  (andbnd, andcheck) <- allTrue cs predfun (map snd args)
  return ([andbnd],
          valfun,
          Just andcheck)

splitRedomap :: Certificates -> Lambda -> [SubExp] -> [VName]
             -> GenM ([Binding], Lambda, Maybe SubExp)
splitRedomap cs fun acc arr = do
  (predfun, valfun) <- splitFoldLambda fun acc
  (andbnd, andcheck) <- allTrue cs predfun arr
  return ([andbnd],
          valfun,
          Just andcheck)

splitMapLambda :: Lambda -> GenM (Lambda, Lambda)
splitMapLambda lam = do
  (Body _ predbnds predres, valbody) <- splitFunBody $ lambdaBody lam
  (pred_params, cpybnds) <- nonuniqueParams $ lambdaParams lam
  let predbody = mkBody (cpybnds <> predbnds) predres
      predlam = lam { lambdaBody = predbody
                    , lambdaReturnType = [Basic Bool]
                    , lambdaParams = pred_params
                    }
      vallam = lam { lambdaBody = valbody }
  return (predlam, vallam)

splitFoldLambda :: Lambda -> [SubExp] -> GenM (Lambda, Lambda)
splitFoldLambda lam acc = do
  (Body _ predbnds predres, valbody) <-
    banning (HS.fromList $ map identName accParams) $
    splitFunBody $ lambdaBody lam
  (pred_params, cpybnds) <- nonuniqueParams arrParams
  let predbody = mkBody (accbnds <> cpybnds <> predbnds) predres
      predlam = lam { lambdaBody = predbody
                    , lambdaReturnType = [Basic Bool]
                    , lambdaParams = pred_params
                    }
      vallam = lam { lambdaBody = valbody }
  return (predlam, vallam)
  where (accParams,arrParams) = splitAt (length acc) $ lambdaParams lam
        accbnds = [ mkLet' [] [p] $ PrimOp $ SubExp e
                  | (p,e) <- zip accParams acc ]

allTrue :: Certificates -> Lambda -> [VName]
        -> GenM (Binding, SubExp)
allTrue cs predfun args = do
  andchecks <- newIdent "allTrue" (Basic Bool)
  andfun <- binOpLambda LogAnd Bool
  innerfun <- predConjFun
  let andbnd = mkLet' [] [andchecks] $ LoopOp $
               Redomap cs andfun innerfun [constant True] args
  return (andbnd,
          Var $ identName andchecks)
  where predConjFun = do
          acc <- newIdent "acc" (Basic Bool)
          res <- newIdent "res" (Basic Bool)
          let Body _ predbnds (Result [se]) = lambdaBody predfun -- XXX
              andbnd = mkLet' [] [res] $ PrimOp $ BinOp LogAnd (Var $ identName acc) se Bool
              body = mkBody (predbnds++[andbnd]) $ Result [Var $ identName res]
          return Lambda { lambdaParams = acc : lambdaParams predfun
                        , lambdaReturnType = [Basic Bool]
                        , lambdaBody = body
                        }
