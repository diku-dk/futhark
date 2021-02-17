{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Naming scheme:
--
-- An adjoint-related object for "x" is named "x_adj".  This means
-- both actual adjoints and statements.
--
-- Do not assume "x'" means anything related to derivatives.
module Futhark.AD.Rev (revVJP) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import qualified Data.Map as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Binder
import Futhark.Construct
import Futhark.IR.SOACS

data RState = RState
  { stateAdjs :: M.Map VName VName,
    stateNameSource :: VNameSource
  }

newtype ADM a = ADM (BinderT SOACS (State RState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState RState,
      MonadFreshNames,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadBinder ADM where
  type Lore ADM = SOACS
  mkExpDecM pat e = ADM $ mkExpDecM pat e
  mkBodyM bnds res = ADM $ mkBodyM bnds res
  mkLetNamesM pat e = ADM $ mkLetNamesM pat e

  addStms = ADM . addStms
  collectStms (ADM m) = ADM $ collectStms m

instance MonadFreshNames (State RState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify (\env -> env {stateNameSource = src})

runADM :: MonadFreshNames m => ADM a -> m a
runADM (ADM m) =
  modifyNameSource $ \vn ->
    second stateNameSource $
      runState
        (fst <$> runBinderT m mempty)
        (RState mempty vn)

adjVName :: VName -> ADM VName
adjVName v = newVName (baseString v <> "_adj")

newAdj :: VName -> ADM VName
newAdj v = do
  v_adj <- adjVName v
  t <- lookupType v
  let update = M.singleton v v_adj
  modify $ \env -> env {stateAdjs = update `M.union` stateAdjs env}
  letBindNames [v_adj] =<< eBlank t
  pure v_adj

insAdj :: VName -> VName -> ADM ()
insAdj v v_adj = modify $ \env ->
  env {stateAdjs = M.insert v v_adj $ stateAdjs env}

insAdjMap :: M.Map VName VName -> ADM ()
insAdjMap update = modify $ \env ->
  env {stateAdjs = update `M.union` stateAdjs env}

class Adjoint a where
  lookupAdj :: a -> ADM VName
  updateAdjoint :: a -> VName -> ADM VName

addBinOp :: PrimType -> BinOp
addBinOp (IntType it) = Add it OverflowWrap
addBinOp (FloatType ft) = FAdd ft
addBinOp Bool = LogAnd
addBinOp Cert = LogAnd

instance Adjoint VName where
  lookupAdj v = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> newAdj v
      Just v_adj -> return v_adj

  updateAdjoint v d = do
    maybeAdj <- gets $ M.lookup v . stateAdjs
    case maybeAdj of
      Nothing -> setAdjoint v (BasicOp . SubExp . Var $ d)
      Just v_adj -> do
        t <- lookupType v
        v_adj' <- letExp "adj" $
          case t of
            Prim pt ->
              BasicOp $ BinOp (addBinOp pt) (Var v_adj) (Var d)
            _ ->
              error $ "updateAdjoint: unexpected type " <> pretty t
        let update = M.singleton v v_adj'
        insAdjMap update
        pure v_adj'

instance Adjoint SubExp where
  lookupAdj (Constant c) =
    letExp "const_adj" =<< eBlank (Prim $ primValueType c)
  lookupAdj (Var v) = lookupAdj v

  updateAdjoint se@Constant {} _ = lookupAdj se
  updateAdjoint (Var v) d = updateAdjoint v d

instance Adjoint (PatElemT Type) where
  lookupAdj = lookupAdj . patElemName
  updateAdjoint = updateAdjoint . patElemName

setAdjoint :: VName -> Exp -> ADM VName
setAdjoint v e = do
  v_adj <- adjVName v
  letBindNames [v_adj] e
  let update = M.singleton v v_adj
  insAdjMap update
  return v_adj

diffStm :: Stm -> ADM () -> ADM ()
diffStm stm@(Let _ _ (BasicOp CmpOp {})) m = do
  addStm stm
  m
diffStm stm@(Let (Pattern [] [pe]) _ (BasicOp (ConvOp op x))) m = do
  addStm stm
  m

  case op of
    FPConv from_t to_t -> do
      pe_adj <- lookupAdj pe
      contrib <-
        letExp "contrib" $
          BasicOp $ ConvOp (FPConv to_t from_t) $ Var pe_adj
      void $ updateAdjoint x contrib
    _ ->
      pure ()
diffStm stm@(Let (Pattern [] [pe]) _aux (BasicOp (UnOp op x))) m = do
  addStm stm
  m

  let t = unOpType op
  pe_adj <- lookupAdj $ patElemName pe
  contrib <- do
    let x_pe = primExpFromSubExp t x
        pe_adj' = primExpFromSubExp t (Var pe_adj)
        dx = pdUnOp op x_pe
    letExp "contrib" <=< toExp $ pe_adj' ~*~ dx

  void $ updateAdjoint x contrib
diffStm stm@(Let (Pattern [] [pe]) _aux (BasicOp (BinOp op x y))) m = do
  addStm stm
  m

  let t = binOpType op
  pe_adj <- lookupAdj $ patElemName pe

  let (wrt_x, wrt_y) =
        pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

      pe_adj' = primExpFromSubExp t $ Var pe_adj

  adj_x <- letExp "adj" <=< toExp $ pe_adj' ~*~ wrt_x
  adj_y <- letExp "adj" <=< toExp $ pe_adj' ~*~ wrt_y
  void $ updateAdjoint x adj_x
  void $ updateAdjoint y adj_y
diffStm stm@(Let (Pattern [] [pe]) _ (BasicOp (SubExp (Var v)))) m = do
  addStm stm
  m
  void $ updateAdjoint v =<< lookupAdj pe
diffStm stm@(Let _ _ (BasicOp (SubExp (Constant _)))) m = do
  addStm stm
  m
diffStm stm@(Let Pattern {} _ (BasicOp Assert {})) m = do
  addStm stm
  m
diffStm stm@(Let (Pattern [] [pe]) _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    m

    pe_adj <- lookupAdj pe
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
        pe_adj' = primExpFromSubExp ret (Var pe_adj)

    contribs <-
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          mapM (letExp "contrib" <=< toExp . (pe_adj' ~*~)) derivs

    let updateArgAdj (Var x, _) x_contrib = void $ updateAdjoint x x_contrib
        updateArgAdj _ _ = pure ()
    zipWithM_ updateArgAdj args contribs
diffStm stm _ = error $ "diffStm unhandled:\n" ++ pretty stm

diffStms :: Stms SOACS -> ADM ()
diffStms all_stms
  | Just (stm, stms) <- stmsHead all_stms =
    diffStm stm $ diffStms stms
  | otherwise =
    pure ()

diffBody :: [VName] -> Body -> ADM Body
diffBody get_adjs_for (Body desc stms res) = do
  (adjs, stms') <- collectStms $ do
    diffStms stms
    mapM lookupAdj get_adjs_for
  pure $ Body desc stms' $ res <> map Var adjs

revVJP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
revVJP scope (Lambda params body@(Body () _ res) ts) =
  runADM . localScope (scope <> scopeOfLParams params) $ do
    let rvars = subExpVars res
    params_adj <- forM (zip rvars ts) $ \(v, t) -> do
      v_adj <- adjVName v
      insAdj v v_adj
      pure $ Param v_adj t

    Body () stms' res' <-
      localScope (scopeOfLParams params_adj) $
        diffBody (map paramName params) body

    let adj_res = drop (length ts) res'
    pure $
      Lambda
        (params ++ params_adj)
        (Body () stms' adj_res)
        (map paramType params)
