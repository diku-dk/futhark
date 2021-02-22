{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.AD.Fwd (fwdJVP) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import qualified Data.Map as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Binder
import Futhark.Construct
import Futhark.IR.SOACS

zeroTan :: Type -> ADM SubExp
zeroTan (Prim t) = return $ constant $ blankPrimValue t

slocal' :: (MonadState s m) => m a -> m a
slocal' = slocal id

slocal :: (MonadState s m) => (s -> s) -> m a -> m a
slocal f m = do
  s <- get
  modify f
  a <- m
  put s
  return a

data RState = RState
  { stateTans :: M.Map VName VName,
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

tanVName :: VName -> ADM VName
tanVName v = newVName (baseString v <> "_tan")

class TanBinder a where
  newTan :: a -> ADM a

instance (TanBinder a) => TanBinder [a] where
  newTan = mapM newTan

instance TanBinder (PatElemT dec) where
  newTan (PatElem p t) = do
    p' <- tanVName p
    modify $ \env -> env {stateTans = M.insert p p' (stateTans env)}
    return $ PatElem p' t

instance TanBinder (PatternT dec) where
  newTan (Pattern [] pes) = Pattern [] <$> newTan pes

instance TanBinder (Param attr) where
  newTan (Param p t) = do
    p' <- tanVName p
    modify $ \env -> env {stateTans = M.insert p p' (stateTans env)}
    return $ Param p' t

class Tangent a where
  tangent :: a -> ADM a

instance Tangent a => Tangent [a] where
  tangent = mapM tangent

instance Tangent VName where
  tangent v = do
    maybeTan <- gets $ M.lookup v . stateTans
    case maybeTan of
      Just v' -> return v'
      Nothing -> error $ "No tangent: " ++ show v

instance Tangent SubExp where
  tangent (Constant c) = zeroTan $ Prim $ primValueType c
  tangent (Var v) = Var <$> tangent v

patNames :: Pattern -> ADM [VName]
patNames (Pattern [] pes) = pure $ map patElemName pes

basicFwd :: Pattern -> StmAux () -> BasicOp -> ADM ()
basicFwd pat aux op = do
  pat_tan <- newTan pat
  pat_v_tan <- patNames pat_tan
  case op of
    SubExp se -> do
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ SubExp se_tan
    Opaque se -> do
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ Opaque se_tan
    ArrayLit ses t -> do
      ses_tan <- tangent ses
      addStm $ Let pat_tan aux $ BasicOp $ ArrayLit ses_tan t
    UnOp unop x -> do
      let t = unOpType unop
          x_pe = primExpFromSubExp t x
          dx = pdUnOp unop x_pe
      x_tan <- primExpFromSubExp t <$> tangent x
      auxing aux $ letBindNames pat_v_tan <=< toExp $ x_tan ~*~ dx
    BinOp bop x y -> do
      let t = binOpType bop
      x_tan <- primExpFromSubExp t <$> tangent x
      y_tan <- primExpFromSubExp t <$> tangent y
      let (wrt_x, wrt_y) =
            pdBinOp bop (primExpFromSubExp t x) (primExpFromSubExp t y)
      auxing aux $
        letBindNames pat_v_tan <=< toExp $
          x_tan ~*~ wrt_x ~+~ y_tan ~*~ wrt_y
    ConvOp cop x -> do
      x_tan <- tangent x
      addStm $ Let pat_tan aux $ BasicOp $ ConvOp cop x_tan
    Assert {} ->
      addStm $ Let pat_tan aux $ BasicOp op
    CmpOp {} ->
      addStm $ Let pat_tan aux $ BasicOp op
    Index arr slice -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Index arr_tan slice
    Reshape reshape arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Reshape reshape arr_tan
    Rearrange perm arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Rearrange perm arr_tan
    Rotate rots arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Rotate rots arr_tan
    Concat d arr arrs w -> do
      arr_tan <- tangent arr
      arrs_tans <- tangent arrs
      addStm $ Let pat_tan aux $ BasicOp $ Concat d arr_tan arrs_tans w
    Replicate n x -> do
      x_tan <- tangent x
      addStm $ Let pat_tan aux $ BasicOp $ Replicate n x_tan

fwdStm :: Stm -> ADM ()
fwdStm stm@(Let pat aux (BasicOp e)) = addStm stm >> basicFwd pat aux e
fwdStm stm@(Let pat _ (Apply f args _ _))
  | Just (_, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    arg_tans <-
      zipWith primExpFromSubExp argts <$> mapM (tangent . fst) args
    pat_tan <- newTan pat
    pat_v_tan <- patNames pat_tan
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
    case pdBuiltin f arg_pes of
      Nothing ->
        error $ "No partial derivative defined for builtin function: " ++ pretty f
      Just derivs ->
        zipWithM_ (letBindNames . pure) pat_v_tan
          =<< mapM toExp (zipWith (~*~) arg_tans derivs)
fwdStm (Let (Pattern ctx pes) aux (If cond t f (IfDec ret ifsort))) = do
  t_tan <- slocal' $ fwdBody t
  f_tan <- slocal' $ fwdBody f
  pes_tan <- newTan pes
  addStm $
    Let (Pattern ctx (pes ++ pes_tan)) aux $
      If cond t_tan f_tan (IfDec (ret ++ ret) ifsort)
fwdStm (Let (Pattern [] pes) aux (DoLoop [] val_pats (WhileLoop v) body)) = do
  let (val_params, vals) = unzip val_pats
  vals_tan <- tangent vals
  pes_tan <- newTan pes
  slocal' $ do
    val_params_tan <- newTan val_params
    let val_pats_tan = zip val_params_tan vals_tan
    body_tan <- fwdBody body
    addStm $
      Let (Pattern [] (pes ++ pes_tan)) aux $
        DoLoop [] (val_pats ++ val_pats_tan) (WhileLoop v) body_tan
fwdStm (Let (Pattern [] pes) aux (DoLoop [] val_pats loop@(ForLoop i it bound []) body)) = do
  let (val_params, vals) = unzip val_pats
  vals_tan <- tangent vals
  pes_tan <- newTan pes
  slocal' $ do
    val_params_tan <- newTan val_params
    let val_pats_tan = zip val_params_tan vals_tan
    body_tan <- inScopeOf loop $ fwdBody body
    addStm $
      Let (Pattern [] (pes ++ pes_tan)) aux $
        DoLoop [] (val_pats ++ val_pats_tan) (ForLoop i it bound []) body_tan
fwdStm (Let (Pattern [] pes) aux (DoLoop [] val_pats loop@(ForLoop i it bound loop_vars) body)) = do
  let (val_params, vals) = unzip val_pats
      (loop_params, loop_vals) = unzip loop_vars
  vals_tan <- tangent vals
  loop_vals_tan <- tangent loop_vals
  pes_tan <- newTan pes
  slocal' $ do
    val_params_tan <- newTan val_params
    loop_params_tan <- newTan loop_params
    let val_pats_tan = zip val_params_tan vals_tan
    let loop_vars_tan = zip loop_params_tan loop_vals_tan
    body_tan <- inScopeOf loop $ fwdBody body
    addStm $
      Let (Pattern [] (pes ++ pes_tan)) aux $
        DoLoop [] (val_pats ++ val_pats_tan) (ForLoop i it bound (loop_vars ++ loop_vars_tan)) body_tan
fwdStm stm =
  error $ "unhandled forward mode AD for Stm: " ++ pretty stm ++ "\n" ++ show stm

fwdBody :: Body -> ADM Body
fwdBody (Body _ stms res) = do
  (res', stms') <- collectStms $ do
    mapM_ fwdStm stms
    mapM tangent res
  return $ mkBody stms' $ res ++ res'

fwdBodyOnlyTangents :: Body -> ADM Body
fwdBodyOnlyTangents (Body _ stms res) = do
  (res', stms') <- collectStms $ do
    mapM_ fwdStm stms
    mapM tangent res
  return $ mkBody stms' res'

fwdJVP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
fwdJVP scope (Lambda params body ret) = do
  runADM . localScope scope $ do
    params' <- newTan params
    body' <- fwdBodyOnlyTangents body
    pure $ Lambda (params ++ params') body' ret
