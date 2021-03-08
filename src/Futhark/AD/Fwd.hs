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
import Data.List (transpose)
import qualified Data.Map as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp.Convert
import Futhark.Binder
import Futhark.Construct
import Futhark.IR.Prop.Names (freeIn, nameIn)
import Futhark.IR.SOACS
import Futhark.Transform.Rename (renameLambda, renameStm)

zeroTan :: Type -> ADM SubExp
zeroTan (Prim t) = return $ constant $ blankPrimValue t

zeroExp :: Type -> Exp
zeroExp (Prim pt) =
  BasicOp $ SubExp $ Constant $ blankPrimValue pt
zeroExp (Array (ElemPrim pt) shape _) =
  BasicOp $ Replicate shape $ Constant $ blankPrimValue pt
zeroExp t = error $ "zeroExp: " ++ show t

tanType :: TypeBase s u -> ADM (TypeBase s u)
tanType (Array (ElemAcc vs) shape u) = do
  vs_tan <- tangent vs
  return $ Array (ElemAcc $ vs <> vs_tan) shape u
tanType (Acc vs) = do
  vs_tan <- tangent vs
  return $ Acc $ vs <> vs_tan
tanType t = return t

slocal' :: ADM a -> ADM a
slocal' = slocal id

slocal :: (RState -> RState) -> ADM a -> ADM a
slocal f m = do
  s <- get
  modify f
  a <- m
  modify $ \s' -> s' {stateTans = stateTans s}
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

insertTan :: VName -> VName -> ADM ()
insertTan v v' =
  modify $ \env -> env {stateTans = M.insert v v' (stateTans env)}

class TanBinder a where
  newTan :: a -> ADM a

instance (TanBinder a) => TanBinder [a] where
  newTan = mapM newTan

instance TanBinder VName where
  newTan v = do
    v' <- tanVName v
    insertTan v v'
    return v'

instance TanBinder (PatElemT (TypeBase s u)) where
  newTan (PatElem p t) = do
    p' <- tanVName p
    insertTan p p'
    t' <- tanType t
    return $ PatElem p' t'

instance TanBinder (PatternT (TypeBase s u)) where
  newTan (Pattern ctx pes) = Pattern ctx <$> newTan pes

instance TanBinder (Param (TypeBase s u)) where
  newTan (Param p t) = do
    p' <- tanVName p
    insertTan p p'
    t' <- tanType t
    return $ Param p' t'

class Tangent a where
  tangent :: a -> ADM a

instance Tangent a => Tangent [a] where
  tangent = mapM tangent

instance Tangent VName where
  tangent v = do
    maybeTan <- gets $ M.lookup v . stateTans
    case maybeTan of
      Just v_tan -> return v_tan
      Nothing -> do
        t <- lookupType v
        v_tan <- newTan v
        letBindNames [v_tan] $ zeroExp t
        return v_tan

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
    CmpOp {} ->
      addStm $ Let pat_tan aux $ BasicOp op
    ConvOp cop x -> do
      x_tan <- tangent x
      addStm $ Let pat_tan aux $ BasicOp $ ConvOp cop x_tan
    Assert {} -> return ()
    Index arr slice -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Index arr_tan slice
    Update arr slice se -> do
      arr_tan <- tangent arr
      se_tan <- tangent se
      addStm $ Let pat_tan aux $ BasicOp $ Update arr_tan slice se_tan
    Concat d arr arrs w -> do
      arr_tan <- tangent arr
      arrs_tans <- tangent arrs
      addStm $ Let pat_tan aux $ BasicOp $ Concat d arr_tan arrs_tans w
    Copy arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Copy arr_tan
    Manifest ds arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Manifest ds arr_tan
    Iota {} -> return ()
    Replicate n x -> do
      x_tan <- tangent x
      addStm $ Let pat_tan aux $ BasicOp $ Replicate n x_tan
    Scratch {} -> return ()
    Reshape reshape arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Reshape reshape arr_tan
    Rearrange perm arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Rearrange perm arr_tan
    Rotate rots arr -> do
      arr_tan <- tangent arr
      addStm $ Let pat_tan aux $ BasicOp $ Rotate rots arr_tan

fwdLambda :: Lambda -> ADM Lambda
fwdLambda (Lambda params body ret) = do
  params_tan <- newTan params
  body' <- fwdBody body
  let params' = interleave params params_tan
  ret_tan <- mapM tanType ret
  pure $ Lambda params' body' $ interleave ret ret_tan

flipLambda :: Lambda -> Lambda
flipLambda (Lambda params body ret) = Lambda (reverse params) body ret

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]

zeroFromSubExp :: SubExp -> ADM VName
zeroFromSubExp (Constant c) =
  letExp "zero" $
    BasicOp $ SubExp $ Constant $ blankPrimValue $ primValueType c
zeroFromSubExp (Var v) = do
  t <- lookupType v
  letExp "zero" $ zeroExp t

fwdSOAC :: Pattern -> StmAux () -> SOAC SOACS -> ADM ()
fwdSOAC pat aux (Screma size (ScremaForm scs reds f) xs) = do
  pat_tan <- newTan pat
  xs_tan <- tangent xs
  scs' <- mapM fwdScan scs
  reds' <- mapM fwdRed reds
  f' <- fwdLambda f
  addStm $ Let (pat <> pat_tan) aux $ Op $ Screma size (ScremaForm scs' reds' f') $ interleave xs xs_tan
  where
    fwdScan :: Scan SOACS -> ADM (Scan SOACS)
    fwdScan sc = do
      op' <- fwdLambda $ scanLambda sc
      neutral_tans <- mapM zeroFromSubExp $ scanNeutral sc
      return $
        Scan
          { scanNeutral = scanNeutral sc ++ map Var neutral_tans,
            scanLambda = op'
          }
    fwdRed :: Reduce SOACS -> ADM (Reduce SOACS)
    fwdRed red = do
      op' <- fwdLambda $ redLambda red
      neutral_tans <- mapM zeroFromSubExp $ redNeutral red
      return $
        Reduce
          { redComm = redComm red,
            redLambda = op',
            redNeutral = redNeutral red ++ map Var neutral_tans
          }
fwdSOAC pat aux (Stream size form lam nes xs) = do
  pat_tan <- newTan pat
  lam' <- fwdLambda lam
  xs_tan <- tangent xs
  nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
  let nes' = interleave nes nes_tan
  case form of
    Sequential -> do
      nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
      addStm $ Let (pat <> pat_tan) aux $ Op $ Stream size Sequential lam' nes' $ xs ++ xs_tan
    Parallel o comm lam0 -> do
      nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
      lam0' <- fwdLambda lam0
      let form' = Parallel o comm lam0'
      addStm $ Let (pat <> pat_tan) aux $ Op $ Stream size form' lam' nes' $ xs ++ xs_tan
fwdSOAC pat aux (Hist len ops bucket_fun imgs) = do
  pat_tan <- newTan pat
  ops' <- mapM fwdHist ops
  bucket_fun' <- fwdLambda bucket_fun
  addStm $ Let (pat <> pat_tan) aux $ Op $ Hist len ops' bucket_fun' imgs
  where
    fwdHist :: HistOp SOACS -> ADM (HistOp SOACS)
    fwdHist (HistOp width rf dest nes op) = do
      dest_tan <- mapM tangent dest
      nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
      op' <- fwdLambda op
      return $
        HistOp
          { histWidth = width,
            histRaceFactor = rf,
            histDest = dest ++ dest_tan,
            histNeutral = interleave nes nes_tan,
            histOp = op'
          }
fwdSOAC pat aux scatter@(Scatter {}) = addStm $ Let pat aux $ Op scatter
fwdSOAC _ _ soac = error $ "Unsupported SOAC: " ++ pretty soac

fwdStm :: Stm -> ADM ()
fwdStm stm@(Let (Pattern ctx [(PatElem p t)]) aux (BasicOp (UnAcc acc ts))) = do
  acc_tan <- tangent acc
  throw <- newVName (baseString p <> "_throwaway")
  p' <- tanVName p
  insertTan p p'
  addStm stm
  addStm $ Let (Pattern ctx [PatElem throw t, PatElem p' t]) aux $ BasicOp $ UnAcc acc_tan $ ts <> ts
fwdStm stm@(Let pat aux (BasicOp (UpdateAcc acc i x))) = do
  pat_tan <- newTan pat
  x_tan <- tangent x
  acc_tan <- tangent acc
  addStm stm
  addStm $ Let pat_tan aux $ BasicOp $ UpdateAcc acc_tan i $ interleave x x_tan
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
fwdStm (Let (Pattern ctx pes) aux (DoLoop l_ctx val_pats (WhileLoop v) body)) = do
  let (val_params, vals) = unzip val_pats
  vals_tan <- tangent vals
  pes_tan <- newTan pes
  slocal' $ do
    val_params_tan <- newTan val_params
    let val_pats_tan = zip val_params_tan vals_tan
    body_tan <- fwdBody body
    addStm $
      Let (Pattern ctx (pes ++ pes_tan)) aux $
        DoLoop l_ctx (val_pats ++ val_pats_tan) (WhileLoop v) body_tan
fwdStm (Let (Pattern ctx pes) aux (DoLoop l_ctx val_pats loop@(ForLoop i it bound loop_vars) body)) = do
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
    inScopeOf loop $ do
      body_tan <- fwdBody body
      addStm $
        Let (Pattern ctx (pes ++ pes_tan)) aux $
          DoLoop l_ctx (val_pats ++ val_pats_tan) (ForLoop i it bound (loop_vars ++ loop_vars_tan)) body_tan
fwdStm stm@(Let (Pattern ctx [pe@(PatElem p t)]) aux (MkAcc accshape arrs ishape op)) = do
  case t of
    Array (ElemAcc ts) s u -> do
      arrs_tan <- tangent arrs
      t_tan <- mapM lookupType arrs_tan
      p' <- newVName (baseString p <> "_combined_tan")
      insertTan p p'
      let t' = Array (ElemAcc $ ts <> arrs_tan) s u
          pat' = Pattern ctx [PatElem p' t']
      op' <- case op of
        Nothing -> return Nothing
        Just (lam, nes) -> do
          nes_tan <- mapM (fmap Var . zeroFromSubExp) nes
          lam' <- fwdLambda lam
          case lam' of
            Lambda (i : _ : ps) body ret -> do
              let lam'' = Lambda (i : ps) body ret
              lam''' <- renameLambda lam''
              return $ Just (lam''', interleave nes nes_tan)
            _ -> error "Malformed lambda in MkAcc."
      addStm stm
      addStm $ Let pat' aux $ MkAcc accshape (arrs <> arrs_tan) ishape op'
fwdStm s@(Let pat aux (Op soac)) = fwdSOAC pat aux soac
fwdStm stm =
  error $ "unhandled forward mode AD for Stm: " ++ pretty stm ++ "\n" ++ show stm

fwdBody :: Body -> ADM Body
fwdBody (Body _ stms res) = do
  (res', stms') <- collectStms $ do
    mapM_ fwdStm stms
    mapM tangent res
  return $ mkBody stms' $ interleave res res'

fwdBodyOnlyTangents :: Body -> ADM Body
fwdBodyOnlyTangents (Body _ stms res) = do
  (res', stms') <- collectStms $ do
    mapM_ fwdStm stms
    mapM tangent res
  return $ mkBody stms' res'

fwdJVP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
fwdJVP scope (Lambda params body ret) = do
  runADM . localScope scope $ do
    params_tan <- newTan params
    body' <- fwdBodyOnlyTangents body
    let params' = interleave params params_tan
    ret_tan <- mapM tanType ret
    pure $ Lambda params' body' $ interleave ret ret_tan
