{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Futhark.AD.Derivatives
import Futhark.AD.Rev.Loop
import Futhark.AD.Rev.Monad
import Futhark.AD.Rev.SOAC
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (takeLast)

patName :: Pat Type -> ADM VName
patName (Pat [pe]) = pure $ patElemName pe
patName pat = error $ "Expected single-element pattern: " ++ pretty pat

-- The vast majority of BasicOps require no special treatment in the
-- forward pass and produce one value (and hence one adjoint).  We
-- deal with that case here.
commonBasicOp :: Pat Type -> StmAux () -> BasicOp -> ADM () -> ADM (VName, VName)
commonBasicOp pat aux op m = do
  addStm $ Let pat aux $ BasicOp op
  m
  pat_v <- patName pat
  pat_adj <- lookupAdjVal pat_v
  pure (pat_v, pat_adj)

diffBasicOp :: Pat Type -> StmAux () -> BasicOp -> ADM () -> ADM ()
diffBasicOp pat aux e m =
  case e of
    CmpOp cmp x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        let t = cmpOpType cmp
            update contrib = do
              void $ updateSubExpAdj x contrib
              void $ updateSubExpAdj y contrib

        case t of
          FloatType ft ->
            update <=< letExp "contrib" $
              If
                (Var pat_adj)
                (resultBody [constant (floatValue ft (1 :: Int))])
                (resultBody [constant (floatValue ft (0 :: Int))])
                (IfDec [Prim (FloatType ft)] IfNormal)
          IntType it ->
            update <=< letExp "contrib" $ BasicOp $ ConvOp (BToI it) (Var pat_adj)
          Bool ->
            update pat_adj
          Unit ->
            pure ()
    --
    ConvOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        contrib <-
          letExp "contrib" $ BasicOp $ ConvOp (flipConvOp op) $ Var pat_adj
        updateSubExpAdj x contrib
    --
    UnOp op x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      returnSweepCode $ do
        let t = unOpType op
        contrib <- do
          let x_pe = primExpFromSubExp t x
              pat_adj' = primExpFromSubExp t (Var pat_adj)
              dx = pdUnOp op x_pe
          letExp "contrib" <=< toExp $ pat_adj' ~*~ dx

        updateSubExpAdj x contrib
    --
    BinOp op x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      returnSweepCode $ do
        let t = binOpType op
            (wrt_x, wrt_y) =
              pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

            pat_adj' = primExpFromSubExp t $ Var pat_adj

        adj_x <- letExp "binop_x_adj" <=< toExp $ pat_adj' ~*~ wrt_x
        adj_y <- letExp "binop_y_adj" <=< toExp $ pat_adj' ~*~ wrt_y
        updateSubExpAdj x adj_x
        updateSubExpAdj y adj_y
    --
    SubExp se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ updateSubExpAdj se pat_adj
    --
    Assert {} ->
      void $ commonBasicOp pat aux e m
    --
    ArrayLit elems _ -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      t <- lookupType pat_adj
      returnSweepCode $ do
        forM_ (zip [(0 :: Int64) ..] elems) $ \(i, se) -> do
          let slice = fullSlice t [DimFix (constant i)]
          updateSubExpAdj se <=< letExp "elem_adj" $ BasicOp $ Index pat_adj slice
    --
    Index arr slice -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        void $ updateAdjSlice slice arr pat_adj
    FlatIndex {} -> error "FlatIndex not handled by AD yet."
    FlatUpdate {} -> error "FlatUpdate not handled by AD yet."
    --
    Opaque _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ updateSubExpAdj se pat_adj
    --
    Reshape _ arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        arr_dims <- arrayDims <$> lookupType arr
        void $
          updateAdj arr <=< letExp "adj_reshape" $
            BasicOp $ Reshape (map DimNew arr_dims) pat_adj
    --
    Rearrange perm arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $
        void $
          updateAdj arr <=< letExp "adj_rearrange" $
            BasicOp $ Rearrange (rearrangeInverse perm) pat_adj
    --
    Rotate rots arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        let neg = BasicOp . BinOp (Sub Int64 OverflowWrap) (intConst Int64 0)
        rots' <- mapM (letSubExp "rot_neg" . neg) rots
        void $
          updateAdj arr <=< letExp "adj_rotate" $
            BasicOp $ Rotate rots' pat_adj
    --
    Replicate (Shape ns) x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        x_t <- subExpType x
        lam <- addLambda x_t
        ne <- letSubExp "zero" $ zeroExp x_t
        n <- letSubExp "rep_size" =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ns
        pat_adj_flat <-
          letExp (baseString pat_adj <> "_flat") $
            BasicOp $ Reshape (map DimNew $ n : arrayDims x_t) pat_adj
        reduce <- reduceSOAC [Reduce Commutative lam [ne]]
        updateSubExpAdj x
          =<< letExp "rep_contrib" (Op $ Screma n [pat_adj_flat] reduce)
    --
    Concat d (arr :| arrs) _ -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        let sliceAdj _ [] = pure []
            sliceAdj start (v : vs) = do
              v_t <- lookupType v
              let w = arraySize 0 v_t
                  slice = DimSlice start w (intConst Int64 1)
              pat_adj_slice <-
                letExp (baseString pat_adj <> "_slice") $
                  BasicOp $ Index pat_adj (sliceAt v_t d [slice])
              start' <- letSubExp "start" $ BasicOp $ BinOp (Add Int64 OverflowUndef) start w
              slices <- sliceAdj start' vs
              pure $ pat_adj_slice : slices

        slices <- sliceAdj (intConst Int64 0) $ arr : arrs

        zipWithM_ updateAdj (arr : arrs) slices
    --
    Copy se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ void $ updateAdj se pat_adj
    --
    Manifest _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ void $ updateAdj se pat_adj
    --
    Scratch {} ->
      void $ commonBasicOp pat aux e m
    --
    Iota n _ _ t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        ne <- letSubExp "zero" $ zeroExp $ Prim $ IntType t
        lam <- addLambda $ Prim $ IntType t
        reduce <- reduceSOAC [Reduce Commutative lam [ne]]
        updateSubExpAdj n
          =<< letExp "iota_contrib" (Op $ Screma n [pat_adj] reduce)
    --
    Update safety arr slice v -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        v_adj <- letExp "update_val_adj" $ BasicOp $ Index pat_adj slice
        t <- lookupType v_adj
        v_adj_copy <-
          case t of
            Array {} -> letExp "update_val_adj_copy" $ BasicOp $ Copy v_adj
            _ -> return v_adj
        updateSubExpAdj v v_adj_copy
        zeroes <- letSubExp "update_zero" . zeroExp =<< subExpType v
        void $
          updateAdj arr
            =<< letExp "update_src_adj" (BasicOp $ Update safety pat_adj slice zeroes)
    UpdateAcc {} -> error "Reverse-mode UpdateAcc not handled yet."

vjpOps :: VjpOps
vjpOps = VjpOps diffLambda diffStm

diffStm :: Stm SOACS -> ADM () -> ADM ()
diffStm (Let pat aux (BasicOp e)) m =
  diffBasicOp pat aux e m
diffStm stm@(Let pat _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    m

    pat_adj <- lookupAdjVal =<< patName pat
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
        pat_adj' = primExpFromSubExp ret (Var pat_adj)
        convert ft tt
          | ft == tt = id
        convert (IntType ft) (IntType tt) =
          ConvOpExp (SExt ft tt)
        convert (FloatType ft) (FloatType tt) =
          ConvOpExp (FPConv ft tt)
        convert ft tt =
          error $ "diffStm.convert: " ++ pretty (ft, tt)

    contribs <-
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          forM (zip derivs argts) $ \(deriv, argt) ->
            letExp "contrib" <=< toExp . convert ret argt $ pat_adj' ~*~ deriv

    zipWithM_ updateSubExpAdj (map fst args) contribs
diffStm stm@(Let pat _ (If cond tbody fbody _)) m = do
  addStm stm
  m
  returnSweepCode $ do
    let tbody_free = freeIn tbody
        fbody_free = freeIn fbody
        branches_free = namesToList $ tbody_free <> fbody_free

    adjs <- mapM lookupAdj $ patNames pat

    branches_free_adj <-
      ( pure . takeLast (length branches_free)
          <=< letTupExp "branch_adj"
          <=< renameExp
        )
        =<< eIf
          (eSubExp cond)
          (diffBody adjs branches_free tbody)
          (diffBody adjs branches_free fbody)
    zipWithM_ insAdj branches_free branches_free_adj
diffStm (Let pat aux (Op soac)) m =
  vjpSOAC vjpOps pat aux soac m
diffStm (Let pat aux loop@DoLoop {}) m =
  diffLoop diffStms pat aux loop m
diffStm stm _ = error $ "diffStm unhandled:\n" ++ pretty stm

diffStms :: Stms SOACS -> ADM ()
diffStms all_stms
  | Just (stm, stms) <- stmsHead all_stms = do
    (subst, copy_stms) <- copyConsumedArrsInStm stm
    let (stm', stms') = substituteNames subst (stm, stms)
    diffStms copy_stms >> diffStm stm' (diffStms stms')
    forM_ (M.toList subst) $ \(from, to) ->
      setAdj from =<< lookupAdj to
  | otherwise =
    pure ()

-- | Preprocess statements before differentiating.
-- For now, it's just stripmining.
preprocess :: Stms SOACS -> ADM (Stms SOACS)
preprocess = stripmineStms

diffBody :: [Adj] -> [VName] -> Body SOACS -> ADM (Body SOACS)
diffBody res_adjs get_adjs_for (Body () stms res) = subAD $
  subSubsts $ do
    let onResult (SubExpRes _ (Constant _)) _ = pure ()
        onResult (SubExpRes _ (Var v)) v_adj = void $ updateAdj v =<< adjVal v_adj
    (adjs, stms') <- collectStms $ do
      zipWithM_ onResult (takeLast (length res_adjs) res) res_adjs
      diffStms =<< preprocess stms
      mapM lookupAdjVal get_adjs_for
    pure $ Body () stms' $ res <> varsRes adjs

diffLambda :: [Adj] -> [VName] -> Lambda SOACS -> ADM (Lambda SOACS)
diffLambda res_adjs get_adjs_for (Lambda params body _) =
  localScope (scopeOfLParams params) $ do
    Body () stms res <- diffBody res_adjs get_adjs_for body
    let body' = Body () stms $ takeLast (length get_adjs_for) res
    ts' <- mapM lookupType get_adjs_for
    pure $ Lambda params body' ts'

revVJP :: MonadFreshNames m => Scope SOACS -> Lambda SOACS -> m (Lambda SOACS)
revVJP scope (Lambda params body ts) =
  runADM . localScope (scope <> scopeOfLParams params) $ do
    params_adj <- forM (zip (map resSubExp (bodyResult body)) ts) $ \(se, t) ->
      Param mempty <$> maybe (newVName "const_adj") adjVName (subExpVar se) <*> pure t

    Body () stms res <-
      localScope (scopeOfLParams params_adj) $
        diffBody
          (map adjFromParam params_adj)
          (map paramName params)
          body
    let body' = Body () stms res

    pure $ Lambda (params ++ params_adj) body' (ts <> map paramType params)
