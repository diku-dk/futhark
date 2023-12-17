{-# LANGUAGE TypeFamilies #-}

-- Naming scheme:
--
-- An adjoint-related object for "x" is named "x_adj".  This means
-- both actual adjoints and statements.
--
-- Do not assume "x'" means anything related to derivatives.
module Futhark.AD.Rev (revVJP) where

import Control.Monad
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
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
patName pat = error $ "Expected single-element pattern: " ++ prettyString pat

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
              Match
                [Var pat_adj]
                [Case [Just $ BoolValue True] $ resultBody [constant (floatValue ft (1 :: Int))]]
                (resultBody [constant (floatValue ft (0 :: Int))])
                (MatchDec [Prim (FloatType ft)] MatchNormal)
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
    Reshape k _ arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        arr_shape <- arrayShape <$> lookupType arr
        void $
          updateAdj arr <=< letExp "adj_reshape" . BasicOp $
            Reshape k arr_shape pat_adj
    --
    Rearrange perm arr -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $
        void $
          updateAdj arr <=< letExp "adj_rearrange" . BasicOp $
            Rearrange (rearrangeInverse perm) pat_adj
    --
    Replicate (Shape []) (Var se) -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ void $ updateAdj se pat_adj
    --
    Replicate (Shape ns) x -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        x_t <- subExpType x
        lam <- addLambda x_t
        ne <- letSubExp "zero" $ zeroExp x_t
        n <- letSubExp "rep_size" =<< foldBinOp (Mul Int64 OverflowUndef) (intConst Int64 1) ns
        pat_adj_flat <-
          letExp (baseString pat_adj <> "_flat") . BasicOp $
            Reshape ReshapeArbitrary (Shape $ n : arrayDims x_t) pat_adj
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
                  BasicOp $
                    Index pat_adj (sliceAt v_t d [slice])
              start' <- letSubExp "start" $ BasicOp $ BinOp (Add Int64 OverflowUndef) start w
              slices <- sliceAdj start' vs
              pure $ pat_adj_slice : slices

        slices <- sliceAdj (intConst Int64 0) $ arr : arrs

        zipWithM_ updateAdj (arr : arrs) slices
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
            Array {} ->
              letExp "update_val_adj_copy" . BasicOp $
                Replicate mempty (Var v_adj)
            _ -> pure v_adj
        updateSubExpAdj v v_adj_copy
        zeroes <- letSubExp "update_zero" . zeroExp =<< subExpType v
        void $
          updateAdj arr
            =<< letExp "update_src_adj" (BasicOp $ Update safety pat_adj slice zeroes)
    -- See Note [Adjoints of accumulators]
    UpdateAcc _ is vs -> do
      addStm $ Let pat aux $ BasicOp e
      m
      pat_adjs <- mapM lookupAdjVal (patNames pat)
      returnSweepCode $ do
        forM_ (zip pat_adjs vs) $ \(adj, v) -> do
          adj_i <- letExp "updateacc_val_adj" $ BasicOp $ Index adj $ Slice $ map DimFix is
          updateSubExpAdj v adj_i

vjpOps :: VjpOps
vjpOps =
  VjpOps
    { vjpLambda = diffLambda,
      vjpStm = diffStm
    }

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
          convert (IntType ft) (IntType tt) = ConvOpExp (SExt ft tt)
          convert (FloatType ft) (FloatType tt) = ConvOpExp (FPConv ft tt)
          convert Bool (FloatType tt) = ConvOpExp (BToF tt)
          convert (FloatType ft) Bool = ConvOpExp (FToB ft)
          convert ft tt = error $ "diffStm.convert: " ++ prettyString (f, ft, tt)

      contribs <-
        case pdBuiltin f arg_pes of
          Nothing ->
            error $ "No partial derivative defined for builtin function: " ++ prettyString f
          Just derivs ->
            forM (zip derivs argts) $ \(deriv, argt) ->
              letExp "contrib" <=< toExp . convert ret argt $ pat_adj' ~*~ deriv

      zipWithM_ updateSubExpAdj (map fst args) contribs
diffStm stm@(Let pat _ (Match ses cases defbody _)) m = do
  addStm stm
  m
  returnSweepCode $ do
    let cases_free = map freeIn cases
        defbody_free = freeIn defbody
        branches_free = namesToList $ mconcat $ defbody_free : cases_free

    adjs <- mapM lookupAdj $ patNames pat

    branches_free_adj <-
      ( pure . takeLast (length branches_free)
          <=< letTupExp "branch_adj"
          <=< renameExp
        )
        =<< eMatch
          ses
          (map (fmap $ diffBody adjs branches_free) cases)
          (diffBody adjs branches_free defbody)
    zipWithM_ insAdj branches_free branches_free_adj
diffStm (Let pat aux (Op soac)) m =
  vjpSOAC vjpOps pat aux soac m
diffStm (Let pat aux loop@Loop {}) m =
  diffLoop diffStms pat aux loop m
-- See Note [Adjoints of accumulators]
diffStm stm@(Let pat _aux (WithAcc inputs lam)) m = do
  addStm stm
  m
  returnSweepCode $ do
    adjs <- mapM lookupAdj $ patNames pat
    lam' <- renameLambda lam
    free_vars <- filterM isActive $ namesToList $ freeIn lam'
    free_accs <- filterM (fmap isAcc . lookupType) free_vars
    let free_vars' = free_vars \\ free_accs
    lam'' <- diffLambda' adjs free_vars' lam'
    inputs' <- mapM renameInputLambda inputs
    free_adjs <- letTupExp "with_acc_contrib" $ WithAcc inputs' lam''
    zipWithM_ insAdj (arrs <> free_vars') free_adjs
  where
    arrs = concatMap (\(_, as, _) -> as) inputs
    renameInputLambda (shape, as, Just (f, nes)) = do
      f' <- renameLambda f
      pure (shape, as, Just (f', nes))
    renameInputLambda input = pure input
    diffLambda' res_adjs get_adjs_for (Lambda params ts body) =
      localScope (scopeOfLParams params) $ do
        Body () stms res <- diffBody res_adjs get_adjs_for body
        let body' = Body () stms $ take (length inputs) res <> takeLast (length get_adjs_for) res
        ts' <- mapM lookupType get_adjs_for
        pure $ Lambda params (take (length inputs) ts <> ts') body'
diffStm stm _ = error $ "diffStm unhandled:\n" ++ prettyString stm

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
diffLambda res_adjs get_adjs_for (Lambda params _ body) =
  localScope (scopeOfLParams params) $ do
    Body () stms res <- diffBody res_adjs get_adjs_for body
    let body' = Body () stms $ takeLast (length get_adjs_for) res
    ts' <- mapM lookupType get_adjs_for
    pure $ Lambda params ts' body'

revVJP :: (MonadFreshNames m) => Scope SOACS -> Lambda SOACS -> m (Lambda SOACS)
revVJP scope (Lambda params ts body) =
  runADM . localScope (scope <> scopeOfLParams params) $ do
    params_adj <- forM (zip (map resSubExp (bodyResult body)) ts) $ \(se, t) ->
      Param mempty <$> maybe (newVName "const_adj") adjVName (subExpVar se) <*> pure t

    body' <-
      localScope (scopeOfLParams params_adj) $
        diffBody
          (map adjFromParam params_adj)
          (map paramName params)
          body

    pure $ Lambda (params ++ params_adj) (ts <> map paramType params) body'

-- Note [Adjoints of accumulators]
--
-- The general case of taking adjoints of WithAcc is tricky.  We make
-- some assumptions and lay down a basic design.
--
-- First, we assume that any WithAccs that occur in the program are
-- the result of previous invocations of VJP.  This means we can rely
-- on the operator having a constant adjoint (it's some kind of
-- addition).
--
-- Second, the adjoint of an accumulator is an array of the same type
-- as the underlying array.  For example, the adjoint type of the
-- primal type 'acc(c, [n], {f64})' is '[n]f64'.  In principle the
-- adjoint of 'acc(c, [n], {f64,f32})' should be two arrays of type
-- '[]f64', '[]f32'.  Our current design assumes that adjoints are
-- single variables.  This is fixable.
--
-- # Adjoint of UpdateAcc
--
--   Consider primal code
--
--     update_acc(acc, i, v)
--
--   Interpreted as an imperative statement, this means
--
--     acc[i] ⊕= v
--
--   for some '⊕'.  Normally all the compiler knows of '⊕' is that it
--   is associative and commutative, but because we assume that all
--   accumulators are the result of previous AD transformations, we
--   can assume that '⊕' actually behaves like addition - that is, has
--   unit partial derivatives.  So the return sweep is
--
--     v += acc_adj[i]
--
-- # Adjoint of Map
--
-- Suppose we have primal code
--
--   let acc' =
--     map (...) acc
--
-- where "acc : acc(c, [n], {f64})" and the width of the Map is "w".
-- Our normal transformation for Map input arrays is to similarly map
-- their adjoint, but clearly this doesn't work here because the
-- semantics of mapping an adjoint is an "implicit replicate".  So
-- when generating the return sweep we actually perform that
-- replication:
--
--   map (...) (replicate w acc_adj)
--
-- But what about the contributions to "acc'"?  Those we also have to
-- take special care of.  The result of the map itself is actually a
-- multidimensional array:
--
--   let acc_contribs =
--     map (...) (replicate w acc'_adj)
--
-- which we must then sum to add to the contribution.
--
--   acc_adj += sum(acc_contribs)
--
-- I'm slightly worried about the asymptotics of this, since my
-- intuition of this is that the contributions might be rather sparse.
-- (Maybe completely zero?  If so it will be simplified away
-- entirely.)  Perhaps a better solution is to treat
-- accumulator-inputs in the primal code as we do free variables, and
-- create accumulators for them in the return sweep.
--
-- # Consumption
--
-- A minor problem is that our usual way of handling consumption (Note
-- [Consumption]) is not viable, because accumulators are not
-- copyable.  Fortunately, while the accumulators that are consumed in
-- the forward sweep will also be present in the return sweep given
-- our current translation rules, they will be dead code.  As long as
-- we are careful to run dead code elimination after revVJP, we should
-- be good.
