{-# LANGUAGE TypeFamilies #-}

-- Naming scheme:
--
-- An adjoint-related object for "x" is named "x_adj".  This means
-- both actual adjoints and statements.
--
-- Do not assume "x'" means anything related to derivatives.
module Futhark.AD.Rev (revVJP) where

import Control.Monad
import Control.Monad.Identity
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Tuple
import Futhark.AD.Derivatives
import Futhark.AD.Rev.Loop
import Futhark.AD.Rev.Monad
import Futhark.AD.Rev.SOAC
import Futhark.AD.Shared
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, takeLast)

patName :: Pat Type -> ADM VName
patName (Pat [pe]) = pure $ patElemName pe
patName pat = error $ "Expected single-element pattern: " ++ prettyString pat

copyIfArray :: VName -> ADM VName
copyIfArray v = do
  v_t <- lookupType v
  case v_t of
    Array {} ->
      letExp (baseName v <> "_copy") . BasicOp $ Replicate mempty (Var v)
    _ -> pure v

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
    CmpOp {} ->
      void $ commonBasicOp pat aux e m
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

        adj_shape <- askShape

        contrib <- letExp "unop_contrib" <=< mapNest adj_shape (MkSolo (Var pat_adj)) $
          \(MkSolo pat_adj') ->
            toExp $ primExpFromSubExp t pat_adj' ~*~ pdUnOp op (primExpFromSubExp t x)

        updateSubExpAdj x contrib
    --
    BinOp op x y -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m

      returnSweepCode $ do
        let t = binOpType op
            (wrt_x, wrt_y) =
              pdBinOp op (primExpFromSubExp t x) (primExpFromSubExp t y)

        adj_shape <- askShape

        adj_x <- letExp "binop_x_adj"
          <=< mapNest adj_shape (MkSolo (Var pat_adj))
          $ \(MkSolo pat_adj') ->
            let pat_adj'' = primExpFromSubExp t pat_adj'
             in toExp $ pat_adj'' ~*~ wrt_x

        adj_y <- letExp "binop_y_adj"
          <=< mapNest adj_shape (MkSolo (Var pat_adj))
          $ \(MkSolo pat_adj') ->
            let pat_adj'' = primExpFromSubExp t pat_adj'
             in toExp $ pat_adj'' ~*~ wrt_y

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
    ArrayVal {} ->
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
      returnSweepCode $ void $ updateAdjSlice slice arr pat_adj
    FlatIndex {} -> error "FlatIndex not handled by AD yet."
    FlatUpdate {} -> error "FlatUpdate not handled by AD yet."
    --
    Opaque _ se -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ updateSubExpAdj se pat_adj
    --
    Reshape arr newshape -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      returnSweepCode $ do
        arr_shape <- arrayShape <$> lookupType arr
        void $
          updateAdj arr <=< letExp "adj_reshape" . BasicOp $
            Reshape pat_adj (reshapeAll (newShape newshape) arr_shape)
    --
    Rearrange arr perm -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
      r <- shapeRank <$> askShape
      returnSweepCode $
        void . updateAdj arr <=< letExp "adj_rearrange" . BasicOp $
          Rearrange pat_adj ([0 .. r - 1] <> map (+ r) (rearrangeInverse perm))
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
          letExp (baseName pat_adj <> "_flat") . BasicOp $
            Reshape pat_adj (reshapeAll (Shape ns) (Shape $ n : arrayDims x_t))
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
                letExp (baseName pat_adj <> "_slice") $
                  BasicOp $
                    Index pat_adj (sliceAt v_t d [slice])
              start' <- letSubExp "start" $ BasicOp $ BinOp (Add Int64 OverflowUndef) start w
              slices <- sliceAdj start' vs
              pure $ pat_adj_slice : slices

        slices <- sliceAdj (intConst Int64 0) $ arr : arrs

        zipWithM_ updateAdj (arr : arrs) slices
    --
    Manifest se _ -> do
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
        v_adj_copy <- copyIfArray v_adj
        updateSubExpAdj v v_adj_copy
        zeroes <- letSubExp "update_zero" . zeroExp =<< subExpType v
        void $
          updateAdj arr
            =<< letExp "update_src_adj" (BasicOp $ Update safety pat_adj slice zeroes)
    -- See Note [Adjoints of accumulators]
    UpdateAcc _ _ is vs -> do
      addStm $ Let pat aux $ BasicOp e
      m
      pat_adjs <- mapM lookupAdjVal (patNames pat)
      returnSweepCode $ do
        forM_ (zip pat_adjs vs) $ \(adj, v) -> do
          adj_t <- lookupType adj
          adj_i <- letExp "updateacc_val_adj" $ BasicOp $ Index adj $ fullSlice adj_t $ map DimFix is
          updateSubExpAdj v adj_i

vjpOps :: VjpOps
vjpOps =
  VjpOps
    { vjpLambda = diffLambda,
      vjpStm = diffStm
    }

-- | Transform updates on accumulators matching the given certificates into
-- updates that write provided zero values.
zeroOutUpdates :: [(VName, [SubExp])] -> Lambda SOACS -> Lambda SOACS
zeroOutUpdates certs_to_zeroes lam = lam {lambdaBody = onBody $ lambdaBody lam}
  where
    onExp = runIdentity . mapExpM mapper
      where
        mapper =
          (identityMapper :: (Monad m) => Mapper SOACS SOACS m)
            { mapOnOp = traverseSOACStms (\_ stms -> pure $ onStms stms),
              mapOnBody = \_ body -> pure $ onBody body
            }
    onStms = fmap onStm
    onStm (Let (Pat [pe]) aux (BasicOp (UpdateAcc safety acc is _)))
      | Acc c _ _ _ <- patElemType pe,
        Just zero <- lookup c certs_to_zeroes =
          Let (Pat [pe]) aux (BasicOp (UpdateAcc safety acc is zero))
    onStm (Let pat aux e) = Let pat aux $ onExp e

    onBody body = body {bodyStms = onStms $ bodyStms body}

diffStm :: Stm SOACS -> ADM () -> ADM ()
diffStm (Let pat aux (BasicOp e)) m =
  diffBasicOp pat aux e m
diffStm stm@(Let pat _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
      addStm stm
      m

      pat_adj <- lookupAdjVal =<< patName pat
      let arg_pes = zipWith primExpFromSubExp argts (map fst args)
          convert ft tt
            | ft == tt = id
          convert (IntType ft) (IntType tt) = ConvOpExp (SExt ft tt)
          convert (FloatType ft) (FloatType tt) = ConvOpExp (FPConv ft tt)
          convert Bool (FloatType tt) = ConvOpExp (BToF tt)
          convert (FloatType ft) Bool = ConvOpExp (FToB ft)
          convert ft tt = error $ "diffStm.convert: " ++ prettyString (f, ft, tt)

      adj_shape <- askShape

      contribs <-
        case pdBuiltin f arg_pes of
          Nothing ->
            error $ "No partial derivative defined for builtin function: " ++ prettyString f
          Just derivs ->
            forM (zip derivs argts) $ \(deriv, argt) ->
              letExp "apply_contrib" <=< mapNest adj_shape (MkSolo (Var pat_adj)) $
                \(MkSolo pat_adj') ->
                  toExp $ convert ret argt $ primExpFromSubExp ret pat_adj' ~*~ deriv

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
    -- See Note [Array Adjoints of Match]
    forM_ (zip branches_free branches_free_adj) $ \(v, v_adj) ->
      insAdj v =<< copyIfArray v_adj
diffStm (Let pat aux (Op soac)) m =
  -- We add the attributes from 'aux' to every SOAC (but only SOAC) produced. We
  -- could do this on *every* stm, but it would be very verbose.
  censorStms (fmap addAttrs) $ vjpSOAC vjpOps pat aux soac m
  where
    addAttrs stm
      | Op _ <- stmExp stm =
          attribute (stmAuxAttrs aux) stm
      | otherwise = stm
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
    (inputs_zeroes, inputs') <-
      unzip <$> zipWithM renameInputLambda (chunks lengths adjs) inputs
    let certs = map paramName $ take (length inputs) $ lambdaParams lam''
    free_adjs <- letTupExp "with_acc_contrib" $ WithAcc inputs' $ zeroOutUpdates (zip certs inputs_zeroes) lam''
    zipWithM_ insAdj (arrs <> free_vars') free_adjs
  where
    lengths = map (\(_, as, _) -> length as) inputs
    arrs = concatMap (\(_, as, _) -> as) inputs
    renameInputLambda as_adj (shape, as, _) = do
      nes_ts <- mapM (fmap (stripArray (shapeRank shape)) . lookupType) as
      zeroes <- mapM (zeroArray mempty) nes_ts
      as' <- mapM adjVal as_adj
      pure (map Var zeroes, (shape, as', Nothing))
    diffLambda' res_adjs get_adjs_for (Lambda params ts body) = do
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
  mkLambda params $ do
    res <- bodyBind =<< diffBody res_adjs get_adjs_for body
    pure $ takeLast (length get_adjs_for) res

revVJP :: (MonadFreshNames m) => Scope SOACS -> Shape -> Lambda SOACS -> m (Lambda SOACS)
revVJP scope shape (Lambda params ts body) = do
  runADM shape . localScope (scope <> scopeOfLParams params) $ do
    adj_shape <- askShape
    params_adj <- forM (zip (map resSubExp (bodyResult body)) ts) $ \(se, t) ->
      Param mempty
        <$> maybe (newVName "const_res_adj") adjVName (subExpVar se)
        <*> pure (t `arrayOfShape` adj_shape)

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
-- come from one of these sources:
--
-- - A previous instance of VJP, which means we can rely on the operator having
--   a constant adjoint (it's addition as appropriate to the type).
--
-- - A scatter, meaning there is no operator.
--
-- (These can actually be distinguished by the presence of an operator, although
-- we do not currently bother.)
--
-- Second, the adjoint of an accumulator is an array of the same type
-- as the underlying array.  For example, the adjoint type of the
-- primal type 'acc(c, [n], {f64})' is '[n]f64'.  In principle the
-- adjoint of 'acc(c, [n], {f64,f32})' should be two arrays of type
-- '[]f64', '[]f32'.  Our current design assumes that adjoints are
-- single variables.  This is fixable.
--
-- In the return sweep, when inserting the with_acc, we still compute the
-- "original" accumulator result, but modified such that its initial value is
-- the adjoint of the result of the accumulator. We also modify the update_accs
-- of these accumulators to be with zero values. This means that the array that
-- is produced will be equal to the adjoint of the result, except for those
-- places that have been updated, where it will be zero. This is intuitively
-- sensible - values that have been overwritten (and so do not contribute to the
-- result) should obviously have zero sensitivity.
--
-- # Adjoint of UpdateAcc
--
-- Consider primal code
--
--     update_acc(acc, i, v)
--
-- Interpreted as an imperative statement, this means
--
--     acc[i] ⊕= v
--
-- for some '⊕'.  Normally all the compiler knows of '⊕' is that it
-- is associative and commutative, but because we assume that all
-- accumulators are the result of previous AD transformations, we
-- can assume that '⊕' actually behaves like addition - that is, has
-- unit partial derivatives.  So the return sweep is
--
--     v_adj += acc_adj[i]
--
-- Further, we modify the primal code so that it becomes
--
--     update_acc(acc, i, 0)
--
-- for some appropriate notion of zero.
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

-- Note [Array Adjoints of Match]
--
-- Some unusual, but sadly not completely contrived, contain Match
-- expressions that return multiple arrays, and there the arrays
-- returned by one branch have overlapping aliases with another
-- branch, although in different places. As an example consider this:
--
--   let (X,Y) = if c
--               then (A, B)
--               else (B, A)
--
-- Because our aliasing representation cannot express mutually
-- exclusive aliases, we will consider X and Y to be aliased to each
-- other. In practice, this means it is unlikely for X or Y to be
-- consumed, because it would also consume the other (although it's
-- possible for carefully written code).
--
-- When producing adjoints for this, it will be something like
--
--   let (X_adj,Y_adj) = if c
--                       then (A_adj, B_adj)
--                       else (B_adj, A_adj)
--
-- which completely reflects the primal code. However, while it is
-- unlikely that any consumption takes place for the original primal
-- variables, it is almost guaranteed that X_adj and Y_adj will be
-- consumed (that is the main way we use adjoints after all), and due
-- to the conservative aliasing, when one is consumed, so is the
-- other! To avoid this tragic fate, we are forced to copy any
-- array-typed adjoints returned by a Match. This can be quite costly.
-- However:
--
-- 1) Futhark has pretty OK copy removal, so maybe it can get rid of
--    these by using information not available to the AD pass.
--
-- 2) In many cases, arrays will have accumulator adjoints, which are
--    not subject to this problem.
--
-- Issue #2228 was caused by neglecting to do this.
