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
import Control.Monad.State.Strict
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe
import Futhark.AD.Derivatives
import Futhark.AD.Rev.Monad
import Futhark.AD.Rev.SOAC
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.Aliases (consumedInStms)
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (takeLast)

patName :: Pat -> ADM VName
patName (Pat [pe]) = pure $ patElemName pe
patName pat = error $ "Expected single-element pattern: " ++ pretty pat

-- The vast majority of BasicOps require no special treatment in the
-- forward pass and produce one value (and hence once adjoint).  We
-- deal with that case here.
commonBasicOp :: Pat -> StmAux () -> BasicOp -> ADM () -> ADM (VName, VName)
commonBasicOp pat aux op m = do
  addStm $ Let pat aux $ BasicOp op
  m
  pat_v <- patName pat
  pat_adj <- lookupAdjVal pat_v
  pure (pat_v, pat_adj)

diffBasicOp :: Pat -> StmAux () -> BasicOp -> ADM () -> ADM ()
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
    ArrayLit elems t -> do
      (_pat_v, pat_adj) <- commonBasicOp pat aux e m
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
    Concat d arr arrs _ -> do
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

setLoopTape :: VName -> VName -> ADM ()
setLoopTape v vs = modify $ \env ->
  env {stateLoopTape = M.insert v vs $ stateLoopTape env}

lookupLoopTape :: VName -> ADM VName
lookupLoopTape v = do
  maybeVs <- gets $ M.lookup v . stateLoopTape
  case maybeVs of
    Nothing -> error "lookupLoopTape: didn't find stored values"
    Just vs -> pure vs

diffLoop :: Pat -> StmAux () -> ExpT SOACS -> ADM () -> ADM ()
diffLoop
  (Pat pats)
  aux
  loop@( DoLoop
           param_tuples
           form@(ForLoop i _it bound _loop_vars)
           body@(Body () stms res)
         )
  m = do
    loop_acc_pats <- fwdLoop
    m
    revLoop loop_acc_pats
    where
      loop_params = map fst param_tuples
      loop_param_names = map paramName loop_params

      fwdLoop :: ADM [PatElem]
      fwdLoop = do
        bound64 <- asIntS Int64 bound
        (loop_acc_pats, loop_acc_params) <- fmap unzip $
          forM loop_params $ \(Param _ v t) -> do
            acc <- newVName $ baseString v <> "_acc"
            pat_acc <- newVName $ baseString v <> "_acc"
            setLoopTape v pat_acc
            let loop_acc_param = Param mempty acc $ arrayOf t (Shape [bound64]) Unique
                loop_acc_pat = PatElem pat_acc $ arrayOf t (Shape [bound64]) NoUniqueness
            return (loop_acc_pat, loop_acc_param)

        zero_accs <- forM loop_params $ \(Param _ v t) ->
          letSubExp (baseString v <> "_zero_acc")
            =<< eBlank (arrayOf t (Shape [bound64]) NoUniqueness)

        (loop_acc_params_ret, stms') <- collectStms $
          localScope (scopeOfFParams $ loop_acc_params <> loop_params) $
            inScopeOf form $ do
              copy_substs <- copyConsumedArrsInBody body
              addStms stms
              i64 <- asIntS Int64 $ Var i
              forM (zip loop_params loop_acc_params) $ \(Param _ v _, Param _ acc t) -> do
                acc' <-
                  letInPlace "loop_acc" acc (fullSlice (fromDecl t) [DimFix i64]) $
                    substituteNames copy_substs $ BasicOp $ SubExp $ Var v
                return $ Param mempty acc' (fromDecl t)

        let body' = mkBody stms' $ res <> varsRes (map paramName loop_acc_params_ret)
            pat' = Pat $ pats <> loop_acc_pats
            param_tuples' = param_tuples <> zip loop_acc_params zero_accs
        addStm $ Let pat' aux $ DoLoop param_tuples' form body'
        return loop_acc_pats

      revLoop :: [PatElem] -> ADM ()
      revLoop accs = do
        loop' <- renameSomething loop

        case loop' of
          DoLoop param_tuples' form'@(ForLoop i' it' bound' loop_vars') (Body () stms' res') -> do
            let loop_params' = map fst param_tuples'
                loop_param_names' = map paramName loop_params'
                subst_loop_tape v v' = lookupLoopTape v >>= setLoopTape v'
                res_vname (SubExpRes _ (Constant _)) = Nothing
                res_vname (SubExpRes _ (Var v)) = Just v
                loop_res = mapMaybe res_vname res'
                loop_var_arrays = map snd loop_vars'
                loop_var_param_names = map (paramName . fst) loop_vars'
                getVName Constant {} = Nothing
                getVName (Var v) = Just v
                loop_free = (namesToList (freeIn loop') \\ loop_var_arrays) \\ mapMaybe (getVName . snd) param_tuples'

            zipWithM_ subst_loop_tape loop_param_names loop_param_names'

            let build_param_tuples_adj r (PatElem pat _, Param _ _ t) =
                  case res_vname r of
                    Just v -> do
                      pat_adj <- lookupAdjVal pat
                      v_adj <- adjVName v
                      return $ Just (Param mempty v_adj (toDecl (fromDecl t) Unique), Var pat_adj)
                    _ -> return Nothing

            param_tuples_res_adj <- catMaybes <$> zipWithM build_param_tuples_adj res' (zip pats loop_params')

            param_tuples_free_adj <-
              forM loop_free $ \v -> do
                adj_v <- adjVName v
                adj_init <- lookupAdjVal v
                t <- lookupType v
                return (Param mempty adj_v (toDecl t Unique), Var adj_init)

            param_tuples_loop_vars_adj <- forM loop_vars' $ \(_, vs) -> do
              adj_vs <- adjVName vs
              adj_init <- lookupAdjVal vs
              t <- lookupType vs
              return (Param mempty adj_vs (toDecl t Unique), Var adj_init)

            let param_tuples_adj = param_tuples_res_adj <> param_tuples_free_adj <> param_tuples_loop_vars_adj

            bound_minus_one <-
              inScopeOf form $
                let one = Constant $ IntValue $ intValue it' (1 :: Int)
                 in letSubExp "bound_minus_one" $ BasicOp $ BinOp (Sub it' OverflowUndef) bound' one
            loop_var_arrays_substs <- fmap M.fromList $
              inScopeOf form $ do
                forM loop_var_arrays $ \xs -> do
                  xs_t <- lookupType xs
                  xs_rev <-
                    letExp "reverse" $
                      BasicOp $
                        Index xs $
                          fullSlice
                            xs_t
                            [DimSlice bound_minus_one bound' (Constant (IntValue (Int64Value (-1))))]
                  return (xs, xs_rev)

            ((i_reverse, i_reverse64), index_stms) <- collectStms $
              inScopeOf form' $ do
                i_reverse <- letExp "i" $ BasicOp $ BinOp (Sub it' OverflowWrap) bound_minus_one (Var i')
                i_reverse64 <- asIntS Int64 $ Var i_reverse
                return (i_reverse, i_reverse64)

            ((loop_param_adjs, loop_free_adjs, loop_vars_adjs), stms_adj) <- collectStms $
              subAD $
                localScope (scopeOfFParams $ map fst param_tuples_adj) $
                  localScope (scopeOfFParams loop_params') $
                    inScopeOf form' $ do
                      zipWithM_
                        (\(p, _) v -> insAdj v (paramName p))
                        param_tuples_adj
                        (loop_res <> loop_free <> loop_var_arrays)
                      diffStms stms'
                      loop_free_adjs <- mapM lookupAdjVal loop_free
                      loop_param_adjs <- mapM (lookupAdjVal . paramName) loop_params'
                      let f vs v = do
                            vs_t <- lookupType vs
                            v_adj <- lookupAdjVal v
                            updateAdjSlice (fullSlice vs_t [DimFix i_reverse64]) vs v_adj
                      zipWithM_ f loop_var_arrays loop_var_param_names
                      loop_vars_adjs <- mapM lookupAdjVal loop_var_arrays
                      return (loop_param_adjs, loop_free_adjs, loop_vars_adjs)

            let restore v = localScope (scopeOfFParams loop_params') $ do
                  vs <- lookupLoopTape v
                  vs_t <- lookupType vs
                  v' <- letExp "restore" $ BasicOp $ Index vs $ fullSlice vs_t [DimFix i_reverse64]
                  t <- lookupType v
                  let consumed = namesToList $ consumedInStms $ fst $ Alias.analyseStms mempty stms_adj
                  v'' <- case (t, v `elem` consumed) of
                    (Array {}, True) -> letExp "restore_copy" $ BasicOp $ Copy v'
                    _ -> return v'
                  return (v, v'')

            (_, stms_adj') <-
              inScopeOf form' $
                localScope (mconcat $ map scopeOfPatElem accs) $
                  collectStms $ do
                    addStms index_stms
                    substs <- mapM (restore . paramName) loop_params'
                    addStms $ substituteNames (M.insert i' i_reverse $ M.fromList substs) stms_adj

            inScopeOf stms_adj' $
              localScope (scopeOfFParams $ map fst param_tuples_adj) $
                localScope (scopeOfPat $ Pat accs) $ do
                  let body_adj =
                        Body () stms_adj' $ varsRes (loop_param_adjs <> loop_free_adjs <> loop_vars_adjs)
                  adjs' <-
                    letTupExp "loop_adj" $
                      substituteNames loop_var_arrays_substs $
                        DoLoop
                          param_tuples_adj
                          form'
                          body_adj

                  returnSweepCode $ do
                    zipWithM_ insSubExpAdj (map snd param_tuples') $ take (length loop_param_adjs) adjs'
                    zipWithM_ (\v v_adj -> do insAdj v v_adj; (void . lookupAdjVal) v) loop_free $ take (length loop_free_adjs) $ drop (length loop_param_adjs) adjs'
                    zipWithM_ (\v v_adj -> do insAdj v v_adj; void $ lookupAdjVal v) loop_var_arrays $ drop (length loop_param_adjs + length loop_free_adjs) adjs'
          _ -> error "diffLoop: unexpected non-loop expression."
diffLoop _ _ _ _ = error "diffLoop: unexpected non-loop expression."

diffStm :: Stm -> ADM () -> ADM ()
diffStm (Let pat aux (BasicOp e)) m =
  diffBasicOp pat aux e m
diffStm stm@(Let pat _ (Apply f args _ _)) m
  | Just (ret, argts) <- M.lookup f builtInFunctions = do
    addStm stm
    m

    pat_adj <- lookupAdjVal =<< patName pat
    let arg_pes = zipWith primExpFromSubExp argts (map fst args)
        pat_adj' = primExpFromSubExp ret (Var pat_adj)

    contribs <-
      case pdBuiltin f arg_pes of
        Nothing ->
          error $ "No partial derivative defined for builtin function: " ++ pretty f
        Just derivs ->
          mapM (letExp "contrib" <=< toExp . (pat_adj' ~*~)) derivs

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
  diffLoop pat aux loop m
diffStm stm _ = error $ "diffStm unhandled:\n" ++ pretty stm

diffStms :: Stms SOACS -> ADM ()
diffStms all_stms
  | Just (stm, stms) <- stmsHead all_stms = do
    (subst, copy_stms) <- copyConsumedArrsInStm stm
    let (stm', stms') = substituteNames subst (stm, stms)
    diffStms copy_stms >> diffStm stm' (diffStms stms')
  | otherwise =
    pure ()

-- | Create copies of all arrays consumed in the given statement, and
-- return statements which include copies of the consumed arrays.
--
-- See Note [Consumption].
copyConsumedArrsInStm :: Stm -> ADM (Substitutions, Stms SOACS)
copyConsumedArrsInStm s = inScopeOf s $ collectStms $ copyConsumedArrsInStm' s
  where
    copyConsumedArrsInStm' stm =
      let onConsumed v = inScopeOf s $ do
            v_t <- lookupType v
            case v_t of
              Acc {} -> error $ "copyConsumedArrsInStms: Acc " <> pretty v
              Array {} -> do
                v' <- letExp (baseString v <> "_ad_copy") (BasicOp $ Copy v)
                addSubstitution v' v
                return [(v, v')]
              _ -> return mempty
       in M.fromList . mconcat
            <$> mapM onConsumed (namesToList $ consumedInStms $ fst (Alias.analyseStms mempty (oneStm stm)))

copyConsumedArrsInBody :: Body -> ADM Substitutions
copyConsumedArrsInBody b =
  mconcat <$> mapM onConsumed (namesToList $ consumedInBody (Alias.analyseBody mempty b))
  where
    onConsumed v = do
      v_t <- lookupType v
      case v_t of
        Acc {} -> error $ "copyConsumedArrs: Acc " <> pretty v
        Array {} -> M.singleton v <$> letExp (baseString v <> "_ad_copy") (BasicOp $ Copy v)
        _ -> pure mempty

diffBody :: [Adj] -> [VName] -> Body -> ADM Body
diffBody res_adjs get_adjs_for (Body () stms res) = subAD $
  subSubsts $ do
    let onResult (SubExpRes _ (Constant _)) _ = pure ()
        onResult (SubExpRes _ (Var v)) v_adj = void $ updateAdj v =<< adjVal v_adj
    (adjs, stms') <- collectStms $ do
      zipWithM_ onResult (takeLast (length res_adjs) res) res_adjs
      diffStms stms
      mapM lookupAdjVal get_adjs_for
    pure $ Body () stms' $ res <> varsRes adjs

diffLambda :: [Adj] -> [VName] -> Lambda -> ADM Lambda
diffLambda res_adjs get_adjs_for (Lambda params body _) =
  localScope (scopeOfLParams params) $ do
    Body () stms res <- diffBody res_adjs get_adjs_for body
    let body' = Body () stms $ takeLast (length get_adjs_for) res
    ts' <- mapM lookupType get_adjs_for
    pure $ Lambda params body' ts'

revVJP :: MonadFreshNames m => Scope SOACS -> Lambda -> m Lambda
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

-- Note [Consumption]
--
-- Parts of this transformation depends on duplicating computation.
-- This is a problem when a primal expression consumes arrays (via
-- e.g. Update).  For example, consider how we handle this conditional:
--
--   if b then ys with [0] = 0 else ys
--
-- This consumes the array 'ys', which means that when we later
-- generate code for the return sweep, we can no longer use 'ys'.
-- This is a problem, because when we call 'diffBody' on the branch
-- bodies, we'll keep the primal code (maybe it'll be removed by
-- simplification later - we cannot know).  A similar issue occurs for
-- SOACs.  Our solution is to make copies of all consumes arrays:
--
--  let ys_copy = copy ys
--
-- Then we generate code for the return sweep as normal, but replace
-- _every instance_ of 'ys' in the generated code with 'ys_copy'.
-- This works because Futhark does not have *semantic* in-place
-- updates - any uniqueness violation can be replaced with copies (on
-- arrays, anyway).
--
-- If we are lucky, the uses of 'ys_copy' will be removed by
-- simplification, and there will be no overhead.  But even if not,
-- this is still (asymptotically) efficient because the array that is
-- being consumed must in any case have been produced within the code
-- that we are differentiating, so a copy is at most a scalar
-- overhead.  This is _not_ the case when loops are involved.
--
-- Also, the above only works for arrays, not accumulator variables.
-- Those will need some other mechanism.
