{-# LANGUAGE TypeFamilies #-}

-- | The simplification engine is only willing to hoist allocations
-- out of loops if the memory block resulting from the allocation is
-- dead at the end of the loop.  If it is not, we may cause data
-- hazards.
--
-- This pass tries to rewrite loops with memory parameters.
-- Specifically, it takes loops of this form:
--
-- @
-- loop {..., A_mem, ..., A, ...} ... do {
--   ...
--   let A_out_mem = alloc(...) -- stores A_out
--   in {..., A_out_mem, ..., A_out, ...}
-- }
-- @
--
-- and turns them into
--
-- @
-- let A_in_mem = alloc(...)
-- let A_out_mem = alloc(...)
-- let A_in = copy A -- in A_in_mem
-- loop {..., A_in_mem, A_out_mem, ..., A=A_in, ...} ... do {
--   ...
--   in {..., A_out_mem, A_mem, ..., A_out, ...}
-- }
-- @
--
-- The result is essentially "pointer swapping" between the two memory
-- initial blocks @A_mem@ and @A_out_mem@.  The invariant is that the
-- array is always stored in the "first" memory block at the beginning
-- of the loop (and also in the final result).  We do need to add an
-- extra element to the pattern, however.  The initial copy of @A@
-- could be elided if @A@ is unique (thus @A_in_mem=A_mem@).  This is
-- because only then is it safe to use @A_mem@ to store loop results.
-- We don't currently do this.
--
-- Unfortunately, not all loops fit the pattern above.  In particular,
-- a nested loop that has been transformed as such does not!
-- Therefore we also have another double buffering strategy, that
-- turns
--
-- @
-- loop {..., A_mem, ..., A, ...} ... do {
--   ...
--   let A_out_mem = alloc(...)
--   -- A in A_out_mem
--   in {..., A_out_mem, ..., A, ...}
-- }
-- @
--
-- into
--
-- @
-- let A_res_mem = alloc(...)
-- loop {..., A_mem, ..., A, ...} ... do {
--   ...
--   let A_out_mem = alloc(...)
--   -- A in A_out_mem
--   let A' = copy A
--   -- A' in A_res_mem
--   in {..., A_res_mem, ..., A, ...}
-- }
-- @
--
-- The allocation of A_out_mem can then be hoisted out because it is
-- dead at the end of the loop.  This always works as long as
-- A_out_mem has a loop-invariant allocation size, but requires a copy
-- per iteration (and an initial one, elided above).
module Futhark.Optimise.DoubleBuffer (doubleBufferGPU, doubleBufferMC) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Construct
import Futhark.IR.GPUMem as GPU
import Futhark.IR.MCMem as MC
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (arraySizeInBytesExp)
import Futhark.Pass.ExplicitAllocations.GPU ()
import Futhark.Transform.Substitute
import Futhark.Util (mapAccumLM, maybeHead)

-- | The double buffering pass definition.
doubleBuffer :: (Mem rep inner) => String -> String -> OptimiseOp rep -> Pass rep rep
doubleBuffer name desc onOp =
  Pass
    { passName = name,
      passDescription = desc,
      passFunction = intraproceduralTransformation optimise
    }
  where
    optimise scope stms = modifyNameSource $ \src ->
      let m =
            runDoubleBufferM $ localScope scope $ optimiseStms $ stmsToList stms
       in runState (runReaderT m env) src

    env = Env mempty doNotTouchLoop onOp
    doNotTouchLoop pat merge body = pure (mempty, pat, merge, body)

-- | The pass for GPU kernels.
doubleBufferGPU :: Pass GPUMem GPUMem
doubleBufferGPU =
  doubleBuffer
    "Double buffer GPU"
    "Double buffer memory in sequential loops (GPU rep)."
    optimiseGPUOp

-- | The pass for multicore
doubleBufferMC :: Pass MCMem MCMem
doubleBufferMC =
  doubleBuffer
    "Double buffer MC"
    "Double buffer memory in sequential loops (MC rep)."
    optimiseMCOp

type OptimiseLoop rep =
  Pat (LetDec rep) ->
  [(FParam rep, SubExp)] ->
  Body rep ->
  DoubleBufferM
    rep
    ( Stms rep,
      Pat (LetDec rep),
      [(FParam rep, SubExp)],
      Body rep
    )

type OptimiseOp rep =
  Op rep -> DoubleBufferM rep (Op rep)

data Env rep = Env
  { envScope :: Scope rep,
    envOptimiseLoop :: OptimiseLoop rep,
    envOptimiseOp :: OptimiseOp rep
  }

newtype DoubleBufferM rep a = DoubleBufferM
  { runDoubleBufferM :: ReaderT (Env rep) (State VNameSource) a
  }
  deriving (Functor, Applicative, Monad, MonadReader (Env rep), MonadFreshNames)

instance (ASTRep rep) => HasScope rep (DoubleBufferM rep) where
  askScope = asks envScope

instance (ASTRep rep) => LocalScope rep (DoubleBufferM rep) where
  localScope scope = local $ \env -> env {envScope = envScope env <> scope}

optimiseBody :: (ASTRep rep) => Body rep -> DoubleBufferM rep (Body rep)
optimiseBody body = do
  stms' <- optimiseStms $ stmsToList $ bodyStms body
  pure $ body {bodyStms = stms'}

optimiseStms :: (ASTRep rep) => [Stm rep] -> DoubleBufferM rep (Stms rep)
optimiseStms [] = pure mempty
optimiseStms (e : es) = do
  e_es <- optimiseStm e
  es' <- localScope (castScope $ scopeOf e_es) $ optimiseStms es
  pure $ e_es <> es'

optimiseStm :: forall rep. (ASTRep rep) => Stm rep -> DoubleBufferM rep (Stms rep)
optimiseStm (Let pat aux (Loop merge form body)) = do
  body' <-
    localScope (scopeOfLoopForm form <> scopeOfFParams (map fst merge)) $
      optimiseBody body
  opt_loop <- asks envOptimiseLoop
  (stms, pat', merge', body'') <- opt_loop pat merge body'
  pure $ stms <> oneStm (Let pat' aux $ Loop merge' form body'')
optimiseStm (Let pat aux e) = do
  onOp <- asks envOptimiseOp
  oneStm . Let pat aux <$> mapExpM (optimise onOp) e
  where
    optimise onOp =
      (identityMapper @rep)
        { mapOnBody = \_ x ->
            optimiseBody x :: DoubleBufferM rep (Body rep),
          mapOnOp = onOp
        }

optimiseGPUOp :: OptimiseOp GPUMem
optimiseGPUOp (Inner (SegOp op)) =
  local inSegOp $ Inner . SegOp <$> mapSegOpM mapper op
  where
    mapper =
      identitySegOpMapper
        { mapOnSegOpLambda = optimiseLambda,
          mapOnSegOpBody = optimiseKernelBody
        }
    inSegOp env = env {envOptimiseLoop = optimiseLoop}
optimiseGPUOp op = pure op

optimiseMCOp :: OptimiseOp MCMem
optimiseMCOp (Inner (ParOp par_op op)) =
  local inSegOp $
    Inner
      <$> (ParOp <$> traverse (mapSegOpM mapper) par_op <*> mapSegOpM mapper op)
  where
    mapper =
      identitySegOpMapper
        { mapOnSegOpLambda = optimiseLambda,
          mapOnSegOpBody = optimiseKernelBody
        }
    inSegOp env = env {envOptimiseLoop = optimiseLoop}
optimiseMCOp op = pure op

optimiseKernelBody ::
  (ASTRep rep) =>
  KernelBody rep ->
  DoubleBufferM rep (KernelBody rep)
optimiseKernelBody kbody = do
  stms' <- optimiseStms $ stmsToList $ kernelBodyStms kbody
  pure $ kbody {kernelBodyStms = stms'}

optimiseLambda ::
  (ASTRep rep) =>
  Lambda rep ->
  DoubleBufferM rep (Lambda rep)
optimiseLambda lam = do
  body <- localScope (castScope $ scopeOf lam) $ optimiseBody $ lambdaBody lam
  pure lam {lambdaBody = body}

type Constraints rep inner =
  ( Mem rep inner,
    BuilderOps rep,
    ExpDec rep ~ (),
    BodyDec rep ~ (),
    LetDec rep ~ LetDecMem
  )

extractAllocOf :: (Constraints rep inner) => Names -> VName -> Stms rep -> Maybe (Stm rep, Stms rep)
extractAllocOf bound needle stms = do
  (stm, stms') <- stmsHead stms
  case stm of
    Let (Pat [pe]) _ (Op (Alloc size _))
      | patElemName pe == needle,
        invariant size ->
          Just (stm, stms')
    _ ->
      let bound' = namesFromList (patNames (stmPat stm)) <> bound
       in second (oneStm stm <>) <$> extractAllocOf bound' needle stms'
  where
    invariant Constant {} = True
    invariant (Var v) = v `notNameIn` bound

optimiseLoop :: (Constraints rep inner) => OptimiseLoop rep
optimiseLoop pat merge body = do
  (outer_stms_1, pat', merge', body') <-
    optimiseLoopBySwitching pat merge body
  (outer_stms_2, pat'', merge'', body'') <-
    inScopeOf outer_stms_1 $ optimiseLoopByCopying pat' merge' body'
  pure (outer_stms_1 <> outer_stms_2, pat'', merge'', body'')

isArrayIn :: VName -> Param FParamMem -> Bool
isArrayIn x (Param _ _ (MemArray _ _ _ (ArrayIn y _))) = x == y
isArrayIn _ _ = False

optimiseLoopBySwitching :: (Constraints rep inner) => OptimiseLoop rep
optimiseLoopBySwitching (Pat pes) merge (Body _ body_stms body_res) = do
  ((pat', merge', body'), outer_stms) <- runBuilder $ do
    ((buffered, body_stms'), (pes', merge', body_res')) <-
      second unzip3 <$> mapAccumLM check (mempty, body_stms) (zip3 pes merge body_res)
    merge'' <- mapM (maybeCopyInitial buffered) $ mconcat merge'
    pure (Pat $ mconcat pes', merge'', Body () body_stms' $ mconcat body_res')
  pure (outer_stms, pat', merge', body')
  where
    merge_bound = namesFromList $ map (paramName . fst) merge

    check (buffered, body_stms') (pe, (param, arg), res)
      | Mem space <- paramType param,
        Var arg_v <- arg,
        -- XXX: what happens if there are multiple arrays in the same
        -- memory block?
        [arr_param] <- filter (isArrayIn (paramName param)) $ map fst merge,
        MemArray pt _ _ (ArrayIn _ ixfun) <- paramDec arr_param,
        not $ merge_bound `namesIntersect` freeIn ixfun,
        Var res_v <- resSubExp res,
        Just (res_v_alloc, body_stms'') <- extractAllocOf merge_bound res_v body_stms' = do
          num_bytes <-
            letSubExp "num_bytes" =<< toExp (primByteSize pt * (1 + IxFun.range ixfun))
          arr_mem_in <-
            letExp (baseString arg_v <> "_in") $ Op $ Alloc num_bytes space
          pe_unused <-
            PatElem
              <$> newVName (baseString (patElemName pe) <> "_unused")
              <*> pure (MemMem space)
          param_out <-
            newParam (baseString (paramName param) <> "_out") (MemMem space)
          addStm res_v_alloc
          pure
            ( ( M.insert (paramName param) arr_mem_in buffered,
                substituteNames (M.singleton res_v (paramName param_out)) body_stms''
              ),
              ( [pe, pe_unused],
                [(param, Var arr_mem_in), (param_out, resSubExp res)],
                [ res {resSubExp = Var $ paramName param_out},
                  subExpRes $ Var $ paramName param
                ]
              )
            )
      | otherwise =
          pure
            ( (buffered, body_stms'),
              ([pe], [(param, arg)], [res])
            )

    maybeCopyInitial buffered (param@(Param _ _ (MemArray _ _ _ (ArrayIn mem _))), Var arg)
      | Just mem' <- mem `M.lookup` buffered = do
          arg_info <- lookupMemInfo arg
          case arg_info of
            MemArray pt shape u (ArrayIn _ arg_ixfun) -> do
              arg_copy <- newVName (baseString arg <> "_dbcopy")
              letBind (Pat [PatElem arg_copy $ MemArray pt shape u $ ArrayIn mem' arg_ixfun]) $
                BasicOp (Replicate mempty $ Var arg)
              -- We need to make this parameter unique to avoid invalid
              -- hoisting (see #1533), because we are invalidating the
              -- underlying memory.
              pure (fmap mkUnique param, Var arg_copy)
            _ -> pure (fmap mkUnique param, Var arg)
    maybeCopyInitial _ (param, arg) = pure (param, arg)

    mkUnique (MemArray bt shape _ ret) = MemArray bt shape Unique ret
    mkUnique x = x

optimiseLoopByCopying :: (Constraints rep inner) => OptimiseLoop rep
optimiseLoopByCopying pat merge body = do
  -- We start out by figuring out which of the merge variables should
  -- be double-buffered.
  buffered <-
    doubleBufferLoopParams
      (zip (map fst merge) (bodyResult body))
      (boundInBody body)
  -- Then create the allocations of the buffers and copies of the
  -- initial values.
  (merge', allocs) <- allocStms merge buffered
  -- Modify the loop body to copy buffered result arrays.
  let body' = doubleBufferResult (map fst merge) buffered body
  pure (stmsFromList allocs, pat, merge', body')

-- | The booleans indicate whether we should also play with the
-- initial merge values.
data DoubleBuffer
  = BufferAlloc VName (PrimExp VName) Space Bool
  | -- | First name is the memory block to copy to,
    -- second is the name of the array copy.
    BufferCopy VName IxFun VName Bool
  | NoBuffer
  deriving (Show)

doubleBufferLoopParams ::
  (MonadFreshNames m) =>
  [(Param FParamMem, SubExpRes)] ->
  Names ->
  m [DoubleBuffer]
doubleBufferLoopParams ctx_and_res bound_in_loop =
  evalStateT (mapM buffer ctx_and_res) M.empty
  where
    params = map fst ctx_and_res
    loopVariant v =
      v
        `nameIn` bound_in_loop
        || v
          `elem` map (paramName . fst) ctx_and_res

    loopInvariantSize (Constant v) =
      Just (Constant v, True)
    loopInvariantSize (Var v) =
      case find ((== v) . paramName . fst) ctx_and_res of
        Just (_, SubExpRes _ (Constant val)) ->
          Just (Constant val, False)
        Just (_, SubExpRes _ (Var v'))
          | not $ loopVariant v' ->
              Just (Var v', False)
        Just _ ->
          Nothing
        Nothing ->
          Just (Var v, True)

    sizeForMem mem = maybeHead $ mapMaybe (arrayInMem . paramDec) params
      where
        arrayInMem (MemArray pt shape _ (ArrayIn arraymem ixfun))
          | IxFun.isDirect ixfun,
            Just (dims, b) <-
              mapAndUnzipM loopInvariantSize $ shapeDims shape,
            mem == arraymem =
              Just
                ( arraySizeInBytesExp $
                    Array pt (Shape dims) NoUniqueness,
                  or b
                )
        arrayInMem _ = Nothing

    buffer (fparam, res) = case paramType fparam of
      Mem space
        | Just (size, b) <- sizeForMem $ paramName fparam,
          Var res_v <- resSubExp res,
          res_v `nameIn` bound_in_loop -> do
            -- Let us double buffer this!
            bufname <- lift $ newVName "double_buffer_mem"
            modify $ M.insert (paramName fparam) (bufname, b)
            pure $ BufferAlloc bufname size space b
      Array {}
        | MemArray _ _ _ (ArrayIn mem ixfun) <- paramDec fparam -> do
            buffered <- gets $ M.lookup mem
            case buffered of
              Just (bufname, b) -> do
                copyname <- lift $ newVName "double_buffer_array"
                pure $ BufferCopy bufname ixfun copyname b
              Nothing ->
                pure NoBuffer
      _ -> pure NoBuffer

allocStms ::
  (Constraints rep inner) =>
  [(FParam rep, SubExp)] ->
  [DoubleBuffer] ->
  DoubleBufferM rep ([(FParam rep, SubExp)], [Stm rep])
allocStms merge = runWriterT . zipWithM allocation merge
  where
    allocation m@(Param attrs pname _, _) (BufferAlloc name size space b) = do
      stms <- lift $
        runBuilder_ $ do
          size' <- toSubExp "double_buffer_size" size
          letBindNames [name] $ Op $ Alloc size' space
      tell $ stmsToList stms
      if b
        then pure (Param attrs pname $ MemMem space, Var name)
        else pure m
    allocation (f, Var v) (BufferCopy mem _ _ b) | b = do
      v_copy <- lift $ newVName $ baseString v ++ "_double_buffer_copy"
      (_v_mem, v_ixfun) <- lift $ lookupArraySummary v
      let bt = elemType $ paramType f
          shape = arrayShape $ paramType f
          bound = MemArray bt shape NoUniqueness $ ArrayIn mem v_ixfun
      tell
        [ Let (Pat [PatElem v_copy bound]) (defAux ()) $
            BasicOp (Replicate mempty $ Var v)
        ]
      -- It is important that we treat this as a consumption, to
      -- avoid the Copy from being hoisted out of any enclosing
      -- loops.  Since we re-use (=overwrite) memory in the loop,
      -- the copy is critical for initialisation.  See issue #816.
      let uniqueMemInfo (MemArray pt pshape _ ret) =
            MemArray pt pshape Unique ret
          uniqueMemInfo info = info
      pure (uniqueMemInfo <$> f, Var v_copy)
    allocation (f, se) _ =
      pure (f, se)

doubleBufferResult ::
  (Constraints rep inner) =>
  [FParam rep] ->
  [DoubleBuffer] ->
  Body rep ->
  Body rep
doubleBufferResult valparams buffered (Body _ stms res) =
  let (ctx_res, val_res) = splitAt (length res - length valparams) res
      (copystms, val_res') =
        unzip $ zipWith3 buffer valparams buffered val_res
   in Body () (stms <> stmsFromList (catMaybes copystms)) $ ctx_res ++ val_res'
  where
    buffer _ (BufferAlloc bufname _ _ _) se =
      (Nothing, se {resSubExp = Var bufname})
    buffer fparam (BufferCopy bufname ixfun copyname _) (SubExpRes cs (Var v)) =
      -- To construct the copy we will need to figure out its type
      -- based on the type of the function parameter.
      let t = resultType $ paramType fparam
          summary = MemArray (elemType t) (arrayShape t) NoUniqueness $ ArrayIn bufname ixfun
          copystm =
            Let
              (Pat [PatElem copyname summary])
              (defAux ())
              (BasicOp $ Replicate mempty $ Var v)
       in (Just copystm, SubExpRes cs (Var copyname))
    buffer _ _ se =
      (Nothing, se)

    parammap = M.fromList $ zip (map paramName valparams) $ map resSubExp res

    resultType t = t `setArrayDims` map substitute (arrayDims t)

    substitute (Var v)
      | Just replacement <- M.lookup v parammap = replacement
    substitute se =
      se
