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

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Construct
import Futhark.IR.GPUMem as GPU
import Futhark.IR.MCMem as MC
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations.GPU ()
import Futhark.Transform.Substitute
import Futhark.Util (mapAccumLM)

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

isArrayIn :: VName -> Param FParamMem -> Bool
isArrayIn x (Param _ _ (MemArray _ _ _ (ArrayIn y _))) = x == y
isArrayIn _ _ = False

doubleBufferSpace :: Space -> Bool
doubleBufferSpace ScalarSpace {} = False
doubleBufferSpace _ = True

optimiseLoop :: (Constraints rep inner) => OptimiseLoop rep
optimiseLoop (Pat pes) merge body@(Body _ body_stms body_res) = do
  ((pat', merge', body'), outer_stms) <- runBuilder $ do
    ((param_changes, body_stms'), (pes', merge', body_res')) <-
      second unzip3 <$> mapAccumLM check (id, body_stms) (zip3 pes merge body_res)
    pure
      ( Pat $ mconcat pes',
        map param_changes $ mconcat merge',
        Body () body_stms' $ mconcat body_res'
      )
  pure (outer_stms, pat', merge', body')
  where
    bound_in_loop =
      namesFromList (map (paramName . fst) merge) <> boundInBody body

    findLmadOfArray v = listToMaybe . mapMaybe onStm $ stmsToList body_stms
      where
        onStm = listToMaybe . mapMaybe onPatElem . patElems . stmPat
        onPatElem (PatElem pe_v (MemArray _ _ _ (ArrayIn _ lmad)))
          | v == pe_v,
            not $ bound_in_loop `namesIntersect` freeIn lmad =
              Just lmad
        onPatElem _ = Nothing

    changeParam p_needle new (p, p_initial) =
      if p == p_needle then new else (p, p_initial)

    check (param_changes, body_stms') (pe, (param, arg), res)
      | Mem space <- paramType param,
        doubleBufferSpace space,
        Var arg_v <- arg,
        -- XXX: what happens if there are multiple arrays in the same
        -- memory block?
        [((arr_param, Var arr_param_initial), Var arr_v)] <-
          filter
            (isArrayIn (paramName param) . fst . fst)
            (zip merge $ map resSubExp body_res),
        MemArray pt shape _ (ArrayIn _ param_lmad) <- paramDec arr_param,
        Var arr_mem_out <- resSubExp res,
        Just arr_lmad <- findLmadOfArray arr_v,
        Just (arr_mem_out_alloc, body_stms'') <-
          extractAllocOf bound_in_loop arr_mem_out body_stms' = do
          -- Put the allocations outside the loop.
          num_bytes <-
            letSubExp "num_bytes" =<< toExp (primByteSize pt * (1 + LMAD.range arr_lmad))
          arr_mem_in <-
            letExp (baseString arg_v <> "_in") $ Op $ Alloc num_bytes space
          addStm arr_mem_out_alloc

          -- Construct additional pattern element and parameter for
          -- the memory block that is not used afterwards.
          pe_unused <-
            PatElem
              <$> newVName (baseString (patElemName pe) <> "_unused")
              <*> pure (MemMem space)
          param_out <-
            newParam (baseString (paramName param) <> "_out") (MemMem space)

          -- Copy the initial array value to the input memory, with
          -- the same index function as the result.
          arr_v_copy <- newVName $ baseString arr_v <> "_db_copy"
          let arr_initial_info =
                MemArray pt shape NoUniqueness $ ArrayIn arr_mem_in arr_lmad
              arr_initial_pe =
                PatElem arr_v_copy arr_initial_info
          addStm . Let (Pat [arr_initial_pe]) (defAux ()) . BasicOp $
            Replicate mempty (Var arr_param_initial)
          -- AS a trick we must make the array parameter Unique to
          -- avoid unfortunate hoisting (see #1533) because we are
          -- invalidating the underlying memory.
          let arr_param' =
                Param mempty (paramName arr_param) $
                  MemArray pt shape Unique (ArrayIn (paramName param) param_lmad)

          -- We must also update the initial values of the parameters
          -- used in the index function of this array parameter, such
          -- that they match the result.
          let mkUpdate lmad_v =
                case L.find ((== lmad_v) . paramName . fst . fst) $
                  zip merge body_res of
                  Nothing -> id
                  Just ((p, _), p_res) -> changeParam p (p, resSubExp p_res)
              updateLmadParam =
                foldl (.) id $ map mkUpdate $ namesToList $ freeIn param_lmad

          pure
            ( ( updateLmadParam
                  . changeParam arr_param (arr_param', Var arr_v_copy)
                  . param_changes,
                substituteNames (M.singleton arr_mem_out (paramName param_out)) body_stms''
              ),
              ( [pe, pe_unused],
                [(param, Var arr_mem_in), (param_out, Var arr_mem_out)],
                [ res {resSubExp = Var $ paramName param_out},
                  subExpRes $ Var $ paramName param
                ]
              )
            )
      | otherwise =
          pure
            ( (param_changes, body_stms'),
              ([pe], [(param, arg)], [res])
            )

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
