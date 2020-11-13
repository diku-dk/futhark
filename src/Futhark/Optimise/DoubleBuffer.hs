{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The simplification engine is only willing to hoist allocations
-- out of loops if the memory block resulting from the allocation is
-- dead at the end of the loop.  If it is not, we may cause data
-- hazards.
--
-- This module rewrites loops with memory block merge parameters such
-- that each memory block is copied at the end of the iteration, thus
-- ensuring that any allocation inside the loop is dead at the end of
-- the loop.  This is only possible for allocations whose size is
-- loop-invariant, although the initial size may differ from the size
-- produced by the loop result.
--
-- Additionally, inside parallel kernels we also copy the initial
-- value.  This has the effect of making the memory block returned by
-- the array non-existential, which is important for later memory
-- expansion to work.
module Futhark.Optimise.DoubleBuffer (doubleBufferKernels, doubleBufferMC) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Construct
import Futhark.IR.KernelsMem as Kernels
import Futhark.IR.MCMem as MC
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (arraySizeInBytesExp)
import Futhark.Pass.ExplicitAllocations.Kernels ()
import Futhark.Util (maybeHead)

-- | The pass for GPU kernels.
doubleBufferKernels :: Pass KernelsMem KernelsMem
doubleBufferKernels = doubleBuffer optimiseKernelsOp

-- | The pass for multicore
doubleBufferMC :: Pass MCMem MCMem
doubleBufferMC = doubleBuffer optimiseMCOp

-- | The double buffering pass definition.
doubleBuffer :: Mem lore => OptimiseOp lore -> Pass lore lore
doubleBuffer onOp =
  Pass
    { passName = "Double buffer",
      passDescription = "Perform double buffering for merge parameters of sequential loops.",
      passFunction = intraproceduralTransformation optimise
    }
  where
    optimise scope stms = modifyNameSource $ \src ->
      let m =
            runDoubleBufferM $
              localScope scope $
                fmap stmsFromList $ optimiseStms $ stmsToList stms
       in runState (runReaderT m env) src

    env = Env mempty doNotTouchLoop onOp
    doNotTouchLoop ctx val body = return (mempty, ctx, val, body)

type OptimiseLoop lore =
  [(FParam lore, SubExp)] ->
  [(FParam lore, SubExp)] ->
  Body lore ->
  DoubleBufferM
    lore
    ( [Stm lore],
      [(FParam lore, SubExp)],
      [(FParam lore, SubExp)],
      Body lore
    )

type OptimiseOp lore =
  Op lore -> DoubleBufferM lore (Op lore)

data Env lore = Env
  { envScope :: Scope lore,
    envOptimiseLoop :: OptimiseLoop lore,
    envOptimiseOp :: OptimiseOp lore
  }

newtype DoubleBufferM lore a = DoubleBufferM
  { runDoubleBufferM :: ReaderT (Env lore) (State VNameSource) a
  }
  deriving (Functor, Applicative, Monad, MonadReader (Env lore), MonadFreshNames)

instance ASTLore lore => HasScope lore (DoubleBufferM lore) where
  askScope = asks envScope

instance ASTLore lore => LocalScope lore (DoubleBufferM lore) where
  localScope scope = local $ \env -> env {envScope = envScope env <> scope}

optimiseBody :: ASTLore lore => Body lore -> DoubleBufferM lore (Body lore)
optimiseBody body = do
  bnds' <- optimiseStms $ stmsToList $ bodyStms body
  return $ body {bodyStms = stmsFromList bnds'}

optimiseStms :: ASTLore lore => [Stm lore] -> DoubleBufferM lore [Stm lore]
optimiseStms [] = return []
optimiseStms (e : es) = do
  e_es <- optimiseStm e
  es' <- localScope (castScope $ scopeOf e_es) $ optimiseStms es
  return $ e_es ++ es'

optimiseStm :: forall lore. ASTLore lore => Stm lore -> DoubleBufferM lore [Stm lore]
optimiseStm (Let pat aux (DoLoop ctx val form body)) = do
  body' <-
    localScope (scopeOf form <> scopeOfFParams (map fst $ ctx ++ val)) $
      optimiseBody body
  opt_loop <- asks envOptimiseLoop
  (bnds, ctx', val', body'') <- opt_loop ctx val body'
  return $ bnds ++ [Let pat aux $ DoLoop ctx' val' form body'']
optimiseStm (Let pat aux e) = do
  onOp <- asks envOptimiseOp
  pure . Let pat aux <$> mapExpM (optimise onOp) e
  where
    optimise onOp =
      identityMapper
        { mapOnBody = \_ x ->
            optimiseBody x :: DoubleBufferM lore (Body lore),
          mapOnOp = onOp
        }

optimiseKernelsOp :: OptimiseOp KernelsMem
optimiseKernelsOp (Inner (SegOp op)) =
  local inSegOp $ Inner . SegOp <$> mapSegOpM mapper op
  where
    mapper =
      identitySegOpMapper
        { mapOnSegOpLambda = optimiseLambda,
          mapOnSegOpBody = optimiseKernelBody
        }
    inSegOp env = env {envOptimiseLoop = optimiseLoop}
optimiseKernelsOp op = return op

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
optimiseMCOp op = return op

optimiseKernelBody ::
  ASTLore lore =>
  KernelBody lore ->
  DoubleBufferM lore (KernelBody lore)
optimiseKernelBody kbody = do
  stms' <- optimiseStms $ stmsToList $ kernelBodyStms kbody
  return $ kbody {kernelBodyStms = stmsFromList stms'}

optimiseLambda ::
  ASTLore lore =>
  Lambda lore ->
  DoubleBufferM lore (Lambda lore)
optimiseLambda lam = do
  body <- localScope (castScope $ scopeOf lam) $ optimiseBody $ lambdaBody lam
  return lam {lambdaBody = body}

type Constraints lore =
  ( ASTLore lore,
    FParamInfo lore ~ FParamMem,
    LParamInfo lore ~ LParamMem,
    RetType lore ~ RetTypeMem,
    LetDec lore ~ LetDecMem,
    BranchType lore ~ BranchTypeMem,
    ExpDec lore ~ (),
    BodyDec lore ~ (),
    OpReturns lore
  )

optimiseLoop :: (Constraints lore, Op lore ~ MemOp inner, BinderOps lore) => OptimiseLoop lore
optimiseLoop ctx val body = do
  -- We start out by figuring out which of the merge variables should
  -- be double-buffered.
  buffered <-
    doubleBufferMergeParams
      (zip (map fst ctx) (bodyResult body))
      (map fst merge)
      (boundInBody body)
  -- Then create the allocations of the buffers and copies of the
  -- initial values.
  (merge', allocs) <- allocStms merge buffered
  -- Modify the loop body to copy buffered result arrays.
  let body' = doubleBufferResult (map fst merge) buffered body
      (ctx', val') = splitAt (length ctx) merge'
  -- Modify the initial merge p
  return (allocs, ctx', val', body')
  where
    merge = ctx ++ val

-- | The booleans indicate whether we should also play with the
-- initial merge values.
data DoubleBuffer
  = BufferAlloc VName (PrimExp VName) Space Bool
  | -- | First name is the memory block to copy to,
    -- second is the name of the array copy.
    BufferCopy VName IxFun VName Bool
  | NoBuffer
  deriving (Show)

doubleBufferMergeParams ::
  MonadFreshNames m =>
  [(Param FParamMem, SubExp)] ->
  [Param FParamMem] ->
  Names ->
  m [DoubleBuffer]
doubleBufferMergeParams ctx_and_res val_params bound_in_loop =
  evalStateT (mapM buffer val_params) M.empty
  where
    loopVariant v =
      v `nameIn` bound_in_loop
        || v `elem` map (paramName . fst) ctx_and_res

    loopInvariantSize (Constant v) =
      Just (Constant v, True)
    loopInvariantSize (Var v) =
      case find ((== v) . paramName . fst) ctx_and_res of
        Just (_, Constant val) ->
          Just (Constant val, False)
        Just (_, Var v')
          | not $ loopVariant v' ->
            Just (Var v', False)
        Just _ ->
          Nothing
        Nothing ->
          Just (Var v, True)

    sizeForMem mem = maybeHead $ mapMaybe (arrayInMem . paramDec) val_params
      where
        arrayInMem (MemArray pt shape _ (ArrayIn arraymem ixfun))
          | IxFun.isDirect ixfun,
            Just (dims, b) <-
              mapAndUnzipM loopInvariantSize $ shapeDims shape,
            mem == arraymem =
            Just
              ( arraySizeInBytesExp $
                  Array (ElemPrim pt) (Shape dims) NoUniqueness,
                or b
              )
        arrayInMem _ = Nothing

    buffer fparam = case paramType fparam of
      Mem space
        | Just (size, b) <- sizeForMem $ paramName fparam -> do
          -- Let us double buffer this!
          bufname <- lift $ newVName "double_buffer_mem"
          modify $ M.insert (paramName fparam) (bufname, b)
          return $ BufferAlloc bufname size space b
      Array {}
        | MemArray _ _ _ (ArrayIn mem ixfun) <- paramDec fparam -> do
          buffered <- gets $ M.lookup mem
          case buffered of
            Just (bufname, b) -> do
              copyname <- lift $ newVName "double_buffer_array"
              return $ BufferCopy bufname ixfun copyname b
            Nothing ->
              return NoBuffer
      _ -> return NoBuffer

allocStms ::
  (Constraints lore, Op lore ~ MemOp inner, BinderOps lore) =>
  [(FParam lore, SubExp)] ->
  [DoubleBuffer] ->
  DoubleBufferM lore ([(FParam lore, SubExp)], [Stm lore])
allocStms merge = runWriterT . zipWithM allocation merge
  where
    allocation m@(Param pname _, _) (BufferAlloc name size space b) = do
      stms <- lift $
        runBinder_ $ do
          size' <- toSubExp "double_buffer_size" size
          letBindNames [name] $ Op $ Alloc size' space
      tell $ stmsToList stms
      if b
        then return (Param pname $ MemMem space, Var name)
        else return m
    allocation (f, Var v) (BufferCopy mem _ _ b) | b = do
      v_copy <- lift $ newVName $ baseString v ++ "_double_buffer_copy"
      (_v_mem, v_ixfun) <- lift $ lookupArraySummary v
      let ElemPrim pt = elemType $ paramType f
          shape = arrayShape $ paramType f
          bound = MemArray pt shape NoUniqueness $ ArrayIn mem v_ixfun
      tell
        [ Let (Pattern [] [PatElem v_copy bound]) (defAux ()) $
            BasicOp $ Copy v
        ]
      -- It is important that we treat this as a consumption, to
      -- avoid the Copy from being hoisted out of any enclosing
      -- loops.  Since we re-use (=overwrite) memory in the loop,
      -- the copy is critical for initialisation.  See issue #816.
      let uniqueMemInfo (MemArray ppt pshape _ ret) =
            MemArray ppt pshape Unique ret
          uniqueMemInfo info = info
      return (uniqueMemInfo <$> f, Var v_copy)
    allocation (f, se) _ =
      return (f, se)

doubleBufferResult ::
  (Constraints lore) =>
  [FParam lore] ->
  [DoubleBuffer] ->
  Body lore ->
  Body lore
doubleBufferResult valparams buffered (Body _ bnds res) =
  let (ctx_res, val_res) = splitAt (length res - length valparams) res
      (copybnds, val_res') =
        unzip $ zipWith3 buffer valparams buffered val_res
   in Body () (bnds <> stmsFromList (catMaybes copybnds)) $ ctx_res ++ val_res'
  where
    buffer _ (BufferAlloc bufname _ _ _) _ =
      (Nothing, Var bufname)
    buffer fparam (BufferCopy bufname ixfun copyname _) (Var v) =
      -- To construct the copy we will need to figure out its type
      -- based on the type of the function parameter.
      let t = resultType $ paramType fparam
          ElemPrim pt = elemType t
          summary = MemArray pt (arrayShape t) NoUniqueness $ ArrayIn bufname ixfun
          copybnd =
            Let (Pattern [] [PatElem copyname summary]) (defAux ()) $
              BasicOp $ Copy v
       in (Just copybnd, Var copyname)
    buffer _ _ se =
      (Nothing, se)

    parammap = M.fromList $ zip (map paramName valparams) res

    resultType t = t `setArrayDims` map substitute (arrayDims t)

    substitute (Var v)
      | Just replacement <- M.lookup v parammap = replacement
    substitute se =
      se
