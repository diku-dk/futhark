{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Futhark.Optimise.DoubleBuffer
       ( doubleBuffer )
       where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.List

import           Prelude

import           Futhark.MonadFreshNames
import           Futhark.Tools (intraproceduralTransformation)
import           Futhark.Representation.AST
import           Futhark.Representation.ExplicitMemory
                 hiding (Prog, Body, Binding, Pattern, PatElem,
                         BasicOp, Exp, Lambda, ExtLambda, FunDef, FParam, LParam, RetType)
import           Futhark.Pass

doubleBuffer :: Pass ExplicitMemory ExplicitMemory
doubleBuffer =
  Pass { passName = "Double buffer"
       , passDescription = "Perform double buffering for merge parameters of sequential loops."
       , passFunction = intraproceduralTransformation optimiseFunDef
       }

-- This pass is written in a slightly weird way because we want to
-- apply essentially the same transformation both outside and inside
-- kernel bodies, which are different (but similar) representations.
-- Thus, the environment is parametrised by the lore and contains the
-- function used to transform 'Op's for the lore.

optimiseFunDef :: MonadFreshNames m => FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
optimiseFunDef fundec = do
  body' <- runReaderT (runDoubleBufferM $ inScopeOf fundec $
                       optimiseBody $ funDefBody fundec) $
           Env emptyScope optimiseKernelOp False
  return fundec { funDefBody = body' }
  where emptyScope :: Scope ExplicitMemory
        emptyScope = mempty

        optimiseKernelOp (Inner k) = do
          scope <- castScope <$> askScope
          runReaderT (runDoubleBufferM $ Inner <$> optimiseKernel k) $
            Env scope optimiseInKernelOp True
          where optimiseKernel =
                  mapKernelM identityKernelMapper
                  { mapOnKernelBody = optimiseBody
                  , mapOnKernelKernelBody = optimiseKernelBody
                  , mapOnKernelLambda = optimiseLambda
                  }
        optimiseKernelOp op = return op

        optimiseInKernelOp = return

data Env lore m = Env { envScope :: Scope lore
                      , envOptimiseOp :: Op lore -> DoubleBufferM lore m (Op lore)
                      , envCopyInit :: Bool
                      -- ^ If true, copy initial values of merge
                      -- parameters.  This is necessary to remove
                      -- existential memory inside kernels, but seems to
                      -- break C compiler vectorisation in sequential
                      -- code.  We set this to true once we enter
                      -- kernels.
                      }

newtype DoubleBufferM lore m a = DoubleBufferM { runDoubleBufferM :: ReaderT (Env lore m) m a }
                          deriving (Functor, Applicative, Monad,
                                    MonadReader (Env lore m), MonadFreshNames)

instance (Annotations lore, Applicative m, Monad m) =>
         HasScope lore (DoubleBufferM lore m) where
  askScope = asks envScope

instance (Annotations lore, Applicative m, Monad m) =>
         LocalScope lore (DoubleBufferM lore m) where
  localScope scope = local $ \env -> env { envScope = envScope env <> scope }

-- | Bunch up all the constraints for less typing.
type LoreConstraints lore inner m =
  (ExpAttr lore ~ (), BodyAttr lore ~ (),
   ExplicitMemorish lore, Op lore ~ MemOp inner, MonadFreshNames m)

optimiseBody :: LoreConstraints lore inner m =>
                Body lore -> DoubleBufferM lore m (Body lore)
optimiseBody body = do
  bnds' <- optimiseBindings $ bodyBindings body
  return $ body { bodyBindings = bnds' }

optimiseBindings :: LoreConstraints lore inner m =>
                    [Binding lore] -> DoubleBufferM lore m [Binding lore]
optimiseBindings [] = return []
optimiseBindings (e:es) = do
  e_es <- optimiseBinding e
  es' <- localScope (castScope $ scopeOf e_es) $ optimiseBindings es
  return $ e_es ++ es'

optimiseBinding :: LoreConstraints lore inner m =>
                   Binding lore -> DoubleBufferM lore m [Binding lore]
optimiseBinding (Let pat () (DoLoop ctx val form body)) = do
  body' <- localScope (scopeOfLoopForm form <> scopeOfFParams (map fst $ ctx++val)) $
           optimiseBody body
  (bnds, ctx', val', body'') <- optimiseLoop ctx val body'
  return $ bnds ++ [Let pat () $ DoLoop ctx' val' form body'']
optimiseBinding (Let pat () e) = pure . Let pat () <$> mapExpM optimise e
  where optimise = identityMapper { mapOnBody = optimiseBody
                                  , mapOnOp = optimiseOp
                                  }

optimiseOp :: MonadFreshNames m =>
              Op lore -> DoubleBufferM lore m (Op lore)
optimiseOp op = do f <- asks envOptimiseOp
                   f op

optimiseKernelBody :: MonadFreshNames m =>
                      KernelBody InKernel
                   -> DoubleBufferM InKernel m (KernelBody InKernel)
optimiseKernelBody kbody = do
  stms' <- optimiseBindings $ kernelBodyStms kbody
  return $ kbody { kernelBodyStms = stms' }

optimiseLambda :: MonadFreshNames m =>
                  Lambda InKernel -> DoubleBufferM InKernel m (Lambda InKernel)
optimiseLambda lam = do
  body <- localScope (castScope $ scopeOf lam) $ optimiseBody $ lambdaBody lam
  return lam { lambdaBody = body }

optimiseLoop :: LoreConstraints lore inner m =>
                [(FParam lore, SubExp)] -> [(FParam lore, SubExp)] -> Body lore
             -> DoubleBufferM lore m ([Binding lore],
                                 [(FParam lore, SubExp)],
                                 [(FParam lore, SubExp)],
                                 Body lore)
optimiseLoop ctx val body = do
  -- We start out by figuring out which of the merge variables should
  -- be double-buffered.
  buffered <- doubleBufferMergeParams
              (zip (map fst ctx) (bodyResult body))
              (map fst merge)
              (boundInBody body)
  -- Then create the allocations of the buffers and copies of the
  -- initial values.
  (merge', allocs) <- allocBindings merge buffered
  -- Modify the loop body to copy buffered result arrays.
  let body' = doubleBufferResult (map fst merge) buffered body
      (ctx', val') = splitAt (length ctx) merge'
  -- Modify the initial merge p
  return (allocs, ctx', val', body')
  where merge = ctx ++ val

-- | The booleans indicate whether we should also play with the
-- initial merge values.
data DoubleBuffer lore = BufferAlloc VName SubExp Space Bool
                       | BufferCopy VName IxFun VName Bool
                       -- ^ First name is the memory block to copy to,
                       -- second is the name of the array copy.
                       | NoBuffer
                    deriving (Show)

doubleBufferMergeParams :: (ExplicitMemorish lore, MonadFreshNames m) =>
                           [(FParam lore,SubExp)]
                        -> [FParam lore] -> Names
                        -> DoubleBufferM lore m [DoubleBuffer lore]
doubleBufferMergeParams ctx_and_res val_params bound_in_loop = do
  copy_init <- asks envCopyInit
  evalStateT (mapM (buffer copy_init) val_params) HM.empty
  where loopInvariantSize copy_init (Constant v) =
          Just (Constant v, copy_init)
        loopInvariantSize copy_init (Var v) =
          case find ((==v) . paramName . fst) ctx_and_res of
            Just (_, Constant val) ->
              Just (Constant val, False)
            Just (_, Var v') | not $ v' `HS.member` bound_in_loop ->
              Just (Var v', False)
            Just _ ->
              Nothing
            Nothing ->
              Just (Var v, copy_init)

        buffer copy_init fparam = case paramType fparam of
          Mem size space
            | Just (size', b) <- loopInvariantSize copy_init size -> do
                -- Let us double buffer this!
                bufname <- lift $ newVName "double_buffer_mem"
                modify $ HM.insert (paramName fparam) (bufname, b)
                return $ BufferAlloc bufname size' space b
          Array {}
            | ArrayMem _ _ _ mem ixfun <- paramAttr fparam -> do
                buffered <- gets $ HM.lookup mem
                case buffered of
                  Just (bufname, b) -> do
                    copyname <- lift $ newVName "double_buffer_array"
                    return $ BufferCopy bufname ixfun copyname b
                  Nothing ->
                    return NoBuffer
          _ -> return NoBuffer

allocBindings :: LoreConstraints lore inner m =>
                 [(FParam lore,SubExp)] -> [DoubleBuffer lore]
              -> DoubleBufferM lore m ([(FParam lore, SubExp)],
                                       [Binding lore])
allocBindings merge = runWriterT . zipWithM allocation merge
  where allocation m@(Param pname _, _) (BufferAlloc name size space b) = do
          tell [Let (Pattern [] [PatElem name BindVar $ MemMem size space]) () $
                Op $ Alloc size space]
          if b
            then return (Param pname $ MemMem size space, Var name)
            else return m
        allocation (f, Var v) (BufferCopy mem _ _ b) | b = do
          v_copy <- lift $ newVName $ baseString v ++ "_double_buffer_copy"
          (_v_mem, v_ixfun) <- lift $ lookupArraySummary v
          let bt = elemType $ paramType f
              shape = arrayShape $ paramType f
              bound = ArrayMem bt shape NoUniqueness mem v_ixfun
          tell [Let (Pattern []
                     [PatElem v_copy BindVar bound]) () $
                BasicOp $ Copy v]
          return (f, Var v_copy)
        allocation (f, se) _ =
          return (f, se)

doubleBufferResult :: (ExplicitMemorish lore,
                       ExpAttr lore ~ (), BodyAttr lore ~ ()) =>
                      [FParam lore] -> [DoubleBuffer lore]
                   -> Body lore -> Body lore
doubleBufferResult valparams buffered (Body () bnds res) =
  let (ctx_res, val_res) = splitAt (length res - length valparams) res
      (copybnds,val_res') =
        unzip $ zipWith3 buffer valparams buffered val_res
  in Body () (bnds++catMaybes copybnds) $ ctx_res ++ val_res'
  where buffer _ (BufferAlloc bufname _ _ _) _ =
          (Nothing, Var bufname)

        buffer fparam (BufferCopy bufname ixfun copyname _) (Var v) =
          -- To construct the copy we will need to figure out its type
          -- based on the type of the function parameter.
          let t = resultType $ paramType fparam
              summary = ArrayMem (elemType t) (arrayShape t) NoUniqueness bufname ixfun
              copybnd = Let (Pattern [] [PatElem copyname BindVar summary]) () $
                        BasicOp $ Copy v
          in (Just copybnd, Var copyname)

        buffer _ _ se =
          (Nothing, se)

        parammap = HM.fromList $ zip (map paramName valparams) res

        resultType t = t `setArrayDims` map substitute (arrayDims t)

        substitute (Var v)
          | Just replacement <- HM.lookup v parammap = replacement
        substitute se =
          se
