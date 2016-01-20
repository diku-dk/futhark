{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import           Futhark.Pass

doubleBuffer :: Pass ExplicitMemory ExplicitMemory
doubleBuffer =
  Pass { passName = "Double buffer"
       , passDescription = "Perform double buffering for merge parameters of sequential loops."
       , passFunction = intraproceduralTransformation optimiseFunDec
       }

optimiseFunDec :: MonadFreshNames m => FunDec -> m FunDec
optimiseFunDec fundec = do
  body' <- runReaderT (inScopeOf fundec $
                       optimiseBody $ funDecBody fundec) emptyScope
  return fundec { funDecBody = body' }
  where emptyScope :: Scope ExplicitMemory
        emptyScope = mempty

type DoubleBufferer m = (MonadFreshNames m, LocalScope ExplicitMemory m)

optimiseBody :: DoubleBufferer m => Body -> m Body
optimiseBody body = do
  bnds' <- optimiseBindings $ bodyBindings body
  return $ body { bodyBindings = bnds' }

optimiseBindings :: DoubleBufferer m => [Binding] -> m [Binding]
optimiseBindings [] = return []
optimiseBindings (e:es) = do
  e_es <- optimiseBinding e
  es' <- inScopeOf e_es $ optimiseBindings es
  return $ e_es ++ es'

optimiseBinding :: DoubleBufferer m => Binding -> m [Binding]
optimiseBinding (Let pat () (LoopOp (DoLoop res merge form body))) = do
  body' <- localScope (scopeOfLoopForm form <> scopeOfFParams (map fst merge)) $
           optimiseBody body
  (bnds, merge', body'') <- optimiseLoop merge body'
  return $ bnds ++ [Let pat () $ LoopOp $ DoLoop res merge' form body'']
optimiseBinding (Let pat () e) = pure <$> Let pat () <$> mapExpM optimise e
  where optimise = identityMapper { mapOnBody = optimiseBody
                                  , mapOnOp = optimiseOp
                                  }
          where optimiseOp (Inner k) = Inner <$> optimiseKernel k
                optimiseOp op = return op
                optimiseKernel = mapKernelM identityKernelMapper
                                 { mapOnKernelBody = optimiseBody
                                 , mapOnKernelLambda = optimiseLambda
                                 }
                optimiseLambda lam = do
                  body <- inScopeOf lam $ optimiseBody $ lambdaBody lam
                  return lam { lambdaBody = body }

optimiseLoop :: DoubleBufferer m =>
                [(FParam, SubExp)] -> Body
             -> m ([Binding], [(FParam, SubExp)], Body)
optimiseLoop mergeparams body = do
  -- We start out by figuring out which of the merge variables should
  -- be double-buffered.
  buffered <- doubleBufferMergeParams
              (zip (map fst mergeparams) (bodyResult body))
              (boundInBody body)
  -- Then create the allocations of the buffers and copies of the
  -- initial values.
  (mergeparams', allocs) <- allocBindings mergeparams buffered
  -- Modify the loop body to copy buffered result arrays.
  let body' = doubleBufferResult (map fst mergeparams) buffered body
  -- Modify the initial merge p
  return (allocs, mergeparams', body')

data DoubleBuffer = BufferAlloc VName SubExp Space
                  | BufferCopy VName IxFun.IxFun VName
                    -- ^ First name is the memory block to copy to,
                    -- second is the name of the array copy.
                  | NoBuffer
                    deriving (Show)

doubleBufferMergeParams :: DoubleBufferer m =>
                           [(FParam,SubExp)] -> Names -> m [DoubleBuffer]
doubleBufferMergeParams params_and_res bound_in_loop = evalStateT (mapM buffer params) HM.empty
  where (params,_) = unzip params_and_res

        loopInvariantSize (Constant v) =
          Just $ Constant v
        loopInvariantSize (Var v) =
          case find ((==v) . paramName . fst) params_and_res of
          -- FIXME: these cases are disabled because the resulting
          -- code is too gnarly for the simplifier to not break.
            Just (_, Constant val) | False ->
              Just $ Constant val
            Just (_, Var v') | not $ v' `HS.member` bound_in_loop, False ->
              Just $ Var v'
            Just _ ->
              Nothing
            Nothing ->
              Just $ Var v

        buffer fparam = case paramType fparam of
          Mem size space
            | Just size' <- loopInvariantSize size -> do
                -- Let us double buffer this!
                bufname <- lift $ newVName "double_buffer_mem"
                modify $ HM.insert (paramName fparam) bufname
                return $ BufferAlloc bufname size' space
          Array {}
            | ArrayMem _ _ _ mem ixfun <- paramAttr fparam -> do
                buffered <- gets $ HM.lookup mem
                case buffered of
                  Just bufname -> do
                    copyname <- lift $ newVName "double_buffer_array"
                    return $ BufferCopy bufname ixfun copyname
                  Nothing ->
                    return NoBuffer
          _ -> return NoBuffer

allocBindings :: DoubleBufferer m =>
                 [(FParam,SubExp)] -> [DoubleBuffer] -> m ([(FParam,SubExp)], [Binding])
allocBindings merge = runWriterT . zipWithM allocation merge
  where allocation (Param pname _, _) (BufferAlloc name size space) = do
          tell [Let (Pattern [] [PatElem name BindVar $ MemMem size space]) () $
                Op $ Alloc size space]
          return (Param pname $ MemMem size space, Var name)
        allocation (f, Var v) (BufferCopy mem _ _) = do
          v_copy <- lift $ newVName $ baseString v ++ "_double_buffer_copy"
          (_v_mem, v_ixfun) <- lift $ lookupArraySummary v
          let bt = elemType $ paramType f
              shape = arrayShape $ paramType f
              bound = ArrayMem bt shape NoUniqueness mem v_ixfun
          tell [Let (Pattern []
                     [PatElem v_copy BindVar bound]) () $
                PrimOp $ Copy v]
          return (f, Var v_copy)
        allocation (f, se) _ =
          return (f, se)

doubleBufferResult :: [FParam] -> [DoubleBuffer] -> Body -> Body
doubleBufferResult mergeparams buffered (Body () bnds res) =
  let (copybnds,ses) =
        unzip $ zipWith3 buffer mergeparams buffered res
  in Body () (bnds++catMaybes copybnds) ses
  where buffer _ (BufferAlloc bufname _ _) _ =
          (Nothing, Var bufname)

        buffer fparam (BufferCopy bufname ixfun copyname) (Var v) =
          -- To construct the copy we will need to figure out its type
          -- based on the type of the function parameter.
          let t = resultType $ paramType fparam
              summary = ArrayMem (elemType t) (arrayShape t) NoUniqueness bufname ixfun
              copybnd = Let (Pattern [] [PatElem copyname BindVar summary]) () $
                        PrimOp $ Copy v
          in (Just copybnd, Var copyname)

        buffer _ _ se =
          (Nothing, se)

        parammap = HM.fromList $ zip (map paramName mergeparams) res

        resultType t = t `setArrayDims` map substitute (arrayDims t)

        substitute (Var v)
          | Just replacement <- HM.lookup v parammap = replacement
        substitute se =
          se
