{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Expand allocations inside of maps when possible.
module Futhark.Pass.ExpandAllocations
       ( expandAllocations )
       where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List
import Data.Monoid

import Prelude hiding (div, quot)

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import Futhark.Tools
import Futhark.Util
import Futhark.Pass
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

expandAllocations :: Pass ExplicitMemory ExplicitMemory
expandAllocations = simplePass
                    "expand allocations"
                    "Expand allocations" $
                    intraproceduralTransformation transformFunDef

transformFunDef :: MonadFreshNames m => FunDef -> m FunDef
transformFunDef fundec = do
  body' <- modifyNameSource $ runState m
  return fundec { funDefBody = body' }
  where m = transformBody $ funDefBody fundec

type ExpandM = State VNameSource

transformBody :: Body -> ExpandM Body
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformBinding bnds
  return $ Body () bnds' res

transformBinding :: Binding -> ExpandM [Binding]

transformBinding (Let pat () e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds ++ [Let pat () e']
  where transform = identityMapper { mapOnBody = transformBody
                                   }

transformExp :: Exp -> ExpandM ([Binding], Exp)
transformExp (Op (Inner (MapKernel cs w thread_num ispace inps returns body)))
  -- Extract allocations from the body.
  | Right (body', thread_allocs) <- extractKernelAllocations bound_before_body body = do

  (alloc_bnds, alloc_offsets) <- expandedAllocations w thread_num thread_allocs
  let body'' = if null alloc_bnds then body'
               else offsetMemoryInBody alloc_offsets body'

  return (alloc_bnds, Op $ Inner $ MapKernel cs w thread_num ispace inps returns body'')
  where bound_before_body =
          HS.fromList $ map fst ispace ++ map kernelInputName inps

transformExp (Op (Inner (ChunkedMapKernel cs w kernel_size ordering lam arrs)))
  -- Extract allocations from the lambda.
  | Right (lam_body', lam_thread_allocs) <-
      extractKernelAllocations bound_in_lam $ lambdaBody lam = do

  (alloc_bnds, alloc_offsets) <-
    expandedAllocations num_threads thread_id lam_thread_allocs

  let lam_body'' = offsetMemoryInBody alloc_offsets lam_body'
      lam' = lam { lambdaBody = lam_body'' }
  return (alloc_bnds,
          Op $ Inner $ ChunkedMapKernel cs w kernel_size ordering lam' arrs)
  where num_threads = kernelNumThreads kernel_size
        bound_in_lam = HS.fromList $ HM.keys $ scopeOf lam
        (thread_id, _, _) = partitionChunkedKernelLambdaParameters $ lambdaParams lam

transformExp (Op (Inner (ReduceKernel cs w kernel_size comm red_lam fold_lam arrs)))
  -- Extract allocations from the lambdas.
  | Right (red_lam_body', red_lam_thread_allocs) <-
      extractKernelAllocations bound_in_red_lam $ lambdaBody red_lam,
    Right (fold_lam_body', fold_lam_thread_allocs) <-
      extractKernelAllocations bound_in_fold_lam $ lambdaBody fold_lam = do

  (red_alloc_bnds, red_alloc_offsets) <-
    expandedAllocations num_threads red_id red_lam_thread_allocs
  (fold_alloc_bnds, fold_alloc_offsets) <-
    expandedAllocations num_threads fold_id fold_lam_thread_allocs

  let red_lam_body'' = offsetMemoryInBody red_alloc_offsets red_lam_body'
      fold_lam_body'' = offsetMemoryInBody fold_alloc_offsets fold_lam_body'
      red_lam' = red_lam { lambdaBody = red_lam_body'' }
      fold_lam' = fold_lam { lambdaBody = fold_lam_body'' }
  return (red_alloc_bnds <> fold_alloc_bnds,
          Op $ Inner $ ReduceKernel cs w kernel_size comm red_lam' fold_lam' arrs)
  where num_threads = kernelNumThreads kernel_size
        (red_id, _, _) = partitionChunkedKernelLambdaParameters $ lambdaParams red_lam
        (fold_id, _, _) = partitionChunkedKernelLambdaParameters $ lambdaParams fold_lam

        bound_in_red_lam = HS.fromList $ HM.keys $ scopeOf red_lam
        bound_in_fold_lam = HS.fromList $ HM.keys $ scopeOf fold_lam

transformExp (Op (Inner (ScanKernel cs w kernel_size lam foldlam nes arrs)))
  -- Extract allocations from the lambda.
  | Right (lam_body', lam_thread_allocs) <-
      extractKernelAllocations bound_in_lam $ lambdaBody lam,
   Right (foldlam_body', foldlam_thread_allocs) <-
      extractKernelAllocations bound_in_foldlam $ lambdaBody foldlam = do

  (alloc_bnds, alloc_offsets) <-
    expandedAllocations num_threads thread_id lam_thread_allocs
  (fold_alloc_bnds, fold_alloc_offsets) <-
    expandedAllocations num_threads fold_thread_id foldlam_thread_allocs

  let lam_body'' = offsetMemoryInBody alloc_offsets lam_body'
      lam' = lam { lambdaBody = lam_body'' }
      foldlam_body'' = offsetMemoryInBody fold_alloc_offsets foldlam_body'
      foldlam' = foldlam { lambdaBody = foldlam_body'' }
  return (alloc_bnds <> fold_alloc_bnds,
          Op $ Inner $ ScanKernel cs w kernel_size lam' foldlam' nes arrs)
  where num_threads = kernelNumThreads kernel_size
        (thread_id, _, _) =
          partitionChunkedKernelLambdaParameters $ lambdaParams lam
        (fold_thread_id, _, _) =
          partitionChunkedKernelLambdaParameters $ lambdaParams foldlam

        bound_in_lam = HS.fromList $ HM.keys $ scopeOf lam
        bound_in_foldlam = HS.fromList $ HM.keys $ scopeOf foldlam

transformExp e =
  return ([], e)

-- | Returns a map from memory block names to their size in bytes,
-- as well as the lambda body where all the allocations have been removed.
-- Only looks at allocations in the immediate body - if there are any
-- further down, we will fail later.  If the size of one of the
-- allocations is not free in the body, we return 'Left' and an
-- error message.
extractKernelAllocations :: Names -> Body
                         -> Either String (Body, HM.HashMap VName (SubExp, Space))
extractKernelAllocations bound_before_body body = do
  (allocs, bnds) <- mapAccumLM isAlloc HM.empty $ bodyBindings body
  return (body { bodyBindings = catMaybes bnds }, allocs)
  where bound_here = bound_before_body `HS.union` boundInBody body

        isAlloc _ (Let (Pattern [] [patElem]) () (Op (Alloc (Var v) _)))
          | v `HS.member` bound_here =
            throwError $ "Size " ++ pretty v ++
            " for block " ++ pretty patElem ++
            " is not lambda-invariant"

        isAlloc allocs (Let (Pattern [] [patElem]) () (Op (Alloc size space))) =
          return (HM.insert (patElemName patElem) (size, space) allocs,
                  Nothing)

        isAlloc allocs bnd =
          return (allocs, Just bnd)

expandedAllocations :: SubExp
                    -> VName
                    -> HM.HashMap VName (SubExp, Space)
                    -> ExpandM ([Binding], RebaseMap)
expandedAllocations num_threads thread_index thread_allocs = do
  -- We expand the allocations by multiplying their size with the
  -- number of kernel threads.
  alloc_bnds <-
    fmap concat $ forM (HM.toList thread_allocs) $ \(mem,(per_thread_size, space)) -> do
      total_size <- newVName "total_size"
      let sizepat = Pattern [] [PatElem total_size BindVar $ Scalar int32]
          allocpat = Pattern [] [PatElem mem BindVar $
                                 MemMem (Var total_size) space]
      return [Let sizepat () $ PrimOp $ BinOp (Mul Int32) num_threads per_thread_size,
              Let allocpat () $ Op $ Alloc (Var total_size) space]
  -- Fix every reference to the memory blocks to be offset by the
  -- thread number.
  let alloc_offsets =
        RebaseMap { rebaseMap =
                    HM.map (const newBase) thread_allocs
                  , indexVariable = thread_index
                  , kernelWidth = num_threads
                  }
  return (alloc_bnds, alloc_offsets)
  where newBase old_shape =
          let perm = [length old_shape, 0] ++ [1..length old_shape-1]
              root_ixfun = IxFun.iota (old_shape ++ [SE.intSubExpToScalExp num_threads])
              permuted_ixfun = IxFun.permute root_ixfun perm
              offset_ixfun = IxFun.applyInd permuted_ixfun [SE.Id thread_index int32]
          in offset_ixfun

data RebaseMap = RebaseMap {
    rebaseMap :: HM.HashMap VName ([SE.ScalExp] -> IxFun.IxFun SE.ScalExp)
    -- ^ A map from memory block names to new index function bases.
  , indexVariable :: VName
  , kernelWidth :: SubExp
  }

lookupNewBase :: VName -> RebaseMap -> Maybe ([SE.ScalExp] -> IxFun.IxFun SE.ScalExp)
lookupNewBase name = HM.lookup name . rebaseMap

offsetMemoryInBody :: RebaseMap -> Body -> Body
offsetMemoryInBody offsets (Body attr bnds res) =
  Body attr (snd $ mapAccumL offsetMemoryInBinding offsets bnds) res

offsetMemoryInBinding :: RebaseMap -> Binding
                      -> (RebaseMap, Binding)
offsetMemoryInBinding offsets (Let pat attr e) =
  (offsets', Let pat' attr $ offsetMemoryInExp offsets e)
  where (offsets', pat') = offsetMemoryInPattern offsets pat

offsetMemoryInPattern :: RebaseMap -> Pattern -> (RebaseMap, Pattern)
offsetMemoryInPattern offsets (Pattern ctx vals) =
  (offsets', Pattern ctx vals')
  where offsets' = foldl inspectCtx offsets ctx
        vals' = map inspectVal vals
        inspectVal patElem =
          patElem { patElemAttr =
                       offsetMemoryInMemBound offsets' $ patElemAttr patElem
                  }
        inspectCtx ctx_offsets patElem
          | Mem _ _ <- patElemType patElem =
              error $ unwords ["Cannot deal with existential memory block",
                               pretty (patElemName patElem),
                               "when expanding inside kernels."]
          | otherwise =
              ctx_offsets

offsetMemoryInParam :: RebaseMap -> Param (MemBound u) -> Param (MemBound u)
offsetMemoryInParam offsets fparam =
  fparam { paramAttr = offsetMemoryInMemBound offsets $ paramAttr fparam }

offsetMemoryInMemBound :: RebaseMap -> MemBound u -> MemBound u
offsetMemoryInMemBound offsets (ArrayMem bt shape u mem ixfun)
  | Just new_base <- lookupNewBase mem offsets =
      ArrayMem bt shape u mem $ IxFun.rebase (new_base $ IxFun.base ixfun) ixfun
offsetMemoryInMemBound _ summary =
  summary

offsetMemoryInExp :: RebaseMap -> Exp -> Exp
offsetMemoryInExp offsets (DoLoop ctx val form body) =
  DoLoop (zip ctxparams' ctxinit) (zip valparams' valinit) form body'
  where (ctxparams, ctxinit) = unzip ctx
        (valparams, valinit) = unzip val
        body' = offsetMemoryInBody offsets body
        ctxparams' = map (offsetMemoryInParam offsets) ctxparams
        valparams' = map (offsetMemoryInParam offsets) valparams
offsetMemoryInExp offsets e = mapExp recurse e
  where recurse = identityMapper { mapOnBody = return . offsetMemoryInBody offsets
                                 }
