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

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Util
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, ExtLambda, FunDef, FParam, LParam, RetType)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

expandAllocations :: Pass ExplicitMemory ExplicitMemory
expandAllocations = simplePass
                    "expand allocations"
                    "Expand allocations" $
                    intraproceduralTransformation transformFunDef

transformFunDef :: MonadFreshNames m => FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFunDef fundec = do
  body' <- modifyNameSource $ runState m
  return fundec { funDefBody = body' }
  where m = transformBody $ funDefBody fundec

type ExpandM = State VNameSource

transformBody :: Body ExplicitMemory -> ExpandM (Body ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExplicitMemory -> ExpandM [Stm ExplicitMemory]

transformStm (Let pat () e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds ++ [Let pat () e']
  where transform = identityMapper { mapOnBody = const transformBody
                                   }

transformExp :: Exp ExplicitMemory -> ExpandM ([Stm ExplicitMemory], Exp ExplicitMemory)

transformExp (Op (Inner (Kernel cs space ts kbody)))
  | Right (kbody', thread_allocs) <- extractKernelBodyAllocations bound_in_kernel kbody = do

      (alloc_bnds, alloc_offsets) <-
        expandedAllocations num_threads global_tid thread_allocs
      let kbody'' = offsetMemoryInKernelBody alloc_offsets kbody'

      return (alloc_bnds,
              Op $ Inner $ Kernel cs space ts kbody'')

  where global_tid = spaceGlobalId space
        num_threads = spaceNumThreads space
        bound_in_kernel =
          HS.fromList $ HM.keys $ scopeOfKernelSpace space <>
          scopeOf (kernelBodyStms kbody)

transformExp e =
  return ([], e)

-- | Extract allocations from 'Thread' statements with
-- 'extractThreadAllocations'.
extractKernelBodyAllocations :: Names -> KernelBody InKernel
                             -> Either String (KernelBody InKernel,
                                               HM.HashMap VName (SubExp, Space))
extractKernelBodyAllocations bound_before_body kbody = do
  (allocs, stms) <- mapAccumLM extract HM.empty $ kernelBodyStms kbody
  return (kbody { kernelBodyStms = concat stms }, allocs)
  where extract allocs bnd = do
          (bnds, body_allocs) <- extractThreadAllocations bound_before_body [bnd]
          return (allocs <> body_allocs, bnds)

extractThreadAllocations :: Names -> [Stm InKernel]
                         -> Either String ([Stm InKernel], HM.HashMap VName (SubExp, Space))
extractThreadAllocations bound_before_body bnds = do
  (allocs, bnds') <- mapAccumLM isAlloc HM.empty bnds
  return (catMaybes bnds', allocs)
  where bound_here = bound_before_body `HS.union` boundByStms bnds

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
                    -> ExpandM ([Stm ExplicitMemory], RebaseMap)
expandedAllocations num_threads thread_index thread_allocs = do
  -- We expand the allocations by multiplying their size with the
  -- number of kernel threads.
  (alloc_bnds, rebase_map) <- unzip <$> mapM expand (HM.toList thread_allocs)

  -- Fix every reference to the memory blocks to be offset by the
  -- thread number.
  let alloc_offsets =
        RebaseMap { rebaseMap = mconcat rebase_map
                  , indexVariable = thread_index
                  , kernelWidth = num_threads
                  }
  return (concat alloc_bnds, alloc_offsets)
  where expand (mem, (per_thread_size, Space "local")) = do
          let allocpat = Pattern [] [PatElem mem BindVar $
                                     MemMem per_thread_size $ Space "local"]
          return ([Let allocpat () $ Op $ Alloc per_thread_size $ Space "local"],
                  mempty)

        expand (mem, (per_thread_size, space)) = do
          total_size <- newVName "total_size"
          let sizepat = Pattern [] [PatElem total_size BindVar $ Scalar int32]
              allocpat = Pattern [] [PatElem mem BindVar $
                                     MemMem (Var total_size) space]
          return ([Let sizepat () $ BasicOp $ BinOp (Mul Int32) num_threads per_thread_size,
                   Let allocpat () $ Op $ Alloc (Var total_size) space],
                   HM.singleton mem newBase)

        newBase old_shape =
          let perm = [length old_shape, 0] ++ [1..length old_shape-1]
              root_ixfun = IxFun.iota (old_shape ++ [primExpFromSubExp int32 num_threads])
              permuted_ixfun = IxFun.permute root_ixfun perm
              offset_ixfun = IxFun.slice permuted_ixfun $
                             fullSliceNum (IxFun.shape permuted_ixfun)
                             [DimFix $ LeafExp thread_index int32]
          in offset_ixfun

data RebaseMap = RebaseMap {
    rebaseMap :: HM.HashMap VName ([PrimExp VName] -> IxFun)
    -- ^ A map from memory block names to new index function bases.
  , indexVariable :: VName
  , kernelWidth :: SubExp
  }

lookupNewBase :: VName -> [PrimExp VName] -> RebaseMap -> Maybe IxFun
lookupNewBase name dims = fmap ($dims) . HM.lookup name . rebaseMap

offsetMemoryInKernelBody :: RebaseMap -> KernelBody InKernel
                         -> KernelBody InKernel
offsetMemoryInKernelBody initial_offsets kbody =
  kbody { kernelBodyStms = stms' }
  where stms' = snd $ mapAccumL offsetMemoryInStm initial_offsets $ kernelBodyStms kbody

offsetMemoryInBody :: RebaseMap -> Body InKernel -> Body InKernel
offsetMemoryInBody offsets (Body attr bnds res) =
  Body attr (snd $ mapAccumL offsetMemoryInStm offsets bnds) res

offsetMemoryInStm :: RebaseMap -> Stm InKernel
                      -> (RebaseMap, Stm InKernel)
offsetMemoryInStm offsets (Let pat attr e) =
  (offsets', Let pat' attr $ offsetMemoryInExp offsets e)
  where (offsets', pat') = offsetMemoryInPattern offsets pat

offsetMemoryInPattern :: RebaseMap -> Pattern InKernel -> (RebaseMap, Pattern InKernel)
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
  | Just new_base <- lookupNewBase mem (IxFun.base ixfun) offsets =
      ArrayMem bt shape u mem $ IxFun.rebase new_base ixfun
offsetMemoryInMemBound _ summary =
  summary

offsetMemoryInExp :: RebaseMap -> Exp InKernel -> Exp InKernel
offsetMemoryInExp offsets (DoLoop ctx val form body) =
  DoLoop (zip ctxparams' ctxinit) (zip valparams' valinit) form body'
  where (ctxparams, ctxinit) = unzip ctx
        (valparams, valinit) = unzip val
        body' = offsetMemoryInBody offsets body
        ctxparams' = map (offsetMemoryInParam offsets) ctxparams
        valparams' = map (offsetMemoryInParam offsets) valparams
offsetMemoryInExp offsets (Op (Inner (GroupStream w max_chunk lam accs arrs))) =
  Op (Inner (GroupStream w max_chunk lam' accs arrs))
  where lam' =
          lam { groupStreamLambdaBody = offsetMemoryInBody offsets $
                                        groupStreamLambdaBody lam
              , groupStreamAccParams = map (offsetMemoryInParam offsets) $
                                       groupStreamAccParams lam
              , groupStreamArrParams = map (offsetMemoryInParam offsets) $
                                       groupStreamArrParams lam
              }
offsetMemoryInExp offsets (Op (Inner (GroupReduce w lam input))) =
  Op (Inner (GroupReduce w lam' input))
  where lam' = lam { lambdaBody = offsetMemoryInBody offsets $ lambdaBody lam }
offsetMemoryInExp offsets e = mapExp recurse e
  where recurse = identityMapper { mapOnBody = const $ return . offsetMemoryInBody offsets
                                 }
