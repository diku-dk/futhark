{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Expand allocations inside of maps when possible.
module Futhark.Pass.ExpandAllocations
       ( expandAllocations )
       where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

transformStm (Let pat aux e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds ++ [Let pat aux e']
  where transform = identityMapper { mapOnBody = const transformBody
                                   }

transformExp :: Exp ExplicitMemory -> ExpandM ([Stm ExplicitMemory], Exp ExplicitMemory)

transformExp (Op (Inner (Kernel desc space ts kbody)))
  | Right (kbody', thread_allocs) <- extractKernelBodyAllocations bound_in_kernel kbody = do

      num_threads64 <- newVName "num_threads64"
      let num_threads64_pat = Pattern [] [PatElem num_threads64 BindVar $ Scalar int64]
          num_threads64_bnd = Let num_threads64_pat (defAux ()) $ BasicOp $
                              ConvOp (SExt Int32 Int64) (spaceNumThreads space)

      (alloc_bnds, alloc_offsets) <-
        expandedAllocations
        (Var num_threads64, spaceNumGroups space, spaceGroupSize space)
        (spaceGlobalId space, spaceGroupId space, spaceLocalId space) thread_allocs
      let kbody'' = offsetMemoryInKernelBody alloc_offsets kbody'

      return (num_threads64_bnd : alloc_bnds,
              Op $ Inner $ Kernel desc space ts kbody'')

  where bound_in_kernel =
          S.fromList $ M.keys $ scopeOfKernelSpace space <>
          scopeOf (kernelBodyStms kbody)

transformExp e =
  return ([], e)

-- | Extract allocations from 'Thread' statements with
-- 'extractThreadAllocations'.
extractKernelBodyAllocations :: Names -> KernelBody InKernel
                             -> Either String (KernelBody InKernel,
                                               M.Map VName (SubExp, Space))
extractKernelBodyAllocations bound_before_body kbody = do
  (allocs, stms) <- mapAccumLM extract M.empty $ kernelBodyStms kbody
  return (kbody { kernelBodyStms = concat stms }, allocs)
  where extract allocs bnd = do
          (bnds, body_allocs) <- extractThreadAllocations bound_before_body [bnd]
          return (allocs <> body_allocs, bnds)

extractThreadAllocations :: Names -> [Stm InKernel]
                         -> Either String ([Stm InKernel], M.Map VName (SubExp, Space))
extractThreadAllocations bound_before_body bnds = do
  (allocs, bnds') <- mapAccumLM isAlloc M.empty bnds
  return (catMaybes bnds', allocs)
  where bound_here = bound_before_body `S.union` boundByStms bnds

        isAlloc _ (Let (Pattern [] [patElem]) _ (Op (Alloc (Var v) _)))
          | v `S.member` bound_here =
            throwError $ "Size " ++ pretty v ++
            " for block " ++ pretty patElem ++
            " is not lambda-invariant"

        isAlloc allocs (Let (Pattern [] [patElem]) _ (Op (Alloc size space))) =
          return (M.insert (patElemName patElem) (size, space) allocs,
                  Nothing)

        isAlloc allocs bnd =
          return (allocs, Just bnd)

expandedAllocations :: (SubExp,SubExp, SubExp)
                    -> (VName, VName, VName)
                    -> M.Map VName (SubExp, Space)
                    -> ExpandM ([Stm ExplicitMemory], RebaseMap)
expandedAllocations (num_threads64, num_groups, group_size) (_thread_index, group_id, local_id) thread_allocs = do
  -- We expand the allocations by multiplying their size with the
  -- number of kernel threads.
  (alloc_bnds, rebase_map) <- unzip <$> mapM expand (M.toList thread_allocs)

  -- Fix every reference to the memory blocks to be offset by the
  -- thread number.
  let alloc_offsets =
        RebaseMap { rebaseMap = mconcat rebase_map
                  , indexVariable = (group_id, local_id)
                  , kernelWidth = (num_groups, group_size)
                  }
  return (concat alloc_bnds, alloc_offsets)
  where expand (mem, (per_thread_size, Space "local")) = do
          let allocpat = Pattern [] [PatElem mem BindVar $
                                     MemMem per_thread_size $ Space "local"]
          return ([Let allocpat (defAux ()) $ Op $ Alloc per_thread_size $ Space "local"],
                  mempty)

        expand (mem, (per_thread_size, space)) = do
          total_size <- newVName "total_size"
          let sizepat = Pattern [] [PatElem total_size BindVar $ Scalar int64]
              allocpat = Pattern [] [PatElem mem BindVar $
                                     MemMem (Var total_size) space]
          return ([Let sizepat (defAux ()) $
                    BasicOp $ BinOp (Mul Int64) num_threads64 per_thread_size,
                   Let allocpat (defAux ()) $
                    Op $ Alloc (Var total_size) space],
                   M.singleton mem newBase)

        newBase old_shape =
          let num_dims = length old_shape
              perm = [0, num_dims+1] ++ [1..num_dims]
              root_ixfun = IxFun.iota (primExpFromSubExp int32 num_groups : old_shape
                                       ++ [primExpFromSubExp int32 group_size])
              permuted_ixfun = IxFun.permute root_ixfun perm
              untouched d = DimSlice 0 d 1
              offset_ixfun = IxFun.slice permuted_ixfun $
                             [DimFix (LeafExp group_id int32),
                              DimFix (LeafExp local_id int32)] ++
                             map untouched old_shape
          in offset_ixfun

data RebaseMap = RebaseMap {
    rebaseMap :: M.Map VName ([PrimExp VName] -> IxFun)
    -- ^ A map from memory block names to new index function bases.
  , indexVariable :: (VName, VName)
  , kernelWidth :: (SubExp, SubExp)
  }

lookupNewBase :: VName -> [PrimExp VName] -> RebaseMap -> Maybe IxFun
lookupNewBase name dims = fmap ($dims) . M.lookup name . rebaseMap

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
offsetMemoryInExp offsets (Op (Inner (Combine cspace ts active body))) =
  Op $ Inner $ Combine cspace ts active $ offsetMemoryInBody offsets body
offsetMemoryInExp offsets e = mapExp recurse e
  where recurse = identityMapper { mapOnBody = const $ return . offsetMemoryInBody offsets
                                 }
