{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Modify how arrays are represented and accessed to ensure
-- coalesced memory access from within GPU kernels.
--
-- This module is presently embryonic: all it does is fiddle with the
-- index functions of some kernels to ensure that their writeback is
-- coalesced.  Most of the actual coalescing work is done in hacky
-- ways in KernelBabysitting and in slightly less hacky ways in
-- ExpandAllocations - in time, we wish to move as much as possible in
-- here, and do it properly.
--
-- This module plays fast and loose with the symbol table (only puts
-- in things that are let-bound) - if you get strange errors about
-- unknown variables, that means you have to fix it to also include
-- lambda-bound and loop indices.
module Futhark.Pass.CoalesceMemoryAccesses
       ( coalesceMemoryAccesses )
       where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.List

import Prelude hiding (div, quot)

import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import Futhark.Tools
import Futhark.Pass
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun

coalesceMemoryAccesses :: Pass ExplicitMemory ExplicitMemory
coalesceMemoryAccesses =
  simplePass
  "coalesce memory accesses"
  "Coalesce memory accesses" $
  intraproceduralTransformation transformFunDef

transformFunDef :: MonadFreshNames m => FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFunDef fundec = do
  body' <- modifyNameSource $ runState (runReaderT m $ scopeOf fundec)
  return fundec { funDefBody = body' }
  where m = transformBody $ funDefBody fundec

type ExpandM = ReaderT (Scope ExplicitMemory) (State VNameSource)

transformBody :: Body ExplicitMemory -> ExpandM (Body ExplicitMemory)
transformBody (Body () bnds res) = inScopeOf bnds $ do
  bnds' <- concat <$> mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExplicitMemory -> ExpandM [Stm ExplicitMemory]

transformStm (Let pat () e)
  | Just (scanred_pat_elems, map_pat_elems, size) <- scanOrReduce pat e,
    not $ null map_pat_elems = do
      (alloc_bnds, map_pat_elems', tr_bnds) <-
        unzip3 <$> mapM (transposePatElem size) map_pat_elems
      return $
        alloc_bnds ++
        [Let (Pattern [] (scanred_pat_elems ++ map_pat_elems')) () e] ++
        tr_bnds
  where transposePatElem size pat_elem = do
          name <- newVName (baseString (patElemName pat_elem) ++ "_transposed")
          case patElemAttr pat_elem of
            ArrayMem bt (Shape old_dims) u mem _old_ixfun -> do
              (memsize, space) <- lookupMem mem
              coalescing_mem <- newVName "coalescing_mem"
              let alloc_pat_elem = PatElem coalescing_mem BindVar $ MemMem memsize space

              let attr
                    | length old_dims == 1 =
                        let imaginary_dims = kernelNumThreads size : kernelElementsPerThread size :
                                             drop 1 old_dims
                            perm = [1..length imaginary_dims-1] ++ [0]
                            tr_dims = rearrangeShape perm imaginary_dims
                        in ArrayMem bt (Shape old_dims) u coalescing_mem $
                           IxFun.reshape (IxFun.permute
                                           (IxFun.iota $ map (primExpFromSubExp int32) tr_dims)
                                          (rearrangeInverse perm)) $
                           map (DimNew . primExpFromSubExp int32) old_dims
                    | otherwise =
                        let perm = [1..length old_dims-1] ++ [0]
                            tr_dims = rearrangeShape perm old_dims
                        in ArrayMem bt (Shape old_dims) u coalescing_mem $
                           IxFun.permute (IxFun.iota $ map (primExpFromSubExp int32) tr_dims)
                           (rearrangeInverse perm)

              return (Let (Pattern [] [alloc_pat_elem]) () $ Op $ Alloc memsize space,

                      PatElem name BindVar attr,

                      Let (Pattern [] [pat_elem]) () $ BasicOp $ Copy name)
            attr ->
              fail $ "Invalid attribute for let-binding of scan or reduce kernel return: " ++ pretty attr


transformStm (Let pat () e) = do
  e' <- mapExpM transform e
  return [Let pat () e']
  where transform = identityMapper { mapOnBody = const transformBody
                                   }

scanOrReduce :: Pattern ExplicitMemory -> Exp ExplicitMemory
             -> Maybe ([PatElem ExplicitMemory],
                       [PatElem ExplicitMemory],
                       KernelSize)
scanOrReduce (Pattern [] pat_elems) (Op (Inner (ScanKernel _ _ size _ lam _ _))) =
  let (scan_pat_elems, map_pat_elems) =
        splitAt (2 * length (lambdaReturnType lam)) pat_elems
  in Just (scan_pat_elems, map_pat_elems, size)
scanOrReduce _ _ =
  Nothing

lookupMem :: (Monad m, HasScope lore m) => VName -> m (SubExp, Space)
lookupMem mem = do
  t <- lookupType mem
  case t of Mem size space -> return (size, space)
            _              -> fail $ "CoalesceMemoryAccesses.lookupMem: " ++
                                      pretty mem ++
                                      " is supposed to be a memory block, but has type "
                                      ++ pretty t
