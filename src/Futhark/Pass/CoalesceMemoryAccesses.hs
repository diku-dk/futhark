{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Modify how arrays are represented and accessed to ensure
-- coalesced memory access from within GPU kernels.
--
-- This module is presently embryonic: all it does is fiddle with the
-- index functions of 'ChunkedMapKernel's to ensure that their
-- writeback is coalesced.  Most of the actual coalescing work is done
-- in hacky ways in KernelBabysitting and in slightly less hacky ways
-- in ExpandAllocations - in time, we wish to move as much as possible
-- in here, and do it properly.
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
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun

coalesceMemoryAccesses :: Pass ExplicitMemory ExplicitMemory
coalesceMemoryAccesses =
  simplePass
  "coalesce memory accesses"
  "Coalesce memory accesses" $
  intraproceduralTransformation transformFunDef

transformFunDef :: MonadFreshNames m => FunDef -> m FunDef
transformFunDef fundec = do
  body' <- modifyNameSource $ runState (runReaderT m $ scopeOf fundec)
  return fundec { funDefBody = body' }
  where m = transformBody $ funDefBody fundec

type ExpandM = ReaderT (Scope ExplicitMemory) (State VNameSource)

transformBody :: Body -> ExpandM Body
transformBody (Body () bnds res) = inScopeOf bnds $ do
  bnds' <- concat <$> mapM transformBinding bnds
  return $ Body () bnds' res

transformBinding :: Binding -> ExpandM [Binding]

transformBinding (Let (Pattern [] pat_elems) ()
                  e@(Op (Inner (ChunkedMapKernel _ _ _ _ lam _)))) = do
  -- We create a new pattern for the kernel that is similar to the old
  -- one, but transposed.  Then we add transpose operations following
  -- the kernel to re-create the original arrays.
  let (nonconcat_pat_elems, concat_pat_elems) =
        splitAt (chunkedKernelNonconcatOutputs lam) pat_elems
  (alloc_bnds, nonconcat_pat_elems', tr_bnds) <- unzip3 <$> mapM transposePatElem nonconcat_pat_elems
  return $
    alloc_bnds ++
    [Let (Pattern [] (nonconcat_pat_elems' ++ concat_pat_elems)) () e] ++
    tr_bnds
  where transposePatElem pat_elem = do
          name <- newVName (baseString (patElemName pat_elem) ++ "_transposed")
          case patElemAttr pat_elem of
            ArrayMem bt (Shape old_dims) u mem _old_ixfun -> do

              (size, space) <- lookupMem mem
              coalescing_mem <- newVName "coalescing_mem"
              let alloc_pat_elem = PatElem coalescing_mem BindVar $ MemMem size space

              let perm = [1..length old_dims-1] ++ [0]
                  tr_dims = rearrangeShape perm old_dims
                  attr = ArrayMem bt (Shape old_dims) u coalescing_mem $
                         IxFun.permute (IxFun.iota $ IxFun.shapeFromSubExps tr_dims)
                         (rearrangeInverse perm)
              return (Let (Pattern [] [alloc_pat_elem]) () $ Op $ Alloc size space,

                      PatElem name BindVar attr,

                      Let (Pattern [] [pat_elem]) () $ PrimOp $ Copy name)
            attr ->
              fail $ "Invalid attribute for let-binding of ChunkedMapKernel return: " ++ pretty attr

        lookupMem mem = do
          t <- lookupType mem
          case t of Mem size space -> return (size, space)
                    _              -> fail $ "CoalesceMemoryAccesses.lookupMem: " ++
                                              pretty mem ++
                                              " is supposed to be a memory block, but has type "
                                              ++ pretty t

transformBinding (Let pat () e) = do
  e' <- mapExpM transform e
  return [Let pat () e']
  where transform = identityMapper { mapOnBody = transformBody
                                   }
