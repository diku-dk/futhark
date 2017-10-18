{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find all Alloc statements and associate their memory blocks with the
-- allocation size.
module Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes
  ( memBlockSizesFunDef, memBlockSizesParamsBodyNonRec
  , Sizes
  ) where

import qualified Data.Map.Strict as M
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
  (ExplicitMemorish, ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


-- | maps memory blocks to its size and space/type
type Sizes = M.Map MName (SubExp, Space) -- Also Space information

newtype FindM lore a = FindM { unFindM :: Writer Sizes a }
  deriving (Monad, Functor, Applicative,
            MonadWriter Sizes)

type LoreConstraints lore = (ExplicitMemorish lore,
                             AllocSizeUtils lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

recordMapping :: VName -> (SubExp, Space) -> FindM lore ()
recordMapping var (size, space) = tell $ M.singleton var (size, space)

memBlockSizesFunDef :: LoreConstraints lore =>
                       FunDef lore -> Sizes
memBlockSizesFunDef fundef =
  let m = unFindM $ do
        mapM_ lookInFParam $ funDefParams fundef
        lookInBody $ funDefBody fundef
      mem_sizes = execWriter m
  in mem_sizes

memBlockSizesParamsBodyNonRec :: LoreConstraints lore =>
                                 [FParam lore] -> Body lore -> Sizes
memBlockSizesParamsBodyNonRec params body =
  let m = unFindM $ do
        mapM_ lookInFParam params
        mapM_ lookInStm $ bodyStms body
      mem_sizes = execWriter m
  in mem_sizes

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param mem (ExpMem.MemMem size space)) =
  recordMapping mem (size, space)
lookInFParam _ = return ()

lookInLParam :: LoreConstraints lore =>
                LParam lore -> FindM lore ()
lookInLParam (Param mem (ExpMem.MemMem size space)) =
  recordMapping mem (size, space)
lookInLParam _ = return ()

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStmRec bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStmRec bnds

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  case patvalelems of
    [PatElem mem _ _] ->
      case lookForAllocSize e of
        Just (size, space) ->
          recordMapping mem (size, space)
        Nothing -> return ()
    _ -> return ()
  mapM_ lookInPatCtxElem patctxelems

lookInStmRec :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStmRec stm@(Let _ _ e) = do
  lookInStm stm

  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          , walkOnLParam = lookInLParam
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInLambda
          , walkOnKernelLParam = lookInLParam
          }

lookInPatCtxElem :: LoreConstraints lore =>
                    PatElem lore -> FindM lore ()
lookInPatCtxElem (PatElem mem _bindage (ExpMem.MemMem size space)) =
  recordMapping mem (size, space)
lookInPatCtxElem _ = return ()

lookInLambda :: LoreConstraints lore =>
                Lambda lore -> FindM lore ()
lookInLambda (Lambda params body _) = do
  forM_ params lookInLParam
  lookInBody body

class AllocSizeUtils lore where
  lookForAllocSize :: Exp lore -> Maybe (SubExp, Space)

instance AllocSizeUtils ExplicitMemory where
  lookForAllocSize (Op (ExpMem.Alloc size space)) = Just (size, space)
  lookForAllocSize _ = Nothing

instance AllocSizeUtils InKernel where
  -- There can be no allocations inside kernels.
  lookForAllocSize _ = Nothing
