{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find safety condition 2 for all statements.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition2
  ( findSafetyCondition2FunDef
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (
  ExplicitMemory, InKernel, ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


type CurrentAllocatedBlocks = MNames
type AllocatedBlocksBeforeCreation = M.Map VName MNames

newtype FindM lore a = FindM { unFindM :: RWS ()
                               AllocatedBlocksBeforeCreation CurrentAllocatedBlocks a }
  deriving (Monad, Functor, Applicative,
            MonadWriter AllocatedBlocksBeforeCreation,
            MonadState CurrentAllocatedBlocks)

type LoreConstraints lore = (ExplicitMemorish lore,
                             IsAlloc lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

findSafetyCondition2FunDef :: FunDef ExplicitMemory
                           -> AllocatedBlocksBeforeCreation
findSafetyCondition2FunDef fundef =
  let m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m () S.empty
  in res

lookInFParam :: LoreConstraints lore =>
                FParam ExplicitMemory -> FindM lore ()
lookInFParam (Param _ membound) =
  -- Unique array function parameters also count as "allocations" in which
  -- memory can be coalesced.
  case membound of
    ExpMem.MemArray _ _ Unique (ExpMem.ArrayIn mem _) ->
      modify $ S.insert mem
    _ -> return ()

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  let new_decls0 = map patElemName (patctxelems ++ patvalelems)
      new_decls1 = case e of
        DoLoop _mergectxparams mergevalparams _loopform _body ->
          -- Technically not a declaration for the current expression, but very
          -- close, and hopefully okay to consider it as one.
          map (paramName . fst) mergevalparams
        _ -> []
      new_decls = new_decls0 ++ new_decls1

  cur_allocated_blocks <- get
  forM_ new_decls $ \x ->
    tell $ M.singleton x cur_allocated_blocks

  case patvalelems of
    [PatElem mem _] ->
      when (isAlloc e) $ modify $ S.insert mem
    _ -> return ()

  -- RECURSIVE BODY WALK.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }

class IsAlloc lore where
  isAlloc :: Exp lore -> Bool

instance IsAlloc ExplicitMemory where
  isAlloc (Op ExpMem.Alloc{}) = True
  isAlloc _ = False

instance IsAlloc InKernel where
  isAlloc _ = False
