{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find first uses for all memory blocks.
--
-- A memory block can have more than one first use.
module Futhark.Optimise.MemoryBlockMerging.Liveness.FirstUse
  ( findFirstUses
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
  (ExplicitMemory, InKernel, ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


type FirstUsesList = [FirstUses]
-- Nicer approach: Make a new Monoid instance for a M.Map wrapper type.  We need
-- the 'M.unionsWith S.union' functionality; the default `M.unions` is too
-- destructive.

getFirstUsesMap :: FirstUsesList -> FirstUses
getFirstUsesMap = M.unionsWith S.union

data Context = Context (VarMemMappings MemorySrc) MemAliases
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: RWS Context FirstUsesList () a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter FirstUsesList)

type LoreConstraints lore = (ExplicitMemorish lore,
                             ArrayUtils lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- Find the memory blocks used or aliased by a variable.
varMems :: VName -> FindM lore Names
varMems var = do
  Context var_to_mem mem_aliases <- ask
  return $ fromMaybe S.empty $ do
    mem <- memSrcName <$> M.lookup var var_to_mem
    return $ S.union (S.singleton mem) $ lookupEmptyable mem mem_aliases

recordMapping :: VName -> VName -> FindM lore ()
recordMapping stmt_var mem = tell [M.singleton stmt_var (S.singleton mem)]

findFirstUses :: LoreConstraints lore =>
                 VarMemMappings MemorySrc -> MemAliases
              -> FunDef lore -> FirstUses
findFirstUses var_to_mem mem_aliases fundef =
  let context = Context var_to_mem mem_aliases
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFunDefFParam
        lookInBody $ funDefBody fundef
      first_uses = cleanupMapping $ expandWithAliases mem_aliases
                   $ getFirstUsesMap $ snd $ evalRWS m context ()
  in first_uses

lookInFunDefFParam :: LoreConstraints lore =>
                      FParam lore -> FindM lore ()
lookInFunDefFParam (Param x (ExpMem.ArrayMem _ _ _ xmem _)) =
  recordMapping x xmem
lookInFunDefFParam _ = return ()

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
  when (createsNewArray e) $ do
    let e_free_vars = freeInExp e
    e_mems <- S.unions <$> mapM varMems (S.toList e_free_vars)
    forM_ patvalelems $ \(PatElem x bindage membound) ->
      case (bindage, membound) of
        (BindVar, ExpMem.ArrayMem _ _ _ xmem _) -> do
          x_mems <- varMems xmem

          -- For the first use to be a proper first use, it must write to
          -- the memory, but not read from it.  We need to check this to
          -- support multiple liveness intervals.  If we don't check this,
          -- the last use analysis and the interference analysis might end
          -- up wrong.
          when (S.null $ S.intersection x_mems e_mems)
            -- We only record the mapping between the statement and the
            -- memory block, not any of its aliased memory blocks.  They
            -- would not be aliased unless they are themselves created at
            -- some point, so they will get their own FirstUses.  Putting
            -- them into first use here would probably also be too
            -- conservative.
            $ recordMapping x xmem
        _ -> return ()

  -- Find first uses of existential memory blocks.  Fairly conservative.
  -- Covers the case where a loop uses multiple arrays by saying every
  -- existential memory block overlaps with every result memory block.  Fine
  -- for now.
  forM_ patctxelems
      $ \p -> forM_ patvalelems
              $ \el -> lookInPatCtxElem (patElemName el) p
  case e of
    DoLoop mergectxparams _mergevalparams _loopform _body ->
      forM_ mergectxparams
      $ \p -> forM_ patvalelems
              $ \el -> lookInMergeCtxParam (patElemName el) p
    _ -> return ()

  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          }

lookInPatCtxElem :: LoreConstraints lore =>
                    VName -> PatElem lore -> FindM lore ()
lookInPatCtxElem x (PatElem xmem _bindage ExpMem.MemMem{}) =
  recordMapping x xmem
lookInPatCtxElem _ _ = return ()

lookInMergeCtxParam :: LoreConstraints lore =>
                       VName -> (FParam lore, SubExp) -> FindM lore ()
lookInMergeCtxParam x (Param xmem ExpMem.MemMem{}, _) =
  recordMapping x xmem
lookInMergeCtxParam _ _ = return ()


createsNewArrayWithoutKernel :: Exp ExplicitMemory -> Bool
createsNewArrayWithoutKernel e = case e of
  Op (ExpMem.Inner ExpMem.Kernel{}) -> True -- Necessary?
  _ -> createsNewArrayBase e

createsNewArrayInKernel :: Exp InKernel -> Bool
createsNewArrayInKernel e = case e of
  Op (ExpMem.Inner ExpMem.GroupReduce{}) -> True
  Op (ExpMem.Inner ExpMem.GroupScan{}) -> True
  Op (ExpMem.Inner ExpMem.GroupStream{}) -> True
  Op (ExpMem.Inner ExpMem.Combine{}) -> True
  _ -> createsNewArrayBase e

createsNewArrayBase :: ExplicitMemorish lore
                    => Exp lore -> Bool
createsNewArrayBase e = case e of
  BasicOp Partition{} -> True
  BasicOp Replicate{} -> True
  BasicOp Iota{} -> True
  BasicOp Manifest{} -> True
  BasicOp ExpMem.Copy{} -> True
  BasicOp Concat{} -> True
  BasicOp ArrayLit{} -> True
  BasicOp Scratch{} -> True
  _ -> False

class ArrayUtils lore where
  createsNewArray :: Exp lore -> Bool

instance ArrayUtils ExplicitMemory where
  createsNewArray = createsNewArrayWithoutKernel

instance ArrayUtils InKernel where
  createsNewArray = createsNewArrayInKernel
