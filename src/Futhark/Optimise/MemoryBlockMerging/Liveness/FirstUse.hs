{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find first uses for all memory blocks.
--
-- Array creation points.  Maps statements to memory block names.
--
-- A memory block can have more than one first use.
module Futhark.Optimise.MemoryBlockMerging.Liveness.FirstUse
  ( findFirstUses
  , createsNewArrayBase
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


data Context = Context
  { ctxVarToMem :: VarMemMappings MemorySrc
  , ctxMemAliases :: MemAliases
  , ctxCurOuterFirstUses :: Names
    -- ^ First uses found in outer bodies.
  }
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: RWS Context () FirstUses a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter (),
            MonadState FirstUses)

type LoreConstraints lore = (ExplicitMemorish lore,
                             ArrayUtils lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- Find the memory blocks used or aliased by a variable.
varMems :: VName -> FindM lore MNames
varMems var = do
  var_to_mem <- asks ctxVarToMem
  mem_aliases <- asks ctxMemAliases
  return $ fromMaybe S.empty $ do
    mem <- memSrcName <$> M.lookup var var_to_mem
    return $ S.union (S.singleton mem) $ lookupEmptyable mem mem_aliases

recordMapping :: VName -> MName -> FindM lore ()
recordMapping stmt_var mem =
  modify $ M.unionWith S.union (M.singleton stmt_var $ S.singleton mem)

-- | Find all first uses of *memory blocks* in a function definition.
findFirstUses :: VarMemMappings MemorySrc -> MemAliases
              -> FunDef ExplicitMemory -> FirstUses
findFirstUses var_to_mem mem_aliases fundef =
  let context = Context { ctxVarToMem = var_to_mem
                        , ctxMemAliases = mem_aliases
                        , ctxCurOuterFirstUses = S.empty
                        }
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFunDefFParam
        lookInBody $ funDefBody fundef
      first_uses = removeEmptyMaps $ expandWithAliases mem_aliases
                   $ fst $ execRWS m context M.empty
  in first_uses

lookInFunDefFParam :: LoreConstraints lore =>
                      FParam lore -> FindM lore ()
lookInFunDefFParam (Param x (ExpMem.MemArray _ _ _ (ExpMem.ArrayIn xmem _))) =
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
  outer_first_uses <- asks ctxCurOuterFirstUses
  when (createsNewArray e) $ do
    let e_free_vars = freeInExp e
    e_mems <- S.unions <$> mapM varMems (S.toList e_free_vars)
    forM_ patvalelems $ \(PatElem x membound) ->
      case membound of
        ExpMem.MemArray _ _ _ (ExpMem.ArrayIn xmem _) -> do
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
            --
            -- If it is a first use of a memory inside a loop or a kernel, and
            -- that memory already has a first use outside the loop, ignore it,
            -- since it is not a proper first use.  This can be an issue after
            -- the coalescing transformation, where multidimensional maps are
            -- first-order-transformed into nested loops, each loop having its
            -- own Scratch expression.  FIXME: This might be too conservative
            -- for multiple liveness intervals, but it does not seem to be a
            -- problem with our tests.  It is quite possible that this case only
            -- occurs because the coalescing pass does not remove the inner
            -- scratches, so maybe it should be fixed there.
            $ unless (xmem `S.member` outer_first_uses)
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

  cur_first_uses <- get
  local (\ctx -> ctx { ctxCurOuterFirstUses = S.unions $ M.elems cur_first_uses })
    $ fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }

lookInPatCtxElem :: LoreConstraints lore =>
                    VName -> PatElem lore -> FindM lore ()
lookInPatCtxElem x (PatElem xmem ExpMem.MemMem{}) =
  recordMapping x xmem
lookInPatCtxElem _ _ = return ()

lookInMergeCtxParam :: LoreConstraints lore =>
                       VName -> (FParam lore, SubExp) -> FindM lore ()
lookInMergeCtxParam x (Param xmem ExpMem.MemMem{}, _) =
  recordMapping x xmem
lookInMergeCtxParam _ _ = return ()

class ArrayUtils lore where
  -- Does an expression constitute a new array?
  createsNewArray :: Exp lore -> Bool

createsNewArrayBase :: Exp lore -> Bool
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

instance ArrayUtils ExplicitMemory where
  createsNewArray e = case e of
    Op (ExpMem.Inner ExpMem.Kernel{}) -> True
    _ -> createsNewArrayBase e

instance ArrayUtils InKernel where
  createsNewArray e = case e of
    Op (ExpMem.Inner ExpMem.GroupReduce{}) -> True
    Op (ExpMem.Inner ExpMem.GroupScan{}) -> True
    Op (ExpMem.Inner ExpMem.GroupStream{}) -> True
    Op (ExpMem.Inner ExpMem.Combine{}) -> True
    _ -> createsNewArrayBase e
