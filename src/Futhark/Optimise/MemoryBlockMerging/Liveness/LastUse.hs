{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find last uses for all memory blocks.
--
-- A memory block can have more than one last use.
module Futhark.Optimise.MemoryBlockMerging.Liveness.LastUse
  ( findLastUses
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


type LastUsesList = [LastUses]

getLastUsesMap :: LastUsesList -> LastUses
getLastUsesMap = M.unionsWith S.union

-- Mapping from a memory block to its currently assumed last use statement
-- variable.
type OptimisticLastUses = M.Map VName VName

data Context = Context
  { _ctxVarToMem :: VarMemMappings MemorySrc
  , _ctxMemAliases :: MemAliases
  , ctxFirstUses :: FirstUses
  , ctxCurFirstUsesBeforeLoop :: Names
  }
  deriving (Show)

data Current = Current
  { curOptimisticLastUses :: OptimisticLastUses
  , curFirstUses :: Names
  }
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: RWS Context LastUsesList Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter LastUsesList,
            MonadState Current)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- Find the memory blocks used or aliased by a variable.
varMems :: VName -> FindM lore Names
varMems var = do
  Context var_to_mem mem_aliases _ _ <- ask
  return $ fromMaybe S.empty $ do
    mem <- memSrcName <$> M.lookup var var_to_mem
    return $ S.union (S.singleton mem) $ lookupEmptyable mem mem_aliases

withLocalCurFirstUses :: FindM lore a -> FindM lore a
withLocalCurFirstUses m = do
  cur_first_uses <- gets curFirstUses
  res <- m
  modify $ \c -> c { curFirstUses = cur_first_uses }
  return res

recordMapping :: VName -> VName -> FindM lore ()
recordMapping stmt_var mem = tell [M.singleton stmt_var (S.singleton mem)]

setOptimistic :: VName -> VName -> FindM lore ()
setOptimistic mem x_lu = modify $ \c ->
  c { curOptimisticLastUses = M.insert mem x_lu
                              $ curOptimisticLastUses c }

commitOptimistic :: VName -> FindM lore ()
commitOptimistic mem = do
  res <- M.lookup mem <$> gets curOptimisticLastUses
  case res of
    Just x_lu -> recordMapping x_lu mem
    Nothing -> return ()

findLastUses :: LoreConstraints lore =>
                VarMemMappings MemorySrc -> MemAliases -> FirstUses
             -> FunDef lore -> LastUses
findLastUses var_to_mem mem_aliases first_uses fundef =
  let context = Context var_to_mem mem_aliases first_uses S.empty
      m = unFindM $ do
        -- We do not need to look in the function parameters, as they should not
        -- contain last uses -- in that case they would have been simplified away.
        lookInBody $ funDefBody fundef
        optimistics <- gets curOptimisticLastUses
        forM_ (M.assocs optimistics) $ \(mem, x_lu) ->
          recordMapping x_lu mem

      last_uses = cleanupMapping $ expandWithAliases mem_aliases $ getLastUsesMap
                  $ snd $ evalRWS m context (Current M.empty S.empty)
  in last_uses

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
lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
  -- First handle all pattern elements by themselves.
  forM_ patvalelems $ \(PatElem x _ membound) ->
    case membound of
      ExpMem.ArrayMem _ _ _ xmem _ -> do
        first_uses_x <- lookupEmptyable x <$> asks ctxFirstUses
        modify $ \c -> c { curFirstUses = S.union first_uses_x $ curFirstUses c }
        -- When this is a new first use of a memory block, commit the previous
        -- optimistic last use of it, so that it can be considered unused in
        -- the statements inbetween.  FIXME: Aliasing problems?  Edge cases?
        when (S.member xmem first_uses_x) $ commitOptimistic xmem
      _ -> return ()

  -- Then find the new memory blocks.
  let e_free_vars = freeInExp e
  e_mems <- S.unions <$> mapM varMems (S.toList e_free_vars)

  -- Then handle the pattern elements by themselves again.
  forM_ patvalelems $ \(PatElem x _ _) ->
    -- Set all memory blocks being used as optimistic last uses.
    forM_ (S.toList e_mems) $ \mem -> do
      first_uses_before_loop <- asks ctxCurFirstUsesBeforeLoop
      unless (mem `S.member` first_uses_before_loop) $
        setOptimistic mem x

  -- When an loop contains a use of an array that is created before the
  -- loop, it must not reuse that memory, because there are cycles in loops.
  -- This should result in more interferences being recorded.  See
  -- 'tests/reuse/loop/copy-from-outside.fut for an example of this.
  mMod <- case e of
    DoLoop{} -> do
      cur_first_uses <- gets curFirstUses
      return $ local (\ctx -> ctx { ctxCurFirstUsesBeforeLoop = cur_first_uses })
    _ -> return id

  withLocalCurFirstUses $ mMod $ fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          }
