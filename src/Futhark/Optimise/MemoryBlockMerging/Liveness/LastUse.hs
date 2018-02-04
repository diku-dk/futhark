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
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


type LastUsesList = [LastUses]

getLastUsesMap :: LastUsesList -> LastUses
getLastUsesMap = M.unionsWith S.union

-- Mapping from a memory block to its currently assumed last use statement
-- variable.
type OptimisticLastUses = M.Map VName (StmOrRes, Bool)

data Context = Context
  { ctxVarToMem :: VarMemMappings MemorySrc
  , ctxMemAliases :: MemAliases
  , ctxFirstUses :: FirstUses
  , ctxExistentials :: Names
  , ctxCurFirstUsesOuter :: Names
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
varMems :: VName -> FindM lore MNames
varMems var = do
  var_to_mem <- asks ctxVarToMem
  return $ fromMaybe S.empty $ do
    mem <- memSrcName <$> M.lookup var var_to_mem
    return $ S.singleton mem

modifyCurOptimisticLastUses :: (OptimisticLastUses -> OptimisticLastUses) -> FindM lore ()
modifyCurOptimisticLastUses f =
  modify $ \c -> c { curOptimisticLastUses = f $ curOptimisticLastUses c }

modifyCurFirstUses :: (Names -> Names) -> FindM lore ()
modifyCurFirstUses f = modify $ \c -> c { curFirstUses = f $ curFirstUses c }

withLocalCurFirstUses :: FindM lore a -> FindM lore a
withLocalCurFirstUses m = do
  cur_first_uses <- gets curFirstUses
  res <- m
  modifyCurFirstUses $ const cur_first_uses
  return res

recordMapping :: StmOrRes -> MName -> FindM lore ()
recordMapping var mem = tell [M.singleton var (S.singleton mem)]

-- | Find all last uses of *memory blocks* in a function definition.
findLastUses :: VarMemMappings MemorySrc -> MemAliases -> FirstUses -> Names
             -> FunDef ExplicitMemory -> LastUses
findLastUses var_to_mem mem_aliases first_uses existentials fundef =
  let context = Context
                { ctxVarToMem = var_to_mem
                , ctxMemAliases = mem_aliases
                , ctxFirstUses = first_uses
                , ctxExistentials = existentials
                , ctxCurFirstUsesOuter = S.empty
                }
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFunDefFParam
        lookInBody $ funDefBody fundef
        mapM_ lookInRes $ bodyResult $ funDefBody fundef
        optimistics <- gets curOptimisticLastUses
        forM_ (M.keys optimistics) $ \mem ->
          commitOptimistic mem

      last_uses = removeEmptyMaps $ getLastUsesMap
                  $ snd $ evalRWS m context (Current M.empty S.empty)
  in last_uses

-- Optimistically say that the last use of 'mem' and all its memory aliases is
-- at 'x_lu'.  Exclude 'exclude' from the memory aliases (necessary in a few
-- edge cases).
setOptimistic :: MName -> StmOrRes -> MNames -> FindM lore ()
setOptimistic mem x_lu exclude = do
  -- Will override any previous optimistic last use.
  mem_aliases <- asks ctxMemAliases
  let mems = S.difference (S.union (S.singleton mem)
                           $ lookupEmptyable mem mem_aliases) exclude

  forM_ mems $ \mem' -> do
    let is_indirect = mem' /= mem
    modifyCurOptimisticLastUses $ M.insert mem' (x_lu, is_indirect)

  let debug =
        putBlock [ "setOptimistic:"
                 , pretty mem
                 , show x_lu
                 , "exclude: " ++ prettySet exclude
                 , prettySet mems
                 ]
  doDebug debug

-- If an optimistic last use 'mem' was added through a memory alias, forget
-- about it.
removeIndirectOptimistic :: MName -> FindM lore ()
removeIndirectOptimistic mem = do
  res <- M.lookup mem <$> gets curOptimisticLastUses
  case res of
    Just (_, True) -> -- Means that is was added indirectly.
      modifyCurOptimisticLastUses $ M.delete mem
    _ -> return ()

-- Set the optimistic last use in stone.
commitOptimistic :: MName -> FindM lore ()
commitOptimistic mem = do
  res <- M.lookup mem <$> gets curOptimisticLastUses
  case res of
    Just (x_lu, _) -> do
      let debug =
            putBlock [ "commitOptimistic:"
                     , pretty mem
                     , show x_lu
                     ]

      withDebug debug $ recordMapping x_lu mem
    Nothing -> return ()

lookInFunDefFParam :: LoreConstraints lore =>
                      FParam lore -> FindM lore ()
lookInFunDefFParam (Param x _) = do
  first_uses_x <- lookupEmptyable x <$> asks ctxFirstUses
  modifyCurFirstUses $ S.union first_uses_x

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
  -- When an loop, a scan, a reduce, or a stream contains a use of an array that
  -- is created before the expression body, it should not get a last use in a
  -- statement inside the inner body, since loops can have cycles, and so its
  -- proper last use should really be in the statement declaring the sub-body,
  -- and not in some statement in the sub-body.  See
  -- 'tests/reuse/loop/copy-from-outside.fut for an example of this.
  cur_first_uses <- gets curFirstUses
  let mMod = case e of
        If{} -> id -- If is the only other expression with a body.
        _ -> local $ \ctx -> ctx { ctxCurFirstUsesOuter = cur_first_uses }

  -- First handle all pattern elements by themselves.
  forM_ patvalelems $ \(PatElem x membound) ->
    case membound of
      ExpMem.MemArray _ _ _ (ExpMem.ArrayIn xmem _) -> do
        first_uses_x <- lookupEmptyable x <$> asks ctxFirstUses
        modifyCurFirstUses $ S.union first_uses_x
        -- When this is a new first use of a memory block, commit the previous
        -- optimistic last use of it, so that it can be considered unused in
        -- the statements inbetween.
        when (S.member xmem first_uses_x) $ commitOptimistic xmem
      _ -> return ()

  -- Then find the new memory blocks.
  let e_free_vars = freeInExp e `S.difference` S.fromList (freeExcludes e)
  e_mems <- S.unions <$> mapM varMems (S.toList e_free_vars)

  mem_aliases <- asks ctxMemAliases
  first_uses_outer <- asks ctxCurFirstUsesOuter
  -- Then handle the pattern elements by themselves again.
  forM_ patvalelems $ \(PatElem x _) ->
    -- Set all memory blocks being used as optimistic last uses.
    forM_ (S.toList e_mems) $ \mem -> do
      -- If the memory has its first use outside the current body, it is
      -- dangerous to set its last use to be in a statement inside the body,
      -- since the body can be run multiple times in cases of loops or kernels,
      -- so we only set the last use of a memory to this statement if it also
      -- has its first use inside the current body.
      --
      -- If it does have its first use outside the body, we remove any existing
      -- optimistic last use, although only if such an optimistic last use was
      -- added as a side effect of adding an existential optimistic last use
      -- (i.e. it was aliased by the existential memory which had a last use).
      if mem `S.member` first_uses_outer
        then removeIndirectOptimistic mem
        else setOptimistic mem (FromStm x) S.empty

      if S.null (lookupEmptyable mem mem_aliases)
        then
        -- If not existential, update the potential last use of any existential
        -- memory aliasing it, but do not set the potential last use of the
        -- memory itself, since there are cycles in loops, and it must also
        -- contain the same data in the next iteration, so it can never be
        -- reused inside the loop body, and must therefore always have its last
        -- use outside the body.  But since the existential memory might in the
        -- current iteration refer to it, its last use needs to be updated.

        -- Note that while it is not wrong to run the code below also when the
        -- memory has its first use inside the body, in that case it should not
        -- be necessary, since we would be outside the body by then, and it
        -- would result in a too conservative analysis.  As an example, see
        -- tests/mix/loop-interference-use.fut.
        when (mem `S.member` first_uses_outer) $ do
          -- If the memory has its first use outside the current body, we need
          -- to find its actual last use (if it occurs in the body) through
          -- memory aliases.
          --
          -- If memory block t aliases memory block u (meaning that the memory of
          -- t *can* be the memory of u), and u has a potential last use here,
          -- then t also has a potential last use here (the relation is not
          -- commutative, so it does not work the other way round).
          let reverse_mem_aliases = M.keys $ M.filter (mem `S.member`) mem_aliases
              exclude = S.singleton mem
          forM_ reverse_mem_aliases $ \mem' ->
            setOptimistic mem' (FromStm x) exclude
        else
        -- Just set the last use.
        setOptimistic mem (FromStm x) S.empty

  cur_optis <- gets curOptimisticLastUses
  let debug =
        putBlock [ "LastUse lookInStm:"
                 , "stm: " ++ show patvalelems
                 , "first uses outer: " ++ prettySet first_uses_outer
                 , "e free vars: " ++ prettySet e_free_vars
                 , "e mems: " ++ prettySet e_mems
                 , "cur optimistics: " ++ show cur_optis
                 ]

  withDebug debug $ withLocalCurFirstUses $ mMod $ fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }

-- Look in body results.
lookInRes :: LoreConstraints lore =>
             SubExp -> FindM lore ()
lookInRes (Var v) = do
  exis <- asks ctxExistentials
  -- If v is a existential variable, there is no reason to record its last use,
  -- as existential memory cannot be reused (this is also the case for other
  -- setOptimistic calls, but not in a clear way).
  unless (v `S.member` exis) $ do
    mem_v <- M.lookup v <$> asks ctxVarToMem
    case mem_v of
      Just mem ->
        setOptimistic (memSrcName mem) (FromRes v) S.empty
      Nothing ->
        return ()
lookInRes _ = return ()

-- Some freeInExp results are too limiting and give us too conservative last use
-- results (especially in the CPU pipeline).  We only care about a free variable
-- if we *read* from it.  If it only exists for *writing*, then we don't have to
-- look at its memory, since whatever is there we overwrite, and so there cannot
-- be any last *use*.
freeExcludes :: LoreConstraints lore =>
                Exp lore -> [VName]
freeExcludes e = case e of
  DoLoop _ _mergevalparams _ _ ->
    -- FIXME: If the returned memory block-associated mergevalparams do not come
    -- directly from a Scratch creation, we should be able to ignore them and
    -- thereby become less conservative.
    []

  BasicOp (Update orig _ _) ->
    [orig]

  _ -> []
