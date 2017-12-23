{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
-- | Find array creations that can be set to use existing memory blocks instead
-- of new allocations.
module Futhark.Optimise.MemoryBlockMerging.Reuse.Core
  ( coreReuseFunDef
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Identity

import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Construct
import Futhark.Representation.AST
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.ExplicitMemory
       (ExplicitMemory, ExplicitMemorish)
import Futhark.Pass.ExplicitAllocations()
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.PrimExps (findPrimExpsFunDef)
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.MemoryUpdater

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes
import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeUses


data Context = Context { ctxFirstUses :: FirstUses
                         -- ^ From the module Liveness.FirstUses
                       , ctxInterferences :: Interferences
                       , ctxPotentialKernelInterferences
                         :: PotentialKernelDataRaceInterferences
                         -- ^ From the module Liveness.Interferences
                       , ctxSizes :: Sizes
                         -- ^ maps a memory block to its size and space
                       , ctxVarToMem :: VarMemMappings MemorySrc
                         -- ^ From the module VariableMemory
                       , ctxActualVars :: M.Map VName Names
                         -- ^ From the module ActualVariables
                       , ctxExistentials :: Names
                         -- ^ From the module Existentials
                       , ctxVarPrimExps :: M.Map VName (PrimExp VName)
                         -- ^ From the module PrimExps
                       , ctxSizeVarsUsesBefore :: M.Map VName Names
                         -- ^ maps a memory name to the size variables available
                         -- at that memory block allocation point
                       }
  deriving (Show)

data Current = Current { curUses :: M.Map MName MNames
                         -- ^ maps a memory block to the memory blocks that
                         -- have been merged into it so far
                       , curEqAsserts :: M.Map VName Names
                         -- ^ maps a variable name to other semantically equal
                         -- variable names

                       , curVarToMemRes :: VarMemMappings MemoryLoc
                         -- ^ The result of the core analysis: maps an array
                         -- name to its memory block.

                       , curVarToMaxExpRes :: M.Map MName Names
                         -- ^ Changes in variable uses where allocation sizes
                         -- are maxed from its elements.  Keyed by statement
                         -- memory name (alloc stmt).  Maps an alloc stmt to the
                         -- sizes that need to be taken max for.

                       , curKernelMaxSizedRes :: M.Map MName (VName,
                                                              ((VName, VName),
                                                               (VName, VName)))
                         -- ^ Maps an alloc stmt to
                         -- (size0,
                         --  ((array0, size_var0, ixfun0),
                         --   (array1, size_var1, ixfun1))).
                         --
                         -- Needed for array creations in kernel
                         -- bodies that can only reuse memory if index functions
                         -- are changed, and the allocation size is maxed.
                         --
                         -- size_var0 is *not* the size of the entire allocation
                         -- of the key memory, but *part of* the allocation
                         -- size.  This part will be replaced by the maximum of
                         -- the two sizes.
                       }
  deriving (Show)

emptyCurrent :: Current
emptyCurrent = Current { curUses = M.empty
                       , curEqAsserts = M.empty
                       , curVarToMemRes = M.empty
                       , curVarToMaxExpRes = M.empty
                       , curKernelMaxSizedRes = M.empty
                       }

newtype FindM lore a = FindM { unFindM :: RWS Context Log Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter Log,
            MonadState Current)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

writeLog :: VName -> String -> String -> FindM lore ()
writeLog var_at topic content =
  tell $ Log $ M.singleton var_at [(topic, content)]

-- Lookup the memory block statically associated with a variable.
lookupVarMem :: MonadReader Context m =>
                VName -> m MemorySrc
lookupVarMem var =
  -- This should always be called from a place where it is certain that 'var'
  -- refers to a statement with an array expression.
  (fromJust ("lookup memory block from " ++ pretty var) . M.lookup var)
  <$> asks ctxVarToMem

lookupActualVars' :: ActualVariables -> VName -> Names
lookupActualVars' actual_vars var =
  -- Do this recursively.
  let actual_vars' = expandWithAliases actual_vars actual_vars
  in fromMaybe (S.singleton var) $ M.lookup var actual_vars'

lookupActualVars :: MonadReader Context m =>
                    VName -> m Names
lookupActualVars var = do
  actual_vars <- asks ctxActualVars
  return $ lookupActualVars' actual_vars var

lookupSize :: MonadReader Context m =>
              VName -> m SubExp
lookupSize var =
  (fst . fromJust ("lookup size from " ++ pretty var) . M.lookup var)
  <$> asks ctxSizes

lookupSpace :: MonadReader Context m =>
               VName -> m Space
lookupSpace var =
  (snd . fromJust ("lookup space from " ++ pretty var) . M.lookup var)
  <$> asks ctxSizes

-- Record that the existing old_mem now also "is the same as" new_mem.
insertUse :: LoreConstraints lore =>
             VName -> VName -> FindM lore ()
insertUse old_mem new_mem =
  modify $ \cur -> cur { curUses = insertOrUpdate old_mem new_mem $ curUses cur }

recordMemMapping :: LoreConstraints lore =>
                    VName -> MemoryLoc -> FindM lore ()
recordMemMapping x mem =
  modify $ \cur -> cur { curVarToMemRes = M.insert x mem $ curVarToMemRes cur }

recordMaxMapping :: LoreConstraints lore =>
                    MName -> VName -> FindM lore ()
recordMaxMapping mem y =
  modify $ \cur -> cur { curVarToMaxExpRes = insertOrUpdate mem y
                                             $ curVarToMaxExpRes cur }

recordKernelMaxMapping :: LoreConstraints lore =>
  MName -> (VName, ((VName, VName),
                    (VName, VName))) -> FindM lore ()
recordKernelMaxMapping mem info =
  modify $ \cur -> cur { curKernelMaxSizedRes =
                           M.insert mem info $ curKernelMaxSizedRes cur
                       }

modifyCurEqAsserts :: (M.Map VName Names -> M.Map VName Names) -> FindM lore ()
modifyCurEqAsserts f = modify $ \c -> c { curEqAsserts = f $ curEqAsserts c }

-- Run a monad with a local copy of the uses.  We don't want any new uses in
-- nested bodies to be available for merging into when we are back in the main
-- body, but we do want updates to existing uses to be propagated.
withLocalUses :: LoreConstraints lore =>
                 FindM lore a -> FindM lore a
withLocalUses m = do
  uses_before <- gets curUses
  res <- m
  uses_after <- gets curUses
  -- Only take the results whose memory block keys were also present prior to
  -- traversing the sub-body.
  let uses_before_updated = M.filterWithKey
                            (\mem _ -> mem `S.member` M.keysSet uses_before)
                            uses_after
  modify $ \cur -> cur { curUses = uses_before_updated }
  return res

coreReuseFunDef :: MonadFreshNames m =>
                   FunDef ExplicitMemory -> FirstUses ->
                   Interferences -> PotentialKernelDataRaceInterferences ->
                   VarMemMappings MemorySrc -> ActualVariables -> Names ->
                   m (FunDef ExplicitMemory, Log)
coreReuseFunDef fundef first_uses interferences potential_kernel_interferences var_to_mem actual_vars existentials = do
  let sizes = memBlockSizesFunDef fundef
      size_uses = findSizeUsesFunDef fundef
      var_to_pe = findPrimExpsFunDef fundef
      context = Context
        { ctxFirstUses = first_uses
        , ctxInterferences = interferences
        , ctxPotentialKernelInterferences = potential_kernel_interferences
        , ctxSizes = sizes
        , ctxVarToMem = var_to_mem
        , ctxActualVars = actual_vars
        , ctxExistentials = existentials
        , ctxVarPrimExps = var_to_pe
        , ctxSizeVarsUsesBefore = size_uses
        }
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      (res, proglog) = execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings (curVarToMemRes res) fundef
  fundef'' <- transformFromVarMaxExpMappings (curVarToMaxExpRes res) fundef'
  fundef''' <- transformFromKernelMaxSizedMappings var_to_pe var_to_mem actual_vars
               (curKernelMaxSizedRes res) fundef''

  let all_mems = S.fromList $ map memSrcName $ M.elems var_to_mem
      mems_changed = S.fromList $ map memSrcName
                     $ mapMaybe (`M.lookup` var_to_mem) $ M.keys $ curVarToMemRes res
      mems_unchanged = S.difference all_mems mems_changed
      debug = fundef''' `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "coreReuseFunDef reuse results:"
        forM_ (M.assocs (curVarToMemRes res)) $ \(src, dstmem) ->
          putStrLn ("Source " ++ pretty src ++ " (old: "
                    ++ pretty (memSrcName $ fromJust "should exist" $ M.lookup src var_to_mem)
                    ++ ") reuses " ++ pretty (memLocName dstmem) ++ ";\nixfun: "
                    ++ pretty (memLocIxFun dstmem))
        putStrLn ""
        forM_ (M.assocs (curVarToMaxExpRes res)) $ \(src, maxs) ->
          putStrLn ("Size of allocation of mem " ++ pretty src ++ " is now maximum of "
                    ++ prettySet maxs)
        putStrLn ""
        putStrLn ("mems changed: " ++ prettySet mems_changed)
        putStrLn ("mems unchanged: " ++ prettySet mems_unchanged)
        putStrLn $ pretty fundef'''
        putStrLn $ replicate 70 '='

  withDebug debug $ return (fundef''', proglog)

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param _ membound) =
  -- Unique array function parameters also count as "allocations" in which
  -- memory can be reused.
  case membound of
    ExpMem.MemArray _ _ Unique (ExpMem.ArrayIn mem _) ->
      insertUse mem mem
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
lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
  var_to_pe <- asks ctxVarPrimExps
  let eqs | BasicOp (Assert (Var v) _ _) <- e
          , Just (CmpOpExp (CmpEq _) (LeafExp v0 _) (LeafExp v1 _)) <- M.lookup v var_to_pe = do
              modifyCurEqAsserts $ insertOrUpdate v0 v1
              modifyCurEqAsserts $ insertOrUpdate v1 v0
          | otherwise = return ()
  eqs

  forM_ patvalelems $ \(PatElem var _ membound) -> do
    -- For every declaration with a first memory use, check (through
    -- handleNewArray) if it can reuse some earlier memory block.
    first_uses_var <- lookupEmptyable var <$> asks ctxFirstUses
    actual_vars_var <- lookupActualVars var
    existentials <- asks ctxExistentials
    case membound of
      ExpMem.MemArray _ _ _ (ExpMem.ArrayIn mem _) ->
        when (-- We require that it must be a first use, i.e. an array creation.
              mem `S.member` first_uses_var
              -- If the array is existential or "aliases" something that is
              -- existential, we do not try to make it reuse any memory.
              && not (var `S.member` existentials)
              && not (any (`S.member` existentials) actual_vars_var))
        $ handleNewArray var mem
      _ -> return ()

  fullWalkExpM walker walker_kernel e

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "Statement."
        print patvalelems
        print e
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()
  where walker = identityWalker
          { walkOnBody = withLocalUses . lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . withLocalUses . lookInBody
          , walkOnKernelKernelBody = coerce . withLocalUses . lookInKernelBody
          , walkOnKernelLambda = coerce . withLocalUses . lookInBody . lambdaBody
          }

-- Check if a new array declaration x with a first use of the memory xmem can be
-- set to use a previously encountered memory block.
handleNewArray :: LoreConstraints lore =>
                  VName -> MName -> FindM lore ()
handleNewArray x xmem = do
  interferences <- asks ctxInterferences

  let notTheSame :: Monad m => MName -> MNames -> m Bool
      notTheSame kmem _used_mems = return (kmem /= xmem)

  let noneInterfere :: Monad m => MName -> MNames -> m Bool
      noneInterfere _kmem used_mems =
        -- A memory block can have already been reused.  We also check for
        -- interference with any previously merged blocks.
        return $ all (\used_mem -> not $ S.member xmem
                                   $ lookupEmptyable used_mem interferences)
        $ S.toList used_mems

  let noneInterfereKernelArray :: MonadReader Context m => MNames -> m Bool
      noneInterfereKernelArray used_mems =
        not <$> anyM (interferesInKernel xmem) (S.toList used_mems)

  let sameSpace :: MonadReader Context m =>
                   MName -> MNames -> m Bool
      sameSpace kmem _used_mems = do
        kspace <- lookupSpace kmem
        xspace <- lookupSpace xmem
        return (kspace == xspace)

  -- Is the size of the new memory block (xmem) equal to any of the memory
  -- blocks (used_mems) using an already used memory block?
  let sizesMatch :: LoreConstraints lore =>
                    MNames -> FindM lore Bool
      sizesMatch used_mems = do
        ok_sizes <- mapM lookupSize $ S.toList used_mems
        new_size <- lookupSize xmem
        -- Check for size equality by checking for variable name equality.
        let eq_simple = new_size `L.elem` ok_sizes

        -- Check for size equality by constructing 'PrimExp's and comparing
        -- those.  Use the custom VarWithLooseEquality type to compare inner
        -- sizes: If an equality assert statement was found earlier, consider
        -- its two operands to be the same.
        var_to_pe <- asks ctxVarPrimExps
        eq_asserts <- gets curEqAsserts
        let sePrimExp se = do
              v <- fromVar se
              pe <- M.lookup v var_to_pe
              let pe_expanded = expandPrimExp var_to_pe pe
              traverse (\v_inner -> -- Has custom Eq instance.
                                       pure $ VarWithLooseEquality v_inner
                                       $ lookupEmptyable v_inner eq_asserts
                       ) pe_expanded
        let ok_sizes_pe = map sePrimExp ok_sizes
        let new_size_pe = sePrimExp new_size

        -- If new_size_pe actually denotes a PrimExp, check if it is among the
        -- constructed 'PrimExp's of the sizes of the memory blocks that have
        -- already been set to use the target memory block.
        let eq_advanced = isJust new_size_pe && new_size_pe `L.elem` ok_sizes_pe

        let debug = do
              putStrLn $ replicate 70 '~'
              putStrLn "sizesMatch:"
              putStrLn ("new: " ++ pretty new_size)
              forM_ ok_sizes $ \ok_size ->
                putStrLn (" ok: " ++ pretty ok_size)
              putStrLn $ replicate 30 '~'
              putStrLn ("new: " ++ show new_size_pe)
              forM_ ok_sizes_pe $ \ok_size_pe ->
                putStrLn (" ok: " ++ show ok_size_pe)
              putStrLn ("eq asserts: " ++ show eq_asserts)
              putStrLn ("eq : " ++ show eq_simple ++ " " ++ show eq_advanced)
              putStrLn $ replicate 70 '~'

        withDebug debug $ return (eq_simple || eq_advanced)

  -- In case sizes do not match: Is it possible to change the size of the target
  -- memory block to be a maximum of itself and the new memory block?
  let sizesCanBeMaxed :: LoreConstraints lore =>
                         MName -> FindM lore Bool
      sizesCanBeMaxed kmem = do
        ksize <- lookupSize kmem
        xsize <- lookupSize xmem
        uses_before <- asks ctxSizeVarsUsesBefore
        let ok = fromMaybe False $ do
              ksize' <- fromVar ksize
              xsize' <- fromVar xsize
              return (xsize' `S.member` fromJust ("is recorded for all size variables "
                                                  ++ pretty ksize')
                      (M.lookup ksize' uses_before))
            debug = do
              putStrLn $ replicate 70 '~'
              putStrLn "sizesCanBeMaxed:"
              putStrLn $ pretty kmem
              putStrLn $ pretty xmem
              print ok
              putStrLn $ replicate 70 '~'
        withDebug debug $ return ok

  let sizesCanBeMaxedKernelArray :: LoreConstraints lore =>
        MName -> MNames ->
        FindM lore (Maybe (VName, ((VName, VName),
                                   (VName, VName))))
      sizesCanBeMaxedKernelArray kmem used_mems = do
        -- Let a kernel body have two indexed array creations result_0 and
        -- result_1 with the index functions
        --
        --   result_0: ixfun_start_0[indices_start_0, 0i64:+res_0*1i64]
        --   result_1: ixfun_start_1[indices_start_1, 0i64:+res_1*1i64]
        --
        -- with the additional requirements that
        --
        --   + ixfun_start_0 is equal to ixfun_start_1 except for mentions of
        --     res_0 and res_1.
        --
        --   + indices_start_0 is equal to indices_start_1.
        --
        -- Example:
        --
        --   result_0: Direct(num_groups, res_0, group_size)[0, 2, 1][group_id, local_tid, 0i64:+res_0*1i64]
        --   result_1: Direct(num_groups, res_1, group_size)[0, 2, 1][group_id, local_tid, 0i64:+res_1*1i64]
        --
        -- By default result_0 and result_1 will be set to interfere because
        -- each thread can access parts of the memory of another thread if they
        -- are merged.  We can fix this my making both index functions describe
        -- the same access pattern except for the final dimension.  We want this
        -- to happen for the example above:
        --
        --   result_0': Direct(num_groups, res_max, group_size)[0, 2, 1][group_id, local_tid, 0i64:+res_0*1i64]
        --   result_1': Direct(num_groups, res_max, group_size)[0, 2, 1][group_id, local_tid, 0i64:+res_1*1i64]
        --
        -- Where res_max = max(res_0, res_1).  Now they cover the same area in
        -- space.  The final index slices are kept as they were, since the shape
        -- of the created array should stay the same.  This means that the
        -- smallest array will not be writing to all of its available space.
        --
        -- We need to check:
        --
        --   + Is res_1 in scope at the allocation?  Allocation size hoisting
        --     has probably been helpful here.
        --
        --   + Does res_0 and res_1 have the same base type size?
        --
        -- If true, modify the program as such:
        --
        --   + Insert a res_max statement before the allocation.
        --
        --   + Change the allocation size to use res_max instead of res_0.
        --
        --   + Modify both index functions to use res_max instead of res_0 and
        --     res_1, respectively, except for at the final index slice.
        --
        -- Extension: If an array reuses an already reused array, remember to
        -- update *all* index functions.  Currently we avoid these cases for
        -- simplicity of implementation.

        potentials <- asks ctxPotentialKernelInterferences
        uses_before <- asks ctxSizeVarsUsesBefore

        let first_usess = filter (\p ->
                                    let pot_mems = map (\(m, _, _, _) -> m) p
                                    in kmem `elem` pot_mems && xmem `elem` pot_mems)
                          potentials
        kmem_size <- (fromJust "should be a var" . fromVar) <$> lookupSize kmem

        return $ case (S.toList used_mems, first_usess) of
          -- We only support the basic case for now.  FIXME.
          --
          -- A used_mems list of size > 1 means that kmem has already been
          -- reused.  This is okay, but a bit harder to keep track of.
          --
          -- A first_usess list of size > 1 means that xmem and kmem
          -- data-race-interfere in multiple kernels.  This will never happen in
          -- the current implementation, but could *potentially* happen in the
          -- future.
          ([_], [first_uses]) -> do
            (_, kmem_array, kmem_pt, kmem_ixfun) <-
              L.find (\(mname, _, _, _) -> mname == kmem) first_uses
            (_, xmem_array, xmem_pt, xmem_ixfun) <-
              L.find (\(mname, _, _, _) -> mname == xmem) first_uses

            if (kmem, kmem_ixfun) `ixFunsCompatible` (xmem, xmem_ixfun)
              then Nothing -- These are not special, and need not special handling.
              else do
              (kmem_ixfun_start, kmem_indices_start, kmem_final_dim) <-
                getInfo kmem_ixfun
              (xmem_ixfun_start, xmem_indices_start, xmem_final_dim) <-
                getInfo xmem_ixfun

              let xmem_final_dim_before_kmem_final_dim =
                    maybe False (xmem_final_dim `S.member`) $
                    M.lookup kmem_final_dim uses_before
                  kmem_ixfun_start' = getIxFun' kmem_ixfun_start
                                      (M.singleton kmem_final_dim xmem_final_dim)
                  xmem_ixfun_start' = getIxFun' xmem_ixfun_start
                                      (M.singleton xmem_final_dim kmem_final_dim)

                  res = if kmem_indices_start == xmem_indices_start &&
                           (kmem, kmem_ixfun_start') `ixFunsCompatible`
                           (xmem, xmem_ixfun_start') &&
                           (primByteSize kmem_pt :: Int) == primByteSize xmem_pt &&
                           xmem_final_dim_before_kmem_final_dim
                        then return (kmem_size,
                                     ((kmem_array, kmem_final_dim),
                                      (xmem_array, xmem_final_dim)))
                        else Nothing

                  debug = res `seq` do
                    putStrLn $ replicate 70 '~'
                    putStrLn "sizesCanBeMaxedKernelArray:"

                    putStrLn ("kmem: " ++ pretty kmem)
                    putStrLn ("kmem array: " ++ pretty kmem_array)
                    putStrLn ("kmem prim type: " ++ pretty kmem_pt)
                    putStrLn ("kmem indices start: " ++ show kmem_indices_start)
                    putStrLn ("kmem final dim: " ++ pretty kmem_final_dim)

                    putStrLn ("xmem: " ++ pretty xmem)
                    putStrLn ("xmem array: " ++ pretty xmem_array)
                    putStrLn ("xmem prim type: " ++ pretty xmem_pt)
                    putStrLn ("xmem indices start: " ++ show xmem_indices_start)
                    putStrLn ("xmem final dim: " ++ pretty xmem_final_dim)

                    putStrLn ("xmem_final_dim_before_kmem_final_dim: " ++
                              show xmem_final_dim_before_kmem_final_dim)
                    putStrLn ("result: " ++ show res)
                    putStrLn $ replicate 70 '~'
                in withDebug debug res
          _ -> Nothing

        where getInfo :: ExpMem.IxFun ->
                         Maybe (ExpMem.IxFun, Slice (PrimExp VName), VName)
              getInfo (IxFun.Index ixfun_start slice) =
                case L.span isDimFix slice of
                  (indices_start, [DimSlice _start_offset
                                   (LeafExp final_dim@VName{} (IntType Int32))
                                   _stride]) ->
                    Just (ixfun_start, indices_start, final_dim)
                  _ -> Nothing
                where isDimFix DimFix{} = True
                      isDimFix _ = False
              getInfo _ = Nothing

              getIxFun' :: ExpMem.IxFun -> M.Map VName VName ->
                           IxFun.IxFun (PrimExp VarWithLooseEquality)
              getIxFun' ixfun others =
                let loose_eq_map name_inner =
                      -- Has custom Eq instance.
                      pure $ VarWithLooseEquality name_inner
                      $ maybe S.empty S.singleton $ M.lookup name_inner others
                in runIdentity $ traverse (traverse loose_eq_map) ixfun

  let sizesCanBeMaxedKernelArray' :: LoreConstraints lore =>
                                    MName -> MNames -> FindM lore Bool
      sizesCanBeMaxedKernelArray' kmem used_mems =
        isJust <$> sizesCanBeMaxedKernelArray kmem used_mems

  let sizesWorkOut :: LoreConstraints lore =>
                      MName -> MNames -> FindM lore Bool
      sizesWorkOut kmem used_mems =
        -- The size of an allocation is okay to reuse if it is the same as the
        -- current memory size, or if it can be changed to be the maximum size
        -- of the two sizes.
        (noneInterfereKernelArray used_mems <&&>
         (sizesMatch used_mems <||> sizesCanBeMaxed kmem))
        <||> sizesCanBeMaxedKernelArray' kmem used_mems

  let canBeUsed t = and <$> mapM (($ t) . uncurry)
                    [notTheSame, noneInterfere, sameSpace, sizesWorkOut]
  cur_uses <- gets curUses
  found_use <- catMaybes <$> mapM (maybeFromBoolM canBeUsed) (M.assocs cur_uses)

  writeLog x "available for reuse" (prettyList found_use)
  not_found_use <-
    mapM (\(k, us) -> do
             let t (s, f) = do
                   r <- f k us
                   return $ if r then [] else [s]
             us' <- concat <$> mapM t [ ("interference", noneInterfere)
                                      , ("different space", sameSpace)
                                      , ("different size variables", \_ um -> sizesMatch um)
                                      , ("size cannot be maxed", \k1 _ -> sizesCanBeMaxed k1)
                                      ]
             return (k, us'))
    $ M.assocs $ M.filterWithKey (\k _ -> k `notElem` map fst found_use) cur_uses

  zipWithM_ (\(t, ws) i ->
               writeLog x ("not available for reuse (" ++ show i ++ ")")
               (pretty t ++ " (failed on: " ++ L.intercalate ", " ws ++ ")"))
    not_found_use [(1::Int)..]
  writeLog x "base interferences" (prettySet $ lookupEmptyable xmem interferences)

  actual_vars <- lookupActualVars x
  case found_use of
    (kmem, used_mems) : _ -> do
      -- There is a previous memory block that we can use.  Record the mapping.
      insertUse kmem xmem
      forM_ actual_vars $ \var -> do
        ixfun <- memSrcIxFun <$> lookupVarMem var
        recordMemMapping var $ MemoryLoc kmem ixfun -- Only change the memory block.

      -- Record any size-maximum change in case of sizesCanBeMaxed returning
      -- True.
      whenM (sizesCanBeMaxed kmem) $ do
        ksize <- lookupSize kmem
        xsize <- lookupSize xmem
        fromMaybe (return ()) $ do
          ksize' <- fromVar ksize
          xsize' <- fromVar xsize
          return $ do
            recordMaxMapping kmem ksize'
            recordMaxMapping kmem xsize'

      -- If we are inside a kernel body, and the current array can use the
      -- memory block of another array if its size gets maximised, record this
      -- change.  The actual program transformation will happen later.
      kernel_maxing <- sizesCanBeMaxedKernelArray kmem used_mems
      onJust kernel_maxing $ \info -> do
        let debug = do
              putStrLn $ replicate 70 '~'
              putStrLn "kernel size maxing result"
              putStrLn ("info: " ++ show info)
              putStrLn $ replicate 70 '~'
        withDebug debug $ recordKernelMaxMapping kmem info

    _ ->
      -- There is no previous memory block available for use.  Record that this
      -- memory block is available.
      insertUse xmem xmem

  let debug = found_use `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "Handle new array."
        putStrLn ("var: " ++ pretty x)
        putStrLn ("actual vars: " ++ prettySet actual_vars)
        putStrLn ("mem: " ++ pretty xmem)
        putStrLn ("cur uses: " ++ show cur_uses)
        putStrLn ("found use: " ++ show found_use)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

data VarWithLooseEquality = VarWithLooseEquality VName Names
  deriving (Show)

instance Eq VarWithLooseEquality where
  VarWithLooseEquality v0 vs0 == VarWithLooseEquality v1 vs1 =
    not $ S.null $ S.intersection (S.insert v0 vs0) (S.insert v1 vs1)

interferesInKernel :: MonadReader Context m => MName -> MName -> m Bool
interferesInKernel mem0 mem1 = do
  potentials <- asks ctxPotentialKernelInterferences

  let interferesInGroup :: PotentialKernelDataRaceInterferenceGroup -> Bool
      interferesInGroup first_uses = fromMaybe False $ do
        (_, _, pt0, ixfun0) <- L.find (\(mname, _, _, _) -> mname == mem0) first_uses
        (_, _, pt1, ixfun1) <- L.find (\(mname, _, _, _) -> mname == mem1) first_uses
        return $ interferes (pt0, ixfun0) (pt1, ixfun1)

      interferes :: (PrimType, ExpMem.IxFun) -> (PrimType, ExpMem.IxFun) -> Bool
      interferes (pt0, ixfun0) (pt1, ixfun1) =
          -- Must be different.
          mem0 /= mem1 &&
          (
            -- Do the index functions range over different memory areas?
            ((ixFunHasIndex ixfun0 || ixFunHasIndex ixfun1) &&
             not (ixFunsCompatible (mem0, ixfun0) (mem1, ixfun1)))
            ||
            -- Do the arrays have different base type size?  If so, they take
            -- up different amounts of space, and will not be compatible.
            ((primByteSize pt0 :: Int) /= primByteSize pt1)
          )

  return $ any interferesInGroup potentials

-- Does an index function contain an Index expression?
--
-- If the index function of the memory annotation uses an index, it means that
-- the array creation does not refer to the entire array.  It is an array
-- creation, but only partially: It creates part of the array, and another part
-- is created in another loop iteration or kernel thread.  The danger in
-- declaring this memory a first use lies in how it can then be reused later in
-- the iteration/thread by some memory with a *different* index in its memory
-- annotation index function, which can affect reads in other threads.
ixFunHasIndex :: IxFun.IxFun num -> Bool
ixFunHasIndex ixfun = case ixfun of
  IxFun.Direct _ -> False
  IxFun.Permute ixfun' _ -> ixFunHasIndex ixfun'
  IxFun.Rotate ixfun' _ -> ixFunHasIndex ixfun'
  IxFun.Index{} -> True
  IxFun.Reshape ixfun' _ -> ixFunHasIndex ixfun'
  IxFun.Repeat ixfun' _ _ -> ixFunHasIndex ixfun'

-- Do the two index functions describe the same range?  In other words, does one
-- array take up precisely the same location (offset) and size as another array
-- relative to the beginning of their respective memory blocks?  FIXME: This can
-- be less conservative, for example by handling that different reshapes of the
-- same array can describe the same offset and space, but do we have any tests
-- or benchmarks where this occurs?
ixFunsCompatible :: (Eq v, Show v) =>
                    (MName, IxFun.IxFun (PrimExp v)) -> (MName, IxFun.IxFun (PrimExp v)) ->
                    Bool
ixFunsCompatible (mem0, ixfun0) (mem1, ixfun1) =
  let res = ixFunsCompatibleRaw ixfun0 ixfun1

      debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "ixFunsCompatible:"
        putStrLn ("ixfun0 " ++ pretty mem0 ++ ": " ++ show ixfun0)
        putStrLn ("ixfun1 " ++ pretty mem1 ++ ": " ++ show ixfun1)
        putStrLn "---"
        putStrLn ("res: " ++ show res)
        putStrLn $ replicate 70 '~'
  in withDebug debug res

-- Are two index functions *identical*?  (Silly approach, but the Eq instance is
-- used for something else.)
ixFunsCompatibleRaw :: Eq num => IxFun.IxFun num -> IxFun.IxFun num -> Bool
ixFunsCompatibleRaw ixfun0 ixfun1 = ixfun0 `primEq` ixfun1
  where primEq a b = case (a, b) of
          (IxFun.Direct sa, IxFun.Direct sb) ->
            sa == sb
          (IxFun.Permute a1 pa, IxFun.Permute b1 pb) ->
            a1 `primEq` b1 && pa == pb
          (IxFun.Rotate a1 ia, IxFun.Rotate b1 ib) ->
            a1 `primEq` b1 && ia == ib
          (IxFun.Index a1 sa, IxFun.Index b1 sb) ->
            a1 `primEq` b1 && sa == sb
          (IxFun.Reshape a1 sa, IxFun.Reshape b1 sb) ->
            a1 `primEq` b1 && sa == sb
          (IxFun.Repeat a1 ssa sa, IxFun.Repeat b1 ssb sb) ->
            a1 `primEq` b1 && ssa == ssb && sa == sb
          _ -> False


-- Replace certain allocation sizes in a program with new variables describing
-- the maximum of two or more allocation sizes.
transformFromVarMaxExpMappings :: MonadFreshNames m =>
                                  M.Map VName Names
                               -> FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFromVarMaxExpMappings var_to_max fundef = do
  var_to_new_var <-
    M.fromList <$> mapM (\(k, v) -> (k,) <$> maxsToReplacement (S.toList v))
    (M.assocs var_to_max)
  return $ insertAndReplace var_to_new_var fundef

-- A replacement is a new size variable and any new subexpressions that the new
-- variable depends on.
data Replacement = Replacement
  { replName :: VName -- The new variable
  , replStms :: [Stm ExplicitMemory] -- The new expressions
  }
  deriving (Show)

-- Take a list of size variables.  Return a replacement consisting of a size
-- variable denoting the maximum of the input sizes.
maxsToReplacement :: MonadFreshNames m =>
                     [VName] -> m Replacement
maxsToReplacement [] = error "maxsToReplacements: Cannot take max of zero variables"
maxsToReplacement [v] = return $ Replacement v []
maxsToReplacement vs = do
  -- Should be O(lg N) number of new expressions.
  let (vs0, vs1) = splitAt (length vs `div` 2) vs
  Replacement m0 es0 <- maxsToReplacement vs0
  Replacement m1 es1 <- maxsToReplacement vs1
  vmax <- newVName "max"
  let emax = BasicOp $ BinOp (SMax Int64) (Var m0) (Var m1)
      new_stm = Let (Pattern [] [PatElem vmax BindVar
                                 (ExpMem.MemPrim (IntType Int64))]) (defAux ()) emax
      prev_stms = es0 ++ es1 ++ [new_stm]
  return $ Replacement vmax prev_stms

-- Modify a function to use the new replacements.
insertAndReplace :: M.Map MName Replacement -> FunDef ExplicitMemory ->
                    FunDef ExplicitMemory
insertAndReplace replaces0 fundef =
  let body' = evalState (transformBody $ funDefBody fundef) replaces0
  in fundef { funDefBody = body' }

  where transformBody :: Body ExplicitMemory ->
                         State (M.Map VName Replacement) (Body ExplicitMemory)
        transformBody body = do
          stms' <- concat <$> mapM transformStm (stmsToList $ bodyStms body)
          return $ body { bodyStms = stmsFromList stms' }

        transformStm :: Stm ExplicitMemory ->
                        State (M.Map VName Replacement) [Stm ExplicitMemory]
        transformStm stm@(Let (Pattern [] [PatElem mem_name BindVar
                                           (ExpMem.MemMem _ pat_space)]) _
                          (Op (ExpMem.Alloc _ space))) = do
          replaces <- get
          case M.lookup mem_name replaces of
            Just repl -> do
              let prev = replStms repl
                  new = Let (Pattern [] [PatElem mem_name BindVar
                                         (ExpMem.MemMem (Var (replName repl))
                                          pat_space)]) (defAux ())
                        (Op (ExpMem.Alloc (Var (replName repl)) space))
              -- We should only generate the new statements once.
              modify $ M.adjust (\repl0 -> repl0 { replStms = [] }) mem_name
              return (prev ++ [new])
            Nothing -> return [stm]
        transformStm (Let pat attr e) = do
          let mapper = identityMapper { mapOnBody = const transformBody }
          e' <- mapExpM mapper e
          return [Let pat attr e']


-- Change certain allocation sizes in a program.
transformFromKernelMaxSizedMappings :: MonadFreshNames m =>
  M.Map VName (PrimExp VName) -> VarMemMappings MemorySrc -> ActualVariables ->
  M.Map MName (VName, ((VName, VName),
                       (VName, VName))) ->
  FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFromKernelMaxSizedMappings
  var_to_pe var_to_mem actual_vars mem_to_info fundef = do
  (mem_to_size_var, arr_to_mem_ixfun) <-
    unzip <$> mapM (uncurry withNewMaxVar) (M.assocs mem_to_info)
  let mem_to_size_var' = M.fromList mem_to_size_var
      arr_to_memloc = M.fromList $ map (\(arr, destmem, ixfun) ->
                                          (arr, MemoryLoc destmem ixfun))
                      $ concat arr_to_mem_ixfun

      fundef' = insertAndReplace mem_to_size_var' fundef
      fundef'' = transformFromVarMemMappings arr_to_memloc fundef'
  return fundef''

  where withNewMaxVar :: MonadFreshNames m =>
                         MName -> (VName,
                                   ((VName, VName),
                                    (VName, VName))) ->
                         m ((MName, Replacement),
                            [(VName, MName, ExpMem.IxFun)])
        withNewMaxVar mem (kmem_size,
                           ((kmem_array, kmem_final_dim),
                            (xmem_array, xmem_final_dim))) = do
          final_dim_max_v <- newVName "max_final_dim"
          let final_dim_max_e =
                BasicOp (BinOp (SMax Int32)
                         (Var kmem_final_dim) (Var xmem_final_dim))

              var_to_pe_extension =
                M.singleton kmem_final_dim (LeafExp final_dim_max_v (IntType Int32))
              var_to_pe' = M.union var_to_pe_extension var_to_pe
              full_size_pe = fromJust "should exist" $ M.lookup kmem_size var_to_pe
              full_size_pe_expanded = expandPrimExp var_to_pe' full_size_pe
              new_full_size_m =
                letExp "max" =<< primExpToExp (return . BasicOp . SubExp . Var)
                full_size_pe_expanded
          (alloc_size_var, alloc_size_stms) <-
            modifyNameSource $ runState $ runBinderT new_full_size_m mempty
          let alloc_size_fd_stm =
                Let (Pattern [] [PatElem final_dim_max_v BindVar
                                 (ExpMem.MemPrim (IntType Int32))]) (defAux ()) final_dim_max_e
              alloc_size_stms' = oneStm alloc_size_fd_stm <> alloc_size_stms

              vars_kmem =
                S.insert kmem_array $ lookupActualVars' actual_vars kmem_array
              vars_xmem =
                S.insert xmem_array $ lookupActualVars' actual_vars xmem_array

              newIxFun (IxFun.Index ixfun_start slice) final_dim =
                let tab = M.singleton final_dim (LeafExp final_dim_max_v (IntType Int32))
                    ixfun_start' = IxFun.substituteInIxFun tab ixfun_start
                in IxFun.Index ixfun_start' slice
              newIxFun _ _ = error "Should not happen"

              arrayToMapping final_dim v =
                let ixfun = memSrcIxFun $ fromJust "should exist"
                            $ M.lookup v var_to_mem
                    ixfun_new = newIxFun ixfun final_dim
                in (v, mem, ixfun_new)
              arr_to_mem_ixfun_kmem = map (arrayToMapping kmem_final_dim)
                                      $ S.toList vars_kmem
              arr_to_mem_ixfun_xmem = map (arrayToMapping xmem_final_dim)
                                      $ S.toList vars_xmem
              arr_to_mem_ixfun = arr_to_mem_ixfun_kmem ++ arr_to_mem_ixfun_xmem

          return ((mem, Replacement alloc_size_var $ stmsToList alloc_size_stms'),
                  arr_to_mem_ixfun)
