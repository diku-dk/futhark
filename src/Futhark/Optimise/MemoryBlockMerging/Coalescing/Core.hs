{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Optimise.MemoryBlockMerging.Coalescing.Core
  ( coreCoalesceFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (maybe, fromMaybe, mapMaybe, isJust)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (
  ExplicitMemory, ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.MemoryUpdater

import Futhark.Optimise.MemoryBlockMerging.PrimExps (findPrimExpsFunDef)
import Futhark.Optimise.MemoryBlockMerging.Coalescing.Exps
import Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition2
import Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition3
import Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition5


-- Some of these attributes could be split into separate Coalescing helper
-- modules if it becomes confusing.  Their computations are fairly independent.
data Current = Current
  { -- Coalescings state.  Also save offsets and slices in the case that an
    -- optimistic coalescing later becomes part of a chain of coalescings, where
    -- it is offset yet again, and where it should maintain its old relative
    -- offset.
    curCoalescedIntos :: CoalescedIntos
  , curMemsCoalesced :: MemsCoalesced
  }
  deriving (Show)

type CoalescedIntos = M.Map VName (S.Set (VName, PrimExp VName,
                                          [Slice (PrimExp VName)]))
type MemsCoalesced = M.Map VName MemoryLoc

emptyCurrent :: Current
emptyCurrent = Current
  { curCoalescedIntos = M.empty
  , curMemsCoalesced = M.empty
  }

data Context = Context
  { ctxFunDef :: FunDef ExplicitMemory
    -- ^ Keep the entire function definition around for lookup purposes.
  , ctxVarToMem :: VarMemMappings MemorySrc
    -- ^ From the module VariableMemory.
  , ctxMemAliases :: MemAliases
    -- ^ From the module MemoryAliases.
  , ctxVarAliases :: VarAliases
    -- ^ From the module VariableAliases.
  , ctxFirstUses :: FirstUses
    -- ^ From the module FirstUses.
  , ctxLastUses :: LastUses
    -- ^ From the module LastUses.
  , ctxActualVars :: M.Map VName Names
    -- ^ From the module ActualVariables.
  , ctxExistentials :: Names
    -- ^ From the module Existentials.
  , ctxVarPrimExps :: M.Map VName (PrimExp VName)
    -- ^ From the module PrimExps.
  , ctxVarExps :: M.Map VName Exp'
    -- ^ Statement-name-to-expression mappins for the entire function.
  , ctxAllocatedBlocksBeforeCreation :: M.Map VName MNames
    -- ^ Safety condition 2.
  , ctxVarsInUseBeforeMem :: M.Map MName Names
    -- ^ Safety condition 5.
  }
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: RWS Context () Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadState Current)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

modifyCurCoalescedIntos :: (CoalescedIntos -> CoalescedIntos) -> FindM lore ()
modifyCurCoalescedIntos f =
  modify $ \c -> c { curCoalescedIntos = f $ curCoalescedIntos c }

modifyCurMemsCoalesced :: (MemsCoalesced -> MemsCoalesced) -> FindM lore ()
modifyCurMemsCoalesced f =
  modify $ \c -> c { curMemsCoalesced = f $ curMemsCoalesced c }

ifExp :: MonadReader Context m =>
         VName -> m (Maybe Exp')
ifExp var = do
  var_exp <- M.lookup var <$> asks ctxVarExps
  return $ case var_exp of
    Just e@(Exp _ If{}) -> Just e
    _ -> Nothing

isIfExp :: MonadReader Context m =>
           VName -> m Bool
isIfExp var = do
  found <- ifExp var
  return $ isJust found

isLoopExp :: MonadReader Context m =>
             VName -> m Bool
isLoopExp var = do
  var_exp <- M.lookup var <$> asks ctxVarExps
  return $ case var_exp of
    Just (Exp _ DoLoop{}) -> True
    _ -> False

isReshapeExp :: MonadReader Context m =>
                VName -> m Bool
isReshapeExp var = do
  var_exp <- M.lookup var <$> asks ctxVarExps
  return $ case var_exp of
    Just (Exp _ (BasicOp Reshape{})) -> True
    _ -> False

-- Lookup the memory block statically associated with a variable.
lookupVarMem :: MonadReader Context m =>
                VName -> m MemorySrc
lookupVarMem var =
  -- This should always be called from a place where it is certain that 'var'
  -- refers to a statement with an array expression.
  (fromJust ("lookup memory block from " ++ pretty var) . M.lookup var)
  <$> asks ctxVarToMem

lookupActualVars :: MonadReader Context m =>
                    VName -> m Names
lookupActualVars var = do
  actual_vars <- asks ctxActualVars
  -- Do this recursively.
  let actual_vars' = expandWithAliases actual_vars actual_vars
  return $ fromMaybe (S.singleton var) $ M.lookup var actual_vars'

-- Lookup the memory block currenty associated with a variable.  In most cases
-- (maybe all) this could probably replace 'lookupVarMem', though it would not
-- always be necessary.
lookupCurrentVarMem :: LoreConstraints lore =>
                       VName -> FindM lore (Maybe VName)
lookupCurrentVarMem var = do
        -- Current result...
        mem_cur <- M.lookup var <$> gets curMemsCoalesced
        -- ... or original result.
        --
        -- This is why we save the variables after creation, not the memory
        -- blocks: Variables stay the same, but memory blocks may change, which
        -- is relevant in the case of a chain of coalescings.
        mem_orig <- M.lookup var <$> asks ctxVarToMem
        return $ case (mem_cur, mem_orig) of
          (Just m, _) -> Just (memLocName m) -- priority choice
          (_, Just m) -> Just (memSrcName m)
          _ -> Nothing

withMemAliases :: MonadReader Context m =>
                  VName -> m Names
withMemAliases mem = do
  -- The only memory blocks with memory aliases are the existiential ones, so
  -- using a static ctxMemAliases should be okay, as they will not change during
  -- the transformation in this module.
  mem_aliases <- lookupEmptyable mem <$> asks ctxMemAliases
  return $ S.union (S.singleton mem) mem_aliases

recordOptimisticCoalescing :: LoreConstraints lore =>
                              VName -> PrimExp VName
                           -> [Slice (PrimExp VName)]
                           -> VName -> MemoryLoc -> Bindage -> FindM lore ()
recordOptimisticCoalescing src offset ixfun_slices dst dst_memloc bindage = do
  modifyCurCoalescedIntos $ insertOrUpdate dst (src, offset, ixfun_slices)

  -- If this is an in-place operation, we future-proof future coalescings by
  -- recording that they also need to take a look at the original array, not
  -- just the result of an in-place update into it.
  case bindage of
    BindVar -> return ()
    BindInPlace orig _ ->
      modifyCurCoalescedIntos $ insertOrUpdate dst (orig, zeroOffset, [])

  modifyCurMemsCoalesced $ M.insert src dst_memloc

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "recordOptimisticCoalescing"
        putStrLn ("dst: " ++ pretty dst ++ "; src: " ++ pretty src)
        putStrLn ("slices: " ++ show ixfun_slices)
        putStrLn $ replicate 70 '~'
  doDebug debug

coreCoalesceFunDef :: FunDef ExplicitMemory -> VarMemMappings MemorySrc
                   -> MemAliases -> VarAliases -> FirstUses -> LastUses
                   -> ActualVariables -> Names -> FunDef ExplicitMemory
coreCoalesceFunDef fundef var_to_mem mem_aliases var_aliases first_uses
  last_uses actual_vars existentials =
  let primexps = findPrimExpsFunDef fundef
      exps = findExpsFunDef fundef
      cond2 = findSafetyCondition2FunDef fundef
      cond5 = findSafetyCondition5FunDef fundef first_uses
      context = Context { ctxFunDef = fundef
                        , ctxVarToMem = var_to_mem
                        , ctxMemAliases = mem_aliases
                        , ctxVarAliases = var_aliases
                        , ctxFirstUses = first_uses
                        , ctxLastUses = last_uses
                        , ctxActualVars = actual_vars
                        , ctxExistentials = existentials
                        , ctxVarPrimExps = primexps
                        , ctxVarExps = exps
                        , ctxAllocatedBlocksBeforeCreation = cond2
                        , ctxVarsInUseBeforeMem = cond5
                        }
      m = unFindM $ lookInBody $ funDefBody fundef
      var_to_mem_res = curMemsCoalesced $ fst $ execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings var_to_mem_res fundef

      debug = var_to_mem_res `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "coreCoalesceFunDef coalescing results:"
        forM_ (M.assocs var_to_mem_res) $ \(src, dstmem) ->
          putStrLn ("Source " ++ pretty src ++ " coalesces into "
                    ++ pretty (memLocName dstmem) ++ "; ixfun: "
                    ++ show (memLocIxFun dstmem))
        putStrLn $ pretty fundef'
        putStrLn $ replicate 70 '='

      debug_json =
        forM_ (M.assocs var_to_mem_res) $ \(src, dstmem) ->
          putStrLn ("[\"coalescing\", \"" ++ pretty src ++
                    "\", \"" ++ pretty (memLocName dstmem) ++ "\"]")

  in withDebugJSON debug_json $ withDebug debug fundef'

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStm bnds

zeroOffset :: PrimExp VName
zeroOffset = primExpFromSubExp (IntType Int32) (constant (0 :: Int32))

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
  -- COALESCING-SPECIFIC HANDLING for Copy and Concat.
  case patvalelems of
    [PatElem dst bindage ExpMem.ArrayMem{}] -> do
      -- We create a function and pass it around instead of just applying it to
      -- the memory of the MemBound.  We do this, since any source variables
      -- might have more actual variables with different index functions that
      -- also need to be fixed -- e.g. in the case of reshape, where both the
      -- reshaped array and the original array need to get their index functions
      -- updated.
      let ixfun_slices = case bindage of
            BindVar -> []
            BindInPlace _ slice ->
              let slice' = map (primExpFromSubExp (IntType Int32) <$>) slice
              in [slice']
      case e of
        -- Copy.
        BasicOp (Copy src) ->
          tryCoalesce dst ixfun_slices bindage src zeroOffset

        -- Concat.
        BasicOp (Concat 0 src0 src0s _) -> do
          let srcs = src0 : src0s
          shapes <- mapM ((memSrcShape <$>) . lookupVarMem) srcs
          let getOffsets offset_prev shape =
                let se = head (shapeDims shape) -- Should work.
                    len = primExpFromSubExp (IntType Int32) se
                    offset_new = BinOpExp (Add Int32) offset_prev len
                in offset_new
              offsets = init (scanl getOffsets zeroOffset shapes)
          zipWithM_ (tryCoalesce dst ixfun_slices bindage) srcs offsets

        _ -> return ()
    _ -> return ()


  -- RECURSIVE BODY WALK.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInBody . lambdaBody
          }

tryCoalesce :: LoreConstraints lore =>
               VName -> [Slice (PrimExp VName)] -> Bindage ->
               VName -> PrimExp VName -> FindM lore ()
tryCoalesce dst ixfun_slices bindage src offset = do
  mem_dst <- lookupVarMem dst

  -- For ifs and loops and some aliasing expressions (e.g. reshape), this tells
  -- us what non-existential source variables actually need to have assigned the
  -- new memory block.
  src's <- S.toList <$> lookupActualVars src

  -- From earlier optimistic coalescings.  Remember to also get the coalescings
  -- from the actual variables in e.g. loops.
  coalesced_intos <- gets curCoalescedIntos
  let (src0s, offset0s, ixfun_slice0ss) =
        unzip3 $ S.toList $ S.unions $ map (`lookupEmptyable` coalesced_intos) (src : src's)

  let srcs = src's ++ src0s
                -- The same number of base offsets as in src's.
      offsets = replicate (length src's) offset
                -- The offsets of any previously optimistically coalesced src0s must be
                -- re-offset relative to the offset of the newest coalescing.
                ++ map (\o0 -> if o0 == zeroOffset && offset == zeroOffset
                                    -- This should not be necessary, and maybe it
                                    -- is not (but there were some problems).
                               then zeroOffset
                               else BinOpExp (Add Int32) offset o0) offset0s
      ixfun_slicess = replicate (length src's) ixfun_slices
                -- Same as above, kind of.
                ++ map (\slices0 -> ixfun_slices ++ slices0) ixfun_slice0ss

  var_to_pe <- asks ctxVarPrimExps
  let ixfuns' = zipWith (\offset_local islices ->
                           let ixfun0 = foldl IxFun.slice (memSrcIxFun mem_dst) islices
                               ixfun1 = if offset_local == zeroOffset
                                        then ixfun0 -- Should not be necessary,
                                                    -- but it makes the type
                                                    -- checker happy for now.
                                        else IxFun.offsetIndex ixfun0 offset_local
                           in expandIxFun var_to_pe ixfun1
                        ) offsets ixfun_slicess

  -- Not everything supported yet.  This dials back the optimisation on areas
  -- where it fails.
  existentials <- asks ctxExistentials
  let currentlyDisabled src_local ixfun_slices_local = do
        -- This case covers the problem described in
        -- tests/coalescing/wip/loop/loop-0.fut.

        src_local_is_loop <- isLoopExp src_local

        let res = src_local_is_loop
                  && src_local `L.elem` existentials
                  && not (L.null ixfun_slices_local)

            debug = do
              putStrLn $ replicate 70 '~'
              putStrLn "currentlyDisabled:"
              putStrLn ("var: " ++ pretty src_local)
              putStrLn ("dst mem: " ++ show mem_dst)
              putStrLn ("existentials: " ++ show existentials)
              putStrLn ("ixfun_slices: " ++ show ixfun_slices_local)
              putStrLn $ replicate 70 '~'
        withDebug debug $ return res

  safe0 <- (not . or) <$> zipWithM currentlyDisabled srcs ixfun_slicess

  -- Safety condition 1 is the same for all eventual previous arrays from srcs
  -- that also need to be coalesced into dst, so we check it here instead of
  -- checking it independently for every sub src.  This also ensures that we
  -- check that the destination memory is lastly used in *just* this statement,
  -- not also in any previous statement that uses the same memory block, which
  -- could very well fail.
  mem_src_base <- lookupVarMem src
  safe1 <- safetyCond1 dst mem_src_base

  when (safe0 && safe1) $ do
    safes <- zipWithM (canBeCoalesced dst) srcs ixfuns'
    when (and safes) $ do
      -- Any previous src0s coalescings must be deleted.
      modifyCurCoalescedIntos $ M.delete src
      -- The rest will be overwritten below.

      -- We then need to record that, from what we currently know, src and any
      -- nested src0s can all use the memory of dst with the new index functions.
      forM_ (L.zip4 srcs offsets ixfun_slicess ixfuns')
        $ \(src_local, offset_local, ixfun_slices_local, ixfun_local) -> do
        denotes_existential <- S.member src_local <$> asks ctxExistentials
        dst_memloc <-
          if denotes_existential
          then do
            -- Only use the new index function.  Keep the existential memory
            -- block.
            mem_src <- lookupVarMem src_local
            return $ MemoryLoc (memSrcName mem_src) ixfun_local
          else
            -- Use both the new memory block and the new index function.
            return $ MemoryLoc (memSrcName mem_dst) ixfun_local
        recordOptimisticCoalescing
          src_local offset_local ixfun_slices_local
          dst dst_memloc bindage

  let debug = safe0 `seq` safe1 `seq` ixfun_slices `seq` coalesced_intos `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn ("tryCoalesce: src " ++ pretty src ++ ", dst " ++ pretty dst)
        putStrLn ("actual vars for src " ++ pretty src ++ ": "
                  ++ prettySet (S.fromList src's))
        putStrLn ("safe0: " ++ show safe0)
        putStrLn ("safe1: " ++ show safe1)
        putStrLn ("input slices: " ++ show ixfun_slices)
        putStrLn ("coalesced-intos full: " ++ show coalesced_intos)
        putStrLn ("coalesced-into vars for " ++ pretty src ++ ": "
                  ++ prettySet (S.fromList src0s))
        putStrLn ("all srcs for " ++ pretty src ++ ": " ++ prettySet (S.fromList srcs))
        putStrLn ("all slices for " ++ pretty src ++ ": " ++ show ixfun_slicess)
        putStrLn ("all offsets for " ++ pretty src ++ ": " ++ show offsets)
        putStrLn ("all ixfuns for " ++ pretty src ++ ":\n" ++ L.intercalate "\n" (map show ixfuns'))
        putStrLn $ replicate 70 '~'
  withDebug debug $ return ()

canBeCoalesced :: LoreConstraints lore =>
                  VName -> VName -> ExpMem.IxFun -> FindM lore Bool
canBeCoalesced dst src ixfun = do
  mem_dst <- lookupVarMem dst
  mem_src <- lookupVarMem src

  safe2 <- safetyCond2 src mem_dst
  safe3 <- safetyCond3 src dst mem_dst
  safe4 <- safetyCond4 src
  safe5 <- safetyCond5 mem_src ixfun

  safe_if <- safetyIf src dst

  let safe_all = safe2 && safe3 && safe4 && safe5 && safe_if

      debug = safe_all `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "canBeCoalesced:"
        putStrLn ("dst: " ++ pretty dst ++ ", src: " ++ pretty src)
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("mem_src: " ++ show mem_src)
        putStrLn ("safe (" ++ pretty src ++ "): " ++ L.intercalate ", "
                  -- Safety condition 1 is true if canBeCoalesced is run.
                  (map show [True, safe2, safe3, safe4, safe5])
                  ++ "; safe If: " ++ show safe_if)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return safe_all

-- Safety conditions for each statement with a Copy or Concat:
--
-- 1. mem_src is not used beyond the statement.  Handle by checking LastUses for
--    the statement.
--
-- 2. The allocation of mem_dst occurs before the creation of src, i.e. the
--    first use of mem_src.  Handle by checking
--    ctxAllocatedBlocksBeforeCreation.
--
-- 3. There is no use or creation of mem_dst after the creation of src and
--    before the current statement.  Handle by calling getVarUsesBetween and
--    looking at both the original var-mem mappings *and* the new, temporary
--    ones.
--
-- 4. src (the variable, not the memory) does not alias anything.  Handle by
--    checking VarAliases.
--
-- 5. The new index function of src only uses variables declared prior to the
--    first use of mem_src.  Handle by first using curVarPrimExps and
--    ExpMem.substituteInIxFun to create a (possibly larger) index function that
--    uses earlier variables.  Then use ctxVarsInUseBeforeMem to check that all
--    the variables in the new index function are available before the creation
--    of mem_src.
--
-- If an array src0 has been coalesced into mem_src, handle that by *also*
-- checking src0 and mem_src0 where src and mem_src are checked.  We choose to
-- coalesce in a top-down fashion, even though that might exclude some potential
-- coalescings -- however, doing it differently might exclude some other
-- potentials, so we just make a choice.
--
-- We only coalesce src into dst if all eventual src0 can also be coalesced into
-- dst.  It does not make sense to coalesce only part of them, since in that
-- case both memory blocks and related allocations will still be around.

safetyCond1 :: MonadReader Context m =>
               VName -> MemorySrc -> m Bool
safetyCond1 dst mem_src = do
  last_uses <- lookupEmptyable (FromStm dst) <$> asks ctxLastUses
  let res = S.member (memSrcName mem_src) last_uses

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond1:"
        putStrLn ("dst: " ++ pretty dst)
        putStrLn ("mem src: " ++ show mem_src)
        putStrLn ("last uses in dst: " ++ prettySet last_uses)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond2 :: MonadReader Context m =>
               VName -> MemorySrc -> m Bool
safetyCond2 src mem_dst = do
  allocs_before_src <- lookupEmptyable src
                       <$> asks ctxAllocatedBlocksBeforeCreation
  let res = S.member (memSrcName mem_dst) allocs_before_src

  let debug = allocs_before_src `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond2:"
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("src: " ++ pretty src)
        putStrLn ("allocs before src: " ++ prettySet allocs_before_src)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond3 :: LoreConstraints lore =>
               VName -> VName -> MemorySrc -> FindM lore Bool
safetyCond3 src dst mem_dst = do
  fundef <- asks ctxFunDef
  let uses_after_src_vars = S.toList $ getVarUsesBetween fundef src dst
  uses_after_src <- mapM (maybe (return S.empty) withMemAliases
                          <=< lookupCurrentVarMem) uses_after_src_vars
  let res = not $ S.member (memSrcName mem_dst) (S.unions uses_after_src)

      dbg_vars_causing_mem_use =
        map fst
        $ filter (\(_var, mems) -> memSrcName mem_dst `S.member` mems)
        $ zip uses_after_src_vars uses_after_src
      debug = uses_after_src `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond3:"
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("src: " ++ pretty src)
        putStrLn ("uses after src vars: " ++ prettyList uses_after_src_vars)
        putStrLn ("uses after src: " ++ prettySet (S.unions uses_after_src))
        putStrLn ("vars causing mem use: " ++ prettyList dbg_vars_causing_mem_use)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond4 :: MonadReader Context m =>
               VName -> m Bool
safetyCond4 src = do
  -- Special If handling: An If can have aliases, but that can be okay and is
  -- checked in safe If: It is okay for it to have one alias (one of the
  -- branches), while two aliases are wrong.
  if_handling <- isIfExp src

  -- Special Reshape handling: If a reshape has variables associated with it, it
  -- is okay to use it.
  src_actuals <- lookupEmptyable src <$> asks ctxActualVars
  reshape_handling <- isReshapeExp src <&&> pure (not (S.null src_actuals))

  -- This needs to be extended if support for e.g. reshape coalescing is wanted:
  -- Some operations can be aliasing, but still be okay to coalesce if you also
  -- coalesce their aliased sources.
  src_aliases <- lookupEmptyable src <$> asks ctxVarAliases
  let res = if_handling || reshape_handling || S.null src_aliases

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond4:"
        putStrLn ("src: " ++ pretty src)
        putStrLn ("src aliases: " ++ prettySet src_aliases)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond5 :: MonadReader Context m =>
               MemorySrc -> ExpMem.IxFun -> m Bool
safetyCond5 mem_src ixfun = do
  in_use_before_mem_src <- lookupEmptyable (memSrcName mem_src)
                           <$> asks ctxVarsInUseBeforeMem
  let used_vars = freeIn ixfun
      res = all (`S.member` in_use_before_mem_src) $ S.toList used_vars

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond5:"
        putStrLn ("mem_src: " ++ show mem_src)
        putStrLn ("ixfun: " ++ show ixfun)
        putStrLn ("in use before mem_src: " ++ prettySet in_use_before_mem_src)
        putStrLn ("used vars: " ++ prettySet used_vars)
        putStrLn ("remaining vars: " ++ prettySet (used_vars `S.difference` in_use_before_mem_src))
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyIf :: LoreConstraints lore =>
            VName -> VName -> FindM lore Bool
safetyIf src dst = do
  -- Special handling: If src refers to an If expression, we need to check that
  -- not just is mem_dst not used after src and before dst, but neither is any
  -- other memory that will be merged after the coalescing.  Normally this is
  -- not an issue, since a coalescing means changing just one memory block --
  -- but in the case of an If expression, each branches can have its own memory
  -- block, and both of them will try to be coalesced.  This extra test only
  -- applies to the actual memory blocks in the branches, not any existential
  -- memory block in the If, which in any case will be "used" in both branches.
  --
  -- See tests/coalescing/if/if-neg-3.fut for an example of where this should
  -- fail.
  mem_src <- lookupVarMem src
  actual_srcs <- S.toList <$> lookupActualVars src
  existentials <- asks ctxExistentials
  var_to_mem <- asks ctxVarToMem
  first_uses_all <- asks ctxFirstUses

  -- Find all variables that have 'src' as an actual var, and then check if one
  -- of those is an If expression.
  reverse_actual_srcs <-
    (S.toList . S.unions . M.elems . M.filter (src `S.member`))
    <$> asks ctxActualVars
  outer <- mapMaybeM ifExp reverse_actual_srcs
  let (is_in_if,
       if_branch_results_from_outer,
       at_least_one_creation_inside) = case outer of
        -- This is the if expression of which we are currently looking at one of
        -- its branch results.
        [Exp npat (If _ body0 body1 _)] ->
          let results_from_outer = S.fromList $ mapMaybe fromVar
                                   $ concatMap bodyResult
                                   $ filter (null . bodyStms) [body0, body1]

              resultCreatedInside body se = fromMaybe False $ do
                res <- fromVar se
                res_mem <- memSrcName <$> M.lookup res var_to_mem
                let body_vars = concatMap (map patElemName . patternValueElements
                                           . stmPattern) $ bodyStms body
                    body_first_uses = S.unions $ map (`lookupEmptyable` first_uses_all)
                                      body_vars
                return $ S.member res_mem body_first_uses

              at_least = resultCreatedInside body0 (bodyResult body0 !! npat)
                         || resultCreatedInside body1 (bodyResult body1 !! npat)
          in (True, results_from_outer, at_least)
        _ -> (False, S.empty, False)

  -- This success requirement is independent of whichever branch we are in right
  -- now.  We say that the results of an if-expression can be coalesced if the
  -- branch-specific requirements hold *and* this general rule holds: Either the
  -- If has no existentials (e.g. if it does in-place updates), or it has
  -- existentials and at least one of the branches returns an array that was
  -- created inside the branch.
  let res_general = not is_in_if || (not (any (`S.member` existentials) actual_srcs)
                                     || at_least_one_creation_inside)

  -- Check if the branch described by 'src' needs special handling.
  let if_handling =
        -- We are sure this is an if.  This might not actually be necessary.
        is_in_if
        -- This does not refer to the result of a branch where the array is
        -- created outside the if.  It is a requirement that there is at most
        -- one such branch.  The extra safety here only relates to branches
        -- whose result arrays are created inside.
        && not (any (`S.member` if_branch_results_from_outer) actual_srcs)
        -- Ignore existentials as well.
        && not (src `S.member` existentials)

  -- This success requirement is part is specific to this branch.
  res_current <-
    if if_handling
    then do
      -- Get the memory used in the other branch.  Use a reverse lookup.
      mem_actual_srcs <- L.nub <$> mapM lookupVarMem reverse_actual_srcs
      let mem_actual_srcs_cur = L.delete mem_src mem_actual_srcs
      and <$> mapM (safetyCond3 src dst) mem_actual_srcs_cur
    else return True

  -- The full result.
  let res = res_general && res_current

  let debug = res `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "safeIf:"
        putStrLn ("src: " ++ pretty src)
        putStrLn ("actual_srcs: " ++ prettySet (S.fromList actual_srcs))
        putStrLn ("reverse actual_srcs: " ++ prettySet (S.fromList reverse_actual_srcs))
        putStrLn ("outer: " ++ show outer)
        putStrLn ("if branch results from outer: "
                  ++ prettySet if_branch_results_from_outer)
        putStrLn ("at least one creation inside: "
                  ++ show at_least_one_creation_inside)
        putStrLn ("if handling: " ++ show if_handling
                  ++ " (is in if: " ++ show is_in_if ++ ")")
        putStrLn ("res safe: " ++ show res)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res
