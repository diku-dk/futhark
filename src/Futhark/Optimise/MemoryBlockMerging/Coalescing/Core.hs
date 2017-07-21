{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Optimise.MemoryBlockMerging.Coalescing.Core
  ( coreCoalesceFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (maybe, fromMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.MemoryUpdater

import Futhark.Optimise.MemoryBlockMerging.Coalescing.PrimExps (findPrimExpsFunDef)


-- Some of these attributes could be split into separate Coalescing helper
-- modules if it becomes confusing.  Their computations are fairly independent.
data Current = Current
  { -- Safety condition 2 state.
    curAllocatedBlocks :: Names
  , curAllocatedBlocksBeforeCreation :: M.Map VName Names

    -- Safety condition 3 state.
  , curVarUsesAfterCreation :: M.Map VName Names

    -- Safety condition 5 state.
  , curDeclarationsSoFar :: Names
  , curVarsInUseBeforeMem :: M.Map VName Names

    -- Coalescings state.  Also save offsets and slices in the case that an
    -- optimistic coalescing later becomes part of a chain of coalescings, where
    -- it is offset yet again, and where it should maintain its old relative
    -- offset.
  , curCoalescedIntos :: M.Map VName (S.Set (VName, PrimExp VName,
                                             [Slice (PrimExp VName)]))
  , curMemsCoalesced :: M.Map VName MemoryLoc

    -- WIP.  Keeps track of loops due to problem in
    -- tests/coalescing/wip/loop/loop-0.fut.
  , curLoopVars :: Names
    -- Keeps track of If expressions to handle more cases with safety condition
    -- 4.  A bit messy.
  , curIfVars :: Names
  }
  deriving (Show)

emptyCurrent :: Current
emptyCurrent = Current
  { curAllocatedBlocks = S.empty
  , curAllocatedBlocksBeforeCreation = M.empty

  , curVarUsesAfterCreation = M.empty

  , curDeclarationsSoFar = S.empty
  , curVarsInUseBeforeMem = M.empty

  , curCoalescedIntos = M.empty
  , curMemsCoalesced = M.empty

  , curLoopVars = S.empty
  , curIfVars = S.empty
  }

data Context = Context
  { ctxVarToMem :: VarMemMappings MemorySrc
  , ctxMemAliases :: MemAliases
  , ctxVarAliases :: VarAliases
  , ctxFirstUses :: FirstUses
  , ctxLastUses :: LastUses

    -- If and DoLoop statements have special requirements, as do some aliasing
    -- expressions.  We don't want to (just) use the obvious statement variable;
    -- sometimes updating the memory block of one variable actually means
    -- updating the memory block of other variables!
  , ctxActualVars :: M.Map VName Names

    -- Sometimes it is convenient to treat existential memory blocks the same as
    -- normal ones (e.g. putting them in curActualVars), but we still must keep
    -- track of them so as to only change the index functions of variables using
    -- them, not the memory block.
  , ctxExistentials :: Names

  , ctxVarPrimExps :: M.Map VName (PrimExp VName)
  }
  deriving (Show)

newtype FindM a = FindM { unFindM :: RWS Context () Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadState Current)

-- Lookup the memory block statically associated with a variable.
lookupVarMem :: VName -> FindM MemorySrc
lookupVarMem var =
  -- This should always be called from a place where it is certain that 'var'
  -- refers to a statement with an array expression.
  (fromJust ("lookup memory block from " ++ pretty var) . M.lookup var)
  <$> asks ctxVarToMem

lookupActualVars :: VName -> FindM Names
lookupActualVars var =
  (fromMaybe (S.singleton var) . M.lookup var) <$> asks ctxActualVars

-- Lookup the memory block currenty associated with a variable.  In most cases
-- (maybe all) this could probably replace 'lookupVarMem', though it would not
-- always be necessary.
lookupCurrentVarMem :: VName -> FindM (Maybe VName)
lookupCurrentVarMem var = do
        -- Current result...
        mem_cur <- M.lookup var <$> gets curMemsCoalesced
        -- ... or original result.
        --
        -- This is why we save the variables after creation (in
        -- 'curVarsInUseBeforeMem'), not the memory blocks: Variables stay the
        -- same, but memory blocks may change, which is relevant in the case of
        -- a chain of coalescings.
        mem_orig <- M.lookup var <$> asks ctxVarToMem
        return $ case (mem_cur, mem_orig) of
          (Just m, _) -> Just (memLocName m) -- priority choice
          (_, Just m) -> Just (memSrcName m)
          _ -> Nothing

recordOptimisticCoalescing :: VName -> PrimExp VName
                           -> [Slice (PrimExp VName)]
                           -> VName -> MemoryLoc -> Bindage -> FindM ()
recordOptimisticCoalescing src offset ixfun_slices dst dst_memloc bindage = do
  modify $ \c -> c { curCoalescedIntos =
                     insertOrUpdate dst (src, offset, ixfun_slices)
                     $ curCoalescedIntos c }

  -- If this is an in-place operation, we future-proof future coalescings by
  -- recording that they also need to take a look at the original array, not
  -- just the result of an in-place update into it.
  case bindage of
    BindVar -> return ()
    BindInPlace _ orig _ ->
      modify $ \c -> c { curCoalescedIntos = insertOrUpdate dst (orig, zeroOffset, [])
                                             $ curCoalescedIntos c }

  modify $ \c -> c { curMemsCoalesced = M.insert src dst_memloc $ curMemsCoalesced c }

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "recordOptimisticCoalescing"
        putStrLn ("dst: " ++ pretty dst ++ "; src: " ++ pretty src)
        putStrLn ("slices: " ++ show ixfun_slices)
        putStrLn $ replicate 70 '~'
  doDebug debug

coreCoalesceFunDef :: FunDef ExplicitMemory -> VarMemMappings MemorySrc
                   -> MemAliases -> VarAliases -> FirstUses -> LastUses
                   -> (ActualVariables, Names) -> FunDef ExplicitMemory
coreCoalesceFunDef fundef var_to_mem mem_aliases var_aliases first_uses last_uses (actual_vars, existentials) =
  let primexps = findPrimExpsFunDef fundef
      context = Context var_to_mem mem_aliases var_aliases first_uses last_uses actual_vars existentials primexps
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
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

  in withDebug debug fundef'

lookInFParam :: FParam ExplicitMemory -> FindM ()
lookInFParam (Param x membound) = do
  modify $ \c -> c { curDeclarationsSoFar = S.insert x $ curDeclarationsSoFar c }

  -- Unique array function parameters also count as "allocations" in which
  -- memory can be coalesced.
  case membound of
    ExpMem.ArrayMem _ _ Unique mem _ ->
      modify $ \c -> c { curAllocatedBlocks = S.insert mem $ curAllocatedBlocks c }
    _ -> return ()

lookInBody :: Body ExplicitMemory -> FindM ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

zeroOffset :: PrimExp VName
zeroOffset = primExpFromSubExp (IntType Int32) (constant (0 :: Int32))

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern patctxelems patvalelems) () e) = do
  case (patvalelems, e) of
    ([PatElem dst _ ExpMem.ArrayMem{}], DoLoop{}) ->
      modify $ \c -> c { curLoopVars = S.insert dst $ curLoopVars c }
    ([PatElem dst _ ExpMem.ArrayMem{}], If{}) ->
      modify $ \c -> c { curIfVars = S.insert dst $ curIfVars c }
    _ -> return ()

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
            BindInPlace _ _ slice ->
              let slice' = map (primExpFromSubExp (IntType Int32) <$>) slice
              in [slice']
      case e of
        -- Copy.
        BasicOp (Copy src) ->
          tryCoalesce dst ixfun_slices bindage src zeroOffset

        -- Concat.
        BasicOp (Concat _ 0 src0 src0s _) -> do
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


  -- GENERAL STATE UPDATING.
  let new_decls0 = map patElemName (patctxelems ++ patvalelems)
      new_decls1 = case e of
        DoLoop _mergectxparams mergevalparams _loopform _body ->
          -- Technically not a declaration for the current expression, but very
          -- close, and hopefully okay to consider it as one.
          map (paramName . fst) mergevalparams
        _ -> []
      new_decls = new_decls0 ++ new_decls1

  -- Update attributes related to safety condition 2.
  forM_ new_decls $ \x ->
    modify $ \c -> c { curAllocatedBlocksBeforeCreation =
                         M.insert x (curAllocatedBlocks c)
                         $ curAllocatedBlocksBeforeCreation c }

  case (patvalelems, e) of
    ([PatElem mem _ _], Op ExpMem.Alloc{}) ->
      modify $ \c -> c { curAllocatedBlocks = S.insert mem $ curAllocatedBlocks c }
    _ -> return ()


  -- Update attributes related to safety condition 3.
  let e_free_vars = freeInExp e -- FIXME: should not include those consumed
      e_used_vars = S.union e_free_vars (S.fromList new_decls)

  -- Update the existing entries.
  --
  -- Note that "used after creation" refers both to used in subsequent
  -- statements AND any statements in any sub-bodies (if and loop).
  uses_after <- gets curVarUsesAfterCreation
  forM_ (M.keys uses_after) $ \x ->
    modify $ \c -> c { curVarUsesAfterCreation =
                         M.adjust (S.union e_used_vars) x
                         $ curVarUsesAfterCreation c }

  -- Create the new entries for the current statement.
  forM_ new_decls $ \x ->
    modify $ \c -> c { curVarUsesAfterCreation =
                         M.insert x S.empty
                         $ curVarUsesAfterCreation c }


  -- Update attributes related to safety condition 5.
  first_uses <- asks ctxFirstUses
  forM_ (S.toList $ S.unions $ map (`lookupEmptyable` first_uses) new_decls) $ \mem ->
    modify $ \c -> c { curVarsInUseBeforeMem =
                       M.insert mem (curDeclarationsSoFar c)
                       $ curVarsInUseBeforeMem c }

  forM_ new_decls $ \x ->
    modify $ \c -> c { curDeclarationsSoFar = S.insert x $ curDeclarationsSoFar c }


  -- RECURSIVE BODY WALK.
  let walker = identityWalker { walkOnBody = lookInBody
                              , walkOnFParam = lookInFParam
                              }
  walkExpM walker e -- Maybe this needs to be run locally for some of the state
                    -- fields.

withMemAliases :: VName -> FindM Names
withMemAliases mem = do
  -- The only memory blocks with memory aliases are the existiential ones, so
  -- using a static ctxMemAliases should be okay, as they will not change during
  -- the transformation in this module.
  mem_aliases <- lookupEmptyable mem <$> asks ctxMemAliases
  return $ S.union (S.singleton mem) mem_aliases

-- Replace variables with subtrees of their constituents wherever possible.  It
-- is debatable whether this is "simplifying", but it can enable more
-- expressions to use the index function.
simplifyIxFun :: ExpMem.IxFun -> FindM ExpMem.IxFun
simplifyIxFun ixfun = do
  var_to_pe <- asks ctxVarPrimExps
  let ixfun' = fixpointIterate (IxFun.substituteInIxFun var_to_pe) ixfun
  return ixfun'

tryCoalesce :: VName -> [Slice (PrimExp VName)] -> Bindage -> VName
            -> PrimExp VName -> FindM ()
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
                ++ map (\o0 -> if o0 == zeroOffset || offset == zeroOffset
                                  -- This should not be necessary, and maybe it
                                  -- is not (but there were some problems).
                               then zeroOffset
                               else BinOpExp (Add Int32) offset o0) offset0s
      ixfun_slicess = replicate (length src's) ixfun_slices
                -- Same as above, kind of.
                ++ map (\slices0 -> ixfun_slices ++ slices0) ixfun_slice0ss

  ixfuns' <- zipWithM (\offset_local islices -> do
                          let ixfun0 = foldl IxFun.slice (memSrcIxFun mem_dst) islices
                              ixfun1 = if offset_local == zeroOffset
                                       then ixfun0 -- Should not be necessary,
                                                   -- but it makes the type
                                                   -- checker happy for now.
                                       else IxFun.offsetIndex ixfun0 offset_local
                          simplifyIxFun ixfun1
                      ) offsets ixfun_slicess

  -- Not everything supported yet.  This dials back the optimisation on areas
  -- where it fails.
  loop_vars <- gets curLoopVars
  existentials <- asks ctxExistentials
  let currentlyDisabled (src_local, ixfun_slices_local) =
        -- This case covers the problem described in
        -- tests/coalescing/wip/loop/loop-0.fut.
        let res = src_local `L.elem` loop_vars
                  && src_local `L.elem` existentials
                  && not (L.null ixfun_slices_local)

            debug = do
              putStrLn $ replicate 70 '~'
              putStrLn "currentlyDisabled:"
              putStrLn ("var: " ++ pretty src_local)
              putStrLn ("dst mem: " ++ show mem_dst)
              putStrLn ("loop vars: " ++ show loop_vars)
              putStrLn ("existentials: " ++ show existentials)
              putStrLn ("ixfun_slices: " ++ show ixfun_slices_local)
              putStrLn $ replicate 70 '~'
        in withDebug debug res

  let safe0 = not $ any currentlyDisabled $ zip srcs ixfun_slicess

  -- Safety condition 1 is the same for all eventual previous arrays from srcs
  -- that also need to be coalesced into dst, so we check it here instead of
  -- checking it independently for every sub src.  This also ensures that we
  -- check that the destination memory is lastly used in *just* this statement,
  -- not also in any previous statement that uses the same memory block, which
  -- could very well fail.
  mem_src_base <- lookupVarMem src
  safe1 <- safetyCond1 dst mem_src_base

  when (safe0 && safe1) $ do
    safes <- zipWithM (canBeCoalesced dst mem_dst) srcs ixfuns'
    when (and safes) $ do
      -- Any previous src0s coalescings must be deleted.
      modify $ \c -> c { curCoalescedIntos = M.delete src $ curCoalescedIntos c }
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

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "tryCoalesce:"
        putStrLn ("actual vars for " ++ pretty src ++ ": "
                  ++ prettySet (S.fromList src's))
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

canBeCoalesced :: VName -> MemorySrc -> VName -> ExpMem.IxFun -> FindM Bool
canBeCoalesced dst mem_dst src ixfun = do
  mem_src <- lookupVarMem src

  safe2 <- safetyCond2 mem_dst src
  safe3 <- safetyCond3 mem_dst src
  safe4 <- safetyCond4 src
  safe5 <- safetyCond5 mem_src ixfun

  let safe_all = safe2 && safe3 && safe4 && safe5

  let debug = safe_all `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "canBeCoalesced:"
        putStrLn ("dst: " ++ pretty dst ++ ", src: " ++ pretty src)
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("safe: " ++ L.intercalate ", "
                  -- Safety condition 1 is true if canBeCoalesced is run.
                  (map show [True, safe2, safe3, safe4, safe5]))
        putStrLn $ replicate 70 '~'
  withDebug debug $ return safe_all

-- Safety conditions for each statement with a Copy or Concat:
--
-- 1. mem_src is not used beyond the statement.  Handle by checking LastUses for
--    the statement.
--
-- 2. The allocation of mem_dst occurs before the creation of src, i.e. the
--    first use of mem_src.  Handle by checking curAllocatedBlocksBeforeCreation
--    (which depends on curAllocatedBlocks).
--
-- 3. There is no use or creation of mem_dst after the creation of src and
--    before the current statement.  Handle by checking curVarUsesAfterCreation
--    and looking at both the original var-mem mappings *and* the new, temporary
--    ones.
--
-- 4. src (the variable, not the memory) does not alias anything.  Handle by
--    checking VarAliases.
--
-- 5. The new index function of src only uses variables declared prior to the
--    first use of mem_src.  Handle by first using curVarPrimExps and
--    ExpMem.substituteInIxFun to create a (possibly larger) index function that
--    uses earlier variables.  Then use curVarsInUseBeforeMem (which depends on
--    curDeclarationsSoFar) to check that all the variables in the new index function
--    are available before the creation of mem_src.
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

safetyCond1 :: VName -> MemorySrc -> FindM Bool
safetyCond1 dst mem_src = do
  last_uses <- lookupEmptyable dst <$> asks ctxLastUses
  let res = S.member (memSrcName mem_src) last_uses

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond1:"
        putStrLn ("dst: " ++ pretty dst)
        putStrLn ("mem src: " ++ show mem_src)
        putStrLn ("last uses in dst: " ++ prettySet last_uses)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond2 :: MemorySrc -> VName -> FindM Bool
safetyCond2 mem_dst src = do
  allocs_before_src <- lookupEmptyable src
                       <$> gets curAllocatedBlocksBeforeCreation
  let res = S.member (memSrcName mem_dst) allocs_before_src

  let debug = allocs_before_src `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond2:"
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("src: " ++ pretty src)
        putStrLn ("allocs before src: " ++ prettySet allocs_before_src)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond3 :: MemorySrc -> VName -> FindM Bool
safetyCond3 mem_dst src = do
  uses_after_src_vars <- lookupEmptyable src <$> gets curVarUsesAfterCreation
  uses_after_src <- S.unions <$> mapM (maybe (return S.empty) withMemAliases
                                       <=< lookupCurrentVarMem)
                    (S.toList uses_after_src_vars)
  let res = not $ S.member (memSrcName mem_dst) uses_after_src

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond3:"
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("src: " ++ pretty src)
        putStrLn ("uses after src vars: " ++ prettySet uses_after_src_vars)
        putStrLn ("uses after src: " ++ prettySet uses_after_src)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond4 :: VName -> FindM Bool
safetyCond4 src = do
  -- Special If handling: If it has been assigned actual variables in the
  -- ActualVariables pass, we know that it is okay for it to have one alias (one
  -- of the branches).  Two aliases are wrong; the previous code has checked
  -- that.
  if_vars <- gets curIfVars
  actual_vars <- asks ctxActualVars
  let if_handling = S.member src if_vars && M.member src actual_vars

  -- This needs to be extended if support for e.g. reshape coalescing is wanted:
  -- Some operations can be aliasing, but still be okay to coalesce if you also
  -- coalesce their aliased sources.
  src_aliases <- lookupEmptyable src <$> asks ctxVarAliases
  let res = if_handling || S.null src_aliases

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond4:"
        putStrLn ("src: " ++ pretty src)
        putStrLn ("src aliases: " ++ prettySet src_aliases)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res

safetyCond5 :: MemorySrc -> ExpMem.IxFun -> FindM Bool
safetyCond5 mem_src ixfun = do
  in_use_before_mem_src <- lookupEmptyable (memSrcName mem_src)
                           <$> gets curVarsInUseBeforeMem
  let used_vars = freeIn ixfun
      res = all (`S.member` in_use_before_mem_src) $ S.toList used_vars

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond5:"
        putStrLn ("mem_src: " ++ show mem_src)
        putStrLn ("ixfun: " ++ show ixfun)
        putStrLn ("in use before mem_src: " ++ prettySet in_use_before_mem_src)
        putStrLn ("used vars: " ++ prettySet used_vars)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return res
