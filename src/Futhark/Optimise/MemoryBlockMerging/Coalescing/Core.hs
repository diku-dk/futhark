{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Optimise.MemoryBlockMerging.Coalescing.Core
  ( coreCoalesceFunDef
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types


-- Some of these attributes could be split into separate Coalescing helper
-- modules if it becomes confusing.  Their computations are fairly independent.
data Current = Current
  { -- Safety condition 2 state.
    curAllocatedBlocks :: Names
  , curAllocatedBlocksBeforeCreation :: M.Map VName Names

    -- Safety condition 3 state.
--  , curMemUsesAfterCreation :: M.Map VName Names
  , curVarUsesAfterCreation :: M.Map VName Names

    -- Safety condition 5 state.
    -- '- For index function simplification.
  , curVarPrimTypes :: M.Map VName PrimType
  , curVarPrimExps :: M.Map VName (PrimExp VName)
    -- '- For index function variable-is-okay-to-use checking.
  , curDeclarationsSoFar :: Names
  , curVarsInUseBeforeMem :: M.Map VName Names

    -- If and DoLoop statements have special requirements, as do some aliasing
    -- expressions.  We don't want to (just) use the obvious statement variable;
    -- sometimes updating the memory block of one variable actually means
    -- updating the memory block of other variables!
  , curActualVars :: M.Map VName Names
    -- Sometimes it is convenient to treat existential memory blocks the same as
    -- normal ones (e.g. putting them in curActualVars), but we still must keep
    -- track of them so as to only change the index functions of variables using
    -- them, not the memory block.
  , curExistentials :: Names

    -- Coalescings state.  Also save offsets in the case that an optimistic
    -- coalescing later becomes part of a chain of coalescings, where it is
    -- offset yet again, and where it should maintain its old relative offset.
    --
    -- This is the worst part of this module right now, but should be fixable.
  , curCoalescedIntos :: M.Map VName [(VName, Maybe (PrimExp VName),
                                       Maybe (ExpMem.IxFun -> ExpMem.IxFun))]
  , curMemsCoalesced :: M.Map VName MemoryLoc
  }
--  deriving (Show)


emptyCurrent :: Current
emptyCurrent = Current
  { curAllocatedBlocks = S.empty
  , curAllocatedBlocksBeforeCreation = M.empty

  , curVarUsesAfterCreation = M.empty

  , curVarPrimTypes = M.empty
  , curVarPrimExps = M.empty
  , curDeclarationsSoFar = S.empty
  , curVarsInUseBeforeMem = M.empty

  , curActualVars = M.empty
  , curExistentials = S.empty

  , curCoalescedIntos = M.empty
  , curMemsCoalesced = M.empty
  }


data Context = Context { ctxVarToMem :: VarMemMappings MemorySrc
                       , ctxMemAliases :: MemAliases
                       , ctxVarAliases :: VarAliases
                       , ctxFirstUses :: FirstUses
                       , ctxLastUses :: LastUses
                       }
  deriving (Show)

newtype FindM a = FindM { unFindM :: RWS Context () Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadState Current)

lookupVarMem :: VName -> FindM (Maybe MemorySrc)
lookupVarMem var = do
  var_to_mem <- asks ctxVarToMem
  return $ M.lookup var var_to_mem


coreCoalesceFunDef :: FunDef ExplicitMemory -> VarMemMappings MemorySrc
                   -> MemAliases -> VarAliases -> FirstUses -> LastUses
                   -> FunDef ExplicitMemory
coreCoalesceFunDef fundef var_to_mem mem_aliases var_aliases first_uses last_uses =
  let context = Context var_to_mem mem_aliases var_aliases first_uses last_uses
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      var_to_mem_res = curMemsCoalesced $ fst $ execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings var_to_mem_res fundef

      debug = var_to_mem_res `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "coreCoalesceFunDef coalescing results:"
        forM_ (M.assocs var_to_mem_res) $ \(src, dstmem) ->
          putStrLn ("Source " ++ pretty src ++ " coalesces into " ++ pretty (memLocName dstmem) ++ "; ixfun: " ++ show (memLocIxFun dstmem))
        -- print fundef
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

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern patctxelems patvalelems) () e) = do
  let zero = primExpFromSubExp (IntType Int32) (constant (0 :: Int32))

  -- COALESCING-SPECIFIC HANDLING for Copy and Concat.
  case patvalelems of
    [PatElem dst bindage ExpMem.ArrayMem{}] -> do
      -- We create a function and pass it around instead of just applying it to
      -- the memory of the MemBound.  We do this, since any source variables
      -- might have more actual variables with different index functions that
      -- also need to be fixed -- e.g. in the case of reshape, where both the
      -- reshaped array and the original array need to get their index functions
      -- updated.
      --
      -- FIXME: This currently fails for reshape expressions, which really are
      -- the only ones benefiting from it, so that's a bit silly.  The problem
      -- is that the slice is relative to the shape of the reshaped array, and
      -- not the original array.
      let ixfunFix = case bindage of
            BindVar -> Nothing
            BindInPlace _ _ slice ->
              let slice' = map (primExpFromSubExp (IntType Int32) <$>) slice
              in Just $ \ixfun -> IxFun.slice ixfun slice'
      case e of
        -- Copy.
        BasicOp (Copy src) ->
          tryCoalesce dst ixfunFix bindage src Nothing

        -- Concat.
        BasicOp (Concat _ 0 src0 src0s _) -> do
          let srcs = src0 : src0s
          shapes <- mapM (fmap (memSrcShape
                                . shouldWork "memory block should exist")
                          . lookupVarMem) srcs
          let getOffsets offset_prev shape =
                let se = head (shapeDims shape) -- Should work.
                    len = primExpFromSubExp (IntType Int32) se
                    offset_new = BinOpExp (Add Int32) offset_prev len
                in offset_new
              offsets = init (scanl getOffsets zero shapes)
          zipWithM_ (tryCoalesce dst ixfunFix bindage) srcs (map Just offsets)

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
  let e_free_vars = freeInExp e -- should not include CONSUMED
      e_used_vars = S.union e_free_vars (S.fromList new_decls) -- okay?

  -- Note that "used after creation" refers both to used in subsequent
  -- statements AND any statements in any sub-bodies (if and loop).

  -- Update the existing ones.
  uses_after <- gets curVarUsesAfterCreation
  forM_ (M.keys uses_after) $ \x ->
    modify $ \c -> c { curVarUsesAfterCreation =
                         M.adjust (S.union e_used_vars) x
                         $ curVarUsesAfterCreation c }

  -- Create the new ones.
  forM_ new_decls $ \x ->
    modify $ \c -> c { curVarUsesAfterCreation =
                         M.insert x S.empty
                         $ curVarUsesAfterCreation c }


  -- Update attributes related to safety condition 5.
  prim_types <- gets curVarPrimTypes
  let varUse :: Exp ExplicitMemory -> Maybe (PrimExp VName)
      varUse (BasicOp (SubExp (Var v))) = do
        pt <- M.lookup v prim_types
        return $ ExpMem.LeafExp v pt
      varUse _ = Nothing
  case patvalelems of
    [PatElem dst _ _] ->
      onJust (primExpFromExp varUse e)
      $ \pe -> modify $ \c -> c { curVarPrimExps = M.insert dst pe $ curVarPrimExps c }
    _ -> return ()

  forM_ patvalelems $ \(PatElem var _ membound) ->
    case typeOf membound of
      Prim pt ->
        modify $ \c -> c { curVarPrimTypes = M.insert var pt $ curVarPrimTypes c }
      _ -> return ()

  first_uses <- asks ctxFirstUses
  forM_ (S.toList $ S.unions $ map (`lookupEmptyable` first_uses) new_decls) $ \mem ->
    modify $ \c -> c { curVarsInUseBeforeMem =
                       M.insert mem (curDeclarationsSoFar c)
                       $ curVarsInUseBeforeMem c }

  forM_ new_decls $ \x ->
    modify $ \c -> c { curDeclarationsSoFar = S.insert x $ curDeclarationsSoFar c }

  forM_ patvalelems $ \(PatElem var bindage _) ->
    case bindage of
      BindInPlace _ orig _ -> do
        -- Record that when coalescing an in-place update statement, also look
        -- at the original array.
        let actuals = [var, orig]
        modify $ \c -> c { curActualVars = M.insert var (S.fromList actuals)
                                           $ curActualVars c }
      _ -> return ()


  -- Special handling of loops, ifs, etc.
  case e of
    DoLoop _mergectxparams mergevalparams _loopform body ->
      forM_ patvalelems $ \(PatElem var _ membound) ->
        case membound of
          ExpMem.ArrayMem _ _ _ mem _ -> do
            -- If mem is existential, we need to find the return memory that it
            -- refers to.  We cannot just look at its memory aliases, since it
            -- likely aliases both the initial memory and the final memory.
            let zipped = zip patctxelems (bodyResult body)
            (var_actual, mem_search) <- case L.find ((== mem) . patElemName . fst) zipped of
              Just (_, Var res_mem) ->
                return (Nothing, res_mem)
              _ -> return (Just var, mem)
            let body_vars0 = mapMaybe (fromVar . snd) mergevalparams
                body_vars1 = map (paramName . fst) mergevalparams
                varFromStm stm = map patElemName (patternValueElements $ stmPattern stm)
                                 ++ foldExp folder [] (stmExp stm)
                folder = identityFolder { foldOnBody = \vs fbody -> return (vs ++ concatMap varFromStm (bodyStms fbody)) }
                body_vars2 = foldExp folder [] e -- Get all statements in the
                                                 -- body *and* any sub-bodies,
                                                 -- etc.
                body_vars = body_vars0 ++ body_vars1 ++ body_vars2
            -- Find the ones using the same memory as the result of the loop
            -- expression.
            body_vars' <- filterM (\v -> do
                                      m <- lookupVarMem v
                                      return $ Just mem_search == (memSrcName <$> m)
                                  ) body_vars
            -- Not only the result variable needs to change its memory block in case
            -- of a future coalescing with it; also the variables extracted above.
            -- FIXME: This is probably an okay way to do it?  What if the memory is
            -- used, but with a different index function?  Can that happen?
            let actuals = maybeToList var_actual ++ body_vars'
            modify $ \c -> c { curActualVars = M.insert var (S.fromList actuals)
                               $ curActualVars c }

            let debug = do
                  putStrLn $ replicate 70 '~'
                  putStrLn "coreCoalesceFunDef lookInStm DoLoop special handling:"
                  putStrLn ("checking memory: " ++ pretty mem_search)
                  putStrLn ("body vars: " ++ prettySet (S.fromList body_vars))
                  putStrLn ("proper body vars: " ++ prettySet (S.fromList body_vars'))
                  putStrLn $ replicate 70 '~'
            withDebug debug $ return ()

            -- If you extend this loop handling, make sure not to target existential
            -- memory blocks.  We want those to stay.
          _ -> return ()

    If _se body_then body_else _types ->
      -- We don't want to coalesce the existiential memory block of the if.
      -- However, if a branch result has a memory block that is firstly used
      -- inside the branch, it is okay to coalesce that in a future statement.
      forM_ (zip3 patvalelems (bodyResult body_then) (bodyResult body_else))
        $ \(PatElem var _ membound, res_then, res_else) ->
        case membound of
          (ExpMem.ArrayMem _ _ _ mem _) -> do
            var_to_mem <- asks ctxVarToMem
            first_uses_all <- asks ctxFirstUses
            let ifBlocks body se = do
                  res <- fromVar se
                  res_mem <- memSrcName <$> M.lookup res var_to_mem
                  let body_vars = concatMap (map patElemName . patternValueElements . stmPattern)
                                  $ bodyStms body
                      body_first_uses = S.unions $ map (`lookupEmptyable` first_uses_all) body_vars
                  Just $ return (S.member res_mem body_first_uses, res)

            case (ifBlocks body_then res_then,
                  ifBlocks body_else res_else) of
              (Just th, Just el) -> do -- prettify this stuff
                (alloc_inside_then, var_then) <- th
                (alloc_inside_else, var_else) <- el
                when (alloc_inside_then || alloc_inside_else) $ do
                  let actuals = [var, var_then, var_else]
                  modify $ \c -> c { curActualVars = M.insert var (S.fromList actuals)
                                     $ curActualVars c }
                  when (L.elem mem $ map patElemName patctxelems) $
                    modify $ \c -> c { curExistentials = S.insert var
                                       $ curExistentials c }
              _ -> return ()

            let debug = do
                  putStrLn $ replicate 70 '~'
                  putStrLn "coreCoalesceFunDef lookInStm If special handling:"
                  putStrLn $ replicate 70 '~'
            withDebug debug $ return ()
          _ -> return ()

    BasicOp (Reshape _ _ src0) -> do
      let var = patElemName $ head patvalelems -- probably ok
      -- Also refer to the array that it's reshaping.

      -- This might be redundant as it is also done later on.
      src0_actuals <- lookupEmptyable src0 <$> gets curActualVars
      let actuals = S.union (S.fromList [var, src0]) src0_actuals

      modify $ \c -> c { curActualVars = M.insert var actuals
                                         $ curActualVars c }


    -- For expressions other than DoLoop and If and some aliasing, don't do
    -- anything special.
    _ -> return ()

  -- RECURSIVE BODY WALK.
  let walker = identityWalker { walkOnBody = lookInBody
                              , walkOnFParam = lookInFParam
                              }
  walkExpM walker e -- Maybe this needs to be run locally for some of the state
                    -- fields.


-- Thought: The only memory blocks with "memory aliases" are the existiential
-- ones, so using a static ctxMemAliases should be okay, as they will not
-- change.
withMemAliases :: VName -> FindM Names
withMemAliases mem = do
  mem_aliases <- asks ctxMemAliases
  return $ S.union (S.singleton mem) $ lookupEmptyable mem mem_aliases

getActualVars :: VName -> FindM [VName]
getActualVars src0 = do
  res <- getActualVars' S.empty src0

  let debug = res `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "getActualVars:"
        putStrLn ("src: " ++ pretty src0)
        putStrLn ("result: " ++ prettySet (S.fromList res))
        putStrLn $ replicate 70 '~'

  withDebug debug $ return res
  where getActualVars' visited src
          | S.member src visited = return []
          | otherwise = do
              actual_vars <- gets curActualVars
              case M.lookup src actual_vars of
                Nothing -> return [src]
                Just a_srcs -> do
                  more <- mapM (getActualVars' (S.insert src visited))
                          $ S.toList a_srcs
                  return (S.toList a_srcs ++ concat more)

tryCoalesce :: VName -> Maybe (ExpMem.IxFun -> ExpMem.IxFun) -> Bindage -> VName
            -> Maybe (PrimExp VName) -> FindM ()
tryCoalesce dst ixfunFix bindage src offset = do
  mem_dst <- shouldWork "if dst is an array, mem_dst should also exist"
             <$> lookupVarMem dst

  -- For ifs and loops and some aliasing expressions (e.g. reshape), this tells
  -- us what non-existential source variables actually need to have assigned the
  -- new memory block.
  src's <- getActualVars src

  -- From earlier optimistic coalescings.  Remember to also get the coalescings
  -- from the actual variables in e.g. loops.
  coalesced_intos <- gets curCoalescedIntos
  let (src0s, offset0s, ixfunFix0s) =
        unzip3 $ concatMap (`lookupEmptyable` coalesced_intos) (src : src's)

  let srcs = src's ++ src0s
                -- The same number of base offsets as in src's.
      offsets = replicate (length src's) offset
                -- The offsets of any previously optimistically coalesced src0s must be
                -- re-offset relative to the offset of the newest coalescing.
                ++ map (\o0 -> do
                           o0' <- o0
                           offset' <- offset
                           return $ BinOpExp (Add Int32) offset' o0') offset0s
      ixfunFixs = replicate (length src's) ixfunFix
                -- Same as above, kind of.
                ++ map (\f0 -> case (f0, ixfunFix) of
                           (Nothing, Nothing) -> Nothing
                           _ -> let f0' = fromMaybe id f0
                                    ixfunFix' = fromMaybe id ixfunFix
                                    -- Run the new fixer first, and then any local fixer.
                                in Just $ f0' . ixfunFix') ixfunFix0s

  -- FIXME: This is mostly a hack to make reshape coalescing kind-of-work in a
  -- few cases.  It is not a hard problem to do properly, but there is some
  -- engineering to handling index functions and offsets across different bases
  -- correctly.  Instead of keeping offsets and ixFunFixs separately, just
  -- integrate it all into ixFunFixs, or save a list of slices.  Also get rid of
  -- the Maybe wrappers.
  ixfuns_src <- mapM (((memSrcIxFun . shouldWork
                        "it is an array, so it should also have a memory block") <$>)
                      . lookupVarMem) srcs
  let ixfuns0 = zipWith (\ifix isrc ->
                           case ifix of
                             Just f -> f $ memSrcIxFun mem_dst
                             Nothing -> isrc) ixfunFixs ixfuns_src
      ixfuns1 = zipWith (\i o -> case o of
                            Just o' -> IxFun.offsetIndex i o'
                            Nothing -> i) ixfuns0 offsets
  ixfuns2 <- mapM simplifyIxFun ixfuns1

  -- Safety condition 1 is the same for all eventual previous arrays from srcs
  -- that also need to be coalesced into dst, so we check it here instead of
  -- checking it independently for every sub src.  This also ensures that we
  -- check that the destination memory is lastly used in *just* this statement,
  -- not also in any previous statement, which could very well fail.
  mem_src_base <- shouldWork "is array" <$> lookupVarMem src
  safe1 <- safetyCond1 dst mem_src_base

  safes <- zipWithM (canBeCoalesced dst mem_dst) srcs ixfuns2
  when (safe1 && and safes) $ do
    -- Any previous src0s coalescings must be deleted.
    modify $ \c -> c { curCoalescedIntos = M.delete src $ curCoalescedIntos c }
    -- The rest will be overwritten below.

    -- We then need to record that, from what we currently know, src and any
    -- nested src0s can all use the memory of dst with the new index functions.
    forM_ (L.zip4 srcs offsets ixfunFixs ixfuns2) $ \(src_local, offset_local, ixfunFix_local, ixfun_local) -> do
      describes_exi <- S.member src_local <$> gets curExistentials
      dst_memloc <-
        if describes_exi
        then do
          -- Only change the index function.  Keep the existential memory block.
          mem_src <- shouldWork "is array" <$> lookupVarMem src
          return $ MemoryLoc (memSrcName mem_src) ixfun_local
        else
          return $ MemoryLoc (memSrcName mem_dst) ixfun_local
      recordOptimisticCoalescing src_local offset_local ixfunFix_local dst dst_memloc bindage

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "tryCoalesce:"
        putStrLn ("actual vars for " ++ pretty src ++ ": " ++ prettySet (S.fromList src's))
        putStrLn ("coalesced-into vars for " ++ pretty src ++ ": " ++ prettySet (S.fromList src0s))
        putStrLn ("all srcs for " ++ pretty src ++ ": " ++ prettySet (S.fromList srcs))
        putStrLn $ replicate 70 '~'
  withDebug debug $ return ()

recordOptimisticCoalescing :: VName -> Maybe (PrimExp VName)
                           -> Maybe (ExpMem.IxFun -> ExpMem.IxFun)
                           -> VName -> MemoryLoc -> Bindage -> FindM ()
recordOptimisticCoalescing src offset ixfunFix dst dst_memloc bindage = do
  modify $ \c -> c { curCoalescedIntos = M.alter (insertOrNew (src, offset, ixfunFix)) dst
                                         $ curCoalescedIntos c }

  -- If this is an in-place operation, we future-proof future coalescings by
  -- recording that they also need to take a look at the original array, not
  -- just the result of an in-place update into it.
  case bindage of
    BindVar -> return ()
    BindInPlace _ orig _ ->
      modify $ \c -> c { curCoalescedIntos = M.alter (insertOrNew (orig, Nothing, Nothing)) dst
                                             $ curCoalescedIntos c }

  modify $ \c -> c { curMemsCoalesced = M.insert src dst_memloc $ curMemsCoalesced c }

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "recordOptimisticCoalescing"
        putStrLn ("dst: " ++ pretty dst ++ "; src: " ++ pretty src)
        putStrLn $ replicate 70 '~'
  withDebug debug $ return ()

insertOrNew :: a -> Maybe [a] -> Maybe [a]
insertOrNew x m = Just $ case m of
  Just s -> x : s -- should nub this
  Nothing -> [x]

-- Replace variables with subtrees of their constituents wherever possible.  It
-- is debatable whether this is "simplifying", but it can enable more
-- expressions to use the index function.
simplifyIxFun :: ExpMem.IxFun -> FindM ExpMem.IxFun
simplifyIxFun ixfun = do
  var_to_pe <- gets curVarPrimExps
  let ixfun' = fixpointIterate (IxFun.substituteInIxFun var_to_pe) ixfun
  return ixfun'

canBeCoalesced :: VName -> MemorySrc -> VName -> ExpMem.IxFun -> FindM Bool
canBeCoalesced dst mem_dst src ixfun = do
  mem_src <- shouldWork "if src is an array, mem_src should also exist"
             <$> lookupVarMem src

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
        putStrLn ("safe: " ++ L.intercalate ", " (map show [safe2, safe3, safe4, safe5]))
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
  last_uses_all <- asks ctxLastUses
  let last_uses_dst = lookupEmptyable dst last_uses_all

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond1:"
        putStrLn ("dst: " ++ pretty dst)
        putStrLn ("mem src: " ++ show mem_src)
        putStrLn ("last uses dst: " ++ prettySet last_uses_dst)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return $ S.member (memSrcName mem_src) last_uses_dst

safetyCond2 :: MemorySrc -> VName -> FindM Bool
safetyCond2 mem_dst src = do
  allocs_all <- gets curAllocatedBlocksBeforeCreation
  let allocs_before_src = lookupEmptyable src allocs_all

  let debug = allocs_before_src `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond2:"
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("src: " ++ pretty src)
        putStrLn ("allocs before src: " ++ prettySet allocs_before_src)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return $ S.member (memSrcName mem_dst) allocs_before_src

safetyCond3 :: MemorySrc -> VName -> FindM Bool
safetyCond3 mem_dst src = do
  afters_vars <- gets curVarUsesAfterCreation
  let uses_after_src_vars = lookupEmptyable src afters_vars

  let getCurrentMem :: VName -> FindM Names
      getCurrentMem var = do
        -- Current result...
        mem_cur <- M.lookup var <$> gets curMemsCoalesced
        -- ... or original result.
        --
        -- This is why we save the variables after creation, not the memory
        -- blocks: Variables stay the same, but memory blocks may change, which
        -- is relevant in the case of a chain of coalescings.
        mem_orig <- lookupVarMem var
        case (mem_cur, mem_orig) of
          (Just m, _) -> withMemAliases $ memLocName m -- priority choice
          (_, Just m) -> withMemAliases $ memSrcName m
          _ -> return S.empty
  uses_after_src <- S.unions <$> mapM getCurrentMem (S.toList uses_after_src_vars)

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond3:"
        putStrLn ("mem_dst: " ++ show mem_dst)
        putStrLn ("src: " ++ pretty src)
        putStrLn ("uses after src vars: " ++ prettySet uses_after_src_vars)
        putStrLn ("uses after src: " ++ prettySet uses_after_src)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return $ not $ S.member (memSrcName mem_dst) uses_after_src

safetyCond4 :: VName -> FindM Bool
safetyCond4 src = do
  -- Aliases are not allowed unless the statement has been handled specially in
  -- lookInStm.  This is the case with reshape.  Still, this might be too broad.
  actual_vars <- gets curActualVars
  case M.lookup src actual_vars of
    Just _ -> return True
    Nothing -> do
      src_aliases <- lookupEmptyable src <$> asks ctxVarAliases
      return $ S.null src_aliases

safetyCond5 :: MemorySrc -> ExpMem.IxFun -> FindM Bool
safetyCond5 mem_src ixfun = do
  in_use_before_mems <- gets curVarsInUseBeforeMem
  let in_use_before_mem_src = lookupEmptyable (memSrcName mem_src) in_use_before_mems
      used_vars = freeIn ixfun

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "safetyCond5:"
        putStrLn ("mem_src: " ++ show mem_src)
        putStrLn ("ixfun: " ++ show ixfun)
        putStrLn ("in use before mem_src: " ++ prettySet in_use_before_mem_src)
        putStrLn ("used vars: " ++ prettySet used_vars)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return $ all (`S.member` in_use_before_mem_src) $ S.toList used_vars
