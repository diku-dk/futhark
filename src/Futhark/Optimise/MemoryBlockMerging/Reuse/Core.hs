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

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Analysis.PrimExp
import Futhark.Representation.ExplicitMemory (
  ExplicitMemory, ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.PrimExps (findPrimExpsFunDef)
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.MemoryUpdater

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes
import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeUses


data Context = Context { ctxFirstUses :: FirstUses
                         -- ^ array creation points;
                         --   maps statements to memory block names
                       , ctxInterferences :: Interferences
                         -- ^ a memory block is mapped to its interference set
                       , ctxSizes :: Sizes
                         -- ^ maps a memory block to its size and space
                       , ctxVarToMem :: VarMemMappings MemorySrc
                         -- ^ maps array names to memory blocks
                       , ctxActualVars :: M.Map VName Names
                         -- ^ maps an array name to (aliased) array names
                         --   used in cases of loops/kernels. (equivalent patterns)
                       , ctxExistentials :: Names
                         -- ^ array names mapped to an existential memory block,
                         --   an existential memory block is one appearing in the
                         --   existential context of some pattern (stmt).
                       , ctxVarPrimExps :: M.Map VName (PrimExp VName)
                         -- ^ maps a size variable to its primexp
                       , ctxSizeVarsUsesBefore :: M.Map VName Names
                         -- ^ maps a memory name to size variables available
                         --   at that memory block allocation point
                       }
  deriving (Show)

data Current = Current { curUses :: M.Map VName Names
                         -- ^ maps a memory block to the memory blocks that
                         --   were decided to be merged into it.
                       , curEqAsserts :: M.Map VName Names
                         -- ^ maps a variable name to other semantically equal
                         --   variable names

                       , curVarToMemRes :: VarMemMappings MemoryLoc
                         -- ^ The result of the core analysis:
                         --   an array name is mapped to its memory block (after merging)
                         --   (Records changes in memory blocks for variables.)

                       , curVarToMaxExpRes :: M.Map VName Names
                         -- ^ Changes in variable uses where allocation sizes are
                         --   maxed from its elements.  Keyed by statement memory
                         --   name (alloc stmt).
                         --   Maps an alloc stmt to the sizes that need to be taken max for.
                       }
  deriving (Show)

emptyCurrent :: Current
emptyCurrent = Current { curUses = M.empty
                       , curEqAsserts = M.empty
                       , curVarToMemRes = M.empty
                       , curVarToMaxExpRes = M.empty
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

lookupActualVars :: MonadReader Context m =>
                    VName -> m Names
lookupActualVars var = do
  actual_vars <- asks ctxActualVars
  -- Do this recursively.
  let actual_vars' = expandWithAliases actual_vars actual_vars
  return $ fromMaybe (S.singleton var) $ M.lookup var actual_vars'

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
                    VName -> VName -> FindM lore ()
recordMaxMapping mem y =
  modify $ \cur -> cur { curVarToMaxExpRes = insertOrUpdate mem y
                                             $ curVarToMaxExpRes cur }

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
                   FunDef ExplicitMemory
                -> FirstUses -> Interferences -> VarMemMappings MemorySrc
                -> ActualVariables -> Names -> m (FunDef ExplicitMemory, Log)
coreReuseFunDef fundef first_uses interferences var_to_mem
  actual_vars existentials = do
  let sizes = memBlockSizesFunDef fundef
      size_uses = findSizeUsesFunDef fundef
      primexps = findPrimExpsFunDef fundef
      context = Context
        { ctxFirstUses = first_uses
        , ctxInterferences = interferences
        , ctxSizes = sizes
        , ctxVarToMem = var_to_mem
        , ctxActualVars = actual_vars
        , ctxExistentials = existentials
        , ctxVarPrimExps = primexps
        , ctxSizeVarsUsesBefore = size_uses
        }
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      (res, proglog) = execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings (curVarToMemRes res) fundef
  fundef'' <- transformFromVarMaxExpMappings (curVarToMaxExpRes res) fundef'

  let all_mems = S.fromList $ map memSrcName $ M.elems var_to_mem
      mems_changed = S.fromList $ map memSrcName
                     $ mapMaybe (`M.lookup` var_to_mem) $ M.keys $ curVarToMemRes res
      mems_unchanged = S.difference all_mems mems_changed
      debug = fundef' `seq` do
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
        putStrLn $ pretty fundef''
        putStrLn $ replicate 70 '='

  withDebug debug $ return (fundef'', proglog)

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param _ membound) =
  -- Unique array function parameters also count as "allocations" in which
  -- memory can be reused.
  case membound of
    ExpMem.ArrayMem _ _ Unique mem _ ->
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
  let eqs | BasicOp (Assert (Var v) _) <- e
          , Just (CmpOpExp (CmpEq _) (LeafExp v0 _) (LeafExp v1 _)) <- M.lookup v var_to_pe = do
              modify $ \c -> c { curEqAsserts = insertOrUpdate v0 v1
                                                $ curEqAsserts c }
              modify $ \c -> c { curEqAsserts = insertOrUpdate v1 v0
                                                $ curEqAsserts c }
          | otherwise = return ()
  eqs

  forM_ patvalelems $ \(PatElem var _ membound) -> do
    -- For every declaration with a first memory use, check (through
    -- handleNewArray) if it can reuse some earlier memory block.
    first_uses_var <- lookupEmptyable var <$> asks ctxFirstUses
    actual_vars_var <- lookupActualVars var
    existentials <- asks ctxExistentials
    case membound of
      ExpMem.ArrayMem _ _ _ mem _ ->
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
                  VName -> VName -> FindM lore ()
handleNewArray x xmem = do
  interferences <- asks ctxInterferences

  let notTheSame :: Monad m => VName -> Names -> m Bool
      notTheSame kmem _used_mems = return (kmem /= xmem)

  let noneInterfere :: Monad m => VName -> Names -> m Bool
      noneInterfere _kmem used_mems =
        -- A memory block can have already been reused.  We also check for
        -- interference with any previously merged blocks.
        return $ all (\used_mem -> not $ S.member xmem
                                   $ lookupEmptyable used_mem interferences)
        $ S.toList used_mems

  let sameSpace :: MonadReader Context m =>
                   VName -> Names -> m Bool
      sameSpace kmem _used_mems = do
        kspace <- lookupSpace kmem
        xspace <- lookupSpace xmem
        return (kspace == xspace)

  -- Is the size of the new memory block (xmem) equal to any of the memory
  -- blocks (used_mems) using an already used memory block?
  let sizesMatch :: LoreConstraints lore =>
                    Names -> FindM lore Bool
      sizesMatch used_mems = do
        ok_sizes <- mapM lookupSize $ S.toList used_mems
        new_size <- lookupSize xmem
        -- Check for size equality by checking for variable name equality.
        let eq_simple = new_size `L.elem` ok_sizes

        -- Check for size equality by constructing 'PrimExp's and comparing
        -- those.  Use the custom VarWithAssertTracking type to compare inner
        -- sizes: If an equality assert statement was found earlier, consider
        -- its two operands to be the same.
        var_to_pe <- asks ctxVarPrimExps
        eq_asserts <- gets curEqAsserts
        let sePrimExp se = do
              v <- fromVar se
              pe <- M.lookup v var_to_pe
              let pe_expanded = expandPrimExp var_to_pe pe
              traverse (\v_inner -> -- Has custom Eq instance.
                                       pure $ VarWithAssertTracking v_inner
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
                         VName -> FindM lore Bool
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

  let sizesWorkOut :: LoreConstraints lore =>
                      VName -> Names -> FindM lore Bool
      sizesWorkOut kmem used_mems =
        -- The size of an allocation is okay to reuse if it is the same as the
        -- current memory size, or if it can be changed to be the maximum size
        -- of the two sizes.
        sizesMatch used_mems <||> sizesCanBeMaxed kmem

  let canBeUsed t = and <$> mapM (($ t) . uncurry)
                    [notTheSame, noneInterfere, sameSpace, sizesWorkOut]
  cur_uses <- gets curUses
  found_use <- catMaybes <$> mapM (maybeFromBoolM canBeUsed) (M.assocs cur_uses)

  -- writeLog x "previously used"
  --   (prettyList $ map (\(k, kuses) -> pretty k ++ " (used by: " ++ prettySet kuses ++ ")")
  --    $ M.assocs cur_uses)
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
               (pretty t ++ " (failed on: " ++ L.intercalate ", " ws ++ ")")) not_found_use [(1::Int)..]
  -- writeLog x "interferences" (prettySet $ lookupEmptyable xmem interferences)

  actual_vars <- lookupActualVars x
  case found_use of
    (kmem, _) : _ -> do
      -- There is a previous memory block that we can use.  Record the mapping.
      insertUse kmem xmem
      forM_ actual_vars $ \var -> do
        ixfun <- memSrcIxFun <$> lookupVarMem var
        recordMemMapping var $ MemoryLoc kmem ixfun

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

data VarWithAssertTracking = VarWithAssertTracking VName Names
  deriving (Show)

instance Eq VarWithAssertTracking where
  VarWithAssertTracking v0 vs0 == VarWithAssertTracking v1 vs1 =
    not $ S.null $ S.intersection (S.insert v0 vs0) (S.insert v1 vs1)

-- Replace certain allocation sizes in a program with new variables describing
-- the maximum of two or more allocation sizes.  This enables more reuse.
transformFromVarMaxExpMappings :: MonadFreshNames m =>
                                  M.Map VName Names
                               -> FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFromVarMaxExpMappings var_to_max fundef = do
  var_to_new_var <- M.fromList <$> mapM (\(k, v) -> (k,) <$> maxsToReplacement (S.toList v))
                    (M.assocs var_to_max)
  return $ insertAndReplace var_to_new_var fundef

-- A replacement is a new size variable and any new subexpressions that the new
-- variable depends on.
data Replacement = Replacement
  { replName :: VName -- The new variable
  , replExps :: [(VName, Exp ExplicitMemory)] -- The new expressions
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
  return $ Replacement vmax (es0 ++ es1 ++ [(vmax, emax)])

-- Modify a function to use the new replacements.
insertAndReplace :: M.Map VName Replacement -> FunDef ExplicitMemory -> FunDef ExplicitMemory
insertAndReplace replaces0 fundef =
  let body' = evalState (transformBody $ funDefBody fundef) replaces0
  in fundef { funDefBody = body' }

  where transformBody :: Body ExplicitMemory ->
                         State (M.Map VName Replacement) (Body ExplicitMemory)
        transformBody body = do
          stms' <- concat <$> mapM transformStm (bodyStms body)
          return $ body { bodyStms = stms' }

        transformStm :: Stm ExplicitMemory ->
                        State (M.Map VName Replacement) [Stm ExplicitMemory]
        transformStm stm@(Let (Pattern [] [PatElem mem_name BindVar
                                           (ExpMem.MemMem _ pat_space)]) ()
                          (Op (ExpMem.Alloc _ space))) = do
          replaces <- get
          case M.lookup mem_name replaces of
            Just repl -> do
              let prev = map (\(name, e) ->
                                Let (Pattern [] [PatElem name BindVar
                                                 (ExpMem.Scalar (IntType Int64))]) () e)
                         (replExps repl)
                  new = Let (Pattern [] [PatElem mem_name BindVar
                                         (ExpMem.MemMem (Var (replName repl)) pat_space)]) ()
                        (Op (ExpMem.Alloc (Var (replName repl)) space))
              -- We should only generate the new statements once.
              modify $ M.adjust (\repl0 -> repl0 { replExps = [] }) mem_name
              return (prev ++ [new])
            Nothing -> return [stm]
        transformStm (Let pat attr e) = do
          let mapper = identityMapper { mapOnBody = const transformBody }
          e' <- mapExpM mapper e
          return [Let pat attr e']
