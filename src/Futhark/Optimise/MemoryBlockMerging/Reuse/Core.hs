{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
-- | Traverse a body to find memory blocks that can be allocated together.
module Futhark.Optimise.MemoryBlockMerging.Reuse.Core
  ( coreReuseFunDef
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Analysis.PrimExp
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.PrimExps (findPrimExpsFunDef)
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.MemoryUpdater

import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes
import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeUses


data Context = Context { ctxFirstUses :: FirstUses
                       , ctxInterferences :: Interferences
                       , ctxSizes :: Sizes
                       , ctxVarToMem :: VarMemMappings MemorySrc
                       , ctxActualVars :: M.Map VName Names
                       , ctxExistentials :: Names
                       , ctxVarPrimExps :: M.Map VName (PrimExp VName)
                       , ctxSizeVarsUsesBefore :: M.Map VName Names
                       , ctxCurLoopBodyRes :: Result
                       }
  deriving (Show)

data Current = Current { curUses :: M.Map VName Names
                       , curEqAsserts :: M.Map VName Names

                         -- Changes in memory blocks for variables.
                       , curVarToMemRes :: VarMemMappings MemoryLoc

                         -- Changes in variable uses where allocation sizes are
                         -- maxed from its elements.
                       , curVarToMaxExpRes :: M.Map VName Names
                       }
  deriving (Show)

emptyCurrent :: Current
emptyCurrent = Current { curUses = M.empty
                       , curEqAsserts = M.empty
                       , curVarToMemRes = M.empty
                       , curVarToMaxExpRes = M.empty
                       }

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
lookupActualVars var = do
  actual_vars <- asks ctxActualVars
  -- Do this recursively.
  let actual_vars' = expandWithAliases actual_vars actual_vars
  return $ fromMaybe (S.singleton var) $ M.lookup var actual_vars'

lookupSize :: VName -> FindM SubExp
lookupSize var =
  (fromJust ("lookup size from " ++ pretty var) . M.lookup var)
  <$> asks ctxSizes

insertUse :: VName -> VName -> FindM ()
insertUse new_mem old_mem =
  modify $ \cur -> cur { curUses = insertOrUpdate new_mem old_mem $ curUses cur }

recordMemMapping :: VName -> MemoryLoc -> FindM ()
recordMemMapping x mem =
  modify $ \cur -> cur { curVarToMemRes = M.insert x mem $ curVarToMemRes cur }

recordMaxMapping :: VName -> VName -> FindM ()
recordMaxMapping x y =
  modify $ \cur -> cur { curVarToMaxExpRes = insertOrUpdate x y
                                             $ curVarToMaxExpRes cur }

withLocalUses :: FindM a -> FindM a
withLocalUses m = do
  uses <- gets curUses
  res <- m
  modify $ \cur -> cur { curUses = uses }
  return res

coreReuseFunDef :: MonadFreshNames m =>
                   FunDef ExplicitMemory
                -> FirstUses -> Interferences -> VarMemMappings MemorySrc
                -> ActualVariables -> Names -> m (FunDef ExplicitMemory)
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
        , ctxCurLoopBodyRes = []
        }
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      res = fst $ execRWS m context emptyCurrent
      fundef' = transformFromVarMemMappings (curVarToMemRes res) fundef
  fundef'' <- transformFromVarMaxExpMappings (curVarToMaxExpRes res) fundef'

  let debug = fundef' `seq` do
        putStrLn $ replicate 70 '='
        putStrLn "coreReuseFunDef reuse results:"
        forM_ (M.assocs (curVarToMemRes res)) $ \(src, dstmem) ->
          putStrLn ("Source " ++ pretty src ++ " reuses "
                    ++ pretty (memLocName dstmem) ++ "; ixfun: "
                    ++ show (memLocIxFun dstmem))
        putStrLn ""
        forM_ (M.assocs (curVarToMaxExpRes res)) $ \(src, maxs) ->
          putStrLn ("Size variable " ++ pretty src ++ " is now maximum of "
                    ++ prettySet maxs)
        putStrLn $ pretty fundef''
        putStrLn $ replicate 70 '='

  withDebug debug $ return fundef''

lookInFParam :: FParam ExplicitMemory -> FindM ()
lookInFParam (Param _ membound) =
  -- Unique array function parameters also count as "allocations" in which
  -- memory can be reused.
  case membound of
    ExpMem.ArrayMem _ _ Unique mem _ ->
      insertUse mem mem
    _ -> return ()

lookInBody :: Body ExplicitMemory
           -> FindM ()
lookInBody (Body () bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern _patctxelems patvalelems) () e) = do
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
    first_uses <- lookupEmptyable var <$> asks ctxFirstUses
    forM_ first_uses $ handleNewArray var membound

  let mMod = case e of
        DoLoop _ _ _ loopbody ->
          local (\ctx -> ctx { ctxCurLoopBodyRes = bodyResult loopbody })
        _ -> id
  withLocalUses $ mMod $ walkExpM walker e

  let debug = do
        putStrLn $ replicate 70 '~'
        putStrLn "Statement."
        print patvalelems
        print e
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

  where walker = identityWalker { walkOnBody = lookInBody }

handleNewArray :: VName -> ExpMem.MemBound u -> VName -> FindM ()
handleNewArray x (ExpMem.ArrayMem _ _ _ _ _xixfun) xmem = do
  interferences <- asks ctxInterferences

  -- If the statement is inside a loop body, and the loop result contains this
  -- array, abort!  That is a sign of trouble.
  --
  -- This can be described in more general terms with interference, but the
  -- existing interference analysis is not good enough, since it has to be rerun
  -- in the case of loop memory block mergings.  We *can* do that, but it might
  -- get messy.
  cur_loop_body_res <- asks ctxCurLoopBodyRes
  let loop_disabled = Var x `L.elem` cur_loop_body_res

  let notTheSame :: VName -> Names -> FindM Bool
      notTheSame kmem _used_mems = return (kmem /= xmem)

  let noneInterfere :: VName -> Names -> FindM Bool
      noneInterfere _kmem used_mems =
        -- A memory block can have already been reused.  For safety's sake, we
        -- also check for interference with any previously merged blocks.  Might
        -- not be necessary?
        return $ all (\used_mem -> not $ S.member xmem
                                   $ lookupEmptyable used_mem interferences)
        $ S.toList used_mems

  let sizesMatch :: Names -> FindM Bool
      sizesMatch used_mems = do
        ok_sizes <- mapM lookupSize $ S.toList used_mems
        new_size <- lookupSize xmem
        let eq_simple = new_size `L.elem` ok_sizes

        var_to_pe <- asks ctxVarPrimExps
        eq_asserts <- gets curEqAsserts
        let sePrimExp se = do
              v <- fromVar se
              pe <- M.lookup v var_to_pe
              let pe_expanded = expandPrimExp var_to_pe pe
              traverse (\v_inner -> -- Has custom Eq instance.
                                       pure $ VarWithAssertTracking v_inner
                                       $ lookupEmptyable v eq_asserts
                       ) pe_expanded
        let ok_sizes_pe = map sePrimExp ok_sizes
        let new_size_pe = sePrimExp new_size

        let eq_advanced = new_size_pe `L.elem` ok_sizes_pe

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
              putStrLn $ replicate 70 '~'

        withDebug debug $ return (eq_simple || eq_advanced)

  let sizesCanBeMaxed :: VName -> FindM Bool
      sizesCanBeMaxed kmem = do
        ksize <- lookupSize kmem
        xsize <- lookupSize xmem
        uses_before <- asks ctxSizeVarsUsesBefore
        return $ fromMaybe False $ do
          ksize' <- fromVar ksize
          xsize' <- fromVar xsize
          return (xsize' `S.member` fromJust ("is recorded for all size variables "
                                              ++ pretty ksize')
                  (M.lookup ksize' uses_before))

  let sizesWorkOut :: VName -> Names -> FindM Bool
      sizesWorkOut kmem used_mems =
        -- The size of an allocation is okay to reuse if it is the same as the
        -- current memory size, or if it can be changed to be the maximum size
        -- of the two sizes.
        sizesMatch used_mems <||> sizesCanBeMaxed kmem

  let canBeUsed t = and <$> mapM (($ t) . uncurry) [notTheSame, noneInterfere, sizesWorkOut]
  cur_uses <- gets curUses
  found_use <- catMaybes <$> mapM (maybeFromBoolM canBeUsed) (M.assocs cur_uses)

  actual_vars <- lookupActualVars x
  existentials <- asks ctxExistentials
  let base_error = loop_disabled
                   || S.null actual_vars
                   || any (`S.member` existentials) actual_vars
  case (base_error, found_use) of
    (False, (kmem, _) : _) -> do
      -- There is a previous memory block that we can use.  Record the mapping.
      insertUse kmem xmem
      forM_ actual_vars $ \var -> do
        ixfun <- memSrcIxFun <$> lookupVarMem var
        recordMemMapping var $ MemoryLoc kmem ixfun
      whenM (sizesCanBeMaxed kmem) $ do
        ksize <- lookupSize kmem
        xsize <- lookupSize xmem
        fromMaybe (return ()) $ do
          ksize' <- fromVar ksize
          xsize' <- fromVar xsize
          return $ do
            recordMaxMapping ksize' ksize'
            recordMaxMapping ksize' xsize'
    _ ->
      -- There is no previous memory block available for use.  Record that this
      -- memory block is available.
      unless (S.member x existentials) $ insertUse xmem xmem

  let debug = found_use `seq` do
        putStrLn $ replicate 70 '~'
        putStrLn "Handle new array."
        putStrLn ("var: " ++ pretty x)
        putStrLn ("actual vars: " ++ prettySet actual_vars)
        putStrLn ("mem: " ++ pretty xmem)
        putStrLn ("loop disabled: " ++ show loop_disabled)
        putStrLn ("cur uses: " ++ show cur_uses)
        putStrLn ("found use: " ++ show found_use)
        putStrLn $ replicate 70 '~'

  withDebug debug $ return ()

handleNewArray _ _ _ = return ()


-- Replace certain allocation sizes in a program with new variables describing
-- the maximum of two or more allocation sizes.  This enables more reuse.
transformFromVarMaxExpMappings :: MonadFreshNames m =>
                                  M.Map VName Names
                               -> FunDef ExplicitMemory -> m (FunDef ExplicitMemory)
transformFromVarMaxExpMappings var_to_max fundef = do
  var_to_new_var <- M.fromList <$> mapM (\(k, v) -> (k,) <$> maxsToReplacement (S.toList v))
                    (M.assocs var_to_max)
  return $ insertAndReplace var_to_new_var fundef

data Replacement = Replacement
  { replName :: VName -- The main variable
  , replExps :: [(VName, Exp ExplicitMemory)] -- The new expressions
  }
  deriving (Show)

maxsToReplacement :: MonadFreshNames m =>
                     [VName] -> m Replacement
maxsToReplacement [] = error "maxsToReplacements: Cannot take max of zero variables"
maxsToReplacement [v] = return $ Replacement v []
maxsToReplacement vs = do
  let (vs0, vs1) = splitAt (length vs `div` 2) vs
  Replacement m0 es0 <- maxsToReplacement vs0
  Replacement m1 es1 <- maxsToReplacement vs1
  vmax <- newVName "max"
  let emax = BasicOp $ BinOp (SMax Int64) (Var m0) (Var m1)
  return $ Replacement vmax (es0 ++ es1 ++ [(vmax, emax)])

insertAndReplace :: M.Map VName Replacement -> FunDef ExplicitMemory
                 -> FunDef ExplicitMemory
insertAndReplace replaces0 fundef =
  let body' = evalState (transformBody $ funDefBody fundef) replaces0
  in fundef { funDefBody = body' }

  where transformBody :: Body ExplicitMemory
                      -> State (M.Map VName Replacement) (Body ExplicitMemory)
        transformBody body = do
          stms' <- concat <$> mapM transformStm (bodyStms body)
          return $ body { bodyStms = stms' }

        transformStm :: Stm ExplicitMemory
                     -> State (M.Map VName Replacement) [Stm ExplicitMemory]
        transformStm stm@(Let (Pattern [] [PatElem pat_name BindVar
                                           (ExpMem.MemMem _ pat_space)]) ()
                          (Op (ExpMem.Alloc (Var size) space))) = do
          replaces <- get
          case M.lookup size replaces of
            Just repl -> do
              let prev = map (\(name, e) ->
                                Let (Pattern [] [PatElem name BindVar
                                                 (ExpMem.Scalar (IntType Int64))]) () e)
                         (replExps repl)
                  new = Let (Pattern [] [PatElem pat_name BindVar
                                         (ExpMem.MemMem (Var (replName repl)) pat_space)]) ()
                        (Op (ExpMem.Alloc (Var (replName repl)) space))
              -- We should only generate the new statements once.
              modify $ M.adjust (\repl0 -> repl0 { replExps = [] }) size
              return (prev ++ [new])
            Nothing -> return [stm]
        transformStm (Let pat attr e) = do
          let mapper = identityMapper { mapOnBody = const transformBody }
          e' <- mapExpM mapper e
          return [Let pat attr e']

data VarWithAssertTracking = VarWithAssertTracking VName Names
  deriving (Show)

instance Eq VarWithAssertTracking where
  VarWithAssertTracking v0 vs0 == VarWithAssertTracking v1 vs1 =
    not $ S.null $ S.intersection (S.insert v0 vs0) (S.insert v1 vs1)
