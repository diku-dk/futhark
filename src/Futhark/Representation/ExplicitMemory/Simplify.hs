{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.ExplicitMemory.Simplify
       ( simplifyExplicitMemory
       , simplifyStms
       )
where

import Control.Monad
import Data.List (find)

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, BasicOp, Exp, Body, Stm,
          Pattern, PatElem, Lambda, FunDef, FParam, LParam, RetType)
import Futhark.Representation.ExplicitMemory
import Futhark.Representation.Kernels.Simplify (simplifyKernelOp)
import Futhark.Pass.ExplicitAllocations
  (simplifiable, arraySizeInBytesExp)
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Construct
import Futhark.Pass
import Futhark.Optimise.Simplify.Rules
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Lore
import Futhark.Util

simpleExplicitMemory :: Simplify.SimpleOps ExplicitMemory
simpleExplicitMemory = simplifiable $ simplifyKernelOp $ const $ return ((), mempty)

simplifyExplicitMemory :: Prog ExplicitMemory -> PassM (Prog ExplicitMemory)
simplifyExplicitMemory =
  Simplify.simplifyProg simpleExplicitMemory callKernelRules
  blockers { Engine.blockHoistBranch = blockAllocs }
  where blockAllocs vtable _ (Let _ _ (Op Alloc{})) =
          not $ ST.simplifyMemory vtable
        blockAllocs _ _ _ = False

simplifyStms :: (HasScope ExplicitMemory m, MonadFreshNames m) =>
                Stms ExplicitMemory -> m (Stms ExplicitMemory)
simplifyStms =
  Simplify.simplifyStms simpleExplicitMemory callKernelRules blockers

isResultAlloc :: Op lore ~ MemOp op => Engine.BlockPred lore
isResultAlloc _ usage (Let (AST.Pattern [] [bindee]) _ (Op Alloc{})) =
  UT.isInResult (patElemName bindee) usage
isResultAlloc _ _ _ = False

-- | Getting the roots of what to hoist, for now only variable
-- names that represent array and memory-block sizes.
getShapeNames :: (ExplicitMemorish lore, Op lore ~ MemOp op) =>
                 Stm (Wise lore) -> Names
getShapeNames stm =
  let ts = map patElemType $ patternElements $ stmPattern stm
  in freeIn (concatMap arrayDims ts) <>
     case stmExp stm of Op (Alloc size _) -> freeIn size
                        _                 -> mempty

isAlloc :: Op lore ~ MemOp op => Engine.BlockPred lore
isAlloc _ _ (Let _ _ (Op Alloc{})) = True
isAlloc _ _ _                      = False

blockers :: Simplify.HoistBlockers ExplicitMemory
blockers = Engine.noExtraHoistBlockers {
    Engine.blockHoistPar    = isAlloc
  , Engine.blockHoistSeq    = isResultAlloc
  , Engine.getArraySizes    = getShapeNames
  , Engine.isAllocation     = isAlloc mempty mempty
  }

callKernelRules :: RuleBook (Wise ExplicitMemory)
callKernelRules = standardRules <>
                  ruleBook [RuleBasicOp copyCopyToCopy,
                            RuleBasicOp removeIdentityCopy,
                            RuleIf unExistentialiseMemory] []

-- | If a branch is returning some existential memory, but the size of
-- the array is not existential, then we can create a block of the
-- proper size and always return there.
unExistentialiseMemory :: TopDownRuleIf (Wise ExplicitMemory)
unExistentialiseMemory vtable pat _ (cond, tbranch, fbranch, ifattr)
  | ST.simplifyMemory vtable,
    fixable <- foldl hasConcretisableMemory mempty $ patternElements pat,
    not $ null fixable = Simplify $ do

      -- Create non-existential memory blocks big enough to hold the
      -- arrays.
      (arr_to_mem, oldmem_to_mem) <-
        fmap unzip $ forM fixable $ \(arr_pe, oldmem, space) -> do
          size <- letSubExp "size" =<<
                  toExp (arraySizeInBytesExp $ patElemType arr_pe)
          mem <- letExp "mem" $ Op $ Alloc size space
          return ((patElemName arr_pe, mem), (oldmem, mem))

      -- Update the branches to contain Copy expressions putting the
      -- arrays where they are expected.
      let updateBody body = insertStmsM $ do
            res <- bodyBind body
            resultBodyM =<<
              zipWithM updateResult (patternElements pat) res
          updateResult pat_elem (Var v)
            | Just mem <- lookup (patElemName pat_elem) arr_to_mem,
              (_, MemArray pt shape u (ArrayIn _ ixfun)) <- patElemAttr pat_elem = do
                v_copy <- newVName $ baseString v <> "_nonext_copy"
                let v_pat = Pattern [] [PatElem v_copy $
                                        MemArray pt shape u $ ArrayIn mem ixfun]
                addStm $ mkWiseLetStm v_pat (defAux ()) $ BasicOp (Copy v)
                return $ Var v_copy
            | Just mem <- lookup (patElemName pat_elem) oldmem_to_mem =
                return $ Var mem
          updateResult _ se =
            return se
      tbranch' <- updateBody tbranch
      fbranch' <- updateBody fbranch
      letBind_ pat $ If cond tbranch' fbranch' ifattr
  where onlyUsedIn name here = not $ any ((name `nameIn`) . freeIn) $
                                          filter ((/=here) . patElemName) $
                                          patternValueElements pat
        knownSize Constant{} = True
        knownSize (Var v) = not $ inContext v
        inContext = (`elem` patternContextNames pat)

        hasConcretisableMemory fixable pat_elem
          | (_, MemArray _ shape _ (ArrayIn mem _)) <- patElemAttr pat_elem,
            Just (j, Mem space) <-
              fmap patElemType <$> find ((mem==) . patElemName . snd)
                                        (zip [(0::Int)..] $ patternElements pat),
            Just tse <- maybeNth j $ bodyResult tbranch,
            Just fse <- maybeNth j $ bodyResult fbranch,
            mem `onlyUsedIn` patElemName pat_elem,
            all knownSize (shapeDims shape),
            fse /= tse =
              (pat_elem, mem, space) : fixable
          | otherwise =
              fixable
unExistentialiseMemory _ _ _ _ = Skip

-- | If we are copying something that is itself a copy, just copy the
-- original one instead.
copyCopyToCopy :: (BinderOps lore,
                   LetAttr lore ~ (VarWisdom, MemBound u)) =>
                  TopDownRuleBasicOp lore
copyCopyToCopy vtable pat@(Pattern [] [pat_elem]) _ (Copy v1)
  | Just (BasicOp (Copy v2), v1_cs) <- ST.lookupExp v1 vtable,

    Just (_, MemArray _ _ _ (ArrayIn srcmem src_ixfun)) <-
      ST.entryLetBoundAttr =<< ST.lookup v1 vtable,

    Just (Mem src_space) <- ST.lookupType srcmem vtable,

    (_, MemArray _ _ _ (ArrayIn destmem dest_ixfun)) <- patElemAttr pat_elem,

    Just (Mem dest_space) <- ST.lookupType destmem vtable,

    src_space == dest_space, dest_ixfun == src_ixfun =

      Simplify $ certifying v1_cs $ letBind_ pat $ BasicOp $ Copy v2

copyCopyToCopy vtable pat _ (Copy v0)
  | Just (BasicOp (Rearrange perm v1), v0_cs) <- ST.lookupExp v0 vtable,
    Just (BasicOp (Copy v2), v1_cs) <- ST.lookupExp v1 vtable = Simplify $ do
      v0' <- certifying (v0_cs<>v1_cs) $
             letExp "rearrange_v0" $ BasicOp $ Rearrange perm v2
      letBind_ pat $ BasicOp $ Copy v0'

copyCopyToCopy _ _ _ _ = Skip

-- | If the destination of a copy is the same as the source, just
-- remove it.
removeIdentityCopy :: (BinderOps lore,
                       LetAttr lore ~ (VarWisdom, MemBound u)) =>
                      TopDownRuleBasicOp lore
removeIdentityCopy vtable pat@(Pattern [] [pe]) _ (Copy v)
  | (_, MemArray _ _ _ (ArrayIn dest_mem dest_ixfun)) <- patElemAttr pe,
    Just (_, MemArray _ _ _ (ArrayIn src_mem src_ixfun)) <-
      ST.entryLetBoundAttr =<< ST.lookup v vtable,
    dest_mem == src_mem, dest_ixfun == src_ixfun =
      Simplify $ letBind_ pat $ BasicOp $ SubExp $ Var v

removeIdentityCopy _ _ _ _ = Skip
