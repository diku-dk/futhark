{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Futhark.Representation.ExplicitMemory.Simplify
       ( simplifyExplicitMemory
       )
where

import Control.Monad
import qualified Data.Set as S
import Data.Monoid
import Data.Maybe
import Data.List

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, BasicOp, Exp, Body, Stm,
          Pattern, PatElem, Lambda, ExtLambda, FunDef, FParam, LParam,
          RetType)
import Futhark.Representation.ExplicitMemory
import Futhark.Representation.Kernels.Simplify
  (simplifyKernelOp, simplifyKernelExp)
import Futhark.Pass.ExplicitAllocations
  (simplifiable, arraySizeInBytesExp)
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Construct
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Lore
import Futhark.Util

simpleExplicitMemory :: Simplifier.SimpleOps ExplicitMemory
simpleExplicitMemory = simplifiable (simplifyKernelOp simpleInKernel inKernelEnv)

simpleInKernel :: Simplifier.SimpleOps InKernel
simpleInKernel = simplifiable simplifyKernelExp

simplifyExplicitMemory :: MonadFreshNames m => Prog ExplicitMemory -> m (Prog ExplicitMemory)
simplifyExplicitMemory =
  Simplifier.simplifyProgWithRules simpleExplicitMemory callKernelRules blockers

isAlloc :: Op lore ~ MemOp op => Engine.BlockPred lore
isAlloc _ (Let _ _ (Op Alloc{})) = True
isAlloc _ _                      = False

isResultAlloc :: Op lore ~ MemOp op => Engine.BlockPred lore
isResultAlloc usage (Let (AST.Pattern [] [bindee]) _ (Op Alloc{})) =
  UT.isInResult (patElemName bindee) usage
isResultAlloc _ _ = False

-- | Getting the roots of what to hoist, for now only variable
-- names that represent array and memory-block sizes.
getShapeNames :: ExplicitMemorish lore =>
                 Stm (Wise lore) -> S.Set VName
getShapeNames bnd =
  let tps = map patElemType $ patternElements $ stmPattern bnd
      ats = map (snd . patElemAttr) $ patternElements $ stmPattern bnd
      nms = mapMaybe (\case
                         MemMem (Var nm) _ -> Just nm
                         MemArray _ _ _ (ArrayIn nm _) -> Just nm
                         _ -> Nothing
                     ) ats
  in  S.fromList $ nms ++ subExpVars (concatMap arrayDims tps)

isAlloc0 :: Op lore ~ MemOp op => AST.Stm lore -> Bool
isAlloc0 (Let _ _ (Op Alloc{})) = True
isAlloc0 _                      = False

inKernelEnv :: Engine.Env InKernel
inKernelEnv = Engine.emptyEnv inKernelRules blockers

blockers ::  (ExplicitMemorish lore, Op lore ~ MemOp op) =>
             Simplifier.HoistBlockers lore
blockers = Engine.HoistBlockers {
    Engine.blockHoistPar = isAlloc
  , Engine.blockHoistSeq = isResultAlloc
  , Engine.getArraySizes = getShapeNames
  , Engine.isAllocation  = isAlloc0
  }


callKernelRules :: RuleBook (Wise ExplicitMemory)
callKernelRules = standardRules <>
                  ruleBook [RuleBasicOp copyCopyToCopy] []

inKernelRules :: RuleBook (Wise InKernel)
inKernelRules = standardRules <>
                ruleBook [RuleBasicOp copyCopyToCopy, RuleIf unExistentialiseMemory] []

-- | If a branch is returning some existential memory, but the size of
-- the array is existential, then we can create a block of the proper
-- size and always return there.
unExistentialiseMemory :: TopDownRuleIf (Wise InKernel)
unExistentialiseMemory _ pat _ (cond, tbranch, fbranch, ifattr)
  | fixable <- foldl hasConcretisableMemory mempty $ patternElements pat,
    not $ null fixable = do

      -- Create non-existential memory blocks big enough to hold the
      -- arrays.
      (arr_to_mem, oldmem_to_mem, oldsize_to_size) <-
        fmap unzip3 $ forM fixable $ \(arr_pe, oldmem, oldsize, space) -> do
          size <- letSubExp "size" =<<
                  toExp (arraySizeInBytesExp $ patElemType arr_pe)
          mem <- letExp "mem" $ Op $ Alloc size space
          return ((patElemName arr_pe, mem), (oldmem, mem), (oldsize, size))

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
                let v_pat = Pattern [] [PatElem v_copy BindVar $
                                        MemArray pt shape u $ ArrayIn mem ixfun]
                addStm $ mkWiseLetStm v_pat (defAux ()) $ BasicOp (Copy v)
                return $ Var v_copy
            | Just mem <- lookup (patElemName pat_elem) oldmem_to_mem =
                return $ Var mem
            | Just size <- lookup (Var (patElemName pat_elem)) oldsize_to_size =
                return size
          updateResult _ se =
            return se
      tbranch' <- updateBody tbranch
      fbranch' <- updateBody fbranch
      letBind_ pat $ If cond tbranch' fbranch' ifattr
  where onlyUsedIn name here = not $ any ((name `S.member`) . freeIn) $
                                          filter ((/=here) . patElemName) $
                                          patternValueElements pat
        knownSize Constant{} = True
        knownSize (Var v) = not $ inContext v
        inContext = (`elem` patternContextNames pat)

        hasConcretisableMemory fixable pat_elem
          | (_, MemArray _ shape _ (ArrayIn mem _)) <- patElemAttr pat_elem,
            Just (j, Mem old_size space) <-
              fmap patElemType <$> find ((mem==) . patElemName . snd)
                                        (zip [(0::Int)..] $ patternElements pat),
            Just tse <- maybeNth j $ bodyResult tbranch,
            Just fse <- maybeNth j $ bodyResult fbranch,
            mem `onlyUsedIn` patElemName pat_elem,
            all knownSize (shapeDims shape),
            fse /= tse =
              (pat_elem, mem, old_size, space) : fixable
          | otherwise =
              fixable
unExistentialiseMemory _ _ _ _ = cannotSimplify

-- | If we are copying something that is itself a copy, just copy the
-- original one instead.
copyCopyToCopy :: (BinderOps lore,
                   LetAttr lore ~ (VarWisdom, MemBound u)) =>
                  TopDownRuleBasicOp lore
copyCopyToCopy vtable (pat@(Pattern [] [pat_elem])) _ (Copy v1)
  | Just (BasicOp (Copy v2), v1_cs) <- ST.lookupExp v1 vtable,

    Just (_, MemArray _ _ _ (ArrayIn srcmem src_ixfun)) <-
      ST.entryLetBoundAttr =<< ST.lookup v1 vtable,

    Just (Mem _ src_space) <- ST.lookupType srcmem vtable,

    (_, MemArray _ _ _ (ArrayIn destmem dest_ixfun)) <- patElemAttr pat_elem,

    Just (Mem _ dest_space) <- ST.lookupType destmem vtable,

    src_space == dest_space, dest_ixfun == src_ixfun =

      certifying v1_cs $ letBind_ pat $ BasicOp $ Copy v2

copyCopyToCopy vtable pat _ (Copy v0)
  | Just (BasicOp (Rearrange perm v1), v0_cs) <- ST.lookupExp v0 vtable,
    Just (BasicOp (Copy v2), v1_cs) <- ST.lookupExp v1 vtable = do
      v0' <- certifying (v0_cs<>v1_cs) $
             letExp "rearrange_v0" $ BasicOp $ Rearrange perm v2
      letBind_ pat $ BasicOp $ Copy v0'

copyCopyToCopy _ _ _ _ = cannotSimplify
