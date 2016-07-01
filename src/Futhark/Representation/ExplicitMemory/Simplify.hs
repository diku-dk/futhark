{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.ExplicitMemory.Simplify
       ( simplifyExplicitMemory
       )
where

import Control.Monad
import qualified Data.HashSet as HS
import Data.Monoid
import Data.Maybe

import Prelude

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, BasicOp, Exp, Body, Binding,
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
import Futhark.Optimise.Simplifier.RuleM
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Lore
import Futhark.MonadFreshNames

simpleExplicitMemory :: Simplifier.SimpleOps ExplicitMemory
simpleExplicitMemory = simplifiable (simplifyKernelOp simpleInKernel inKernelEnv)

simpleInKernel :: Simplifier.SimpleOps InKernel
simpleInKernel = simplifiable simplifyKernelExp

simplifyExplicitMemory :: MonadFreshNames m => Prog ExplicitMemory -> m (Prog ExplicitMemory)
simplifyExplicitMemory =
  Simplifier.simplifyProgWithRules simpleExplicitMemory explicitMemoryRules blockers

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
                 Binding (Wise lore) -> HS.HashSet VName
getShapeNames bnd =
  let tps = map patElemType $ patternElements $ bindingPattern bnd
      ats = map (snd . patElemAttr) $ patternElements $ bindingPattern bnd
      nms = mapMaybe (\attr -> case attr of
                                 MemMem (Var nm) _   -> Just nm
                                 ArrayMem _ _ _ nm _ -> Just nm
                                 _                   -> Nothing
                     ) ats
  in  HS.fromList $ nms ++ subExpVars (concatMap arrayDims tps)

isAlloc0 :: Op lore ~ MemOp op => AST.Binding lore -> Bool
isAlloc0 (Let _ _ (Op Alloc{})) = True
isAlloc0 _                      = False

inKernelEnv :: Engine.Env (Engine.SimpleM InKernel)
inKernelEnv = Engine.emptyEnv explicitMemoryRules blockers

blockers ::  (ExplicitMemorish lore, Op lore ~ MemOp op) =>
             Simplifier.HoistBlockers (Engine.SimpleM lore)
blockers = Engine.HoistBlockers {
    Engine.blockHoistPar = isAlloc
  , Engine.blockHoistSeq = isResultAlloc
  , Engine.getArraySizes = getShapeNames
  , Engine.isAllocation  = isAlloc0
  }


explicitMemoryRules :: (MonadBinder m,
                        Op (Lore m) ~ MemOp inner,
                        LetAttr (Lore m) ~ (VarWisdom, MemBound u),
                        Lore m ~ Wise lore,
                        LocalScope (Wise lore) m,
                        ExplicitMemorish lore) => RuleBook m
explicitMemoryRules = (std_td_rules <> [ unExistentialiseMemory
                                       , copyCopyToCopy
                                       ],
                       std_bu_rules <> [])
  where (std_td_rules, std_bu_rules) = standardRules

-- | If a branch is returning some existential memory, but we know the
-- size of the corresponding array non-existentially, then we can
-- create a block of the proper size and always return there.
unExistentialiseMemory :: (MonadBinder m,
                           Op (Lore m) ~ MemOp inner,
                           LetAttr (Lore m) ~ (VarWisdom, MemBound u)) =>
                          TopDownRule m
unExistentialiseMemory _ (Let pat _ (If cond tbranch fbranch ret))
  | (remaining_ctx, concretised) <-
      foldl hasConcretisableMemory
      (patternContextElements pat, mempty)
      (patternValueElements pat),
    not $ null concretised = do

      -- Create non-existential memory blocks big enough to hold the
      -- arrays.
      forM_ concretised $ \(pat_elem, (mem, size, space)) -> do
        case size of
          Constant{} ->
            return ()
          Var size_v ->
            letBindNames'_ [size_v] =<<
            toExp (arraySizeInBytesExp $ patElemType pat_elem)
        letBindNames'_ [mem] $ Op $ Alloc size space

      -- Update the branches to contain Copy expressions putting the
      -- arrays where they are expected.
      let updateBody body = insertBindingsM $ do
            res <- bodyBind body
            resultBodyM =<<
              zipWithM updateResult (patternValueElements pat) res
          updateResult pat_elem (Var v)
            | Just _ <- lookup pat_elem concretised = do
                v_copy <- newVName $ textual v <> "_copy"
                letBind_ (Pattern [] [PatElem v_copy BindVar $ patElemAttr pat_elem]) $
                  BasicOp $ Copy v
                return $ Var v_copy
          updateResult _ se =
            return se
      tbranch' <- updateBody tbranch
      fbranch' <- updateBody fbranch

      letBind_ pat { patternContextElements = remaining_ctx} $
        If cond tbranch' fbranch' ret
  where onlyUsedIn name here = not $ any ((name `HS.member`) . freeIn) $
                                          filter ((/=here) . patElemName) $
                                          patternValueElements pat
        knownSize Constant{} = True
        knownSize (Var v) = not $ inContext v
        inContext = (`elem` patternContextNames pat)

        hasConcretisableMemory (ctx, concretised) pat_elem
          | (_, ArrayMem _ shape _ mem _) <- patElemAttr pat_elem,
            all knownSize (shapeDims shape),
            mem `onlyUsedIn` patElemName pat_elem,
            Just (size, space, ctx') <- getMemFromContext mem ctx,
            let concretised' = (pat_elem, (mem, size, space)) : concretised =
              case size of
                Constant{} ->
                  (ctx',
                   concretised')
                Var size_v | size_v `onlyUsedIn` mem ->
                  (filter ((/=size_v) . patElemName) ctx',
                   concretised')
                _ ->
                  (ctx,
                   concretised)
          | otherwise =
              (ctx, concretised)

        getMemFromContext _ [] = Nothing
        getMemFromContext mem (PatElem name _ (_, MemMem size space) : ctx)
          | name == mem = Just (size, space, ctx)
        getMemFromContext mem (pat_elem : ctx) = do
          (size, space, ctx') <- getMemFromContext mem ctx
          return (size, space, pat_elem : ctx')

unExistentialiseMemory _ _ = cannotSimplify

-- | If we are copying something that is itself a copy, just copy the
-- original one instead.
copyCopyToCopy :: (MonadBinder m,
                   LetAttr (Lore m) ~ (VarWisdom, MemBound u)) =>
                  TopDownRule m
copyCopyToCopy vtable (Let pat@(Pattern [] [pat_elem]) _ (BasicOp (Copy v1)))
  | Just (BasicOp (Copy v2)) <- ST.lookupExp v1 vtable,

    Just (_, ArrayMem _ _ _ srcmem src_ixfun) <-
      ST.entryLetBoundAttr =<< ST.lookup v1 vtable,

    Just (Mem _ src_space) <- ST.lookupType srcmem vtable,

    (_, ArrayMem _ _ _ destmem dest_ixfun) <- patElemAttr pat_elem,

    Just (Mem _ dest_space) <- ST.lookupType destmem vtable,

    src_space == dest_space, dest_ixfun == src_ixfun =

      letBind_ pat $ BasicOp $ Copy v2

copyCopyToCopy vtable (Let pat _ (BasicOp (Copy v0)))
  | Just (BasicOp (Rearrange cs perm v1)) <- ST.lookupExp v0 vtable,
    Just (BasicOp (Copy v2)) <- ST.lookupExp v1 vtable = do
      v0' <- letExp "rearrange_v0" $ BasicOp $ Rearrange cs perm v2
      letBind_ pat $ BasicOp $ Copy v0'

copyCopyToCopy _ _ = cannotSimplify
