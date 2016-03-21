{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Representation.ExplicitMemory.Simplify
       ( simplifyExplicitMemory
       )
where

import Control.Monad
import qualified Data.HashSet as HS
import Data.Monoid

import Prelude

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, Exp, Body, Binding,
          Pattern, PatElem, Lambda, ExtLambda, FunDef, FParam, LParam,
          RetType)
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.ExplicitMemory
import Futhark.Representation.Kernels.Simplify()
import Futhark.Pass.ExplicitAllocations (simplifiable, arraySizeInBytesExp)
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Construct
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.RuleM
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Lore
import Futhark.MonadFreshNames

simplifyExplicitMemory :: MonadFreshNames m => Prog -> m Prog
simplifyExplicitMemory =
  Simplifier.simplifyProgWithRules simplifiable explicitMemoryRules blockers
  where blockers =
          Engine.HoistBlockers {
            Engine.blockHoistPar = isAlloc
          , Engine.blockHoistSeq = isResultAlloc
          }

isAlloc :: Op lore ~ MemOp lore => Engine.BlockPred lore
isAlloc _ (Let _ _ (Op Alloc{})) = True
isAlloc _ _                      = False

isResultAlloc :: Op lore ~ MemOp lore => Engine.BlockPred lore
isResultAlloc usage (Let (AST.Pattern [] [bindee]) _
                     (Op Alloc{})) =
  UT.isInResult (patElemName bindee) usage
isResultAlloc _ _ = False

explicitMemoryRules :: (MonadBinder m,
                        LocalScope (Lore m) m,
                        (Lore m) ~ Wise ExplicitMemory,
                        Aliased (Lore m)) => RuleBook m
explicitMemoryRules = (std_td_rules <> topDownRules,
                       std_bu_rules <> bottomUpRules)
  where (std_td_rules, std_bu_rules) = standardRules

topDownRules :: (MonadBinder m,
                 LocalScope (Lore m) m,
                 (Lore m) ~ Wise ExplicitMemory,
                 Aliased (Lore m)) => TopDownRules m
topDownRules = [ unExistentialiseMemory
               , copyCopyToCopy
               ]

bottomUpRules :: (MonadBinder m,
                  LocalScope (Lore m) m,
                  Op (Lore m) ~ MemOp (Lore m)) => BottomUpRules m
bottomUpRules = [
                ]

unExistentialiseMemory :: (MonadBinder m, (Lore m) ~ Wise ExplicitMemory) =>
                          TopDownRule m

-- | If a branch is returning some existential memory, but we know the
-- size of the corresponding array non-existentially, then we can
-- create a block of the proper size and always return there.
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
            SE.fromScalExp (arraySizeInBytesExp $ patElemType pat_elem)
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
                  PrimOp $ Copy v
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
copyCopyToCopy :: (MonadBinder m, (Lore m) ~ Wise ExplicitMemory) =>
                  TopDownRule m
copyCopyToCopy vtable (Let pat@(Pattern [] [pat_elem]) _ (PrimOp (Copy v1)))
  | Just (PrimOp (Copy v2)) <- ST.lookupExp v1 vtable,

    Just (_, ArrayMem _ _ _ srcmem src_ixfun) <-
      ST.entryLetBoundAttr =<< ST.lookup v1 vtable,

    Just (Mem _ src_space) <- ST.lookupType srcmem vtable,

    (_, ArrayMem _ _ _ destmem dest_ixfun) <- patElemAttr pat_elem,

    Just (Mem _ dest_space) <- ST.lookupType destmem vtable,

    src_space == dest_space, dest_ixfun == src_ixfun =

      letBind_ pat $ PrimOp $ Copy v2

copyCopyToCopy vtable (Let pat _ (PrimOp (Copy v0)))
  | Just (PrimOp (Rearrange cs perm v1)) <- ST.lookupExp v0 vtable,
    Just (PrimOp (Copy v2)) <- ST.lookupExp v1 vtable = do
      v0' <- letExp "rearrange_v0" $ PrimOp $ Rearrange cs perm v2
      letBind_ pat $ PrimOp $ Copy v0'

copyCopyToCopy _ _ = cannotSimplify
