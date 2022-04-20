{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.Mem.Simplify
  ( simplifyProgGeneric,
    simplifyStmsGeneric,
    simpleGeneric,
    SimplifyMemory,
  )
where

import Control.Monad
import Data.List (find)
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Construct
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (simplifiable)
import Futhark.Util

simpleGeneric ::
  (SimplifyMemory rep inner) =>
  (OpWithWisdom inner -> UT.UsageTable) ->
  Simplify.SimplifyOp rep (OpWithWisdom inner) ->
  Simplify.SimpleOps rep
simpleGeneric = simplifiable

simplifyProgGeneric ::
  (SimplifyMemory rep inner) =>
  Simplify.SimpleOps rep ->
  Prog rep ->
  PassM (Prog rep)
simplifyProgGeneric ops =
  Simplify.simplifyProg
    ops
    callKernelRules
    blockers {Engine.blockHoistBranch = blockAllocs}
  where
    blockAllocs vtable _ (Let _ _ (Op Alloc {})) =
      not $ ST.simplifyMemory vtable
    -- Do not hoist statements that produce arrays.  This is
    -- because in the KernelsMem representation, multiple
    -- arrays can be located in the same memory block, and moving
    -- their creation out of a branch can thus cause memory
    -- corruption.  At this point in the compiler we have probably
    -- already moved all the array creations that matter.
    blockAllocs _ _ (Let pat _ _) =
      not $ all primType $ patTypes pat

simplifyStmsGeneric ::
  ( HasScope rep m,
    MonadFreshNames m,
    SimplifyMemory rep inner
  ) =>
  Simplify.SimpleOps rep ->
  Stms rep ->
  m (Stms rep)
simplifyStmsGeneric ops stms = do
  scope <- askScope
  Simplify.simplifyStms
    ops
    callKernelRules
    blockers
    scope
    stms

isResultAlloc :: Op rep ~ MemOp op => Engine.BlockPred rep
isResultAlloc _ usage (Let (Pat [pe]) _ (Op Alloc {})) =
  UT.isInResult (patElemName pe) usage
isResultAlloc _ _ _ = False

isAlloc :: Op rep ~ MemOp op => Engine.BlockPred rep
isAlloc _ _ (Let _ _ (Op Alloc {})) = True
isAlloc _ _ _ = False

blockers ::
  (Op rep ~ MemOp inner) =>
  Simplify.HoistBlockers rep
blockers =
  Engine.noExtraHoistBlockers
    { Engine.blockHoistPar = isAlloc,
      Engine.blockHoistSeq = isResultAlloc,
      Engine.isAllocation = isAlloc mempty mempty
    }

-- | Some constraints that must hold for the simplification rules to work.
type SimplifyMemory rep inner =
  ( Simplify.SimplifiableRep rep,
    LetDec rep ~ LetDecMem,
    ExpDec rep ~ (),
    BodyDec rep ~ (),
    CanBeWise (Op rep),
    BuilderOps (Wise rep),
    Mem rep inner
  )

callKernelRules :: SimplifyMemory rep inner => RuleBook (Wise rep)
callKernelRules =
  standardRules
    <> ruleBook
      [ RuleBasicOp copyCopyToCopy,
        RuleIf unExistentialiseMemory,
        RuleOp decertifySafeAlloc
      ]
      []

-- | If a branch is returning some existential memory, but the size of
-- the array is not existential, and the index function of the array
-- does not refer to any names in the pattern, then we can create a
-- block of the proper size and always return there.
unExistentialiseMemory :: SimplifyMemory rep inner => TopDownRuleIf (Wise rep)
unExistentialiseMemory vtable pat _ (cond, tbranch, fbranch, ifdec)
  | ST.simplifyMemory vtable,
    fixable <- foldl hasConcretisableMemory mempty $ patElems pat,
    not $ null fixable = Simplify $ do
      -- Create non-existential memory blocks big enough to hold the
      -- arrays.
      (arr_to_mem, oldmem_to_mem) <-
        fmap unzip $
          forM fixable $ \(arr_pe, mem_size, oldmem, space) -> do
            size <- toSubExp "size" mem_size
            mem <- letExp "mem" $ Op $ Alloc size space
            pure ((patElemName arr_pe, mem), (oldmem, mem))

      -- Update the branches to contain Copy expressions putting the
      -- arrays where they are expected.
      let updateBody body = buildBody_ $ do
            res <- bodyBind body
            zipWithM updateResult (patElems pat) res
          updateResult pat_elem (SubExpRes cs (Var v))
            | Just mem <- lookup (patElemName pat_elem) arr_to_mem,
              (_, MemArray pt shape u (ArrayIn _ ixfun)) <- patElemDec pat_elem = do
                v_copy <- newVName $ baseString v <> "_nonext_copy"
                let v_pat =
                      Pat [PatElem v_copy $ MemArray pt shape u $ ArrayIn mem ixfun]
                addStm $ mkWiseStm v_pat (defAux ()) $ BasicOp (Copy v)
                pure $ SubExpRes cs $ Var v_copy
            | Just mem <- lookup (patElemName pat_elem) oldmem_to_mem =
                pure $ SubExpRes cs $ Var mem
          updateResult _ se =
            pure se
      tbranch' <- updateBody tbranch
      fbranch' <- updateBody fbranch
      letBind pat $ If cond tbranch' fbranch' ifdec
  where
    onlyUsedIn name here =
      not . any ((name `nameIn`) . freeIn) . filter ((/= here) . patElemName) $
        patElems pat
    knownSize Constant {} = True
    knownSize (Var v) = not $ inContext v
    inContext = (`elem` patNames pat)

    hasConcretisableMemory fixable pat_elem
      | (_, MemArray pt shape _ (ArrayIn mem ixfun)) <- patElemDec pat_elem,
        Just (j, Mem space) <-
          fmap patElemType
            <$> find
              ((mem ==) . patElemName . snd)
              (zip [(0 :: Int) ..] $ patElems pat),
        Just tse <- maybeNth j $ bodyResult tbranch,
        Just fse <- maybeNth j $ bodyResult fbranch,
        mem `onlyUsedIn` patElemName pat_elem,
        length (IxFun.base ixfun) == shapeRank shape, -- See #1325
        all knownSize (shapeDims shape),
        not $ freeIn ixfun `namesIntersect` namesFromList (patNames pat),
        fse /= tse =
          let mem_size =
                untyped $ product $ primByteSize pt : map sExt64 (IxFun.base ixfun)
           in (pat_elem, mem_size, mem, space) : fixable
      | otherwise =
          fixable
unExistentialiseMemory _ _ _ _ = Skip

-- | If we are copying something that is itself a copy, just copy the
-- original one instead.
copyCopyToCopy ::
  ( BuilderOps rep,
    LetDec rep ~ (VarWisdom, MemBound u)
  ) =>
  TopDownRuleBasicOp rep
copyCopyToCopy vtable pat@(Pat [pat_elem]) _ (Copy v1)
  | Just (BasicOp (Copy v2), v1_cs) <- ST.lookupExp v1 vtable,
    Just (_, MemArray _ _ _ (ArrayIn srcmem src_ixfun)) <-
      ST.entryLetBoundDec =<< ST.lookup v1 vtable,
    Just (Mem src_space) <- ST.lookupType srcmem vtable,
    (_, MemArray _ _ _ (ArrayIn destmem dest_ixfun)) <- patElemDec pat_elem,
    Just (Mem dest_space) <- ST.lookupType destmem vtable,
    src_space == dest_space,
    dest_ixfun == src_ixfun =
      Simplify $ certifying v1_cs $ letBind pat $ BasicOp $ Copy v2
copyCopyToCopy vtable pat _ (Copy v0)
  | Just (BasicOp (Rearrange perm v1), v0_cs) <- ST.lookupExp v0 vtable,
    Just (BasicOp (Copy v2), v1_cs) <- ST.lookupExp v1 vtable = Simplify $ do
      v0' <-
        certifying (v0_cs <> v1_cs) $
          letExp "rearrange_v0" $ BasicOp $ Rearrange perm v2
      letBind pat $ BasicOp $ Copy v0'
copyCopyToCopy _ _ _ _ = Skip

-- If an allocation is statically known to be safe, then we can remove
-- the certificates on it.  This can help hoist things that would
-- otherwise be stuck inside loops or branches.
decertifySafeAlloc :: SimplifyMemory rep inner => TopDownRuleOp (Wise rep)
decertifySafeAlloc _ pat (StmAux cs attrs _) op
  | cs /= mempty,
    [Mem _] <- patTypes pat,
    safeOp op =
      Simplify $ attributing attrs $ letBind pat $ Op op
decertifySafeAlloc _ _ _ _ = Skip
