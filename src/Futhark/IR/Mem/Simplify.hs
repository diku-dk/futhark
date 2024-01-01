{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.Mem.Simplify
  ( simplifyProgGeneric,
    simplifyStmsGeneric,
    simpleGeneric,
    SimplifyMemory,
    memRuleBook,
  )
where

import Control.Monad
import Data.List (find)
import Data.Maybe (isJust)
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.Construct
import Futhark.IR.Mem
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.IR.Prop.Aliases (AliasedOp)
import Futhark.Optimise.Simplify qualified as Simplify
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (simplifiable)
import Futhark.Util

-- | Some constraints that must hold for the simplification rules to work.
type SimplifyMemory rep inner =
  ( Simplify.SimplifiableRep rep,
    LetDec rep ~ LetDecMem,
    ExpDec rep ~ (),
    BodyDec rep ~ (),
    CanBeWise (OpC rep),
    BuilderOps (Wise rep),
    OpReturns (inner (Wise rep)),
    ST.IndexOp (inner (Wise rep)),
    AliasedOp (inner (Wise rep)),
    Mem rep inner,
    CanBeWise inner,
    RephraseOp inner
  )

simpleGeneric ::
  (SimplifyMemory rep inner) =>
  (inner (Wise rep) -> UT.UsageTable) ->
  Simplify.SimplifyOp rep (inner (Wise rep)) ->
  Simplify.SimpleOps rep
simpleGeneric = simplifiable

simplifyProgGeneric ::
  (SimplifyMemory rep inner) =>
  RuleBook (Wise rep) ->
  Simplify.SimpleOps rep ->
  Prog rep ->
  PassM (Prog rep)
simplifyProgGeneric rules ops =
  Simplify.simplifyProg
    ops
    rules
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
  RuleBook (Wise rep) ->
  Simplify.SimpleOps rep ->
  Stms rep ->
  m (Stms rep)
simplifyStmsGeneric rules ops stms = do
  scope <- askScope
  Simplify.simplifyStms
    ops
    rules
    blockers
    scope
    stms

isResultAlloc :: (OpC rep ~ MemOp op) => Engine.BlockPred rep
isResultAlloc _ usage (Let (Pat [pe]) _ (Op Alloc {})) =
  UT.isInResult (patElemName pe) usage
isResultAlloc _ _ _ = False

isAlloc :: (OpC rep ~ MemOp op) => Engine.BlockPred rep
isAlloc _ _ (Let _ _ (Op Alloc {})) = True
isAlloc _ _ _ = False

blockers ::
  (OpC rep ~ MemOp inner) =>
  Simplify.HoistBlockers rep
blockers =
  Engine.noExtraHoistBlockers
    { Engine.blockHoistPar = isAlloc,
      Engine.blockHoistSeq = isResultAlloc,
      Engine.isAllocation = isAlloc mempty mempty
    }

-- | Standard collection of simplification rules for representations
-- with memory.
memRuleBook :: (SimplifyMemory rep inner) => RuleBook (Wise rep)
memRuleBook =
  standardRules
    <> ruleBook
      [ RuleMatch unExistentialiseMemory,
        RuleOp decertifySafeAlloc
      ]
      []

-- | If a branch is returning some existential memory, but the size of
-- the array is not existential, and the index function of the array
-- does not refer to any names in the pattern, then we can create a
-- block of the proper size and always return there.
unExistentialiseMemory :: (SimplifyMemory rep inner) => TopDownRuleMatch (Wise rep)
unExistentialiseMemory vtable pat _ (cond, cases, defbody, ifdec)
  | ST.simplifyMemory vtable,
    fixable <- foldl hasConcretisableMemory mempty $ zip [0 ..] $ patElems pat,
    not $ null fixable = Simplify $ do
      -- Create non-existential memory blocks big enough to hold the
      -- arrays.
      (arr_to_mem, oldmem_to_mem) <-
        fmap unzip $
          forM fixable $ \(arr_pe, mem_size, oldmem, space) -> do
            size <- toSubExp "unext_mem_size" mem_size
            mem <- letExp "unext_mem" $ Op $ Alloc size space
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
                addStm $ mkWiseStm v_pat (defAux ()) $ BasicOp $ Replicate mempty $ Var v
                pure $ SubExpRes cs $ Var v_copy
            | Just mem <- lookup (patElemName pat_elem) oldmem_to_mem =
                pure $ SubExpRes cs $ Var mem
          updateResult _ se =
            pure se
      cases' <- mapM (traverse updateBody) cases
      defbody' <- updateBody defbody
      letBind pat $ Match cond cases' defbody' ifdec
  where
    onlyUsedIn name here =
      not . any ((name `nameIn`) . freeIn) . filter ((/= here) . patElemName) $
        patElems pat
    knownSize Constant {} = True
    knownSize (Var v) = not $ inContext v
    inContext = (`elem` patNames pat)

    hasConcretisableMemory fixable (i, pat_elem)
      | (_, MemArray pt shape _ (ArrayIn mem ixfun)) <- patElemDec pat_elem,
        Just (j, Mem space) <-
          fmap patElemType
            <$> find
              ((mem ==) . patElemName . snd)
              (zip [(0 :: Int) ..] $ patElems pat),
        Just cases_ses <- mapM (maybeNth j . bodyResult . caseBody) cases,
        Just defbody_se <- maybeNth j $ bodyResult defbody,
        mem `onlyUsedIn` patElemName pat_elem,
        all knownSize (shapeDims shape),
        not $ freeIn ixfun `namesIntersect` namesFromList (patNames pat),
        any (defbody_se /=) cases_ses,
        all (notIndex i) (defbody : map caseBody cases) =
          let mem_size = untyped $ primByteSize pt * (1 + IxFun.range ixfun)
           in (pat_elem, mem_size, mem, space) : fixable
      | otherwise =
          fixable

    -- Check if the i'th result is not an index; see #1325. This is a
    -- rather crude check, but this is also a rather crude
    -- simplification rule. We only need to keep it around until
    -- memory expansion can handle existential memory generally.
    notIndex i body
      | Just (SubExpRes _ (Var v)) <- maybeNth (i :: Int) $ bodyResult body =
          not $ any (bad v) $ bodyStms body
      where
        bad v (Let index_pat _ (BasicOp (Index _ slice))) =
          (v `elem` patNames index_pat) && any (isJust . dimFix) (unSlice slice)
        bad _ _ = False
    notIndex _ _ = True
unExistentialiseMemory _ _ _ _ = Skip

-- If an allocation is statically known to be safe, then we can remove
-- the certificates on it.  This can help hoist things that would
-- otherwise be stuck inside loops or branches.
decertifySafeAlloc :: (SimplifyMemory rep inner) => TopDownRuleOp (Wise rep)
decertifySafeAlloc _ pat (StmAux cs attrs _) op
  | cs /= mempty,
    [Mem _] <- patTypes pat,
    safeOp op =
      Simplify $ attributing attrs $ letBind pat $ Op op
decertifySafeAlloc _ _ _ _ = Skip
