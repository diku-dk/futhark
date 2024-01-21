{-# LANGUAGE TypeFamilies #-}

module Futhark.IR.Mem.Simplify
  ( simplifyProgGeneric,
    simplifyStmsGeneric,
    simpleGeneric,
    SimplifyMemory,
    memRuleBook,
  )
where

import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.Construct
import Futhark.IR.Mem
import Futhark.IR.Prop.Aliases (AliasedOp)
import Futhark.Optimise.Simplify qualified as Simplify
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (simplifiable)

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
  Simplify.simplifyStms ops rules blockers scope stms

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
      [ RuleOp decertifySafeAlloc
      ]
      []

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
