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
import Futhark.IR.Syntax hiding
  ( BasicOp,
    Body,
    Exp,
    FParam,
    FunDef,
    LParam,
    Lambda,
    PatElem,
    Pattern,
    Prog,
    RetType,
    Stm,
  )
import qualified Futhark.IR.Syntax as AST
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Lore
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations (simplifiable)
import Futhark.Util

simpleGeneric ::
  (SimplifyMemory lore, Op lore ~ MemOp inner) =>
  (OpWithWisdom inner -> UT.UsageTable) ->
  Simplify.SimplifyOp lore inner ->
  Simplify.SimpleOps lore
simpleGeneric = simplifiable

simplifyProgGeneric ::
  (SimplifyMemory lore, Op lore ~ MemOp inner) =>
  Simplify.SimpleOps lore ->
  Prog lore ->
  PassM (Prog lore)
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
      not $ all primType $ patternTypes pat

simplifyStmsGeneric ::
  ( HasScope lore m,
    MonadFreshNames m,
    SimplifyMemory lore,
    Op lore ~ MemOp inner
  ) =>
  Simplify.SimpleOps lore ->
  Stms lore ->
  m (ST.SymbolTable (Wise lore), Stms lore)
simplifyStmsGeneric ops stms = do
  scope <- askScope
  Simplify.simplifyStms
    ops
    callKernelRules
    blockers
    scope
    stms

isResultAlloc :: Op lore ~ MemOp op => Engine.BlockPred lore
isResultAlloc _ usage (Let (AST.Pattern [] [bindee]) _ (Op Alloc {})) =
  UT.isInResult (patElemName bindee) usage
isResultAlloc _ _ _ = False

isAlloc :: Op lore ~ MemOp op => Engine.BlockPred lore
isAlloc _ _ (Let _ _ (Op Alloc {})) = True
isAlloc _ _ _ = False

blockers ::
  (Op lore ~ MemOp inner) =>
  Simplify.HoistBlockers lore
blockers =
  Engine.noExtraHoistBlockers
    { Engine.blockHoistPar = isAlloc,
      Engine.blockHoistSeq = isResultAlloc,
      Engine.isAllocation = isAlloc mempty mempty
    }

-- | Some constraints that must hold for the simplification rules to work.
type SimplifyMemory lore =
  ( Simplify.SimplifiableLore lore,
    ExpDec lore ~ (),
    BodyDec lore ~ (),
    AllocOp (Op (Wise lore)),
    CanBeWise (Op lore),
    BinderOps (Wise lore),
    Mem lore
  )

callKernelRules :: SimplifyMemory lore => RuleBook (Wise lore)
callKernelRules =
  standardRules
    <> ruleBook
      [ RuleBasicOp copyCopyToCopy,
        RuleBasicOp removeIdentityCopy,
        RuleIf unExistentialiseMemory
      ]
      []

-- | If a branch is returning some existential memory, but the size of
-- the array is not existential, and the index function of the array
-- does not refer to any names in the pattern, then we can create a
-- block of the proper size and always return there.
unExistentialiseMemory :: SimplifyMemory lore => TopDownRuleIf (Wise lore)
unExistentialiseMemory vtable pat _ (cond, tbranch, fbranch, ifdec)
  | ST.simplifyMemory vtable,
    fixable <- foldl hasConcretisableMemory mempty $ patternElements pat,
    not $ null fixable = Simplify $ do
    -- Create non-existential memory blocks big enough to hold the
    -- arrays.
    (arr_to_mem, oldmem_to_mem) <-
      fmap unzip $
        forM fixable $ \(arr_pe, mem_size, oldmem, space) -> do
          size <- toSubExp "size" mem_size
          mem <- letExp "mem" $ Op $ allocOp size space
          return ((patElemName arr_pe, mem), (oldmem, mem))

    -- Update the branches to contain Copy expressions putting the
    -- arrays where they are expected.
    let updateBody body = insertStmsM $ do
          res <- bodyBind body
          resultBodyM
            =<< zipWithM updateResult (patternElements pat) res
        updateResult pat_elem (Var v)
          | Just mem <- lookup (patElemName pat_elem) arr_to_mem,
            (_, MemArray pt shape u (ArrayIn _ ixfun)) <- patElemDec pat_elem = do
            v_copy <- newVName $ baseString v <> "_nonext_copy"
            let v_pat =
                  Pattern
                    []
                    [ PatElem v_copy $
                        MemArray pt shape u $ ArrayIn mem ixfun
                    ]
            addStm $ mkWiseLetStm v_pat (defAux ()) $ BasicOp (Copy v)
            return $ Var v_copy
          | Just mem <- lookup (patElemName pat_elem) oldmem_to_mem =
            return $ Var mem
        updateResult _ se =
          return se
    tbranch' <- updateBody tbranch
    fbranch' <- updateBody fbranch
    letBind pat $ If cond tbranch' fbranch' ifdec
  where
    onlyUsedIn name here =
      not $
        any ((name `nameIn`) . freeIn) $
          filter ((/= here) . patElemName) $
            patternValueElements pat
    knownSize Constant {} = True
    knownSize (Var v) = not $ inContext v
    inContext = (`elem` patternContextNames pat)

    hasConcretisableMemory fixable pat_elem
      | (_, MemArray pt shape _ (ArrayIn mem ixfun)) <- patElemDec pat_elem,
        Just (j, Mem space) <-
          fmap patElemType
            <$> find
              ((mem ==) . patElemName . snd)
              (zip [(0 :: Int) ..] $ patternElements pat),
        Just tse <- maybeNth j $ bodyResult tbranch,
        Just fse <- maybeNth j $ bodyResult fbranch,
        mem `onlyUsedIn` patElemName pat_elem,
        all knownSize (shapeDims shape),
        not $ freeIn ixfun `namesIntersect` namesFromList (patternNames pat),
        fse /= tse =
        let mem_size =
              sExt Int64 $ product $ primByteSize pt : IxFun.base ixfun
         in (pat_elem, mem_size, mem, space) : fixable
      | otherwise =
        fixable
unExistentialiseMemory _ _ _ _ = Skip

-- | If we are copying something that is itself a copy, just copy the
-- original one instead.
copyCopyToCopy ::
  ( BinderOps lore,
    LetDec lore ~ (VarWisdom, MemBound u)
  ) =>
  TopDownRuleBasicOp lore
copyCopyToCopy vtable pat@(Pattern [] [pat_elem]) _ (Copy v1)
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

-- | If the destination of a copy is the same as the source, just
-- remove it.
removeIdentityCopy ::
  ( BinderOps lore,
    LetDec lore ~ (VarWisdom, MemBound u)
  ) =>
  TopDownRuleBasicOp lore
removeIdentityCopy vtable pat@(Pattern [] [pe]) _ (Copy v)
  | (_, MemArray _ _ _ (ArrayIn dest_mem dest_ixfun)) <- patElemDec pe,
    Just (_, MemArray _ _ _ (ArrayIn src_mem src_ixfun)) <-
      ST.entryLetBoundDec =<< ST.lookup v vtable,
    dest_mem == src_mem,
    dest_ixfun == src_ixfun =
    Simplify $ letBind pat $ BasicOp $ SubExp $ Var v
removeIdentityCopy _ _ _ _ = Skip
