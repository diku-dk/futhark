{-# LANGUAGE TypeFamilies #-}

-- | This module defines a collection of simplification rules, as per
-- "Futhark.Optimise.Simplify.Rule".  They are used in the
-- simplifier.
--
-- For performance reasons, many sufficiently simple logically
-- separate rules are merged into single "super-rules", like ruleIf
-- and ruleBasicOp.  This is because it is relatively expensive to
-- activate a rule just to determine that it does not apply.  Thus, it
-- is more efficient to have a few very fat rules than a lot of small
-- rules.  This does not affect the compiler result in any way; it is
-- purely an optimisation to speed up compilation.
module Futhark.Optimise.Simplify.Rules
  ( standardRules,
    removeUnnecessaryCopy,
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (insert, unzip4, zip4)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.Construct
import Futhark.IR
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.BasicOp
import Futhark.Optimise.Simplify.Rules.Index
import Futhark.Optimise.Simplify.Rules.Loop
import Futhark.Optimise.Simplify.Rules.Match
import Futhark.Util

topDownRules :: (BuilderOps rep) => [TopDownRule rep]
topDownRules =
  [ RuleGeneric constantFoldPrimFun,
    RuleGeneric withAccTopDown,
    RuleGeneric emptyArrayToScratch
  ]

bottomUpRules :: (BuilderOps rep, TraverseOpStms rep) => [BottomUpRule rep]
bottomUpRules =
  [ RuleGeneric withAccBottomUp,
    RuleBasicOp simplifyIndex
  ]

-- | A set of standard simplification rules.  These assume pure
-- functional semantics, and so probably should not be applied after
-- memory block merging.
standardRules :: (BuilderOps rep, TraverseOpStms rep) => RuleBook rep
standardRules =
  ruleBook topDownRules bottomUpRules
    <> loopRules
    <> basicOpRules
    <> matchRules

-- | Turn @copy(x)@ into @x@ iff @x@ is not used after this copy
-- statement and it can be consumed.
--
-- This simplistic rule is only valid before we introduce memory.
removeUnnecessaryCopy :: (BuilderOps rep) => BottomUpRuleBasicOp rep
removeUnnecessaryCopy (vtable, used) (Pat [d]) aux (Replicate (Shape []) (Var v))
  | not (v `UT.isConsumed` used),
    -- This two first clauses below are too conservative, but the
    -- problem is that 'v' might not look like it has been consumed if
    -- it is consumed in an outer scope.  This is because the
    -- simplifier applies bottom-up rules in a kind of deepest-first
    -- order.
    not (patElemName d `UT.isInResult` used)
      || (patElemName d `UT.isConsumed` used)
      -- Always OK to remove the copy if 'v' has no aliases and is never
      -- used again.
      || (v_is_fresh && v_not_used_again),
    (v_not_used_again && consumable) || not (patElemName d `UT.isConsumed` used) =
      Simplify $ auxing aux $ letBindNames [patElemName d] $ BasicOp $ SubExp $ Var v
  where
    v_not_used_again = not (v `UT.used` used)
    v_is_fresh = v `ST.lookupAliases` vtable == mempty
    -- We need to make sure we can even consume the original.  The big
    -- missing piece here is that we cannot do copy removal inside of
    -- 'map' and other SOACs, but that is handled by SOAC-specific rules.
    consumable = fromMaybe False $ do
      e <- ST.lookup v vtable
      guard $ ST.entryDepth e == ST.loopDepth vtable
      consumableStm e `mplus` consumableFParam e
    consumableFParam =
      Just . maybe False (unique . declTypeOf) . ST.entryFParam
    consumableStm e = do
      void $ ST.entryStm e -- Must be a stm.
      guard v_is_fresh
      pure True
removeUnnecessaryCopy _ _ _ _ = Skip

constantFoldPrimFun :: (BuilderOps rep) => TopDownRuleGeneric rep
constantFoldPrimFun _ (Let pat (StmAux cs attrs _) (Apply fname args _ _))
  | Just args' <- mapM (isConst . fst) args,
    Just (_, _, fun) <- M.lookup (nameToText fname) primFuns,
    Just result <- fun args' =
      Simplify $
        certifying cs $
          attributing attrs $
            letBind pat $
              BasicOp $
                SubExp $
                  Constant result
  where
    isConst (Constant v) = Just v
    isConst _ = Nothing
constantFoldPrimFun _ _ = Skip

-- | If an expression produces an array with a constant zero anywhere
-- in its shape, just turn that into a Scratch.
emptyArrayToScratch :: (BuilderOps rep) => TopDownRuleGeneric rep
emptyArrayToScratch _ (Let pat@(Pat [pe]) aux e)
  | Just (pt, shape) <- isEmptyArray $ patElemType pe,
    not $ isScratch e =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ Scratch pt $ shapeDims shape
  where
    isScratch (BasicOp Scratch {}) = True
    isScratch _ = False
emptyArrayToScratch _ _ = Skip

simplifyIndex :: (BuilderOps rep) => BottomUpRuleBasicOp rep
simplifyIndex (vtable, used) pat@(Pat [pe]) (StmAux cs attrs _) (Index idd inds)
  | Just m <- simplifyIndexing vtable seType idd inds consumed consuming =
      Simplify $ certifying cs $ do
        res <- m
        attributing attrs $ case res of
          SubExpResult cs' se ->
            certifying cs' $ letBindNames (patNames pat) $ BasicOp $ SubExp se
          IndexResult extra_cs idd' inds' ->
            certifying extra_cs $ letBindNames (patNames pat) $ BasicOp $ Index idd' inds'
  where
    consuming = (`UT.isConsumed` used)
    consumed = consuming $ patElemName pe
    seType (Var v) = ST.lookupType v vtable
    seType (Constant v) = Just $ Prim $ primValueType v
simplifyIndex _ _ _ _ = Skip

withAccTopDown :: (BuilderOps rep) => TopDownRuleGeneric rep
-- A WithAcc with no accumulators is sent to Valhalla.
withAccTopDown _ (Let pat aux (WithAcc [] lam)) = Simplify . auxing aux $ do
  lam_res <- bodyBind $ lambdaBody lam
  forM_ (zip (patNames pat) lam_res) $ \(v, SubExpRes cs se) ->
    certifying cs $ letBindNames [v] $ BasicOp $ SubExp se
-- Identify those results in 'lam' that are free and move them out.
withAccTopDown vtable (Let pat aux (WithAcc inputs lam)) = Simplify . auxing aux $ do
  let (cert_params, acc_params) =
        splitAt (length inputs) $ lambdaParams lam
      (acc_res, nonacc_res) =
        splitFromEnd num_nonaccs $ bodyResult $ lambdaBody lam
      (acc_pes, nonacc_pes) =
        splitFromEnd num_nonaccs $ patElems pat

  -- Look at accumulator results.
  (acc_pes', inputs', params', acc_res') <-
    fmap (unzip4 . catMaybes) . mapM tryMoveAcc $
      zip4
        (chunks (map inputArrs inputs) acc_pes)
        inputs
        (zip cert_params acc_params)
        acc_res
  let (cert_params', acc_params') = unzip params'

  -- Look at non-accumulator results.
  (nonacc_pes', nonacc_res') <-
    unzip . catMaybes <$> mapM tryMoveNonAcc (zip nonacc_pes nonacc_res)

  when (concat acc_pes' == acc_pes && nonacc_pes' == nonacc_pes) cannotSimplify

  lam' <-
    mkLambda (cert_params' ++ acc_params') $
      bodyBind $
        (lambdaBody lam) {bodyResult = acc_res' <> nonacc_res'}

  letBind (Pat (concat acc_pes' <> nonacc_pes')) $ WithAcc inputs' lam'
  where
    num_nonaccs = length (lambdaReturnType lam) - length inputs
    inputArrs (_, arrs, _) = length arrs

    tryMoveAcc (pes, (_, arrs, _), (_, acc_p), SubExpRes cs (Var v))
      | paramName acc_p == v,
        cs == mempty = do
          forM_ (zip pes arrs) $ \(pe, arr) ->
            letBindNames [patElemName pe] $ BasicOp $ SubExp $ Var arr
          pure Nothing
    tryMoveAcc x =
      pure $ Just x

    tryMoveNonAcc (pe, SubExpRes cs (Var v))
      | v `ST.elem` vtable,
        cs == mempty = do
          letBindNames [patElemName pe] $ BasicOp $ SubExp $ Var v
          pure Nothing
    tryMoveNonAcc (pe, SubExpRes cs (Constant v))
      | cs == mempty = do
          letBindNames [patElemName pe] $ BasicOp $ SubExp $ Constant v
          pure Nothing
    tryMoveNonAcc x =
      pure $ Just x
withAccTopDown _ _ = Skip

elimUpdates :: forall rep. (ASTRep rep, TraverseOpStms rep) => [VName] -> Body rep -> (Body rep, [VName])
elimUpdates get_rid_of = flip runState mempty . onBody
  where
    onBody body = do
      stms' <- onStms $ bodyStms body
      pure body {bodyStms = stms'}
    onStms = traverse onStm
    onStm (Let pat@(Pat [PatElem _ dec]) aux (BasicOp (UpdateAcc _ acc _ _)))
      | Acc c _ _ _ <- typeOf dec,
        c `elem` get_rid_of = do
          modify (insert c)
          pure $ Let pat aux $ BasicOp $ SubExp $ Var acc
    onStm (Let pat aux e) = Let pat aux <$> onExp e
    onExp = mapExpM mapper
      where
        mapper =
          (identityMapper :: forall m. (Monad m) => Mapper rep rep m)
            { mapOnOp = traverseOpStms (\_ stms -> onStms stms),
              mapOnBody = \_ body -> onBody body
            }

withAccBottomUp :: (TraverseOpStms rep, BuilderOps rep) => BottomUpRuleGeneric rep
-- Eliminate dead results.  See Note [Dead Code Elimination for WithAcc]
withAccBottomUp (_, utable) (Let pat aux (WithAcc inputs lam))
  | not $ all (`UT.used` utable) $ patNames pat = Simplify $ do
      let (acc_res, nonacc_res) =
            splitFromEnd num_nonaccs $ bodyResult $ lambdaBody lam
          (acc_pes, nonacc_pes) =
            splitFromEnd num_nonaccs $ patElems pat
          (cert_params, acc_params) =
            splitAt (length inputs) $ lambdaParams lam

      -- Eliminate unused accumulator results
      let get_rid_of =
            map snd . filter getRidOf
              $ zip
                (chunks (map inputArrs inputs) acc_pes)
              $ map paramName cert_params

      -- Eliminate unused non-accumulator results
      let (nonacc_pes', nonacc_res') =
            unzip $ filter keepNonAccRes $ zip nonacc_pes nonacc_res

      when (null get_rid_of && nonacc_pes' == nonacc_pes) cannotSimplify

      let (body', eliminated) = elimUpdates get_rid_of $ lambdaBody lam

      when (null eliminated && nonacc_pes' == nonacc_pes) cannotSimplify

      let pes' = acc_pes ++ nonacc_pes'

      lam' <- mkLambda (cert_params ++ acc_params) $ do
        void $ bodyBind body'
        pure $ acc_res ++ nonacc_res'

      auxing aux $ letBind (Pat pes') $ WithAcc inputs lam'
  where
    num_nonaccs = length (lambdaReturnType lam) - length inputs
    inputArrs (_, arrs, _) = length arrs
    getRidOf (pes, _) = not $ any ((`UT.used` utable) . patElemName) pes
    keepNonAccRes (pe, _) = patElemName pe `UT.used` utable
withAccBottomUp _ _ = Skip

-- Note [Dead Code Elimination for WithAcc]
--
-- Our static semantics for accumulators are basically those of linear
-- types.  This makes dead code elimination somewhat tricky.  First,
-- what we consider dead code is when we have a WithAcc where at least
-- one of the array results (that internally correspond to an
-- accumulator) are unused.  E.g
--
-- let {X',Y'} =
--   with_acc {X, Y} (\X_p Y_p X_acc Y_acc -> ... {X_acc', Y_acc'})
--
-- where X' is not used later.  Note that Y' is still used later.  If
-- none of the results of the WithAcc are used, then the Stm as a
-- whole is dead and can be removed.  That's the trivial case, done
-- implicitly by the simplifier.  The interesting case is exactly when
-- some of the results are unused.  How do we get rid of them?
--
-- Naively, we might just remove them:
--
-- let Y' =
--   with_acc Y (\Y_p Y_acc -> ... Y_acc')
--
-- This is safe *only* if X_acc is used *only* in the result (i.e. an
-- "identity" WithAcc).  Otherwise we end up with references to X_acc,
-- which no longer exists.  This simple case is actually handled in
-- the withAccTopDown rule, and is easy enough.
--
-- What we actually do when we decide to eliminate X_acc is that we
-- inspect the body of the WithAcc and eliminate all UpdateAcc
-- operations that refer to the same accumulator as X_acc (identified
-- by the X_p token).  I.e. we turn every
--
-- let B = update_acc(A, ...)
--
-- where 'A' is ultimately decided from X_acc into
--
-- let B = A
--
-- That's it!  We then let ordinary dead code elimination eventually
-- simplify the body enough that we have an "identity" WithAcc.  There
-- is no _guarantee_ that this will happen, but our general dead code
-- elimination tends to be pretty good.
