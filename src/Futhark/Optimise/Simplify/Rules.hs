{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Either
import Data.List (insert, partition, transpose, unzip4, zip4, zip5)
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.BasicOp
import Futhark.Optimise.Simplify.Rules.Index
import Futhark.Optimise.Simplify.Rules.Loop
import Futhark.Util

topDownRules :: BuilderOps rep => [TopDownRule rep]
topDownRules =
  [ RuleGeneric constantFoldPrimFun,
    RuleMatch ruleMatch,
    RuleMatch hoistBranchInvariant,
    RuleGeneric withAccTopDown
  ]

bottomUpRules :: (BuilderOps rep, TraverseOpStms rep) => [BottomUpRule rep]
bottomUpRules =
  [ RuleMatch removeDeadBranchResult,
    RuleGeneric withAccBottomUp,
    RuleBasicOp simplifyIndex
  ]

-- | A set of standard simplification rules.  These assume pure
-- functional semantics, and so probably should not be applied after
-- memory block merging.
standardRules :: (BuilderOps rep, TraverseOpStms rep, Aliased rep) => RuleBook rep
standardRules = ruleBook topDownRules bottomUpRules <> loopRules <> basicOpRules

-- | Turn @copy(x)@ into @x@ iff @x@ is not used after this copy
-- statement and it can be consumed.
--
-- This simplistic rule is only valid before we introduce memory.
removeUnnecessaryCopy :: BuilderOps rep => BottomUpRuleBasicOp rep
removeUnnecessaryCopy (vtable, used) (Pat [d]) _ (Copy v)
  | not (v `UT.isConsumed` used),
    -- This two first clauses below are too conservative, but the
    -- problem is that 'v' might not look like it has been consumed if
    -- it is consumed in an outer scope.  This is because the
    -- simplifier applies bottom-up rules in a kind of deepest-first
    -- order.
    not (patElemName d `UT.isInResult` used)
      || patElemName d
      `UT.isConsumed` used
      -- Always OK to remove the copy if 'v' has no aliases and is never
      -- used again.
      || (v_is_fresh && v_not_used_again),
    (v_not_used_again && consumable) || not (patElemName d `UT.isConsumed` used) =
      Simplify $ letBindNames [patElemName d] $ BasicOp $ SubExp $ Var v
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

constantFoldPrimFun :: BuilderOps rep => TopDownRuleGeneric rep
constantFoldPrimFun _ (Let pat (StmAux cs attrs _) (Apply fname args _ _))
  | Just args' <- mapM (isConst . fst) args,
    Just (_, _, fun) <- M.lookup (nameToString fname) primFuns,
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

simplifyIndex :: BuilderOps rep => BottomUpRuleBasicOp rep
simplifyIndex (vtable, used) pat@(Pat [pe]) (StmAux cs attrs _) (Index idd inds)
  | Just m <- simplifyIndexing vtable seType idd inds consumed = Simplify $ do
      res <- m
      attributing attrs $ case res of
        SubExpResult cs' se ->
          certifying (cs <> cs') $
            letBindNames (patNames pat) $
              BasicOp $
                SubExp se
        IndexResult extra_cs idd' inds' ->
          certifying (cs <> extra_cs) $
            letBindNames (patNames pat) $
              BasicOp $
                Index idd' inds'
  where
    consumed = patElemName pe `UT.isConsumed` used
    seType (Var v) = ST.lookupType v vtable
    seType (Constant v) = Just $ Prim $ primValueType v
simplifyIndex _ _ _ _ = Skip

-- Does this case always match the scrutinees?
caseMatches :: [SubExp] -> Case a -> Bool
caseMatches ses = and . zipWith match ses . casePat
  where
    match se (Just v) = se == Constant v
    match _ Nothing = True

-- Can this case never match the scrutinees?
caseNeverMatches :: [SubExp] -> Case a -> Bool
caseNeverMatches ses = or . zipWith impossible ses . casePat
  where
    impossible se (Just v) = se /= Constant v
    impossible _ Nothing = False

ruleMatch :: BuilderOps rep => TopDownRuleMatch rep
-- Remove impossible cases.
ruleMatch _ pat _ (cond, cases, defbody, ifdec)
  | (impossible, cases') <- partition (caseNeverMatches cond) cases,
    not $ null impossible =
      Simplify $ letBind pat $ Match cond cases' defbody ifdec
-- Find new default case.
ruleMatch _ pat _ (cond, cases, _, ifdec)
  | (always_matches, cases') <- partition (caseMatches cond) cases,
    new_default : _ <- reverse always_matches =
      Simplify $ letBind pat $ Match cond cases' (caseBody new_default) ifdec
-- Remove caseless match.
ruleMatch _ pat (StmAux cs _ _) (_, [], defbody, _) = Simplify $ do
  defbody_res <- bodyBind defbody
  certifying cs $ forM_ (zip (patElems pat) defbody_res) $ \(pe, res) ->
    certifying (resCerts res) . letBind (Pat [pe]) $
      BasicOp (SubExp $ resSubExp res)
-- IMPROVE: the following two rules can be generalised to work in more
-- cases, especially when the branches have bindings, or return more
-- than one value.
--
-- if c then True else v == c || v
ruleMatch
  _
  pat
  _
  ( [cond],
    [ Case
        [Just (BoolValue True)]
        (Body _ tstms [SubExpRes tcs (Constant (BoolValue True))])
      ],
    Body _ fstms [SubExpRes fcs se],
    IfDec ts _
    )
    | null tstms,
      null fstms,
      [Prim Bool] <- map extTypeOf ts =
        Simplify $ certifying (tcs <> fcs) $ letBind pat $ BasicOp $ BinOp LogOr cond se
-- When type(x)==bool, if c then x else y == (c && x) || (!c && y)
ruleMatch _ pat _ ([cond], [Case [Just (BoolValue True)] tb], fb, IfDec ts _)
  | Body _ tstms [SubExpRes tcs tres] <- tb,
    Body _ fstms [SubExpRes fcs fres] <- fb,
    all (safeExp . stmExp) $ tstms <> fstms,
    all ((== Prim Bool) . extTypeOf) ts = Simplify $ do
      addStms tstms
      addStms fstms
      e <-
        eBinOp
          LogOr
          (pure $ BasicOp $ BinOp LogAnd cond tres)
          ( eBinOp
              LogAnd
              (pure $ BasicOp $ UnOp Not cond)
              (pure $ BasicOp $ SubExp fres)
          )
      certifying (tcs <> fcs) $ letBind pat e
ruleMatch _ pat _ (_, [Case _ tbranch], _, IfDec _ IfFallback)
  | all (safeExp . stmExp) $ bodyStms tbranch = Simplify $ do
      let ses = bodyResult tbranch
      addStms $ bodyStms tbranch
      sequence_
        [ certifying cs $ letBindNames [patElemName p] $ BasicOp $ SubExp se
          | (p, SubExpRes cs se) <- zip (patElems pat) ses
        ]
ruleMatch _ pat _ ([cond], [Case _ tb], fb, _)
  | Body _ _ [SubExpRes tcs (Constant (IntValue t))] <- tb,
    Body _ _ [SubExpRes fcs (Constant (IntValue f))] <- fb =
      if oneIshInt t && zeroIshInt f && tcs == mempty && fcs == mempty
        then
          Simplify . letBind pat . BasicOp $
            ConvOp (BToI (intValueType t)) cond
        else
          if zeroIshInt t && oneIshInt f
            then Simplify $ do
              cond_neg <- letSubExp "cond_neg" $ BasicOp $ UnOp Not cond
              letBind pat $ BasicOp $ ConvOp (BToI (intValueType t)) cond_neg
            else Skip
-- Simplify
--
--   let z = if c then x else y
--
-- to
--
--   let z = y
--
-- in the case where 'x' is a loop parameter with initial value 'y'
-- and the new value of the loop parameter is 'z'.  ('x' and 'y' can
-- be flipped.)
ruleMatch vtable (Pat [pe]) aux (_c, [Case _ tb], fb, IfDec [_] _)
  | Body _ tstms [SubExpRes xcs x] <- tb,
    null tstms,
    Body _ fstms [SubExpRes ycs y] <- fb,
    null fstms,
    matches x y || matches y x =
      Simplify . certifying (stmAuxCerts aux <> xcs <> ycs) $
        letBind (Pat [pe]) (BasicOp $ SubExp y)
  where
    z = patElemName pe
    matches (Var x) y
      | Just (initial, res) <- ST.lookupLoopParam x vtable =
          initial == y && res == Var z
    matches _ _ = False
ruleMatch _ _ _ _ = Skip

-- | Move out results of a conditional expression whose computation is
-- either invariant to the branches (only done for results used for
-- existentials), or the same in both branches.
hoistBranchInvariant :: BuilderOps rep => TopDownRuleMatch rep
hoistBranchInvariant _ pat _ (cond, cases, defbody, IfDec ret ifsort) = Simplify $ do
  let case_reses = map (bodyResult . caseBody) cases
      defbody_res = bodyResult defbody
  (hoistings, (pes, ts, case_reses_tr, defbody_res')) <-
    fmap (fmap unzip4 . partitionEithers) . mapM branchInvariant $
      zip5 [0 ..] (patElems pat) ret (transpose case_reses) defbody_res
  let ctx_fixes = catMaybes hoistings
      onCase (Case vs body) case_res = Case vs $ body {bodyResult = case_res}
      cases' = zipWith onCase cases $ transpose case_reses_tr
      defbody' = defbody {bodyResult = defbody_res'}
      ret' = foldr (uncurry fixExt) ts ctx_fixes
  if not $ null hoistings -- Was something hoisted?
    then do
      -- We may have to add some reshapes if we made the type
      -- less existential.
      cases'' <- mapM (traverse $ reshapeBodyResults $ map extTypeOf ret') cases'
      defbody'' <- reshapeBodyResults (map extTypeOf ret') defbody'
      letBind (Pat pes) $ Match cond cases'' defbody'' (IfDec ret' ifsort)
    else cannotSimplify
  where
    bound_in_branches =
      namesFromList . concatMap (patNames . stmPat) $
        foldMap (bodyStms . caseBody) cases <> bodyStms defbody
    invariant Constant {} = True
    invariant (Var v) = v `notNameIn` bound_in_branches

    branchInvariant (i, pe, t, case_reses, defres)
      -- Do all branches return the same value?
      | all ((== resSubExp defres) . resSubExp) case_reses = do
          certifying (foldMap resCerts case_reses <> resCerts defres) $
            letBindNames [patElemName pe] . BasicOp . SubExp $
              resSubExp defres
          hoisted i pe

      -- Do both branches return values that are free in the
      -- branch, and are we not the only pattern element?  The
      -- latter is to avoid infinite application of this rule.
      | all (invariant . resSubExp) case_reses,
        invariant $ resSubExp defres,
        patSize pat > 1,
        Prim _ <- patElemType pe = do
          bt <- expTypesFromPat $ Pat [pe]
          letBindNames [patElemName pe]
            =<< ( Match cond
                    <$> ( zipWith Case (map casePat cases)
                            <$> mapM (resultBodyM . pure . resSubExp) case_reses
                        )
                    <*> resultBodyM [resSubExp defres]
                    <*> pure (IfDec bt ifsort)
                )
          hoisted i pe
      | otherwise =
          pure $ Right (pe, t, case_reses, defres)

    hoisted i pe = pure $ Left $ Just (i, Var $ patElemName pe)

    reshapeBodyResults rets body = buildBody_ $ do
      ses <- bodyBind body
      let (ctx_ses, val_ses) = splitFromEnd (length rets) ses
      (ctx_ses ++) <$> zipWithM reshapeResult val_ses rets
    reshapeResult (SubExpRes cs (Var v)) t@Array {} = do
      v_t <- lookupType v
      let newshape = arrayDims $ removeExistentials t v_t
      SubExpRes cs
        <$> if newshape /= arrayDims v_t
          then letSubExp "branch_ctx_reshaped" (shapeCoerce newshape v)
          else pure $ Var v
    reshapeResult se _ =
      pure se

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: BuilderOps rep => BottomUpRuleMatch rep
removeDeadBranchResult (_, used) pat _ (cond, cases, defbody, IfDec rettype ifsort)
  | -- Only if there is no existential binding...
    all (`notNameIn` foldMap freeIn (patElems pat)) (patNames pat),
    -- Figure out which of the names in 'pat' are used...
    patused <- map (`UT.isUsedDirectly` used) $ patNames pat,
    -- If they are not all used, then this rule applies.
    not (and patused) = do
      -- Remove the parts of the branch-results that correspond to dead
      -- return value bindings.  Note that this leaves dead code in the
      -- branch bodies, but that will be removed later.
      let pick :: [a] -> [a]
          pick = map snd . filter fst . zip patused
          pat' = pick $ patElems pat
          rettype' = pick rettype
      Simplify $ do
        cases' <- mapM (traverse $ onBody pick) cases
        defbody' <- onBody pick defbody
        letBind (Pat pat') $ Match cond cases' defbody' $ IfDec rettype' ifsort
  | otherwise = Skip
  where
    onBody pick (Body _ stms res) = mkBodyM stms $ pick res

withAccTopDown :: BuilderOps rep => TopDownRuleGeneric rep
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

elimUpdates :: (ASTRep rep, TraverseOpStms rep) => [VName] -> Body rep -> (Body rep, [VName])
elimUpdates get_rid_of = flip runState mempty . onBody
  where
    onBody body = do
      stms' <- onStms $ bodyStms body
      pure body {bodyStms = stms'}
    onStms = traverse onStm
    onStm (Let pat@(Pat [PatElem _ dec]) aux (BasicOp (UpdateAcc acc _ _)))
      | Acc c _ _ _ <- typeOf dec,
        c `elem` get_rid_of = do
          modify (insert c)
          pure $ Let pat aux $ BasicOp $ SubExp $ Var acc
    onStm (Let pat aux e) = Let pat aux <$> onExp e
    onExp = mapExpM mapper
      where
        mapper =
          identityMapper
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
