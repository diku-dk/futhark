-- | Loop simplification rules.
module Futhark.Optimise.Simplify.Rules.Loop
  ( loopRules,
    -- | Peel an @is_first@ toggle out of a 'Loop'.  Kept separate from
    -- 'loopRules' so it can be enabled only by the per-backend rule
    -- books (post-fusion, post-extraction).  See 'peelIsFirstParam'.
    peelIsFirstRules,
  )
where

import Control.Monad
import Data.Bifunctor (second)
import Data.List (partition)
import Data.Maybe
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.Construct
import Futhark.IR
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.ClosedForm
import Futhark.Transform.Rename

-- This next one is tricky - it's easy enough to determine that some
-- loop result is not used after the loop, but here, we must also make
-- sure that it does not affect any other values.
--
-- I do not claim that the current implementation of this rule is
-- perfect, but it should suffice for many cases, and should never
-- generate wrong code.
removeRedundantLoopParams :: (BuilderOps rep) => BottomUpRuleLoop rep
removeRedundantLoopParams (_, used) pat aux (merge, form, body)
  | not $ all (usedAfterLoop . fst) merge =
      let necessaryForReturned =
            findNecessaryForReturned
              usedAfterLoopOrInForm
              (zip (map fst merge) (map resSubExp $ bodyResult body))
              (dataDependencies body)

          resIsNecessary ((v, _), _) =
            usedAfterLoop v
              || (paramName v `nameIn` necessaryForReturned)
              || referencedInPat v
              || referencedInForm v

          (keep_valpart, discard_valpart) =
            partition (resIsNecessary . snd) $
              zip (patElems pat) $
                zip merge $
                  bodyResult body

          (keep_valpatelems, keep_val) = unzip keep_valpart
          (_discard_valpatelems, discard_val) = unzip discard_valpart
          (merge', val_es') = unzip keep_val

          body' = body {bodyResult = val_es'}

          pat' = Pat keep_valpatelems
       in if merge' == merge
            then Skip
            else Simplify $ do
              -- We can't just remove the bindings in 'discard', since the loop
              -- body may still use their names in (now-dead) expressions.
              -- Hence, we add them inside the loop, fully aware that dead-code
              -- removal will eventually get rid of them.  Some care is
              -- necessary to handle unique bindings.
              body'' <- insertStmsM $ do
                mapM_ (uncurry letBindNames) $ dummyStms discard_val
                pure body'
              auxing aux $ letBind pat' $ Loop merge' form body''
  where
    pat_used = map (`UT.isUsedDirectly` used) $ patNames pat
    used_vals = map fst $ filter snd $ zip (map (paramName . fst) merge) pat_used
    usedAfterLoop = flip elem used_vals . paramName
    usedAfterLoopOrInForm p =
      usedAfterLoop p || paramName p `nameIn` freeIn form
    patAnnotNames = freeIn $ map fst merge
    referencedInPat = (`nameIn` patAnnotNames) . paramName
    referencedInForm = (`nameIn` freeIn form) . paramName

    dummyStms = map dummyStm
    dummyStm ((p, e), _)
      | unique (paramDeclType p),
        Var v <- e =
          ([paramName p], BasicOp $ Replicate mempty $ Var v)
      | otherwise = ([paramName p], BasicOp $ SubExp e)
removeRedundantLoopParams _ _ _ _ =
  Skip

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantLoopParams :: (BuilderOps rep) => TopDownRuleLoop rep
hoistLoopInvariantLoopParams vtable pat aux (merge, form, loopbody) = do
  -- Figure out which of the elements of loopresult are
  -- loop-invariant, and hoist them out.
  let explpat = zip (patElems pat) $ map (paramName . fst) merge
  case foldr checkInvariance ([], explpat, [], []) $
    zip3 (patNames pat) merge res of
    ([], _, _, _) ->
      -- Nothing is invariant.
      Skip
    (invariant, explpat', merge', res') -> Simplify . auxing aux $ do
      -- We have moved something invariant out of the loop.
      let loopbody' = loopbody {bodyResult = res'}
          explpat'' = map fst explpat'
      forM_ invariant $ \(v1, (v2, cs)) ->
        certifying cs $ letBindNames [identName v1] $ BasicOp $ SubExp v2
      letBind (Pat explpat'') $ Loop merge' form loopbody'
  where
    res = bodyResult loopbody

    namesOfLoopParams = namesFromList $ map (paramName . fst) merge

    removeFromResult (mergeParam, mergeInit) explpat' =
      case partition ((== paramName mergeParam) . snd) explpat' of
        ([(patelem, _)], rest) ->
          (Just (patElemIdent patelem, (mergeInit, mempty)), rest)
        (_, _) ->
          (Nothing, explpat')

    checkInvariance
      (pat_name, (mergeParam, mergeInit), resExp)
      (invariant, explpat', merge', resExps)
        | isInvariant =
            let -- Remove certificates (some of which may be loop-variant)
                -- because this corresponds to dead code anyway.
                (stm, explpat'') = removeFromResult (mergeParam, mergeInit) explpat'
             in ( maybe id (:) stm $
                    (paramIdent mergeParam, (mergeInit, mempty)) : invariant,
                  explpat'',
                  merge',
                  resExps
                )
        where
          -- A loop parameter is invariant if one of the
          -- following is true:
          isInvariant
            -- (0) The result is a variable of the same name as the
            -- parameter, where all existential parameters are already
            -- known to be invariant
            | Var v2 <- resSubExp resExp,
              paramName mergeParam == v2 =
                allExistentialInvariant
                  (namesFromList $ map (identName . fst) invariant)
                  mergeParam
            -- (1) The result is identical to the initial parameter value.
            | mergeInit == resSubExp resExp = True
            -- (2) The initial parameter value is equal to an outer
            -- loop parameter 'P', where the initial value of 'P' is
            -- equal to 'resExp', AND 'resExp' ultimately becomes the
            -- new value of 'P'.  XXX: it's a bit clumsy that this
            -- only works for one level of nesting, and I think it
            -- would not be too hard to generalise.
            | Var init_v <- mergeInit,
              Just (p_init, p_res) <- ST.lookupLoopParam init_v vtable,
              p_init == resSubExp resExp,
              p_res == Var pat_name =
                True
            -- (3) It is a statically empty array.
            | isJust $ isEmptyArray (paramType mergeParam) = True
            | otherwise = False
    checkInvariance
      (_pat_name, (mergeParam, mergeInit), resExp)
      (invariant, explpat', merge', resExps) =
        (invariant, explpat', (mergeParam, mergeInit) : merge', resExp : resExps)

    allExistentialInvariant namesOfInvariant mergeParam =
      allNames (invariantOrNotMergeParam namesOfInvariant) $
        freeIn mergeParam `namesSubtract` oneName (paramName mergeParam)
    invariantOrNotMergeParam namesOfInvariant name =
      (name `notNameIn` namesOfLoopParams)
        || (name `nameIn` namesOfInvariant)

simplifyClosedFormLoop :: (BuilderOps rep) => TopDownRuleLoop rep
simplifyClosedFormLoop _ pat _ (val, ForLoop i it bound, body) =
  Simplify $ loopClosedForm pat val (oneName i) it bound body
simplifyClosedFormLoop _ _ _ _ = Skip

unroll ::
  (BuilderOps rep) =>
  Integer ->
  [(FParam rep, SubExpRes)] ->
  (VName, IntType, Integer) ->
  Body rep ->
  RuleM rep [SubExpRes]
unroll n merge (iv, it, i) body
  | i >= n =
      pure $ map snd merge
  | otherwise = do
      iter_body <- insertStmsM $ do
        forM_ merge $ \(mergevar, SubExpRes cs mergeinit) ->
          certifying cs $ letBindNames [paramName mergevar] $ BasicOp $ SubExp mergeinit

        letBindNames [iv] $ BasicOp $ SubExp $ intConst it i

        -- Some of the sizes in the types here might be temporarily wrong
        -- until copy propagation fixes it up.
        pure body

      iter_body' <- renameBody iter_body
      addStms $ bodyStms iter_body'

      let merge' = zip (map fst merge) $ bodyResult iter_body'
      unroll n merge' (iv, it, i + 1) body

simplifyKnownIterationLoop :: (BuilderOps rep) => TopDownRuleLoop rep
simplifyKnownIterationLoop _ pat aux (merge, ForLoop i it (Constant iters), body)
  | IntValue n <- iters,
    zeroIshInt n || oneIshInt n || "unroll" `inAttrs` stmAuxAttrs aux = Simplify $ do
      res <- unroll (valueIntegral n) (map (second subExpRes) merge) (i, it, 0) body
      forM_ (zip (patNames pat) res) $ \(v, SubExpRes cs se) ->
        certifying cs $ letBindNames [v] $ BasicOp $ SubExp se
simplifyKnownIterationLoop _ _ _ _ =
  Skip

-- | Peel a merge parameter that follows the \"is_first\" pattern:
--
--   * The parameter has type @bool@.
--   * Its initial value is the constant @true@.
--   * The body's result for that position is the constant @false@.
--   * Every top-level body statement that mentions the parameter is a
--     'Match' whose sole scrutinee is exactly that parameter, and the
--     parameter does not occur elsewhere in the body's result.
--
-- When matched, we peel iteration 0 (with the parameter substituted to
-- @true@, so each match collapses to its @True@ branch) and run the
-- remainder of the loop with the parameter removed.  This eliminates
-- the cross-iteration boolean and the in-loop branch.
--
-- Restricted to constant bounds @≥ 1@ (so peeling never changes
-- observable behaviour) and to top-level matches (nested matches and
-- indirect uses cause the rule to skip).
peelIsFirstParam :: (BuilderOps rep) => TopDownRuleLoop rep
peelIsFirstParam _ pat aux (merge, ForLoop i it (Constant bound_v), body)
  | IntValue bound_n <- bound_v,
    not (zeroIshInt bound_n),
    Just (idx, p) <- findIsFirstSlot merge body,
    bodyUsesParamOnlyAsTopLevelMatchCond idx (paramName p) body =
      Simplify $ do
        -- Peel iteration 0 by binding merge params to their initial
        -- values (substituting @true@ for the is_first param) and
        -- inlining the body once.
        forM_ (zip merge [0 :: Int ..]) $ \((p', se), j) ->
          if j == idx
            then
              letBindNames [paramName p'] . BasicOp . SubExp $
                Constant (BoolValue True)
            else letBindNames [paramName p'] . BasicOp $ SubExp se
        letBindNames [i] . BasicOp . SubExp $ intConst it 0
        peeled_body <- renameBody body
        addStms $ bodyStms peeled_body
        let peeled_res = bodyResult peeled_body

        let keepIdx j = j /= idx
            new_merge =
              [ (p', new_se)
                | ((p', _), j, new_se) <-
                    zip3
                      merge
                      [0 :: Int ..]
                      (map resSubExp peeled_res),
                  keepIdx j
              ]
            new_body_results =
              [ se
                | (se, j) <- zip (bodyResult body) [0 :: Int ..],
                  keepIdx j
              ]

        -- Rebuild the body with @i@ rebound to @i' + 1@ and the
        -- is_first param substituted to @false@.
        i' <- newVName (baseName i <> "_peel")
        one_se <- letSubExp "one" . BasicOp . SubExp $ intConst it 1
        rest_body <- insertStmsM $ do
          letBindNames [i] . BasicOp $
            BinOp (Add it OverflowWrap) (Var i') one_se
          letBindNames [paramName p] . BasicOp . SubExp $
            Constant (BoolValue False)
          addStms $ bodyStms body
          mkBodyM mempty new_body_results

        let bound_minus_one =
              constant
                ( case bound_n of
                    Int8Value n -> Int8Value (n - 1)
                    Int16Value n -> Int16Value (n - 1)
                    Int32Value n -> Int32Value (n - 1)
                    Int64Value n -> Int64Value (n - 1)
                    _ -> bound_n
                )
            rest_loop =
              Loop new_merge (ForLoop i' it bound_minus_one) rest_body
            new_pat_elems =
              [ pe
                | (pe, j) <- zip (patElems pat) [0 :: Int ..],
                  keepIdx j
              ]
            bool_pat_elem = patElems pat !! idx

        rest_results <- letTupExp "peel_rest" rest_loop
        auxing aux $ do
          letBindNames [patElemName bool_pat_elem] . BasicOp . SubExp $
            Constant (BoolValue False)
          forM_ (zip (map patElemName new_pat_elems) rest_results) $ \(v, r) ->
            letBindNames [v] . BasicOp . SubExp $ Var r
peelIsFirstParam _ _ _ _ = Skip

-- | Find an @is_first@-shaped merge param: bool, init True, body
-- result False.  Returns its position in the merge and the parameter.
findIsFirstSlot ::
  (ASTRep rep) =>
  [(FParam rep, SubExp)] ->
  Body rep ->
  Maybe (Int, FParam rep)
findIsFirstSlot merge body =
  listToMaybe
    [ (j, p)
      | (j, (p, init_se), body_se) <-
          zip3 [0 ..] merge (map resSubExp (bodyResult body)),
        paramType p == Prim Bool,
        init_se == Constant (BoolValue True),
        body_se == Constant (BoolValue False)
    ]

-- | Conservative check: every top-level body statement that mentions
-- @v@ does so only as the sole scrutinee of a 'Match', and the body's
-- result tuple does not mention @v@ outside its own slot.
bodyUsesParamOnlyAsTopLevelMatchCond ::
  (ASTRep rep) =>
  Int ->
  VName ->
  Body rep ->
  Bool
bodyUsesParamOnlyAsTopLevelMatchCond own_idx v body =
  let inResultElsewhere =
        any
          (\(j, se) -> v `nameIn` freeIn se && j /= own_idx)
          (zip [0 :: Int ..] (map resSubExp (bodyResult body)))
      okStm stm
        | v `nameIn` freeIn stm =
            case stmExp stm of
              Match [Var scrut] _ _ _ -> scrut == v
              _ -> False
        | otherwise = True
   in not inResultElsewhere && all okStm (bodyStms body)

topDownRules :: (BuilderOps rep) => [TopDownRule rep]
topDownRules =
  [ RuleLoop hoistLoopInvariantLoopParams,
    RuleLoop simplifyClosedFormLoop,
    RuleLoop simplifyKnownIterationLoop
  ]

bottomUpRules :: (BuilderOps rep) => [BottomUpRule rep]
bottomUpRules =
  [ RuleLoop removeRedundantLoopParams
  ]

-- | Standard loop simplification rules.
loopRules :: (BuilderOps rep) => RuleBook rep
loopRules = ruleBook topDownRules bottomUpRules

-- | The @is_first@-peeling rule, packaged as a separate rule book so
-- it can be enabled only after fusion has had a chance to operate on
-- the loop.  Adding this to the SOACS-stage simplify is harmless in
-- practice (the pattern does not appear until after stream extraction
-- or first-order transform) but conceptually inappropriate.
peelIsFirstRules :: (BuilderOps rep) => RuleBook rep
peelIsFirstRules = ruleBook [RuleLoop peelIsFirstParam] []
