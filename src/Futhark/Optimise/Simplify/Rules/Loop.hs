-- | Loop simplification rules.
module Futhark.Optimise.Simplify.Rules.Loop (loopRules) where

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
      all (invariantOrNotMergeParam namesOfInvariant) $
        namesToList $
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
