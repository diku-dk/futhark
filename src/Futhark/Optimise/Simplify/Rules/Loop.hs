{-# LANGUAGE OverloadedStrings #-}

-- | Loop simplification rules.
module Futhark.Optimise.Simplify.Rules.Loop (loopRules) where

import Control.Monad
import Data.Bifunctor (second)
import Data.List (partition)
import Data.Maybe
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.ClosedForm
import Futhark.Optimise.Simplify.Rules.Index
import Futhark.Transform.Rename

-- This next one is tricky - it's easy enough to determine that some
-- loop result is not used after the loop, but here, we must also make
-- sure that it does not affect any other values.
--
-- I do not claim that the current implementation of this rule is
-- perfect, but it should suffice for many cases, and should never
-- generate wrong code.
removeRedundantMergeVariables :: BuilderOps rep => BottomUpRuleDoLoop rep
removeRedundantMergeVariables (_, used) pat aux (merge, form, body)
  | not $ all (usedAfterLoop . fst) merge =
      let necessaryForReturned =
            findNecessaryForReturned
              usedAfterLoopOrInForm
              (zip (map fst merge) (map resSubExp $ bodyResult body))
              (dataDependencies body)

          resIsNecessary ((v, _), _) =
            usedAfterLoop v
              || paramName v `nameIn` necessaryForReturned
              || referencedInPat v
              || referencedInForm v

          (keep_valpart, discard_valpart) =
            partition (resIsNecessary . snd) $
              zip (patElems pat) $ zip merge $ bodyResult body

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
              auxing aux $ letBind pat' $ DoLoop merge' form body''
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
          ([paramName p], BasicOp $ Copy v)
      | otherwise = ([paramName p], BasicOp $ SubExp e)
removeRedundantMergeVariables _ _ _ _ =
  Skip

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantMergeVariables :: BuilderOps rep => TopDownRuleDoLoop rep
hoistLoopInvariantMergeVariables vtable pat aux (merge, form, loopbody) = do
  -- Figure out which of the elements of loopresult are
  -- loop-invariant, and hoist them out.
  let explpat = zip (patElems pat) $ map (paramName . fst) merge
  case foldr checkInvariance ([], explpat, [], []) $
    zip3 (patNames pat) merge res of
    ([], _, _, _) ->
      -- Nothing is invariant.
      Skip
    (invariant, explpat', merge', res') -> Simplify $ do
      -- We have moved something invariant out of the loop.
      let loopbody' = loopbody {bodyResult = res'}
          explpat'' = map fst explpat'
      forM_ invariant $ \(v1, v2) ->
        letBindNames [identName v1] $ BasicOp $ SubExp v2
      auxing aux $ letBind (Pat explpat'') $ DoLoop merge' form loopbody'
  where
    res = bodyResult loopbody

    namesOfMergeParams = namesFromList $ map (paramName . fst) merge

    removeFromResult (mergeParam, mergeInit) explpat' =
      case partition ((== paramName mergeParam) . snd) explpat' of
        ([(patelem, _)], rest) ->
          (Just (patElemIdent patelem, mergeInit), rest)
        (_, _) ->
          (Nothing, explpat')

    checkInvariance
      (pat_name, (mergeParam, mergeInit), resExp)
      (invariant, explpat', merge', resExps)
        | isInvariant,
          -- Also do not remove the condition in a while-loop.
          not $ paramName mergeParam `nameIn` freeIn form =
            let (stm, explpat'') =
                  removeFromResult (mergeParam, mergeInit) explpat'
             in ( maybe id (:) stm $ (paramIdent mergeParam, mergeInit) : invariant,
                  explpat'',
                  merge',
                  resExps
                )
        where
          -- A non-unique merge variable is invariant if one of the
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
      not (name `nameIn` namesOfMergeParams)
        || name `nameIn` namesOfInvariant

simplifyClosedFormLoop :: BuilderOps rep => TopDownRuleDoLoop rep
simplifyClosedFormLoop _ pat _ (val, ForLoop i it bound [], body) =
  Simplify $ loopClosedForm pat val (oneName i) it bound body
simplifyClosedFormLoop _ _ _ _ = Skip

simplifyLoopVariables :: (BuilderOps rep, Aliased rep) => TopDownRuleDoLoop rep
simplifyLoopVariables vtable pat aux (merge, form@(ForLoop i it num_iters loop_vars), body)
  | simplifiable <- map checkIfSimplifiable loop_vars,
    not $ all isNothing simplifiable = Simplify $ do
      -- Check if the simplifications throw away more information than
      -- we are comfortable with at this stage.
      (maybe_loop_vars, body_prefix_stms) <-
        localScope (scopeOf form) $
          unzip <$> zipWithM onLoopVar loop_vars simplifiable
      if maybe_loop_vars == map Just loop_vars
        then cannotSimplify
        else do
          body' <- buildBody_ $ do
            addStms $ mconcat body_prefix_stms
            bodyBind body
          let form' = ForLoop i it num_iters $ catMaybes maybe_loop_vars
          auxing aux $ letBind pat $ DoLoop merge form' body'
  where
    seType (Var v)
      | v == i = Just $ Prim $ IntType it
      | otherwise = ST.lookupType v vtable
    seType (Constant v) = Just $ Prim $ primValueType v
    consumed_in_body = consumedInBody body

    vtable' = ST.fromScope (scopeOf form) <> vtable

    checkIfSimplifiable (p, arr) =
      simplifyIndexing
        vtable'
        seType
        arr
        (Slice (DimFix (Var i) : unSlice (fullSlice (paramType p) [])))
        $ paramName p `nameIn` consumed_in_body

    -- We only want this simplification if the result does not refer
    -- to 'i' at all, or does not contain accesses.
    onLoopVar (p, arr) Nothing =
      pure (Just (p, arr), mempty)
    onLoopVar (p, arr) (Just m) = do
      (x, x_stms) <- collectStms m
      case x of
        IndexResult cs arr' (Slice slice)
          | not $ any ((i `nameIn`) . freeIn) x_stms,
            DimFix (Var j) : slice' <- slice,
            j == i,
            not $ i `nameIn` freeIn slice -> do
              addStms x_stms
              w <- arraySize 0 <$> lookupType arr'
              for_in_partial <-
                certifying cs $
                  letExp "for_in_partial" . BasicOp . Index arr' . Slice $
                    DimSlice (intConst Int64 0) w (intConst Int64 1) : slice'
              pure (Just (p, for_in_partial), mempty)
        SubExpResult cs se
          | all (notIndex . stmExp) x_stms -> do
              x_stms' <- collectStms_ $
                certifying cs $ do
                  addStms x_stms
                  letBindNames [paramName p] $ BasicOp $ SubExp se
              pure (Nothing, x_stms')
        _ -> pure (Just (p, arr), mempty)

    notIndex (BasicOp Index {}) = False
    notIndex _ = True
simplifyLoopVariables _ _ _ _ = Skip

-- If a for-loop with no loop variables has a counter of type Int64,
-- and the bound is just a constant or sign-extended integer of
-- smaller type, then change the loop to iterate over the smaller type
-- instead.  We then move the sign extension inside the loop instead.
-- This addresses loops of the form @for i in x..<y@ in the source
-- language.
narrowLoopType :: (BuilderOps rep) => TopDownRuleDoLoop rep
narrowLoopType vtable pat aux (merge, ForLoop i Int64 n [], body)
  | Just (n', it', cs) <- smallerType =
      Simplify $ do
        i' <- newVName $ baseString i
        let form' = ForLoop i' it' n' []
        body' <- insertStmsM . inScopeOf form' $ do
          letBindNames [i] $ BasicOp $ ConvOp (SExt it' Int64) (Var i')
          pure body
        auxing aux $ certifying cs $ letBind pat $ DoLoop merge form' body'
  where
    smallerType
      | Var n' <- n,
        Just (ConvOp (SExt it' _) n'', cs) <- ST.lookupBasicOp n' vtable =
          Just (n'', it', cs)
      | Constant (IntValue (Int64Value n')) <- n,
        toInteger n' <= toInteger (maxBound :: Int32) =
          Just (intConst Int32 (toInteger n'), Int32, mempty)
      | otherwise =
          Nothing
narrowLoopType _ _ _ _ = Skip

unroll ::
  BuilderOps rep =>
  Integer ->
  [(FParam rep, SubExpRes)] ->
  (VName, IntType, Integer) ->
  [(LParam rep, VName)] ->
  Body rep ->
  RuleM rep [SubExpRes]
unroll n merge (iv, it, i) loop_vars body
  | i >= n =
      pure $ map snd merge
  | otherwise = do
      iter_body <- insertStmsM $ do
        forM_ merge $ \(mergevar, SubExpRes cs mergeinit) ->
          certifying cs $ letBindNames [paramName mergevar] $ BasicOp $ SubExp mergeinit

        letBindNames [iv] $ BasicOp $ SubExp $ intConst it i

        forM_ loop_vars $ \(p, arr) ->
          letBindNames [paramName p] . BasicOp . Index arr . Slice $
            DimFix (intConst Int64 i) : unSlice (fullSlice (paramType p) [])

        -- Some of the sizes in the types here might be temporarily wrong
        -- until copy propagation fixes it up.
        pure body

      iter_body' <- renameBody iter_body
      addStms $ bodyStms iter_body'

      let merge' = zip (map fst merge) $ bodyResult iter_body'
      unroll n merge' (iv, it, i + 1) loop_vars body

simplifyKnownIterationLoop :: BuilderOps rep => TopDownRuleDoLoop rep
simplifyKnownIterationLoop _ pat aux (merge, ForLoop i it (Constant iters) loop_vars, body)
  | IntValue n <- iters,
    zeroIshInt n || oneIshInt n || "unroll" `inAttrs` stmAuxAttrs aux = Simplify $ do
      res <- unroll (valueIntegral n) (map (second subExpRes) merge) (i, it, 0) loop_vars body
      forM_ (zip (patNames pat) res) $ \(v, SubExpRes cs se) ->
        certifying cs $ letBindNames [v] $ BasicOp $ SubExp se
simplifyKnownIterationLoop _ _ _ _ =
  Skip

topDownRules :: (BuilderOps rep, Aliased rep) => [TopDownRule rep]
topDownRules =
  [ RuleDoLoop hoistLoopInvariantMergeVariables,
    RuleDoLoop simplifyClosedFormLoop,
    RuleDoLoop simplifyKnownIterationLoop,
    RuleDoLoop simplifyLoopVariables,
    RuleDoLoop narrowLoopType
  ]

bottomUpRules :: BuilderOps rep => [BottomUpRule rep]
bottomUpRules =
  [ RuleDoLoop removeRedundantMergeVariables
  ]

-- | Standard loop simplification rules.
loopRules :: (BuilderOps rep, Aliased rep) => RuleBook rep
loopRules = ruleBook topDownRules bottomUpRules
