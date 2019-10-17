{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
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
  ( standardRules
  , removeUnnecessaryCopy
  )
where

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.DataDependencies
import Futhark.Optimise.Simplify.ClosedForm
import Futhark.Optimise.Simplify.Rule
import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Construct
import Futhark.Util

topDownRules :: (BinderOps lore, Aliased lore) => [TopDownRule lore]
topDownRules = [ RuleDoLoop hoistLoopInvariantMergeVariables
               , RuleDoLoop simplifyClosedFormLoop
               , RuleDoLoop simplifKnownIterationLoop
               , RuleDoLoop simplifyLoopVariables
               , RuleGeneric constantFoldPrimFun
               , RuleIf ruleIf
               , RuleIf hoistBranchInvariant
               , RuleBasicOp ruleBasicOp
               ]

bottomUpRules :: BinderOps lore => [BottomUpRule lore]
bottomUpRules = [ RuleDoLoop removeRedundantMergeVariables
                , RuleIf removeDeadBranchResult
                , RuleBasicOp simplifyIndex
                , RuleBasicOp simplifyConcat
                ]

asInt32PrimExp :: PrimExp v -> PrimExp v
asInt32PrimExp pe
  | IntType it <- primExpType pe, it /= Int32 =
      ConvOpExp (SExt it Int32) pe
  | otherwise =
      pe

-- | A set of standard simplification rules.  These assume pure
-- functional semantics, and so probably should not be applied after
-- memory block merging.
standardRules :: (BinderOps lore, Aliased lore) => RuleBook lore
standardRules = ruleBook topDownRules bottomUpRules

-- This next one is tricky - it's easy enough to determine that some
-- loop result is not used after the loop, but here, we must also make
-- sure that it does not affect any other values.
--
-- I do not claim that the current implementation of this rule is
-- perfect, but it should suffice for many cases, and should never
-- generate wrong code.
removeRedundantMergeVariables :: BinderOps lore => BottomUpRuleDoLoop lore
removeRedundantMergeVariables (_, used) pat _ (ctx, val, form, body)
  | not $ all (usedAfterLoop . fst) val,
    null ctx = -- FIXME: things get tricky if we can remove all vals
               -- but some ctxs are still used.  We take the easy way
               -- out for now.
  let (ctx_es, val_es) = splitAt (length ctx) $ bodyResult body
      necessaryForReturned =
        findNecessaryForReturned usedAfterLoopOrInForm
        (zip (map fst $ ctx++val) $ ctx_es++val_es) (dataDependencies body)

      resIsNecessary ((v,_), _) =
        usedAfterLoop v ||
        paramName v `nameIn` necessaryForReturned ||
        referencedInPat v ||
        referencedInForm v

      (keep_ctx, discard_ctx) =
        partition resIsNecessary $ zip ctx ctx_es
      (keep_valpart, discard_valpart) =
        partition (resIsNecessary . snd) $
        zip (patternValueElements pat) $ zip val val_es

      (keep_valpatelems, keep_val) = unzip keep_valpart
      (_discard_valpatelems, discard_val) = unzip discard_valpart
      (ctx', ctx_es') = unzip keep_ctx
      (val', val_es') = unzip keep_val

      body' = body { bodyResult = ctx_es' ++ val_es' }
      free_in_keeps = freeIn keep_valpatelems

      stillUsedContext pat_elem =
        patElemName pat_elem `nameIn`
        (free_in_keeps <>
         freeIn (filter (/=pat_elem) $ patternContextElements pat))

      pat' = pat { patternValueElements = keep_valpatelems
                 , patternContextElements =
                     filter stillUsedContext $ patternContextElements pat }
  in if ctx' ++ val' == ctx ++ val
     then Skip
     else Simplify $ do
       -- We can't just remove the bindings in 'discard', since the loop
       -- body may still use their names in (now-dead) expressions.
       -- Hence, we add them inside the loop, fully aware that dead-code
       -- removal will eventually get rid of them.  Some care is
       -- necessary to handle unique bindings.
       body'' <- insertStmsM $ do
         mapM_ (uncurry letBindNames) $ dummyStms discard_ctx
         mapM_ (uncurry letBindNames) $ dummyStms discard_val
         return body'
       letBind_ pat' $ DoLoop ctx' val' form body''
  where pat_used = map (`UT.isUsedDirectly` used) $ patternValueNames pat
        used_vals = map fst $ filter snd $ zip (map (paramName . fst) val) pat_used
        usedAfterLoop = flip elem used_vals . paramName
        usedAfterLoopOrInForm p =
          usedAfterLoop p || paramName p `nameIn` freeIn form
        patAnnotNames = freeIn $ map fst $ ctx++val
        referencedInPat = (`nameIn` patAnnotNames) . paramName
        referencedInForm = (`nameIn` freeIn form) . paramName

        dummyStms = map dummyStm
        dummyStm ((p,e), _)
          | unique (paramDeclType p),
            Var v <- e            = ([paramName p], BasicOp $ Copy v)
          | otherwise             = ([paramName p], BasicOp $ SubExp e)
removeRedundantMergeVariables _ _ _ _ =
  Skip

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantMergeVariables :: BinderOps lore => TopDownRuleDoLoop lore
hoistLoopInvariantMergeVariables _ pat _ (ctx, val, form, loopbody) =
    -- Figure out which of the elements of loopresult are
    -- loop-invariant, and hoist them out.
  case foldr checkInvariance ([], explpat, [], []) $
       zip merge res of
    ([], _, _, _) ->
      -- Nothing is invariant.
      Skip
    (invariant, explpat', merge', res') -> Simplify $ do
      -- We have moved something invariant out of the loop.
      let loopbody' = loopbody { bodyResult = res' }
          invariantShape :: (a, VName) -> Bool
          invariantShape (_, shapemerge) = shapemerge `elem`
                                           map (paramName . fst) merge'
          (implpat',implinvariant) = partition invariantShape implpat
          implinvariant' = [ (patElemIdent p, Var v) | (p,v) <- implinvariant ]
          implpat'' = map fst implpat'
          explpat'' = map fst explpat'
          (ctx', val') = splitAt (length implpat') merge'
      forM_ (invariant ++ implinvariant') $ \(v1,v2) ->
        letBindNames_ [identName v1] $ BasicOp $ SubExp v2
      letBind_ (Pattern implpat'' explpat'') $
        DoLoop ctx' val' form loopbody'
  where merge = ctx ++ val
        res = bodyResult loopbody

        implpat = zip (patternContextElements pat) $
                  map (paramName . fst) ctx
        explpat = zip (patternValueElements pat) $
                  map (paramName . fst) val

        namesOfMergeParams = namesFromList $ map (paramName . fst) $ ctx++val

        removeFromResult (mergeParam,mergeInit) explpat' =
          case partition ((==paramName mergeParam) . snd) explpat' of
            ([(patelem,_)], rest) ->
              (Just (patElemIdent patelem, mergeInit), rest)
            (_,      _) ->
              (Nothing, explpat')

        checkInvariance
          ((mergeParam,mergeInit), resExp)
          (invariant, explpat', merge', resExps)
          | not (unique (paramDeclType mergeParam)) ||
            arrayRank (paramDeclType mergeParam) == 1,
            isInvariant resExp,
            -- Also do not remove the condition in a while-loop.
            not $ paramName mergeParam `nameIn` freeIn form =
          let (bnd, explpat'') =
                removeFromResult (mergeParam,mergeInit) explpat'
          in (maybe id (:) bnd $ (paramIdent mergeParam, mergeInit) : invariant,
              explpat'', merge', resExps)
          where
            -- A non-unique merge variable is invariant if the corresponding
            -- subexp in the result is EITHER:
            --
            --  (0) a variable of the same name as the parameter, where
            --  all existential parameters are already known to be
            --  invariant
            isInvariant (Var v2)
              | paramName mergeParam == v2 =
                allExistentialInvariant
                (namesFromList $ map (identName . fst) invariant) mergeParam
            --  (1) or identical to the initial value of the parameter.
            isInvariant _ = mergeInit == resExp

        checkInvariance ((mergeParam,mergeInit), resExp) (invariant, explpat', merge', resExps) =
          (invariant, explpat', (mergeParam,mergeInit):merge', resExp:resExps)

        allExistentialInvariant namesOfInvariant mergeParam =
          all (invariantOrNotMergeParam namesOfInvariant) $ namesToList $
          freeIn mergeParam `namesSubtract` oneName (paramName mergeParam)
        invariantOrNotMergeParam namesOfInvariant name =
          not (name `nameIn` namesOfMergeParams) ||
          name `nameIn` namesOfInvariant

-- | A function that, given a subexpression, returns its type.
type TypeLookup = SubExp -> Maybe Type

-- | A simple rule is a top-down rule that can be expressed as a pure
-- function.
type SimpleRule lore = VarLookup lore -> TypeLookup -> BasicOp lore -> Maybe (BasicOp lore, Certificates)

simpleRules :: [SimpleRule lore]
simpleRules = [ simplifyBinOp
              , simplifyCmpOp
              , simplifyUnOp
              , simplifyConvOp
              , simplifyAssert
              , copyScratchToScratch
              , simplifyIdentityReshape
              , simplifyReshapeReshape
              , simplifyReshapeScratch
              , simplifyReshapeReplicate
              , simplifyReshapeIota
              , improveReshape ]

simplifyClosedFormLoop :: BinderOps lore => TopDownRuleDoLoop lore
simplifyClosedFormLoop _ pat _ ([], val, ForLoop i _ bound [], body) =
  Simplify $ loopClosedForm pat val (oneName i) bound body
simplifyClosedFormLoop _ _ _ _ = Skip

simplifyLoopVariables :: (BinderOps lore, Aliased lore) => TopDownRuleDoLoop lore
simplifyLoopVariables vtable pat _ (ctx, val, form@(ForLoop i it num_iters loop_vars), body)
  | simplifiable <- map checkIfSimplifiable loop_vars,
    not $ all isNothing simplifiable = Simplify $ do
      -- Check if the simplifications throw away more information than
      -- we are comfortable with at this stage.
      (maybe_loop_vars, body_prefix_stms) <-
        localScope (scopeOf form) $
        unzip <$> zipWithM onLoopVar loop_vars simplifiable
      if maybe_loop_vars == map Just loop_vars
        then cannotSimplify
        else do body' <- insertStmsM $ do
                  addStms $ mconcat body_prefix_stms
                  resultBodyM =<< bodyBind body
                letBind_ pat $ DoLoop ctx val
                  (ForLoop i it num_iters $ catMaybes maybe_loop_vars) body'

  where seType (Var v)
          | v == i = Just $ Prim $ IntType it
          | otherwise = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v
        consumed_in_body = consumedInBody body

        vtable' = ST.fromScope (scopeOf form) <> vtable

        checkIfSimplifiable (p,arr) =
          simplifyIndexing vtable' seType arr
          (DimFix (Var i) : fullSlice (paramType p) []) $
          paramName p `nameIn` consumed_in_body

        -- We only want this simplification if the result does not refer
        -- to 'i' at all, or does not contain accesses.
        onLoopVar (p,arr) Nothing =
          return (Just (p,arr), mempty)
        onLoopVar (p,arr) (Just m) = do
          (x,x_stms) <- collectStms m
          case x of
            IndexResult cs arr' slice
              | all (not . (i `nameIn`) . freeIn) x_stms,
                DimFix (Var j) : slice' <- slice,
                j == i, not $ i `nameIn` freeIn slice -> do
                  addStms x_stms
                  w <- arraySize 0 <$> lookupType arr'
                  for_in_partial <-
                    certifying cs $ letExp "for_in_partial" $ BasicOp $ Index arr' $
                    DimSlice (intConst Int32 0) w (intConst Int32 1) : slice'
                  return (Just (p, for_in_partial), mempty)

            SubExpResult cs se
              | all (notIndex . stmExp) x_stms -> do
                  x_stms' <- collectStms_ $ certifying cs $ do
                    addStms x_stms
                    letBindNames_ [paramName p] $ BasicOp $ SubExp se
                  return (Nothing, x_stms')

            _ -> return (Just (p,arr), mempty)

        notIndex (BasicOp Index{}) = False
        notIndex _                 = True
simplifyLoopVariables _ _ _ _ = Skip

simplifKnownIterationLoop :: BinderOps lore => TopDownRuleDoLoop lore
simplifKnownIterationLoop _ pat _ (ctx, val, ForLoop i it (Constant iters) loop_vars, body)
  | zeroIsh iters = Simplify $ do
      let bindResult p r = letBindNames [patElemName p] $ BasicOp $ SubExp r
      zipWithM_ bindResult (patternContextElements pat) (map snd ctx)
      zipWithM_ bindResult (patternValueElements pat) (map snd val)

  | oneIsh iters = Simplify $ do

  forM_ (ctx++val) $ \(mergevar, mergeinit) ->
    letBindNames [paramName mergevar] $ BasicOp $ SubExp mergeinit

  letBindNames_ [i] $ BasicOp $ SubExp $ intConst it 0

  forM_ loop_vars $ \(p,arr) ->
    letBindNames_ [paramName p] $ BasicOp $ Index arr $
    DimFix (intConst Int32 0) : fullSlice (paramType p) []

  -- Some of the sizes in the types here might be temporarily wrong
  -- until copy propagation fixes it up.
  res <- bodyBind body
  forM_ (zip (patternNames pat) res) $ \(v, se) ->
    letBindNames_ [v] $ BasicOp $ SubExp se
simplifKnownIterationLoop _ _ _ _ =
  Skip

-- | Turn @copy(x)@ into @x@ iff @x@ is not used after this copy
-- statement and it can be consumed.
--
-- This simplistic rule is only valid before we introduce memory.
removeUnnecessaryCopy :: BinderOps lore => BottomUpRuleBasicOp lore
removeUnnecessaryCopy (vtable,used) (Pattern [] [d]) _ (Copy v)
  | not (v `UT.isConsumed` used),
    (not (v `UT.used` used) && consumable) || not (patElemName d `UT.isConsumed` used) =
      Simplify $ letBind_ (Pattern [] [d]) $ BasicOp $ SubExp $ Var v
  where -- We need to make sure we can even consume the original.
        -- This is currently a hacky check, much too conservative,
        -- because we don't have the information conveniently
        -- available.
        consumable = case M.lookup v $ ST.toScope vtable of
                       Just (FParamInfo info) -> unique $ declTypeOf info
                       _ -> False
removeUnnecessaryCopy _ _ _ _ = Skip

simplifyCmpOp :: SimpleRule lore
simplifyCmpOp _ _ (CmpOp cmp e1 e2)
  | e1 == e2 = constRes $ BoolValue $
               case cmp of CmpEq{}  -> True
                           CmpSlt{} -> False
                           CmpUlt{} -> False
                           CmpSle{} -> True
                           CmpUle{} -> True
                           FCmpLt{} -> False
                           FCmpLe{} -> True
                           CmpLlt -> False
                           CmpLle -> True
simplifyCmpOp _ _ (CmpOp cmp (Constant v1) (Constant v2)) =
  constRes =<< BoolValue <$> doCmpOp cmp v1 v2
simplifyCmpOp _ _ _ = Nothing

simplifyBinOp :: SimpleRule lore

simplifyBinOp _ _ (BinOp op (Constant v1) (Constant v2))
  | Just res <- doBinOp op v1 v2 =
      constRes res

simplifyBinOp _ _ (BinOp Add{} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp FAdd{} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1


simplifyBinOp look _ (BinOp Sub{} e1 e2)
  | isCt0 e2 = subExpRes e1
  -- Cases for simplifying (a+b)-b and permutations.
  | Var v1 <- e1,
    Just (BasicOp (BinOp Add{} e1_a e1_b), cs) <- look v1,
    e1_a == e2 = Just (SubExp e1_b, cs)
  | Var v1 <- e1,
    Just (BasicOp (BinOp Add{} e1_a e1_b), cs) <- look v1,
    e1_b == e2 = Just (SubExp e1_a, cs)
  | Var v2 <- e2,
    Just (BasicOp (BinOp Add{} e2_a e2_b), cs) <- look v2,
    e2_a == e1 = Just (SubExp e2_b, cs)
  | Var v2 <- e1,
    Just (BasicOp (BinOp Add{} e2_a e2_b), cs) <- look v2,
    e2_b == e1 = Just (SubExp e2_a, cs)

simplifyBinOp _ _ (BinOp FSub{} e1 e2)
  | isCt0 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp Mul{} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt0 e2 = subExpRes e2
  | isCt1 e1 = subExpRes e2
  | isCt1 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp FMul{} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt0 e2 = subExpRes e2
  | isCt1 e1 = subExpRes e2
  | isCt1 e2 = subExpRes e1

simplifyBinOp look _ (BinOp (SMod t) e1 e2)
  | isCt1 e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | e1 == e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | Var v1 <- e1,
    Just (BasicOp (BinOp SMod{} _ e4), v1_cs) <- look v1,
    e4 == e2 = Just (SubExp e1, v1_cs)

simplifyBinOp _ _ (BinOp SDiv{} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing

simplifyBinOp _ _ (BinOp FDiv{} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing

simplifyBinOp _ _ (BinOp (SRem t) e1 e2)
  | isCt1 e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | e1 == e2 = constRes $ IntValue $ intValue t (1 :: Int)

simplifyBinOp _ _ (BinOp SQuot{} e1 e2)
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing

simplifyBinOp _ _ (BinOp (FPow t) e1 e2)
  | isCt0 e2 = subExpRes $ floatConst t 1
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp (Shl t) e1 e2)
  | isCt0 e2 = subExpRes e1
  | isCt0 e1 = subExpRes $ intConst t 0

simplifyBinOp _ _ (BinOp AShr{} e1 e2)
  | isCt0 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp (And t) e1 e2)
  | isCt0 e1 = subExpRes $ intConst t 0
  | isCt0 e2 = subExpRes $ intConst t 0
  | e1 == e2 = subExpRes e1

simplifyBinOp _ _ (BinOp Or{} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  | e1 == e2 = subExpRes e1

simplifyBinOp _ _ (BinOp (Xor t) e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  | e1 == e2 = subExpRes $ intConst t 0

simplifyBinOp defOf _ (BinOp LogAnd e1 e2)
  | isCt0 e1 = constRes $ BoolValue False
  | isCt0 e2 = constRes $ BoolValue False
  | isCt1 e1 = subExpRes e2
  | isCt1 e2 = subExpRes e1
  | Var v <- e1,
    Just (BasicOp (UnOp Not e1'), v_cs) <- defOf v,
    e1' == e2 = Just (SubExp $ Constant $ BoolValue False, v_cs)
  | Var v <- e2,
    Just (BasicOp (UnOp Not e2'), v_cs) <- defOf v,
    e2' == e1 = Just (SubExp $ Constant $ BoolValue False, v_cs)

simplifyBinOp defOf _ (BinOp LogOr e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  | isCt1 e1 = constRes $ BoolValue True
  | isCt1 e2 = constRes $ BoolValue True
  | Var v <- e1,
    Just (BasicOp (UnOp Not e1'), v_cs) <- defOf v,
    e1' == e2 = Just (SubExp $ Constant $ BoolValue True, v_cs)
  | Var v <- e2,
    Just (BasicOp (UnOp Not e2'), v_cs) <- defOf v,
    e2' == e1 = Just (SubExp $ Constant $ BoolValue True, v_cs)

simplifyBinOp defOf _ (BinOp (SMax it) e1 e2)
  | e1 == e2 =
      subExpRes e1
  | Var v1 <- e1,
    Just (BasicOp (BinOp (SMax _) e1_1 e1_2), v1_cs) <- defOf v1,
    e1_1 == e2 =
      Just (BinOp (SMax it) e1_2 e2, v1_cs)
  | Var v1 <- e1,
    Just (BasicOp (BinOp (SMax _) e1_1 e1_2), v1_cs) <- defOf v1,
    e1_2 == e2 =
      Just (BinOp (SMax it) e1_1 e2, v1_cs)
  | Var v2 <- e2,
    Just (BasicOp (BinOp (SMax _) e2_1 e2_2), v2_cs) <- defOf v2,
    e2_1 == e1 =
      Just (BinOp (SMax it) e2_2 e1, v2_cs)
  | Var v2 <- e2,
    Just (BasicOp (BinOp (SMax _) e2_1 e2_2), v2_cs) <- defOf v2,
    e2_2 == e1 =
      Just (BinOp (SMax it) e2_1 e1, v2_cs)

simplifyBinOp _ _ _ = Nothing

constRes :: PrimValue -> Maybe (BasicOp lore, Certificates)
constRes = Just . (,mempty) . SubExp . Constant

subExpRes :: SubExp -> Maybe (BasicOp lore, Certificates)
subExpRes = Just . (,mempty) . SubExp

simplifyUnOp :: SimpleRule lore
simplifyUnOp _ _ (UnOp op (Constant v)) =
  constRes =<< doUnOp op v
simplifyUnOp defOf _ (UnOp Not (Var v))
  | Just (BasicOp (UnOp Not v2), v_cs) <- defOf v =
      Just (SubExp v2, v_cs)
simplifyUnOp _ _ _ =
  Nothing

simplifyConvOp :: SimpleRule lore
simplifyConvOp _ _ (ConvOp op (Constant v)) =
  constRes =<< doConvOp op v
simplifyConvOp _ _ (ConvOp op se)
  | (from, to) <- convOpType op, from == to =
  subExpRes se
simplifyConvOp lookupVar _ (ConvOp (SExt t2 t1) (Var v))
  | Just (BasicOp (ConvOp (SExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (SExt t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (ZExt t2 t1) (Var v))
  | Just (BasicOp (ConvOp (ZExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (ZExt t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (SIToFP t2 t1) (Var v))
  | Just (BasicOp (ConvOp (SExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (SIToFP t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (UIToFP t2 t1) (Var v))
  | Just (BasicOp (ConvOp (ZExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (UIToFP t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (FPConv t2 t1) (Var v))
  | Just (BasicOp (ConvOp (FPConv t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (FPConv t3 t1) se, v_cs)
simplifyConvOp _ _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: SimpleRule lore
simplifyAssert _ _ (Assert (Constant (BoolValue True)) _ _) =
  constRes Checked
simplifyAssert _ _ _ =
  Nothing

constantFoldPrimFun :: BinderOps lore => TopDownRuleGeneric lore
constantFoldPrimFun _ (Let pat (StmAux cs _) (Apply fname args _ _))
  | Just args' <- mapM (isConst . fst) args,
    Just (_, _, fun) <- M.lookup (nameToString fname) primFuns,
    Just result <- fun args' =
      Simplify $ certifying cs $ letBind_ pat $ BasicOp $ SubExp $ Constant result
  where isConst (Constant v) = Just v
        isConst _ = Nothing
constantFoldPrimFun _ _ = Skip

simplifyIndex :: BinderOps lore => BottomUpRuleBasicOp lore
simplifyIndex (vtable, used) pat@(Pattern [] [pe]) (StmAux cs _) (Index idd inds)
  | Just m <- simplifyIndexing vtable seType idd inds consumed = Simplify $ do
      res <- m
      case res of
        SubExpResult cs' se ->
          certifying (cs<>cs') $ letBindNames_ (patternNames pat) $
          BasicOp $ SubExp se
        IndexResult extra_cs idd' inds' ->
          certifying (cs<>extra_cs) $
          letBindNames_ (patternNames pat) $ BasicOp $ Index idd' inds'
  where consumed = patElemName pe `UT.isConsumed` used
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v

simplifyIndex _ _ _ _ = Skip

data IndexResult = IndexResult Certificates VName (Slice SubExp)
                 | SubExpResult Certificates SubExp

simplifyIndexing :: MonadBinder m =>
                    ST.SymbolTable (Lore m) -> TypeLookup
                 -> VName -> Slice SubExp -> Bool
                 -> Maybe (m IndexResult)
simplifyIndexing vtable seType idd inds consuming =
  case defOf idd of
    _ | Just t <- seType (Var idd),
        inds == fullSlice t [] ->
          Just $ pure $ SubExpResult mempty $ Var idd

      | Just inds' <- sliceIndices inds,
        Just (e, cs) <- ST.index idd inds' vtable,
        worthInlining e,
        all (`ST.elem` vtable) (unCertificates cs) ->
        Just $ SubExpResult cs <$> (letSubExp "index_primexp" =<< toExp e)

    Nothing -> Nothing

    Just (SubExp (Var v), cs) -> Just $ pure $ IndexResult cs v inds

    Just (Iota _ x s to_it, cs)
      | [DimFix ii] <- inds,
        Just (Prim (IntType from_it)) <- seType ii ->
          Just $
          fmap (SubExpResult cs) $ letSubExp "index_iota" <=< toExp $
          ConvOpExp (SExt from_it to_it) (primExpFromSubExp (IntType from_it) ii)
          * primExpFromSubExp (IntType to_it) s
          + primExpFromSubExp (IntType to_it) x
      | [DimSlice i_offset i_n i_stride] <- inds ->
          Just $ do
            i_offset' <- asIntS to_it i_offset
            i_stride' <- asIntS to_it i_stride
            i_offset'' <- letSubExp "iota_offset" $
                          BasicOp $ BinOp (Add Int32) x i_offset'
            i_stride'' <- letSubExp "iota_offset" $
                          BasicOp $ BinOp (Mul Int32) s i_stride'
            fmap (SubExpResult cs) $ letSubExp "slice_iota" $
              BasicOp $ Iota i_n i_offset'' i_stride'' to_it

    Just (Rotate offsets a, cs) -> Just $ do
      dims <- arrayDims <$> lookupType a
      let adjustI i o d = do
            i_p_o <- letSubExp "i_p_o" $ BasicOp $ BinOp (Add Int32) i o
            letSubExp "rot_i" (BasicOp $ BinOp (SMod Int32) i_p_o d)
          adjust (DimFix i, o, d) =
            DimFix <$> adjustI i o d
          adjust (DimSlice i n s, o, d) =
            DimSlice <$> adjustI i o d <*> pure n <*> pure s
      IndexResult cs a <$> mapM adjust (zip3 inds offsets dims)

    Just (Index aa ais, cs) ->
      Just $ IndexResult cs aa <$> sliceSlice ais inds

    Just (Replicate (Shape [_]) (Var vv), cs)
      | [DimFix{}]   <- inds, not consuming -> Just $ pure $ SubExpResult cs $ Var vv
      | DimFix{}:is' <- inds, not consuming -> Just $ pure $ IndexResult cs vv is'

    Just (Replicate (Shape [_]) val@(Constant _), cs)
      | [DimFix{}] <- inds, not consuming -> Just $ pure $ SubExpResult cs val

    Just (Replicate (Shape ds) v, cs)
      | (ds_inds, rest_inds) <- splitAt (length ds) inds,
        (ds', ds_inds') <- unzip $ mapMaybe index ds_inds,
        ds' /= ds ->
        Just $ do
          arr <- letExp "smaller_replicate" $ BasicOp $ Replicate (Shape ds') v
          return $ IndexResult cs arr $ ds_inds' ++ rest_inds
      where index DimFix{} = Nothing
            index (DimSlice _ n s) = Just (n, DimSlice (constant (0::Int32)) n s)

    Just (Rearrange perm src, cs)
       | rearrangeReach perm <= length (takeWhile isIndex inds) ->
         let inds' = rearrangeShape (rearrangeInverse perm) inds
         in Just $ pure $ IndexResult cs src inds'
      where isIndex DimFix{} = True
            isIndex _          = False

    Just (Copy src, cs)
      | Just dims <- arrayDims <$> seType (Var src),
        length inds == length dims,
        not consuming, ST.available src vtable ->
          Just $ pure $ IndexResult cs src inds

    Just (Reshape newshape src, cs)
      | Just newdims <- shapeCoercion newshape,
        Just olddims <- arrayDims <$> seType (Var src),
        changed_dims <- zipWith (/=) newdims olddims,
        not $ or $ drop (length inds) changed_dims ->
        Just $ pure $ IndexResult cs src inds

      | Just newdims <- shapeCoercion newshape,
        Just olddims <- arrayDims <$> seType (Var src),
        length newshape == length inds,
        length olddims == length newdims ->
        Just $ pure $ IndexResult cs src inds

    Just (Reshape [_] v2, cs)
      | Just [_] <- arrayDims <$> seType (Var v2) ->
        Just $ pure $ IndexResult cs v2 inds

    Just (Concat d x xs _, cs)
      | Just (ibef, DimFix i, iaft) <- focusNth d inds,
        Just (Prim res_t) <- (`setArrayDims` sliceDims inds) <$>
                             ST.lookupType x vtable -> Just $ do
      x_len <- arraySize d <$> lookupType x
      xs_lens <- mapM (fmap (arraySize d) . lookupType) xs

      let add n m = do
            added <- letSubExp "index_concat_add" $ BasicOp $ BinOp (Add Int32) n m
            return (added, n)
      (_, starts) <- mapAccumLM add x_len xs_lens
      let xs_and_starts = reverse $ zip xs starts

      let mkBranch [] =
            letSubExp "index_concat" $ BasicOp $ Index x $ ibef ++ DimFix i : iaft
          mkBranch ((x', start):xs_and_starts') = do
            cmp <- letSubExp "index_concat_cmp" $ BasicOp $ CmpOp (CmpSle Int32) start i
            (thisres, thisbnds) <- collectStms $ do
              i' <- letSubExp "index_concat_i" $ BasicOp $ BinOp (Sub Int32) i start
              letSubExp "index_concat" $ BasicOp $ Index x' $ ibef ++ DimFix i' : iaft
            thisbody <- mkBodyM thisbnds [thisres]
            (altres, altbnds) <- collectStms $ mkBranch xs_and_starts'
            altbody <- mkBodyM altbnds [altres]
            letSubExp "index_concat_branch" $ If cmp thisbody altbody $
              IfAttr [primBodyType res_t] IfNormal
      SubExpResult cs <$> mkBranch xs_and_starts

    Just (ArrayLit ses _, cs)
      | DimFix (Constant (IntValue (Int32Value i))) : inds' <- inds,
        Just se <- maybeNth i ses ->
        case inds' of
          [] -> Just $ pure $ SubExpResult cs se
          _ | Var v2 <- se  -> Just $ pure $ IndexResult cs v2 inds'
          _ -> Nothing

    -- Indexing single-element arrays.  We know the index must be 0.
    _ | Just t <- seType $ Var idd, isCt1 $ arraySize 0 t,
        DimFix i : inds' <- inds, not $ isCt0 i ->
          Just $ pure $ IndexResult mempty idd $
          DimFix (constant (0::Int32)) : inds'

    _ -> Nothing

    where defOf v = do (BasicOp op, def_cs) <- ST.lookupExp v vtable
                       return (op, def_cs)

          -- | A crude heuristic for determining when a PrimExp is
          -- worth inlining over keeping it in an array and reading it
          -- from memory.
          worthInlining e
            | length e > 10 = False -- totally ad-hoc.
          worthInlining (BinOpExp Pow{} _ _) = False
          worthInlining (BinOpExp FPow{} _ _) = False
          worthInlining (BinOpExp _ x y) = worthInlining x && worthInlining y
          worthInlining (CmpOpExp _ x y) = worthInlining x && worthInlining y
          worthInlining (ConvOpExp _ x) = worthInlining x
          worthInlining (UnOpExp _ x) = worthInlining x
          worthInlining FunExp{} = False
          worthInlining _ = True

sliceSlice :: MonadBinder m =>
              [DimIndex SubExp] -> [DimIndex SubExp] -> m [DimIndex SubExp]
sliceSlice (DimFix j:js') is' = (DimFix j:) <$> sliceSlice js' is'
sliceSlice (DimSlice j _ s:js') (DimFix i:is') = do
  i_t_s <- letSubExp "j_t_s" $ BasicOp $ BinOp (Mul Int32) i s
  j_p_i_t_s <- letSubExp "j_p_i_t_s" $ BasicOp $ BinOp (Add Int32) j i_t_s
  (DimFix j_p_i_t_s:) <$> sliceSlice js' is'
sliceSlice (DimSlice j _ s0:js') (DimSlice i n s1:is') = do
  s0_t_i <- letSubExp "s0_t_i" $ BasicOp $ BinOp (Mul Int32) s0 i
  j_p_s0_t_i <- letSubExp "j_p_s0_t_i" $ BasicOp $ BinOp (Add Int32) j s0_t_i
  (DimSlice j_p_s0_t_i n s1:) <$> sliceSlice js' is'
sliceSlice _ _ = return []


simplifyConcat :: BinderOps lore => BottomUpRuleBasicOp lore

-- concat@1(transpose(x),transpose(y)) == transpose(concat@0(x,y))
simplifyConcat (vtable, _) pat _ (Concat i x xs new_d)
  | Just r <- arrayRank <$> ST.lookupType x vtable,
    let perm = [i] ++ [0..i-1] ++ [i+1..r-1],
    Just (x',x_cs) <- transposedBy perm x,
    Just (xs',xs_cs) <- unzip <$> mapM (transposedBy perm) xs = Simplify $ do
      concat_rearrange <-
        certifying (x_cs<>mconcat xs_cs) $
        letExp "concat_rearrange" $ BasicOp $ Concat 0 x' xs' new_d
      letBind_ pat $ BasicOp $ Rearrange perm concat_rearrange
  where transposedBy perm1 v =
          case ST.lookupExp v vtable of
            Just (BasicOp (Rearrange perm2 v'), vcs)
              | perm1 == perm2 -> Just (v', vcs)
            _ -> Nothing

-- concat xs (concat ys zs) == concat xs ys zs
simplifyConcat (vtable, _) pat (StmAux cs _) (Concat i x xs new_d)
  | x' /= x || concat xs' /= xs = Simplify $
      certifying (cs<>x_cs<>mconcat xs_cs) $
      letBind_ pat $ BasicOp $ Concat i x' (zs++concat xs') new_d
  where (x':zs, x_cs) = isConcat x
        (xs', xs_cs) = unzip $ map isConcat xs
        isConcat v = case ST.lookupBasicOp v vtable of
                       Just (Concat j y ys _, v_cs) | j == i -> (y : ys, v_cs)
                       _ -> ([v], mempty)

-- If concatenating a bunch of array literals (or equivalent
-- replicate), just construct the array literal instead.
simplifyConcat (vtable, _) pat (StmAux cs _) (Concat 0 x xs _)
  | Just (vs, vcs) <- unzip <$> mapM isArrayLit (x:xs) = Simplify $ do
      rt <- rowType <$> lookupType x
      certifying (cs <> mconcat vcs) $
        letBind_ pat $ BasicOp $ ArrayLit vs rt
      where isArrayLit v
              | Just (Replicate shape se, vcs) <- ST.lookupBasicOp v vtable,
                unitShape shape = Just (se, vcs)
              | Just (ArrayLit [se] _, vcs) <- ST.lookupBasicOp v vtable =
                  Just (se, vcs)
              | otherwise =
                  Nothing

            unitShape = (==Shape [Constant $ IntValue $ Int32Value 1])

simplifyConcat _ _ _  _ = Skip

ruleIf :: BinderOps lore => TopDownRuleIf lore

ruleIf _ pat _ (e1, tb, fb, IfAttr _ ifsort)
  | Just branch <- checkBranch,
    ifsort /= IfFallback || isCt1 e1 = Simplify $ do
  let ses = bodyResult branch
  addStms $ bodyStms branch
  sequence_ [ letBind (Pattern [] [p]) $ BasicOp $ SubExp se
            | (p,se) <- zip (patternElements pat) ses]

  where checkBranch
          | isCt1 e1  = Just tb
          | isCt0 e1  = Just fb
          | otherwise = Nothing

-- IMPROVE: the following two rules can be generalised to work in more
-- cases, especially when the branches have bindings, or return more
-- than one value.
--
-- if c then True else v == c || v
ruleIf _ pat _
  (cond, Body _ tstms [Constant (BoolValue True)],
         Body _ fstms [se], IfAttr ts _)
  | null tstms, null fstms, [Prim Bool] <- bodyTypeValues ts =
      Simplify $ letBind_ pat $ BasicOp $ BinOp LogOr cond se

-- When type(x)==bool, if c then x else y == (c && x) || (!c && y)
ruleIf _ pat _ (cond, tb, fb, IfAttr ts _)
  | Body _ tstms [tres] <- tb,
    Body _ fstms [fres] <- fb,
    all (safeExp . stmExp) $ tstms <> fstms,
    all (==Prim Bool) $ bodyTypeValues ts = Simplify $ do
  addStms tstms
  addStms fstms
  e <- eBinOp LogOr (pure $ BasicOp $ BinOp LogAnd cond tres)
                    (eBinOp LogAnd (pure $ BasicOp $ UnOp Not cond)
                     (pure $ BasicOp $ SubExp fres))
  letBind_ pat e

ruleIf _ pat _ (_, tbranch, _, IfAttr _ IfFallback)
  | null $ patternContextNames pat,
    all (safeExp . stmExp) $ bodyStms tbranch = Simplify $ do
      let ses = bodyResult tbranch
      addStms $ bodyStms tbranch
      sequence_ [ letBind (Pattern [] [p]) $ BasicOp $ SubExp se
                | (p,se) <- zip (patternElements pat) ses]

ruleIf _ _ _ _ = Skip

-- | Move out results of a conditional expression whose computation is
-- either invariant to the branches (only done for results in the
-- context), or the same in both branches.
hoistBranchInvariant :: BinderOps lore => TopDownRuleIf lore
hoistBranchInvariant _ pat _ (cond, tb, fb, IfAttr ret ifsort) = Simplify $ do
  let tses = bodyResult tb
      fses = bodyResult fb
  (hoistings, (pes, ts, res)) <-
    fmap (fmap unzip3 . partitionEithers) $ mapM branchInvariant $
      zip3 (patternElements pat)
           (map Left [0..num_ctx-1] ++ map Right ret)
           (zip tses fses)
  let ctx_fixes = catMaybes hoistings
      (tses', fses') = unzip res
      tb' = tb { bodyResult = tses' }
      fb' = fb { bodyResult = fses' }
      ret' = foldr (uncurry fixExt) (rights ts) ctx_fixes
      (ctx_pes, val_pes) = splitFromEnd (length ret') pes
  if not $ null hoistings -- Was something hoisted?
     then do -- We may have to add some reshapes if we made the type
             -- less existential.
             tb'' <- reshapeBodyResults tb' $ map extTypeOf ret'
             fb'' <- reshapeBodyResults fb' $ map extTypeOf ret'
             letBind_ (Pattern ctx_pes val_pes) $
               If cond tb'' fb'' (IfAttr ret' ifsort)
     else cannotSimplify
  where num_ctx = length $ patternContextElements pat
        bound_in_branches = namesFromList $ concatMap (patternNames . stmPattern) $
                            bodyStms tb <> bodyStms fb
        mem_sizes = freeIn $ filter (isMem . patElemType) $ patternElements pat
        invariant Constant{} = True
        invariant (Var v) = not $ v `nameIn` bound_in_branches

        isMem Mem{} = True
        isMem _ = False
        sizeOfMem v = v `nameIn` mem_sizes

        branchInvariant (pe, t, (tse, fse))
          -- Do both branches return the same value?
          | tse == fse = do
              letBind_ (Pattern [] [pe]) $ BasicOp $ SubExp tse
              hoisted pe t

          -- Do both branches return values that are free in the
          -- branch, and are we not the only pattern element?  The
          -- latter is to avoid infinite application of this rule.
          | invariant tse, invariant fse, patternSize pat > 1,
            Prim _ <- patElemType pe, not $ sizeOfMem $ patElemName pe = do
              bt <- expTypesFromPattern $ Pattern [] [pe]
              letBind_ (Pattern [] [pe]) =<<
                (If cond <$> resultBodyM [tse]
                         <*> resultBodyM [fse]
                         <*> pure (IfAttr bt ifsort))
              hoisted pe t

          | otherwise =
              return $ Right (pe, t, (tse,fse))

        hoisted pe (Left i) = return $ Left $ Just (i, Var $ patElemName pe)
        hoisted _ Right{}   = return $ Left Nothing

        reshapeBodyResults body rets = insertStmsM $ do
          ses <- bodyBind body
          let (ctx_ses, val_ses) = splitFromEnd (length rets) ses
          resultBodyM . (ctx_ses++) =<< zipWithM reshapeResult val_ses rets
        reshapeResult (Var v) t@Array{} = do
          v_t <- lookupType v
          let newshape = arrayDims $ removeExistentials t v_t
          if newshape /= arrayDims v_t
            then letSubExp "branch_ctx_reshaped" $ shapeCoerce newshape v
            else return $ Var v
        reshapeResult se _ =
          return se

simplifyIdentityReshape :: SimpleRule lore
simplifyIdentityReshape _ seType (Reshape newshape v)
  | Just t <- seType $ Var v,
    newDims newshape == arrayDims t = -- No-op reshape.
      subExpRes $ Var v
simplifyIdentityReshape _ _ _ = Nothing

simplifyReshapeReshape :: SimpleRule lore
simplifyReshapeReshape defOf _ (Reshape newshape v)
  | Just (BasicOp (Reshape oldshape v2), v_cs) <- defOf v =
    Just (Reshape (fuseReshape oldshape newshape) v2, v_cs)
simplifyReshapeReshape _ _ _ = Nothing

simplifyReshapeScratch :: SimpleRule lore
simplifyReshapeScratch defOf _ (Reshape newshape v)
  | Just (BasicOp (Scratch bt _), v_cs) <- defOf v =
    Just (Scratch bt $ newDims newshape, v_cs)
simplifyReshapeScratch _ _ _ = Nothing

simplifyReshapeReplicate :: SimpleRule lore
simplifyReshapeReplicate defOf seType (Reshape newshape v)
  | Just (BasicOp (Replicate _ se), v_cs) <- defOf v,
    Just oldshape <- arrayShape <$> seType se,
    shapeDims oldshape `isSuffixOf` newDims newshape =
      let new = take (length newshape - shapeRank oldshape) $
                newDims newshape
      in Just (Replicate (Shape new) se, v_cs)
simplifyReshapeReplicate _ _ _ = Nothing

simplifyReshapeIota :: SimpleRule lore
simplifyReshapeIota defOf _ (Reshape newshape v)
  | Just (BasicOp (Iota _ offset stride it), v_cs) <- defOf v,
    [n] <- newDims newshape =
      Just (Iota n offset stride it, v_cs)
simplifyReshapeIota _ _ _ = Nothing

improveReshape :: SimpleRule lore
improveReshape _ seType (Reshape newshape v)
  | Just t <- seType $ Var v,
    newshape' <- informReshape (arrayDims t) newshape,
    newshape' /= newshape =
      Just (Reshape newshape' v, mempty)
improveReshape _ _ _ = Nothing

-- | If we are copying a scratch array (possibly indirectly), just turn it into a scratch by
-- itself.
copyScratchToScratch :: SimpleRule lore
copyScratchToScratch defOf seType (Copy src) = do
  t <- seType $ Var src
  if isActuallyScratch src then
    Just (Scratch (elemType t) (arrayDims t), mempty)
    else Nothing
  where isActuallyScratch v =
          case asBasicOp . fst =<< defOf v of
            Just Scratch{} -> True
            Just (Rearrange _ v') -> isActuallyScratch v'
            Just (Reshape _ v') -> isActuallyScratch v'
            _ -> False
copyScratchToScratch _ _ _ =
  Nothing

ruleBasicOp :: BinderOps lore => TopDownRuleBasicOp lore

-- Check all the simpleRules.
ruleBasicOp vtable pat aux op
  | Just (op', cs) <- msum [ rule defOf seType op | rule <- simpleRules ] =
      Simplify $ certifying (cs <> stmAuxCerts aux) $ letBind_ pat $ BasicOp op'
  where defOf = (`ST.lookupExp` vtable)
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v

ruleBasicOp vtable pat _ (Update src _ (Var v))
  | Just (BasicOp Scratch{}, _) <- ST.lookupExp v vtable =
      Simplify $ letBind_ pat $ BasicOp $ SubExp $ Var src

ruleBasicOp vtable pat _ (Update dest destis (Var v))
  | Just (e, _) <- ST.lookupExp v vtable,
    arrayFrom e =
      Simplify $ letBind_ pat $ BasicOp $ SubExp $ Var dest
  where arrayFrom (BasicOp (Copy copy_v))
          | Just (e',_) <- ST.lookupExp copy_v vtable =
              arrayFrom e'
        arrayFrom (BasicOp (Index src srcis)) =
          src == dest && destis == srcis
        arrayFrom (BasicOp (Replicate v_shape v_se))
          | Just (Replicate dest_shape dest_se, _) <- ST.lookupBasicOp dest vtable,
            v_se == dest_se,
            shapeDims v_shape `isSuffixOf` shapeDims dest_shape =
              True
        arrayFrom _ =
          False

-- | Turn in-place updates that replace an entire array into just
-- array literals.
ruleBasicOp vtable pat _ (Update dest is se)
  | Just dest_t <- ST.lookupType dest vtable,
    isFullSlice (arrayShape dest_t) is =
      Simplify $ letBind_ pat $ BasicOp $
      case se of
        Var v | not $ null $ sliceDims is ->
                  Reshape (map DimNew $ arrayDims dest_t) v
        _ -> ArrayLit [se] $ rowType dest_t

-- | Simplify a chain of in-place updates and copies.  This chain is
-- often produced by in-place lowering.
ruleBasicOp vtable pat (StmAux cs1 _) (Update dest1 is1 (Var v1))
  | Just (Update dest2 is2 se2, cs2) <- ST.lookupBasicOp v1 vtable,
    Just (Copy v3, cs3) <- ST.lookupBasicOp dest2 vtable,
    Just (Index v4 is4, cs4) <- ST.lookupBasicOp v3 vtable,
    is4 == is1, v4 == dest1 =
      Simplify $ certifying (cs1 <> cs2 <> cs3 <> cs4) $ do
      is5 <- sliceSlice is1 is2
      letBind_ pat $ BasicOp $ Update dest1 is5 se2

-- | If we are comparing X against the result of a branch of the form
-- @if P then Y else Z@ then replace comparison with '(P && X == Y) ||
-- (!P && X == Z').  This may allow us to get rid of a branch, and the
-- extra comparisons may be constant-folded out.  Question: maybe we
-- should have some more checks to ensure that we only do this if that
-- is actually the case, such as if we will obtain at least one
-- constant-to-constant comparison?
ruleBasicOp vtable pat _ (CmpOp (CmpEq t) se1 se2)
  | Just m <- simplifyWith se1 se2 = Simplify m
  | Just m <- simplifyWith se2 se1 = Simplify m
  where simplifyWith (Var v) x
          | Just bnd <- ST.entryStm =<< ST.lookup v vtable,
            If p tbranch fbranch _ <- stmExp bnd,
            Just (y, z) <-
              returns v (stmPattern bnd) tbranch fbranch,
            not $ boundInBody tbranch `namesIntersect` freeIn y,
            not $ boundInBody fbranch `namesIntersect` freeIn z = Just $ do
                eq_x_y <-
                  letSubExp "eq_x_y" $ BasicOp $ CmpOp (CmpEq t) x y
                eq_x_z <-
                  letSubExp "eq_x_z" $ BasicOp $ CmpOp (CmpEq t) x z
                p_and_eq_x_y <-
                  letSubExp "p_and_eq_x_y" $ BasicOp $ BinOp LogAnd p eq_x_y
                not_p <-
                  letSubExp "not_p" $ BasicOp $ UnOp Not p
                not_p_and_eq_x_z <-
                  letSubExp "p_and_eq_x_y" $ BasicOp $ BinOp LogAnd not_p eq_x_z
                letBind_ pat $
                  BasicOp $ BinOp LogOr p_and_eq_x_y not_p_and_eq_x_z
        simplifyWith _ _ =
          Nothing

        returns v ifpat tbranch fbranch =
          fmap snd $
          find ((==v) . patElemName . fst) $
          zip (patternValueElements ifpat) $
          zip (bodyResult tbranch) (bodyResult fbranch)

ruleBasicOp _ pat _ (Replicate (Shape []) se@Constant{}) =
  Simplify $ letBind_ pat $ BasicOp $ SubExp se
ruleBasicOp _ pat _ (Replicate (Shape []) (Var v)) = Simplify $ do
  v_t <- lookupType v
  letBind_ pat $ BasicOp $ if primType v_t
                           then SubExp $ Var v
                           else Copy v
ruleBasicOp vtable pat _  (Replicate shape (Var v))
  | Just (BasicOp (Replicate shape2 se), cs) <- ST.lookupExp v vtable =
      Simplify $ certifying cs $ letBind_ pat $ BasicOp $ Replicate (shape<>shape2) se

-- | Turn array literals with identical elements into replicates.
ruleBasicOp _ pat _ (ArrayLit (se:ses) _)
  | all (==se) ses =
    Simplify $ let n = constant (fromIntegral (length ses) + 1 :: Int32)
               in letBind_ pat $ BasicOp $ Replicate (Shape [n]) se

ruleBasicOp vtable pat (StmAux cs _) (Index idd slice)
  | Just inds <- sliceIndices slice,
    Just (BasicOp (Reshape newshape idd2), idd_cs) <- ST.lookupExp idd vtable,
    length newshape == length inds =
      Simplify $
      case shapeCoercion newshape of
        Just _ ->
          certifying (cs<>idd_cs) $
            letBind_ pat $ BasicOp $ Index idd2 slice
        Nothing -> do
          -- Linearise indices and map to old index space.
          oldshape <- arrayDims <$> lookupType idd2
          let new_inds =
                reshapeIndex (map (primExpFromSubExp int32) oldshape)
                             (map (primExpFromSubExp int32) $ newDims newshape)
                             (map (primExpFromSubExp int32) inds)
          new_inds' <-
            mapM (letSubExp "new_index" <=< toExp . asInt32PrimExp) new_inds
          certifying (cs<>idd_cs) $
            letBind_ pat $ BasicOp $ Index idd2 $ map DimFix new_inds'

ruleBasicOp _ pat _ (BinOp (Pow t) e1 e2)
  | e1 == intConst t 2 =
      Simplify $ letBind_ pat $ BasicOp $ BinOp (Shl t) (intConst t 1) e2

-- Handle identity permutation.
ruleBasicOp _ pat _ (Rearrange perm v)
  | sort perm == perm =
      Simplify $ letBind_ pat $ BasicOp $ SubExp $ Var v

ruleBasicOp vtable pat (StmAux cs _) (Rearrange perm v)
  | Just (BasicOp (Rearrange perm2 e), v_cs) <- ST.lookupExp v vtable =
      -- Rearranging a rearranging: compose the permutations.
      Simplify $ certifying (cs<>v_cs) $
      letBind_ pat $ BasicOp $ Rearrange (perm `rearrangeCompose` perm2) e

ruleBasicOp vtable pat (StmAux cs _) (Rearrange perm v)
  | Just (BasicOp (Rotate offsets v2), v_cs) <- ST.lookupExp v vtable,
    Just (BasicOp (Rearrange perm3 v3), v2_cs) <- ST.lookupExp v2 vtable = Simplify $ do
      let offsets' = rearrangeShape (rearrangeInverse perm3) offsets
      rearrange_rotate <- letExp "rearrange_rotate" $ BasicOp $ Rotate offsets' v3
      certifying (cs<>v_cs<>v2_cs) $
        letBind_ pat $ BasicOp $ Rearrange (perm `rearrangeCompose` perm3) rearrange_rotate

-- Rearranging a replicate where the outer dimension is left untouched.
ruleBasicOp vtable pat (StmAux cs _) (Rearrange perm v1)
  | Just (BasicOp (Replicate dims (Var v2)), v1_cs) <- ST.lookupExp v1 vtable,
    num_dims <- shapeRank dims,
    (rep_perm, rest_perm) <- splitAt num_dims perm,
    not $ null rest_perm,
    rep_perm == [0..length rep_perm-1] = Simplify $ certifying (cs<>v1_cs) $ do
      v <- letSubExp "rearrange_replicate" $
           BasicOp $ Rearrange (map (subtract num_dims) rest_perm) v2
      letBind_ pat $ BasicOp $ Replicate dims v

-- A zero-rotation is identity.
ruleBasicOp _ pat _ (Rotate offsets v)
  | all isCt0 offsets = Simplify $ letBind_ pat $ BasicOp $ SubExp $ Var v

ruleBasicOp vtable pat (StmAux cs _) (Rotate offsets v)
  | Just (BasicOp (Rearrange perm v2), v_cs) <- ST.lookupExp v vtable,
    Just (BasicOp (Rotate offsets2 v3), v2_cs) <- ST.lookupExp v2 vtable = Simplify $ do
      let offsets2' = rearrangeShape (rearrangeInverse perm) offsets2
          addOffsets x y = letSubExp "summed_offset" $ BasicOp $ BinOp (Add Int32) x y
      offsets' <- zipWithM addOffsets offsets offsets2'
      rotate_rearrange <-
        certifying cs $ letExp "rotate_rearrange" $ BasicOp $ Rearrange perm v3
      certifying (v_cs <> v2_cs) $
        letBind_ pat $ BasicOp $ Rotate offsets' rotate_rearrange

-- Combining Rotates.
ruleBasicOp vtable pat (StmAux cs _) (Rotate offsets1 v)
  | Just (BasicOp (Rotate offsets2 v2), v_cs) <- ST.lookupExp v vtable = Simplify $ do
      offsets <- zipWithM add offsets1 offsets2
      certifying (cs<>v_cs) $
        letBind_ pat $ BasicOp $ Rotate offsets v2
        where add x y = letSubExp "offset" $ BasicOp $ BinOp (Add Int32) x y

-- If we see an Update with a scalar where the value to be written is
-- the result of indexing some other array, then we convert it into an
-- Update with a slice of that array.  This matters when the arrays
-- are far away (on the GPU, say), because it avoids a copy of the
-- scalar to and from the host.
ruleBasicOp vtable pat (StmAux cs_x _) (Update arr_x slice_x (Var v))
  | Just _ <- sliceIndices slice_x,
    Just (Index arr_y slice_y, cs_y) <- ST.lookupBasicOp v vtable,
    ST.available arr_y vtable,
    -- XXX: we should check for proper aliasing here instead.
    arr_y /= arr_x,
    Just (slice_x_bef, DimFix i, []) <- focusNth (length slice_x - 1) slice_x,
    Just (slice_y_bef, DimFix j, []) <- focusNth (length slice_y - 1) slice_y = Simplify $ do
      let slice_x' = slice_x_bef ++ [DimSlice i (intConst Int32 1) (intConst Int32 1)]
          slice_y' = slice_y_bef ++ [DimSlice j (intConst Int32 1) (intConst Int32 1)]
      v' <- letExp (baseString v ++ "_slice") $ BasicOp $ Index arr_y slice_y'
      certifying (cs_x <> cs_y) $
        letBind_ pat $ BasicOp $ Update arr_x slice_x' $ Var v'

ruleBasicOp _ _ _ _ =
  Skip

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: BinderOps lore => BottomUpRuleIf lore
removeDeadBranchResult (_, used) pat _ (e1, tb, fb, IfAttr rettype ifsort)
  | -- Only if there is no existential context...
    patternSize pat == length rettype,
    -- Figure out which of the names in 'pat' are used...
    patused <- map (`UT.isUsedDirectly` used) $ patternNames pat,
    -- If they are not all used, then this rule applies.
    not (and patused) =
  -- Remove the parts of the branch-results that correspond to dead
  -- return value bindings.  Note that this leaves dead code in the
  -- branch bodies, but that will be removed later.
  let tses = bodyResult tb
      fses = bodyResult fb
      pick :: [a] -> [a]
      pick = map snd . filter fst . zip patused
      tb' = tb { bodyResult = pick tses }
      fb' = fb { bodyResult = pick fses }
      pat' = pick $ patternElements pat
      rettype' = pick rettype
  in Simplify $ letBind_ (Pattern [] pat') $ If e1 tb' fb' $ IfAttr rettype' ifsort

  | otherwise = Skip


-- Some helper functions

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False
