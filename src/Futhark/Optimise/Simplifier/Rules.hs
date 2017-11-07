{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module defines a collection of simplification rules, as per
-- "Futhark.Optimise.Simplifier.Rule".  They are used in the
-- simplifier.
module Futhark.Optimise.Simplifier.Rules
  ( standardRules
  )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Foldable (all)
import Data.List hiding (all)
import Data.Maybe
import Data.Monoid

import qualified Data.Map.Strict as M
import qualified Data.Set      as S

import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.DataDependencies
import Futhark.Optimise.Simplifier.ClosedForm
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.RuleM
import qualified Futhark.Analysis.AlgSimplify as AS
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Construct
import Futhark.Transform.Substitute
import Futhark.Util

import Prelude hiding (all)

topDownRules :: (MonadBinder m, Aliased (Lore m)) => TopDownRules m
topDownRules = [ hoistLoopInvariantMergeVariables
               , simplifyClosedFormLoop
               , simplifKnownIterationLoop
               , simplifyLoopVariables
               , simplifyRearrange
               , simplifyRotate
               , letRule simplifyBinOp
               , letRule simplifyCmpOp
               , letRule simplifyUnOp
               , letRule simplifyConvOp
               , letRule simplifyAssert
               , letRule copyScratchToScratch
               , constantFoldPrimFun
               , simplifyIndexIntoReshape
               , removeEmptySplits
               , removeSingletonSplits
               , evaluateBranch
               , simplifyBoolBranch
               , hoistBranchInvariant
               , simplifyScalExp
               , letRule simplifyIdentityReshape
               , letRule simplifyReshapeReshape
               , letRule simplifyReshapeScratch
               , letRule simplifyReshapeReplicate
               , letRule improveReshape
               , removeScratchValue
               , simplifyFallbackBranch
               , removeIdentityInPlace
               , removeFullInPlace
               , simplifyBranchResultComparison
               , simplifyReplicate
               , arrayLitToReplicate
               ]

bottomUpRules :: MonadBinder m => BottomUpRules m
bottomUpRules = [ removeRedundantMergeVariables
                , removeDeadBranchResult
                , simplifyIndex
                , simplifyConcat
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
standardRules :: (MonadBinder m, Aliased (Lore m)) => RuleBook m
standardRules = RuleBook topDownRules bottomUpRules

-- This next one is tricky - it's easy enough to determine that some
-- loop result is not used after the loop, but here, we must also make
-- sure that it does not affect any other values.
--
-- I do not claim that the current implementation of this rule is
-- perfect, but it should suffice for many cases, and should never
-- generate wrong code.
removeRedundantMergeVariables :: MonadBinder m => BottomUpRule m
removeRedundantMergeVariables (_, used) (Let pat _ (DoLoop ctx val form body))
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
        paramName v `S.member` necessaryForReturned ||
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
        patElemName pat_elem `S.member`
        (free_in_keeps <>
         freeIn (filter (/=pat_elem) $ patternContextElements pat))

      pat' = pat { patternValueElements = keep_valpatelems
                 , patternContextElements =
                     filter stillUsedContext $ patternContextElements pat }
  in if ctx' ++ val' == ctx ++ val
     then cannotSimplify
     else do
       -- We can't just remove the bindings in 'discard', since the loop
       -- body may still use their names in (now-dead) expressions.
       -- Hence, we add them inside the loop, fully aware that dead-code
       -- removal will eventually get rid of them.  Some care is
       -- necessary to handle unique bindings.
       body'' <- insertStmsM $ do
         mapM_ (uncurry letBindNames') $ dummyStms discard_ctx
         mapM_ (uncurry letBindNames') $ dummyStms discard_val
         return body'
       letBind_ pat' $ DoLoop ctx' val' form body''
  where pat_used = map (`UT.used` used) $ patternValueNames pat
        used_vals = map fst $ filter snd $ zip (map (paramName . fst) val) pat_used
        usedAfterLoop = flip elem used_vals . paramName
        usedAfterLoopOrInForm p =
          usedAfterLoop p || paramName p `S.member` freeIn form
        patAnnotNames = freeIn $ map fst $ ctx++val
        referencedInPat = (`S.member` patAnnotNames) . paramName
        referencedInForm = (`S.member` freeIn form) . paramName

        dummyStms = map dummyStm
        dummyStm ((p,e), _)
          | unique (paramDeclType p),
            Var v <- e            = ([paramName p], BasicOp $ Copy v)
          | otherwise             = ([paramName p], BasicOp $ SubExp e)
removeRedundantMergeVariables _ _ =
  cannotSimplify

findNecessaryForReturned :: (Param attr -> Bool) -> [(Param attr, SubExp)]
                         -> M.Map VName Names
                         -> Names
findNecessaryForReturned usedAfterLoop merge_and_res allDependencies =
  iterateNecessary mempty
  where iterateNecessary prev_necessary
          | necessary == prev_necessary = necessary
          | otherwise                   = iterateNecessary necessary
          where necessary = mconcat $ map dependencies returnedResultSubExps
                usedAfterLoopOrNecessary param =
                  usedAfterLoop param || paramName param `S.member` prev_necessary
                returnedResultSubExps =
                  map snd $ filter (usedAfterLoopOrNecessary . fst) merge_and_res
                dependencies (Constant _) =
                  S.empty
                dependencies (Var v)      =
                  M.findWithDefault (S.singleton v) v allDependencies

-- We may change the type of the loop if we hoist out a shape
-- annotation, in which case we also need to tweak the bound pattern.
hoistLoopInvariantMergeVariables :: forall m.MonadBinder m => TopDownRule m
hoistLoopInvariantMergeVariables _ (Let pat _ (DoLoop ctx val form loopbody)) =
    -- Figure out which of the elements of loopresult are
    -- loop-invariant, and hoist them out.
  case foldr checkInvariance ([], explpat, [], []) $
       zip merge res of
    ([], _, _, _) ->
      -- Nothing is invariant.
      cannotSimplify
    (invariant, explpat', merge', res') -> do
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
        letBindNames'_ [identName v1] $ BasicOp $ SubExp v2
      letBind_ (Pattern implpat'' explpat'') $
        DoLoop ctx' val' form loopbody'
  where merge = ctx ++ val
        res = bodyResult loopbody

        implpat = zip (patternContextElements pat) $
                  map paramName $ loopResultContext (map fst ctx) (map fst val)
        explpat = zip (patternValueElements pat) $
                  map (paramName . fst) val

        namesOfMergeParams = S.fromList $ map (paramName . fst) $ ctx++val

        removeFromResult (mergeParam,mergeInit) explpat' =
          case partition ((==paramName mergeParam) . snd) explpat' of
            ([(patelem,_)], rest) ->
              (Just (patElemIdent patelem, mergeInit), rest)
            (_,      _) ->
              (Nothing, explpat')

        checkInvariance
          ((mergeParam,mergeInit), resExp)
          (invariant, explpat', merge', resExps)
          | not (unique (paramDeclType mergeParam)) || arrayRank (paramDeclType mergeParam) == 1,
            isInvariant resExp =
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
                (S.fromList $ map (identName . fst) invariant) mergeParam
            --  (1) or identical to the initial value of the parameter.
            isInvariant _ = mergeInit == resExp

        checkInvariance ((mergeParam,mergeInit), resExp) (invariant, explpat', merge', resExps) =
          (invariant, explpat', (mergeParam,mergeInit):merge', resExp:resExps)

        allExistentialInvariant namesOfInvariant mergeParam =
          all (invariantOrNotMergeParam namesOfInvariant)
          (paramName mergeParam `S.delete` freeIn mergeParam)
        invariantOrNotMergeParam namesOfInvariant name =
          not (name `S.member` namesOfMergeParams) ||
          name `S.member` namesOfInvariant
hoistLoopInvariantMergeVariables _ _ = cannotSimplify

-- | A function that, given a variable name, returns its definition.
type VarLookup lore = VName -> Maybe (Exp lore, Certificates)

-- | A function that, given a subexpression, returns its type.
type TypeLookup = SubExp -> Maybe Type

type LetTopDownRule lore u = VarLookup lore -> TypeLookup
                             -> BasicOp lore -> Maybe (BasicOp lore, Certificates)

letRule :: MonadBinder m => LetTopDownRule (Lore m) u -> TopDownRule m
letRule rule vtable (Let pat aux (BasicOp op)) = do
  (op', cs) <- liftMaybe $ rule defOf seType op
  certifying (cs <> stmAuxCerts aux) $ letBind_ pat $ BasicOp op'
  where defOf = (`ST.lookupExp` vtable)
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v
letRule _ _ _ =
  cannotSimplify

simplifyClosedFormLoop :: MonadBinder m => TopDownRule m
simplifyClosedFormLoop _ (Let pat _ (DoLoop [] val (ForLoop i _ bound []) body)) =
  loopClosedForm pat val (S.singleton i) bound body
simplifyClosedFormLoop _ _ = cannotSimplify

simplifyLoopVariables :: (MonadBinder m, Aliased (Lore m)) => TopDownRule m
simplifyLoopVariables vtable (Let pat _
                              (DoLoop ctx val form@(ForLoop i it num_iters loop_vars) body))
  | simplifiable <- map checkIfSimplifiable loop_vars,
    not $ all isNothing simplifiable = do
      -- Check if the simplifications throw away more information than
      -- we are comfortable with at this stage.
      (maybe_loop_vars, body_prefix_stms) <-
        localScope (scopeOf form) $
        unzip <$> zipWithM onLoopVar loop_vars simplifiable
      if maybe_loop_vars == map Just loop_vars
        then cannotSimplify
        else do body' <- insertStmsM $ do
                  mapM_ addStm $ concat body_prefix_stms
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
          paramName p `S.member` consumed_in_body

        -- We only want this simplification it the result does not refer
        -- to 'i' at all, or does not contain accesses.
        onLoopVar (p,arr) Nothing =
          return (Just (p,arr), mempty)
        onLoopVar (p,arr) (Just m) = do
          (x,x_stms) <- collectStms m
          case x of
            IndexResult cs arr' slice
              | all (not . (i `S.member`) . freeInStm) x_stms,
                DimFix (Var j) : slice' <- slice,
                j == i, not $ i `S.member` freeIn slice -> do
                  mapM_ addStm x_stms
                  w <- arraySize 0 <$> lookupType arr'
                  for_in_partial <-
                    certifying cs $ letExp "for_in_partial" $ BasicOp $ Index arr' $
                    DimSlice (intConst Int32 0) w (intConst Int32 1) : slice'
                  return (Just (p, for_in_partial), mempty)

            SubExpResult cs se
              | all (safeExp . stmExp) x_stms -> do
                  x_stms' <- collectStms_ $ certifying cs $ do
                    mapM_ addStm x_stms
                    letBindNames'_ [paramName p] $ BasicOp $ SubExp se
                  return (Nothing, x_stms')

            _ -> return (Just (p,arr), mempty)
simplifyLoopVariables _ _ = cannotSimplify

simplifKnownIterationLoop :: MonadBinder m => TopDownRule m
simplifKnownIterationLoop _ (Let pat _
                                (DoLoop ctx val
                                 (ForLoop i it (Constant iters) loop_vars) body))
  | zeroIsh iters = do
      let bindResult p r = letBindNames' [patElemName p] $ BasicOp $ SubExp r
      zipWithM_ bindResult (patternContextElements pat) (map snd ctx)
      zipWithM_ bindResult (patternValueElements pat) (map snd val)

  | oneIsh iters = do

  forM_ (ctx++val) $ \(mergevar, mergeinit) ->
    letBindNames' [paramName mergevar] $ BasicOp $ SubExp mergeinit

  letBindNames'_ [i] $ BasicOp $ SubExp $ intConst it 0

  forM_ loop_vars $ \(p,arr) ->
    letBindNames'_ [paramName p] $ BasicOp $ Index arr $
    DimFix (intConst Int32 0) : fullSlice (paramType p) []

  (loop_body_ctx, loop_body_val) <- splitAt (length ctx) <$> (mapM asVar =<< bodyBind body)
  let subst = M.fromList $ zip (map (paramName . fst) ctx) loop_body_ctx
      ctx_params = substituteNames subst $ map fst ctx
      val_params = substituteNames subst $ map fst val
      res_context = loopResultContext ctx_params val_params
  forM_ (zip (patternContextElements pat) res_context) $ \(pat_elem, p) ->
    letBind_ (Pattern [] [pat_elem]) $ BasicOp $ SubExp $ Var $ paramName p
  forM_ (zip (patternValueElements pat) loop_body_val) $ \(pat_elem, v) ->
    letBind_ (Pattern [] [pat_elem]) $ BasicOp $ SubExp $ Var v
  where asVar (Var v)      = return v
        asVar (Constant v) = letExp "named" $ BasicOp $ SubExp $ Constant v
simplifKnownIterationLoop _ _ =
  cannotSimplify

simplifyRearrange :: MonadBinder m => TopDownRule m

-- Handle identity permutation.
simplifyRearrange _ (Let pat _ (BasicOp (Rearrange perm v)))
  | sort perm == perm =
      letBind_ pat $ BasicOp $ SubExp $ Var v

simplifyRearrange vtable (Let pat (StmAux cs _) (BasicOp (Rearrange perm v)))
  | Just (BasicOp (Rearrange perm2 e), v_cs) <- ST.lookupExp v vtable =
      -- Rearranging a rearranging: compose the permutations.
      certifying (cs<>v_cs) $
      letBind_ pat $ BasicOp $ Rearrange (perm `rearrangeCompose` perm2) e

simplifyRearrange vtable (Let pat (StmAux cs _) (BasicOp (Rearrange perm v)))
  | Just (BasicOp (Rotate offsets v2), v_cs) <- ST.lookupExp v vtable,
    Just (BasicOp (Rearrange perm3 v3), v2_cs) <- ST.lookupExp v2 vtable = do
      let offsets' = rearrangeShape (rearrangeInverse perm3) offsets
      rearrange_rotate <- letExp "rearrange_rotate" $ BasicOp $ Rotate offsets' v3
      certifying (cs<>v_cs<>v2_cs) $
        letBind_ pat $ BasicOp $ Rearrange (perm `rearrangeCompose` perm3) rearrange_rotate

simplifyRearrange vtable (Let pat (StmAux cs1 _) (BasicOp (Rearrange perm1 v1)))
  | Just (to_drop, to_take, cs2, 0, v2) <- isDropTake v1 vtable,
    Just (BasicOp (Rearrange perm3 v3), v2_cs) <- ST.lookupExp v2 vtable,
    dim1:_ <- perm1,
    perm1 == rearrangeInverse perm3 = do
      to_drop' <- letSubExp "drop" =<< toExp (asInt32PrimExp to_drop)
      to_take' <- letSubExp "take" =<< toExp (asInt32PrimExp to_take)
      [_, v] <- certifying (cs2<>cs1) $ letTupExp' "simplify_rearrange" $
                BasicOp $ Split dim1 [to_drop', to_take'] v3
      certifying v2_cs $ letBind_ pat $ BasicOp $ SubExp v

-- Rearranging a replicate where the outer dimension is left untouched.
simplifyRearrange vtable (Let pat (StmAux cs _) (BasicOp (Rearrange perm v1)))
  | Just (BasicOp (Replicate dims (Var v2)), v1_cs) <- ST.lookupExp v1 vtable,
    num_dims <- shapeRank dims,
    (rep_perm, rest_perm) <- splitAt num_dims perm,
    not $ null rest_perm,
    rep_perm == [0..length rep_perm-1] = certifying (cs<>v1_cs) $ do
      v <- letSubExp "rearrange_replicate" $
           BasicOp $ Rearrange (map (subtract num_dims) rest_perm) v2
      letBind_ pat $ BasicOp $ Replicate dims v

simplifyRearrange _ _ = cannotSimplify

isDropTake :: VName -> ST.SymbolTable lore
           -> Maybe (PrimExp VName, PrimExp VName,
                     Certificates, Int, VName)
isDropTake v vtable = do
  Let pat (StmAux cs _) (BasicOp (Split dim splits v')) <- ST.entryStm =<< ST.lookup v vtable
  i <- elemIndex v $ patternValueNames pat
  return (offs $ take i splits,
          offs $ take 1 $ drop i splits,
          cs, dim, v')
  where offs = sum . map (primExpFromSubExp int32)

simplifyRotate :: MonadBinder m => TopDownRule m
-- A zero-rotation is identity.
simplifyRotate _ (Let pat _ (BasicOp (Rotate offsets v)))
  | all isCt0 offsets = letBind_ pat $ BasicOp $ SubExp $ Var v

simplifyRotate vtable (Let pat (StmAux cs _) (BasicOp (Rotate offsets v)))
  | Just (BasicOp (Rearrange perm v2), v_cs) <- ST.lookupExp v vtable,
    Just (BasicOp (Rotate offsets2 v3), v2_cs) <- ST.lookupExp v2 vtable = do
      let offsets2' = rearrangeShape (rearrangeInverse perm) offsets2
          addOffsets x y = letSubExp "summed_offset" $ BasicOp $ BinOp (Add Int32) x y
      offsets' <- zipWithM addOffsets offsets offsets2'
      rotate_rearrange <-
        certifying cs $ letExp "rotate_rearrange" $ BasicOp $ Rearrange perm v3
      certifying (v_cs <> v2_cs) $
        letBind_ pat $ BasicOp $ Rotate offsets' rotate_rearrange

simplifyRotate _ _ = cannotSimplify

simplifyReplicate :: MonadBinder m => TopDownRule m
simplifyReplicate _ (Let pat _ (BasicOp (Replicate (Shape []) se@Constant{}))) =
  letBind_ pat $ BasicOp $ SubExp se
simplifyReplicate _ (Let pat _ (BasicOp (Replicate (Shape []) (Var v)))) = do
  v_t <- lookupType v
  letBind_ pat $ BasicOp $ if primType v_t
                          then SubExp $ Var v
                          else Copy v
simplifyReplicate vtable (Let pat _ (BasicOp (Replicate shape (Var v))))
  | Just (BasicOp (Replicate shape2 se), cs) <- ST.lookupExp v vtable =
      certifying cs $ letBind_ pat $ BasicOp $ Replicate (shape<>shape2) se
simplifyReplicate _ _ = cannotSimplify

-- | Turn array literals with identical elements into replicates.
arrayLitToReplicate :: MonadBinder m => TopDownRule m
arrayLitToReplicate _ (Let pat _ (BasicOp (ArrayLit (se:ses) _)))
  | all (==se) ses =
    let n = constant (genericLength ses + 1 :: Int32)
    in letBind_ pat $ BasicOp $ Replicate (Shape [n]) se
arrayLitToReplicate _ _ = cannotSimplify

simplifyCmpOp :: LetTopDownRule lore u
simplifyCmpOp _ _ (CmpOp cmp e1 e2)
  | e1 == e2 = constRes $ BoolValue $
               case cmp of CmpEq{}  -> True
                           CmpSlt{} -> False
                           CmpUlt{} -> False
                           CmpSle{} -> True
                           CmpUle{} -> True
                           FCmpLt{} -> False
                           FCmpLe{} -> True
simplifyCmpOp _ _ (CmpOp cmp (Constant v1) (Constant v2)) =
  constRes =<< BoolValue <$> doCmpOp cmp v1 v2
simplifyCmpOp _ _ _ = Nothing

simplifyBinOp :: LetTopDownRule lore u

simplifyBinOp _ _ (BinOp op (Constant v1) (Constant v2))
  | Just res <- doBinOp op v1 v2 =
      constRes res

simplifyBinOp _ _ (BinOp Add{} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp FAdd{} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1

simplifyBinOp _ _ (BinOp Sub{} e1 e2)
  | isCt0 e2 = subExpRes e1

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

simplifyUnOp :: LetTopDownRule lore u
simplifyUnOp _ _ (UnOp op (Constant v)) =
  constRes =<< doUnOp op v
simplifyUnOp defOf _ (UnOp Not (Var v))
  | Just (BasicOp (UnOp Not v2), v_cs) <- defOf v =
      Just (SubExp v2, v_cs)
simplifyUnOp _ _ _ =
  Nothing

simplifyConvOp :: LetTopDownRule lore u
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
simplifyAssert :: LetTopDownRule lore u
simplifyAssert _ _ (Assert (Constant (BoolValue True)) _ _) =
  constRes Checked
simplifyAssert _ _ _ =
  Nothing

constantFoldPrimFun :: MonadBinder m => TopDownRule m
constantFoldPrimFun _ (Let pat (StmAux cs _) (Apply fname args _ _))
  | Just args' <- mapM (isConst . fst) args,
    Just (_, _, fun) <- M.lookup (nameToString fname) primFuns,
    Just result <- fun args' =
      certifying cs $ letBind_ pat $ BasicOp $ SubExp $ Constant result
  where isConst (Constant v) = Just v
        isConst _ = Nothing
constantFoldPrimFun _ _ = cannotSimplify

simplifyIndex :: MonadBinder m => BottomUpRule m
simplifyIndex (vtable, used) (Let pat@(Pattern [] [pe]) (StmAux cs _) (BasicOp (Index idd inds)))
  | Just m <- simplifyIndexing vtable seType idd inds consumed = do
      res <- m
      case res of
        SubExpResult cs' se ->
          certifying (cs<>cs') $ letBindNames'_ (patternNames pat) $
          BasicOp $ SubExp se
        IndexResult extra_cs idd' inds' ->
          certifying (cs<>extra_cs) $
          letBindNames'_ (patternNames pat) $ BasicOp $ Index idd' inds'
  where consumed = patElemName pe `UT.isConsumed` used
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v

simplifyIndex _ _ = cannotSimplify

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
        Just (e, cs) <- ST.index idd inds' vtable ->
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
      Just $ fmap (IndexResult cs aa) $ do
      let adjust (DimFix j:js') is' = (DimFix j:) <$> adjust js' is'
          adjust (DimSlice j _ s:js') (DimFix i:is') = do
            i_t_s <- letSubExp "j_t_s" $ BasicOp $ BinOp (Mul Int32) i s
            j_p_i_t_s <- letSubExp "j_p_i_t_s" $ BasicOp $ BinOp (Add Int32) j i_t_s
            (DimFix j_p_i_t_s:) <$> adjust js' is'
          adjust (DimSlice j _ s0:js') (DimSlice i n s1:is') = do
            s0_t_i <- letSubExp "s0_t_i" $ BasicOp $ BinOp (Mul Int32) s0 i
            j_p_s0_t_i <- letSubExp "j_p_s0_t_i" $ BasicOp $ BinOp (Add Int32) j s0_t_i
            (DimSlice j_p_s0_t_i n s1:) <$> adjust js' is'
          adjust _ _ = return []
      adjust ais inds

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
        not consuming ->
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
      | Just (ibef, DimFix i, iaft) <- focusNth d inds -> Just $ do
      Prim res_t <- stripArray (length inds) <$> lookupType x
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

    _ -> case ST.entryStm =<< ST.lookup idd vtable of
           Just (Let split_pat (StmAux cs2 _) (BasicOp (Split 0 ns idd2)))
             | DimFix first_index : rest_indices <- inds -> Just $ do
               -- Figure out the extra offset that we should add to the first index.
               let plus = eBinOp (Add Int32)
                   esum [] = return $ BasicOp $ SubExp $ constant (0 :: Int32)
                   esum (x:xs) = foldl plus x xs

               patElem_and_offset <-
                 zip (patternValueElements split_pat) <$>
                 mapM esum (inits $ map eSubExp ns)
               case find ((==idd) . patElemName . fst) patElem_and_offset of
                 Nothing ->
                   fail "simplifyIndexing: could not find pattern element."
                 Just (_, offset_e) -> do
                   offset <- letSubExp "offset" offset_e
                   offset_index <- letSubExp "offset_index" $
                                   BasicOp $ BinOp (Add Int32) first_index offset
                   return $ IndexResult cs2 idd2 $ DimFix offset_index:rest_indices
           _ -> Nothing

    where defOf v = do (BasicOp op, def_cs) <- ST.lookupExp v vtable
                       return (op, def_cs)

simplifyIndexIntoReshape :: MonadBinder m => TopDownRule m
simplifyIndexIntoReshape vtable (Let pat (StmAux cs _) (BasicOp (Index idd slice)))
  | Just inds <- sliceIndices slice,
    Just (BasicOp (Reshape newshape idd2), idd_cs) <- ST.lookupExp idd vtable,
    length newshape == length inds =
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
simplifyIndexIntoReshape _ _ =
  cannotSimplify

removeEmptySplits :: MonadBinder m => TopDownRule m
removeEmptySplits _ (Let pat (StmAux cs _) (BasicOp (Split i ns arr)))
  | (pointless,sane) <- partition (isCt0 . snd) $ zip (patternValueElements pat) ns,
    not (null pointless) = do
      rt <- rowType <$> lookupType arr
      certifying cs $ letBind_ (Pattern [] $ map fst sane) $
        BasicOp $ Split i (map snd sane) arr
      forM_ pointless $ \(patElem,_) ->
        letBindNames' [patElemName patElem] $ BasicOp $ ArrayLit [] rt
removeEmptySplits _ _ =
  cannotSimplify

removeSingletonSplits :: MonadBinder m => TopDownRule m
removeSingletonSplits _ (Let pat _ (BasicOp (Split i [n] arr))) = do
  size <- arraySize i <$> lookupType arr
  if size == n then
    letBind_ pat $ BasicOp $ SubExp $ Var arr
    else cannotSimplify
removeSingletonSplits _ _ =
  cannotSimplify

simplifyConcat :: MonadBinder m => BottomUpRule m

-- concat@1(transpose(x),transpose(y)) == transpose(concat@0(x,y))
simplifyConcat (vtable, _) (Let pat _ (BasicOp (Concat i x xs new_d)))
  | Just r <- arrayRank <$> ST.lookupType x vtable,
    let perm = [i] ++ [0..i-1] ++ [i+1..r-1],
    Just (x',x_cs) <- transposedBy perm x,
    Just (xs',xs_cs) <- unzip <$> mapM (transposedBy perm) xs = do
      concat_rearrange <-
        certifying (x_cs<>mconcat xs_cs) $
        letExp "concat_rearrange" $ BasicOp $ Concat 0 x' xs' new_d
      letBind_ pat $ BasicOp $ Rearrange perm concat_rearrange
  where transposedBy perm1 v =
          case ST.lookupExp v vtable of
            Just (BasicOp (Rearrange perm2 v'), vcs)
              | perm1 == perm2 -> Just (v', vcs)
            _ -> Nothing

-- concat of a split array is identity.
simplifyConcat (vtable, used) (Let pat (StmAux cs _) (BasicOp (Concat i x xs new_d)))
  | Just (Let split_pat (StmAux split_cs _) (BasicOp (Split split_i _ split_arr))) <-
      ST.lookupStm x vtable,
    i == split_i,
    x:xs == patternNames split_pat = do
      split_arr_t <- lookupType split_arr
      let reshape = map DimCoercion $ arrayDims $ setDimSize i split_arr_t new_d
      split_arr' <- certifying  (cs<>split_cs) $ letExp "concat_reshape" $
                    BasicOp $ Reshape reshape split_arr
      if any (`UT.isConsumed` used) $ patternNames pat
        then letBind_ pat $ BasicOp $ Copy split_arr'
        else letBind_ pat $ BasicOp $ SubExp $ Var split_arr'

simplifyConcat _ _ =
  cannotSimplify

evaluateBranch :: MonadBinder m => TopDownRule m
evaluateBranch _ (Let pat _ (If e1 tb fb (IfAttr t ifsort)))
  | Just branch <- checkBranch,
    ifsort /= IfFallback || isCt1 e1 = do
  let ses = bodyResult branch
  mapM_ addStm $ bodyStms branch
  ctx <- subExpShapeContext (bodyTypeValues t) ses
  let ses' = ctx ++ ses
  sequence_ [ letBind (Pattern [] [p]) $ BasicOp $ SubExp se
            | (p,se) <- zip (patternElements pat) ses']
  where checkBranch
          | isCt1 e1  = Just tb
          | isCt0 e1  = Just fb
          | otherwise = Nothing
evaluateBranch _ _ = cannotSimplify

-- IMPROVE: This rule can be generalised to work in more cases,
-- especially when the branches have bindings, or return more than one
-- value.
simplifyBoolBranch :: MonadBinder m => TopDownRule m
-- if c then True else v == c || v
simplifyBoolBranch _
  (Let pat _ (If cond
              (Body _ [] [Constant (BoolValue True)])
              (Body _ [] [se])
              (IfAttr ts _)))
  | [Prim Bool] <- bodyTypeValues ts =
      letBind_ pat $ BasicOp $ BinOp LogOr cond se
-- When seType(x)==bool, if c then x else y == (c && x) || (!c && y)
simplifyBoolBranch _ (Let pat _ (If cond tb fb (IfAttr ts _)))
  | Body _ tstms [tres] <- tb,
    Body _ fstms [fres] <- fb,
    all (safeExp . stmExp) $ tstms ++ fstms,
    all (==Prim Bool) $ bodyTypeValues ts = do
  mapM_ addStm tstms
  mapM_ addStm fstms
  e <- eBinOp LogOr (pure $ BasicOp $ BinOp LogAnd cond tres)
                    (eBinOp LogAnd (pure $ BasicOp $ UnOp Not cond)
                     (pure $ BasicOp $ SubExp fres))
  letBind_ pat e
simplifyBoolBranch _ _ = cannotSimplify

simplifyFallbackBranch :: MonadBinder m => TopDownRule m
simplifyFallbackBranch _ (Let pat _ (If _ tbranch _ (IfAttr _ IfFallback)))
  | null $ patternContextNames pat,
    all (safeExp . stmExp) $ bodyStms tbranch = do
      let ses = bodyResult tbranch
      mapM_ addStm $ bodyStms tbranch
      sequence_ [ letBind (Pattern [] [p]) $ BasicOp $ SubExp se
                | (p,se) <- zip (patternElements pat) ses]
simplifyFallbackBranch _ _ =
  cannotSimplify

-- | Move out results of a conditional expression whose computation is
-- either invariant to the branches (only done for results in the
-- context), or the same in both branches.
hoistBranchInvariant :: MonadBinder m => TopDownRule m
hoistBranchInvariant _ (Let pat _ (If cond tb fb (IfAttr ret ifsort))) = do
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
        bound_in_branches = S.fromList $ concatMap (patternNames . stmPattern) $
                            bodyStms tb ++ bodyStms fb
        mem_sizes = freeIn $ filter (isMem . patElemType) $ patternElements pat
        invariant Constant{} = True
        invariant (Var v) = not $ v `S.member` bound_in_branches

        isMem Mem{} = True
        isMem _ = False
        sizeOfMem v = v `S.member` mem_sizes

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
        reshapeResult (Var v) (t@Array{}) = do
          v_t <- lookupType v
          let newshape = arrayDims $ removeExistentials t v_t
          if newshape /= arrayDims v_t
            then letSubExp "branch_ctx_reshaped" $ shapeCoerce newshape v
            else return $ Var v
        reshapeResult se _ =
          return se


hoistBranchInvariant _ _ = cannotSimplify

simplifyScalExp :: MonadBinder m => TopDownRule m
simplifyScalExp vtable (Let pat _ e) = do
  res <- SE.toScalExp (`ST.lookupScalExp` vtable) e
  case res of
    -- If the sufficient condition is 'True', then it statically succeeds.
    Just se
      | not $ isConstant se,
        Just ses <- lth0s se,
        all ((<size_bound) . SE.scalExpSize) ses,
        Right True <- and <$> mapM truish ses ->
        letBind_ pat $ BasicOp $ SubExp $ Constant $ BoolValue True
      | isNothing $ valOrVar se,
        SE.scalExpSize se < size_bound,
        Just se' <- valOrVar $ AS.simplify se ranges ->
        letBind_ pat $ BasicOp $ SubExp se'
    _ -> cannotSimplify
  where ranges = ST.rangesRep vtable
        size_bound = 30 -- don't touch scalexps bigger than this.
        lth0s se@(SE.RelExp SE.LTH0 _) = Just [se]
        lth0s (SE.RelExp SE.LEQ0 x) = Just [SE.RelExp SE.LTH0 $ x - 1]
        lth0s (SE.SLogAnd x y) = (++) <$> lth0s x <*> lth0s y
        lth0s _ = Nothing

        isConstant SE.Val{} = True
        isConstant _        = False

        valOrVar (SE.Val v)  = Just $ Constant v
        valOrVar (SE.Id v _) = Just $ Var v
        valOrVar _           = Nothing

        truish se =
          (SE.Val (BoolValue True)==) . mkDisj <$> AS.mkSuffConds se ranges

        mkDisj []     = SE.Val $ BoolValue False
        mkDisj (x:xs) = foldl SE.SLogOr (mkConj x) $ map mkConj xs
        mkConj []     = SE.Val $ BoolValue True
        mkConj (x:xs) = foldl SE.SLogAnd x xs

simplifyIdentityReshape :: LetTopDownRule lore u
simplifyIdentityReshape _ seType (Reshape newshape v)
  | Just t <- seType $ Var v,
    newDims newshape == arrayDims t = -- No-op reshape.
      subExpRes $ Var v
simplifyIdentityReshape _ _ _ = Nothing

simplifyReshapeReshape :: LetTopDownRule lore u
simplifyReshapeReshape defOf _ (Reshape newshape v)
  | Just (BasicOp (Reshape oldshape v2), v_cs) <- defOf v =
    Just (Reshape (fuseReshape oldshape newshape) v2, v_cs)
simplifyReshapeReshape _ _ _ = Nothing

simplifyReshapeScratch :: LetTopDownRule lore u
simplifyReshapeScratch defOf _ (Reshape newshape v)
  | Just (BasicOp (Scratch bt _), v_cs) <- defOf v =
    Just (Scratch bt $ newDims newshape, v_cs)
simplifyReshapeScratch _ _ _ = Nothing

simplifyReshapeReplicate :: LetTopDownRule lore u
simplifyReshapeReplicate defOf seType (Reshape newshape v)
  | Just (BasicOp (Replicate _ se), v_cs) <- defOf v,
    Just oldshape <- arrayShape <$> seType se,
    shapeDims oldshape `isSuffixOf` newDims newshape =
      let new = take (length newshape - shapeRank oldshape) $
                newDims newshape
      in Just (Replicate (Shape new) se, v_cs)
simplifyReshapeReplicate _ _ _ = Nothing


improveReshape :: LetTopDownRule lore u
improveReshape _ seType (Reshape newshape v)
  | Just t <- seType $ Var v,
    newshape' <- informReshape (arrayDims t) newshape,
    newshape' /= newshape =
      Just (Reshape newshape' v, mempty)
improveReshape _ _ _ = Nothing

-- | If we are copying a scratch array (possibly indirectly), just turn it into a scratch by
-- itself.
copyScratchToScratch :: LetTopDownRule lore u
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

removeIdentityInPlace :: MonadBinder m => TopDownRule m
removeIdentityInPlace vtable (Let (Pattern [] [d]) _ e)
  | BindInPlace dest destis <- patElemBindage d,
    arrayFrom e dest destis =
    letBind_ (Pattern [] [d { patElemBindage = BindVar}]) $ BasicOp $ SubExp $ Var dest
  where arrayFrom (BasicOp (Copy v)) dest destis
          | Just (e',_) <- ST.lookupExp v vtable =
              arrayFrom e' dest destis
        arrayFrom (BasicOp (Index src srcis)) dest destis =
          src == dest && destis == srcis
        arrayFrom _ _ _ =
          False
removeIdentityInPlace _ _ =
  cannotSimplify

-- | Turn in-place updates that replace an entire array into just
-- array literals.
removeFullInPlace :: MonadBinder m => TopDownRule m
removeFullInPlace vtable (Let (Pattern [] [d]) _ e)
  | BindInPlace dest is <- patElemBindage d,
    Just dest_t <- ST.lookupType dest vtable,
    isFullSlice (arrayShape dest_t) is  = do
      in_place_val <- letSubExp "in_place_val" e
      letBind_ (Pattern [] [d { patElemBindage = BindVar}]) $
        BasicOp $ ArrayLit [in_place_val] $ rowType dest_t
removeFullInPlace _ _ =
  cannotSimplify

removeScratchValue :: MonadBinder m => TopDownRule m
removeScratchValue _ (Let (Pattern [] [PatElem v (BindInPlace src _) _])
                      _ (BasicOp Scratch{})) =
    letBindNames'_ [v] $ BasicOp $ SubExp $ Var src
removeScratchValue _ _ =
  cannotSimplify

-- | Remove the return values of a branch, that are not actually used
-- after a branch.  Standard dead code removal can remove the branch
-- if *none* of the return values are used, but this rule is more
-- precise.
removeDeadBranchResult :: MonadBinder m => BottomUpRule m
removeDeadBranchResult (_, used) (Let pat _ (If e1 tb fb (IfAttr rettype ifsort)))
  | -- Only if there is no existential context...
    patternSize pat == length rettype,
    -- Figure out which of the names in 'pat' are used...
    patused <- map (`UT.used` used) $ patternNames pat,
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
  in letBind_ (Pattern [] pat') $ If e1 tb' fb' $ IfAttr rettype' ifsort
removeDeadBranchResult _ _ = cannotSimplify

-- | If we are comparing X against the result of a branch of the form
-- @if P then Y else Z@ then replace comparison with '(P && X == Y) ||
-- (!P && X == Z').  This may allow us to get rid of a branch, and the
-- extra comparisons may be constant-folded out.  Question: maybe we
-- should have some more checks to ensure that we only do this if that
-- is actually the case, such as if we will obtain at least one
-- constant-to-constant comparison?
simplifyBranchResultComparison :: MonadBinder m => TopDownRule m
simplifyBranchResultComparison vtable (Let pat _ (BasicOp (CmpOp (CmpEq t) se1 se2)))
  | Just m <- simplifyWith se1 se2 = m
  | Just m <- simplifyWith se2 se1 = m
  where simplifyWith (Var v) x
          | Just bnd <- ST.entryStm =<< ST.lookup v vtable,
            If p tbranch fbranch _ <- stmExp bnd,
            Just (y, z) <-
              returns v (stmPattern bnd) tbranch fbranch,
            S.null $ freeIn y `S.intersection` boundInBody tbranch,
            S.null $ freeIn z `S.intersection` boundInBody fbranch = Just $ do
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

simplifyBranchResultComparison _ _ =
  cannotSimplify

-- Some helper functions

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False
