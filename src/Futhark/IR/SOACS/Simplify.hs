{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.SOACS.Simplify
  ( simplifySOACS,
    simplifyLambda,
    simplifyFun,
    simplifyStms,
    simplifyConsts,
    simpleSOACS,
    simplifySOAC,
    soacRules,
    HasSOAC (..),
    simplifyKnownIterationSOAC,
    removeReplicateMapping,
    removeUnusedSOACInput,
    liftIdentityMapping,
    simplifyMapIota,
    SOACS,
  )
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Either
import Data.Foldable
import Data.List (partition, transpose, unzip4, unzip6, zip6)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (reshapeInner)
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify qualified as Simplify
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Optimise.Simplify.Rules.ClosedForm
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util

simpleSOACS :: Simplify.SimpleOps SOACS
simpleSOACS = Simplify.bindableSimpleOps simplifySOAC

simplifySOACS :: Prog SOACS -> PassM (Prog SOACS)
simplifySOACS =
  Simplify.simplifyProg simpleSOACS soacRules Engine.noExtraHoistBlockers

simplifyFun ::
  (MonadFreshNames m) =>
  ST.SymbolTable (Wise SOACS) ->
  FunDef SOACS ->
  m (FunDef SOACS)
simplifyFun =
  Simplify.simplifyFun simpleSOACS soacRules Engine.noExtraHoistBlockers

simplifyLambda ::
  (HasScope SOACS m, MonadFreshNames m) => Lambda SOACS -> m (Lambda SOACS)
simplifyLambda =
  Simplify.simplifyLambda simpleSOACS soacRules Engine.noExtraHoistBlockers

simplifyStms ::
  (HasScope SOACS m, MonadFreshNames m) => Stms SOACS -> m (Stms SOACS)
simplifyStms stms = do
  scope <- askScope
  Simplify.simplifyStms simpleSOACS soacRules Engine.noExtraHoistBlockers scope stms

simplifyConsts ::
  (MonadFreshNames m) => Stms SOACS -> m (Stms SOACS)
simplifyConsts =
  Simplify.simplifyStms simpleSOACS soacRules Engine.noExtraHoistBlockers mempty

simplifySOAC ::
  (Simplify.SimplifiableRep rep) =>
  Simplify.SimplifyOp rep (SOAC (Wise rep))
simplifySOAC (VJP arr vec lam) = do
  (lam', hoisted) <- Engine.simplifyLambda mempty lam
  arr' <- mapM Engine.simplify arr
  vec' <- mapM Engine.simplify vec
  pure (VJP arr' vec' lam', hoisted)
simplifySOAC (JVP arr vec lam) = do
  (lam', hoisted) <- Engine.simplifyLambda mempty lam
  arr' <- mapM Engine.simplify arr
  vec' <- mapM Engine.simplify vec
  pure (JVP arr' vec' lam', hoisted)
simplifySOAC (Stream outerdim arr nes lam) = do
  outerdim' <- Engine.simplify outerdim
  nes' <- mapM Engine.simplify nes
  arr' <- mapM Engine.simplify arr
  (lam', lam_hoisted) <- Engine.enterLoop $ Engine.simplifyLambda mempty lam
  pure (Stream outerdim' arr' nes' lam', lam_hoisted)
simplifySOAC (Scatter w ivs as lam) = do
  w' <- Engine.simplify w
  (lam', hoisted) <- Engine.enterLoop $ Engine.simplifyLambda mempty lam
  ivs' <- mapM Engine.simplify ivs
  as' <- mapM Engine.simplify as
  pure (Scatter w' ivs' as' lam', hoisted)
simplifySOAC (Hist w imgs ops bfun) = do
  w' <- Engine.simplify w
  (ops', hoisted) <- fmap unzip $
    forM ops $ \(HistOp dests_w rf dests nes op) -> do
      dests_w' <- Engine.simplify dests_w
      rf' <- Engine.simplify rf
      dests' <- Engine.simplify dests
      nes' <- mapM Engine.simplify nes
      (op', hoisted) <- Engine.enterLoop $ Engine.simplifyLambda mempty op
      pure (HistOp dests_w' rf' dests' nes' op', hoisted)
  imgs' <- mapM Engine.simplify imgs
  (bfun', bfun_hoisted) <- Engine.enterLoop $ Engine.simplifyLambda mempty bfun
  pure (Hist w' imgs' ops' bfun', mconcat hoisted <> bfun_hoisted)
simplifySOAC (Screma w arrs (ScremaForm map_lam scans reds)) = do
  (scans', scans_hoisted) <- fmap unzip $
    forM scans $ \(Scan lam nes) -> do
      (lam', hoisted) <- Engine.simplifyLambda mempty lam
      nes' <- Engine.simplify nes
      pure (Scan lam' nes', hoisted)

  (reds', reds_hoisted) <- fmap unzip $
    forM reds $ \(Reduce comm lam nes) -> do
      (lam', hoisted) <- Engine.simplifyLambda mempty lam
      nes' <- Engine.simplify nes
      pure (Reduce comm lam' nes', hoisted)

  (map_lam', map_lam_hoisted) <- Engine.enterLoop $ Engine.simplifyLambda mempty map_lam

  (,)
    <$> ( Screma
            <$> Engine.simplify w
            <*> Engine.simplify arrs
            <*> pure (ScremaForm map_lam' scans' reds')
        )
    <*> pure (mconcat scans_hoisted <> mconcat reds_hoisted <> map_lam_hoisted)

instance BuilderOps (Wise SOACS)

instance TraverseOpStms (Wise SOACS) where
  traverseOpStms = traverseSOACStms

fixLambdaParams ::
  (MonadBuilder m, Buildable (Rep m), BuilderOps (Rep m)) =>
  Lambda (Rep m) ->
  [Maybe SubExp] ->
  m (Lambda (Rep m))
fixLambdaParams lam fixes = do
  body <- runBodyBuilder $
    localScope (scopeOfLParams $ lambdaParams lam) $ do
      zipWithM_ maybeFix (lambdaParams lam) fixes'
      bodyBind $ lambdaBody lam
  pure
    lam
      { lambdaBody = body,
        lambdaParams =
          map fst $
            filter (isNothing . snd) $
              zip (lambdaParams lam) fixes'
      }
  where
    fixes' = fixes ++ repeat Nothing
    maybeFix p (Just x) = letBindNames [paramName p] $ BasicOp $ SubExp x
    maybeFix _ Nothing = pure ()

removeLambdaResults :: [Bool] -> Lambda rep -> Lambda rep
removeLambdaResults keep lam =
  lam
    { lambdaBody = lam_body',
      lambdaReturnType = ret
    }
  where
    keep' :: [a] -> [a]
    keep' = map snd . filter fst . zip (keep ++ repeat True)
    lam_body = lambdaBody lam
    lam_body' = lam_body {bodyResult = keep' $ bodyResult lam_body}
    ret = keep' $ lambdaReturnType lam

soacRules :: RuleBook (Wise SOACS)
soacRules = standardRules <> ruleBook topDownRules bottomUpRules

-- | Does this rep contain 'SOAC's in its t'Op's?  A rep must be an
-- instance of this class for the simplification rules to work.
class HasSOAC rep where
  asSOAC :: Op rep -> Maybe (SOAC rep)
  soacOp :: SOAC rep -> Op rep

instance HasSOAC (Wise SOACS) where
  asSOAC = Just
  soacOp = id

topDownRules :: [TopDownRule (Wise SOACS)]
topDownRules =
  [ RuleOp hoistCerts,
    RuleOp removeReplicateMapping,
    RuleOp removeReplicateWrite,
    RuleOp removeUnusedSOACInput,
    RuleOp simplifyClosedFormReduce,
    RuleOp simplifyKnownIterationSOAC,
    RuleOp liftIdentityMapping,
    RuleOp removeDuplicateMapOutput,
    RuleOp fuseConcatScatter,
    RuleOp simplifyMapIota,
    RuleOp moveTransformToInput,
    RuleOp moveTransformToOutput
  ]

bottomUpRules :: [BottomUpRule (Wise SOACS)]
bottomUpRules =
  [ RuleOp removeDeadMapping,
    RuleOp removeDeadReduction,
    RuleOp removeDeadWrite,
    RuleBasicOp removeUnnecessaryCopy,
    RuleOp liftIdentityStreaming,
    RuleOp mapOpToOp
  ]

-- Any certificates attached to a trivial Stm in the body might as
-- well be applied to the SOAC itself.
hoistCerts :: TopDownRuleOp (Wise SOACS)
hoistCerts vtable pat aux soac
  | (soac', hoisted) <- runState (mapSOACM mapper soac) mempty,
    hoisted /= mempty =
      Simplify $ auxing aux $ certifying hoisted $ letBind pat $ Op soac'
  where
    mapper = identitySOACMapper {mapOnSOACLambda = onLambda}
    onLambda lam = do
      stms' <- mapM onStm $ bodyStms $ lambdaBody lam
      pure
        lam
          { lambdaBody =
              mkBody stms' $ bodyResult $ lambdaBody lam
          }
    onStm (Let se_pat se_aux (BasicOp (SubExp se))) = do
      let (invariant, variant) =
            partition (`ST.elem` vtable) $
              unCerts $
                stmAuxCerts se_aux
          se_aux' = se_aux {stmAuxCerts = Certs variant}
      modify (Certs invariant <>)
      pure $ Let se_pat se_aux' $ BasicOp $ SubExp se
    onStm stm = pure stm
hoistCerts _ _ _ _ =
  Skip

liftIdentityMapping ::
  forall rep.
  (Buildable rep, BuilderOps rep, HasSOAC rep) =>
  TopDownRuleOp rep
liftIdentityMapping _ pat aux op
  | Just (Screma w arrs form :: SOAC rep) <- asSOAC op,
    Just fun <- isMapSOAC form = do
      let inputMap = M.fromList $ zip (map paramName $ lambdaParams fun) arrs
          free = freeIn $ lambdaBody fun
          rettype = lambdaReturnType fun
          ses = bodyResult $ lambdaBody fun

          freeOrConst (Var v) = v `nameIn` free
          freeOrConst Constant {} = True

          checkInvariance (outId, SubExpRes _ (Var v), _) (invariant, mapresult, rettype')
            | Just inp <- M.lookup v inputMap =
                ( (Pat [outId], e inp) : invariant,
                  mapresult,
                  rettype'
                )
            where
              e inp = case patElemType outId of
                Acc {} -> BasicOp $ SubExp $ Var inp
                _ -> BasicOp (Replicate mempty (Var inp))
          checkInvariance (outId, SubExpRes _ e, t) (invariant, mapresult, rettype')
            | freeOrConst e =
                ( (Pat [outId], BasicOp $ Replicate (Shape [w]) e) : invariant,
                  mapresult,
                  rettype'
                )
            | otherwise =
                ( invariant,
                  (outId, e) : mapresult,
                  t : rettype'
                )

      case foldr checkInvariance ([], [], []) $
        zip3 (patElems pat) ses rettype of
        ([], _, _) -> Skip
        (invariant, mapresult, rettype') -> Simplify $ do
          let (pat', ses') = unzip mapresult
              fun' =
                fun
                  { lambdaBody = (lambdaBody fun) {bodyResult = subExpsRes ses'},
                    lambdaReturnType = rettype'
                  }
          mapM_ (uncurry letBind) invariant
          auxing aux $
            letBindNames (map patElemName pat') $
              Op $
                soacOp $
                  Screma w arrs (mapSOAC fun')
liftIdentityMapping _ _ _ _ = Skip

liftIdentityStreaming :: BottomUpRuleOp (Wise SOACS)
liftIdentityStreaming _ (Pat pes) aux (Stream w arrs nes lam)
  | (variant_map, invariant_map) <-
      partitionEithers $ map isInvariantRes $ zip3 map_ts map_pes map_res,
    not $ null invariant_map = Simplify $ do
      forM_ invariant_map $ \(pe, arr) ->
        letBind (Pat [pe]) $ BasicOp $ Replicate mempty $ Var arr

      let (variant_map_ts, variant_map_pes, variant_map_res) = unzip3 variant_map
          lam' =
            lam
              { lambdaBody = (lambdaBody lam) {bodyResult = fold_res ++ variant_map_res},
                lambdaReturnType = fold_ts ++ variant_map_ts
              }

      auxing aux . letBind (Pat $ fold_pes ++ variant_map_pes) . Op $
        Stream w arrs nes lam'
  where
    num_folds = length nes
    (fold_pes, map_pes) = splitAt num_folds pes
    (fold_ts, map_ts) = splitAt num_folds $ lambdaReturnType lam
    lam_res = bodyResult $ lambdaBody lam
    (fold_res, map_res) = splitAt num_folds lam_res
    params_to_arrs = zip (map paramName $ drop (1 + num_folds) $ lambdaParams lam) arrs

    isInvariantRes (_, pe, SubExpRes _ (Var v))
      | Just arr <- lookup v params_to_arrs =
          Right (pe, arr)
    isInvariantRes x =
      Left x
liftIdentityStreaming _ _ _ _ = Skip

-- | Remove all arguments to the map that are simply replicates.
-- These can be turned into free variables instead.
removeReplicateMapping ::
  (Aliased rep, BuilderOps rep, HasSOAC rep) =>
  TopDownRuleOp rep
removeReplicateMapping vtable pat aux op
  | Just (Screma w arrs form) <- asSOAC op,
    Just fun <- isMapSOAC form,
    Just (stms, fun', arrs') <- removeReplicateInput vtable fun arrs = Simplify $ do
      forM_ stms $ \(vs, cs, e) -> certifying cs $ letBindNames vs e
      auxing aux $ letBind pat $ Op $ soacOp $ Screma w arrs' $ mapSOAC fun'
removeReplicateMapping _ _ _ _ = Skip

-- | Like 'removeReplicateMapping', but for 'Scatter'.
removeReplicateWrite :: TopDownRuleOp (Wise SOACS)
removeReplicateWrite vtable pat aux (Scatter w ivs as lam)
  | Just (stms, lam', ivs') <- removeReplicateInput vtable lam ivs = Simplify $ do
      forM_ stms $ \(vs, cs, e) -> certifying cs $ letBindNames vs e
      auxing aux $ letBind pat $ Op $ Scatter w ivs' as lam'
removeReplicateWrite _ _ _ _ = Skip

removeReplicateInput ::
  (Aliased rep) =>
  ST.SymbolTable rep ->
  Lambda rep ->
  [VName] ->
  Maybe
    ( [([VName], Certs, Exp rep)],
      Lambda rep,
      [VName]
    )
removeReplicateInput vtable fun arrs
  | not $ null parameterBnds = do
      let (arr_params', arrs') = unzip params_and_arrs
          fun' = fun {lambdaParams = acc_params <> arr_params'}
      pure (parameterBnds, fun', arrs')
  | otherwise = Nothing
  where
    params = lambdaParams fun
    (acc_params, arr_params) =
      splitAt (length params - length arrs) params
    (params_and_arrs, parameterBnds) =
      partitionEithers $ zipWith isReplicateAndNotConsumed arr_params arrs

    isReplicateAndNotConsumed p v
      | Just (BasicOp (Replicate (Shape (_ : ds)) e), v_cs) <-
          ST.lookupExp v vtable,
        paramName p `notNameIn` consumedByLambda fun =
          Right
            ( [paramName p],
              v_cs,
              case ds of
                [] -> BasicOp $ SubExp e
                _ -> BasicOp $ Replicate (Shape ds) e
            )
      | otherwise =
          Left (p, v)

-- | Remove inputs that are not used inside the SOAC.
removeUnusedSOACInput ::
  forall rep.
  (Aliased rep, Buildable rep, BuilderOps rep, HasSOAC rep) =>
  TopDownRuleOp rep
removeUnusedSOACInput _ pat aux op
  | Just (Screma w arrs form :: SOAC rep) <- asSOAC op,
    ScremaForm map_lam scan reduce <- form,
    Just (used_arrs, map_lam') <- remove map_lam arrs =
      Simplify . auxing aux . letBind pat . Op $
        soacOp (Screma w used_arrs (ScremaForm map_lam' scan reduce))
  | Just (Scatter w arrs dests map_lam :: SOAC rep) <- asSOAC op,
    Just (used_arrs, map_lam') <- remove map_lam arrs =
      Simplify . auxing aux . letBind pat . Op $
        soacOp (Scatter w used_arrs dests map_lam')
  where
    used_in_body map_lam = freeIn $ lambdaBody map_lam
    usedInput map_lam (param, _) = paramName param `nameIn` used_in_body map_lam
    remove map_lam arrs =
      let (used, unused) = partition (usedInput map_lam) (zip (lambdaParams map_lam) arrs)
          (used_params, used_arrs) = unzip used
          map_lam' = map_lam {lambdaParams = used_params}
       in if null unused then Nothing else Just (used_arrs, map_lam')
removeUnusedSOACInput _ _ _ _ = Skip

removeDeadMapping :: BottomUpRuleOp (Wise SOACS)
removeDeadMapping (_, used) (Pat pes) aux (Screma w arrs (ScremaForm lam scans reds))
  | (nonmap_pes, map_pes) <- splitAt num_nonmap_res pes,
    not $ null map_pes =
      let (nonmap_res, map_res) = splitAt num_nonmap_res $ bodyResult $ lambdaBody lam
          (nonmap_ts, map_ts) = splitAt num_nonmap_res $ lambdaReturnType lam
          isUsed (bindee, _, _) = (`UT.used` used) $ patElemName bindee
          (map_pes', map_res', map_ts') =
            unzip3 $ filter isUsed $ zip3 map_pes map_res map_ts
          lam' =
            lam
              { lambdaBody = (lambdaBody lam) {bodyResult = nonmap_res <> map_res'},
                lambdaReturnType = nonmap_ts <> map_ts'
              }
       in if map_pes /= map_pes'
            then
              Simplify . auxing aux $
                letBind (Pat $ nonmap_pes <> map_pes') . Op $
                  Screma w arrs (ScremaForm lam' scans reds)
            else Skip
  where
    num_nonmap_res = scanResults scans + redResults reds
removeDeadMapping _ _ _ _ = Skip

removeDuplicateMapOutput :: TopDownRuleOp (Wise SOACS)
removeDuplicateMapOutput _ (Pat pes) aux (Screma w arrs form)
  | Just fun <- isMapSOAC form =
      let ses = bodyResult $ lambdaBody fun
          ts = lambdaReturnType fun
          ses_ts_pes = zip3 ses ts pes
          (ses_ts_pes', copies) =
            foldl checkForDuplicates (mempty, mempty) ses_ts_pes
       in if null copies
            then Skip
            else Simplify $ do
              let (ses', ts', pes') = unzip3 ses_ts_pes'
                  fun' =
                    fun
                      { lambdaBody = (lambdaBody fun) {bodyResult = ses'},
                        lambdaReturnType = ts'
                      }
              auxing aux $ letBind (Pat pes') $ Op $ Screma w arrs $ mapSOAC fun'
              forM_ copies $ \(from, to) ->
                letBind (Pat [to]) $ BasicOp $ Replicate mempty $ Var $ patElemName from
  where
    checkForDuplicates (ses_ts_pes', copies) (se, t, pe)
      | Just (_, _, pe') <- find (\(x, _, _) -> resSubExp x == resSubExp se) ses_ts_pes' =
          -- This result has been returned before, producing the
          -- array pe'.
          (ses_ts_pes', (pe', pe) : copies)
      | otherwise = (ses_ts_pes' ++ [(se, t, pe)], copies)
removeDuplicateMapOutput _ _ _ _ = Skip

reshapeInner :: SubExp -> NewShape SubExp -> NewShape SubExp
reshapeInner w new_shape =
  reshapeCoerce outer <> newshapeInner outer new_shape
  where
    outer = Shape [w]

-- Mapping some operations becomes an extension of that operation.
mapOpToOp :: BottomUpRuleOp (Wise SOACS)
mapOpToOp (_, used) pat aux1 e
  | Just (map_pe, cs, w, BasicOp (Reshape newshape reshape_arr), [p], [arr]) <-
      isMapWithOp pat e,
    paramName p == reshape_arr,
    not $ UT.isConsumed (patElemName map_pe) used = Simplify $ do
      certifying (stmAuxCerts aux1 <> cs) . letBind pat . BasicOp $
        Reshape (reshapeInner w newshape) arr
  | Just (_, cs, _, BasicOp (Concat d (arr :| arrs) dw), ps, outer_arr : outer_arrs) <-
      isMapWithOp pat e,
    (arr : arrs) == map paramName ps =
      Simplify . certifying (stmAuxCerts aux1 <> cs) . letBind pat . BasicOp $
        Concat (d + 1) (outer_arr :| outer_arrs) dw
  | Just
      (map_pe, cs, _, BasicOp (Rearrange perm rearrange_arr), [p], [arr]) <-
      isMapWithOp pat e,
    paramName p == rearrange_arr,
    not $ UT.isConsumed (patElemName map_pe) used =
      Simplify . certifying (stmAuxCerts aux1 <> cs) . letBind pat . BasicOp $
        Rearrange (0 : map (1 +) perm) arr
mapOpToOp _ _ _ _ = Skip

isMapWithOp ::
  Pat dec ->
  SOAC (Wise SOACS) ->
  Maybe
    ( PatElem dec,
      Certs,
      SubExp,
      Exp (Wise SOACS),
      [Param Type],
      [VName]
    )
isMapWithOp pat e
  | Pat [map_pe] <- pat,
    Screma w arrs form <- e,
    Just map_lam <- isMapSOAC form,
    [Let (Pat [pe]) aux2 e'] <- stmsToList $ bodyStms $ lambdaBody map_lam,
    [SubExpRes _ (Var r)] <- bodyResult $ lambdaBody map_lam,
    r == patElemName pe =
      Just (map_pe, stmAuxCerts aux2, w, e', lambdaParams map_lam, arrs)
  | otherwise = Nothing

-- | Some of the results of a reduction (or really: Redomap) may be
-- dead.  We remove them here.  The trick is that we need to look at
-- the data dependencies to see that the "dead" result is not
-- actually used for computing one of the live ones.
removeDeadReduction :: BottomUpRuleOp (Wise SOACS)
removeDeadReduction (_, used) pat aux (Screma w arrs form) =
  case isRedomapSOAC form of
    Just ([Reduce comm redlam rednes], maplam) ->
      let mkOp lam nes' = redomapSOAC [Reduce comm lam nes']
       in removeDeadReduction' redlam rednes maplam mkOp
    _ ->
      case isScanomapSOAC form of
        Just ([Scan scanlam nes], maplam) ->
          let mkOp lam nes' = scanomapSOAC [Scan lam nes']
           in removeDeadReduction' scanlam nes maplam mkOp
        _ -> Skip
  where
    removeDeadReduction' redlam nes maplam mkOp
      | not $ all (`UT.used` used) $ patNames pat, -- Quick/cheap check
        let (red_pes, map_pes) = splitAt (length nes) $ patElems pat,
        let redlam_deps = dataDependencies $ lambdaBody redlam,
        let redlam_res = bodyResult $ lambdaBody redlam,
        let redlam_params = lambdaParams redlam,
        let (redlam_xparams, redlam_yparams) =
              splitAt (length nes) redlam_params,
        let used_after =
              map snd . filter ((`UT.used` used) . patElemName . fst) $
                zip (red_pes <> red_pes) redlam_params,
        let necessary =
              findNecessaryForReturned
                (`elem` used_after)
                (zip redlam_params $ map resSubExp $ redlam_res <> redlam_res)
                redlam_deps,
        let alive_mask =
              zipWith
                (||)
                (map ((`nameIn` necessary) . paramName) redlam_xparams)
                (map ((`nameIn` necessary) . paramName) redlam_yparams),
        not $ and alive_mask = Simplify $ do
          let fixDeadToNeutral lives ne = if lives then Nothing else Just ne
              dead_fix = zipWith fixDeadToNeutral alive_mask nes
              (used_red_pes, used_nes) =
                unzip . map snd . filter fst $ zip alive_mask $ zip red_pes nes

          when (used_nes == nes) cannotSimplify

          let maplam' = removeLambdaResults alive_mask maplam
          redlam' <-
            removeLambdaResults alive_mask
              <$> fixLambdaParams redlam (dead_fix ++ dead_fix)

          auxing aux . letBind (Pat $ used_red_pes ++ map_pes) . Op $
            Screma w arrs (mkOp redlam' used_nes maplam')
    removeDeadReduction' _ _ _ _ = Skip
removeDeadReduction _ _ _ _ = Skip

-- | If we are writing to an array that is never used, get rid of it.
removeDeadWrite :: BottomUpRuleOp (Wise SOACS)
removeDeadWrite (_, used) pat aux (Scatter w arrs dests fun) =
  let (i_ses, v_ses) = unzip $ groupScatterResults' dests $ bodyResult $ lambdaBody fun
      (i_ts, v_ts) = unzip $ groupScatterResults' dests $ lambdaReturnType fun
      isUsed (bindee, _, _, _, _, _) = (`UT.used` used) $ patElemName bindee
      (pat', i_ses', v_ses', i_ts', v_ts', dests') =
        unzip6 $ filter isUsed $ zip6 (patElems pat) i_ses v_ses i_ts v_ts dests
      fun' =
        fun
          { lambdaBody =
              mkBody (bodyStms (lambdaBody fun)) (concat i_ses' ++ v_ses'),
            lambdaReturnType = concat i_ts' ++ v_ts'
          }
   in if pat /= Pat pat'
        then
          Simplify . auxing aux . letBind (Pat pat') $
            Op (Scatter w arrs dests' fun')
        else Skip
removeDeadWrite _ _ _ _ = Skip

-- handles now concatenation of more than two arrays
fuseConcatScatter :: TopDownRuleOp (Wise SOACS)
fuseConcatScatter vtable pat _ (Scatter _ arrs dests fun)
  | Just (ws@(w' : _), xss, css) <- unzip3 <$> mapM isConcat arrs,
    xivs <- transpose xss,
    all (w' ==) ws = Simplify $ do
      let r = length xivs
      fun2s <- replicateM (r - 1) (renameLambda fun)
      let (fun_is, fun_vs) =
            unzip . map (splitScatterResults dests . bodyResult . lambdaBody) $
              fun : fun2s
          (its, vts) =
            unzip . replicate r . splitScatterResults dests $ lambdaReturnType fun
          new_stmts = mconcat $ map (bodyStms . lambdaBody) (fun : fun2s)
      let fun' =
            Lambda
              { lambdaParams = mconcat $ map lambdaParams (fun : fun2s),
                lambdaBody = mkBody new_stmts $ mix fun_is <> mix fun_vs,
                lambdaReturnType = mix its <> mix vts
              }
      certifying (mconcat css) . letBind pat . Op $
        Scatter w' (concat xivs) (map (incWrites r) dests) fun'
  where
    sizeOf :: VName -> Maybe SubExp
    sizeOf x = arraySize 0 . typeOf <$> ST.lookup x vtable
    mix = concat . transpose
    incWrites r (w, n, a) = (w, n * r, a) -- ToDO: is it (n*r) or (n+r-1)??
    isConcat v = case ST.lookupExp v vtable of
      Just (BasicOp (Concat 0 (x :| ys) _), cs) -> do
        x_w <- sizeOf x
        y_ws <- mapM sizeOf ys
        guard $ all (x_w ==) y_ws
        pure (x_w, x : ys, cs)
      Just (BasicOp (Reshape newshape arr), cs)
        | ReshapeCoerce <- reshapeKind newshape -> do
            (a, b, cs') <- isConcat arr
            pure (a, b, cs <> cs')
      _ -> Nothing
fuseConcatScatter _ _ _ _ = Skip

simplifyClosedFormReduce :: TopDownRuleOp (Wise SOACS)
simplifyClosedFormReduce _ pat _ (Screma (Constant w) _ form)
  | Just nes <- concatMap redNeutral . fst <$> isRedomapSOAC form,
    zeroIsh w =
      Simplify . forM_ (zip (patNames pat) nes) $ \(v, ne) ->
        letBindNames [v] $ BasicOp $ SubExp ne
simplifyClosedFormReduce vtable pat _ (Screma _ arrs form)
  | Just [Reduce _ red_fun nes] <- isReduceSOAC form =
      Simplify $ foldClosedForm (`ST.lookupExp` vtable) pat red_fun nes arrs
simplifyClosedFormReduce _ _ _ _ = Skip

-- For now we just remove singleton SOACs and those with unroll attributes.
simplifyKnownIterationSOAC ::
  (Buildable rep, BuilderOps rep, HasSOAC rep) =>
  TopDownRuleOp rep
simplifyKnownIterationSOAC _ pat _ op
  | Just (Screma (Constant k) arrs (ScremaForm map_lam scans reds)) <- asSOAC op,
    oneIsh k = Simplify $ do
      let (Reduce _ red_lam red_nes) = singleReduce reds
          (Scan scan_lam scan_nes) = singleScan scans
          (scan_pes, red_pes, map_pes) =
            splitAt3 (length scan_nes) (length red_nes) $
              patElems pat
          bindMapParam p a = do
            a_t <- lookupType a
            letBindNames [paramName p] $
              BasicOp $
                Index a $
                  fullSlice a_t [DimFix $ constant (0 :: Int64)]
          bindArrayResult pe (SubExpRes cs se) =
            certifying cs . letBindNames [patElemName pe] $
              BasicOp $
                ArrayLit [se] $
                  rowType $
                    patElemType pe
          bindResult pe (SubExpRes cs se) =
            certifying cs $ letBindNames [patElemName pe] $ BasicOp $ SubExp se

      zipWithM_ bindMapParam (lambdaParams map_lam) arrs
      (to_scan, to_red, map_res) <-
        splitAt3 (length scan_nes) (length red_nes)
          <$> bodyBind (lambdaBody map_lam)
      scan_res <- eLambda scan_lam $ map eSubExp $ scan_nes ++ map resSubExp to_scan
      red_res <- eLambda red_lam $ map eSubExp $ red_nes ++ map resSubExp to_red

      zipWithM_ bindArrayResult scan_pes scan_res
      zipWithM_ bindResult red_pes red_res
      zipWithM_ bindArrayResult map_pes map_res
simplifyKnownIterationSOAC _ pat _ op
  | Just (Stream (Constant k) arrs nes fold_lam) <- asSOAC op,
    oneIsh k = Simplify $ do
      let (chunk_param, acc_params, slice_params) =
            partitionChunkedFoldParameters (length nes) (lambdaParams fold_lam)

      letBindNames [paramName chunk_param] $
        BasicOp $
          SubExp $
            intConst Int64 1

      forM_ (zip acc_params nes) $ \(p, ne) ->
        letBindNames [paramName p] $ BasicOp $ SubExp ne

      forM_ (zip slice_params arrs) $ \(p, arr) ->
        letBindNames [paramName p] $ BasicOp $ SubExp $ Var arr

      res <- bodyBind $ lambdaBody fold_lam

      forM_ (zip (patNames pat) res) $ \(v, SubExpRes cs se) ->
        certifying cs $ letBindNames [v] $ BasicOp $ SubExp se
--
simplifyKnownIterationSOAC _ pat aux op
  | Just (Screma (Constant (IntValue (Int64Value k))) arrs (ScremaForm map_lam [] [])) <- asSOAC op,
    "unroll" `inAttrs` stmAuxAttrs aux = Simplify $ do
      arrs_elems <- fmap transpose . forM [0 .. k - 1] $ \i -> do
        map_lam' <- renameLambda map_lam
        eLambda map_lam' $ map (`eIndex` [eSubExp (constant i)]) arrs
      forM_ (zip3 (patNames pat) arrs_elems (lambdaReturnType map_lam)) $
        \(v, arr_elems, t) ->
          certifying (mconcat (map resCerts arr_elems)) $
            letBindNames [v] . BasicOp $
              ArrayLit (map resSubExp arr_elems) t
--
simplifyKnownIterationSOAC _ _ _ _ = Skip

data ArrayOp
  = ArrayIndexing Certs VName (Slice SubExp)
  | ArrayRearrange Certs VName [Int]
  | ArrayReshape Certs VName (NewShape SubExp)
  | ArrayCopy Certs VName
  | -- | Never constructed.
    ArrayVar Certs VName
  deriving (Eq, Ord, Show)

arrayOpArr :: ArrayOp -> VName
arrayOpArr (ArrayIndexing _ arr _) = arr
arrayOpArr (ArrayRearrange _ arr _) = arr
arrayOpArr (ArrayReshape _ arr _) = arr
arrayOpArr (ArrayCopy _ arr) = arr
arrayOpArr (ArrayVar _ arr) = arr

arrayOpCerts :: ArrayOp -> Certs
arrayOpCerts (ArrayIndexing cs _ _) = cs
arrayOpCerts (ArrayRearrange cs _ _) = cs
arrayOpCerts (ArrayReshape cs _ _) = cs
arrayOpCerts (ArrayCopy cs _) = cs
arrayOpCerts (ArrayVar cs _) = cs

isArrayOp :: Certs -> Exp rep -> Maybe ArrayOp
isArrayOp cs (BasicOp (Index arr slice)) =
  Just $ ArrayIndexing cs arr slice
isArrayOp cs (BasicOp (Rearrange perm arr)) =
  Just $ ArrayRearrange cs arr perm
isArrayOp cs (BasicOp (Reshape new_shape arr)) =
  Just $ ArrayReshape cs arr new_shape
isArrayOp cs (BasicOp (Replicate (Shape []) (Var arr))) =
  Just $ ArrayCopy cs arr
isArrayOp _ _ =
  Nothing

fromArrayOp :: ArrayOp -> (Certs, Exp rep)
fromArrayOp (ArrayIndexing cs arr slice) = (cs, BasicOp $ Index arr slice)
fromArrayOp (ArrayRearrange cs arr perm) = (cs, BasicOp $ Rearrange perm arr)
fromArrayOp (ArrayReshape cs arr new_shape) = (cs, BasicOp $ Reshape new_shape arr)
fromArrayOp (ArrayCopy cs arr) = (cs, BasicOp $ Replicate mempty $ Var arr)
fromArrayOp (ArrayVar cs arr) = (cs, BasicOp $ SubExp $ Var arr)

arrayOps ::
  forall rep.
  (Buildable rep, HasSOAC rep) =>
  Certs ->
  Body rep ->
  S.Set (Pat (LetDec rep), ArrayOp)
arrayOps cs = mconcat . map onStm . stmsToList . bodyStms
  where
    -- It is not safe to move everything out of branches (#1874) or
    -- loops (#2015); probably we need to put some more intelligence
    -- in here somehow.
    onStm (Let _ _ Match {}) = mempty
    onStm (Let _ _ Loop {}) = mempty
    onStm (Let pat aux e) =
      case isArrayOp (cs <> stmAuxCerts aux) e of
        Just op -> S.singleton (pat, op)
        Nothing -> execState (walkExpM (walker (stmAuxCerts aux)) e) mempty
    onOp more_cs op
      | Just soac <- asSOAC op =
          -- Copies are not safe to move out of nested ops (#1753).
          S.filter (notCopy . snd) $
            execWriter $
              mapSOACM
                identitySOACMapper {mapOnSOACLambda = onLambda more_cs}
                (soac :: SOAC rep)
      | otherwise =
          mempty
    onLambda more_cs lam = do
      tell $ arrayOps (cs <> more_cs) $ lambdaBody lam
      pure lam
    walker more_cs =
      (identityWalker @rep)
        { walkOnBody = const $ modify . (<>) . arrayOps (cs <> more_cs),
          walkOnOp = modify . (<>) . onOp more_cs
        }
    notCopy (ArrayCopy {}) = False
    notCopy _ = True

replaceArrayOps ::
  forall rep.
  (Buildable rep, BuilderOps rep, HasSOAC rep) =>
  M.Map (Pat (LetDec rep)) ArrayOp ->
  Body rep ->
  Body rep
replaceArrayOps substs (Body _ stms res) =
  mkBody (fmap onStm stms) res
  where
    onStm (Let pat aux e) =
      let (cs', e') =
            maybe (mempty, mapExp mapper e) fromArrayOp $ M.lookup pat substs
       in certify cs' $ mkLet' (patIdents pat) aux e'
    mapper =
      (identityMapper @rep)
        { mapOnBody = const $ pure . replaceArrayOps substs,
          mapOnOp = pure . onOp
        }
    onOp op
      | Just (soac :: SOAC rep) <- asSOAC op =
          soacOp . runIdentity $
            mapSOACM identitySOACMapper {mapOnSOACLambda = pure . onLambda} soac
      | otherwise =
          op
    onLambda lam = lam {lambdaBody = replaceArrayOps substs $ lambdaBody lam}

-- Turn
--
--    map (\i -> ... xs[i] ...) (iota n)
--
-- into
--
--    map (\i x -> ... x ...) (iota n) xs
--
-- This is not because we want to encourage the map-iota pattern, but
-- it may be present in generated code.  This is an unfortunately
-- expensive simplification rule, since it requires multiple passes
-- over the entire lambda body.  It only handles the very simplest
-- case - if you find yourself planning to extend it to handle more
-- complex situations (rotate or whatnot), consider turning it into a
-- separate compiler pass instead.
simplifyMapIota ::
  forall rep.
  (Buildable rep, BuilderOps rep, HasSOAC rep) =>
  TopDownRuleOp rep
simplifyMapIota vtable screma_pat aux op
  | Just (Screma w arrs (ScremaForm map_lam scan reduce) :: SOAC rep) <- asSOAC op,
    Just (p, _) <- find isIota (zip (lambdaParams map_lam) arrs),
    indexings <-
      mapMaybe (indexesWith (paramName p)) . S.toList $
        arrayOps mempty $
          lambdaBody map_lam,
    not $ null indexings = Simplify $ do
      -- For each indexing with iota, add the corresponding array to
      -- the Screma, and construct a new lambda parameter.
      (more_arrs, more_params, replacements) <-
        unzip3 . catMaybes <$> mapM (mapOverArr w) indexings
      let substs = M.fromList replacements
          map_lam' =
            map_lam
              { lambdaParams = lambdaParams map_lam <> more_params,
                lambdaBody = replaceArrayOps substs $ lambdaBody map_lam
              }

      auxing aux . letBind screma_pat . Op . soacOp $
        Screma w (arrs <> more_arrs) (ScremaForm map_lam' scan reduce)
  where
    isIota (_, arr) = case ST.lookupBasicOp arr vtable of
      Just (Iota _ (Constant o) (Constant s) _, _) ->
        zeroIsh o && oneIsh s
      _ -> False

    -- Find a 'DimFix i', optionally preceded by other DimFixes, and
    -- if so return those DimFixes.
    fixWith i (DimFix j : slice)
      | Var i == j = Just []
      | otherwise = (j :) <$> fixWith i slice
    fixWith _ _ = Nothing

    indexesWith v (pat, idx@(ArrayIndexing cs arr (Slice js)))
      | arr `ST.elem` vtable,
        all (`ST.elem` vtable) $ unCerts cs,
        Just js' <- fixWith v js,
        all (`ST.elem` vtable) $ namesToList $ freeIn js' =
          Just (pat, js', idx)
    indexesWith _ _ = Nothing

    properArr [] arr = pure arr
    properArr js arr = do
      arr_t <- lookupType arr
      letExp (baseString arr) $ BasicOp $ Index arr $ fullSlice arr_t $ map DimFix js

    mapOverArr w (pat, js, ArrayIndexing cs arr slice) = do
      arr' <- properArr js arr
      arr_t <- lookupType arr'
      arr'' <-
        if arraySize 0 arr_t == w
          then pure arr'
          else
            certifying cs . letExp (baseString arr ++ "_prefix") . BasicOp . Index arr' $
              fullSlice arr_t [DimSlice (intConst Int64 0) w (intConst Int64 1)]
      arr_elem_param <- newParam (baseString arr ++ "_elem") (rowType arr_t)
      pure $
        Just
          ( arr'',
            arr_elem_param,
            ( pat,
              ArrayIndexing cs (paramName arr_elem_param) (Slice (drop (length js + 1) (unSlice slice)))
            )
          )
    mapOverArr _ _ = pure Nothing
simplifyMapIota _ _ _ _ = Skip

-- If a Screma's map function contains a transformation
-- (e.g. transpose) on a parameter, create a new parameter
-- corresponding to that transformation performed on the rows of the
-- full array.
moveTransformToInput :: TopDownRuleOp (Wise SOACS)
moveTransformToInput vtable screma_pat aux soac@(Screma w arrs (ScremaForm map_lam scan reduce))
  | ops <- filter arrayIsMapParam $ S.toList $ arrayOps mempty $ lambdaBody map_lam,
    not $ null ops = Simplify $ do
      (more_arrs, more_params, replacements) <-
        unzip3 . catMaybes <$> mapM mapOverArr ops

      when (null more_arrs) cannotSimplify

      let map_lam' =
            map_lam
              { lambdaParams = lambdaParams map_lam <> more_params,
                lambdaBody = replaceArrayOps (M.fromList replacements) $ lambdaBody map_lam
              }

      auxing aux . letBind screma_pat . Op $
        Screma w (arrs <> more_arrs) (ScremaForm map_lam' scan reduce)
  where
    -- It is not safe to move the transform if the root array is being
    -- consumed by the Screma.  This is a bit too conservative - it's
    -- actually safe if we completely replace the original input, but
    -- this rule is not that precise.
    consumed = consumedInOp soac
    map_param_names = map paramName (lambdaParams map_lam)
    topLevelPat = (`elem` fmap stmPat (bodyStms (lambdaBody map_lam)))
    onlyUsedOnce arr =
      case filter ((arr `nameIn`) . freeIn) $ stmsToList $ bodyStms $ lambdaBody map_lam of
        _ : _ : _ -> False
        _ -> True

    -- It's not just about whether the array is a parameter;
    -- everything else must be map-invariant.
    arrayIsMapParam (pat', ArrayIndexing cs arr slice) =
      arr `elem` map_param_names
        && all (`ST.elem` vtable) (namesToList $ freeIn cs <> freeIn slice)
        && not (null slice)
        && (not (null $ sliceDims slice) || (topLevelPat pat' && onlyUsedOnce arr))
    arrayIsMapParam (_, ArrayRearrange cs arr perm) =
      arr `elem` map_param_names
        && all (`ST.elem` vtable) (namesToList $ freeIn cs)
        && not (null perm)
    arrayIsMapParam (_, ArrayReshape cs arr new_shape) =
      arr `elem` map_param_names
        && all (`ST.elem` vtable) (namesToList $ freeIn cs <> freeIn new_shape)
    arrayIsMapParam (_, ArrayCopy cs arr) =
      arr `elem` map_param_names
        && all (`ST.elem` vtable) (namesToList $ freeIn cs)
    arrayIsMapParam (_, ArrayVar {}) =
      False

    mapOverArr (pat, op)
      | Just (_, arr) <- find ((== arrayOpArr op) . fst) (zip map_param_names arrs),
        arr `notNameIn` consumed = do
          arr_t <- lookupType arr
          let whole_dim = DimSlice (intConst Int64 0) (arraySize 0 arr_t) (intConst Int64 1)
          arr_transformed <- certifying (arrayOpCerts op) $
            letExp (baseString arr ++ "_transformed") $
              case op of
                ArrayIndexing _ _ (Slice slice) ->
                  BasicOp $ Index arr $ Slice $ whole_dim : slice
                ArrayRearrange _ _ perm ->
                  BasicOp $ Rearrange (0 : map (+ 1) perm) arr
                ArrayReshape _ _ new_shape ->
                  BasicOp $ Reshape (reshapeInner w new_shape) arr
                ArrayCopy {} ->
                  BasicOp $ Replicate mempty $ Var arr
                ArrayVar {} ->
                  BasicOp $ SubExp $ Var arr
          arr_transformed_t <- lookupType arr_transformed
          arr_transformed_row <- newVName $ baseString arr ++ "_transformed_row"
          pure $
            Just
              ( arr_transformed,
                Param mempty arr_transformed_row (rowType arr_transformed_t),
                (pat, ArrayVar mempty arr_transformed_row)
              )
    mapOverArr _ = pure Nothing
moveTransformToInput _ _ _ _ =
  Skip

-- The idea behidn this rule is to tak cases such as
--
--   let ...A... =
--     map (\x -> ...
--              let x = ...
--              ...
--              let y = f(x)
--              ...
--              in ...y ...)
--
-- where 'f' is some transformation like a reshape, and move it out
-- such that we get
--
--   let ...A'... =
--     map (\x -> ...
--              let x = ...
--              ...
--              in ...x ...)
--   let A' = f'(A')
--
-- This can improve simplification in case A' fuses or simplifies with
-- something else.
--
-- TODO: currently we only handle reshapes here, but the principle
-- should actually hold for any ArrayTransform.
moveTransformToOutput :: TopDownRuleOp (Wise SOACS)
moveTransformToOutput vtable screma_pat screma_aux (Screma w arrs (ScremaForm map_lam scan reduce))
  | (transformed, map_infos, stms') <-
      foldl' onStm ([], zip3 map_res map_rets map_pes, mempty) $ bodyStms $ lambdaBody map_lam,
    (map_res', map_rets', map_pes') <- unzip3 map_infos,
    not $ null transformed = Simplify $ do
      (tr_res, tr_rets, tr_names, post) <- unzip4 <$> mapM mkTransformed transformed
      let map_lam' =
            map_lam
              { lambdaBody = mkBody stms' $ nonmap_res <> map_res' <> tr_res,
                lambdaReturnType = nonmap_rets <> map_rets' <> tr_rets
              }
          pat_names = map patElemName (nonmap_pes <> map_pes') <> tr_names
      auxing screma_aux . letBindNames pat_names . Op $
        Screma w arrs (ScremaForm map_lam' scan reduce)
      sequence_ post
  where
    num_nonmap_res = scanResults scan + redResults reduce
    (nonmap_pes, map_pes) =
      splitAt num_nonmap_res $ patElems screma_pat
    (nonmap_rets, map_rets) =
      splitAt num_nonmap_res $ lambdaReturnType map_lam
    (nonmap_res, map_res) =
      splitAt num_nonmap_res $ bodyResult $ lambdaBody map_lam

    scope = scopeOf $ bodyStms $ lambdaBody map_lam

    invariantToMap = all (`ST.elem` vtable) . namesToList . freeIn

    onStm (transformed, map_infos, stms) (Let (Pat [pe]) aux (BasicOp (Reshape new_shape arr)))
      | ([(res, _, screma_pe)], map_pesres') <- partition matches map_infos,
        Just t <- typeOf <$> M.lookup arr scope,
        invariantToMap (t, new_shape) =
          let cs = stmAuxCerts aux <> resCerts res
              transform = (arr, cs, BasicOp . Reshape (reshapeInner w new_shape))
           in ((t, screma_pe, transform) : transformed, map_pesres', stms)
      where
        matches (r, _, _) = resSubExp r == Var (patElemName pe)
    onStm (transformed, map_infos, stms) stm =
      (transformed, map_infos, stms <> oneStm stm)

    mkTransformed (t, pe, (arr, cs, f)) = do
      v <- newVName (baseString (patElemName pe) <> "_pretr")
      let bind = letBindNames [patElemName pe] $ f v
      pure (SubExpRes cs (Var arr), t, v, bind)
moveTransformToOutput _ _ _ _ =
  Skip
