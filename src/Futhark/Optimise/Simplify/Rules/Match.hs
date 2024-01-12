{-# LANGUAGE TypeFamilies #-}

-- | Match simplification rules.
module Futhark.Optimise.Simplify.Rules.Match (matchRules) where

import Control.Monad
import Data.Either
import Data.List (partition, transpose, unzip4, zip5)
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.Construct
import Futhark.IR
import Futhark.Optimise.Simplify.Rule
import Futhark.Util

-- Does this case always match the scrutinees?
caseAlwaysMatches :: [SubExp] -> Case a -> Bool
caseAlwaysMatches ses = and . zipWith match ses . casePat
  where
    match se (Just v) = se == Constant v
    match _ Nothing = True

-- Can this case never match the scrutinees?
caseNeverMatches :: [SubExp] -> Case a -> Bool
caseNeverMatches ses = or . zipWith impossible ses . casePat
  where
    impossible (Constant v1) (Just v2) = v1 /= v2
    impossible _ _ = False

ruleMatch :: (BuilderOps rep) => TopDownRuleMatch rep
-- Remove impossible cases.
ruleMatch _ pat _ (cond, cases, defbody, ifdec)
  | (impossible, cases') <- partition (caseNeverMatches cond) cases,
    not $ null impossible =
      Simplify $ letBind pat $ Match cond cases' defbody ifdec
-- Find new default case.
ruleMatch _ pat _ (cond, cases, _, ifdec)
  | (always_matches, cases') <- partition (caseAlwaysMatches cond) cases,
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
    MatchDec ts _
    )
    | null tstms,
      null fstms,
      [Prim Bool] <- map extTypeOf ts =
        Simplify $ certifying (tcs <> fcs) $ letBind pat $ BasicOp $ BinOp LogOr cond se
-- When type(x)==bool, if c then x else y == (c && x) || (!c && y)
ruleMatch _ pat _ ([cond], [Case [Just (BoolValue True)] tb], fb, MatchDec ts _)
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
ruleMatch _ pat _ (_, [Case _ tbranch], _, MatchDec _ MatchFallback)
  | all (safeExp . stmExp) $ bodyStms tbranch = Simplify $ do
      let ses = bodyResult tbranch
      addStms $ bodyStms tbranch
      sequence_
        [ certifying cs $ letBindNames [patElemName p] $ BasicOp $ SubExp se
          | (p, SubExpRes cs se) <- zip (patElems pat) ses
        ]
ruleMatch _ pat _ ([cond], [Case [Just (BoolValue True)] tb], fb, _)
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
ruleMatch vtable (Pat [pe]) aux (_c, [Case _ tb], fb, MatchDec [_] _)
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
hoistBranchInvariant :: (BuilderOps rep) => TopDownRuleMatch rep
hoistBranchInvariant _ pat _ (cond, cases, defbody, MatchDec ret ifsort) =
  let case_reses = map (bodyResult . caseBody) cases
      defbody_res = bodyResult defbody
      (hoistings, (pes, ts, case_reses_tr, defbody_res')) =
        (fmap unzip4 . partitionEithers) . map branchInvariant $
          zip5 [0 ..] (patElems pat) ret (transpose case_reses) defbody_res
   in if null hoistings
        then Skip
        else Simplify $ do
          ctx_fixes <- sequence hoistings
          let onCase (Case vs body) case_res = Case vs $ body {bodyResult = case_res}
              cases' = zipWith onCase cases $ transpose case_reses_tr
              defbody' = defbody {bodyResult = defbody_res'}
              ret' = foldr (uncurry fixExt) ts ctx_fixes
          -- We may have to add some reshapes if we made the type
          -- less existential.
          cases'' <- mapM (traverse $ reshapeBodyResults $ map extTypeOf ret') cases'
          defbody'' <- reshapeBodyResults (map extTypeOf ret') defbody'
          letBind (Pat pes) $ Match cond cases'' defbody'' (MatchDec ret' ifsort)
  where
    bound_in_branches =
      namesFromList . concatMap (patNames . stmPat) $
        foldMap (bodyStms . caseBody) cases <> bodyStms defbody

    branchInvariant (i, pe, t, case_reses, defres)
      -- If just one branch has a variant result, then we give up.
      | namesIntersect bound_in_branches $ freeIn $ defres : case_reses =
          noHoisting
      -- Do all branches return the same value?
      | all ((== resSubExp defres) . resSubExp) case_reses = Left $ do
          certifying (foldMap resCerts case_reses <> resCerts defres) $
            letBindNames [patElemName pe] . BasicOp . SubExp $
              resSubExp defres
          hoisted i pe

      -- Do all branches return values that are free in the
      -- branch, and are we not the only pattern element?  The
      -- latter is to avoid infinite application of this rule.
      | not $ namesIntersect bound_in_branches $ freeIn $ defres : case_reses,
        patSize pat > 1,
        Prim _ <- patElemType pe = Left $ do
          bt <- expTypesFromPat $ Pat [pe]
          letBindNames [patElemName pe]
            =<< ( Match cond
                    <$> ( zipWith Case (map casePat cases)
                            <$> mapM (resultBodyM . pure . resSubExp) case_reses
                        )
                    <*> resultBodyM [resSubExp defres]
                    <*> pure (MatchDec bt ifsort)
                )
          hoisted i pe
      | otherwise = noHoisting
      where
        noHoisting = Right (pe, t, case_reses, defres)

    hoisted i pe = pure (i, Var $ patElemName pe)

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
removeDeadBranchResult :: (BuilderOps rep) => BottomUpRuleMatch rep
removeDeadBranchResult (_, used) pat _ (cond, cases, defbody, MatchDec rettype ifsort)
  | -- Figure out which of the names in 'pat' are used...
    patused <- map keep $ patNames pat,
    -- If they are not all used, then this rule applies.
    not (and patused) = do
      -- Remove the parts of the branch-results that correspond to dead
      -- return value bindings.  Note that this leaves dead code in the
      -- branch bodies, but that will be removed later.
      let pick :: [a] -> [a]
          pick = map snd . filter fst . zip patused
          pat' = pick $ patElems pat
          rettype' = pick rettype
          -- We also need to adjust the existential references in the
          -- branch type.
          exts = scanl (+) 0 [if b then 1 else 0 | b <- patused]
          adjust = mapExt (exts !!)
      Simplify $ do
        cases' <- mapM (traverse $ onBody pick) cases
        defbody' <- onBody pick defbody
        letBind (Pat pat') $ Match cond cases' defbody' $ MatchDec (map adjust rettype') ifsort
  | otherwise = Skip
  where
    usedDirectly v = v `UT.isUsedDirectly` used
    usedIndirectly v =
      any
        (\pe -> v `nameIn` freeIn pe && usedDirectly (patElemName pe))
        (patElems pat)
    keep v = usedDirectly v || usedIndirectly v

    onBody pick (Body _ stms res) = mkBodyM stms $ pick res

topDownRules :: (BuilderOps rep) => [TopDownRule rep]
topDownRules =
  [ RuleMatch ruleMatch,
    RuleMatch hoistBranchInvariant
  ]

bottomUpRules :: (BuilderOps rep) => [BottomUpRule rep]
bottomUpRules =
  [ RuleMatch removeDeadBranchResult
  ]

matchRules :: (BuilderOps rep) => RuleBook rep
matchRules = ruleBook topDownRules bottomUpRules
