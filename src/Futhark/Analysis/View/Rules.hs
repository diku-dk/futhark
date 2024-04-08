module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Debug.Trace (trace, traceM)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import qualified Data.Map as M

normalise :: View -> ViewM View
normalise view =
  pure $ toNNF' $ idMap m view
  where
    m =
      ASTMapper
        { mapOnExp = normExp }
    normExp (Var x) = pure $ Var x
    normExp (x :&& y) = do
      x' <- normExp x
      y' <- normExp y
      case (x', y') of
        (Bool True, b) -> pure b
        (a, Bool True) -> pure a
        (Bool False, _) -> pure (Bool False)
        (_, Bool False) -> pure (Bool False)
        (a, b) | a == b ->
          pure a
        (a, b) | a == toNNF (Not b) -> -- A contradiction.
          pure (Bool False)
        (a, b) ->
          pure $ a :&& b
    normExp (x :|| y) = do
      x' <- normExp x
      y' <- normExp y
      case (x', y') of
        (Bool True, _) -> pure (Bool True)
        (_, Bool True) -> pure (Bool True)
        (Bool False, b) -> pure b
        (a, Bool False) -> pure a
        (a, b) -> pure $ a :|| b
    normExp x@(SoP _) = do
      x' <- astMap m x
      case x' of
        SoP sop -> pure . SoP . normaliseNegation . mergeSumRanges $ sop
        _ -> pure x'
      where
        -- TODO extend this to find any 1 + -1*[[c]] without them being adjacent
        -- or the only terms.
        normaliseNegation sop -- 1 + -1*[[c]] => [[not c]]
          | [([], 1), ([Indicator c], -1)] <- getSoP sop =
            SoP.sym2SoP $ Indicator (Not c)
        normaliseNegation sop = sop

        mergeSumRanges sop =
          let sop' = getSoP sop
          in SoP.sopFromList $ foldl f [] sop'

        -- Apply merge at most once.
        f [] term = [term]
        f (t:ts) term
          | Just t' <- merge t term =
            t':ts
        f (t:ts) term = t : f ts term

        -- Merge
        --   sum_{j=lb}^ub e[j] + e[lb-1] ==> sum_{j=lb-1}^ub e[j].
        -- Specifically to clean up the Recursive Sum rule:
        --   sum_{j=1}^n e[j] + e[0] ==> sum_{j=0}^n e[j].
        merge ([Sum (Var j) (SoP lb) ub e1], 1) ([e2], 1) =
          -- | lb SoP..-. SoP.int2SoP 1 == lbm1,
          --   e1 == e2 = -- TODO replace j in e1 with lb - 1 for matching
            let lbm1 = SoP $ lb SoP..-. SoP.int2SoP 1
            in  if lel (M.singleton j lbm1) e1 == e2
                then Just ([Sum (Var j) lbm1 ub e1], 1)
                else Nothing
        -- Add two sums.
        -- merge ([Sum i lb1 ub1 e1], 1) ([Sum _j lb2 ub2 e2], 1)
        --   | lb2 == ub1,
        --     e1 == e2 = -- TODO replace j in e2 with i for matching
        --       Just ([Sum i lb1 ub2 e1], 1)
        merge a b =
          trace ("🪲 merge on: " <> prettyString a <> " and " <> prettyString b) $
            trace ("    " <> show a) $
              trace ("    " <> show b)
                Nothing
        -- merge _ _ = Nothing
    normExp v = astMap m v

simplify :: View -> ViewM View
simplify view =
  removeDeadCases view
  >>= simplifyRule3
  >>= removeDeadCases

removeDeadCases :: View -> ViewM View
removeDeadCases (View it (Cases cases))
  | xs <- NE.filter f cases,
    not $ null xs,
    length xs /= length cases = -- Something actualy got removed.
  trace "👀 Removing dead cases" $
    pure $ View it $ Cases (NE.fromList xs)
  where
    f (Bool False, _) = False
    f _ = True
removeDeadCases view = pure view

-- TODO Maybe this should only apply to | True => 1 | False => 0
-- (and its negation)?
-- Applies if all case values are integer constants.
simplifyRule3 :: View -> ViewM View
simplifyRule3 v@(View _ (Cases ((Bool True, _) NE.:| []))) = pure v
simplifyRule3 (View it (Cases cases))
  | Just sops <- mapM (justSoP . snd) cases = 
  let preds = NE.map fst cases
      sumOfIndicators =
        SoP.normalize . foldl1 (SoP..+.) . NE.toList $
          NE.zipWith
            (\p x -> SoP.sym2SoP (Indicator p) SoP..*. SoP.int2SoP x)
            preds
            sops
  in  trace "👀 Using Simplification Rule 3" $
        pure $ View it $ Cases (NE.singleton (Bool True, SoP sumOfIndicators))
  where
    justSoP (SoP sop) = SoP.justConstant sop
    justSoP _ = Nothing
simplifyRule3 v = pure v


-- View {iterator = Forall (VName (Name "i") 6204) (Iota (Var (VName (Name "n") 6068))),
--       value = Cases ((Idx (Var (VName (Name "conds") 6070)) (Var (VName (Name "i") 6204)),
--                        Sum (Var (VName (Name "j") 6200)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},1)]})) (Var (VName (Name "i") 6204)) (Indicator (Idx (Var (VName (Name "conds") 6070)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},-1),(Term {getTerm = fromOccurList [(Var (VName (Name "j") 6200),1)]},1)]})))))
--                     :| [(Not (Idx (Var (VName (Name "conds") 6070)) (Var (VName (Name "i") 6204))),
--                           SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},-1),(Term {getTerm = fromOccurList [(Sum (Var (VName (Name "j") 6200)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},1)]})) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},-1),(Term {getTerm = fromOccurList [(Var (VName (Name "n") 6068),1)]},1)]})) (Indicator (Idx (Var (VName (Name "conds") 6070)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},-1),(Term {getTerm = fromOccurList [(Var (VName (Name "j") 6200),1)]},1)]})))),1)]},1),(Term {getTerm = fromOccurList [(Sum (Var (VName (Name "j") 6202)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},1)]})) (Var (VName (Name "i") 6204)) (Indicator (Not (Idx (Var (VName (Name "conds") 6070)) (Var (VName (Name "j") 6202))))),1)]},1),(Term {getTerm = fromOccurList [(Indicator (Idx (Var (VName (Name "conds") 6070)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},-1),(Term {getTerm = fromOccurList [(Var (VName (Name "n") 6068),1)]},1)]}))),1)]},1),(Term {getTerm = fromOccurList [(Indicator (Not (Idx (Var (VName (Name "conds") 6070)) (SoP (SoP {getTerms = fromList [(Term {getTerm = fromOccurList []},0)]})))),1)]},1)]}))
--                         ])}

-- mergeSums :: View -> ViewM View
-- mergeSums (View _it (Cases cases))
--   | cases is a SoP =
--     -- merge sums in SoP if compatible; just do simple O(n^2) matching
--     undefined
-- mergeSums view = pure view


-- XXX Currently changing recursive sum rule to be indifferent to the
-- base case. If the base case is mergeable with the recursive
-- case, we merge it later based on sum merging rules.
rewrite :: View -> ViewM View
-- Rule 4 (recursive sum)
--
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    | i == b => e              (e may depend on i)
--    | i /= b => y[i-1] ⊕ x[i]
-- ____________________________________
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . e{b/i} ⊕ (⊕_{j=b+1}^i x[j])
--
-- If e{b/i} happens to be x[b] it later simplifies to
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . (⊕_{j=b}^i x[j])
rewrite (View it@(Forall i'' (Iota _)) (Cases cases))
  | (Var i :== b, x) :| [(Not (Var i' :== b'), y)] <- cases,
    i == i'',
    i == i',
    b == b',
    b == SoP (SoP.int2SoP 0), -- Domain is iota so b must be 0.
    Just x' <- justTermPlusRecurence y,
    x == x' || x == SoP (SoP.int2SoP 0) = do
      traceM "👀 Using Rule 4 (recursive sum)"
      j <- Var <$> newNameFromString "j"
      let lb = b ~+~ SoP (SoP.int2SoP 1)
      let ub = Var i
      base <- substituteName i b x
      z <- substituteName i j x'
      pure $ View it (toCases $ base ~+~ Sum j lb ub z)
  where
    justTermPlusRecurence :: Exp -> Maybe Exp
    justTermPlusRecurence (SoP sop)
      | [([x], 1), ([Recurrence], 1)] <- getSoP sop =
          Just x
    justTermPlusRecurence _ = Nothing
rewrite view = pure view

toNNF' :: View -> View
toNNF' (View i (Cases cs)) =
  View i (Cases (NE.map (bimap toNNF toNNF) cs))
