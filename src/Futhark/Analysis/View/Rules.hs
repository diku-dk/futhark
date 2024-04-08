module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Debug.Trace (trace, traceM)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Futhark.MonadFreshNames
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
        SoP sop -> pure . SoP . normaliseNegation . mergeSums $ sop
        _ -> pure x'
      where
        -- TODO extend this to find any 1 + -1*[[c]] without them being adjacent
        -- or the only terms.
        normaliseNegation sop -- 1 + -1*[[c]] => [[not c]]
          | [([], 1), ([Indicator c], -1)] <- getSoP sop =
            SoP.sym2SoP $ Indicator (Not c)
        normaliseNegation sop = sop

        -- Takes a sum of products which may have Sum terms and tries to absorb terms into
        -- those Sums. Time complexity is quadratic in the number of terms in the SoP.
        mergeSums sop =
          let sop' = getSoP sop
          in SoP.sopFromList $
               foldl (absorbTerm (applyFirstMatch [mergeUb, mergeLb])) [] sop'

        absorbTerm _ [] term = [term]
        absorbTerm f (t:ts) term
          | Just t' <- f t term =
            t':ts
        absorbTerm f (t:ts) term = t : absorbTerm f ts term

        -- Naively apply the first function with a Just result
        -- from a list of functions.
        applyFirstMatch :: [a -> a -> Maybe a] -> a -> a -> Maybe a
        applyFirstMatch [] _ _ = Nothing
        applyFirstMatch (f:fs) x y =
          case f x y of
            Nothing -> applyFirstMatch fs x y
            z -> z

        -- Rewrite sum_{j=lb}^ub e[j] + e[ub+1] ==> sum_{j=lb}^{ub+1} e[j].
        -- Relies on sorting of SoP and Exp to match.
        mergeUb ([Sum (Var j) lb (SoP ub) e1], 1) ([e2], 1) =
            let ubp1 = SoP $ ub SoP..+. SoP.int2SoP 1
            in  if substituteNames' (M.singleton j ubp1) e1 == e2
                then Just ([Sum (Var j) lb ubp1 e1], 1)
                else Nothing
        mergeUb _ _ = Nothing

        -- Rewrite sum_{j=lb}^ub e[j] + e[lb-1] ==> sum_{j=lb-1}^ub e[j].
        -- Relies on sorting of SoP and Exp to match.
        mergeLb ([Sum (Var j) (SoP lb) ub e1], 1) ([e2], 1) =
            let lbm1 = SoP $ lb SoP..-. SoP.int2SoP 1
            in  if substituteNames' (M.singleton j lbm1) e1 == e2
                then Just ([Sum (Var j) lbm1 ub e1], 1)
                else Nothing
        mergeLb _ _ = Nothing
    normExp v = astMap m v

simplify :: View -> ViewM View
simplify view =
  removeDeadCases view
  >>= simplifyRule3
  >>= removeDeadCases
  >>= normalise

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


rewriteRule4 :: View -> ViewM View
-- Rule 4 (recursive sum)
--
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    | i == b => e              (e may depend on i)
--    | i /= b => y[i-1] ⊕ x[i]
-- _______________________________________________________________
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . e{b/i} ⊕ (⊕_{j=b+1}^i x[j])
--
-- If e{b/i} happens to be x[b] it later simplifies to
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . (⊕_{j=b}^i x[j])
rewriteRule4 (View it@(Forall i'' (Iota _)) (Cases cases))
  | (Var i :== b, e) :| [(Not (Var i' :== b'), x)] <- cases,
    i == i'',
    i == i',
    b == b',
    b == SoP (SoP.int2SoP 0), -- Domain is iota so b must be 0.
    Just x' <- justTermPlusRecurence x = do
      traceM "👀 Using Rule 4 (recursive sum)"
      j <- Var <$> newNameFromString "j"
      let lb = b ~+~ SoP (SoP.int2SoP 1)
      let ub = Var i
      base <- substituteName i b e
      x'' <- substituteName i j x'
      pure $ View it (toCases $ base ~+~ Sum j lb ub x'')
  where
    justTermPlusRecurence :: Exp -> Maybe Exp
    justTermPlusRecurence (SoP sop)
      | [([x], 1), ([Recurrence], 1)] <- getSoP sop =
          Just x
    justTermPlusRecurence _ = Nothing
rewriteRule4 view = pure view

rewrite :: View -> ViewM View
rewrite view =
  simplify view >>=
  rewriteRule4 >>=
  simplify

toNNF' :: View -> View
toNNF' (View i (Cases cs)) =
  View i (Cases (NE.map (bimap toNNF toNNF) cs))
