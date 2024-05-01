module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Debug.Trace (trace, traceM)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Futhark.MonadFreshNames

normalise :: IndexFn -> IndexFnM IndexFn
normalise indexfn =
  pure $ toNNF' $ idMap m indexfn
  where
    toNNF' (IndexFn i (Cases cs)) =
      IndexFn i (Cases (NE.map (bimap toNNF toNNF) cs))
    m =
      ASTMapper
        { mapOnTerm = normTerm,
          mapOnVName = pure
        }
    normTerm (Var x) = pure $ Var x
    normTerm (x :&& y) = do
      x' <- normTerm x
      y' <- normTerm y
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
    normTerm (x :|| y) = do
      x' <- normTerm x
      y' <- normTerm y
      case (x', y') of
        (Bool True, _) -> pure (Bool True)
        (_, Bool True) -> pure (Bool True)
        (Bool False, b) -> pure b
        (a, Bool False) -> pure a
        (a, b) -> pure $ a :|| b
    normTerm (Sum j lb ub x) = do
      lb' <- astMap m lb
      ub' <- astMap m ub
      x' <- astMap m x
      -- List of (multiplied symbols, factor) typed [([Term], Integer)].
      let terms = SoP.sopToLists x'
      -- Create a sum for each product (term) in the sum of products,
      -- pulling factors outside the sum.
      -- XXX this craziness exemplifies why Sum should probably be SumSlice...
      let sums = map (\(symbs, c) ->
                        let term = SoP.term2SoP symbs 1
                        in  case SoP.justConstant term of
                              Just k ->
                                -- This would be c*∑j∈[lb, ..., ub] k so rewrite it to (ub - lb + 1) * c * k.
                                SoP.scaleSoP c (SoP.scaleSoP k (ub' SoP..-. lb' SoP..+. SoP.int2SoP 1))
                              _ ->
                                SoP.scaleSoP c . termToSoP $ Sum j lb ub term
                     ) terms
      -- Create sum of products of (sums of sum of products).
      pure $ SoP2 $ foldl1 (SoP..+.) sums
    normTerm x@(SoP2 _) = do
      x' <- astMap m x
      case x' of
        SoP2 sop -> pure . SoP2 . normaliseNegation . mergeSums $ sop
        _ -> pure x'
      where
        -- TODO extend this to find any 1 + -1*[[c]] without them being adjacent
        -- or the only terms.
        normaliseNegation sop -- [[not c]] => 1 + -1*[[c]]
          | [([Indicator (Not c)], 1)] <- getSoP sop =
            SoP.int2SoP 1 SoP..-. SoP.sym2SoP (Indicator c)
          -- | [([], 1), ([Indicator (Not c)], -1)] <- getSoP sop =
        normaliseNegation sop = sop

        -- Takes a sum of products which may have Sum terms and merges other
        -- compatible terms into those Sums. Time complexity is quadratic in
        -- the number of terms in the SoP.
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

        -- Rewrite sum_{j=lb}^ub e(j) + e(lb+1) ==> sum_{j=lb}^{ub+1} e(j).
        -- Relies on sorting of SoP and Term to match.
        mergeUb ([Sum j lb ub e1], c) ([e2], c')
          | c == c' =
            let ubp1 = ub SoP..+. SoP.int2SoP 1
            in  if substituteName j (SoP2 ubp1) e1 == termToSoP e2
                then Just ([Sum j lb ubp1 e1], c)
                else Nothing
        mergeUb _ _ = Nothing
        -- Rewrite sum_{j=lb}^ub e(j) + e(lb-1) ==> sum_{j=lb-1}^ub e(j).
        -- Relies on sorting of SoP and Term to match.
        mergeLb ([Sum j lb ub e1], c) ([e2], c')
          | c == c' =
            let lbm1 = lb SoP..-. SoP.int2SoP 1
            in  if substituteName j (SoP2 lbm1) e1 == termToSoP e2
                then Just ([Sum j lbm1 ub e1], c)
                else Nothing
        mergeLb _ _ = Nothing

        -- -- Rewrite sum e[lb:ub] + e[ub+1] ==> sum e[lb:ub+1].
        -- -- Relies on sorting of SoP and Term to match.
        -- merge ([SumSlice x lb ub], 1) ([Idx y i], 1)
        --   | x == y,
        --     i == ub SoP..+. SoP.int2SoP 1 =
        --       Just ([SumSlice x lb i], 1)
        -- merge ([SumSlice x lb ub], 1) ([Idx y i], 1)
        --   | x == y,
        --     i == lb SoP..-. SoP.int2SoP 1 =
        --       Just ([SumSlice x i ub], 1)
        -- merge _ _ = Nothing
    normTerm v = astMap m v

simplify :: IndexFn -> IndexFnM IndexFn
simplify indexfn =
  normalise indexfn
  >>= removeDeadCases
  >>= simplifyRule3
  >>= removeDeadCases
  >>= normalise

removeDeadCases :: IndexFn -> IndexFnM IndexFn
removeDeadCases (IndexFn it (Cases cases))
  | xs <- NE.filter f cases,
    not $ null xs,
    length xs /= length cases = -- Something actualy got removed.
  trace "👀 Removing dead cases" $
    pure $ IndexFn it $ Cases (NE.fromList xs)
  where
    f (Bool False, _) = False
    f _ = True
removeDeadCases indexfn = pure indexfn

-- TODO Maybe this should only apply to | True => 1 | False => 0
-- (and its negation)?
-- Applies if all case values are integer constants.
simplifyRule3 :: IndexFn -> IndexFnM IndexFn
simplifyRule3 v@(IndexFn _ (Cases ((Bool True, _) NE.:| []))) = pure v
simplifyRule3 (IndexFn it (Cases cases))
  | Just sops <- mapM (justConstant . snd) cases =
  let preds = NE.map fst cases
      sumOfIndicators =
        SoP.normalize . foldl1 (SoP..+.) . NE.toList $
          NE.zipWith
            (\p x -> SoP.sym2SoP (Indicator p) SoP..*. SoP.int2SoP x)
            preds
            sops
  in  trace "👀 Using Simplification Rule 3" $
        pure $ IndexFn it $ Cases (NE.singleton (Bool True, SoP2 sumOfIndicators))
  where
    justConstant (SoP2 sop) = SoP.justConstant sop
    justConstant _ = Nothing
simplifyRule3 v = pure v


rewriteRule4 :: IndexFn -> IndexFnM IndexFn
-- Rule 4 (recursive sum)
--
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    | i == b => e1              (e1 may depend on i)
--    | i /= b => y[i-1] + e2     (e2 may depend on i)
-- _______________________________________________________________
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . e1{b/i} + (Σ_{j=b+1}^i e2{j/i})
--
-- If e1{b/i} == e2{j/i}, it later simplifies to
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . (Σ_{j=b}^i e2{j/i})
rewriteRule4 (IndexFn it@(Forall i'' (Iota _)) (Cases cases))
  | (Var i :== b, e1) :| [(Not (Var i' :== b'), x)] <- cases,
    -- Extract terms (multiplied symbols, factor) from the sum of products.
    Just e2 <- justTermPlusRecurence x,
    i == i',
    i == i'',
    b == SoP2 (SoP.int2SoP 0), -- Domain is iota so b must be 0.
    b == b' = do
      traceM "👀 Using Rule 4 (recursive sum)"
      let lb = termToSoP b SoP..+. SoP.int2SoP 1
      let ub = SoP.sym2SoP (Var i)
      j <- newNameFromString "j"
      let e1' = substituteName i b e1
      let e2' = substituteName i (Var j) e2
      pure $ IndexFn it (toCases $ e1' ~+~ Sum j lb ub e2')
  where
    justTermPlusRecurence :: Term -> Maybe (SoP.SoP Term)
    justTermPlusRecurence (SoP2 sop)
      | termsWithFactors <- SoP.sopToLists sop,
        [Recurrence] `elem` map fst termsWithFactors =
          Just . SoP.sopFromList $ filter (\(ts,_) -> ts /= [Recurrence]) termsWithFactors
    justTermPlusRecurence _ = Nothing
-- rewriteRule4 (IndexFn it@(Forall i'' (Iota _)) (Cases cases))
--   | (Var i :== b, e) :| [(Not (Var i' :== b'), x)] <- cases,
--     -- Extract terms (multiplied symbols, factor) from the sum of products.
--     Just terms <- justTermPlusRecurence x,
--     i == i',
--     i == i'',
--     b == SoP2 (SoP.int2SoP 0), -- Domain is iota so b must be 0.
--     b == b' = do
--       traceM "👀 Using Rule 4 (recursive sum)"
--       let lb = termToSoP b SoP..+. SoP.int2SoP 1
--       let ub = SoP.sym2SoP (Var i)
--       j <- newNameFromString "j"
--       let base = substituteName i b e
--       -- Create a sum for each product (term) in the sum of products,
--       -- pulling factors outside the sum.
--       -- XXX this craziness exemplifies why Sum should probably be SumSlice...
--       let sums = map (\(symbs, c) ->
--                         SoP2 . SoP.scaleSoP c . termToSoP $
--                           Sum j lb ub $
--                             substituteName i (Var j) $
--                               SoP.term2SoP symbs 1
--                      ) terms
--       pure $ IndexFn it (toCases $ base ~+~ foldl1 (~+~) sums)
--   where
--     justTermPlusRecurence :: Term -> Maybe [([Term], Integer)]
--     justTermPlusRecurence (SoP2 sop)
--       | termsWithFactors <- SoP.sopToLists sop,
--         [Recurrence] `elem` map fst termsWithFactors =
--           Just $ filter (\(ts,_) -> ts /= [Recurrence]) termsWithFactors
--     justTermPlusRecurence _ = Nothing
rewriteRule4 indexfn = pure indexfn

rewrite :: IndexFn -> IndexFnM IndexFn
rewrite indexfn =
  simplify indexfn >>=
  rewriteRule4 >>=
  simplify
