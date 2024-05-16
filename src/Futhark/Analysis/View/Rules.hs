module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Futhark.Analysis.View.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Data.Set (notMember)
import Control.Monad.RWS hiding (Sum)
import Futhark.MonadFreshNames (newNameFromString)

normalise :: (Applicative f, ASTMappable a) => a -> f a
normalise indexfn =
  pure $ idMap m indexfn
  where
    m =
      ASTMapper
        { mapOnTerm = normTerm,
          mapOnVName = pure
        }
    normTerm (Var x) = pure $ Var x
    normTerm (Not x) = do
      x' <- normTerm x
      pure $ toNNF (Not x')
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
        (a, b) | a == toNNF (Not b) -> pure (Bool True) -- A tautology.
        (a, b) -> pure $ a :|| b
    normTerm (Indicator c) = do
      c' <- normTerm c
      case c' of
        -- [[not x]] => 1 + -1*[[x]]
        Not x -> pure . SoP2 $ SoP.int2SoP 1 SoP..-. SoP.sym2SoP (Indicator x)
        _ -> pure (Indicator c')
    normTerm (Sum j lb ub x) = do
      lb' <- astMap m lb
      ub' <- astMap m ub
      x' <- astMap m x
      -- List of (multiplied symbols, factor) typed [([Term], Integer)].
      let terms = SoP.sopToLists x'
      -- Create a sum for each product (term) in the sum of products,
      -- pulling factors outside the sum.
      let sums = map (\(symbs, c) ->
                        let term = SoP.term2SoP symbs 1
                        in  case SoP.justConstant term of
                              Just k ->
                                -- This would be c*∑j∈[lb, ..., ub] k so rewrite it to (ub - lb + 1) * c * k.
                                SoP.scaleSoP c (SoP.scaleSoP k (ub' SoP..-. lb' SoP..+. SoP.int2SoP 1))
                              _ ->
                                SoP.scaleSoP c . termToSoP $ Sum j lb' ub' term
                     ) terms
      -- Create sum of products of (sums of sum of products).
      pure $ SoP2 $ foldl1 (SoP..+.) sums
    normTerm x@(SoP2 _) = do
      mergeSums <$> astMap m x
      where
        -- Takes a sum of products which may have Sum terms and merges other
        -- compatible terms into those Sums. Time complexity is quadratic in
        -- the number of terms in the SoP.
        -- mergeSums sop =
        --   SoP.sopFromList $ foldl (absorbTerm merge) [] (getSoP sop)
        mergeSums :: Term -> Term
        mergeSums (SoP2 s) = do
          SoP2 $ SoP.sopFromList (foldl absorbTerm [] (getSoP s))
        mergeSums t = t

        absorbTerm [] t2 = [t2]
        absorbTerm (t1:ts) t2
          | Just t' <- merge t1 t2 =
            t':ts
        absorbTerm (t1:ts) t2 = t1 : absorbTerm ts t2

        -- Rewrite sum y[lb:ub] + y[ub+1] ==> sum y[lb:ub+1].
        -- Relies on sorting of SoP and Term to match.
        merge ([Sum j lb ub e1], c) ([e2], c')
          | c == c',
            ubp1 <- ub SoP..+. SoP.int2SoP 1,
            substituteName j (SoP2 ubp1) e1 == termToSoP e2 =
              Just ([Sum j lb ubp1 e1], c)
        merge ([Sum j lb ub e1], c) ([e2], c')
          | c == c',
            lbm1 <- lb SoP..-. SoP.int2SoP 1,
            substituteName j (SoP2 lbm1) e1 == termToSoP e2 =
              Just ([Sum j lbm1 ub e1], c)
        merge _ _ = Nothing
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
  | Just sops <- mapM (justConstant . snd) cases = do
    let preds = NE.map fst cases
        sumOfIndicators =
          SoP.normalize . foldl1 (SoP..+.) . NE.toList $
            NE.zipWith
              (\p x -> SoP.sym2SoP (Indicator p) SoP..*. SoP.int2SoP x)
              preds
              sops
    tell ["Using simplification rule: integer-valued cases"]
    pure $ IndexFn it $ Cases (NE.singleton (Bool True, SoP2 sumOfIndicators))
  where
    justConstant (SoP2 sop) = SoP.justConstant sop
    justConstant _ = Nothing
simplifyRule3 v = pure v


-- Check that SoP is a linear combination of symbols (possibly with a
-- constant term) and return a list of it's terms.
isAffineSoP :: Ord a => SoP.SoP a -> Bool
isAffineSoP sop = all ((<= 1) . length . fst) (SoP.sopToLists sop)

domainSegStart :: Domain -> Term
domainSegStart (Iota n) = domainStart (Iota n)
domainSegStart (Cat _k _m seg_start) = seg_start


rewritePrefixSum :: IndexFn -> IndexFnM IndexFn
-- Rule 4 (prefix sum)
--
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    | i == b => e1              (e1 may depend on i)
--    | i /= b => y[i-1] + e2     (e2 may depend on i)
--
-- e2 is an affine SoP where each term is either a constant,
-- an indexing statement or an indicator of an indexing statement.
-- Below, we write e2.0, ..., e2.l for each term.
-- TODO doesn't need to be affine
-- _______________________________________________________________
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    e1{b/i} + (Σ_{j=b+1}^i e2.0{j/i}) + ... + (Σ_{j=b+1}^i e2.l{j/i})
rewritePrefixSum (IndexFn it@(Forall i'' dom) (Cases cases))
  | (Var i :== b, e1) :| [(Var i' :/= b', recur)] <- cases,
    -- Extract terms (multiplied symbols, factor) from the sum of products.
    Just terms <- justAffinePlusRecurrence recur,
    -- Create a sum for each product (term) in the sum of products.
    i == i',
    i == i'',
    b == b',
    b == domainSegStart dom = do
      tell ["Using prefix sum rule"]
      -- let lb = termToSoP b SoP..+. SoP.int2SoP 1
      -- let ub = termToSoP (Var i)
      -- j <- newNameFromString "j"
      -- let e2 = substituteName i (Var j) $ SoP.sopFromList terms
      -- pure $ IndexFn it (toCases $ e1' ~+~ Sum j lb ub e2)
      let e1' = substituteName i b e1
      sums <- mapM (mkSum i b) terms
      pure $ IndexFn it (toCases $ e1' ~+~ SoP2 (foldl1 (SoP..+.) sums))
  where
    justAffinePlusRecurrence :: Term -> Maybe [([Term], Integer)]
    justAffinePlusRecurrence (SoP2 sop)
      | termsWithFactors <- SoP.sopToLists (SoP.padWithZero sop),
        [Recurrence] `elem` map fst termsWithFactors,
        isAffineSoP sop =
          Just $ filter (\(ts,_) -> ts /= [Recurrence]) termsWithFactors
    justAffinePlusRecurrence _ = Nothing

    -- Create sum for (term, factor), pulling the factor outside the sum.
    -- mkSum :: a -> Term -> ([Term], Integer)
    mkSum i b ([], c) =
      -- This would be ∑j∈[lb, ..., ub] c so rewrite it to (ub - lb + 1) * c.
      let lb = termToSoP b SoP..+. SoP.int2SoP 1
          ub = termToSoP (Var i)
      in  pure $ SoP.scaleSoP c (ub SoP..-. lb SoP..+. SoP.int2SoP 1)
    mkSum i b ([Idx (Var x) idx], c) = do
      let lb = substituteName i (SoP2 $ termToSoP b SoP..+. SoP.int2SoP 1) idx
      j <- newNameFromString "j"
      pure . SoP.scaleSoP c . termToSoP $
        Sum j lb idx (termToSoP $ Idx (Var x) (SoP.sym2SoP (Var j)))
    mkSum i b ([Indicator (Idx (Var x) idx)], c) = do
      let lb = substituteName i (SoP2 $ termToSoP b SoP..+. SoP.int2SoP 1) idx
      j <- newNameFromString "j"
      pure . SoP.scaleSoP c . termToSoP $
        Sum j lb idx (termToSoP . Indicator $ Idx (Var x) (SoP.sym2SoP (Var j)))
    mkSum i b (es, c) = do
      let lb = termToSoP b SoP..+. SoP.int2SoP 1
          ub = termToSoP (Var i)
      j <- newNameFromString "j"
      let es' = substituteName i (Var j) $ SoP.sopFromList [(es, 1)]
      pure . SoP.scaleSoP c . termToSoP $ Sum j lb ub es'
rewritePrefixSum indexfn = pure indexfn

rewriteCarry :: IndexFn -> IndexFnM IndexFn
-- Rule 5 (carry)
--
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] .
--    | i == b => e1              (e1 may NOT depend on i)
--    | i /= b => y[i-1]
--
-- Note that e1 could depend on, for example, k in
-- i ∈ ⊎k₆₂₁₄=iota m₆₀₆₉ [b, b+1, ..., b + n - 1].
-- _______________________________________________________________
-- y = ∀i ∈ [b, b+1, ..., b + n - 1] . c
rewriteCarry (IndexFn it@(Forall i'' dom) (Cases cases))
  | (Var i :== b, c) :| [(Var i' :/= b', Recurrence)] <- cases,
    i == i',
    i == i'',
    b == b',
    b == domainSegStart dom,
    i `notMember` freeIn c = do
      tell ["Using carry rule"]
      pure $ IndexFn it (toCases c)
rewriteCarry indexfn = pure indexfn

rewrite :: IndexFn -> IndexFnM IndexFn
rewrite indexfn =
  simplify indexfn >>=
  rewritePrefixSum >>=
  simplify >>=
  rewriteCarry >>=
  simplify
