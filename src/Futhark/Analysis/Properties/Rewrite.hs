{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Rewrite (rewrite, rewriteWithoutRules, unifiesWith, solveIx) where

import Control.Monad (filterM, (<=<))
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraBridge (Answer (..), addRelDim, addRelShape, algebraContext, andM, assume, isFalse, isUnknown, simplify)
import Futhark.Analysis.Properties.IndexFn (Domain (Iota), IndexFn (..), Quantified (..), cases, casesToList)
import Futhark.Analysis.Properties.Monad (IndexFnM, prettyStr, printAlgEnv, printM, rollbackAlgEnv)
import Futhark.Analysis.Properties.Query ((=>?))
import Futhark.Analysis.Properties.Rule (Rule (..), applyRuleBook, rulesIndexFn)
import Futhark.Analysis.Properties.Symbol (Symbol (..), toCNF)
import Futhark.Analysis.Properties.Traversals
import Futhark.Analysis.Properties.Unify (Renameable, Substitution, Unify, fv, renameSame, sub, unify)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, filterSoP, int2SoP, isZero, justConstant, justSym, sym2SoP, term2SoP, (.*.), (.+.), (.-.), (./.))
import Futhark.Util.Pretty (Pretty)
import Language.Futhark (VName)

class (Monad m) => Rewritable v m where
  rewrite :: v -> m v

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = convergeRename simplify

instance Rewritable Symbol IndexFnM where
  rewrite = convergeRename simplify

instance Rewritable IndexFn IndexFnM where
  rewrite = convergeRename (rewrite_ <=< applyRuleBook rulesIndexFn)

convergeRename :: (Renameable t, Eq t) => (t -> IndexFnM t) -> t -> IndexFnM t
convergeRename f x = do
  y <- f x
  (x', y') <- renameSame x y
  if x' == y' then pure x else convergeRename f y

rewrite_ :: IndexFn -> IndexFnM IndexFn
rewrite_ fn@(IndexFn it xs) = simplifyIndexFn
  where
    simplifyIndexFn = algebraContext fn $ do
      addRelShape it
      ys <- simplifyCases xs
      pure $ IndexFn it ys

    simplifyCases cs = do
      let (ps, vs) = unzip $ casesToList cs
      ps_simplified <- mapM rewrite ps
      cs' <-
        eliminateFalsifiableCases (zip ps_simplified vs)
          >>= mapM simplifyCase
          >>= mergeCases
      pure $ cases cs'

    -- Simplify x under the assumption that p is true.
    simplifyCase (p, x)
      | Just _ <- justConstant x =
          pure (p, x)
    simplifyCase (p, x) = rollbackAlgEnv $ do
      assume (toCNF p)
      y <- rewrite =<< solveIx it x
      pure (p, y)

    -- Attempt to merge cases that are equivalent given their predicates.
    -- For example, in
    --   | k > 0  => sum_{j=0}^{k-1} e_j
    --   | k <= 0 => 0
    -- the second case is covered by the first when k <= 0:
    --   | True  => sum_{j=0}^{k-1} e_j
    mergeCases = go []
      where
        go acc [] = pure $ reverse acc
        go [] (c : cs) = go [c] cs
        go (case1 : acc) (case2 : cs) = do
          result <- tryMergeWithAny case1 (case2 : cs) []
          case result of
            Just (mergedCase, cs') -> go (mergedCase : acc) cs'
            Nothing -> go (case2 : case1 : acc) cs

        tryMergeWithAny _ [] _ = pure Nothing
        tryMergeWithAny case1 (case2 : rest) acc = do
          tryMerge case1 case2 >>= \case
            Just mergedCase -> pure $ Just (mergedCase, reverse acc ++ rest)
            Nothing -> tryMergeWithAny case1 rest (case2 : acc)

        tryMerge (c, e) (c', e') = do
          (_, e_under_c') <- simplifyCase (c', e)
          if e_under_c' == e'
            then pure $ Just (c :|| c', e)
            else do
              (_, e'_under_c) <- simplifyCase (c, e')
              pure $
                if e'_under_c == e
                  then Just (c :|| c', e')
                  else Nothing
    --- toCNF -> simplify -> toDNF

    eliminateFalsifiableCases = filterM (fmap isUnknown . isFalse . fst)

rewriteWithoutRules :: IndexFn -> IndexFnM IndexFn
rewriteWithoutRules =
  convergeRename rewrite_

solveIx :: (ASTMappable Symbol a) => [[Quantified Domain]] -> a -> IndexFnM a
solveIx [dim] =
  astMap
    ( ASTMapper
        { mapOnSymbol = solveIdx1 dim,
          mapOnSoP = applyRuleBook rulesSoP
        }
    )
  where
    rulesSoP :: IndexFnM [Rule (SoP Symbol) Symbol IndexFnM]
    rulesSoP = do
      h1 <- newVName "h"
      h2 <- newVName "h"
      h3 <- newVName "h"
      h4 <- newVName "h"
      pure
        [ Rule
            { name = "SolveIdx0",
              from = hole h4 .*. sym2SoP (Ix (hole h1) (hole h2) (hole h3)),
              to = \s -> do
                n <- sub s (hole h1)
                m <- sub s (hole h2)
                e_idx <- sub s (hole h3)
                t <- sub s (hole h4)
                (t .*.) <$> solveIdx0 dim (Ix n m e_idx),
              sideCondition = \s -> do
                n <- sub s (hole h1)
                m <- sub s (hole h2)
                e_idx <- sub s (hole h3)
                solveIdx0sidecond dim (Ix n m e_idx)
            }
        ]
      where
        hole = sym2SoP . Hole
solveIx _shape = pure

solveIdx0sidecond :: [Quantified Domain] -> Symbol -> IndexFnM Bool
solveIdx0sidecond [d1@(Forall _ (Iota _)), d2@(Forall _ (Iota e2))] sym@(Ix _ m e_idx) = do
  dimensions_match <- e2 `unifiesWith` m
  let multiples_of_m = filterSoP (\t c -> isJust (term2SoP t c ./. m)) e_idx
  pure $ dimensions_match && not (isZero multiples_of_m)
solveIdx0sidecond _ _ = pure False

solveIdx0 :: [Quantified Domain] -> Symbol -> IndexFnM (SoP Symbol)
solveIdx0 [d1@(Forall i1 (Iota _)), d2@(Forall i2 (Iota e2))] sym@(Ix n m e_idx)
  | i1 `S.notMember` fv e2 = do
      printM 2 $ "solveIdx0 \n  |_ d1 " <> prettyStr d1
      printM 2 $ "  |_ d2 " <> prettyStr d2
      printM 2 $ "  |_ sym " <> prettyStr sym
      let multiples_of_m = filterSoP (\t c -> isJust (term2SoP t c ./. m)) e_idx
      printM 2 $ "  |_ multiples_of_m " <> prettyStr multiples_of_m
      -- The rest assumes solveIdx0sidecond is true.
      let e_i = fromJust (multiples_of_m ./. m)
      let e_j = e_idx .-. multiples_of_m
      printM 2 $ "  |_ e_i " <> prettyStr e_i
      printM 2 $ "  |_ e_j " <> prettyStr e_j

-- TODO
-- * need to do this before e_idx has been simplified
--      ((i₅₀₉₁)*(2**(qm1₄₆₀₄)) + i₅₂₁₀ + -1*(2**(-1 + lgn₄₆₀₂ + -1*qm1₄₆₀₄))*(2**(qm1₄₆₀₄)))
--    gets simplified to
--      ((i₅₀₆₇)*(2**(qm1₄₆₀₄)) + i₅₁₇₉ + -1*2**(-1 + lgn₄₆₀₂))
--    by the algebra layer.
-- * can solveIdx0 be invoked already in Convert of (++)? (or in Substitute before simplifications)
      
      equation_solved <- checkRange e_i i1 n `andM` checkRange e_j i2 m
      case equation_solved of
        Yes -> pure e_i
        Unknown -> pure (sym2SoP sym)
  where
    checkRange e k ub
      | justSym e == Just (Var k) = pure Yes
      | otherwise = Bool True =>? (int2SoP 0 :<= e :&& e :< ub)
solveIdx0 _ sym = pure (sym2SoP sym)

solveIdx1 :: [Quantified Domain] -> Symbol -> IndexFnM Symbol
solveIdx1 [d1@(Forall i1 (Iota e1)), d2@(Forall _ (Iota e2))] sym@(Ix n m e_idx)
  -- SolveIdx1-Simplified from the supplementary material.
  | i1 `S.notMember` fv e2 = do
      dimensions_match <- e2 `unifiesWith` m
      -- XXX update rule to reflect no unification on e1?
      q <-
        if dimensions_match
          then Bool True =>? (sVar i1 .*. e2 :<= e_idx :&& e_idx :< (sVar i1 .+. int2SoP 1) .*. e2)
          else pure Unknown
      case q of
        Yes -> pure (Var i1)
        Unknown -> pure sym
solveIdx1 _ sym = pure sym

sVar :: VName -> SoP Symbol
sVar = sym2SoP . Var

unifiesWith :: (Unify v Symbol, Pretty v) => v -> v -> IndexFnM Bool
unifiesWith a b = do
  equiv :: Maybe (Substitution Symbol) <- unify a b
  pure $ isJust equiv
