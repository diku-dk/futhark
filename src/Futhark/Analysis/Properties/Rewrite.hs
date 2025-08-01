{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Rewrite (rewrite, rewriteWithoutRules) where

import Control.Monad (filterM, (<=<))
import Futhark.Analysis.Properties.AlgebraBridge (addRelShape, algebraContext, assume, isFalse, isUnknown, simplify)
import Futhark.Analysis.Properties.IndexFn (IndexFn (..), cases, casesToList)
import Futhark.Analysis.Properties.Monad (IndexFnM, rollbackAlgEnv)
import Futhark.Analysis.Properties.Rule (applyRuleBook, rulesIndexFn)
import Futhark.Analysis.Properties.Symbol (Symbol (..), toCNF)
import Futhark.Analysis.Properties.Unify (Renameable, renameSame)
import Futhark.SoP.SoP (SoP, justConstant)

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
      y <- rewrite x
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
