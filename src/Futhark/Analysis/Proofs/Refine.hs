-- Rewrite using algebraic solver.
module Futhark.Analysis.Proofs.Refine where

import Control.Monad.RWS (gets, modify)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), VEnv (..), cases, casesToList)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))
import Futhark.SoP.Monad (addRange)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Range (Range), Rel (..), SoP, int2SoP, justAffine, (.-.))
import Futhark.SoP.SoP qualified as SoP

refineSymbol :: Symbol -> IndexFnM Symbol
refineSymbol symbol = case symbol of
  x :== y -> refineRelation (:==) x y
  x :/= y -> refineRelation (:/=) x y
  x :> y -> refineRelation (:>) x y
  x :>= y -> refineRelation (:>=) x y
  x :< y -> refineRelation (:<) x y
  x :<= y -> refineRelation (:<=) x y
  x :&& y -> refineRelation (:&&) x y
  x :|| y -> refineRelation (:||) x y
  x -> pure x
  where
    refineRelation rel x y = do
      b <- solve (x `rel` y)
      pure $ if b then Bool True else x `rel` y

    -- Use Fourier-Motzkin elimination to determine the truth value
    -- of an expresion, if it can be determined in the given environment.
    -- If the truth value cannot be determined, False is also returned.
    solve (Bool True) = pure True
    solve (a :== b) = a $==$ b
    solve (a :/= b) = a $/=$ b
    solve (a :> b) = a $>$ b
    solve (a :>= b) = a $>=$ b
    solve (a :< b) = a $<$ b
    solve (a :<= b) = a $<=$ b
    solve _ = pure False

rollbackAlgEnv :: IndexFnM a -> IndexFnM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env {algenv = alg})
  pure res

refineIndexFn :: (SoP Symbol -> IndexFnM (SoP Symbol)) -> IndexFn -> IndexFnM IndexFn
refineIndexFn simplify (IndexFn it xs) = do
  ys <-
    rollbackAlgEnv
      ( do
          addIterator it
          refineCases xs
      )
  pure $ IndexFn it ys
  where
    refineCases cs = do
      let (preds, vals) = unzip $ casesToList cs
      -- Eliminate cases for which the predicate is always False. (The solver
      -- may return false when the query is undecidable, so we instead check
      -- if the negated predicate is True.)
      -- TODO ^can we return Nothing when undecidable instead?
      neg_preds <- mapM (refineSymbol . Not) preds
      let (_neg_ps, ps, vs) =
            unzip3 $
              filter (\(negp, _, _) -> negp /= Bool True) $
                zip3 neg_preds preds vals
      -- Cases are considered sequentially, so negation of previous cases
      -- are part of current predicate.
      vs' <- mapM refineCase (zip ps vs)
      cases <$> mergeEquivCases (zip ps vs')

    refineCase :: (Symbol, SoP Symbol) -> IndexFnM (SoP Symbol)
    refineCase (p, v)
      | Just rel <- toRel p =
          rollbackAlgEnv
            ( do
                addRel rel
                simplify v
            )
    refineCase (_, v) =
      simplify v

    -- Attempt to merge cases that are equivalent given their predicates.
    -- For example, in
    --   | k > 0  => sum_{j=0}^{k-1} e_j
    --   | k <= 0 => 0
    -- the second case is covered by the first when k <= 0. So we want just:
    --   | True  => sum_{j=0}^{k-1} e_j
    mergeEquivCases cs@[(_p1, v1), (p2, v2)] = do
      v1' <- refineCase (p2, v1)
      if v1' == v2
        then pure [(Bool True, v1)]
        else pure cs
    mergeEquivCases cs = pure cs

mkRange :: SoP Symbol -> SoP Symbol -> Range Symbol
mkRange lb ub = Range (S.singleton lb) 1 (S.singleton ub)

-- mkRangeLB :: SoP Symbol -> Range Symbol
-- mkRangeLB x = Range (S.singleton x) 1 mempty

mkRangeUB :: SoP Symbol -> Range Symbol
mkRangeUB x = Range mempty 1 (S.singleton x)

int :: Int -> SoP Symbol
int n = int2SoP (toInteger n)

addIterator :: Iterator -> IndexFnM ()
addIterator (Forall i dom) = case dom of
  Iota n -> do
    addRange (Var i) (mkRange (domainStart dom) (domainEnd dom))
    boundIndexValues n
  Cat k m _ -> do
    addRange (Var k) (mkRange (int 0) (m .-. int 1))
    addRange (Var i) (mkRange (domainStart dom) (domainEnd dom))
    addRange (Var i) (mkRangeUB (intervalEnd dom .-. int 1))
    boundIndexValues m
  where
    boundIndexValues e = do
      -- The type of Range restricts us to bound affine n. Lower bound is 1
      -- since otherwise Iterator would be a single point (and hence Empty).
      case justAffine (SoP.normalize e) of
        Just (c, x, b) ->
          let ub = int2SoP $ toInteger (maxBound :: Int) - b
           in addRange x (Range (S.singleton $ int 1) c (S.singleton ub))
        Nothing -> pure ()
addIterator _ = pure ()

-- I assume exp is already in NNF.
toRel :: Symbol -> Maybe (Rel Symbol)
toRel (x :<= y) = Just $ x :<=: y
toRel (x :< y) = Just $ x :<: y
toRel (x :> y) = Just $ x :>: y
toRel (x :>= y) = Just $ x :>=: y
toRel (x :== y) = Just $ x :==: y
-- toRel (x :/= y) = Just $ x :/=: y -- TODO
toRel (x :&& y) = (:&&:) <$> toRel x <*> toRel y
toRel (x :|| y) = (:||:) <$> toRel x <*> toRel y
toRel _ = Nothing
