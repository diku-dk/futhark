-- Rewrite using algebraic solver.
module Futhark.Analysis.Proofs.Refine where

import Control.Monad (filterM, foldM, forM_)
import Control.Monad.RWS (gets, modify)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), VEnv (..), cases, casesToList)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, toDNF)
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))
import Futhark.SoP.Monad (addRange)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Range (Range), Rel (..), SoP, int2SoP, justAffine, justSym, sym2SoP, (.-.))
import Futhark.SoP.SoP qualified as SoP


querySolver indexfn query = undefined

-- Note that this does not recurse.
refineSymbol :: Symbol -> IndexFnM Symbol
refineSymbol symbol = case symbol of
  x :== y -> refineComparison (:==) x y
  x :/= y -> refineComparison (:/=) x y
  x :> y -> refineComparison (:>) x y
  x :>= y -> refineComparison (:>=) x y
  x :< y -> refineComparison (:<) x y
  x :<= y -> refineComparison (:<=) x y
  x -> pure x
  where
    refineComparison rel x y = do
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

-- Returns true if the predicate can be shown to be false.
isFalse :: (SoP Symbol -> IndexFnM (SoP Symbol)) -> Symbol -> IndexFnM Bool
-- isFalse _ p | trace (show p) False = undefined
isFalse simplify p = do
  -- Our solver may return False when the query is undecidable,
  -- so we instead check if the negation of p is true.
  let neg_p_dnf = toDNF (neg p)
  not_p <- refine neg_p_dnf
  if not_p
    then pure True
    else do
      -- If p is in CNF, a sufficient condition for p to be false
      -- is that some clause q in p is false. Now we can assume
      -- all other clauses to be true and use that information when
      -- checking q. This lets us easily falsify, for example,
      -- x == 1 :&& x == 2.
      let p_cnf = cnfToList $ neg neg_p_dnf -- toCNF p
      foldM
        ( \acc i ->
            if acc
              then pure acc
              else refineUnderAssumptions (pickAndNegate i p_cnf)
        )
        False
        [0 .. length p_cnf - 1]
  where
    cnfToList (a :&& b) = a : cnfToList b
    cnfToList x = [x]

    -- Pick the nth q out of qs and negate it.
    pickAndNegate n qs =
      let (as, bs) = splitAt n qs
       in (neg $ head bs, as <> tail bs)

    refine q = do
      q' <- simplify (sym2SoP q)
      case justSym q' of
        Just (Bool True) -> pure True
        _ -> pure False

    -- Assuming qs, can we show q?
    refineUnderAssumptions (q, qs) = rollbackAlgEnv $ do
      forM_ (mapMaybe toRel qs) addRel
      refine q

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
      (ps, vs) <-
        unzip
          <$> filterM (fmap not . isFalse simplify . fst) (casesToList cs)
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
