-- Answer queries on index functions using algebraic solver.
module Futhark.Analysis.Proofs.Query where

import Control.Monad.RWS (gets, modify)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), VEnv (..), getCase)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.Monad (addRange)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Range (Range), Rel (..), SoP, int2SoP, justAffine, justSym, (.-.))
import Futhark.SoP.SoP qualified as SoP
import Futhark.SoP.FourierMotzkin (($>$), ($<$))
import Control.Monad (when)


-- TODO implement FromSoP (AlgebraPC.Symbol) (SoP Symbol)
--                ToSoP -.-

data Query
  = CaseIsMonotonic
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseTransform (SoP Symbol -> SoP Symbol)

data Answer = Yes | Unknown

simplify = undefined

-- Answers the query for each case in an index function.
ask :: Query -> IndexFn -> Int -> IndexFnM Answer
ask query (IndexFn it cs) case_idx = rollbackAlgEnv $ do
  let (p, q) = getCase case_idx cs
  addIterator it
  addPred p
  case query of
    CaseTransform transf -> do
      x <- simplify $ transf q
      case justSym x of
        Just (Bool True) -> pure Yes
        _ -> pure Unknown
    _ -> undefined


rollbackAlgEnv :: IndexFnM a -> IndexFnM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env {algenv = alg})
  pure res

addPred :: Symbol -> IndexFnM ()
addPred (x :/= y) = do
  -- Express not equal as one is greater than the other.
  -- Figure out which one.
  x_is_greater <- x $>$ y
  if x_is_greater then addRel (x :>: y)
  else do
    y_is_greater <- x $<$ y
    when y_is_greater $ addRel (x :<: y)
addPred p | Just rel <- toRel p = addRel rel
addPred _ = pure ()

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
    mkRange lb ub = Range (S.singleton lb) 1 (S.singleton ub)

    mkRangeUB x = Range mempty 1 (S.singleton x)

    int :: Int -> SoP Symbol
    int n = int2SoP (toInteger n)

    boundIndexValues e = do
      -- The type of Range restricts us to bound affine n. Lower bound is 1
      -- since otherwise Iterator would be a single point (and hence Empty).
      case justAffine (SoP.normalize e) of
        Just (c, x, b) ->
          let ub = int2SoP $ toInteger (maxBound :: Int) - b
           in addRange x (Range (S.singleton $ int 1) c (S.singleton ub))
        Nothing -> pure ()
addIterator _ = pure ()

-- Argument should be in NNF to work properly.
toRel :: Symbol -> Maybe (Rel Symbol)
toRel (x :<= y) = Just $ x :<=: y
toRel (x :< y) = Just $ x :<: y
toRel (x :> y) = Just $ x :>: y
toRel (x :>= y) = Just $ x :>=: y
toRel (x :== y) = Just $ x :==: y
toRel (x :&& y) = (:&&:) <$> toRel x <*> toRel y
toRel (x :|| y) = (:||:) <$> toRel x <*> toRel y
toRel _ = Nothing
