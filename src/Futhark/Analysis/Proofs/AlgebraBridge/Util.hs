-- Utilities for using the Algebra layer from the IndexFn layer.
module Futhark.Analysis.Proofs.AlgebraBridge.Util where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraBridge.Translate (isBooleanM, toAlgebra, toAlgebraSymbol)
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (..), Iterator (..))
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrettyM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg)
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))
import Futhark.SoP.Monad (addEquiv, addRange, mkRange)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Range (Range), Rel (..), SoP, int2SoP, justAffine, (.-.))
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty (Pretty (pretty), viaShow)

assume :: Symbol -> IndexFnM ()
assume (Not x) = do
  -- If x is boolean, it will be in NNF, so x can only be
  -- Var, Idx, or Apply.
  booltype <- isBooleanM x
  x' <- toAlgebraSymbol x
  when booltype $ addEquiv x' (int2SoP 0)
  not_x <- toAlgebraSymbol $ neg x
  when booltype $ addEquiv not_x (int2SoP 1)
  addRelSymbol (neg x)
assume (x :&& y) =
  assume x >> assume y
assume x = do
  booltype <- isBooleanM x
  debugPrettyM "assume" x
  debugPrettyM "bool??" booltype
  x' <- toAlgebraSymbol x
  when booltype $ addEquiv x' (int2SoP 1)
  not_x <- toAlgebraSymbol $ neg x
  when booltype $ addEquiv not_x (int2SoP 0)
  addRelSymbol x

-- | Adds a relation on symbols to the algebraic environment.
-- No-op if `p` is not a relation.
addRelSymbol :: Symbol -> IndexFnM ()
addRelSymbol p = do
  rel <- toRel p
  maybe (pure ()) addRel rel
  where
    toRel = runMaybeT . toRel_

    convOp transf op x y = do
      a <- transf x
      b <- transf y
      a `op` b

    liftOp op a b = pure $ a `op` b
    convCmp op = convOp (lift . toAlgebra) (liftOp op)

    toRel_ :: Symbol -> MaybeT IndexFnM (Rel Algebra.Symbol)
    toRel_ (x :< y) = convCmp (:<:) x y
    toRel_ (x :<= y) = convCmp (:<=:) x y
    toRel_ (x :> y) = convCmp (:>:) x y
    toRel_ (x :>= y) = convCmp (:>=:) x y
    toRel_ (x :== y) = convCmp (:==:) x y
    toRel_ (x :&& y) = convOp toRel_ (liftOp (:&&:)) x y
    toRel_ (x :|| y) = convOp toRel_ (liftOp (:||:)) x y
    -- toRel_ (x :/= y) = convCmp (:/=:) x y -- This won't work with addRel.
    toRel_ _ = fail ""

-- | Add relations derived from the iterator to the algebraic environment.
addRelIterator :: Iterator -> IndexFnM ()
addRelIterator (Forall i dom) = case dom of
  Iota n -> do
    n' <- toAlgebra n
    boundIndexValues n'
    dom_end <- toAlgebra $ domainEnd dom
    addRange (Algebra.Var i) (mkRange (int2SoP 0) dom_end)
  Cat k m b -> do
    m' <- toAlgebra m
    boundIndexValues m'
    addRange (Algebra.Var k) (mkRange (int2SoP 0) (m' .-. int2SoP 1))
    dom_start <- toAlgebra $ domainStart dom
    dom_end <- toAlgebra $ domainEnd dom
    addRange (Algebra.Var i) (mkRange dom_start dom_end)
    interval_start <- toAlgebra b
    interval_end <- toAlgebra $ intervalEnd dom
    addRange (Algebra.Var i) (mkRange interval_start interval_end)
  where
    boundIndexValues e = do
      -- The type of Range restricts us to bound affine n. Lower bound is 1
      -- since otherwise Iterator would be a single point (and hence Empty).
      case justAffine (SoP.normalize e) of
        Just (c, x, b) | c > 0 -> do
          -- Add bound 1 - b <= c*x <= infinity
          let lb = int2SoP $ toInteger (1 :: Int) - b
          addRange x (Range (S.singleton lb) c mempty)
        _ -> pure ()
addRelIterator _ = pure ()

-- Fourer Motzkin Elimination solver may return True or False.
-- True means the query holds. False means "I don't know".
data Answer = Yes | Unknown
  deriving (Show, Eq)

answerFromBool :: Bool -> Answer
answerFromBool True = Yes
answerFromBool False = Unknown

instance Pretty Answer where
  pretty = viaShow

convFME :: (SoP Algebra.Symbol -> SoP Algebra.Symbol -> IndexFnM Bool) -> SoP Symbol -> SoP Symbol -> IndexFnM Answer
convFME op x y = do
  a <- toAlgebra x
  b <- toAlgebra y
  -- debugPrettyM "FME" (a, b)
  ans <- a `op` b
  pure $ if ans then Yes else Unknown

($<) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($<) = convFME ($<$)

($<=) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($<=) = convFME ($<=$)

($>) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($>) = convFME ($>$)

($>=) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($>=) = convFME ($>=$)

($==) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($==) = convFME ($==$)

($/=) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($/=) = convFME ($/=$)

infixr 4 $<

infixr 4 $<=

infixr 4 $>

infixr 4 $>=

infixr 4 $==

infixr 4 $/=
