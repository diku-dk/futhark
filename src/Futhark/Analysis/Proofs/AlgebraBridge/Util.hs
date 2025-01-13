-- Utilities for using the Algebra layer from the IndexFn layer.
module Futhark.Analysis.Proofs.AlgebraBridge.Util
  ( Answer (..),
    assume,
    addRelIterator,
    addRelSymbol,
    answerFromBool,
    ($<),
    ($<=),
    ($>),
    ($>=),
    ($==),
    ($/=),
  )
where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraBridge.Translate (getDisjoint, toAlgebra)
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (..), Iterator (..))
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Monad (IndexFnM, rollbackAlgEnv)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))
import Futhark.SoP.Refine (addRel, addRels)
import Futhark.SoP.SoP (Rel (..), SoP, int2SoP, sym2SoP, (.-.))
import Futhark.Util.Pretty (Pretty (pretty), viaShow)

-- Fourer Motzkin Elimination solver may return True or False.
-- True means the query holds. False means "I don't know".
data Answer = Yes | Unknown
  deriving (Show, Eq)

instance Pretty Answer where
  pretty = viaShow

assume :: Symbol -> IndexFnM ()
assume p = do
  -- FIXME I'd like to have the below safety check, which is only really
  -- relevant for me accidentally calling assume on something non-bool.
  -- The source program has been type checked, so things that are in
  -- boolean places (such as predicates in cases) are really boolean.
  --
  -- Can't have it right now due to assume on c in E.If c t f, in Convert.hs.
  -- (Try to uncomment the below and run tests.)
  --
  -- booltype <- isBooleanM p
  -- unless booltype (error $ "Assume on non-boolean: " <> prettyString p)
  addRelSymbol p
  addEq 1 p
  -- Add that pairwise disjoint symbols are false.
  mapM_ (addEq 0) =<< getDisjoint p
  case p of
    p1 :&& p2 -> assume p1 >> assume p2
    _ -> pure ()
  where
    addEq n sym = do
      x <- toAlgebra (sym2SoP sym)
      addRel (x :==: int2SoP n)

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
    toRel_ (x :/= y) = do
      -- Inequality can only be expressed indirectly.
      gte <- lift $ x $>= y
      case gte of
        Yes -> toRel_ (x :> y)
        _ -> do
          lte <- lift $ x $<= y
          case lte of
            Yes -> toRel_ (x :< y)
            _ -> fail ""
    toRel_ _ = fail ""

-- | Add relations derived from the iterator to the algebraic environment.
addRelIterator :: Iterator -> IndexFnM ()
addRelIterator (Forall i dom) = case dom of
  Iota n -> do
    n' <- toAlgebra n
    dom_end <- Algebra.simplify =<< toAlgebra (domainEnd dom)
    addRels $
      S.fromList
        [ int2SoP 1 :<=: n',
          int2SoP 0 :<=: sVar i,
          sVar i :<=: dom_end
        ]
  Cat k m b -> do
    m' <- toAlgebra m
    dom_start <- Algebra.simplify =<< toAlgebra (domainStart dom)
    dom_end <- Algebra.simplify =<< toAlgebra (domainEnd dom)
    interval_start <- Algebra.simplify =<< toAlgebra b
    interval_end <- Algebra.simplify =<< toAlgebra (intervalEnd dom)
    addRels $
      S.fromList
        [ interval_start :<=: sVar i,
          sVar i :<=: interval_end,
          dom_start :<=: sVar i,
          sVar i :<=: dom_end,
          int2SoP 1 :<=: m',
          int2SoP 0 :<=: sVar k,
          sVar k :<=: m' .-. int2SoP 1
        ]
  where
    sVar = sym2SoP . Algebra.Var
addRelIterator _ = pure ()

answerFromBool :: Bool -> Answer
answerFromBool True = Yes
answerFromBool False = Unknown

convFME :: (SoP Algebra.Symbol -> SoP Algebra.Symbol -> IndexFnM Bool) -> SoP Symbol -> SoP Symbol -> IndexFnM Answer
convFME op x y = rollbackAlgEnv $ do
  a <- toAlgebra x
  b <- toAlgebra y
  -- debugPrettyM2 "FME" (a, b)
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
