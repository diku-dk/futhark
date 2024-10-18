-- Answer queries on index functions using algebraic solver.
{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.Query where

import Control.Monad (when, foldM, forM_)
import Control.Monad.RWS (gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.AlgebraPC.Solve qualified as Solve
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), IndexFnM, Iterator (..), VEnv (..), getCase)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), toDNF, neg, normalizeSymbol)
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))
import Futhark.SoP.Monad (addRange, mkRange)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Range (Range), Rel (..), SoP, int2SoP, justAffine, justSym, mapSymSoP2M, (.-.), sym2SoP)
import Futhark.SoP.SoP qualified as SoP
import Futhark.Analysis.Proofs.AlgebraBridge (toAlgebraSoP, fromAlgebra, toAlgebra)
import Data.Maybe (catMaybes)
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), ASTMappable (..))
import Futhark.Analysis.Proofs.Util (converge)

data Query
  = CaseIsMonotonic
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseTransform (SoP Symbol -> SoP Symbol)

data Answer = Yes | Unknown

-- Answers a query on an index function case.
ask :: Query -> IndexFn -> Int -> IndexFnM Answer
ask query (IndexFn it cs) case_idx = rollbackAlgEnv $ do
  let (p, q) = getCase case_idx cs
  addRelIterator it
  addRelSymbol p
  case query of
    CaseTransform transf -> isTrue $ transf q
    _ -> undefined

-- XXX TODO want to rewrite symbol, not just simplify it, before checking
-- | Does this symbol simplify to true?
isTrue :: SoP Symbol -> IndexFnM Answer
isTrue sym = do
  p <- simplify sym
  case justSym p of
    Just (Bool True) -> pure Yes
    _ -> pure Unknown

-- | Does this symbol simplify to false?
isFalse :: Symbol -> IndexFnM Answer
isFalse p = do
  -- Our solver may return False when the query is undecidable,
  -- so instead check if the negation of p is true.
  let neg_p_dnf = toDNF (neg p)
  not_p <- isTrue (sym2SoP neg_p_dnf)
  case not_p of
    Yes -> pure Yes
    Unknown -> do
      let p_cnf = cnfToList $ neg neg_p_dnf -- Converts p to CNF.
      -- Given p in CNF, a sufficient condition for p to be false
      -- is that some clause q in p is false. Hence we can pick a clause q,
      -- assume all other clauses to be true, and use that information when
      -- checking q. This lets us easily falsify, for example,
      -- x == 1 :&& x == 2.
      foldM
        ( \acc i ->
            case acc of
              Yes -> pure Yes
              Unknown -> rollbackAlgEnv $ do
                let (q, qs) = pick i p_cnf
                assumeTrue qs
                isTrue (sym2SoP $ neg q)
        )
        Unknown
        [0 .. length p_cnf - 1]
  where
    cnfToList (a :&& b) = a : cnfToList b
    cnfToList x = [x]

    -- Returns the ith element of qs and qs without the ith element.
    pick n qs =
      let (as, bs) = splitAt n qs
       in (head bs, as <> tail bs)

    assumeTrue qs = do
      rels <- mapM toRel qs
      forM_ (catMaybes rels) addRel

-- | Simplify symbols using algebraic solver.
simplify :: (ASTMappable Symbol a) => a -> IndexFnM a
simplify = astMap m
  where
    m :: ASTMapper Symbol IndexFnM =
      ASTMapper
        { mapOnSymbol = converge (fmap normalizeSymbol . simplifySymbol),
          mapOnSoP = simplifySoP
        }

    simplifySoP :: SoP Symbol -> IndexFnM (SoP Symbol)
    simplifySoP x = rollbackAlgEnv $ do
      y <- toAlgebraSoP x
      -- debugPrettyM "simplify" y
      -- algenv <- gets algenv
      -- debugPrettyM "under" algenv
      z <- Solve.simplify y
      mapSymSoP2M fromAlgebra z

    -- Note that this does not recurse.
    simplifySymbol :: Symbol -> IndexFnM Symbol
    simplifySymbol symbol = case symbol of
      x :== y -> refineCmp (:==) x y
      x :/= y -> refineCmp (:/=) x y
      x :> y -> refineCmp (:>) x y
      x :>= y -> refineCmp (:>=) x y
      x :< y -> refineCmp (:<) x y
      x :<= y -> refineCmp (:<=) x y
      x -> pure x
      where
        refineCmp rel x y = do
          b <- solve (x `rel` y)
          pure $ if b then Bool True else x `rel` y

        -- Use Fourier-Motzkin elimination to determine the truth value
        -- of an expresion, if it can be determined in the given environment.
        -- If the truth value cannot be determined, False is also returned.
        solve (Bool True) = pure True
        solve (a :== b) = a $== b
        solve (a :/= b) = a $/= b
        solve (a :> b) = a $> b
        solve (a :>= b) = a $>= b
        solve (a :< b) = a $< b
        solve (a :<= b) = a $<= b
        solve _ = pure False

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
rollbackAlgEnv :: IndexFnM a -> IndexFnM a
rollbackAlgEnv computation = do
  alg <- gets algenv
  res <- computation
  modify (\env -> env {algenv = alg})
  pure res

-- | Adds a relation on symbols to the algebraic environment.
addRelSymbol :: Symbol -> IndexFnM ()
addRelSymbol (x :/= y) = do
  -- Can be expressed, if we know which symbol is greater than the other.
  a <- toAlgebraSoP x
  b <- toAlgebraSoP y
  x_is_greater <- a $>$ b
  if x_is_greater
    then addRel (a :>: b)
    else do
      y_is_greater <- a $<$ b
      when y_is_greater $ addRel (a :<: b)
addRelSymbol p = do
  rel <- toRel p
  maybe (pure ()) addRel rel

-- | Add relations derived from the iterator to the algebraic environment.
addRelIterator :: Iterator -> IndexFnM ()
addRelIterator (Forall i dom) = case dom of
  Iota n -> do
    addAlgRange i (domainStart dom) (domainEnd dom)
    boundIndexValues n
  Cat k m _ -> do
    addAlgRange k (int 0) (m .-. int 1)
    addAlgRange i (domainStart dom) (domainEnd dom)
    addAlgRangeUB i (intervalEnd dom .-. int 1)
    boundIndexValues m
  where
    addAlgRangeUB vn x = do
      a <- toAlgebraSoP x
      addRange (Algebra.Var vn) (Range mempty 1 (S.singleton a))

    int :: Int -> SoP Symbol
    int n = int2SoP (toInteger n)

    boundIndexValues e = do
      -- The type of Range restricts us to bound affine n. Lower bound is 1
      -- since otherwise Iterator would be a single point (and hence Empty).
      case justAffine (SoP.normalize e) of
        Just (c, x, b) -> do
          let lb = int2SoP $ toInteger (1 :: Int)
          let ub = int2SoP $ toInteger (maxBound :: Int) - b
          a <- toAlgebra x
          addRange a (Range (S.singleton lb) c (S.singleton ub))
        Nothing -> pure ()
addRelIterator _ = pure ()

addAlgRange vn x y = do
  a <- toAlgebraSoP x
  b <- toAlgebraSoP y
  addRange (Algebra.Var vn) (mkRange a b)

toRel :: Symbol -> IndexFnM (Maybe (Rel Algebra.Symbol))
toRel = runMaybeT . toRel_
  where
    liftOp op a b = pure $ a `op` b
    convCmp op = convOp (lift . toAlgebraSoP) (liftOp op)

    toRel_ :: Symbol -> MaybeT IndexFnM (Rel Algebra.Symbol)
    toRel_ (x :< y) = convCmp (:<:) x y
    toRel_ (x :<= y) = convCmp (:<=:) x y
    toRel_ (x :> y) = convCmp (:>:) x y
    toRel_ (x :>= y) = convCmp (:>=:) x y
    toRel_ (x :== y) = convCmp (:==:) x y
    toRel_ (x :/= y) = convCmp (:/=:) x y
    toRel_ (x :&& y) = convOp toRel_ (liftOp (:&&:)) x y
    toRel_ (x :|| y) = convOp toRel_ (liftOp (:||:)) x y
    toRel_ _ = fail ""

convOp :: (Monad m) => (t1 -> m t2) -> (t2 -> t2 -> m b) -> t1 -> t1 -> m b
convOp transf op x y = do
  a <- transf x
  b <- transf y
  a `op` b

($<) :: SoP Symbol -> SoP Symbol -> IndexFnM Bool
($<) = convOp toAlgebraSoP ($<$)

($<=) :: SoP Symbol -> SoP Symbol -> IndexFnM Bool
($<=) = convOp toAlgebraSoP ($<=$)

($>) :: SoP Symbol -> SoP Symbol -> IndexFnM Bool
($>) = convOp toAlgebraSoP ($>$)

($>=) :: SoP Symbol -> SoP Symbol -> IndexFnM Bool
($>=) = convOp toAlgebraSoP ($>=$)

($==) :: SoP Symbol -> SoP Symbol -> IndexFnM Bool
($==) = convOp toAlgebraSoP ($==$)

($/=) :: SoP Symbol -> SoP Symbol -> IndexFnM Bool
($/=) = convOp toAlgebraSoP ($/=$)

infixr 4 $<

infixr 4 $<=

infixr 4 $>

infixr 4 $>=

infixr 4 $==

infixr 4 $/=
