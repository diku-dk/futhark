-- Answer queries on index functions using algebraic solver.

module Futhark.Analysis.Proofs.Query
  ( Answer (..),
    MonoDir (..),
    Query (..),
    Property (..),
    ask,
    prove,
    isTrue,
    isFalse,
    isUnknown,
  )
where

import Control.Monad (foldM, forM_, unless)
import Data.Maybe (catMaybes, fromJust)
import Futhark.Analysis.Proofs.AlgebraBridge (Answer (..), addRelIterator, addRelSymbol, algebraContext, assume, rollbackAlgEnv, simplify, toRel, ($<), ($<=), ($>))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (Iota), IndexFn (..), Iterator (..), casesToList, getCase)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrettyM, debugPrintAlgEnv, debugT)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, toDNF)
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newNameFromString, newVName)
import Futhark.SoP.Monad (MonadSoP, addRange, mkRange, mkRangeLB, mkRangeUB)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, sym2SoP, (.-.))
import Language.Futhark (VName)

data MonoDir = Inc | IncStrict | Dec | DecStrict
  deriving (Show, Eq, Ord)

data Query
  = CaseIsMonotonic MonoDir
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseTransform (SoP Symbol -> SoP Symbol)

-- | Answers a query on an index function case.
ask :: Query -> IndexFn -> Int -> IndexFnM Answer
ask query fn@(IndexFn it cs) case_idx = algebraContext fn $ do
  let (p, q) = getCase case_idx cs
  addRelIterator it
  addRelSymbol p
  case query of
    CaseTransform transf -> isTrue $ transf q
    CaseIsMonotonic dir ->
      case it of
        Forall i _ -> do
          -- Add j < i. Check p(j) ^ p(i) => q(j) `rel` q(i).
          addRange (Algebra.Var i) (mkRangeLB $ int2SoP 1)
          j <- newVName "j"
          addRange (Algebra.Var j) (mkRange (int2SoP 0) (sym2SoP (Algebra.Var i) .-. int2SoP 1))
          let p_j = fromJust . justSym $ p @ Var j
          let q_j = q @ Var j
          addRelSymbol p_j
          let rel = case dir of
                Inc -> (:<=)
                IncStrict -> (:<)
                Dec -> (:>=)
                DecStrict -> (:>)
          debugPrintAlgEnv
          debugPrettyM "ask" (q_j `rel` q)
          isTrue . sym2SoP $ q_j `rel` q
          where
            f @ x = rep (mkRep i x) f
        Empty -> undefined

data Property
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (PermutationOf {}) _fn = undefined
prove (PermutationOfZeroTo m) fn@(IndexFn (Forall iter (Iota n)) cs) = algebraContext fn $ do
  -- 1. Show that m = n.
  ans <- isTrue . sym2SoP $ m :== n
  case ans of
    Unknown -> pure Unknown
    Yes -> do
      -- Hardcode two cases for now.
      case casesToList cs of
        [(p, f), (not_p, g)] -> do
          unless (p == neg not_p) $ error "inconsistency"
          i <- newNameFromString "i"
          j <- newNameFromString "j"
          addRange (Algebra.Var i) (mkRangeLB (int2SoP 0))
          addRange (Algebra.Var j) (mkRangeLB (int2SoP 0))
          assume (fromJust . justSym $ p @ Var i)
          assume (fromJust . justSym $ not_p @ Var j)
          -- 2. Prove no overlap between cases.
          -- It is sufficient to show one case is always smaller than the other.
          -- Specifically, for two indices i and j where p(i) is true
          -- and p(j) is false, we need to show:
          --   {forall i /= j . f(i) < g(j)} OR {forall i /= j . f(i) > g(j)}
          -- given constraints
          --           0 <= i < n
          --           0 <= j < n
          --           1 <= n
          --
          -- We proceed case-by-case.
          let f_rel_g rel = do
                f_rel_g1 <- rollbackAlgEnv $ do
                  -- Case i < j => f(i) `rel` g(j).
                  addRelIterator (Forall j (Iota n))
                  i +< j
                  (f @ Var i) `rel` (g @ Var j)
                let f_rel_g2 = rollbackAlgEnv $ do
                      -- Case i > j => f(i) `rel` g(j):
                      addRelIterator (Forall i (Iota n))
                      j +< i
                      (f @ Var i) `rel` (g @ Var j)
                f_rel_g1 `andF` f_rel_g2
          -- Case i /= j => f(i) < g(j):
          let f_LT_g = debugT "f_LT_g" $ f_rel_g ($<)
          -- Case i /= j => f(i) > g(j):
          let f_GT_g = debugT "f_GT_g" $ f_rel_g ($>)
          let no_overlap = debugT "no_overlap" $ f_LT_g `orM` f_GT_g
          -- 3. Prove both cases are bounded from below by 0 and above by m.
          -- Order matters here for short-circuiting lazy actions.
          let within_bounds = rollbackAlgEnv $ do
                addRelIterator (Forall i (Iota n))
                addRelIterator (Forall j (Iota n))
                (int2SoP 0 $<= (f @ Var i))
                  `andM` (int2SoP 0 $<= (g @ Var j))
                  `andM` ((f @ Var i) $< n)
                  `andM` ((g @ Var j) $< n)
          within_bounds `andM` no_overlap
        _ -> undefined
  where
    f @ x = rep (mkRep iter x) f
prove (PermutationOfZeroTo {}) _ = pure Unknown

(+<) :: (MonadSoP Algebra.Symbol e p m) => VName -> VName -> m ()
i +< j = do
  addRange (Algebra.Var i) (mkRangeUB (sym2SoP (Algebra.Var j) .-. int2SoP 1))

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
      -- If we convert p to CNF, a sufficient condition for p to be false
      -- is that some clause q in p is false. Hence we can pick a clause q,
      -- assume all other clauses to be true, and use that information when
      -- checking q. This lets us easily falsify, for example,
      -- x == 1 :&& x == 2.
      let p_cnf = cnfToList $ neg neg_p_dnf -- Converts p to CNF.
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

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False

-- Short-circuit evaluation `and`.
andF :: (Applicative f) => Answer -> f Answer -> f Answer
andF Yes m = m
andF Unknown _ = pure Unknown

andM :: (Monad m) => m Answer -> m Answer -> m Answer
andM m1 m2 = do
  ans <- m1
  ans `andF` m2

orM :: (Monad m) => m Answer -> m Answer -> m Answer
orM m1 m2 = do
  a1 <- m1
  case a1 of
    Yes -> pure Yes
    Unknown -> m2
