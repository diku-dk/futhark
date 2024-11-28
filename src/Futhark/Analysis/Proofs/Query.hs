-- Answer queries on index functions using algebraic solver.

module Futhark.Analysis.Proofs.Query
  ( Answer (..),
    MonoDir (..),
    Query (..),
    Property (..),
    askQ,
    prove,
    isYes,
    isUnknown,
    orM,
    allCases,
  )
where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Futhark.Analysis.Proofs.AlgebraBridge (Answer (..), addRelIterator, algebraContext, answerFromBool, assume, rollbackAlgEnv, ($<), ($<=), ($>), isTrue, ($>=), ($==), ($/=))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (Iota), IndexFn (..), Iterator (..), casesToList, getCase)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrettyM, debugPrintAlgEnv, debugT, debugM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newNameFromString, newVName)
import Futhark.SoP.Monad (MonadSoP, addRange, mkRange, mkRangeLB, mkRangeUB)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, sym2SoP, (.-.))
import Language.Futhark (VName, prettyString)
import Prelude hiding (GT, LT)

data MonoDir = Inc | IncStrict | Dec | DecStrict
  deriving (Show, Eq, Ord)

data Query
  = CaseIsMonotonic MonoDir
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseCheck (SoP Symbol -> Symbol)

check :: Symbol -> IndexFnM Answer
check (a :&& b) = check a `andM` check b
check (a :|| b) = check a `orM` check b
check (a :== b) = a $== b
check (a :/= b) = a $/= b
check (a :> b) = a $> b
check (a :>= b) = a $>= b
check (a :< b) = a $< b
check (a :<= b) = a $<= b
check a = isTrue a

allCases :: (IndexFn -> Int -> IndexFnM Answer) -> IndexFn -> IndexFnM Answer
allCases query fn@(IndexFn _ cs) =
  allM $ zipWith (\_ i -> query fn i) (casesToList cs) [0..]

-- | Answers a query on an index function case.
askQ :: Query -> IndexFn -> Int -> IndexFnM Answer
askQ query fn@(IndexFn it cs) case_idx = algebraContext fn $ do
  let (p, q) = getCase case_idx cs
  addRelIterator it
  assume p
  case query of
    CaseCheck transf -> check (transf q)
    CaseIsMonotonic dir ->
      case it of
        Forall i _ -> do
          -- Add j < i. Check p(j) ^ p(i) => q(j) `rel` q(i).
          addRange (Algebra.Var i) (mkRangeLB $ int2SoP 1)
          j <- newVName "j"
          addRange (Algebra.Var j) (mkRange (int2SoP 0) (sym2SoP (Algebra.Var i) .-. int2SoP 1))
          assume (fromJust . justSym $ p @ Var j)
          let rel = case dir of
                Inc -> ($<=)
                IncStrict -> ($<)
                Dec -> ($>=)
                DecStrict -> ($>)
          (q @ Var j) `rel` q
          where
            f @ x = rep (mkRep i x) f
        Empty -> undefined

data Property
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)

data Order = LT | GT | Undefined
  deriving (Eq, Show)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (PermutationOf {}) _fn = undefined
prove (PermutationOfZeroTo m) fn@(IndexFn (Forall iter (Iota n)) cs) = algebraContext fn $ do
  -- 1. Show that m = n.
  -- 2. Prove no overlap between case values.
  --  It is sufficient to show that the case values can be sorted
  --  in a strictly increasing order. That is, given a list of branches
  --  on the form (p_f => f) and two indices i /= j in [0,...,n-1],
  --  we want to show
  --    forall (p_f => f) /= (p_g => g) in our list of branches .
  --      p_f(i) ^ p_g(j) ==> f(i) < g(j) OR f(i) > g(j).
  --
  --  We define a comparison operator that returns the appropriate
  --  relation above, if it exists, and use a sorting algorithm
  --  to reduce the number of tests needed, but the only thing
  --  that matters is that this sorting exists.
  -- 3. Prove that all branches are within bounds.
  debugM "prove permutation"
  debugPrettyM "\n" fn
  debugPrintAlgEnv
  m_eq_n <- m $== n
  case m_eq_n of
    Unknown -> pure Unknown
    Yes -> do
      let branches = casesToList cs
      i <- newNameFromString "i"
      j <- newNameFromString "j"
      addRange (Algebra.Var i) (mkRangeLB (int2SoP 0))
      addRange (Algebra.Var j) (mkRangeLB (int2SoP 0))
      let (p_f, f) `cmp` (p_g, g) = do
            assume (fromJust . justSym $ p_f @ i)
            assume (fromJust . justSym $ p_g @ j)
            let f_rel_g rel =
                  -- Try to show: forall i /= j . f(i) `rel` g(j)
                  let case_i_lt_j = rollbackAlgEnv $ do
                        -- Case i < j => f(i) `rel` g(j).
                        addRelIterator (Forall j (Iota n))
                        i +< j
                        (f @ i) `rel` (g @ j)
                      case_i_gt_j = rollbackAlgEnv $ do
                        -- Case i > j => f(i) `rel` g(j):
                        addRelIterator (Forall i (Iota n))
                        j +< i
                        (f @ i) `rel` (g @ j)
                  in case_i_lt_j `andM` case_i_gt_j
            debugPrettyM "f:" (f @ i :: SoP Symbol)
            debugPrettyM "g:" (g @ j :: SoP Symbol)
            f_LT_g <- f_rel_g ($<)
            debugT "  f `cmp` g" $
              case f_LT_g of
                  Yes -> pure LT
                  Unknown -> do
                    f_GT_g <- f_rel_g ($>)
                    case f_GT_g of
                      Yes -> pure GT
                      Unknown -> debugPrintAlgEnv >> pure Undefined
      let no_overlap = answerFromBool . isJust <$> sorted cmp branches
      let within_bounds =
            map
              ( \(p, f) -> rollbackAlgEnv $ do
                  addRelIterator (Forall i (Iota n))
                  assume (fromJust . justSym $ p @ i)
                  debugPrettyM "CASE:" (p @ i :: SoP Symbol)
                  let bug1 = debugT ("  0 <= " <> prettyString (f @ i :: SoP Symbol))
                  let bug2 = debugT ("  n >  " <> prettyString (f @ i :: SoP Symbol))
                  bug1 (int2SoP 0 $<= f @ i) `andM` bug2 (f @ i $< n)
              )
              branches
      allM within_bounds `andM` no_overlap
  where
    f @ x = rep (mkRep iter (Var x)) f
prove (PermutationOfZeroTo {}) _ = pure Unknown

-- Strict sorting.
sorted :: (t -> t -> IndexFnM Order) -> [t] -> IndexFnM (Maybe [t])
sorted cmp wat = runMaybeT $ quicksort wat
  where
    quicksort [] = pure []
    quicksort (p : xs) = do
      ordering <- mapM (`cmpLifted` p) xs
      let (lesser, greater) = partition ((== LT) . fst) (zip ordering xs)
      l <- quicksort $ map snd lesser
      r <- quicksort $ map snd greater
      pure $ l ++ [p] ++ r

    x `cmpLifted` y = do
      ord <- lift (x `cmp` y)
      case ord of
        Undefined -> fail ""
        _ -> pure ord

(+<) :: (MonadSoP Algebra.Symbol e p m) => VName -> VName -> m ()
i +< j = do
  addRange (Algebra.Var i) (mkRangeUB (sym2SoP (Algebra.Var j) .-. int2SoP 1))

isYes :: Answer -> Bool
isYes Yes = True
isYes _ = False

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False

-- Short-circuit evaluation `and`.
andF :: (Applicative f) => Answer -> f Answer -> f Answer
andF Yes m = m
andF Unknown _ = pure Unknown

andM :: Monad m => m Answer -> m Answer -> m Answer
andM m1 m2 = do
  ans <- m1
  ans `andF` m2

orM :: (Monad m) => m Answer -> m Answer -> m Answer
orM m1 m2 = do
  a1 <- m1
  case a1 of
    Yes -> pure Yes
    Unknown -> m2

allM :: [IndexFnM Answer] -> IndexFnM Answer
allM = foldl1 andM
