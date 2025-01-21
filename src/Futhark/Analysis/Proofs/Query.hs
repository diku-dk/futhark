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
    foreachCase,
    askRefinement,
    askRefinements,
    (+<),
  )
where

import Control.Monad (forM_, zipWithM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (partition, tails)
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraBridge (Answer (..), addRelIterator, algDebugPrettyM, algebraContext, answerFromBool, assume, isTrue, ($/=), ($<), ($<=), ($==), ($>), ($>=))
import Futhark.Analysis.Proofs.AlgebraBridge.Util (addRelSymbol)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), Iterator (..), casesToList, getCase)
import Futhark.Analysis.Proofs.IndexFnPlus (repDomain, domainEnd)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugM, debugPrettyM, debugPrintAlgEnv, debugT, rollbackAlgEnv)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, sop2Symbol)
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newNameFromString, newVName)
import Futhark.SoP.Monad (lookupRange)
import Futhark.SoP.Refine (addRels)
import Futhark.SoP.SoP (Range (..), Rel (..), SoP, int2SoP, justSym, sym2SoP, (.*.))
import Futhark.Util.Pretty (prettyStringW)
import Language.Futhark (VName, prettyString)
import Prelude hiding (GT, LT)

data MonoDir = Inc | IncStrict | Dec | DecStrict
  deriving (Show, Eq, Ord)

data Query
  = CaseIsMonotonic MonoDir
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseCheck (SoP Symbol -> Symbol)
  | -- Check whether case is true.
    Truth

askRefinement :: IndexFn -> IndexFnM Answer
askRefinement = allCases (askQ Truth)

askRefinements :: [IndexFn] -> IndexFnM Answer
askRefinements = allM . map askRefinement

-- | Answers a query on an index function case.
askQ :: Query -> IndexFn -> Int -> IndexFnM Answer
askQ Truth fn case_idx = askQ (CaseCheck sop2Symbol) fn case_idx
askQ query fn case_idx = algebraContext fn $ do
  let (p, q) = getCase case_idx (body fn)
  addRelIterator (iterator fn)
  assume p
  case query of
    CaseCheck transf -> do
      debugPrettyM "askQ check: " (transf q)
      whenUnknown debugPrintAlgEnv $ check (transf q)
    CaseIsMonotonic dir ->
      debugT "  " $
        case iterator fn of
          Forall i _ -> do
            -- Check j < i ^ p(j) ^ p(i) => q(j) `rel` q(i).
            j <- newVName "j"
            j +< i
            assume (fromJust . justSym $ p @ Var j)
            let rel = case dir of
                  Inc -> ($<=)
                  IncStrict -> ($<)
                  Dec -> ($>=)
                  DecStrict -> ($>)
            debugM $ "  Monotonic " <> show dir <> ": " <> prettyString p
            algDebugPrettyM "" (sym2SoP p)
            algDebugPrettyM "    =>" q
            (q @ Var j) `rel` q
            where
              f @ x = rep (mkRep i x) f
          Empty -> undefined

whenUnknown effect m = do
  ans <- m
  case ans of
    Yes -> pure ans
    Unknown -> effect >> pure ans

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
  allM $ zipWith (\_ i -> query fn i) (casesToList cs) [0 ..]

foreachCase :: IndexFn -> (Int -> IndexFnM a) -> IndexFnM ()
foreachCase (IndexFn _ cs) f =
  forM_ (zip (casesToList cs) [0 ..]) $ \(_, i) -> f i

data Property
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)
  | -- | InRange (SoP Symbol) (SoP Symbol) -- TODO seperate out proof embedded in PermutationOfZeroTo.
    PermutationOfRange (SoP Symbol) (SoP Symbol)
  | -- For all k in Cat k _ _, prove property f(k).
    ForallSegments (VName -> Property)
  | InjectiveOn (SoP Symbol) (SoP Symbol)
  | Injective

data Order = LT | GT | LTE | GTE | Undefined
  deriving (Eq, Show)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (PermutationOf {}) _fn = undefined
prove (PermutationOfZeroTo m) fn = prove (PermutationOfRange (int2SoP 0) m) fn
prove Injective fn@(IndexFn (Forall _ dom) _) =
  prove (InjectiveOn (int2SoP 0) (domainEnd dom)) fn
prove (InjectiveOn start end) fn@(IndexFn (Forall i0 dom) cs) = algebraContext fn $ do
  let branches = casesToList cs
  i <- newNameFromString "i"
  j <- newNameFromString "j"
  let iter_i = Forall i $ repDomain (mkRep i0 (Var i)) dom
  let iter_j = Forall j $ repDomain (mkRep i0 (Var j)) dom

  debugPrettyM "Showing i /= j . f(i) /= g(j)" fn
  let (p_f, f) != (p_g, g) = rollbackAlgEnv $ do
        debugPrettyM "  " (sop2Symbol (p_f @ i) :&& sop2Symbol (p_g @ j))
        debugPrettyM "    => " ((f @ i) :/= (g @ j))
        -- Try to show: forall i /= j . f(i) /= g(j)
        let case_i_lt_j = rollbackAlgEnv $ do
              -- Case i < j => f(i) `rel` g(j).
              addRelIterator iter_j
              i +< j
              debugPrintAlgEnv
              assume (sop2Symbol $ p_f @ i)
              assume (sop2Symbol $ p_g @ j)

              (f @ i) $/= (g @ j)
              `orM`
              check (f @ i :< start :|| end :< f @ i
                      :||
                      g @ j :< start :|| end :< g @ j)
            case_i_gt_j = rollbackAlgEnv $ do
              -- Case i > j => f(i) `rel` g(j):
              addRelIterator iter_i
              j +< i
              assume (sop2Symbol $ p_f @ i)
              assume (sop2Symbol $ p_g @ j)

              (f @ i) $/= (g @ j)
              `orM`
              check (f @ i :< start :|| end :< f @ i
                      :||
                      g @ j :< start :|| end :< g @ j)
         in case_i_lt_j `orM` case_i_gt_j

  -- NOTE could optimise this by sorting the distinct branches,
  -- (just see how to do that below) but I don't think it's worth the added complexity.
  allM [x != y | (x : ys) <- tails branches, y <- ys]
    `andM` allM [x != x | x <- branches]
  where
    f @ x = rep (mkRep i0 (Var x)) f
prove (PermutationOfRange start end) fn@(IndexFn (Forall i0 dom) cs) = algebraContext fn $ do
  -- 1. Prove that each case is strictly monotonic.
  -- 2. Prove that all branches are within bounds (start, end - 1).
  -- 3. Prove no overlap between case values.
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
  debugM $
    "Prove permutation of " <> prettyString start <> " .. " <> prettyString end
  debugPrettyM "\n" fn

  let branches = casesToList cs
  i <- newNameFromString "i"
  j <- newNameFromString "j"
  let iter_i = Forall i $ repDomain (mkRep i0 (Var i)) dom
  let iter_j = Forall j $ repDomain (mkRep i0 (Var j)) dom

  let case_monotonic c =
        askQ (CaseIsMonotonic IncStrict) fn c
          `orM` askQ (CaseIsMonotonic DecStrict) fn c

  let case_in_bounds (p, f) = rollbackAlgEnv $ do
        addRelIterator iter_i
        assume (fromJust . justSym $ p @ i)
        algDebugPrettyM "Case:" (p @ i :: SoP Symbol)
        algDebugPrettyM "\t=>" (f @ i :: SoP Symbol)
        let bug1 = debugT ("    >= " <> prettyStringW 110 start)
        let bug2 = debugT ("    <= " <> prettyStringW 110 end)
        bug1 (start $<= f @ i) `andM` bug2 (f @ i $<= end)

  let (p_f, f) `cmp` (p_g, g) = rollbackAlgEnv $ do
        assume (fromJust . justSym $ p_f @ i)
        assume (fromJust . justSym $ p_g @ j)
        let f_rel_g rel =
              -- Try to show: forall i /= j . f(i) `rel` g(j)
              let case_i_lt_j = rollbackAlgEnv $ do
                    -- Case i < j => f(i) `rel` g(j).
                    addRelIterator iter_j
                    i +< j
                    (f @ i) `rel` (g @ j)
                  case_i_gt_j = rollbackAlgEnv $ do
                    -- Case i > j => f(i) `rel` g(j):
                    addRelIterator iter_i
                    j +< i
                    (f @ i) `rel` (g @ j)
               in case_i_lt_j `andM` case_i_gt_j
        algDebugPrettyM "f(i):" (p_f @ i :: SoP Symbol)
        algDebugPrettyM "\t=>" (f @ i :: SoP Symbol)
        algDebugPrettyM "g(i):" (p_g @ i :: SoP Symbol)
        algDebugPrettyM "\t=>" (g @ j :: SoP Symbol)
        f_LT_g <- f_rel_g ($<)
        debugT "\tf `cmp` g" $
          case f_LT_g of
            Yes -> pure LT
            Unknown -> do
              f_GT_g <- f_rel_g ($>)
              case f_GT_g of
                Yes -> pure GT
                Unknown -> pure Undefined

  let monotonic = do
        debugM "1.  MONOTONICITY"
        allM $ zipWith (curry (case_monotonic . snd)) branches [0 ..]
  let within_bounds = do
        debugM "2.  WITHIN BOUNDS"
        allM $ map case_in_bounds branches
  let no_overlap = do
        debugM "3.  NO OVERLAP"
        answerFromBool . isJust <$> sorted cmp branches
  monotonic `andM` within_bounds `andM` no_overlap
  where
    f @ x = rep (mkRep i0 (Var x)) f
prove (ForallSegments fprop) fn@(IndexFn (Forall _ (Cat k _ _)) _) =
  prove (fprop k) fn
prove _ _ = error "Not implemented yet."

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

-- Bounds i like j, but with the additional constraint that i < j.
(+<) :: VName -> VName -> IndexFnM ()
i +< j = do
  range <- lookupRange j'
  let ki = int2SoP (rangeMult range) .*. sym2SoP i'
  addRels $
    S.map (:<=: ki) (lowerBound range)
      <> S.map (:>=: ki) (upperBound range)
      <> S.singleton (sym2SoP i' :<: sym2SoP j')
  where
    j' = Algebra.Var j
    i' = Algebra.Var i

isYes :: Answer -> Bool
isYes Yes = True
isYes _ = False

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False

-- Short-circuit evaluation `and`. (Unless debugging is on.)
andF :: Answer -> IndexFnM Answer -> IndexFnM Answer
andF Yes m = m
-- andF Unknown m = whenDebug (void m) >> pure Unknown
andF Unknown _ = pure Unknown

andM :: IndexFnM Answer -> IndexFnM Answer -> IndexFnM Answer
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
allM [] = pure Yes
allM xs = foldl1 andM xs
