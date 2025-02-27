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

import Control.Monad (forM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.AlgebraBridge (Answer (..), addRelIterator, algebraContext, answerFromBool, assume, isTrue, simplify, ($/=), ($<), ($<=), ($==), ($>), ($>=))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), Iterator (..), casesToList, getCase)
import Futhark.Analysis.Proofs.IndexFnPlus (intervalEnd, intervalStart, repDomain)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugT, printM, rollbackAlgEnv)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), sop2Symbol, toDNF)
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newNameFromString, newVName)
import Futhark.SoP.Monad (lookupRange)
import Futhark.SoP.Refine (addRels)
import Futhark.SoP.SoP (Range (..), Rel (..), SoP, int2SoP, justSym, sym2SoP, (.*.), (.+.), (.-.))
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
  case query of
    CaseCheck transf -> do
      p =>? transf q
    CaseIsMonotonic dir ->
      debugT "  " $
        case iterator fn of
          Forall i _ -> do
            -- Check j < i ^ p(j) ^ p(i) => q(j) `rel` q(i).
            j <- newVName "j"
            j +< i
            let rel = case dir of
                  Inc -> ($<=)
                  IncStrict -> ($<)
                  Dec -> ($>=)
                  DecStrict -> ($>)
            dnfQuery (p :&& (fromJust . justSym $ p @ Var j)) ((q @ Var j) `rel` q)
            where
              f @ x = rep (mkRep i x) f
          Empty -> undefined

dnfQuery :: Symbol -> IndexFnM Answer -> IndexFnM Answer
dnfQuery p f = do
  allM $ map (\q -> rollbackAlgEnv (assume q >> f)) (disjToList $ toDNF p)
  where
    disjToList (a :|| b) = disjToList a <> disjToList b
    disjToList x = [x]

(=>?) :: Symbol -> Symbol -> IndexFnM Answer
p =>? q = dnfQuery p (check q)

infixl 8 =>?

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

foreachCase :: IndexFn -> (Int -> IndexFnM a) -> IndexFnM [a]
foreachCase (IndexFn _ cs) f =
  forM (zip (casesToList cs) [0 ..]) $ \(_, i) -> f i

data Property
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)
  | -- | InRange (SoP Symbol) (SoP Symbol) -- TODO seperate out proof embedded in PermutationOfZeroTo.
    PermutationOfRange (SoP Symbol) (SoP Symbol)
  | -- For all k in Cat k _ _, prove property f(k).
    ForallSegments (VName -> Property)
  -- The restriction of f to the preimage [a,b] is injective.
  | InjectivePreimage (SoP Symbol, SoP Symbol)
  -- The restriction of f to the preimage [a,b] is bijective.
  | BijectivePreimage (SoP Symbol, SoP Symbol)

data Order = LT | GT | Undefined
  deriving (Eq, Show)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (InjectivePreimage (a,b)) fn@(IndexFn (Forall i0 dom) cs) = algebraContext fn $ do
  printM 1000 $ "Proving InjectiveOn " <> prettyString (a, b, fn)
  let guards = casesToList cs
  i <- newNameFromString "i"
  j <- newNameFromString "j"
  let iter_i = Forall i $ repDomain (mkRep i0 (Var i)) dom
  let iter_j = Forall j $ repDomain (mkRep i0 (Var j)) dom

  -- (1) Prove that within in a given case, there are no duplicate values in [a,b].
  -- (2) Prove that there are no duplicate values in [a,b] across cases.
  --  It is sufficient to show that the case values can be sorted
  --  in a strictly increasing order. That is, given
  --    i /= j in [0,...,n-1]
  --  we want to show:
  --    forall (c_1 => e_1) /= (c_2 => e_2) .
  --      c_1(i) ^ c_2(j) ==> f(i) < g(j) OR f(i) > g(j).
  --
  --  We define a comparison operator that returns the appropriate
  --  relation above, if it exists, and use a sorting algorithm
  --  to reduce the number of tests needed, but the only thing
  --  that matters is that this sorting exists.
  --
  let out_of_range x = x :< a :|| b :< x

  -- Proof of (1).
  let step1 = allM [no_dups g | g <- guards]
        where
          no_dups (c, e) = rollbackAlgEnv $ do
            -- WTS: i < j ^ c(i) ^ c(j) ^ a <= e(i) <= b ^ a <= e(j) <= b
            --        => e(i) /= e(j).
            addRelIterator iter_j
            i +< j

            let oob =
                  (sop2Symbol (c @ i) :&& sop2Symbol (c @ j))
                    =>? (out_of_range (e @ i) :|| out_of_range (e @ j))
            let neq =
                  (sop2Symbol (c @ i) :&& sop2Symbol (c @ j)) -- XXX could use in_range f@i g@j here
                    =>? (e @ i :/= e @ j)
            oob `orM` neq

  -- Proof of (2).
  let step2 = answerFromBool . isJust <$> sorted cmp guards
        where
          (p_f, f) `cmp` (p_g, g) = rollbackAlgEnv $ do
            let p =
                  (fromJust . justSym $ p_f @ i) :&& (fromJust . justSym $ p_g @ j)
            let f_rel_g rel =
                  -- WTS: forall i /= j ^ p_f(i) ^ p_g(j) . f(i) `rel` g(j)
                  -- XXX could use in_range f@i g@j here
                  let case_i_lt_j = rollbackAlgEnv $ do
                        addRelIterator iter_j
                        i +< j
                        p =>? (f @ i) `rel` (g @ j)
                      case_i_gt_j = rollbackAlgEnv $ do
                        addRelIterator iter_i
                        j +< i
                        p =>? (f @ i) `rel` (g @ j)
                   in case_i_lt_j `andM` case_i_gt_j
            ifM
              (f_rel_g (:<))
              LT
              ( ifM
                  (f_rel_g (:>))
                  GT
                  (pure Undefined)
              )

  step1 `andM` step2
  where
    f @ x = rep (mkRep i0 (Var x)) f

    ifM m t f = do
      res <- m
      case res of
        Yes -> pure t
        Unknown -> f
prove (PermutationOfRange start end) fn@(IndexFn (Forall i0 dom) cs) = algebraContext fn $ do
  printM 1000 $ "Proving PermutationOfRange" <> prettyString (start, end, fn)
  let guards = casesToList cs
  i <- newNameFromString "i"
  let iter_i = Forall i $ repDomain (mkRep i0 (Var i)) dom

  -- (1) Prove that the size of the domain equals (end - start + 1).
  -- (2) Prove that all branches are within bounds (start, end - 1).
  -- (3) Prove that fn values have no duplicates in [start, end].
  --

  let step1 = do
        range_sz <- simplify $ end .-. start .+. int2SoP 1
        case dom of
          Iota n ->
            n $== range_sz
          Cat {} -> do
            n <- simplify (intervalEnd dom .-. intervalStart dom .+. int2SoP 1)
            n $== range_sz

  let step2 = allM $ map case_in_bounds guards
        where
          case_in_bounds (p, f) = rollbackAlgEnv $ do
            addRelIterator iter_i
            assume (fromJust . justSym $ p @ i)
            (start $<= f @ i) `andM` (f @ i $<= end)

  let step3 = prove (InjectivePreimage (start, end)) fn

  step1 `andM` step2 `andM` step3
  where
    f @ x = rep (mkRep i0 (Var x)) f
prove (PermutationOfZeroTo m) fn =
  prove (PermutationOfRange (int2SoP 0) m) fn
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
