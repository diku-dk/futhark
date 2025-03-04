module Futhark.Analysis.Proofs.Properties
  ( Property (..),
    prove,
  )
where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, algebraContext, answerFromBool, assume, simplify, ($<=), ($==))
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), Iterator (..), cases, guards)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd, intervalStart, repDomain)
import Futhark.Analysis.Proofs.Monad (IndexFnM, printM, printTrace, rollbackAlgEnv)
import Futhark.Analysis.Proofs.Query
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Substitute qualified as Subst ((@))
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, sop2Symbol)
import Futhark.Analysis.Proofs.Unify (Substitution, mkRep, rep, unify)
import Futhark.Analysis.Proofs.Util (prettyIndent)
import Futhark.MonadFreshNames (newNameFromString, newVName)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, sym2SoP, (.+.), (.-.))
import Language.Futhark (VName, prettyString)
import Prelude hiding (GT, LT)

data Property
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)
  | PermutationOfRange (SoP Symbol) (SoP Symbol)
  | -- For all k in Cat k _ _, prove property f(k).
    ForallSegments (VName -> Property)
  | -- The restriction of f to the preimage of [a,b] is injective.
    InjectiveRCD (SoP Symbol, SoP Symbol)
  | -- BijectiveRCD (a,b) (c,d).
    -- The restriction of f to the preimage of [a,b] is bijective.
    -- [c,d] (subset of [a,b]) is the image of this restricted f.
    BijectiveRCD (SoP Symbol, SoP Symbol) (SoP Symbol, SoP Symbol)

data Order = LT | GT | Undefined
  deriving (Eq, Show)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (InjectiveRCD (a, b)) fn@(IndexFn (Forall i0 dom) _) = algebraContext fn $ do
  printM 1000 $
    title "Proving InjectiveRCD "
      <> "\n  RCD (a,b) = "
      <> prettyString (a, b)
      <> "\n"
      <> prettyIndent 2 fn
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
  let step1 = allM [no_dups g | g <- guards fn]
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
  let step2 = answerFromBool . isJust <$> sorted cmp (guards fn)
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
prove (BijectiveRCD (a, b) (c, d)) f@(IndexFn it@(Forall i0 dom) _) = rollbackAlgEnv $ do
  printM 1000 $
    title "Proving BijectiveRCD "
      <> "\n  RCD (a,b) = "
      <> prettyString (a, b)
      <> "\n  RCD_Img (c,d) = "
      <> prettyString (c, d)
      <> "\n"
      <> prettyIndent 2 f
  i <- newNameFromString "i"
  let iter_i = Forall i $ repDomain (mkRep i0 (Var i)) dom

  -- Let X be the preimage of [a, b] under f
  -- so that f restricted to X is the function:
  --   f|X : X -> [a, b].
  -- WTS(1): f|X is injective.
  -- WTS(2): f|X is surjective with Img(f|X) = [c,d].
  --
  -- Even though we know the image of
  --   f|X : X -> [a, b]
  -- is [c, d] (subset of [a,b]), using [a, b] to restrict
  -- the codomain of f additionally tells us that ([a,b] \ [c,d])
  -- under f is empty, if (1) and (2) are true.

  let step1 =
        printTrace 1000 "Step (1)" $
          prove (InjectiveRCD (a, b)) f

  let step2 = rollbackAlgEnv $ do
        -- WTS(2.1): If |X| = |[c,d]| and (y in f(X) => y in [c,d]), then (2) holds.
        -- Proof. Assume that |X| = |[c,d]| and that (y in f(X) => y in [c,d]).
        -- By step (1), f|X is injective, so f maps X to exactly |X| = |[c,d]|
        -- distinct values in [c,d]. Hence the set must be covered.
        --
        -- WTS(2.2): |X| = |[c,d]|.
        -- We can find the cardinality of X by counting how many values
        -- in the (unrestricted) domain of f are mapped to values in [a,b].
        -- We then show equivalence by unifying the two sizes.       ^1
        --
        -- WTS(2.3): y in f(X) => y in [c,d].
        -- We show this by querying the solver: c <= y <= d.
        --
        -- ______
        -- \^1 Not a typo; using [a,b] over [c,d] is what gives us that
        -- ([a,b] \ [c,d]) is empty under f.
        addRelIterator it

        x <- newVName "x"
        let x_i = sym2SoP (Idx (Var x) (sym2SoP (Var i0)))
        let in_RCD = a :<= x_i :&& x_i :<= b
        infinity <- sym2SoP . Var <$> newVName "∞"
        f_restricted_to_X <-
          IndexFn (Forall i0 dom) (cases [(in_RCD, x_i), (neg in_RCD, infinity)])
            Subst.@ (x, f)
        fX <- rewrite f_restricted_to_X

        let guards_in_RCD = [(p, e) | (p, e) <- guards fX, e /= infinity]

        let step_2_2 = algebraContext fX $ do
              let size_RCD_image = d .-. c .+. int2SoP 1

              start <- rewrite $ domainStart dom
              end <- rewrite $ domainEnd dom
              cs <- rewrite $ foldl1 (:||) (map fst guards_in_RCD)

              j_sum <- newVName "j"
              let size_X = sym2SoP $ Sum j_sum start end (sop2Symbol $ cs @ j_sum)

              ans <- rewrite $ size_RCD_image :== size_X
              printM 1000 $ "size_RCD_image " <> prettyString size_RCD_image
              printM 1000 $ "size_X         " <> prettyString size_X
              printM 1000 $ "Step (2.2) " <> prettyString ans
              case ans of
                Bool True -> pure Yes
                _ -> pure Unknown

        let step_2_3 = algebraContext fX $ do
              printTrace 1000 "Step (2.3)" $
                allM $
                  map case_in_bounds guards_in_RCD
              where
                case_in_bounds (p, e) = rollbackAlgEnv $ do
                  addRelIterator iter_i
                  assume (fromJust . justSym $ p @ i)
                  (c $<= e @ i) `andM` (e @ i $<= d)

        -- NOTES:
        -- [ ] BijF1 has a possibly unintended effect where
        -- if you specify an RCD/RCD_Img that's smaller than
        -- the actual image of f restricted to the preimage of RCD,
        -- then the unification test on RCD_img will fail
        -- because the sum over predicates range over
        -- the unrestricted domain of f.
        -- So currently RCD_img must be the exact image
        -- of f restricted to the preimage of RCD.
        printM 1000 $ "f restricted to X: " <> prettyString fX
        printTrace 1000 "Step (2)" $
          step_2_2 `andM` step_2_3

  step1 `andM` step2
  where
    e @ x = rep (mkRep i0 (Var x)) e
prove (PermutationOfRange start end) fn@(IndexFn (Forall i0 dom) _) = algebraContext fn $ do
  -- equivalent with BijectivePreimage (start dom, end dom)

  printM 1000 $ title "Proving PermutationOfRange" <> prettyString (start, end, fn)
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

  let step2 = allM $ map case_in_bounds (guards fn)
        where
          case_in_bounds (p, f) = rollbackAlgEnv $ do
            addRelIterator iter_i
            assume (fromJust . justSym $ p @ i)
            (start $<= f @ i) `andM` (f @ i $<= end)

  let step3 = prove (InjectiveRCD (start, end)) fn

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

title :: String -> String
title s = "\ESC[4m" <> s <> "\ESC[0m"
