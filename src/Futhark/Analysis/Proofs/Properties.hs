module Futhark.Analysis.Proofs.Properties
  ( Property (..),
    prove,
    sumOverIndexFn,
  )
where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, algebraContext, answerFromBool, addRelSymbol)
import Futhark.Analysis.Proofs.IndexFn (Domain (..), IndexFn (..), Iterator (..), cases, flattenCases, guards)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd, intervalStart, repDomain)
import Futhark.Analysis.Proofs.Monad (IndexFnM, printM, printTrace, rollbackAlgEnv, warningString)
import Futhark.Analysis.Proofs.Query
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Substitute qualified as Subst
import Futhark.Analysis.Proofs.Symbol (Symbol (..), sop2Symbol)
import Futhark.Analysis.Proofs.SymbolPlus (toSumOfSums)
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
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
  | -- The index functions are the filtering and partitioning predicates.
    FiltPartInv IndexFn IndexFn (SoP Symbol)

data Order = LT | GT | Undefined
  deriving (Eq, Show)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (ForallSegments fprop) f@(IndexFn (Forall _ (Cat k _ _)) _) =
  prove_ True (fprop k) f
prove prop f = prove_ False prop f

prove_ :: Bool -> Property -> IndexFn -> IndexFnM Answer
prove_ _ (InjectiveRCD (a, b)) fn@(IndexFn (Forall i0 dom) _) = algebraContext fn $ do
  printM 1000 $
    title "Proving InjectiveRCD "
      <> "\n  RCD (a,b) = "
      <> prettyString (a, b)
      <> "\n"
      <> prettyIndent 2 fn
  i <- newNameFromString "i"
  j <- newNameFromString "I"
  let iter_i = Forall i $ repDomain (mkRep i0 (Var i)) dom
  let iter_j = Forall j $ repDomain (mkRep i0 (Var j)) dom

  -- WTS(1): Within in a given case, there are no duplicate values in [a,b].
  --
  -- WTS(2): There are no duplicate values in [a,b] across cases.
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
  -- WTS(3): If fn is segmented, there are no duplicates across segments.
  --   Shown similarly to (2), by choosing i in [e_k, ..., e_{k+1}]
  --   and j in [e_{k+1},...,e_{k+2}].
  --
  let out_of_range x = x :< a :|| b :< x

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

  let step2 = guards fn `canBeSortedBy` cmp
        where
          -- Order guards by querying the solver.
          (p_f, f) `cmp` (p_g, g) = rollbackAlgEnv $ do
            let p = (fromJust . justSym $ p_f @ i) :&& (fromJust . justSym $ p_g @ j)
            let f_rel_g rel =
                  -- WTS: forall i /= j . p_f(i) ^ p_g(j) => f(i) `rel` g(j)
                  let case_i_lt_j = rollbackAlgEnv $ do
                        addRelIterator iter_j
                        i +< j
                        p =>? (f @ i) `rel` (g @ j)
                      case_i_gt_j = rollbackAlgEnv $ do
                        addRelIterator iter_i
                        j +< i
                        p =>? (f @ i) `rel` (g @ j)
                   in case_i_lt_j `andM` case_i_gt_j
            relationToOrder f_rel_g

  let step3 = case dom of
        Iota {} -> pure Yes
        Cat k _ _ -> guards fn `canBeSortedBy` cmp
          where
            kp1_rep = mkRep k $ sym2SoP (Var k) .+. int2SoP 1
            dom' = repDomain kp1_rep dom
            iter_j' = Forall j $ repDomain (mkRep i0 (Var j)) dom'

            -- Order guards by querying the solver.
            (p_f, f) `cmp` (p_g', g') = rollbackAlgEnv $ do
              let (p_g, g) = (rep kp1_rep p_g', rep kp1_rep g')
              let p = (fromJust . justSym $ p_f @ i) :&& (fromJust . justSym $ p_g @ j)
              -- WTS: forall i,j . e_k <= i < e_{k+1}
              --                     ^ e_{k+1} <= j < e_{k+2}
              --                     ^ p_f(i) ^ p_g(j)
              --                       => f(i) `rel` g(j)
              let f_rel_g rel =
                    rollbackAlgEnv $ do
                      addRelIterator iter_j'
                      addRelIterator iter_i
                      p =>? (f @ i) `rel` (g @ j)
              relationToOrder f_rel_g

  step1 `andM` step2 `andM` step3
  where
    f @ x = rep (mkRep i0 (Var x)) f

    xs `canBeSortedBy` cmp = answerFromBool . isJust <$> sorted cmp xs

    relationToOrder rel = do
      lt <- rel (:<)
      case lt of
        Yes -> pure LT
        Unknown -> do
          gt <- rel (:>)
          case gt of
            Yes -> pure GT
            Unknown -> pure Undefined
prove_ is_segmented (BijectiveRCD (a, b) (c, d)) f@(IndexFn (Forall i dom) _) = rollbackAlgEnv $ do
  printM 1000 $
    title "Proving BijectiveRCD "
      <> "\n  RCD (a,b) = "
      <> prettyString (a, b)
      <> "\n  RCD_Img (c,d) = "
      <> prettyString (c, d)
      <> "\n"
      <> prettyIndent 2 f

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
          prove_ is_segmented (InjectiveRCD (a, b)) f

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
        infinity <- sym2SoP . Var <$> newVName "∞"

        -- f_restricted_to_X <-
        --   IndexFn (Forall i0 dom) (cases [(in_RCD, x_i), (neg in_RCD, infinity)])
        --     Subst.@ (x, f)
        -- fX <- rewrite f_restricted_to_X
        let in_RCD x = a :<= x :&& x :<= b
        answers <- mapM (askQ (CaseCheck in_RCD) f) [0 .. length (guards f) - 1]
        let guards' =
              zipWith
                (\(p, e) ans -> if isYes ans then (p, e) else (p, infinity))
                (guards f)
                answers
        fX <- rewrite $ IndexFn (Forall i dom) (cases guards')

        let guards_in_RCD = [(p, e) | (p, e) <- guards fX, e /= infinity]

        let step_2_2 = algebraContext fX $ do
              addRelIterator (iterator fX)
              let size_RCD_image = d .-. c .+. int2SoP 1

              start <- rewrite $ if is_segmented then intervalStart dom else domainStart dom
              end <- rewrite $ if is_segmented then intervalEnd dom else domainEnd dom

              j_sum <- newVName "j"
              let cs = map ((@ j_sum) . sym2SoP . fst) guards_in_RCD
              let size_X =
                    if null cs
                      then int2SoP 0
                      else toSumOfSums j_sum start end $ foldl1 (.+.) cs

              ans <- rewrite $ size_RCD_image :== size_X
              printM 1000 $ "size_RCD_image " <> prettyString size_RCD_image
              printM 1000 $ "size_X         " <> prettyString size_X
              printM 1000 $ "Step (2.2) " <> prettyString ans
              case ans of
                Bool True -> pure Yes
                _ -> pure Unknown

        let step_2_3 = algebraContext fX $ do
              addRelIterator (iterator fX)
              printTrace 1000 "Step (2.3)" $
                allM $
                  map (\(p, e) -> p =>? (c :<= e :&& e :<= d)) guards_in_RCD

        printM 1000 $ "f restricted to X:\n" <> prettyIndent 4 fX
        printTrace 1000 "Step (2)" $
          step_2_2 `andM` step_2_3

  step1 `andM` step2
  where
    e @ x = rep (mkRep i (Var x)) e
prove_ baggage (FiltPartInv filt part split) f@(IndexFn (Forall i _) _) = rollbackAlgEnv $ do
  printM 1000 $
    title "Proving FiltPartInv\n"
      <> "  filter:\n"
      <> prettyIndent 4 filt
      <> "\n  partition:\n"
      <> prettyIndent 4 part
      <> "\n  partition split:\n"
      <> prettyString split
      <> "\n  function:\n"
      <> prettyIndent 4 f

  -- Construct size after filtering.
  m <- sumOverIndexFn filt
  let mm1 = m .-. int2SoP 1

  step1 <- prove_ baggage (BijectiveRCD (int2SoP 0, mm1) (int2SoP 0, mm1)) f

  let step2 = algebraContext f $ do
        vn_filt <- newVName "filt"
        let filt_i = Idx (Var vn_filt) (sym2SoP (Var i))
        vn_f <- newVName "f"
        let f_i = Idx (Var vn_f) (sym2SoP (Var i))
        checker <-
          IndexFn
            (iterator f)
            ( cases
                [ (filt_i, sym2SoP $ Bool True),
                  (Not filt_i, sym2SoP $ int2SoP 0 :> sym2SoP f_i :|| sym2SoP f_i :>= m)
                ]
            )
            Subst.@ (vn_filt, filt)
            >>= (Subst.@ (vn_f, f))
            >>= rewrite
        printM 3000 $ "checker " <> prettyString checker
        printTrace 1000 "FiltPartInv Step (2)" $
          allCases (askQ Truth) checker

  infinity <- sym2SoP . Var <$> newVName "∞"
  f_filtered <- do
    vn_filt <- newVName "filt"
    let filt_i = Idx (Var vn_filt) (sym2SoP (Var i))
    vn_f <- newVName "f"
    f' <-
      IndexFn
        { iterator = iterator f,
          body =
            cases
              [ (filt_i, sym2SoP $ Idx (Var vn_f) (sym2SoP (Var i))),
                (Not filt_i, infinity)
              ]
        }
        Subst.@ (vn_filt, filt)
        >>= (Subst.@ (vn_f, f))
        >>= rewrite
    let guards_filtered = [(p, e) | (p, e) <- guards f', e /= infinity]
    pure $ IndexFn (iterator f') (cases guards_filtered)

  printM 3000 $ "f_filtered:\n" <> prettyIndent 4 f_filtered

  j <- newNameFromString "j"
  let step3and4 = algebraContext f_filtered $ do
        addRelIterator (iterator f_filtered)
        j +< i
        printTrace 1000 "FiltPartInv Steps (3--4)" $
          allM [mono_strict_inc g | g <- guards f_filtered]
        where
          -- mono_strict_inc (_, e) | infinity `S.member` fv e = pure Yes
          mono_strict_inc (c, e) = rollbackAlgEnv $ do
            -- WTS: j < i ^ c(i) ^ c(j) => e(i) < e(j).
            (sop2Symbol (c @ j) :&& sop2Symbol (c @ i)) =>? (e @ j :< e @ i)

  let step5and6 = do
        vn_part <- newVName "part"
        let part_i = Idx (Var vn_part) (sym2SoP (Var i))
        vn_f <- newVName "f"
        let f_i = sym2SoP (Idx (Var vn_f) (sym2SoP (Var i)))
        checker <-
          IndexFn
            { iterator = iterator f_filtered,
              body =
                cases
                  [ (part_i, sym2SoP $ f_i :< split),
                    (Not part_i, sym2SoP $ f_i :>= split)
                  ]
            }
            Subst.@ (vn_part, part)
            >>= (Subst.@ (vn_f, f_filtered))
            >>= rewrite
        printM 3000 $ "checker " <> prettyString checker
        printTrace 1000 "FiltPartInv Steps (5--6)" $
          allCases (askQ Truth) checker

  pure step1 `andM` step2 `andM` step3and4 `andM` step5and6
  where
    fn @ idx = rep (mkRep i (Var idx)) fn
prove_ baggage (PermutationOfRange start end) f =
  prove_ baggage (BijectiveRCD (start, end) (start, end)) f
prove_ baggage (PermutationOfZeroTo m) fn =
  prove_ baggage (PermutationOfRange (int2SoP 0) m) fn
prove_ _ (ForallSegments _) _ =
  undefined
prove_ _ _ _ = error "Not implemented yet."

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

sumOverIndexFn :: IndexFn -> IndexFnM (SoP Symbol)
sumOverIndexFn f@(IndexFn (Forall _ dom) _) = do
  n <- rewrite $ domainEnd dom
  j <- newVName "j"
  x <- newVName "x"
  let sum_part = Sum j (int2SoP 0) n (Idx (Var x) (sym2SoP $ Var j))
  f_split <-
    IndexFn Empty (cases [(Bool True, sym2SoP sum_part)])
      Subst.@ (x, f)
  rewrite $ flattenCases (body f_split)
sumOverIndexFn (IndexFn Empty _) = undefined
