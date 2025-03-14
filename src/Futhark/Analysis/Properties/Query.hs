-- Answer queries on index functions using algebraic solver.
module Futhark.Analysis.Properties.Query
  ( Answer (..),
    Query (..),
    queryCase,
    askRefinement,
    askRefinements,
    allM,
    andM,
    askQ,
    foreachCase,
    orM,
    isYes,
    isUnknown,
    (+<),
    (=>?),
    Statement (..),
    prove,
    proveFn,
  )
where

import Control.Monad (forM, join, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bifunctor (second)
import Data.List (partition)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraBridge
import Futhark.Analysis.Properties.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (domainEnd, domainStart, intervalEnd, intervalStart, repDomain)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Property
import Futhark.Analysis.Properties.Symbol (Symbol (..), sop2Symbol, toDNF)
import Futhark.Analysis.Properties.SymbolPlus (toSumOfSums)
import Futhark.Analysis.Properties.Unify
import Futhark.Analysis.Properties.Util
import Futhark.MonadFreshNames (newNameFromString, newVName)
import Futhark.SoP.Monad
import Futhark.SoP.Refine (addRels)
import Futhark.SoP.SoP (Range (..), Rel (..), SoP, int2SoP, justSym, sym2SoP, (.*.), (.+.), (.-.))
import Language.Futhark (VName)
import Prelude hiding (GT, LT)

data Query
  = CaseIsMonotonic MonDir
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseCheck (SoP Symbol -> Symbol)
  | -- Check whether case is true.
    Truth

-- NOTE rewriting askQ so that it can also check proof obligations.
--
-- Want to also parse prelude properties into index functions
-- to allow for substitution of formal arguments.
--
--   f = for i < n . true => x[i] >= 0 && IsTrue (InjectiveRCD x (0, n))
--
-- Then askQ can discharge proofs of properties inside IsTrue () to Prove.hs.
--
-- - Need to be able to add properties nested inside symbols to env:
--     x[i] >= 0 && IsTrue prop
--   Does this work simply by extending addRelSymbol to also handle IsTrue prop?
--   This might even make sense in the paper; \Alg is a conjunction of boolean symbols,
--   but a property is just a relation, hence (when fully applied) it too is just a
--   boolean. So \Alg ^ x[i] >= 0 ^ InjectiveRCD x (0, n) is naturally valid.
-- - Possible to use Properties.Property? (Defined in terms of Algebra symbols.)
--   E.g., by making Properties.Property parametric over the symbol type;
--   then we can just translate between Algebra and IndexFn symbols?
--   For example, index fns can have (Property Symbol), and then when
--   addRelSymbol is called this Property gets translated into (Property Algebra.Symbol)
--   and is added to the Alg env.
--
-- [x] Add Property to IndexFn Symbol.
-- [ ] Parse property prelude into IndexFn symbols; i.e., don't check immediately.
--     - should allow for properties in preconditions
askQ :: Query -> IndexFn -> IndexFnM Answer
askQ query fn =
  allM $ zipWith (\_ i -> queryCase query fn i) (guards fn) [0 ..]

askRefinement :: IndexFn -> IndexFnM Answer
askRefinement = askQ Truth

askRefinements :: [IndexFn] -> IndexFnM Answer
askRefinements = allM . map askRefinement

-- | Answers a query on an index function case.
queryCase :: Query -> IndexFn -> Int -> IndexFnM Answer
queryCase Truth fn case_idx = queryCase (CaseCheck sop2Symbol) fn case_idx
queryCase query fn case_idx = algebraContext fn $ do
  let (p, q) = getCase case_idx (body fn)
  addRelIterator (iterator fn)
  case query of
    CaseCheck transf -> p =>? transf q
    CaseIsMonotonic dir ->
      debugT "  " $
        case iterator fn of
          Forall i _ -> do
            -- Check j < i ^ p(j) ^ p(i) => q(j) `rel` q(i).
            j <- newVName "j"
            j +< i
            let rel = case dir of
                  Inc -> ($<=)
                  IncS -> ($<)
                  Dec -> ($>=)
                  DecS -> ($>)
            dnfQuery (p :&& (fromJust . justSym $ p @ Var j)) ((q @ Var j) `rel` q)
            where
              f @ x = rep (mkRep i x) f
          Empty -> undefined

dnfQuery :: Symbol -> IndexFnM Answer -> IndexFnM Answer
dnfQuery p q = allM $ map (\p' -> rollbackAlgEnv (assume p' >> q)) (disjToList $ toDNF p)
  where
    disjToList (a :|| b) = disjToList a <> disjToList b
    disjToList x = [x]

(=>?) :: Symbol -> Symbol -> IndexFnM Answer
-- p =>? q = dnfQuery p (check q)
p =>? q | p == q = pure Yes
p =>? q = do
  ans <- dnfQuery p (check q)
  when (isUnknown ans) $
    printM 3000 $
      "Failed to show:\n" <> prettyIndent 4 p <> "\n =>?\n" <> prettyIndent 4 q
  pure ans

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
check (Prop prop) = prove prop
check a = isTrue a

foreachCase :: IndexFn -> (Int -> IndexFnM a) -> IndexFnM [a]
foreachCase (IndexFn _ cs) f =
  forM (zip (casesToList cs) [0 ..]) $ \(_, i) -> f i

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

{-
              Proofs
-}
-- TODO eliminate this and define proofs directly on Property.Property?
data Statement
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)
  | PermutationOfRange (SoP Symbol) (SoP Symbol)
  | -- For all k in Cat k _ _, prove property f(k).
    ForallSegments (VName -> Statement)
  | -- The restriction of f to the preimage of [a,b] is injective.
    PInjectiveRCD (SoP Symbol, SoP Symbol)
  | -- BijectiveRCD (a,b) (c,d).
    -- The restriction of f to the preimage of [a,b] is bijective.
    -- [c,d] (subset of [a,b]) is the image of this restricted f.
    PBijectiveRCD (SoP Symbol, SoP Symbol) (SoP Symbol, SoP Symbol)
  | PFiltPartInv (VName -> Symbol) (VName -> Symbol) (SoP Symbol)

data Order = LT | GT | Undefined
  deriving (Eq, Show)

prove :: Property Symbol -> IndexFnM Answer
prove prop = failOnUnknown <$> matchProof prop
  where
    failOnUnknown Unknown = error $ "Failed to verify " <> prettyStr prop
    failOnUnknown Yes = Yes

    matchProof Boolean = error "prove called on Boolean property (nothing to prove)"
    matchProof Disjoint {} = error "prove called on Disjoint property (nothing to prove)"
    matchProof Monotonic {} = error "Not implemented yet"
    matchProof (InjectiveRCD x rcd) = do
      printAlgEnv 1
      indexfns <- getIndexFns
      fp <- traverse fromAlgebra =<< askFiltPart (Algebra.Var x)
      case fp of
        Just (FiltPartInv y pf _ :: Property Symbol)
          | Just [f@(IndexFn (Forall i0 d) _)] <- M.lookup y indexfns ->
              -- x is a filtering/partition of y, hence x will be "opaque" (a gather
              -- on inverse indices), but we can try to prove the following logically equivalent query
              -- x[i] = x[j] => i = j   <=>   y[i'] = y[j'] ^ pf(i') ^ pf(j') => i' = j'
              algebraContext f $ do
                i <- newNameFromString "i"
                j <- newNameFromString "j"
                -- pf' <- fromAlgebra pf
                assume (predToFun pf i)
                assume (predToFun pf j)
                nextGenProver (PInjGe i j d rcd (body f))
                error "hello"
        -- NEXT-UP: ^ add FiltPart ys to env to pattern match here
        _ ->
          proveFn (PInjectiveRCD rcd) =<< getFn x
    matchProof (BijectiveRCD x rcd img) =
      proveFn (PBijectiveRCD rcd img) =<< getFn x
    matchProof (FiltPartInv x pf [(pp, split)]) = do
      f_X <- getFn x
      proveFn (PFiltPartInv (predToFun pf) (predToFun pp) split) f_X
    matchProof (FiltPartInv {}) = error "Not implemented yet"
    matchProof (FiltPart y x pf pps) = do
      -- FPV2
      f_Y <- getFn y
      i <- newNameFromString "i"
      n <- sym2SoP . Hole <$> newNameFromString "n"
      is_inv_hole <- newNameFromString "is^-1"
      let pattern_Y =
            IndexFn
              { iterator = Forall i $ Iota n,
                body =
                  cases
                    [ ( Bool True,
                        sym2SoP $ Idx (Var x) (sym2SoP $ Idx (Hole is_inv_hole) (sym2SoP $ Hole i))
                      )
                    ]
              }
      s <- unify pattern_Y f_Y
      -- Get is (the inverse of is^-1).
      let is_inv = justName =<< (M.!? is_inv_hole) . mapping =<< s
      is <- join <$> traverse getInvAlias is_inv
      mfpi <- join <$> traverse (askFiltPartInv . Algebra.Var) is
      mfpi' :: Maybe (Property Symbol) <- traverse fromAlgebra mfpi
      case mfpi' of
        Just (FiltPartInv z pf' pps') | Just z == is -> do
          -- If the predicates and split points are equivalent, we are done.
          s' <- unify (FiltPartInv z pf pps) (FiltPartInv z pf' pps')
          if isJust (s' :: Maybe (Substitution Symbol))
            then pure Yes
            else pure Unknown
        _ | Just z <- is -> prove (FiltPartInv z pf pps)
        _ -> pure Unknown

    getFn vn = do
      fs <- lookupIndexFn vn
      case fs of
        Just [f] -> pure f
        _ -> error $ "internal error: getFn " <> prettyStr vn <> " " <> prettyStr fs

    predToFun (Predicate vn e) arg =
      sop2Symbol $ rep (mkRep vn (sym2SoP $ Var arg)) e

proveFn :: Statement -> IndexFn -> IndexFnM Answer
proveFn (ForallSegments fprop) f@(IndexFn (Forall _ (Cat k _ _)) _) =
  prove_ True (fprop k) f
proveFn prop f = prove_ False prop f

data PStatement
  = -- Fresh i; fresh j; domain of index function; RCD; cases of index function.
    PInjGe VName VName Domain (SoP Symbol, SoP Symbol) (Cases Symbol (SoP Symbol))
  | PFiltPart VName (Predicate Symbol) (Predicate Symbol, SoP Symbol)

nextGenProver :: PStatement -> IndexFnM Answer
nextGenProver (PInjGe i j d (a, b) ges) = do
  -- WTS: e(i) = e(j) ^ c(i) ^ c(j) ^ a <= e(i) <= b ^ a <= e(j) <= b => i = j.
  allM [no_dups g | g <- casesToList ges]
  where
    e @ arg = rep (mkRep i (Var arg)) e

    iter_i = Forall i d
    iter_j = Forall j d

    out_of_range x = x :< a :|| b :< x

    no_dups (c, e) = rollbackAlgEnv $ do
      addRelIterator iter_i
      addRelIterator iter_j

      let oob =
            (sop2Symbol (c @ i) :&& sop2Symbol (c @ j))
              =>? (out_of_range (e @ i) :|| out_of_range (e @ j))
      let eq =
            (sop2Symbol (c @ i) :&& sop2Symbol (c @ j) :&& e @ i :== e @ j)
              =>? (sym2SoP (Var i) :== sym2SoP (Var j))
      oob `orM` eq
nextGenProver (PFiltPart {}) = undefined

prove_ :: Bool -> Statement -> IndexFn -> IndexFnM Answer
prove_ _ (PInjectiveRCD (a, b)) fn@(IndexFn (Forall i0 dom) _) = algebraContext fn $ do
  printM 1000 $
    title "Proving InjectiveRCD "
      <> "\n  RCD (a,b) = "
      <> prettyStr (a, b)
      <> "\n"
      <> prettyIndent 2 fn
  i <- newNameFromString "i"
  j <- newNameFromString "j"
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
            -- printM 1 $ prettyStr (e @ i .-. e @ j :: SoP Symbol)
            -- lol <- simplify (e @ i .-. e @ j :: SoP Symbol)
            -- printM 1 $ prettyStr lol
            oob `orM` neq

  -- printAlgEnv 1

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
prove_ is_segmented (PBijectiveRCD (a, b) (c, d)) f@(IndexFn (Forall i dom) _) = rollbackAlgEnv $ do
  printM 1000 $
    title "Proving BijectiveRCD "
      <> "\n  RCD (a,b) = "
      <> prettyStr (a, b)
      <> "\n  RCD_Img (c,d) = "
      <> prettyStr (c, d)
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
          prove_ is_segmented (PInjectiveRCD (a, b)) f

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
        answers <- mapM (queryCase (CaseCheck in_RCD) f) [0 .. length (guards f) - 1]
        let guards' =
              zipWith
                (\(p, e) ans -> if isYes ans then (p, e) else (p, infinity))
                (guards f)
                answers
        fX <- simplify $ IndexFn (Forall i dom) (cases guards')

        let guards_in_RCD = [(p, e) | (p, e) <- guards fX, e /= infinity]

        let step_2_2 = algebraContext fX $ do
              addRelIterator (iterator fX)
              let size_RCD_image = d .-. c .+. int2SoP 1

              start <- simplify $ if is_segmented then intervalStart dom else domainStart dom
              end <- simplify $ if is_segmented then intervalEnd dom else domainEnd dom

              j_sum <- newVName "j"
              let cs = map ((@ j_sum) . sym2SoP . fst) guards_in_RCD
              let size_X =
                    if null cs
                      then int2SoP 0
                      else toSumOfSums j_sum start end $ foldl1 (.+.) cs

              ans <- simplify $ size_RCD_image :== size_X
              printM 1000 $ "size_RCD_image " <> prettyStr size_RCD_image
              printM 1000 $ "size_X         " <> prettyStr size_X
              printM 1000 $ "Step (2.2) " <> prettyStr ans
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
prove_ baggage (PFiltPartInv pf pp split) f@(IndexFn (Forall i dom) _) = rollbackAlgEnv $ do
  printM 1000 $
    title "Proving FiltPartInv\n"
      <> "  filter:\n"
      <> prettyIndent 4 (Predicate i $ pf i)
      <> "\n  partition:\n"
      <> prettyIndent 4 (Predicate i $ pp i)
      <> "\n  partition split:\n"
      <> prettyIndent 4 split
      <> "\n  function:\n"
      <> prettyIndent 4 f
  -- Construct size after filtering.
  n <- simplify $ domainEnd dom
  j_sum <- newNameFromString "j_sum"
  m <- simplify $ toSumOfSums j_sum (int2SoP 0) n (sym2SoP $ pf j_sum)
  let mm1 = m .-. int2SoP 1

  -- (1) f restricted to [0, m - 1] is a permutation.
  step1 <-
    printTrace 1000 "FiltPartInv Step (1)" $
      prove_ baggage (PBijectiveRCD (int2SoP 0, mm1) (int2SoP 0, mm1)) f

  -- (2) f maps filtered indices to values that are not in [0, m].
  let step2 = algebraContext f $ do
        addRelIterator (iterator f)
        printTrace 1000 "FiltPartInv Step (2)" $
          allM [if_filtered_then_OOB g | g <- guards f]
        where
          if_filtered_then_OOB (c, e) = rollbackAlgEnv $ do
            (c =>? pf i) `orM` (c =>? (int2SoP 0 :> e :|| e :>= m))
  -- let step2 = askQ (CaseCheck if_filtered_then_OOB) f
  --       where
  --         if_filtered_then_OOB e =
  --           pf i :|| (int2SoP 0 :> e :|| e :>= m)

  j <- newNameFromString "j"
  let step3and4 = algebraContext f $ do
        addRelIterator (iterator f)
        j +< i
        printTrace 1000 "FiltPartInv Steps (3--4)" $
          allM [mono_strict_inc g | g <- guards f]
        where
          mono_strict_inc (c, e) =
            rollbackAlgEnv $
              do
                -- WTS: j < i ^ c(i) ^ c(j) => e(i) < e(j).
                sop2Symbol (c @ i) =>? Not (pf i)
                `orM` (sop2Symbol (c @ j) =>? Not (pf j))
                `orM` ((sop2Symbol (c @ j) :&& sop2Symbol (c @ i)) =>? (e @ j :< e @ i))

  let step5and6 = do
        gs <- simplify $ cases (map (second sym2SoP) $ gsT <> gsF)
        let gs' = cases [(c, e) | (c, e) <- casesToList gs, c /= Bool False]
        printTrace 1000 "FiltPartInv Steps (5--6)" $
          askQ Truth (IndexFn (iterator f) gs')
        where
          gsT = [(pp i :&& c, Not (pf i) :|| e :< split) | (c, e) <- guards f]
          gsF = [(Not (pp i) :&& c, Not (pf i) :|| e :>= split) | (c, e) <- guards f]

  pure step1 `andM` step2 `andM` step3and4 `andM` step5and6
  where
    fn @ idx = rep (mkRep i (Var idx)) fn
prove_ baggage (PermutationOfRange start end) f =
  prove_ baggage (PBijectiveRCD (start, end) (start, end)) f
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

justName :: SoP Symbol -> Maybe VName
justName e | Just (Var vn) <- justSym e = Just vn
justName _ = Nothing
