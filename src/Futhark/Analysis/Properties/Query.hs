-- Answer queries on index functions using algebraic solver.
module Futhark.Analysis.Properties.Query
  ( Answer (..),
    MonoDir (..),
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
  )
where

import Control.Monad (forM, when)
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraBridge (Answer (..), addRelIterator, algebraContext, assume, isTrue, ($/=), ($<), ($<=), ($==), ($>), ($>=))
import Futhark.Analysis.Properties.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Properties.IndexFn (IndexFn (..), Iterator (..), casesToList, getCase, guards)
import Futhark.Analysis.Properties.Monad (IndexFnM, debugT, rollbackAlgEnv, printM)
import Futhark.Analysis.Properties.Symbol (Symbol (..), sop2Symbol, toDNF)
import Futhark.Analysis.Properties.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.Monad (lookupRange)
import Futhark.SoP.Refine (addRels)
import Futhark.SoP.SoP (Range (..), Rel (..), SoP, int2SoP, justSym, sym2SoP, (.*.))
import Language.Futhark (VName)
import Prelude hiding (GT, LT)
import Futhark.Analysis.Properties.Util (prettyIndent)

data MonoDir = Inc | IncStrict | Dec | DecStrict
  deriving (Show, Eq, Ord)

data Query
  = CaseIsMonotonic MonoDir
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
-- p =>? q = dnfQuery p (check q)
p =>? q = do
  ans <- dnfQuery p (check q)
  when (isUnknown ans) $
    printM 3000 $ "Failed to show:\n" <> prettyIndent 4 p <> "\n =>?\n" <> prettyIndent 4 q
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
check (Prop prop) = undefined
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
