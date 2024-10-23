-- Answer queries on index functions using algebraic solver.

module Futhark.Analysis.Proofs.Query where

import Control.Monad (foldM, forM_, unless)
import Data.Maybe (catMaybes, fromJust)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, addRelSymbol, rollbackAlgEnv, simplify, toRel)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (IndexFn (..), Iterator (..), getCase, casesToList, Domain (Iota))
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrintAlgEnv, debugPrettyM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, toDNF)
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.Monad (addRange, mkRange, mkRangeLB)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, sym2SoP, (.-.))
import Language.Futhark (VName)
import GHC.Base (assert)
import Control.Monad (when)

data MonoDir = Inc | IncStrict | Dec | DecStrict
  deriving (Show, Eq, Ord)

data Query
  = CaseIsMonotonic MonoDir
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseTransform (SoP Symbol -> SoP Symbol)

data Answer = Yes | Unknown
  deriving (Show, Eq)

-- | Answers a query on an index function case.
ask :: Query -> IndexFn -> Int -> IndexFnM Answer
ask query (IndexFn it cs) case_idx = rollbackAlgEnv $ do
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
          let p_j = fromJust . justSym $ rep (mkRep i (Var j)) p
          let q_j = rep (mkRep i (Var j)) q
          addRelSymbol p_j
          let rel = case dir of
                Inc -> (:<=)
                IncStrict -> (:<)
                Dec -> (:>=)
                DecStrict -> (:>)
          debugPrintAlgEnv
          debugPrettyM "ask" (q_j `rel` q)
          isTrue . sym2SoP $ q_j `rel` q
        Empty -> undefined

data Property
  = PermutationOf VName
  | PermutationOfZeroTo (SoP Symbol)

prove :: Property -> IndexFn -> IndexFnM Answer
prove (PermutationOf {}) _fn = undefined
prove (PermutationOfZeroTo m) (IndexFn it@(Forall i (Iota n)) cs) = do
  -- 1. Show that m = n.
  ans <- isTrue . sym2SoP $ m :== n
  case ans of
    Unknown -> pure Unknown
    Yes -> do
      addRelIterator it
      -- Hardcode two cases for now.
      case casesToList cs of
        [(p, x), (not_p, y)] -> do
          unless (p == neg not_p) $ error "inconsistency"
          -- 2. Prove no overlap in cases.
          -- TODO considering x the "smaller" case here, also need to switch roles
          -- Sufficient: case 1 < case 2.
          -- Case: i1 > i2
          -- [ ] fresh i1 i2
          -- [ ] assume i1 > i2
          -- [ ] substitute i1 and i2 appropriately
          no_overlap1 <- isTrue . sym2SoP $ x :< y
          -- Case: i1 < i2
          let no_overlap2 = undefined
          let no_overlap = no_overlap1 `also` no_overlap2
          -- 3. Prove no duplicates in first case.
          -- Make it a property? Whose proof could be done using `ask CaseIsMonotonic`?
          -- 4. Prove no duplicates in second case.
          -- 5. Prove both cases are bounded from below by 0.
          -- x >= 0
          -- y >= 0
          -- 6. Prove both cases are bounded from above by m.
          -- x <= m
          -- y <= m
          pure $ no_overlap `also` undefined
        _ -> undefined
prove (PermutationOfZeroTo {}) _ = pure Unknown

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

isYes :: Answer -> Bool
isYes Yes = True
isYes _ = False

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False

also :: Answer -> Answer -> Answer
also Yes Yes = Yes
also _ _ = Unknown
