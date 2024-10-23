-- Answer queries on index functions using algebraic solver.

module Futhark.Analysis.Proofs.Query where

import Control.Monad (foldM, forM_)
import Data.Maybe (catMaybes, fromJust)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, addRelSymbol, rollbackAlgEnv, simplify, toRel)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (IndexFn (..), Iterator (..), getCase)
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrintAlgEnv, debugPrettyM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, toDNF)
import Futhark.Analysis.Proofs.Unify (mkRep, rep)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.Monad (addRange, mkRange, mkRangeLB)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, sym2SoP, (.-.))

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
    _ -> undefined

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

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False
