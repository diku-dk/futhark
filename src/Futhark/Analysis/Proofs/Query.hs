-- Answer queries on index functions using algebraic solver.
{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.Query where

import Control.Monad (foldM, forM_)
import Data.Maybe (catMaybes)
import Futhark.Analysis.Proofs.IndexFn (IndexFn (..), getCase)
import Futhark.Analysis.Proofs.Monad (IndexFnM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, toDNF)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (SoP, justSym, sym2SoP)
import Futhark.Analysis.Proofs.AlgebraBridge (rollbackAlgEnv, addRelIterator, addRelSymbol, simplify, toRel)

data Query
  = CaseIsMonotonic
  | -- Apply transform to case value, then check whether it simplifies to true.
    CaseTransform (SoP Symbol -> SoP Symbol)

data Answer = Yes | Unknown

-- Answers a query on an index function case.
ask :: Query -> IndexFn -> Int -> IndexFnM Answer
ask query (IndexFn it cs) case_idx = rollbackAlgEnv $ do
  let (p, q) = getCase case_idx cs
  addRelIterator it
  addRelSymbol p
  case query of
    CaseTransform transf -> isTrue $ transf q
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
