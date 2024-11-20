{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.IndexFn where

import Control.Monad ((<=<))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Futhark.Analysis.Proofs.Symbol
import Futhark.SoP.SoP (SoP, int2SoP, justSym)
import Language.Futhark (VName)

data IndexFn = IndexFn
  { iterator :: Iterator,
    body :: Cases Symbol (SoP Symbol)
  }
  deriving (Show, Eq)

data Domain
  = Iota (SoP Symbol) -- [0, ..., n-1]
  | Cat -- Catenate_{k=0}^{m-1} [b_k, ..., b_{k+1})
      VName -- k
      (SoP Symbol) -- m
      (SoP Symbol) -- b
  deriving (Show, Eq, Ord)

data Iterator
  = Forall VName Domain
  | Empty
  deriving (Show)

instance Eq Iterator where
  (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
  (Forall _ u) == (Forall _ v) = u == v
  Empty == Empty = True
  _ == _ = False

newtype Cases a b = Cases (NE.NonEmpty (a, b))
  deriving (Show, Eq, Ord)

cases :: [(a, b)] -> Cases a b
cases = Cases . NE.fromList

casesToList :: Cases a b -> [(a, b)]
casesToList (Cases cs) = NE.toList cs

getCase :: Int -> Cases a b -> (a, b)
getCase n (Cases cs) = NE.toList cs !! n

getPredicates :: IndexFn -> [Symbol]
getPredicates (IndexFn _ cs) = map fst $ casesToList cs

getIteratorVariable :: IndexFn -> Maybe VName
getIteratorVariable (IndexFn (Forall i _) _) = Just i
getIteratorVariable _ = Nothing

getCases :: IndexFn -> Cases Symbol (SoP Symbol)
getCases (IndexFn _ cs) = cs

justSingleCase :: IndexFn -> Maybe (SoP Symbol)
justSingleCase f
  | [(Bool True, f_val)] <- casesToList $ body f =
    Just f_val
  | otherwise =
    Nothing

getCatIteratorVariable :: IndexFn -> Maybe VName
getCatIteratorVariable (IndexFn (Forall _ (Cat k _ _)) _) = Just k
getCatIteratorVariable _ = Nothing

domainSegStart :: Domain -> SoP Symbol
domainSegStart (Iota _) = int2SoP 0
domainSegStart (Cat _ _ b) = b

unzipT :: IndexFn -> [IndexFn]
unzipT (IndexFn iter (Cases cs))
  | Just vss <- mapM (getTuple <=< justSym . snd) (NE.toList cs),
    n <- length (head vss),
    all ((==) n . length) vss -- All branches are n-tuples.
    =
      let ps = map fst (NE.toList cs)
       in map (\vs -> IndexFn iter (cases $ zip ps vs)) (L.transpose vss)
  where
    getTuple (Tuple vs) = Just vs
    getTuple _ = Nothing
unzipT indexfn = [indexfn]
