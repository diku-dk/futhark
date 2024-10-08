-- | Performs a traversal of a SoP that matches each
--   pattern of a term with all patterns of the other
--   terms.
module Futhark.Analysis.Proofs.AlgebraPC.All2AllDriver
  ( simplifyAll2All,
  )
where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.MultiSet qualified as MS
import Futhark.Analysis.Proofs.AlgebraPC.BinaryRules
import Futhark.Analysis.Proofs.AlgebraPC.Monad
import Futhark.Analysis.Proofs.AlgebraPC.Symbol
import Futhark.SoP.Expression
import Futhark.SoP.Monad
import Futhark.SoP.SoP

simplifyAll2All ::
  (MonadSoP Symbol e Property (AlgM e), Expression e, Ord e) =>
  SoP Symbol ->
  AlgM e (Bool, SoP Symbol)
simplifyAll2All sop = do
  let exp_terms =
        map expandSumIdxTerm $
          M.toList $
            getTerms sop
  -- \^ this is a list of Maps matching sop's terms
  mr <- matchLstQuad exp_terms
  -- \^ try all with all matches (quadratic)
  case mr of
    Nothing -> pure (False, sop)
    Just (sop_new, sop_old) -> do
      (_, sop') <- simplifyAll2All $ (sop .-. sop_old) .+. sop_new
      -- \^ simplify to a fix point.
      pure (True, sop')
  where
    matchLstQuad [] = pure Nothing
    matchLstQuad (el : els) = do
      mr <- foldM (ff el) Nothing els
      case mr of
        Just {} -> pure mr
        Nothing -> matchLstQuad els
    ff _ acc@Just {} _ = pure acc
    ff tab1 Nothing tab2 = matchMapWithMap tab1 tab2
    matchMapWithMap tab1 tab2 =
      matchAllWithAll simplifyPair (M.toList tab1) (M.toList tab2)

expandSumIdxTerm :: (Term Symbol, Integer) -> M.Map Symbol (Term Symbol, Integer)
expandSumIdxTerm (Term ms, k) =
  M.fromList $ mapMaybe f $ MS.toOccurList ms
  where
    newSymTerm sym = Just (sym, (Term (MS.delete sym ms), k))
    f (sym@Idx {}, 1) = newSymTerm sym
    f (sym@Sum {}, 1) = newSymTerm sym
    f (sym@Mdf {}, 1) = newSymTerm sym
    f _ = Nothing

matchAllWithAll ::
  (a -> a -> AlgM e (Maybe b)) ->
  [a] ->
  [a] ->
  AlgM e (Maybe b)
matchAllWithAll fMatch els1 els2 =
  foldM (ff1 els2) Nothing els1
  where
    ff1 _ acc@Just {} _ = pure acc
    ff1 lst Nothing el1 = foldM (ff2 el1) Nothing lst
    ff2 _ acc@Just {} _ = pure acc
    ff2 el1 Nothing el2 = fMatch el1 el2
