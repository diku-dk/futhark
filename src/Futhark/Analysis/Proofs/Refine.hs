module Futhark.Analysis.Proofs.Refine
where

import Futhark.SoP.SoP (SoP, sym2SoP, scaleSoP, int2SoP, (.+.), (.*.), Term, termToList, sopToList)
import Futhark.Analysis.Proofs.Symbol (Symbol(..))
import Futhark.SoP.Monad (substEquivs)
import Futhark.SoP.FourierMotzkin (($==$), ($/=$), ($>=$), ($>$), ($<$), ($<=$))
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Data.Foldable (foldrM)

class Monad m => Refineable u v m where
  refine :: u -> m v

-- getSoP :: SoP u -> [([u], Integer)]
-- getSoP = SoP.sopToLists . SoP.normalize

instance Refineable (Term Symbol, Integer) (SoP Symbol) IndexFnM where
  refine (ts, c) = do
    terms <- mapM refine $ termToList ts
    pure . scaleSoP c $ foldr (.*.) (int2SoP 1) terms

instance Refineable (SoP Symbol) (SoP Symbol) IndexFnM where
  refine x = do
    sops <- mapM refine $ sopToList x
    pure $ foldr (.+.) (int2SoP 0) sops

instance Refineable Symbol (SoP Symbol) IndexFnM where
  refine (Var vn) = substEquivs . sym2SoP $ Var vn
  refine (x :== y) = sym2SoP <$> refineRelation (:==) x y
  refine (x :/= y) = sym2SoP <$> refineRelation (:/=) x y
  refine (x :> y)  = sym2SoP <$> refineRelation (:>) x y
  refine (x :>= y) = sym2SoP <$> refineRelation (:>=) x y
  refine (x :< y) = sym2SoP <$> refineRelation (:<) x y
  refine (x :<= y) = sym2SoP <$> refineRelation (:<=) x y
  -- refine (x :&& y) = refineRelation (:&&) x y
  -- refine (x :|| y) = refineRelation (:||) x y
  refine x = pure . sym2SoP $ x

refineRelation :: (SoP Symbol -> SoP Symbol -> Symbol) -> SoP Symbol -> SoP Symbol -> IndexFnM Symbol
refineRelation rel x y = do
  x' <- refine x
  y' <- refine y
  b <- solve (x' `rel` y')
  pure $ if b then Bool True else x' `rel` y'
  where
    -- Use Fourier-Motzkin elimination to determine the truth value
    -- of an expresion, if it can be determined in the given environment.
    -- If the truth value cannot be determined, False is also returned.
    solve (Bool True :&& Bool True) = pure True
    solve (Bool True :|| _) = pure True
    solve (_ :|| Bool True) = pure True
    solve (a :== b) = a $==$ b
    solve (a :/= b) = a $/=$ b
    solve (a :> b)  = a $>$ b
    solve (a :>= b) = a $>=$ b
    solve (a :< b)  = a $<$ b
    solve (a :<= b) = a $<=$ b
    solve _ = pure False
