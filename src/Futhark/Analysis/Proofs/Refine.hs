module Futhark.Analysis.Proofs.Refine
where

import Futhark.SoP.SoP (SoP, sym2SoP, scaleSoP, int2SoP, (.+.), (.*.), Term, termToList, sopToList)
import Futhark.Analysis.Proofs.Symbol (Symbol(..))
import Futhark.SoP.Monad (substEquivs)
import Futhark.SoP.FourierMotzkin (($==$), ($/=$), ($>=$), ($>$), ($<$), ($<=$))
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Futhark.Analysis.Proofs.Traversals (normalize)

class Monad m => Refineable u v m where
  refine :: u -> m v

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
  refine x = sym2SoP <$> refine x

instance Refineable Symbol Symbol IndexFnM where
  refine symbol = do
    symbol' <- normalize symbol
    case symbol' of
      Not x -> Not <$> refine x
      x :== y -> refineRelation (:==) x y
      x :/= y -> refineRelation (:/=) x y
      x :> y -> refineRelation (:>) x y
      x :>= y -> refineRelation (:>=) x y
      x :< y -> refineRelation (:<) x y
      x :<= y -> refineRelation (:<=) x y
      x :&& y -> refineRelation (:&&) x y
      x :|| y -> refineRelation (:||) x y
      x -> pure x
      where
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
