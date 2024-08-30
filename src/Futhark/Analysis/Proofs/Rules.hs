module Futhark.Analysis.Proofs.Rules
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP, (.-.))
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol (Symbol(..))
import Control.Monad (foldM)
import Futhark.Util.Pretty
import Debug.Trace (traceM)

data Rule a b = Rule {
    name :: String,
    from :: a,
    to :: Substitution b -> b
  }

class Unify u u m => Rewritable u m where
  rules :: m [Rule u u]
  rewrite :: u -> m u
  rewrite x = rules >>= foldM applyRule x
    where
      applyRule v rule = do
        s <- unify (from rule) v
        -- traceM ("applyRule Substitution \n" <> prettyString s)
        pure $ maybe v (to rule) s

instance MonadFreshNames m => Rewritable (SoP Symbol) m where
  rules :: m [Rule (SoP Symbol) (SoP Symbol)]
  rules = do
    i <- newVName "i"
    h1 <- newVName "h"
    h2 <- newVName "h"
    h3 <- newVName "h"
    pure [ Rule -- TODO check that h2 + 1 == lb using Fourier Motzkin instead?
             "Extend sum lower bound (1)"
             (Idx (Var h1) (sop h2) ~+~ LinComb i (sop h2 .+. int 1) (sop h3) (Var h1))
             (\s -> rep s $ LinComb i (sop h2) (sop h3) (Var h1))
         , Rule -- TODO check that h2 - 1 == lb using Fourier Motzkin instead?
             "Extend sum lower bound (2)"
             (Idx (Var h1) (sop h2 .-. int 1) ~+~ LinComb i (sop h2) (sop h3) (Var h1))
             (\s -> rep s $ LinComb i (sop h2 .-. int 1) (sop h3) (Var h1))
         , Rule -- TODO check that h2 - 1 == lb using Fourier Motzkin instead?
             "Extend sum lower bound (2)"
             (Idx (Var h1) (sop h2 .-. int 1) ~+~ LinComb i (sop h2) (sop h3) (Var h1))
             (\s -> rep s $ LinComb i (sop h2 .-. int 1) (sop h3) (Var h1))
         ]
    where
      int = int2SoP
      sop = sym2SoP . Var
      a ~+~ b = sym2SoP a .+. sym2SoP b
