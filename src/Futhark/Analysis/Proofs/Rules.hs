module Futhark.Analysis.Proofs.Rules
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP)
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Term (Term(..))
import Control.Monad (foldM)
import Futhark.Util.Pretty
import Debug.Trace (traceM)

data Rule a b = Rule {
    name :: String,
    from :: a,
    to :: Substitution b -> b
  }

rewrite :: (Pretty v, Unify v v m) => m [Rule v v] -> v -> m v
rewrite rules x = rules >>= foldM applyRule x
  where
    applyRule v rule = do
      s <- unify (from rule) v
      traceM ("applyRule Substitution \n" <> prettyString s)
      pure $ maybe v (to rule) s

sopRules :: MonadFreshNames m => m [Rule (SoP Term) (SoP Term)]
sopRules = do
  i <- newVName "i"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  pure [ Rule
           "Extend sum lower bound"
           (Idx (Var h1) (sop h2) ~+~ LinComb i (sop h2 .+. int 1) (sop h3) (Var h1))
           (\s -> rep s $ LinComb i (sop h2) (sop h3) (Var h1))
       ]
  where
    int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
