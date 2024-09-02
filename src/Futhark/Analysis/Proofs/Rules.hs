module Futhark.Analysis.Proofs.Rules
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP, (.-.))
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol (Symbol(..))
import Control.Monad (foldM)
import Futhark.SoP.Monad (MonadSoP (..))
import Language.Futhark qualified as E
import Futhark.SoP.FourierMotzkin (($<=$))

data Rule a b m = Rule {
    name :: String,
    from :: a,
    to :: Substitution b -> m b
  }
data CommutativeRule a b c m = CommutativeRule {
    commName :: String,
    commFrom :: c -> c -> a,
    commArg1 :: c,
    commArg2 :: c,
    commTo :: Substitution b -> m b
  }

rewrite :: (MonadSoP u E.Exp m, Unify u (SoP u) m, Replaceable u (SoP u)) => m [Rule (SoP u) (SoP u) m] -> SoP u -> m (SoP u)
rewrite rules x = rules >>= foldM applyRule x
  where
    applyRule v rule = do
      s <- unify (from rule) v
      maybe (pure v) (to rule) s

-- TODO rewrite as type class once I am sure that FourierMotzkin is needed in rules.
sopRules :: MonadSoP Symbol E.Exp m => m [Rule (SoP Symbol) (SoP Symbol) m]
sopRules = do
  i <- newVName "i"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  x1 <- newVName "x"
  y1 <- newVName "y"
  pure $ [ Rule
           "Extend sum lower bound (1)"
           (LinComb i (sop h1 .+. int 1) (sop h2) (Var h3) ~+~ Idx (Var h3) (sop h1))
           (\s -> pure . rep s $ LinComb i (sop h1) (sop h2) (Var h3))
       , Rule
           "Extend sum lower bound (2)"
           (LinComb i (sop h1) (sop h2) (Var h3) ~+~ Idx (Var h3) (sop h1 .-. int 1))
           (\s -> pure . rep s $ LinComb i (sop h1 .-. int 1) (sop h2) (Var h3))
       , Rule
           "Extend sum upper bound (1)"
           (LinComb i (sop h1) (sop h2 .-. int 1) (Var h3) ~+~ Idx (Var h3) (sop h2))
           (\s -> pure . rep s $ LinComb i (sop h1) (sop h2) (Var h3))
       , Rule
           "Extend sum upper bound (2)"
           (LinComb i (sop h1) (sop h2) (Var h3) ~+~ Idx (Var h3) (sop h2 .+. int 1))
           (\s -> pure . rep s $ LinComb i (sop h1) (sop h2 .+. int 1) (Var h3))
       , let from = LinComb i (sop h1) (sop x1) (Var h2) ~-~ LinComb i (sop h1) (sop y1) (Var h2)
         in Rule
           "Merge sum-subtractation"
           (LinComb i (sop h1) (sop x1) (Var h2) ~-~ LinComb i (sop h1) (sop y1) (Var h2))
           (\s -> do
              b <- rep s (sop y1) $<=$ rep s (sop x1)
              if b
              then pure . rep s $ LinComb i (sop y1 .+. int 1) (sop x1) (Var h2)
              else pure . rep s $ from)
       ] ++ commutative [
        CommutativeRule
          "&& identity"
          (\x y -> sym2SoP $ x :&& y) (Bool True) (Var h1)
          (\s -> pure . rep s $ Var h1),
        CommutativeRule
          "&& annihilation"
          (\x y -> sym2SoP $ x :&& y) (Bool False) (Var h1)
          (\_ -> pure . sym2SoP $ Bool False),
        CommutativeRule
          "|| identity"
          (\x y -> sym2SoP $ x :|| y) (Bool False) (Var h1)
          (\s -> pure . rep s $ Var h1),
        CommutativeRule
          "|| annihilation"
          (\x y -> sym2SoP $ x :|| y) (Bool True) (Var h1)
          (\_ -> pure . sym2SoP $ Bool True )
      ]
  where
    int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b
    commutative = concatMap comm2rules
    comm2rules (CommutativeRule n f x y t) = [Rule n (f x y) t, Rule n (f y x) t]
