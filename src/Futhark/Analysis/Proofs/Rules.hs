module Futhark.Analysis.Proofs.Rules
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP, (.-.), sopToList, sopFromList)
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol (Symbol(..), sop2Symbol)
import Control.Monad (foldM, msum)
import Futhark.SoP.FourierMotzkin (($<=$))
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Data.List (subsequences, (\\))
import Futhark.Util.Pretty (Pretty)

data Rule a b m = Rule {
    name :: String,
    from :: a,
    to :: Substitution b -> m a
  }

data CommutativeRule a b c m = CommutativeRule {
    commName :: String,
    commFrom :: c -> c -> a,
    commArg1 :: c,
    commArg2 :: c,
    commTo :: Substitution b -> m a
  }

class RuleMatchable a b m where
  match :: (Monad m) => Rule a b m -> a -> m a

instance ( MonadFreshNames m
         , Replaceable u (SoP u)
         , Renameable u
         , Unify u (SoP u) m
         , Show u
         , Pretty u
         , Ord u) => RuleMatchable (SoP u) (SoP u) m where
  match rule sop = do
    subs :: [Maybe (Substitution (SoP u))] <- mapM (unify $ from rule) matchables
    -- Get first valid substitution and context.
    case msum $ zipWith (\x y -> (,y) <$> x) subs contexts of
      Just (s, ctx) -> (.+. ctx) <$> to rule s
      Nothing -> pure sop
    where
      -- Get all "sub-SoPs" of length k.
      k = length (sopToList $ from rule)
      -- combinations xs = [s | s <- subsequences xs, length s == k]
      -- Get all (k-combination, remaining elements).
      combinations xs = [(s, xs \\ s) | s <- subsequences xs, length s == k]
      (subterms, otherTerms) = unzip . combinations . sopToList $ sop
      matchables = map sopFromList subterms
      contexts = map sopFromList otherTerms

class Monad m => Rewritable u m where
  rewrite :: u -> m u

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite x = rules >>= foldM applyRule x
    where
      applyRule :: SoP Symbol -> Rule (SoP Symbol) (SoP Symbol) IndexFnM -> IndexFnM (SoP Symbol)
      applyRule v rule = do
        s <- unify (from rule) v
        maybe (pure v) (to rule) s
      rules :: IndexFnM [Rule (SoP Symbol) (SoP Symbol) IndexFnM]
      rules = do
        i <- newVName "i"
        h1 <- newVName "h"
        h2 <- newVName "h"
        h3 <- newVName "h"
        x1 <- newVName "x"
        y1 <- newVName "y"
        pure [ Rule
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
                    pure $
                      if b
                      then rep s $ LinComb i (sop y1 .+. int 1) (sop x1) (Var h2)
                      else rep s from)
             ]
      int = int2SoP
      sop = sym2SoP . Var
      a ~+~ b = sym2SoP a .+. sym2SoP b
      a ~-~ b = sym2SoP a .-. sym2SoP b

instance Rewritable Symbol IndexFnM where
  rewrite symbol = do
    rs <- rules
    foldM applyRule symbol rs
    where
      applyRule :: Symbol -> Rule Symbol (SoP Symbol) IndexFnM -> IndexFnM Symbol
      applyRule v rule = do
        s <- unify (from rule) v
        maybe (pure v) (to rule) s
      rules :: IndexFnM [Rule Symbol (SoP Symbol) IndexFnM]
      rules = do
        h1 <- newVName "h"
        pure $ commutative
              [ CommutativeRule
                  "&& identity"
                  (:&&) (Bool True) (Var h1)
                  (\s -> pure . sop2Symbol . rep s $ Var h1)
              , CommutativeRule
                  "&& annihilation"
                  (:&&) (Bool False) (Var h1)
                  (\_ -> pure $ Bool False)
              , CommutativeRule
                  "|| identity"
                  (:||) (Bool False) (Var h1)
                  (\s -> pure . sop2Symbol . rep s $ Var h1)
              , CommutativeRule
                  "|| annihilation"
                  (:||) (Bool True) (Var h1)
                  (\_ -> pure $ Bool True )
              ]
              -- ++ [
              --   Rule {
              --     name = "&& idempotence",
              --     from = Var h1 :&& Var h2,
              --     to = \s -> do
              --       let x = sop2Symbol $ rep s (sym2SoP $ Var h1)
              --       let y = sop2Symbol $ rep s (sym2SoP $ Var h2)
              --       pure $ if x == y then x else x :&& y
              --   }
              -- ]
      commutative = concatMap comm2rules
      comm2rules (CommutativeRule n f x y t) =
        [Rule n (f x y) t, Rule n (f y x) t]
