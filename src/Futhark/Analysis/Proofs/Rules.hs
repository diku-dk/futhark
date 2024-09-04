module Futhark.Analysis.Proofs.Rules
where

import Futhark.Analysis.Proofs.Unify
import Futhark.SoP.SoP (SoP, sym2SoP, (.+.), int2SoP, (.-.), sopToList, sopFromList, numTerms)
import Futhark.MonadFreshNames
import Futhark.Analysis.Proofs.Symbol (Symbol(..), sop2Symbol)
import Control.Monad (foldM, msum)
import Futhark.SoP.FourierMotzkin (($<=$))
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Data.List (subsequences, (\\))
import Futhark.SoP.Util (ifM)
import Futhark.Analysis.Proofs.Traversals (ASTMapper(..), astMap)

data Rule a b m = Rule {
    name :: String,
    from :: a,
    to :: Substitution b -> m a
  }

class Monad m => Rewritable u m where
  rewrite :: u -> m u

yoink :: SoP Symbol -> IndexFnM (SoP Symbol)
yoink sop = rulesSoP >>= foldM (flip matchSoP) sop

instance Rewritable (SoP Symbol) IndexFnM where
  rewrite = astMap m
    where
      m = ASTMapper { mapOnSymbol = rewrite,
                      mapOnSoP = \sop -> rulesSoP >>= foldM (flip matchSoP) sop
                    }

instance Rewritable Symbol IndexFnM where
  rewrite = astMap m
    where
      m = ASTMapper { mapOnSymbol = \x -> rulesSymbol >>= foldM (flip matchSymbol) x,
                      mapOnSoP = rewrite
                    }

-- Apply SoP-rule with k terms to all matching k-subterms in a SoP.
-- For example, given rule `x + x => 2x` and SoP `a + b + c + a + b`,
-- it matches `a + a` and `b + b` and returns `2a + 2b + c`.
matchSoP :: ( Replaceable u (SoP u)
            , Unify u (SoP u) m
            , Ord u) => Rule (SoP u) (SoP u) m -> SoP u -> m (SoP u)
matchSoP rule sop
  | numTerms (from rule) <= numTerms sop = do
    let (subterms, contexts) = unzip . combinations $ sopToList sop
    -- Get first valid subterm substitution. Recursively match context.
    subs <- mapM (unify (from rule) . sopFromList) subterms
    case msum $ zipWith (\x y -> (,y) <$> x) subs contexts of
      Just (sub, ctx) -> (.+.) <$> matchSoP rule (sopFromList ctx) <*> to rule sub
      Nothing -> pure sop
  | otherwise = pure sop
  where
    -- Get all (k-subterms, remaining subterms).
    k = numTerms (from rule)
    combinations xs = [(s, xs \\ s) | s <- subsequences xs, length s == k]

matchSymbol :: Rule Symbol (SoP Symbol) IndexFnM -> Symbol -> IndexFnM Symbol
matchSymbol rule symbol = do
    s :: Maybe (Substitution (SoP Symbol)) <- case from rule of
      x :&& y -> matchCommutativeRule (:&&) x y
      x :|| y -> matchCommutativeRule (:||) x y
      x -> unify x symbol
    maybe (pure symbol) (to rule) s
    where
      matchCommutativeRule op x y =
        msum <$> mapM (flip unify symbol) [x `op` y, y `op` x]

rulesSoP :: IndexFnM [Rule (SoP Symbol) (SoP Symbol) IndexFnM]
rulesSoP = do
  i <- newVName "i"
  h1 <- newVName "h"
  h2 <- newVName "h"
  h3 <- newVName "h"
  x1 <- newVName "x"
  y1 <- newVName "y"
  pure
    [ Rule
        { name = "Extend sum lower bound (1)"
        , from = LinComb i (sop h1 .+. int 1) (sop h2) (Var h3)
                   ~+~ Idx (Var h3) (sop h1)
        , to = \s -> pure . rep s $ LinComb i (sop h1) (sop h2) (Var h3)
        }
    , Rule
        { name = "Extend sum lower bound (2)"
        , from = LinComb i (sop h1) (sop h2) (Var h3)
                   ~+~ Idx (Var h3) (sop h1 .-. int 1)
        , to = \s -> pure . rep s $
                  LinComb i (sop h1 .-. int 1) (sop h2) (Var h3)
        }
    , Rule
        { name = "Extend sum upper bound (1)"
        , from = LinComb i (sop h1) (sop h2 .-. int 1) (Var h3)
                   ~+~ Idx (Var h3) (sop h2)
        , to = \s -> pure . rep s $ LinComb i (sop h1) (sop h2) (Var h3)
        }
    , Rule
        { name = "Extend sum upper bound (2)"
        , from = LinComb i (sop h1) (sop h2) (Var h3)
                   ~+~ Idx (Var h3) (sop h2 .+. int 1)
        , to = \s -> pure . rep s $
                  LinComb i (sop h1) (sop h2 .+. int 1) (Var h3)
        }
    , let e = LinComb i (sop h1) (sop x1) (Var h2)
                ~-~ LinComb i (sop h1) (sop y1) (Var h2)
      in Rule
        { name = "Merge sum-subtractation"
        , from = e
        , to = \s ->
           ifM
             (rep s (sop y1) $<=$ rep s (sop x1))
             (pure . rep s $ LinComb i (sop y1 .+. int 1) (sop x1) (Var h2))
             (pure $ rep s e)
        }
      , Rule
          { name = "[[¬x]] => 1 - [[x]]"
          , from = sym2SoP $ Indicator (Not (Var h1))
          , to = \s -> pure . rep s $ int 1 .-. sym2SoP (Indicator (Var h1))
          }
    ]
  where
    int = int2SoP
    sop = sym2SoP . Var
    a ~+~ b = sym2SoP a .+. sym2SoP b
    a ~-~ b = sym2SoP a .-. sym2SoP b

-- TODO can all of these be handled by `normalize`? If so, remove.
rulesSymbol :: IndexFnM [Rule Symbol (SoP Symbol) IndexFnM]
rulesSymbol = do
  h1 <- newVName "h"
  pure
    [ Rule
        { name = ":&& identity"
        , from = Bool True :&& Var h1
        , to = \s -> pure . sop2Symbol . rep s $ Var h1
        }
    , Rule
        { name = ":&& annihilation"
        , from = Bool False :&& Var h1
        , to = \_ -> pure $ Bool False
        }
    , Rule
        { name = ":|| identity"
        , from = Bool False :|| Var h1
        , to = \s -> pure . sop2Symbol . rep s $ Var h1
        }
    , Rule
        { name = ":|| annihilation"
        , from = Bool True :|| Var h1
        , to = \_ -> pure $ Bool True
        }
    ]
    --   Rule {
    --     name = "&& idempotence",
    --     from = Var h1 :&& Var h2,
    --     to = \s -> do
    --       let x = sop2Symbol $ rep s (sym2SoP $ Var h1)
    --       let y = sop2Symbol $ rep s (sym2SoP $ Var h2)
    --       pure $ if x == y then x else x :&& y
    --   }
