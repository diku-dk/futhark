module Futhark.Analysis.Properties.AlgebraBridge
  ( module Futhark.Analysis.Properties.AlgebraBridge.Translate,
    module Futhark.Analysis.Properties.AlgebraBridge.Util,
    simplify,
    isTrue,
    isFalse,
    printAlgM,
  )
where

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import Futhark.Analysis.Properties.AlgebraBridge.Translate
import Futhark.Analysis.Properties.AlgebraBridge.Util
import Futhark.Analysis.Properties.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Rule (Rule (..), applyRuleBook, vacuous)
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.Traversals (ASTMappable (..), ASTMapper (..))
import Futhark.Analysis.Properties.Unify (Substitution, sub, unify)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, int2SoP, justConstant, sym2SoP, (.+.), (.-.))

-- | Simplify symbols using algebraic solver.
simplify :: (ASTMappable Symbol a) => a -> IndexFnM a
simplify = astMap m
  where
    m :: ASTMapper Symbol IndexFnM =
      ASTMapper
        { mapOnSymbol = simplifySymbol . toDNF,
          mapOnSoP = simplifyAlgebra <=< applyRuleBook rulesSoP
        }

    rulesSoP :: IndexFnM [Rule (SoP Symbol) Symbol IndexFnM]
    rulesSoP = do
      h1 <- newVName "h"
      h2 <- newVName "h"
      h3 <- newVName "h"
      pure
        [ Rule
            { name = "Sum True",
              from = sym2SoP $ Sum h1 (hole h2) (hole h3) (Bool True),
              to = \s -> do
                a <- sub s (hole h2)
                b <- sub s (hole h3)
                nicely_non_empty <- a $<= b .+. int2SoP 1
                case nicely_non_empty of
                  Yes -> pure $ b .-. a .+. int2SoP 1
                  Unknown -> do
                    empty <- a $> b
                    case empty of
                      Yes -> pure $ int2SoP 0
                      Unknown -> sub s $ Sum h1 (hole h2) (hole h3) (Bool True),
              sideCondition = vacuous
            }
        ]
      where
        hole = sym2SoP . Hole

    simplifyAlgebra :: SoP Symbol -> IndexFnM (SoP Symbol)
    simplifyAlgebra x = rollbackAlgEnv $ do
      y <- toAlgebra x
      z <- Algebra.simplify y
      fromAlgebra z

    simplifySymbol :: Symbol -> IndexFnM Symbol
    simplifySymbol symbol = case symbol of
      _ :== _ -> refine symbol
      _ :/= _ -> refine symbol
      _ :> _ -> refine symbol
      _ :>= _ -> refine symbol
      _ :< _ -> refine symbol
      _ :<= _ -> refine symbol
      (p :&& q) -> do
        case (p, q) of
          (Bool True, _) -> pure q -- Identity.
          (_, Bool True) -> pure p
          (Bool False, _) -> pure $ Bool False -- Annihilation.
          (_, Bool False) -> pure $ Bool False
          (_, _) | p == q -> pure p -- Idempotence.
          (_, _) | p == neg q -> pure $ Bool False -- A contradiction.
          (_, _) -> do
            -- TODO should we treat all ps at once or is this enough?
            --      let ps = cnfToList symbol
            --      ... check all p,q in ps.
            s :: Maybe (Substitution Symbol) <- unify p q
            let p_equiv_q = isJust s
            -- Check if p => q or q => p. Simplify accordingly.
            let p_implies_q = rollbackAlgEnv $ do
                  assume p
                  isTrue q
            let q_implies_p = rollbackAlgEnv $ do
                  assume q
                  isTrue p
            if p_equiv_q
              then pure p
              else do
                p_implies_q' <- p_implies_q
                case p_implies_q' of
                  Yes -> pure p
                  Unknown -> do
                    q_implies_p' <- q_implies_p
                    case q_implies_p' of
                      Yes -> pure q
                      Unknown -> pure (p :&& q)
      (p :|| q) -> do
        pure $ case (p, q) of
          (Bool False, _) -> q -- Identity.
          (_, Bool False) -> p
          (Bool True, _) -> Bool True -- Annihilation.
          (_, Bool True) -> Bool True
          (_, _) | p == q -> p -- Idempotence.
          (_, _) | p == neg q -> Bool True -- A tautology.
          -- Check for factoring opportunity, e.g., (a ^ b) v (a ^ !b).
          (a :&& b, c :&& d)
            | a == c && b == neg d -> a
            | a == d && b == neg c -> a
            | b == c && a == neg d -> b
            | b == d && a == neg c -> b
          (_, _) -> p :|| q
      x -> pure x

    refine relation = do
      b <- solve relation
      case b of
        Yes -> pure $ Bool True
        Unknown -> pure relation

    -- Use Fourier-Motzkin elimination to determine the truth value
    -- of an expresion, if it can be determined in the given environment.
    -- If the truth value cannot be determined, False is also returned.
    solve expr = case expr of
      Bool True -> pure Yes
      a :== b -> solveBinOp (==) ($==) a b
      a :/= b -> solveBinOp (/=) ($/=) a b
      a :> b -> solveBinOp (>) ($>) a b
      a :>= b -> solveBinOp (>=) ($>=) a b
      a :< b -> solveBinOp (<) ($<) a b
      a :<= b -> solveBinOp (<=) ($<=) a b
      _ -> pure Unknown

    solveBinOp op solver a b
      | Just c <- justConstant a,
        Just d <- justConstant b =
          pure (answerFromBool $ op c d)
      | otherwise = solver a b

-- | Does this symbol simplify to true?
isTrue :: Symbol -> IndexFnM Answer
isTrue (Bool True) = pure Yes
isTrue sym = do
  p <- simplify sym
  case p of
    Bool True -> pure Yes
    _ -> pure Unknown

-- | Does this symbol simplify to false?
isFalse :: Symbol -> IndexFnM Answer
isFalse (Bool False) = pure Yes
isFalse p = do
  -- If we convert p to CNF, a sufficient condition for p to be false
  -- is that some clause q in p is false. Hence we can pick a clause q,
  -- assume all other clauses to be true, and use that information when
  -- checking q. This lets us easily falsify, for example, x == 1 :&& x == 2.
  falsify (conjToList $ toCNF p) []
  where
    falsify [] _ = pure Unknown
    falsify (q : left) right = do
      ans <- rollbackAlgEnv $ do
        mapM_ assume (left <> right)
        isTrue (neg q)
      case ans of
        Yes -> pure Yes
        Unknown -> falsify left (q : right)

printAlgM :: Int -> SoP Symbol -> IndexFnM ()
printAlgM level x = rollbackAlgEnv $ do
  alg_x :: SoP Algebra.Symbol <- toAlgebra x
  printM level (prettyStr alg_x)

-- env <- getAlgEnv
-- printM level $ "under alg env " <> prettyStr env
