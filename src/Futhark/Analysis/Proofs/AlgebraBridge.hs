module Futhark.Analysis.Proofs.AlgebraBridge
  ( module Futhark.Analysis.Proofs.AlgebraBridge.Translate,
    module Futhark.Analysis.Proofs.AlgebraBridge.Util,
    simplify,
    isTrue,
    isFalse,
    algDebugPrettyM,
    printAlgM,
  )
where

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.AlgebraBridge.Translate
import Futhark.Analysis.Proofs.AlgebraBridge.Util
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.Monad (IndexFnM, getAlgEnv, printM, rollbackAlgEnv, whenDebug)
import Futhark.Analysis.Proofs.Rule (Rule (..), applyRuleBook, vacuous)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, toCNF, toDNF)
import Futhark.Analysis.Proofs.Traversals (ASTMappable (..), ASTMapper (..))
import Futhark.Analysis.Proofs.Unify (Substitution, sub, unify)
import Futhark.MonadFreshNames (newVName)
import Futhark.SoP.SoP (SoP, int2SoP, sym2SoP, (.+.), (.-.))
import Futhark.Util.Pretty (docStringW, pretty, prettyString)

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
            { name = "⟦¬x⟧ => 1 - ⟦x⟧",
              from = sym2SoP $ Not (Hole h1),
              to = \s -> sub s $ int2SoP 1 .-. sym2SoP (Hole h1),
              sideCondition = vacuous
            },
          Rule
            { name = "Sum True",
              from = sym2SoP $ Sum h1 (hole h2) (hole h3) (Bool True),
              to = \s -> do
                a <- sub s (hole h2)
                b <- sub s (hole h3)
                non_empty <- a $<= b
                case non_empty of
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
    solve (Bool True) = pure Yes
    solve (a :== b) = a $== b
    solve (a :/= b) = a $/= b
    solve (a :> b) = a $> b
    solve (a :>= b) = a $>= b
    solve (a :< b) = a $< b
    solve (a :<= b) = a $<= b
    solve _ = pure Unknown

-- | Does this symbol simplify to true?
isTrue :: Symbol -> IndexFnM Answer
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

    conjToList (a :&& b) = conjToList a <> conjToList b
    conjToList x = [x]

algDebugPrettyM :: String -> SoP Symbol -> IndexFnM ()
algDebugPrettyM msg x = rollbackAlgEnv $ do
  alg_x <- toAlgebra x
  whenDebug $ traceM $ docStringW 110 $ "  " <> pretty msg <> " " <> pretty alg_x

printAlgM :: SoP Symbol -> IndexFnM ()
printAlgM x = rollbackAlgEnv $ do
  alg_x <- toAlgebra x
  printM 3000 (prettyString alg_x)
  env <- getAlgEnv
  printM 3000 $ "under alg env " <> prettyString env
