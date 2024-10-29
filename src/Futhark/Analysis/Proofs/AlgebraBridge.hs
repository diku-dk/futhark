module Futhark.Analysis.Proofs.AlgebraBridge
  ( module Futhark.Analysis.Proofs.AlgebraBridge.Translate,
    module Futhark.Analysis.Proofs.AlgebraBridge.Util,
    simplify,
  )
where

import Control.Monad ((<=<))
import Futhark.Analysis.Proofs.AlgebraBridge.Translate
import Futhark.Analysis.Proofs.AlgebraBridge.Util
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.Monad (IndexFnM)
import Futhark.Analysis.Proofs.Rule (applyRuleBook, rulesSoP)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), normalizeSymbol)
import Futhark.Analysis.Proofs.Traversals (ASTMappable (..), ASTMapper (..))
import Futhark.Analysis.Proofs.Util (converge)
import Futhark.SoP.SoP (SoP)

-- | Simplify symbols using algebraic solver.
simplify :: (ASTMappable Symbol a) => a -> IndexFnM a
simplify = astMap m
  where
    m :: ASTMapper Symbol IndexFnM =
      ASTMapper
        { mapOnSymbol = converge (fmap normalizeSymbol . simplifySymbol),
          mapOnSoP = simplifyAlgebra <=< applyRuleBook rulesSoP
        }

    simplifyAlgebra :: SoP Symbol -> IndexFnM (SoP Symbol)
    simplifyAlgebra x = rollbackAlgEnv $ do
      y <- toAlgebra x
      z <- Algebra.simplify y
      -- debugPrettyM "simplify" x
      -- debugPrettyM "========" y
      -- debugPrettyM "resultin" z
      -- debugPrintAlgEnv
      -- debugLn
      fromAlgebra z

    simplifySymbol :: Symbol -> IndexFnM Symbol
    simplifySymbol symbol = case symbol of
      _ :== _ -> refine symbol
      _ :/= _ -> refine symbol
      _ :> _ -> refine symbol
      _ :>= _ -> refine symbol
      _ :< _ -> refine symbol
      _ :<= _ -> refine symbol
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
