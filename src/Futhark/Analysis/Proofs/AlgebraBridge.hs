module Futhark.Analysis.Proofs.AlgebraBridge
  ( module Futhark.Analysis.Proofs.AlgebraBridge.Translate,
    module Futhark.Analysis.Proofs.AlgebraBridge.Util,
    simplify,
  )
where

import Futhark.Analysis.Proofs.AlgebraBridge.Translate
import Futhark.Analysis.Proofs.AlgebraBridge.Util

import Control.Monad ((<=<))
import Futhark.Analysis.Proofs.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Proofs.Monad (IndexFnM, debugPrettyM, debugM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), normalizeSymbol)
import Futhark.Analysis.Proofs.Traversals (ASTMappable (..), ASTMapper (..))
import Futhark.Analysis.Proofs.Util (converge)
import Futhark.SoP.SoP (SoP)
import Futhark.Analysis.Proofs.Rule (rulesSoP, applyRuleBook)
import Futhark.SoP.Monad (getUntrans, MonadSoP (getRanges, getProperties))

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
      debugM "\n\n"
      debugPrettyM "simplify" x
      debugPrettyM "========" y
      debugPrettyM "resultin" z
      untrans <- getUntrans
      ranges <- getRanges
      properties <- getProperties
      debugPrettyM "untrans" untrans
      debugPrettyM "ranges" ranges
      debugPrettyM "props" properties
      fromAlgebra z

    simplifySymbol :: Symbol -> IndexFnM Symbol
    simplifySymbol symbol = case symbol of
      x :== y -> refineCmp (:==) x y
      x :/= y -> refineCmp (:/=) x y
      x :> y -> refineCmp (:>) x y
      x :>= y -> refineCmp (:>=) x y
      x :< y -> refineCmp (:<) x y
      x :<= y -> refineCmp (:<=) x y
      x -> pure x

    refineCmp rel x y = do
      b <- solve (x `rel` y)
      pure $ if b then Bool True else x `rel` y

    -- Use Fourier-Motzkin elimination to determine the truth value
    -- of an expresion, if it can be determined in the given environment.
    -- If the truth value cannot be determined, False is also returned.
    solve (Bool True) = pure True
    solve (a :== b) = a $== b
    solve (a :/= b) = a $/= b
    solve (a :> b) = a $> b
    solve (a :>= b) = a $>= b
    solve (a :< b) = a $< b
    solve (a :<= b) = a $<= b
    solve _ = pure False
