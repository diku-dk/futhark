-- Rewrite using range environment and Fourier-Motzkin elimination.
module Futhark.Analysis.Proofs.Refine where

import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))

refineSymbol :: Symbol -> IndexFnM Symbol
refineSymbol symbol = case symbol of
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
      b <- solve (x `rel` y)
      pure $ if b then Bool True else x `rel` y

    -- Use Fourier-Motzkin elimination to determine the truth value
    -- of an expresion, if it can be determined in the given environment.
    -- If the truth value cannot be determined, False is also returned.
    solve (Bool True) = pure True
    solve (a :== b) = a $==$ b
    solve (a :/= b) = a $/=$ b
    solve (a :> b) = a $>$ b
    solve (a :>= b) = a $>=$ b
    solve (a :< b) = a $<$ b
    solve (a :<= b) = a $<=$ b
    solve _ = pure False
