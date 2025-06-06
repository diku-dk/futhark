module Language.Futhark.TypeChecker.TySolveBenchmarks (benchmarks) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Map qualified as M
import Language.Futhark.Syntax
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker.Constraints
  ( CtTy (..),
    Level,
    Reason (..),
    TyParams,
    TyVarInfo (..),
    TyVars,
  )
import Language.Futhark.TypeChecker.Monad (TypeError (..))
import Language.Futhark.TypeChecker.TySolve (Solution, UnconTyVar, solve)

(~) :: TypeBase () NoUniqueness -> TypeBase () NoUniqueness -> CtTy ()
t1 ~ t2 = CtEq (Reason mempty) t1 t2

tv :: VName -> Level -> (VName, (Level, TyVarInfo ()))
tv v lvl = (v, (lvl, TyVarFree mempty Unlifted))

solve' ::
  ( [CtTy ()],
    TyParams,
    TyVars ()
  ) ->
  Either TypeError ([UnconTyVar], Solution)
solve' (constraints, typarams, tyvars) = solve constraints typarams tyvars

benchmarks :: Benchmark
benchmarks =
  bgroup
    "TySolve"
    [ bench "trivial" $
        whnf
          solve'
          ( ["a_0" ~ "b_1"],
            mempty,
            M.fromList [tv "a_0" 0]
          )
    ]
