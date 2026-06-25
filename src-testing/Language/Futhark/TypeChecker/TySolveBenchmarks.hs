module Language.Futhark.TypeChecker.TySolveBenchmarks (benchmarks) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Map qualified as M
import Generated.AllFutBenchmarks
import Language.Futhark (qualName)
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
import Language.Futhark.TypeChecker.TySolve as N (Solution, UnconTyVar, solve)
import Language.Futhark.TypeChecker.TySolveOld as O (solve)

(~) :: TypeBase () NoUniqueness -> TypeBase () NoUniqueness -> CtTy ()
t1 ~ t2 = CtEq (Reason mempty) t1 t2

tv :: VName -> Level -> (VName, (Level, TyVarInfo ()))
tv v lvl = (v, (lvl, TyVarFree mempty Unlifted))

solveNew ::
  ( [CtTy ()],
    TyParams,
    TyVars ()
  ) ->
  Either TypeError ([UnconTyVar], Solution)
solveNew (constraints, typarams, tyvars) = N.solve constraints typarams tyvars

solveOld ::
  ( [CtTy ()],
    TyParams,
    TyVars ()
  ) ->
  Either TypeError ([UnconTyVar], Solution)
solveOld (constraints, typarams, tyvars) = O.solve constraints typarams tyvars

generateContraints :: Int -> ([CtTy ()], TyParams, TyVars ())
generateContraints num_vars
  | num_vars <= 0 =
      ([], mempty, mempty)
  | num_vars == 1 =
      let v0_name = VName (nameFromString "v_0") 0
          ty_vars = M.fromList [tv v0_name 0]
       in ([], mempty, ty_vars)
  | otherwise =
      let var_names =
            [ VName (nameFromString ("v_" ++ show i)) i
              | i <- [0 .. num_vars - 1]
            ]

          ty_vars = M.fromList $ map (`tv` 0) var_names

          mkTy :: VName -> TypeBase () NoUniqueness
          mkTy v = Scalar (TypeVar NoUniqueness (qualName v) [])

          cts =
            zipWith
              (\v_i v_j -> mkTy v_i ~ mkTy v_j)
              (init var_names)
              (tail var_names)
              ++ ["v_0" ~ "i32"]

          ty_params = mempty
       in (cts, ty_params, ty_vars)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "TySolve"
    [ bgroup "Synthetic" $
        concatMap
          ( \n ->
              [ bench (show n ++ " variables (new)") $
                  whnf solveNew (generateContraints n),
                bench (show n ++ " variables (old)") $
                  whnf solveOld (generateContraints n)
              ]
          )
          sizes,
      bgroup "Converted" $
        concatMap
          ( \(name, dataCase) ->
              [ bench (name <> " (new)") $ whnf solveNew dataCase,
                bench (name <> " (old)") $ whnf solveOld dataCase
              ]
          )
          allFutBenchmarkCases
    ]
  where
    start = 100
    end = 1000
    i = 100
    sizes = [start, start + i .. end]
