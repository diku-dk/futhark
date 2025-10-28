module Generated.AllFutBenchmarks
  ( allFutBenchmarkCases,
    BenchmarkCaseData,
  )
where

import Generated.AllFutBenchmarks.Accelerate.Nbody.Nbodybh qualified as AllFutBenchmarksAccelerateNbodyNbodybh
import Language.Futhark.TypeChecker.Constraints (CtTy, TyParams, TyVars)

type BenchmarkCaseData = ([CtTy ()], TyParams, TyVars ())

allFutBenchmarkCases :: [(String, BenchmarkCaseData)]
allFutBenchmarkCases =
  [ ("accelerate/nbody/nbody-bh.fut (Block 1/10) (Cons: 121)", head AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList),
    ("accelerate/nbody/nbody-bh.fut (Block 2/10) (Cons: 146)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 1),
    ("accelerate/nbody/nbody-bh.fut (Block 3/10) (Cons: 133)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 2),
    ("accelerate/nbody/nbody-bh.fut (Block 4/10) (Cons: 45)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 3),
    ("accelerate/nbody/nbody-bh.fut (Block 5/10) (Cons: 210)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 4),
    ("accelerate/nbody/nbody-bh.fut (Block 6/10) (Cons: 401)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 5),
    ("accelerate/nbody/nbody-bh.fut (Block 7/10) (Cons: 39)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 6),
    ("accelerate/nbody/nbody-bh.fut (Block 8/10) (Cons: 173)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 7),
    ("accelerate/nbody/nbody-bh.fut (Block 9/10) (Cons: 164)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 8),
    ("accelerate/nbody/nbody-bh.fut (Block 10/10) (Cons: 40)", AllFutBenchmarksAccelerateNbodyNbodybh.benchmarkDataList !! 9)
  ]
