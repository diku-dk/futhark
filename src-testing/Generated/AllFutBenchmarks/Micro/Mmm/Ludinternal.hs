module Generated.AllFutBenchmarks.Micro.Mmm.Ludinternal (benchmarkDataList) where

import Data.Map qualified as M
import Futhark.Util.Loc (Loc (NoLoc))
import Language.Futhark.Syntax
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker.Constraints
  ( CtTy (..),
    Reason (..),
    TyParams,
    TyVarInfo (..),
    TyVars,
  )

(~) :: TypeBase () NoUniqueness -> TypeBase () NoUniqueness -> CtTy ()
t1 ~ t2 = CtEq (Reason mempty) t1 t2

type BenchmarkCaseData = ([CtTy ()], TyParams, TyVars ())

benchmarkDataList :: [BenchmarkCaseData]
benchmarkDataList =
  [ ( [ "x_8321" ~ "[][][]x_8321_8326_8329",
        "a_8325_8329" ~ "[][][]a_8325_8329_8327_8320",
        "b_8326_8320" ~ "[][]b_8326_8320_8327_8321",
        "x_8323" ~ "[][]x_8323_8327_8322",
        "a_8324_8329" ~ "[][]a_8324_8329_8327_8323",
        "b_8325_8320" ~ "[][]b_8325_8320_8327_8324",
        "x_8325" ~ "[]x_8325_8327_8325",
        "a_8323_8329" ~ "[]a_8323_8329_8327_8326",
        "b_8324_8320" ~ "[]b_8324_8320_8327_8327",
        "b_8327" ~ "[]b_8327_8327_8328",
        "t_8321_8320" ~ "[]t_8321_8320_8327_8329",
        "t_8322_8321" ~ "[]t_8322_8321_8328_8320",
        "t_8321_8324 -> t_8321_8324 -> t_8321_8324" ~ "a_8321_8321 -> b_8321_8322 -> x_8321_8323",
        "[]f32" ~ "[]a_8321_8321",
        "t_8321_8320" ~ "[]b_8321_8322",
        "t_8322_8321" ~ "[]x_8321_8323",
        "t_8322_8321" ~ "[]f32",
        "t_8322_8324" ~ "f32",
        "t_8329" ~ "t_8322_8325",
        "t_8322_8324" ~ "t_8322_8325",
        "[][]f32" ~ "[][]t_8323_8320",
        "{mat_el: t_8329} -> {top_row: t_8321_8320} -> t_8322_8325" ~ "a_8326 -> b_8327 -> x_8328",
        "[]f32" ~ "[]a_8326",
        "[][]t_8323_8320" ~ "[]b_8327",
        "[]f32" ~ "[]x_8328",
        "[][]f32" ~ "[]a_8323_8329",
        "[][]f32" ~ "[]b_8324_8320",
        "([]f32, []f32) -> []f32" ~ "a_8324 -> x_8325",
        "[](a_8323_8329, b_8324_8320)" ~ "[]a_8324",
        "[][]f32" ~ "[]x_8325",
        "[][][]f32" ~ "[]a_8324_8329",
        "[][][]f32" ~ "[]b_8325_8320",
        "([][]f32, [][]f32) -> [][]f32" ~ "a_8322 -> x_8323",
        "[](a_8324_8329, b_8325_8320)" ~ "[]a_8322",
        "[][][]f32" ~ "[]x_8323",
        "[][][][]f32" ~ "[]a_8325_8329",
        "[][][]f32" ~ "[]b_8326_8320",
        "([][][]f32, [][]f32) -> [][][]f32" ~ "a_8320 -> x_8321",
        "[](a_8325_8329, b_8326_8320)" ~ "[]a_8320",
        "[][][][]f32" ~ "[]x_8321"
      ],
      M.empty,
      M.fromList [("a_8320", (4, TyVarFree NoLoc Unlifted)), ("x_8321", (4, TyVarFree NoLoc Unlifted)), ("a_8322", (6, TyVarFree NoLoc Unlifted)), ("x_8323", (6, TyVarFree NoLoc Unlifted)), ("a_8324", (8, TyVarFree NoLoc Unlifted)), ("x_8325", (8, TyVarFree NoLoc Unlifted)), ("a_8326", (10, TyVarFree NoLoc Unlifted)), ("b_8327", (10, TyVarFree NoLoc Unlifted)), ("x_8328", (10, TyVarFree NoLoc Unlifted)), ("t_8329", (11, TyVarFree NoLoc Lifted)), ("t_8321_8320", (12, TyVarFree NoLoc Lifted)), ("a_8321_8321", (13, TyVarFree NoLoc Unlifted)), ("b_8321_8322", (13, TyVarFree NoLoc Unlifted)), ("x_8321_8323", (13, TyVarFree NoLoc Unlifted)), ("t_8321_8324", (13, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8321", (14, TyVarFree NoLoc Lifted)), ("t_8322_8324", (16, TyVarFree NoLoc Lifted)), ("t_8322_8325", (17, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8323_8320", (10, TyVarFree NoLoc Unlifted)), ("a_8323_8329", (8, TyVarFree NoLoc Unlifted)), ("b_8324_8320", (8, TyVarFree NoLoc Unlifted)), ("a_8324_8329", (6, TyVarFree NoLoc Unlifted)), ("b_8325_8320", (6, TyVarFree NoLoc Unlifted)), ("a_8325_8329", (4, TyVarFree NoLoc Unlifted)), ("b_8326_8320", (4, TyVarFree NoLoc Unlifted)), ("x_8321_8326_8329", (4, TyVarFree NoLoc Unlifted)), ("a_8325_8329_8327_8320", (4, TyVarFree NoLoc Unlifted)), ("b_8326_8320_8327_8321", (4, TyVarFree NoLoc Unlifted)), ("x_8323_8327_8322", (6, TyVarFree NoLoc Unlifted)), ("a_8324_8329_8327_8323", (6, TyVarFree NoLoc Unlifted)), ("b_8325_8320_8327_8324", (6, TyVarFree NoLoc Unlifted)), ("x_8325_8327_8325", (8, TyVarFree NoLoc Unlifted)), ("a_8323_8329_8327_8326", (8, TyVarFree NoLoc Unlifted)), ("b_8324_8320_8327_8327", (8, TyVarFree NoLoc Unlifted)), ("b_8327_8327_8328", (10, TyVarFree NoLoc Unlifted)), ("t_8321_8320_8327_8329", (12, TyVarFree NoLoc Lifted)), ("t_8322_8321_8328_8320", (14, TyVarFree NoLoc Lifted))]
    )
  ]
