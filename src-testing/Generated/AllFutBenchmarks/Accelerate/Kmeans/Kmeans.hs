module Generated.AllFutBenchmarks.Accelerate.Kmeans.Kmeans (benchmarkDataList) where

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
  [ ( [ "t_8324_8325" ~ "[]t_8324_8325_8327_8324",
        "t_8322_8325" ~ "[]t_8322_8325_8327_8325",
        "i32 -> i64" ~ "a_8323 -> x_8324",
        "[]i32" ~ "[]a_8323",
        "i64" ~ "i64",
        "num_8321_8320" ~ "t_8329",
        "t_8321 -> t_8321 -> t_8321" ~ "a_8320 -> a_8320 -> a_8320",
        "num_8322" ~ "a_8320",
        "i64" ~ "i64",
        "[]x_8324" ~ "[]i64",
        "[]t_8329" ~ "[]a_8320",
        "t_8322_8325" ~ "[]a_8320",
        "i32 -> i64" ~ "a_8322_8329 -> x_8323_8320",
        "[]i32" ~ "[]a_8322_8329",
        "(f32, f32) -> (f32, f32) -> (f32, f32)" ~ "a_8322_8326 -> a_8322_8326 -> a_8322_8326",
        "(num_8322_8327, num_8322_8328)" ~ "a_8322_8326",
        "i64" ~ "i64",
        "[]x_8323_8320" ~ "[]i64",
        "[](f32, f32)" ~ "[]a_8322_8326",
        "t_8324_8325" ~ "[]a_8322_8326",
        "num_8325_8322" ~ "t_8325_8321",
        "t_8325_8323" ~ "t_8325_8321",
        "{x: i32} -> f32" ~ "a_8325_8328 -> x_8325_8329",
        "t_8322_8325" ~ "[]a_8325_8328",
        "t_8325_8323 -> t_8325_8321" ~ "a_8324_8329 -> x_8325_8320",
        "[]x_8325_8329" ~ "[]a_8324_8329",
        "(f32, f32) -> {s: f32} -> (f32, f32)" ~ "a_8324_8326 -> b_8324_8327 -> x_8324_8328",
        "t_8324_8325" ~ "[]a_8324_8326",
        "[]x_8325_8320" ~ "[]b_8324_8327",
        "[](f32, f32)" ~ "[]x_8324_8328"
      ],
      M.empty,
      M.fromList [("a_8320", (4, TyVarFree NoLoc Unlifted)), ("t_8321", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8322", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("a_8323", (4, TyVarFree NoLoc Unlifted)), ("x_8324", (4, TyVarFree NoLoc Unlifted)), ("t_8329", (4, TyVarFree NoLoc Unlifted)), ("num_8321_8320", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8325", (5, TyVarFree NoLoc Lifted)), ("a_8322_8326", (6, TyVarFree NoLoc Unlifted)), ("num_8322_8327", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8322_8328", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("a_8322_8329", (6, TyVarFree NoLoc Unlifted)), ("x_8323_8320", (6, TyVarFree NoLoc Unlifted)), ("t_8324_8325", (7, TyVarFree NoLoc Lifted)), ("a_8324_8326", (8, TyVarFree NoLoc Unlifted)), ("b_8324_8327", (8, TyVarFree NoLoc Unlifted)), ("x_8324_8328", (8, TyVarFree NoLoc Unlifted)), ("a_8324_8329", (8, TyVarFree NoLoc Unlifted)), ("x_8325_8320", (8, TyVarFree NoLoc Unlifted)), ("t_8325_8321", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8325_8322", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8325_8323", (8, TyVarFree NoLoc Lifted)), ("a_8325_8328", (8, TyVarFree NoLoc Unlifted)), ("x_8325_8329", (8, TyVarFree NoLoc Unlifted)), ("t_8324_8325_8327_8324", (7, TyVarFree NoLoc Lifted)), ("t_8322_8325_8327_8325", (5, TyVarFree NoLoc Lifted))]
    ),
    ( [ "x_8326_8323" ~ "[]x_8326_8323_8327_8323",
        "t_8326_8320" ~ "[]t_8326_8320_8327_8324",
        "t_8324_8324" ~ "[]t_8324_8324_8327_8325",
        "t_8324_8325" ~ "[]t_8324_8325_8327_8326",
        "t_8325_8329" ~ "[]t_8325_8329_8327_8327",
        "t_8322_8325" ~ "[]t_8322_8325_8327_8328",
        "t_8323_8324" ~ "[]t_8323_8324_8327_8329",
        "t_8321_8329" ~ "[]t_8321_8329_8328_8320",
        "a_8324" ~ "[]a_8324_8328_8321",
        "t_8326" ~ "[]t_8326_8328_8322",
        "num_8320" ~ "i64",
        "i32" ~ "i32",
        "t_8323" ~ "i64",
        "t_8328" ~ "num_8327",
        "index_8329" ~ "index_elem_8321_8320",
        "t_8326" ~ "[]index_elem_8321_8320",
        "t_8321_8322" ~ "num_8321_8321",
        "index_8321_8323" ~ "index_elem_8321_8324",
        "t_8326" ~ "[]index_elem_8321_8324",
        "{point: t_8326} -> (index_8329, index_8321_8323)" ~ "a_8324 -> x_8325",
        "[][]f32" ~ "[]a_8324",
        "t_8321_8329" ~ "[]x_8325",
        "t_8323" ~ "i64",
        "t_8321_8329" ~ "[]t_8322_8320",
        "t_8322_8325" ~ "[]t_8322_8320",
        "t_8323" ~ "i64",
        "[](f32, f32)" ~ "[](f32, f32)",
        "{pt: (f32, f32)} -> i32" ~ "a_8322_8326 -> x_8322_8327",
        "t_8321_8329" ~ "[]a_8322_8326",
        "t_8323_8324" ~ "[]x_8322_8327",
        "t_8323" ~ "i64",
        "t_8321_8329" ~ "[](f32, f32)",
        "t_8323_8324" ~ "[]i32",
        "t_8322_8325" ~ "[](f32, f32)",
        "(t_8324_8324, t_8324_8325, t_8324_8326)" ~ "(t_8322_8325, [](f32, f32), num_8324_8323)",
        "t_8324_8324" ~ "[](f32, f32)",
        "t_8324_8325" ~ "[](f32, f32)",
        "t_8324_8325" ~ "[](f32, f32)",
        "t_8324_8326" ~ "t_8325_8323",
        "num_8325_8324" ~ "t_8325_8323",
        "(t_8322_8325, [](f32, f32), num_8324_8323)" ~ "(t_8324_8325, [](f32, f32), t_8325_8323)",
        "(t_8325_8329, t_8326_8320, t_8326_8321)" ~ "(t_8324_8324, t_8324_8325, t_8324_8326)",
        "t_8326_8324" ~ "et_8326_8326",
        "t_8326_8325" ~ "et_8326_8326",
        "(t_8326_8324, t_8326_8325) -> []et_8326_8326" ~ "a_8326_8322 -> x_8326_8323",
        "t_8326_8320" ~ "[]a_8326_8322",
        "num_8327_8322" ~ "i64",
        "([][]f32, i32)" ~ "([]x_8326_8323, t_8326_8321)"
      ],
      M.empty,
      M.fromList [("num_8320", (2, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8323", (4, TyVarFree NoLoc Lifted)), ("a_8324", (5, TyVarFree NoLoc Unlifted)), ("x_8325", (5, TyVarFree NoLoc Unlifted)), ("t_8326", (6, TyVarFree NoLoc Lifted)), ("num_8327", (7, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8328", (7, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8329", (7, TyVarFree NoLoc Unlifted)), ("index_elem_8321_8320", (7, TyVarFree NoLoc Unlifted)), ("num_8321_8321", (7, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8321_8322", (7, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8321_8323", (7, TyVarFree NoLoc Unlifted)), ("index_elem_8321_8324", (7, TyVarFree NoLoc Unlifted)), ("t_8321_8329", (6, TyVarFree NoLoc Lifted)), ("t_8322_8320", (7, TyVarFree NoLoc Unlifted)), ("t_8322_8325", (8, TyVarFree NoLoc Lifted)), ("a_8322_8326", (11, TyVarFree NoLoc Unlifted)), ("x_8322_8327", (11, TyVarFree NoLoc Unlifted)), ("t_8323_8324", (12, TyVarFree NoLoc Lifted)), ("num_8324_8323", (9, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8324_8324", (9, TyVarFree NoLoc Lifted)), ("t_8324_8325", (9, TyVarFree NoLoc Lifted)), ("t_8324_8326", (9, TyVarFree NoLoc Lifted)), ("t_8325_8323", (9, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8325_8324", (9, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8325_8329", (10, TyVarFree NoLoc Lifted)), ("t_8326_8320", (10, TyVarFree NoLoc Lifted)), ("t_8326_8321", (10, TyVarFree NoLoc Lifted)), ("a_8326_8322", (11, TyVarFree NoLoc Unlifted)), ("x_8326_8323", (11, TyVarFree NoLoc Unlifted)), ("t_8326_8324", (12, TyVarFree NoLoc Lifted)), ("t_8326_8325", (12, TyVarFree NoLoc Lifted)), ("et_8326_8326", (13, TyVarFree NoLoc Unlifted)), ("num_8327_8322", (3, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("x_8326_8323_8327_8323", (11, TyVarFree NoLoc Unlifted)), ("t_8326_8320_8327_8324", (10, TyVarFree NoLoc Lifted)), ("t_8324_8324_8327_8325", (9, TyVarFree NoLoc Lifted)), ("t_8324_8325_8327_8326", (9, TyVarFree NoLoc Lifted)), ("t_8325_8329_8327_8327", (10, TyVarFree NoLoc Lifted)), ("t_8322_8325_8327_8328", (8, TyVarFree NoLoc Lifted)), ("t_8323_8324_8327_8329", (12, TyVarFree NoLoc Lifted)), ("t_8321_8329_8328_8320", (6, TyVarFree NoLoc Lifted)), ("a_8324_8328_8321", (5, TyVarFree NoLoc Unlifted)), ("t_8326_8328_8322", (6, TyVarFree NoLoc Lifted))]
    )
  ]
