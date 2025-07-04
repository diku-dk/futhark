module Generated.AllFutBenchmarks.Misc.Knnbykdtree.Util (benchmarkDataList) where

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
  [ ( [ "t_8329_8323" ~ "[]t_8329_8323_8329_8324",
        "t_8326_8324" ~ "[]t_8326_8324_8329_8325",
        "t_8322_8320" ~ "[]t_8322_8320_8329_8326",
        "t_8324_8321" ~ "[]t_8324_8321_8329_8327",
        "t_8323_8321" ~ "[]t_8323_8321_8329_8328",
        "t_8321_8320" ~ "[]t_8321_8320_8329_8329",
        "t_8322" ~ "bool",
        "num_8323" ~ "if_t_8325",
        "num_8324" ~ "if_t_8325",
        "{f: t_8322} -> if_t_8325" ~ "a_8320 -> x_8321",
        "[]bool" ~ "[]a_8320",
        "t_8321_8320" ~ "[]x_8321",
        "t_8321_8322 -> t_8321_8322 -> t_8321_8322" ~ "a_8321_8321 -> a_8321_8321 -> a_8321_8321",
        "num_8321_8323" ~ "a_8321_8321",
        "t_8321_8320" ~ "[]a_8321_8321",
        "t_8322_8320" ~ "[]a_8321_8321",
        "t_8322_8323" ~ "bool",
        "num_8322_8324" ~ "if_t_8322_8326",
        "num_8322_8325" ~ "if_t_8322_8326",
        "{f: t_8322_8323} -> if_t_8322_8326" ~ "a_8322_8321 -> x_8322_8322",
        "[]bool" ~ "[]a_8322_8321",
        "t_8323_8321" ~ "[]x_8322_8322",
        "t_8323_8323 -> t_8323_8323 -> t_8323_8323" ~ "a_8323_8322 -> a_8323_8322 -> a_8323_8322",
        "num_8323_8324" ~ "a_8323_8322",
        "t_8323_8321" ~ "[]a_8323_8322",
        "t_8324_8321" ~ "[]a_8323_8322",
        "i64" ~ "t_8324_8322",
        "num_8324_8323" ~ "t_8324_8322",
        "t_8324_8328" ~ "t_8324_8322",
        "index_8324_8329" ~ "index_elem_8325_8320",
        "t_8322_8320" ~ "[]index_elem_8325_8320",
        "t_8325_8321" ~ "index_8324_8329",
        "t_8325_8325" ~ "t_8325_8324",
        "t_8325_8321" ~ "t_8325_8324",
        "t_8325_8325 -> t_8325_8324" ~ "a_8325_8322 -> x_8325_8323",
        "t_8324_8321" ~ "[]a_8325_8322",
        "t_8326_8324" ~ "[]x_8325_8323",
        "t_8327_8320" ~ "t_8327_8322",
        "num_8327_8323" ~ "t_8327_8322",
        "t_8327_8321" ~ "t_8327_8328",
        "num_8327_8329" ~ "t_8327_8328",
        "t_8326_8329" ~ "bool",
        "t_8327_8322" ~ "if_t_8328_8324",
        "t_8327_8328" ~ "if_t_8328_8324",
        "{c: t_8326_8329} -> (iT: t_8327_8320) -> (iF: t_8327_8321) -> if_t_8328_8324" ~ "a_8326_8325 -> b_8326_8326 -> c_8326_8327 -> x_8326_8328",
        "[]bool" ~ "[]a_8326_8325",
        "t_8322_8320" ~ "[]b_8326_8326",
        "t_8326_8324" ~ "[]c_8326_8327",
        "t_8329_8323" ~ "[]x_8326_8328",
        "([]i32, i32)" ~ "(t_8329_8323, t_8325_8321)"
      ],
      M.empty,
      M.fromList [("a_8320", (2, TyVarFree NoLoc Unlifted)), ("x_8321", (2, TyVarFree NoLoc Unlifted)), ("t_8322", (3, TyVarFree NoLoc Lifted)), ("num_8323", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8324", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("if_t_8325", (4, TyVarFree NoLoc SizeLifted)), ("t_8321_8320", (3, TyVarFree NoLoc Lifted)), ("a_8321_8321", (4, TyVarFree NoLoc Unlifted)), ("t_8321_8322", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8321_8323", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8320", (5, TyVarFree NoLoc Lifted)), ("a_8322_8321", (6, TyVarFree NoLoc Unlifted)), ("x_8322_8322", (6, TyVarFree NoLoc Unlifted)), ("t_8322_8323", (7, TyVarFree NoLoc Lifted)), ("num_8322_8324", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8322_8325", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("if_t_8322_8326", (8, TyVarFree NoLoc SizeLifted)), ("t_8323_8321", (7, TyVarFree NoLoc Lifted)), ("a_8323_8322", (8, TyVarFree NoLoc Unlifted)), ("t_8323_8323", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8323_8324", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8324_8321", (9, TyVarFree NoLoc Lifted)), ("t_8324_8322", (10, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8324_8323", (10, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8324_8328", (10, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8324_8329", (10, TyVarFree NoLoc Unlifted)), ("index_elem_8325_8320", (10, TyVarFree NoLoc Unlifted)), ("t_8325_8321", (11, TyVarFree NoLoc Lifted)), ("a_8325_8322", (12, TyVarFree NoLoc Unlifted)), ("x_8325_8323", (12, TyVarFree NoLoc Unlifted)), ("t_8325_8324", (12, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8325_8325", (12, TyVarFree NoLoc Lifted)), ("t_8326_8324", (13, TyVarFree NoLoc Lifted)), ("a_8326_8325", (14, TyVarFree NoLoc Unlifted)), ("b_8326_8326", (14, TyVarFree NoLoc Unlifted)), ("c_8326_8327", (14, TyVarFree NoLoc Unlifted)), ("x_8326_8328", (14, TyVarFree NoLoc Unlifted)), ("t_8326_8329", (15, TyVarFree NoLoc Lifted)), ("t_8327_8320", (16, TyVarFree NoLoc Lifted)), ("t_8327_8321", (17, TyVarFree NoLoc Lifted)), ("t_8327_8322", (18, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8327_8323", (18, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8327_8328", (18, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8327_8329", (18, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("if_t_8328_8324", (18, TyVarFree NoLoc SizeLifted)), ("t_8329_8323", (15, TyVarFree NoLoc Lifted)), ("t_8329_8323_8329_8324", (15, TyVarFree NoLoc Lifted)), ("t_8326_8324_8329_8325", (13, TyVarFree NoLoc Lifted)), ("t_8322_8320_8329_8326", (5, TyVarFree NoLoc Lifted)), ("t_8324_8321_8329_8327", (9, TyVarFree NoLoc Lifted)), ("t_8323_8321_8329_8328", (7, TyVarFree NoLoc Lifted)), ("t_8321_8320_8329_8329", (3, TyVarFree NoLoc Lifted))]
    ),
    ( [ "t_8326_8325" ~ "[]t_8326_8325_8326_8329",
        "t_8325_8321" ~ "[]t_8325_8321_8327_8320",
        "t_8322" ~ "i64",
        "i64" ~ "i64",
        "i32" ~ "t_8323",
        "i32" ~ "t_8323",
        "t_8322" ~ "i64",
        "i64" ~ "i64",
        "i32" ~ "t_8321_8322",
        "i32" ~ "t_8321_8322",
        "(t_8322_8321, t_8322_8322)" ~ "(t_8323, t_8321_8322)",
        "t_8322_8325" ~ "t_8322_8321",
        "index_8322_8326" ~ "index_elem_8322_8327",
        "[]i32" ~ "[]index_elem_8322_8327",
        "i64" ~ "i64",
        "index_8322_8326" ~ "t_8322_8324",
        "i32" ~ "t_8322_8324",
        "t_8322_8324" ~ "t_8322_8323",
        "t_8322_8322" ~ "t_8322_8323",
        "t_8322_8323" ~ "i32",
        "i64" ~ "t_8324_8320",
        "i64" ~ "t_8324_8320",
        "t_8324_8320" ~ "i64",
        "{i: t_8322} -> i64" ~ "a_8320 -> x_8321",
        "[]i64" ~ "[]a_8320",
        "t_8325_8321" ~ "[]x_8321",
        "[][]t_1" ~ "[][]t_8325_8323",
        "[][]t_1" ~ "[][]t_8325_8326",
        "[]t_8325_8323" ~ "[]t_8325_8322",
        "t_8325_8321" ~ "[]i64",
        "[]t_8325_8326" ~ "[]t_8325_8322",
        "t_8326_8325" ~ "[]t_8325_8322",
        "t_8326_8325" ~ "[]t_8326_8326",
        "[][]t_1" ~ "[][]t_8326_8326"
      ],
      M.fromList [("t_1", (0, Unlifted, NoLoc))],
      M.fromList [("a_8320", (4, TyVarFree NoLoc Unlifted)), ("x_8321", (4, TyVarFree NoLoc Unlifted)), ("t_8322", (5, TyVarFree NoLoc Lifted)), ("t_8323", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8321_8322", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8321", (7, TyVarFree NoLoc Lifted)), ("t_8322_8322", (7, TyVarFree NoLoc Lifted)), ("t_8322_8323", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8324", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8325", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8322_8326", (8, TyVarFree NoLoc Unlifted)), ("index_elem_8322_8327", (8, TyVarFree NoLoc Unlifted)), ("t_8324_8320", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8325_8321", (5, TyVarFree NoLoc Lifted)), ("t_8325_8322", (6, TyVarFree NoLoc Unlifted)), ("t_8325_8323", (6, TyVarFree NoLoc Unlifted)), ("t_8325_8326", (6, TyVarFree NoLoc Unlifted)), ("t_8326_8325", (7, TyVarFree NoLoc Lifted)), ("t_8326_8326", (8, TyVarFree NoLoc Unlifted)), ("t_8326_8325_8326_8329", (7, TyVarFree NoLoc Lifted)), ("t_8325_8321_8327_8320", (5, TyVarFree NoLoc Lifted))]
    )
  ]
