module Generated.AllFutBenchmarks.Micro.Mmm.Mmmbatch (benchmarkDataList) where

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
  [ ( [ "x_8322" ~ "[][][]x_8322_8326_8328",
        "b_8321" ~ "[][][]b_8321_8326_8329",
        "a_8320" ~ "[][][]a_8320_8327_8320",
        "t_8323" ~ "[][][]t_8323_8327_8321",
        "t_8324" ~ "[][][]t_8324_8327_8322",
        "x_8327" ~ "[][]x_8327_8327_8323",
        "b_8326" ~ "[][]b_8326_8327_8324",
        "a_8325" ~ "[][]a_8325_8327_8325",
        "t_8328" ~ "[][]t_8328_8327_8326",
        "t_8329" ~ "[][]t_8329_8327_8327",
        "x_8321_8321" ~ "[]x_8321_8321_8327_8328",
        "a_8321_8320" ~ "[]a_8321_8320_8327_8329",
        "t_8321_8322" ~ "[]t_8321_8322_8328_8320",
        "a_8321_8323" ~ "[]a_8321_8323_8328_8321",
        "t_8321_8325" ~ "[]t_8321_8325_8328_8322",
        "a_8321_8326" ~ "[]a_8321_8326_8328_8323",
        "t_8322_8321" ~ "t_8322_8323",
        "t_8322_8322" ~ "t_8322_8323",
        "{x: t_8322_8321} -> {y: t_8322_8322} -> t_8322_8323" ~ "a_8321_8328 -> b_8321_8329 -> x_8322_8320",
        "t_8321_8322" ~ "[]a_8321_8328",
        "t_8321_8325" ~ "[]b_8321_8329",
        "t_8323_8325 -> t_8323_8325 -> t_8323_8325" ~ "a_8323_8324 -> a_8323_8324 -> a_8323_8324",
        "f32" ~ "a_8323_8324",
        "[]x_8322_8320" ~ "a_8321_8326",
        "{as: []a_8323_8324} -> a_8323_8324" ~ "a_8321_8326 -> b_8321_8327",
        "t_8324_8324" ~ "b_8321_8327",
        "t_8329" ~ "[][]t_8324_8325",
        "(Bcol: t_8321_8325) -> t_8324_8324" ~ "a_8321_8323 -> x_8321_8324",
        "[][]t_8324_8325" ~ "[]a_8321_8323",
        "(Arow: t_8321_8322) -> []x_8321_8324" ~ "a_8321_8320 -> x_8321_8321",
        "t_8328" ~ "[]a_8321_8320",
        "(A: t_8328) -> (B: t_8329) -> []x_8321_8321" ~ "a_8325 -> b_8326 -> x_8327",
        "t_8323" ~ "[]a_8325",
        "t_8324" ~ "[]b_8326",
        "(A1: t_8323) -> (B1: t_8324) -> []x_8327" ~ "a_8320 -> b_8321 -> x_8322",
        "[][][][]f32" ~ "[]a_8320",
        "[][][][]f32" ~ "[]b_8321",
        "[][][][]f32" ~ "[]x_8322"
      ],
      M.empty,
      M.fromList [("a_8320", (3, TyVarFree NoLoc Unlifted)), ("b_8321", (3, TyVarFree NoLoc Unlifted)), ("x_8322", (3, TyVarFree NoLoc Unlifted)), ("t_8323", (4, TyVarFree NoLoc Lifted)), ("t_8324", (5, TyVarFree NoLoc Lifted)), ("a_8325", (6, TyVarFree NoLoc Unlifted)), ("b_8326", (6, TyVarFree NoLoc Unlifted)), ("x_8327", (6, TyVarFree NoLoc Unlifted)), ("t_8328", (7, TyVarFree NoLoc Lifted)), ("t_8329", (8, TyVarFree NoLoc Lifted)), ("a_8321_8320", (9, TyVarFree NoLoc Unlifted)), ("x_8321_8321", (9, TyVarFree NoLoc Unlifted)), ("t_8321_8322", (10, TyVarFree NoLoc Lifted)), ("a_8321_8323", (11, TyVarFree NoLoc Unlifted)), ("x_8321_8324", (11, TyVarFree NoLoc Unlifted)), ("t_8321_8325", (12, TyVarFree NoLoc Lifted)), ("a_8321_8326", (13, TyVarFree NoLoc Lifted)), ("b_8321_8327", (13, TyVarFree NoLoc Lifted)), ("a_8321_8328", (13, TyVarFree NoLoc Unlifted)), ("b_8321_8329", (13, TyVarFree NoLoc Unlifted)), ("x_8322_8320", (13, TyVarFree NoLoc Unlifted)), ("t_8322_8321", (14, TyVarFree NoLoc Lifted)), ("t_8322_8322", (15, TyVarFree NoLoc Lifted)), ("t_8322_8323", (16, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("a_8323_8324", (13, TyVarFree NoLoc Unlifted)), ("t_8323_8325", (13, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8324_8324", (14, TyVarFree NoLoc Lifted)), ("t_8324_8325", (11, TyVarFree NoLoc Unlifted)), ("x_8322_8326_8328", (3, TyVarFree NoLoc Unlifted)), ("b_8321_8326_8329", (3, TyVarFree NoLoc Unlifted)), ("a_8320_8327_8320", (3, TyVarFree NoLoc Unlifted)), ("t_8323_8327_8321", (4, TyVarFree NoLoc Lifted)), ("t_8324_8327_8322", (5, TyVarFree NoLoc Lifted)), ("x_8327_8327_8323", (6, TyVarFree NoLoc Unlifted)), ("b_8326_8327_8324", (6, TyVarFree NoLoc Unlifted)), ("a_8325_8327_8325", (6, TyVarFree NoLoc Unlifted)), ("t_8328_8327_8326", (7, TyVarFree NoLoc Lifted)), ("t_8329_8327_8327", (8, TyVarFree NoLoc Lifted)), ("x_8321_8321_8327_8328", (9, TyVarFree NoLoc Unlifted)), ("a_8321_8320_8327_8329", (9, TyVarFree NoLoc Unlifted)), ("t_8321_8322_8328_8320", (10, TyVarFree NoLoc Lifted)), ("a_8321_8323_8328_8321", (11, TyVarFree NoLoc Unlifted)), ("t_8321_8325_8328_8322", (12, TyVarFree NoLoc Lifted)), ("a_8321_8326_8328_8323", (13, TyVarFree NoLoc Lifted))]
    )
  ]
