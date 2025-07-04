module Generated.AllFutBenchmarks.Misc.Knnbykdtree.Bruteforce (benchmarkDataList) where

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
  [ ( [ "t_8323" ~ "[]t_8323_8327_8322",
        "t_8320" ~ "[]t_8320_8327_8323",
        "if_t_8327_8321" ~ "[]if_t_8327_8321_8327_8324",
        "t_8327_8320" ~ "[]t_8327_8320_8327_8325",
        "t_8324_8325" ~ "[]t_8324_8325_8327_8326",
        "index_8321_8320" ~ "[]index_8321_8320_8327_8327",
        "index_elem_8321_8321" ~ "[]index_elem_8321_8321_8327_8328",
        "[](i32, f32)" ~ "t_8320",
        "t_8323" ~ "t_8320",
        "i64" ~ "i64",
        "t_8326" ~ "i32",
        "t_8329" ~ "i32",
        "index_8321_8320" ~ "index_elem_8321_8321",
        "[][]f32" ~ "[]index_elem_8321_8321",
        "[]f32" ~ "[]f32",
        "index_8321_8320" ~ "[]f32",
        "f32 -> f32" ~ "a_8327 -> b_8328",
        "f32" ~ "a_8327",
        "t_8322_8320" ~ "b_8328",
        "i64" ~ "t_8322_8322",
        "num_8322_8323" ~ "t_8322_8322",
        "t_8322_8328" ~ "t_8322_8322",
        "index_8322_8329" ~ "index_elem_8323_8320",
        "t_8323" ~ "[]index_elem_8323_8320",
        "index_8322_8329" ~ "t_8323_8322",
        "t_8322_8320" ~ "t_8322_8321",
        "kt_8323_8321" ~ "t_8322_8321",
        "i32" ~ "t_8323_8327",
        "i32" ~ "t_8323_8327",
        "t_8324_8322" ~ "t_8323_8327",
        "(t_8324_8323, t_8324_8324, t_8324_8325)" ~ "(t_8322_8320, t_8324_8322, t_8323)",
        "t_8324_8326" ~ "i64",
        "t_8324_8327" ~ "i64",
        "index_8324_8328" ~ "index_elem_8324_8329",
        "t_8324_8325" ~ "[]index_elem_8324_8329",
        "index_8324_8328" ~ "t_8325_8321",
        "t_8325_8322" ~ "kt_8325_8320",
        "t_8324_8323" ~ "t_8325_8323",
        "t_8325_8322" ~ "t_8325_8323",
        "t_8325_8328" ~ "i64",
        "index_8325_8329" ~ "index_elem_8326_8320",
        "t_8324_8325" ~ "[]index_elem_8326_8320",
        "index_8325_8329" ~ "t_8326_8322",
        "t_8326_8323" ~ "kt_8326_8321",
        "t_8326_8324" ~ "i64",
        "t_8324_8325" ~ "[]update_elem_8326_8325",
        "(t_8324_8324, t_8324_8323)" ~ "update_elem_8326_8325",
        "t_8326_8326" ~ "t_8326_8323",
        "bool" ~ "bool",
        "(t_8324_8323, t_8324_8324, t_8324_8325)" ~ "if_t_8326_8327",
        "(t_8325_8322, t_8326_8326, t_8324_8325)" ~ "if_t_8326_8327",
        "(t_8322_8320, t_8324_8322, t_8323)" ~ "if_t_8326_8327",
        "(t_8326_8328, t_8326_8329, t_8327_8320)" ~ "(t_8324_8323, t_8324_8324, t_8324_8325)",
        "bool" ~ "bool",
        "t_8323" ~ "if_t_8327_8321",
        "t_8327_8320" ~ "if_t_8327_8321",
        "t_8320" ~ "if_t_8327_8321",
        "[](i32, f32)" ~ "t_8323"
      ],
      M.empty,
      M.fromList [("t_8320", (4, TyVarFree NoLoc Unlifted)), ("t_8323", (4, TyVarFree NoLoc Lifted)), ("t_8326", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64])), ("a_8327", (4, TyVarFree NoLoc Lifted)), ("b_8328", (4, TyVarFree NoLoc Lifted)), ("t_8329", (4, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8321_8320", (4, TyVarFree NoLoc Unlifted)), ("index_elem_8321_8321", (4, TyVarFree NoLoc Unlifted)), ("t_8322_8320", (5, TyVarFree NoLoc Lifted)), ("t_8322_8321", (6, TyVarPrim NoLoc [Bool, Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8322", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("num_8322_8323", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8322_8328", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8322_8329", (6, TyVarFree NoLoc Unlifted)), ("index_elem_8323_8320", (6, TyVarFree NoLoc Unlifted)), ("kt_8323_8321", (6, TyVarFree NoLoc Lifted)), ("t_8323_8322", (6, TyVarRecord NoLoc (M.fromList [("1", Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName "kt_8323_8321" 10004}) []))]))), ("t_8323_8327", (6, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8324_8322", (7, TyVarFree NoLoc Lifted)), ("t_8324_8323", (8, TyVarFree NoLoc Lifted)), ("t_8324_8324", (8, TyVarFree NoLoc Lifted)), ("t_8324_8325", (8, TyVarFree NoLoc Lifted)), ("t_8324_8326", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64])), ("t_8324_8327", (8, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8324_8328", (8, TyVarFree NoLoc Unlifted)), ("index_elem_8324_8329", (8, TyVarFree NoLoc Unlifted)), ("kt_8325_8320", (8, TyVarFree NoLoc Lifted)), ("t_8325_8321", (8, TyVarRecord NoLoc (M.fromList [("1", Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName "kt_8325_8320" 10026}) []))]))), ("t_8325_8322", (9, TyVarFree NoLoc Lifted)), ("t_8325_8323", (10, TyVarPrim NoLoc [Bool, Signed Int8, Signed Int16, Signed Int32, Signed Int64, Unsigned Int8, Unsigned Int16, Unsigned Int32, Unsigned Int64, FloatType Float16, FloatType Float32, FloatType Float64])), ("t_8325_8328", (10, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("index_8325_8329", (10, TyVarFree NoLoc Unlifted)), ("index_elem_8326_8320", (10, TyVarFree NoLoc Unlifted)), ("kt_8326_8321", (10, TyVarFree NoLoc Lifted)), ("t_8326_8322", (10, TyVarRecord NoLoc (M.fromList [("0", Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName "kt_8326_8321" 10038}) []))]))), ("t_8326_8323", (11, TyVarFree NoLoc Lifted)), ("t_8326_8324", (12, TyVarPrim NoLoc [Signed Int8, Signed Int16, Signed Int32, Signed Int64])), ("update_elem_8326_8325", (12, TyVarFree NoLoc Unlifted)), ("t_8326_8326", (13, TyVarFree NoLoc Lifted)), ("if_t_8326_8327", (10, TyVarFree NoLoc SizeLifted)), ("t_8326_8328", (9, TyVarFree NoLoc Lifted)), ("t_8326_8329", (9, TyVarFree NoLoc Lifted)), ("t_8327_8320", (9, TyVarFree NoLoc Lifted)), ("if_t_8327_8321", (6, TyVarFree NoLoc SizeLifted)), ("t_8323_8327_8322", (4, TyVarFree NoLoc Lifted)), ("t_8320_8327_8323", (4, TyVarFree NoLoc Unlifted)), ("if_t_8327_8321_8327_8324", (6, TyVarFree NoLoc SizeLifted)), ("t_8327_8320_8327_8325", (9, TyVarFree NoLoc Lifted)), ("t_8324_8325_8327_8326", (8, TyVarFree NoLoc Lifted)), ("index_8321_8320_8327_8327", (4, TyVarFree NoLoc Unlifted)), ("index_elem_8321_8321_8327_8328", (4, TyVarFree NoLoc Unlifted))]
    )
  ]
