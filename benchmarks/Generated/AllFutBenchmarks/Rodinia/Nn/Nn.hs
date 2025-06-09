module Generated.AllFutBenchmarks.Rodinia.Nn.Nn (benchmarkDataList) where

import Futhark.Util.Loc (Loc(NoLoc))
import Data.Map qualified as M
import Language.Futhark.Syntax
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker.Constraints
  ( CtTy (..),
    Reason (..),
    TyParams,
    TyVarInfo (..),
    TyVars
  )

(~) :: TypeBase () NoUniqueness -> TypeBase () NoUniqueness -> CtTy ()
t1 ~ t2 = CtEq (Reason mempty) t1 t2

type BenchmarkCaseData = ([CtTy ()], TyParams, TyVars ())

benchmarkDataList :: [BenchmarkCaseData]
benchmarkDataList =
  [   (    
    [
    "t_8321_8320_8328" ~ "[]t_8321_8320_8328_8321_8321_8326",
    "t_8325_8329" ~ "[]t_8325_8329_8321_8321_8327",
    "t_8326_8320" ~ "[]t_8326_8320_8321_8321_8328",
    "t_8321_8320_8329" ~ "[]t_8321_8320_8329_8321_8321_8329",
    "t_8325_8328" ~ "[]t_8325_8328_8321_8322_8320",
    "t_8325_8320" ~ "[]t_8325_8320_8321_8322_8321",
    "t_8326" ~ "[]t_8326_8321_8322_8322",
    "[]f32" ~ "[]a_8320",
    "[]f32" ~ "[]b_8321",
    "t_8326" ~ "[](a_8320, b_8321)",
    "f32" ~ "t_8329",
    "f32" ~ "t_8329",
    "f32" ~ "t_8321_8324",
    "f32" ~ "t_8321_8324",
    "t_8329" ~ "t_8328",
    "t_8321_8324" ~ "t_8328",
    "f32" ~ "t_8322_8324",
    "f32" ~ "t_8322_8324",
    "f32" ~ "t_8322_8329",
    "f32" ~ "t_8322_8329",
    "t_8322_8324" ~ "t_8322_8323",
    "t_8322_8329" ~ "t_8322_8323",
    "t_8328" ~ "t_8327",
    "t_8322_8323" ~ "t_8327",
    "t_8327" ~ "f32",
    "(f32, f32) -> f32" ~ "a_8324_8324 -> x_8324_8325",
    "t_8326" ~ "[]a_8324_8324",
    "t_8325_8320" ~ "[]x_8324_8325",
    "i32" ~ "i32",
    "i64" ~ "i64",
    "(i32, f32)" ~ "t_8325_8321",
    "t_8325_8328" ~ "[]t_8325_8321",
    "(t_8325_8329, t_8326_8320)" ~ "(t_8325_8328, t_8325_8320)",
    "t_8326_8321" ~ "i32",
    "t_8326_8323" ~ "t_8326_8327",
    "t_8326_8325" ~ "t_8326_8327",
    "t_8326_8325" ~ "t_8327_8322",
    "t_8326_8323" ~ "t_8327_8322",
    "t_8326_8324" ~ "t_8327_8327",
    "t_8326_8326" ~ "t_8327_8327",
    "bool" ~ "bool",
    "(t_8326_8323, t_8326_8324)" ~ "if_t_8328_8322",
    "(t_8326_8325, t_8326_8326)" ~ "if_t_8328_8322",
    "bool" ~ "bool",
    "(t_8326_8325, t_8326_8326)" ~ "if_t_8328_8323",
    "if_t_8328_8322" ~ "if_t_8328_8323",
    "bool" ~ "bool",
    "(t_8326_8323, t_8326_8324)" ~ "if_t_8328_8324",
    "if_t_8328_8323" ~ "if_t_8328_8324",
    "i64" ~ "i64",
    "t_8326_8320" ~ "[]a_8328_8326",
    "[]i64" ~ "[]b_8328_8327",
    "(t_8326_8323, t_8326_8324) -> (t_8326_8325, t_8326_8326) -> if_t_8328_8324" ~ "a_8326_8322 -> a_8326_8322 -> a_8326_8322",
    "(f32, num_8328_8325)" ~ "a_8326_8322",
    "[](a_8328_8326, b_8328_8327)" ~ "[]a_8326_8322",
    "(t_8321_8320_8320, t_8321_8320_8321)" ~ "a_8326_8322",
    "t_8321_8320_8322" ~ "t_8321_8320_8321",
    "t_8326_8320" ~ "[]update_elem_8321_8320_8323",
    "f32" ~ "update_elem_8321_8320_8323",
    "t_8321_8320_8324" ~ "i32",
    "t_8321_8320_8321" ~ "i64",
    "t_8325_8329" ~ "[]update_elem_8321_8320_8327",
    "(i32, t_8321_8320_8320)" ~ "update_elem_8321_8320_8327",
    "(t_8325_8328, t_8325_8320)" ~ "(t_8325_8329, t_8326_8320)",
    "(t_8321_8320_8328, t_8321_8320_8329)" ~ "(t_8325_8329, t_8326_8320)",
    "t_8321_8320_8328" ~ "[](a_8321_8321_8320, b_8321_8321_8321)",
    "([]i32, []f32)" ~ "([]a_8321_8321_8320, []b_8321_8321_8321)"
    ],
    M.fromList [],
    M.fromList [("a_8320",(6,TyVarFree NoLoc Unlifted)),("b_8321",(6,TyVarFree NoLoc Unlifted)),("t_8326",(7,TyVarFree NoLoc Lifted)),("t_8327",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8328",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8329",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8321_8324",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8323",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8324",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8329",(10,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("a_8324_8324",(8,TyVarFree NoLoc Unlifted)),("x_8324_8325",(8,TyVarFree NoLoc Unlifted)),("t_8325_8320",(9,TyVarFree NoLoc Lifted)),("t_8325_8321",(10,TyVarFree NoLoc Unlifted)),("t_8325_8328",(11,TyVarFree NoLoc Lifted)),("t_8325_8329",(12,TyVarFree NoLoc Lifted)),("t_8326_8320",(12,TyVarFree NoLoc Lifted)),("t_8326_8321",(12,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64])),("a_8326_8322",(12,TyVarFree NoLoc Unlifted)),("t_8326_8323",(13,TyVarFree NoLoc Lifted)),("t_8326_8324",(13,TyVarFree NoLoc Lifted)),("t_8326_8325",(14,TyVarFree NoLoc Lifted)),("t_8326_8326",(14,TyVarFree NoLoc Lifted)),("t_8326_8327",(15,TyVarPrim NoLoc [Bool,Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8327_8322",(15,TyVarPrim NoLoc [Bool,Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8327_8327",(15,TyVarPrim NoLoc [Bool,Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("if_t_8328_8322",(15,TyVarFree NoLoc SizeLifted)),("if_t_8328_8323",(15,TyVarFree NoLoc SizeLifted)),("if_t_8328_8324",(15,TyVarFree NoLoc SizeLifted)),("num_8328_8325",(12,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("a_8328_8326",(12,TyVarFree NoLoc Unlifted)),("b_8328_8327",(12,TyVarFree NoLoc Unlifted)),("t_8321_8320_8320",(13,TyVarFree NoLoc Lifted)),("t_8321_8320_8321",(13,TyVarFree NoLoc Lifted)),("t_8321_8320_8322",(14,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("update_elem_8321_8320_8323",(14,TyVarFree NoLoc Unlifted)),("t_8321_8320_8324",(14,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("update_elem_8321_8320_8327",(14,TyVarFree NoLoc Unlifted)),("t_8321_8320_8328",(13,TyVarFree NoLoc Lifted)),("t_8321_8320_8329",(13,TyVarFree NoLoc Lifted)),("a_8321_8321_8320",(14,TyVarFree NoLoc Unlifted)),("b_8321_8321_8321",(14,TyVarFree NoLoc Unlifted)),("t_8321_8320_8328_8321_8321_8326",(13,TyVarFree NoLoc Lifted)),("t_8325_8329_8321_8321_8327",(12,TyVarFree NoLoc Lifted)),("t_8326_8320_8321_8321_8328",(12,TyVarFree NoLoc Lifted)),("t_8321_8320_8329_8321_8321_8329",(13,TyVarFree NoLoc Lifted)),("t_8325_8328_8321_8322_8320",(11,TyVarFree NoLoc Lifted)),("t_8325_8320_8321_8322_8321",(9,TyVarFree NoLoc Lifted)),("t_8326_8321_8322_8322",(7,TyVarFree NoLoc Lifted))]
  )
  ]
