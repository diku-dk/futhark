module Generated.AllFutBenchmarks.Micro.Mmm.Sgemm (benchmarkDataList) where

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
    "x_8322" ~ "[]x_8322_8326_8329",
    "b_8321" ~ "[]b_8321_8327_8320",
    "a_8320" ~ "[]a_8320_8327_8321",
    "t_8323" ~ "[]t_8323_8327_8322",
    "t_8324" ~ "[]t_8324_8327_8323",
    "a_8325" ~ "[]a_8325_8327_8324",
    "t_8328" ~ "[]t_8328_8327_8325",
    "a_8321_8320" ~ "[]a_8321_8320_8327_8326",
    "t_8321_8325" ~ "t_8321_8327",
    "t_8321_8326" ~ "t_8321_8327",
    "{a: t_8321_8325} -> {b: t_8321_8326} -> t_8321_8327" ~ "a_8321_8322 -> b_8321_8323 -> x_8321_8324",
    "t_8323" ~ "[]a_8321_8322",
    "t_8328" ~ "[]b_8321_8323",
    "t_8322_8329 -> t_8322_8329 -> t_8322_8329" ~ "a_8322_8328 -> a_8322_8328 -> a_8322_8328",
    "f32" ~ "a_8322_8328",
    "[]x_8321_8324" ~ "a_8321_8320",
    "{as: []a_8322_8328} -> a_8322_8328" ~ "a_8321_8320 -> b_8321_8321",
    "t_8323_8328" ~ "b_8321_8321",
    "f32" ~ "t_8324_8320",
    "t_8323_8328" ~ "t_8324_8320",
    "f32" ~ "t_8324_8325",
    "t_8329" ~ "t_8324_8325",
    "t_8324_8320" ~ "t_8323_8329",
    "t_8324_8325" ~ "t_8323_8329",
    "[][]f32" ~ "[][]t_8325_8324",
    "(Bcol: t_8328) -> {c: t_8329} -> t_8323_8329" ~ "a_8325 -> b_8326 -> x_8327",
    "[][]t_8325_8324" ~ "[]a_8325",
    "t_8324" ~ "[]b_8326",
    "(Arow: t_8323) -> (Crow: t_8324) -> []x_8327" ~ "a_8320 -> b_8321 -> x_8322",
    "[][]f32" ~ "[]a_8320",
    "[][]f32" ~ "[]b_8321",
    "[][]f32" ~ "[]x_8322"
    ],
    M.fromList [],
    M.fromList [("a_8320",(6,TyVarFree NoLoc Unlifted)),("b_8321",(6,TyVarFree NoLoc Unlifted)),("x_8322",(6,TyVarFree NoLoc Unlifted)),("t_8323",(7,TyVarFree NoLoc Lifted)),("t_8324",(8,TyVarFree NoLoc Lifted)),("a_8325",(9,TyVarFree NoLoc Unlifted)),("b_8326",(9,TyVarFree NoLoc Unlifted)),("x_8327",(9,TyVarFree NoLoc Unlifted)),("t_8328",(10,TyVarFree NoLoc Lifted)),("t_8329",(11,TyVarFree NoLoc Lifted)),("a_8321_8320",(12,TyVarFree NoLoc Lifted)),("b_8321_8321",(12,TyVarFree NoLoc Lifted)),("a_8321_8322",(12,TyVarFree NoLoc Unlifted)),("b_8321_8323",(12,TyVarFree NoLoc Unlifted)),("x_8321_8324",(12,TyVarFree NoLoc Unlifted)),("t_8321_8325",(13,TyVarFree NoLoc Lifted)),("t_8321_8326",(14,TyVarFree NoLoc Lifted)),("t_8321_8327",(15,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("a_8322_8328",(12,TyVarFree NoLoc Unlifted)),("t_8322_8329",(12,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8323_8328",(13,TyVarFree NoLoc Lifted)),("t_8323_8329",(14,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8324_8320",(14,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8324_8325",(14,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8325_8324",(9,TyVarFree NoLoc Unlifted)),("x_8322_8326_8329",(6,TyVarFree NoLoc Unlifted)),("b_8321_8327_8320",(6,TyVarFree NoLoc Unlifted)),("a_8320_8327_8321",(6,TyVarFree NoLoc Unlifted)),("t_8323_8327_8322",(7,TyVarFree NoLoc Lifted)),("t_8324_8327_8323",(8,TyVarFree NoLoc Lifted)),("a_8325_8327_8324",(9,TyVarFree NoLoc Unlifted)),("t_8328_8327_8325",(10,TyVarFree NoLoc Lifted)),("a_8321_8320_8327_8326",(12,TyVarFree NoLoc Lifted))]
  )
  ]
