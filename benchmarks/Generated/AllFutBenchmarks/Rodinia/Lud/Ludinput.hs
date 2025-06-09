module Generated.AllFutBenchmarks.Rodinia.Lud.Ludinput (benchmarkDataList) where

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
    "x_8326_8325" ~ "[]x_8326_8325_8321_8320_8320",
    "t_8321" ~ "float_8320",
    "t_8322" ~ "float_8320",
    "i64" ~ "t_8323",
    "i64" ~ "t_8323",
    "t_8321_8320" ~ "i64",
    "i64" ~ "t_8329",
    "i64" ~ "t_8329",
    "t_8329" ~ "t_8328",
    "num_8321_8325" ~ "t_8328",
    "t_8322_8320" ~ "t_8328",
    "t_8322_8320" ~ "i64",
    "t_8322" ~ "t_8322_8323",
    "f32" ~ "t_8322_8323",
    "t_8322_8323" ~ "f32",
    "float_8322_8322" ~ "t_8322_8321",
    "f32" ~ "t_8322_8321",
    "i64" ~ "t_8323_8327",
    "i64" ~ "t_8323_8327",
    "t_8323_8327" ~ "t_8323_8326",
    "num_8324_8322" ~ "t_8323_8326",
    "t_8324_8327" ~ "t_8323_8326",
    "t_8324_8327" ~ "i64",
    "t_8322" ~ "t_8325_8320",
    "f32" ~ "t_8325_8320",
    "t_8325_8320" ~ "f32",
    "float_8324_8329" ~ "t_8324_8328",
    "f32" ~ "t_8324_8328",
    "bool" ~ "bool",
    "t_8322_8321" ~ "if_t_8326_8323",
    "t_8324_8328" ~ "if_t_8326_8323",
    "i64" ~ "t_8327_8322",
    "num_8327_8323" ~ "t_8327_8322",
    "t_8327_8322" ~ "t_8327_8321",
    "t_8326_8326" ~ "t_8327_8321",
    "t_8327_8321" ~ "t_8327_8320",
    "t_8326_8329" ~ "t_8327_8320",
    "t_8327_8320" ~ "i64",
    "i64" ~ "i64",
    "{j: t_8326_8329} -> if_t_8326_8323" ~ "a_8326_8327 -> x_8326_8328",
    "[]i64" ~ "[]a_8326_8327",
    "i64" ~ "i64",
    "{i: t_8326_8326} -> []x_8326_8328" ~ "a_8326_8324 -> x_8326_8325",
    "[]i64" ~ "[]a_8326_8324",
    "[][]f32" ~ "[]x_8326_8325"
    ],
    M.fromList [],
    M.fromList [("float_8320",(2,TyVarPrim NoLoc [FloatType Float16,FloatType Float32,FloatType Float64])),("t_8321",(2,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322",(3,TyVarFree NoLoc Lifted)),("t_8323",(6,TyVarPrim NoLoc [Bool,Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8328",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8329",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8321_8320",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8321_8325",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8320",(7,TyVarFree NoLoc Lifted)),("t_8322_8321",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("float_8322_8322",(8,TyVarPrim NoLoc [FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8323",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8323_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8323_8327",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8324_8322",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8324_8327",(7,TyVarFree NoLoc Lifted)),("t_8324_8328",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("float_8324_8329",(8,TyVarPrim NoLoc [FloatType Float16,FloatType Float32,FloatType Float64])),("t_8325_8320",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("if_t_8326_8323",(6,TyVarFree NoLoc SizeLifted)),("a_8326_8324",(4,TyVarFree NoLoc Unlifted)),("x_8326_8325",(4,TyVarFree NoLoc Unlifted)),("t_8326_8326",(5,TyVarFree NoLoc Lifted)),("a_8326_8327",(6,TyVarFree NoLoc Unlifted)),("x_8326_8328",(6,TyVarFree NoLoc Unlifted)),("t_8326_8329",(7,TyVarFree NoLoc Lifted)),("t_8327_8320",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8327_8321",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8327_8322",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8327_8323",(8,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("x_8326_8325_8321_8320_8320",(4,TyVarFree NoLoc Unlifted))]
  )
  ]
