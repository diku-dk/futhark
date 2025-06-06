module Generated.AllFutBenchmarks.Accelerate.Nbody.Nbody (benchmarkDataList) where

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
    "index_8322_8325" ~ "[]index_8322_8325_8329_8323",
    "index_8326_8323" ~ "[]index_8326_8323_8329_8324",
    "t_8321_8329" ~ "[]t_8321_8329_8329_8325",
    "t_8321_8328" ~ "[]t_8321_8328_8329_8326",
    "index_8324_8324" ~ "[]index_8324_8324_8329_8327",
    "t_8320" ~ "a_1",
    "t_8320" ~ "a_1",
    "bool" ~ "bool",
    "num_8326" ~ "if_t_8328",
    "num_8327" ~ "if_t_8328",
    "bool" ~ "bool",
    "num_8323" ~ "if_t_8329",
    "if_t_8328" ~ "if_t_8329",
    "num_8321_8321" ~ "i32",
    "{x: t_8320} -> if_t_8329" ~ "a_8321_8320 -> i64",
    "[]a_1" ~ "[]a_8321_8320",
    "(t_8321_8328, t_8321_8329)" ~ "([]a_8321_8320, []i64)",
    "i64" ~ "num_8322_8320",
    "t_8322_8322" ~ "num_8322_8321",
    "index_8322_8323" ~ "index_elem_8322_8324",
    "t_8321_8329" ~ "[]index_elem_8322_8324",
    "i64" ~ "index_8322_8323",
    "index_8322_8325" ~ "[]index_elem_8322_8326",
    "t_8321_8328" ~ "[]index_elem_8322_8326",
    "t_8322_8328" ~ "num_8322_8327",
    "index_8322_8329" ~ "index_elem_8323_8320",
    "t_8321_8329" ~ "[]index_elem_8323_8320",
    "i64" ~ "index_8322_8329",
    "t_8323_8323" ~ "num_8323_8322",
    "index_8323_8324" ~ "index_elem_8323_8325",
    "t_8321_8329" ~ "[]index_elem_8323_8325",
    "t_8323_8327" ~ "num_8323_8326",
    "index_8323_8328" ~ "index_elem_8323_8329",
    "t_8321_8329" ~ "[]index_elem_8323_8329",
    "index_8323_8324" ~ "t_8323_8321",
    "index_8323_8328" ~ "t_8323_8321",
    "i64" ~ "t_8323_8321",
    "index_8324_8324" ~ "[]index_elem_8324_8325",
    "t_8321_8328" ~ "[]index_elem_8324_8325",
    "t_8324_8327" ~ "num_8324_8326",
    "index_8324_8328" ~ "index_elem_8324_8329",
    "t_8321_8329" ~ "[]index_elem_8324_8329",
    "index_8324_8328" ~ "i64",
    "index_8324_8324" ~ "[]a_1",
    "t_8325_8322" ~ "num_8325_8321",
    "index_8325_8323" ~ "index_elem_8325_8324",
    "t_8321_8329" ~ "[]index_elem_8325_8324",
    "t_8325_8326" ~ "num_8325_8325",
    "index_8325_8327" ~ "index_elem_8325_8328",
    "t_8321_8329" ~ "[]index_elem_8325_8328",
    "index_8325_8323" ~ "t_8325_8320",
    "index_8325_8327" ~ "t_8325_8320",
    "i64" ~ "t_8325_8320",
    "index_8326_8323" ~ "[]index_elem_8326_8324",
    "t_8321_8328" ~ "[]index_elem_8326_8324",
    "t_8326_8328" ~ "num_8326_8327",
    "index_8326_8329" ~ "index_elem_8327_8320",
    "t_8321_8329" ~ "[]index_elem_8327_8320",
    "i64" ~ "t_8326_8326",
    "index_8326_8329" ~ "t_8326_8326",
    "t_8327_8326" ~ "num_8327_8325",
    "index_8327_8327" ~ "index_elem_8327_8328",
    "t_8321_8329" ~ "[]index_elem_8327_8328",
    "t_8326_8326" ~ "t_8326_8325",
    "index_8327_8327" ~ "t_8326_8325",
    "t_8326_8325" ~ "i64",
    "index_8326_8323" ~ "[]a_1",
    "i64" ~ "t_8328_8324",
    "i64" ~ "t_8328_8324",
    "t_8328_8324" ~ "t_8328_8323",
    "i64" ~ "t_8328_8323",
    "t_8328_8323" ~ "i64",
    "([]a_1, []a_1, []a_1)" ~ "(index_8322_8325, []a_1, []a_1)"
    ],
    M.fromList [("a_1",(0,Unlifted,NoLoc))],
    M.fromList [("t_8320",(5,TyVarFree NoLoc Lifted)),("num_8323",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8327",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("if_t_8328",(6,TyVarFree NoLoc SizeLifted)),("if_t_8329",(6,TyVarFree NoLoc SizeLifted)),("a_8321_8320",(4,TyVarFree NoLoc Unlifted)),("num_8321_8321",(4,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8321_8328",(5,TyVarFree NoLoc Lifted)),("t_8321_8329",(5,TyVarFree NoLoc Lifted)),("num_8322_8320",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8322_8321",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8322",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8322_8323",(6,TyVarFree NoLoc Unlifted)),("index_elem_8322_8324",(6,TyVarFree NoLoc Unlifted)),("index_8322_8325",(6,TyVarFree NoLoc Unlifted)),("index_elem_8322_8326",(6,TyVarFree NoLoc Unlifted)),("num_8322_8327",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8322_8328",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8322_8329",(6,TyVarFree NoLoc Unlifted)),("index_elem_8323_8320",(6,TyVarFree NoLoc Unlifted)),("t_8323_8321",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8323_8322",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8323_8323",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8323_8324",(6,TyVarFree NoLoc Unlifted)),("index_elem_8323_8325",(6,TyVarFree NoLoc Unlifted)),("num_8323_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8323_8327",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8323_8328",(6,TyVarFree NoLoc Unlifted)),("index_elem_8323_8329",(6,TyVarFree NoLoc Unlifted)),("index_8324_8324",(6,TyVarFree NoLoc Unlifted)),("index_elem_8324_8325",(6,TyVarFree NoLoc Unlifted)),("num_8324_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8324_8327",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8324_8328",(6,TyVarFree NoLoc Unlifted)),("index_elem_8324_8329",(6,TyVarFree NoLoc Unlifted)),("t_8325_8320",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8325_8321",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8325_8322",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8325_8323",(6,TyVarFree NoLoc Unlifted)),("index_elem_8325_8324",(6,TyVarFree NoLoc Unlifted)),("num_8325_8325",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8325_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8325_8327",(6,TyVarFree NoLoc Unlifted)),("index_elem_8325_8328",(6,TyVarFree NoLoc Unlifted)),("index_8326_8323",(6,TyVarFree NoLoc Unlifted)),("index_elem_8326_8324",(6,TyVarFree NoLoc Unlifted)),("t_8326_8325",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8326_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("num_8326_8327",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8326_8328",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8326_8329",(6,TyVarFree NoLoc Unlifted)),("index_elem_8327_8320",(6,TyVarFree NoLoc Unlifted)),("num_8327_8325",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8327_8326",(6,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64])),("index_8327_8327",(6,TyVarFree NoLoc Unlifted)),("index_elem_8327_8328",(6,TyVarFree NoLoc Unlifted)),("t_8328_8323",(4,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("t_8328_8324",(4,TyVarPrim NoLoc [Signed Int8,Signed Int16,Signed Int32,Signed Int64,Unsigned Int8,Unsigned Int16,Unsigned Int32,Unsigned Int64,FloatType Float16,FloatType Float32,FloatType Float64])),("index_8322_8325_8329_8323",(6,TyVarFree NoLoc Unlifted)),("index_8326_8323_8329_8324",(6,TyVarFree NoLoc Unlifted)),("t_8321_8329_8329_8325",(5,TyVarFree NoLoc Lifted)),("t_8321_8328_8329_8326",(5,TyVarFree NoLoc Lifted)),("index_8324_8324_8329_8327",(6,TyVarFree NoLoc Unlifted))]
  )
  ]
