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
    "t_8326_8323" ~ "[]t_8326_8323_8327_8324",
    "t_8326_8324" ~ "[]t_8326_8324_8327_8325",
    "t_8326_8325" ~ "[]t_8326_8325_8327_8326",
    "t_8325_8326" ~ "[]t_8325_8326_8327_8327",
    "t_8327_8321" ~ "[]t_8327_8321_8327_8328",
    "t_8327_8322" ~ "[]t_8327_8322_8327_8329",
    "t_8327_8323" ~ "[]t_8327_8323_8328_8320",
    "t_8325_8327" ~ "[]t_8325_8327_8328_8321",
    "t_8325_8325" ~ "[]t_8325_8325_8328_8322",
    "a_8324_8320" ~ "[]a_8324_8320_8328_8323",
    "t_8323_8329" ~ "[]t_8323_8329_8328_8324",
    "t_8323_8320" ~ "[]t_8323_8320_8328_8325",
    "[]f32" ~ "[]a_8324",
    "[]f32" ~ "[]b_8325",
    "[]f32" ~ "[]c_8326",
    "[]f32" ~ "[]a_8321_8323",
    "[]f32" ~ "[]b_8321_8324",
    "[]f32" ~ "[]c_8321_8325",
    "(f32, f32, f32) -> {mass: f32} -> (f32, f32, f32) -> {mass: f32, position: {x: f32, y: f32, z: f32}, velocity: {x: f32, y: f32, z: f32}}" ~ "a_8320 -> b_8321 -> c_8322 -> x_8323",
    "[](a_8324, b_8325, c_8326)" ~ "[]a_8320",
    "[]f32" ~ "[]b_8321",
    "[](a_8321_8323, b_8321_8324, c_8321_8325)" ~ "[]c_8322",
    "t_8323_8320" ~ "[]x_8323",
    "i32" ~ "i32",
    "f32" ~ "f32",
    "f32" ~ "f32",
    "t_8323_8320" ~ "[]{mass: f32, position: {x: f32, y: f32, z: f32}, velocity: {x: f32, y: f32, z: f32}}",
    "t_8323_8329" ~ "[]{mass: f32, position: {x: f32, y: f32, z: f32}, velocity: {x: f32, y: f32, z: f32}}",
    "{b: {mass: f32, position: {x: f32, y: f32, z: f32}, velocity: {x: f32, y: f32, z: f32}}} -> ((f32, f32, f32), f32, (f32, f32, f32))" ~ "a_8324_8322 -> x_8324_8323",
    "t_8323_8329" ~ "[]a_8324_8322",
    "[]x_8324_8323" ~ "a_8324_8320",
    "{xs: [](a_8324_8328, b_8324_8329, c_8325_8320)} -> ([]a_8324_8328, []b_8324_8329, []c_8325_8320)" ~ "a_8324_8320 -> b_8324_8321",
    "(t_8325_8325, t_8325_8326, t_8325_8327)" ~ "b_8324_8321",
    "t_8325_8325" ~ "[](a_8325_8328, b_8325_8329, c_8326_8320)",
    "(t_8326_8323, t_8326_8324, t_8326_8325)" ~ "([]a_8325_8328, []b_8325_8329, []c_8326_8320)",
    "t_8325_8327" ~ "[](a_8326_8326, b_8326_8327, c_8326_8328)",
    "(t_8327_8321, t_8327_8322, t_8327_8323)" ~ "([]a_8326_8326, []b_8326_8327, []c_8326_8328)",
    "([]f32, []f32, []f32, []f32, []f32, []f32, []f32)" ~ "(t_8326_8323, t_8326_8324, t_8326_8325, t_8325_8326, t_8327_8321, t_8327_8322, t_8327_8323)"
    ],
    M.fromList [],
    M.fromList [("a_8320",(11,TyVarFree NoLoc Unlifted)),("b_8321",(11,TyVarFree NoLoc Unlifted)),("c_8322",(11,TyVarFree NoLoc Unlifted)),("x_8323",(11,TyVarFree NoLoc Unlifted)),("a_8324",(11,TyVarFree NoLoc Unlifted)),("b_8325",(11,TyVarFree NoLoc Unlifted)),("c_8326",(11,TyVarFree NoLoc Unlifted)),("a_8321_8323",(11,TyVarFree NoLoc Unlifted)),("b_8321_8324",(11,TyVarFree NoLoc Unlifted)),("c_8321_8325",(11,TyVarFree NoLoc Unlifted)),("t_8323_8320",(12,TyVarFree NoLoc Lifted)),("t_8323_8329",(14,TyVarFree NoLoc Lifted)),("a_8324_8320",(15,TyVarFree NoLoc Lifted)),("b_8324_8321",(15,TyVarFree NoLoc Lifted)),("a_8324_8322",(15,TyVarFree NoLoc Unlifted)),("x_8324_8323",(15,TyVarFree NoLoc Unlifted)),("a_8324_8328",(15,TyVarFree NoLoc Unlifted)),("b_8324_8329",(15,TyVarFree NoLoc Unlifted)),("c_8325_8320",(15,TyVarFree NoLoc Unlifted)),("t_8325_8325",(16,TyVarFree NoLoc Lifted)),("t_8325_8326",(16,TyVarFree NoLoc Lifted)),("t_8325_8327",(16,TyVarFree NoLoc Lifted)),("a_8325_8328",(17,TyVarFree NoLoc Unlifted)),("b_8325_8329",(17,TyVarFree NoLoc Unlifted)),("c_8326_8320",(17,TyVarFree NoLoc Unlifted)),("t_8326_8323",(18,TyVarFree NoLoc Lifted)),("t_8326_8324",(18,TyVarFree NoLoc Lifted)),("t_8326_8325",(18,TyVarFree NoLoc Lifted)),("a_8326_8326",(19,TyVarFree NoLoc Unlifted)),("b_8326_8327",(19,TyVarFree NoLoc Unlifted)),("c_8326_8328",(19,TyVarFree NoLoc Unlifted)),("t_8327_8321",(20,TyVarFree NoLoc Lifted)),("t_8327_8322",(20,TyVarFree NoLoc Lifted)),("t_8327_8323",(20,TyVarFree NoLoc Lifted)),("t_8326_8323_8327_8324",(18,TyVarFree NoLoc Lifted)),("t_8326_8324_8327_8325",(18,TyVarFree NoLoc Lifted)),("t_8326_8325_8327_8326",(18,TyVarFree NoLoc Lifted)),("t_8325_8326_8327_8327",(16,TyVarFree NoLoc Lifted)),("t_8327_8321_8327_8328",(20,TyVarFree NoLoc Lifted)),("t_8327_8322_8327_8329",(20,TyVarFree NoLoc Lifted)),("t_8327_8323_8328_8320",(20,TyVarFree NoLoc Lifted)),("t_8325_8327_8328_8321",(16,TyVarFree NoLoc Lifted)),("t_8325_8325_8328_8322",(16,TyVarFree NoLoc Lifted)),("a_8324_8320_8328_8323",(15,TyVarFree NoLoc Lifted)),("t_8323_8329_8328_8324",(14,TyVarFree NoLoc Lifted)),("t_8323_8320_8328_8325",(12,TyVarFree NoLoc Lifted))]
  )
  ]
