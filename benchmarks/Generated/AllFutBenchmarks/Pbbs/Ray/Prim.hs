module Generated.AllFutBenchmarks.Pbbs.Ray.Prim (benchmarkDataList) where

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
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8321",
    "kt_8320" ~ "t_8323",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8325",
    "kt_8324" ~ "t_8327",
    "kt_8322" ~ "f32",
    "kt_8326" ~ "f32",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8321_8323",
    "kt_8321_8322" ~ "t_8321_8325",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8321_8327",
    "kt_8321_8326" ~ "t_8321_8329",
    "kt_8321_8324" ~ "f32",
    "kt_8321_8328" ~ "f32",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8322_8325",
    "kt_8322_8324" ~ "t_8322_8327",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8322_8329",
    "kt_8322_8328" ~ "t_8323_8321",
    "kt_8322_8326" ~ "f32",
    "kt_8323_8320" ~ "f32",
    "(f32, f32, f32)" ~ "(f32, f32, f32)",
    "t_8323_8328" ~ "{x: f32, y: f32, z: f32}",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8324_8320",
    "kt_8323_8329" ~ "t_8324_8322",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8324_8324",
    "kt_8324_8323" ~ "t_8324_8326",
    "kt_8324_8321" ~ "f32",
    "kt_8324_8325" ~ "f32",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8325_8322",
    "kt_8325_8321" ~ "t_8325_8324",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8325_8326",
    "kt_8325_8325" ~ "t_8325_8328",
    "kt_8325_8323" ~ "f32",
    "kt_8325_8327" ~ "f32",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8326_8324",
    "kt_8326_8323" ~ "t_8326_8326",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "t_8326_8328",
    "kt_8326_8327" ~ "t_8327_8320",
    "kt_8326_8325" ~ "f32",
    "kt_8326_8329" ~ "f32",
    "(f32, f32, f32)" ~ "(f32, f32, f32)",
    "t_8327_8327" ~ "{x: f32, y: f32, z: f32}",
    "{max: {x: f32, y: f32, z: f32}, min: {x: f32, y: f32, z: f32}}" ~ "{max: t_8327_8327, min: t_8323_8328}"
    ],
    M.fromList [],
    M.fromList [("kt_8320",(3,TyVarFree NoLoc Lifted)),("t_8321",(3,TyVarRecord NoLoc (M.fromList [("min",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8320") 11933}) []))]))),("kt_8322",(3,TyVarFree NoLoc Lifted)),("t_8323",(3,TyVarRecord NoLoc (M.fromList [("x",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8322") 11935}) []))]))),("kt_8324",(3,TyVarFree NoLoc Lifted)),("t_8325",(3,TyVarRecord NoLoc (M.fromList [("min",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8324") 11937}) []))]))),("kt_8326",(3,TyVarFree NoLoc Lifted)),("t_8327",(3,TyVarRecord NoLoc (M.fromList [("x",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8326") 11939}) []))]))),("kt_8321_8322",(3,TyVarFree NoLoc Lifted)),("t_8321_8323",(3,TyVarRecord NoLoc (M.fromList [("min",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8321_8322") 11946}) []))]))),("kt_8321_8324",(3,TyVarFree NoLoc Lifted)),("t_8321_8325",(3,TyVarRecord NoLoc (M.fromList [("y",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8321_8324") 11948}) []))]))),("kt_8321_8326",(3,TyVarFree NoLoc Lifted)),("t_8321_8327",(3,TyVarRecord NoLoc (M.fromList [("min",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8321_8326") 11950}) []))]))),("kt_8321_8328",(3,TyVarFree NoLoc Lifted)),("t_8321_8329",(3,TyVarRecord NoLoc (M.fromList [("y",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8321_8328") 11952}) []))]))),("kt_8322_8324",(3,TyVarFree NoLoc Lifted)),("t_8322_8325",(3,TyVarRecord NoLoc (M.fromList [("min",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8322_8324") 11959}) []))]))),("kt_8322_8326",(3,TyVarFree NoLoc Lifted)),("t_8322_8327",(3,TyVarRecord NoLoc (M.fromList [("z",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8322_8326") 11961}) []))]))),("kt_8322_8328",(3,TyVarFree NoLoc Lifted)),("t_8322_8329",(3,TyVarRecord NoLoc (M.fromList [("min",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8322_8328") 11963}) []))]))),("kt_8323_8320",(3,TyVarFree NoLoc Lifted)),("t_8323_8321",(3,TyVarRecord NoLoc (M.fromList [("z",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8323_8320") 11965}) []))]))),("t_8323_8328",(4,TyVarFree NoLoc Lifted)),("kt_8323_8329",(5,TyVarFree NoLoc Lifted)),("t_8324_8320",(5,TyVarRecord NoLoc (M.fromList [("max",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8323_8329") 11976}) []))]))),("kt_8324_8321",(5,TyVarFree NoLoc Lifted)),("t_8324_8322",(5,TyVarRecord NoLoc (M.fromList [("x",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8324_8321") 11978}) []))]))),("kt_8324_8323",(5,TyVarFree NoLoc Lifted)),("t_8324_8324",(5,TyVarRecord NoLoc (M.fromList [("max",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8324_8323") 11980}) []))]))),("kt_8324_8325",(5,TyVarFree NoLoc Lifted)),("t_8324_8326",(5,TyVarRecord NoLoc (M.fromList [("x",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8324_8325") 11982}) []))]))),("kt_8325_8321",(5,TyVarFree NoLoc Lifted)),("t_8325_8322",(5,TyVarRecord NoLoc (M.fromList [("max",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8325_8321") 11989}) []))]))),("kt_8325_8323",(5,TyVarFree NoLoc Lifted)),("t_8325_8324",(5,TyVarRecord NoLoc (M.fromList [("y",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8325_8323") 11991}) []))]))),("kt_8325_8325",(5,TyVarFree NoLoc Lifted)),("t_8325_8326",(5,TyVarRecord NoLoc (M.fromList [("max",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8325_8325") 11993}) []))]))),("kt_8325_8327",(5,TyVarFree NoLoc Lifted)),("t_8325_8328",(5,TyVarRecord NoLoc (M.fromList [("y",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8325_8327") 11995}) []))]))),("kt_8326_8323",(5,TyVarFree NoLoc Lifted)),("t_8326_8324",(5,TyVarRecord NoLoc (M.fromList [("max",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8326_8323") 12002}) []))]))),("kt_8326_8325",(5,TyVarFree NoLoc Lifted)),("t_8326_8326",(5,TyVarRecord NoLoc (M.fromList [("z",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8326_8325") 12004}) []))]))),("kt_8326_8327",(5,TyVarFree NoLoc Lifted)),("t_8326_8328",(5,TyVarRecord NoLoc (M.fromList [("max",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8326_8327") 12006}) []))]))),("kt_8326_8329",(5,TyVarFree NoLoc Lifted)),("t_8327_8320",(5,TyVarRecord NoLoc (M.fromList [("z",Scalar (TypeVar NoUniqueness (QualName {qualQuals = [], qualLeaf = VName ("kt_8326_8329") 12008}) []))]))),("t_8327_8327",(6,TyVarFree NoLoc Lifted))]
  )
  ]
