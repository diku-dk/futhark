{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched.Env
  ( FwdTileTab
  , InvTileTab
  , Env (..)
  , emptyEnv
  , addTileBinding
  , addTransf2Env
  , addBinOp2Env
  , expandSE
  , eqPEs
  , addPes
  , mulPes
  , minPes
  , invPerm
  )
where

import Data.List qualified as L
import Data.Maybe
import Data.Map.Strict qualified as M
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.Tools
import Futhark.Util.Pretty hiding (line, sep, (</>))

--import Debug.Trace

-----------------------------------
--- Environment
-----------------------------------

-- | Binds a size expression to its tiled
--     approximation: `e -> [t1,...,tm]`,
--     where `t1 * ... * tm` is an upper
--     bound of `e`, i.e., `e <= t1*...*tm`
--   E.g., `let (m, t1, t2) = strip2 (2*M)`
--     produces `2*M -> [m, t1, t2]`
--   ToDo: maybe use the Poly IR here?
type FwdTileTab = M.Map (PrimExp VName) Names

-- ^ Binds a symbolic tile names to a tuple:
--     1. the original size that was tiled
--     2. the list containing the produced tiles.
--   E.g., `let (m, t1, t2) = strip2 (2*M)`
--     produces `m -> (2*M, [m, t1, t2])`
type InvTileTab = M.Map VName (PrimExp VName, Names)

data Env = Env
  { appTilesFwd :: FwdTileTab,
    appTilesInv :: InvTileTab,
    iotas       :: M.Map VName (PrimExp VName, SubExp, Stms SOACS),
    -- ^ binds the name of an iota array to its size
    scalars     :: M.Map VName (PrimExp VName),
    -- ^ binds the name of a scalar var to its value expression
    arrTransf   :: M.Map VName (VName, H.ArrayTransforms)
    -- ^ binds the name of an array to its transformations
  } deriving Show

emptyEnv :: Env
emptyEnv = Env mempty mempty mempty mempty mempty

addTileBinding :: Env -> PrimExp VName -> [VName] -> Env
addTileBinding env pe tile_nms =
  let nms = namesFromList tile_nms
      fwdenv = M.insert pe nms $ appTilesFwd env
      bwdenv = foldl (foldfun nms) (appTilesInv env) tile_nms
  in  Env { appTilesFwd = fwdenv, appTilesInv = bwdenv, iotas = iotas env, scalars = mempty, arrTransf = mempty }
  where
    foldfun nms env_cur nm = M.insert nm (pe, nms) env_cur

instance Pretty FwdTileTab where
  pretty = pretty . M.toList

instance Pretty InvTileTab where
  pretty = pretty . M.toList

addTransf2Env :: Env -> VName -> VName -> H.ArrayTransforms -> Env
addTransf2Env env nm_new nm_old trsfs =
  let (nm_old', trsfs') =
       case M.lookup nm_old (arrTransf env) of
         Nothing -> (nm_old, H.noTransforms)
         Just (nm', trsf') -> (nm', trsf')
  in env { arrTransf = M.insert nm_new (nm_old', trsfs' <> trsfs) (arrTransf env) }

expandSE :: Env -> SubExp -> PrimType -> PrimExp  VName
expandSE _ (Constant pval) _ = ValueExp pval
expandSE env (Var vnm) ptp = fromMaybe (LeafExp vnm ptp) $ M.lookup vnm $ scalars env

addBinOp2Env :: Env -> (VName, Type) -> BinOp -> SubExp -> SubExp -> Env
addBinOp2Env env (nm,tp) bop s1 s2 =
  env { scalars = M.insert nm (BinOpExp bop (expSE s1) (expSE s2)) (scalars env) }
  where expSE se = expandSE env se $ elemType tp

eqPEs :: PrimExp VName -> PrimExp VName -> Bool
eqPEs p1 p2 | p1 == p2 = True
eqPEs (BinOpExp bop1 pe11 pe12) ((BinOpExp bop2 pe21 pe22))
  | pe11 == pe22 && pe12 == pe21 && bop1 == bop2 = isCommBop bop1
  where
    isCommBop Add{} = True
    isCommBop Mul{} = True
    isCommBop SMax{}= True
    isCommBop SMin{}= True
    isCommBop UMin{}= True
    isCommBop UMax{}= True
    isCommBop _     = False
eqPEs _ _ = False

pe0 :: PrimExp VName
pe0 = ValueExp $ IntValue $ Int64Value 0

pe1 :: PrimExp VName
pe1 = ValueExp $ IntValue $ Int64Value 1

addPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
addPes e1 e2 | e1 == pe0 = e2
addPes e1 e2 | e2 == pe0 = e1
addPes e1 e2 = BinOpExp (Add Int64 OverflowWrap) e1 e2  -- OverflowUndef

mulPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
mulPes e1 e2 | e1 == pe1 = e2
mulPes e1 e2 | e2 == pe1 = e1
mulPes e1 e2 = BinOpExp (Mul Int64 OverflowWrap) e1 e2  -- OverflowUndef

minPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
minPes e1 e2 | e1 == e2 = e1
minPes e1 e2 = BinOpExp (SMin Int64) e1 e2

invPerm :: [Int] -> [Int]
invPerm xs =
  map f [0..length xs-1]
  where
    f i | Just ind <- L.elemIndex i xs = ind
    f _ = error "Violation of assumed permutation semantics"

