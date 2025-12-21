{-# LANGUAGE Strict #-}

-- | This module contains the definition of the
--   Top-Down Environment of Schedules and related
--   utility functions.
module Futhark.Optimise.Schedules.EnvUtils
  ( FwdTileTab
  , InvTileTab
  , TopEnv (..)
  , freshTopEnv
  , addTileBinding
  , addIota2Env
  , addTransf2Env
  , addBinOp2Env
  , tileFuns
  , expandSE
  , eqPEs
  , addPes
  , mulPes
  , minPes
  , invPerm
  , identityPerm
  , findType
  , findPType
  , peFromSe
  )
where

import Data.List qualified as L
import Data.Maybe
import Data.Map.Strict qualified as M
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.Tools

--import Debug.Trace

-----------------------------------
--- names of special functions  ---
-----------------------------------

tileFuns :: [String]
tileFuns = ["strip1", "strip2"]


-----------------------------------
--- Environment
-----------------------------------

-- type LMAD = LMAD.LMAD (TPrimExp Int64 VName)

-- | Binds a size expression to its tiled
--     approximation: `e -> [t1,...,tm]`,
--     where `t1 * ... * tm` is an upper
--     bound of `e`, i.e., `e <= t1*...*tm`
--   E.g., `let (m, t1, t2) = strip2 (2*M)`
--     produces `2*M -> [m, t1, t2]`
--   ToDo:  maybe use the Poly IR here?
type FwdTileTab = M.Map (PrimExp VName) Names

-- ^ Binds a symbolic tile names to a tuple:
--     1. the original size that was tiled
--     2. the list containing the produced tiles.
--   E.g., `let (m, t1, t2) = strip2 (2*M)`
--     produces `m -> (2*M, [m, t1, t2])`
type InvTileTab = M.Map VName (PrimExp VName, Names)

data TopEnv = TopEnv
  { bdy_res     :: [SubExp]
  , appTilesFwd :: FwdTileTab
  , appTilesInv :: InvTileTab
  , iotas       :: M.Map VName (SubExp, PrimExp VName) -- , Stms SOACS
    -- ^ binds the name of an iota array to its size
  , inv_iotas   :: M.Map (PrimExp VName) (VName, SubExp)
    -- ^ binds a size pexp to the name of a iota and its SubExp size
  , scalars     :: M.Map VName (PrimExp VName)
    -- ^ binds the name of a scalar var to its expression
  , arrayLits   :: M.Map VName [PrimExp VName]
    -- ^ binds the name of an array literal to its expression 
  , arrTransf   :: M.Map VName (VName, [Int])
    -- ^ Models a chain of rearrange transformations:
    --   binds the name of an array to (1) the name of its base array,
    --   and (2) the complete permutation
  } deriving Show

freshTopEnv :: TopEnv
freshTopEnv = TopEnv [] mempty mempty mempty mempty mempty mempty mempty

addIota2Env :: TopEnv -> (VName, SubExp, PrimExp VName) -> TopEnv
addIota2Env env (iotnm, w_se, w_pe) =
  env { iotas     = M.insert iotnm (w_se,  w_pe) (iotas env)
      , inv_iotas = M.insert w_pe  (iotnm, w_se) (inv_iotas env) }

addTileBinding :: TopEnv -> PrimExp VName -> [VName] -> TopEnv
addTileBinding env pe tile_nms =
  let nms = namesFromList tile_nms
      fwdenv = M.insert pe nms $ appTilesFwd env
      bwdenv = foldl (foldfun nms) (appTilesInv env) tile_nms
  in  env { appTilesFwd = fwdenv, appTilesInv = bwdenv }
  where
    foldfun nms env_cur nm = M.insert nm (pe, nms) env_cur 

addTransf2Env :: TopEnv -> VName -> VName -> [Int] -> TopEnv
addTransf2Env env nm_new nm_old perm_new =
  let (nm_old', perm') =
       case M.lookup nm_old (arrTransf env) of
         Nothing  -> (nm_old, perm_new)
         Just (nm_old0, perm_old) ->
           (nm_old0, composePerms perm_new perm_old)
  in env { arrTransf = M.insert nm_new (nm_old', perm') (arrTransf env) }

expandSE :: TopEnv -> SubExp -> PrimType -> PrimExp  VName
expandSE _ (Constant pval) _ = ValueExp pval
expandSE env (Var vnm) ptp = fromMaybe (LeafExp vnm ptp) $ M.lookup vnm $ scalars env

addBinOp2Env :: TopEnv -> (VName, Type) -> BinOp -> SubExp -> SubExp -> TopEnv
addBinOp2Env env (nm,tp) bop s1 s2 =
  env { scalars = M.insert nm (BinOpExp bop (expSE s1) (expSE s2)) (scalars env) }
  where expSE se = expandSE env se $ elemType tp

-------------------------------------------------------
--- Simple Utilities
-------------------------------------------------------

identityPerm :: [Int] -> Bool
identityPerm [] = True
identityPerm perm = perm == [0 .. length perm - 1]

invPerm :: [Int] -> [Int]
invPerm xs =
  map f [0..length xs-1]
  where
    f i | Just ind <- L.elemIndex i xs = ind
    f _ = error "Violation of assumed permutation semantics"

composePerms :: [Int] -> [Int] -> [Int]
composePerms perm_new perm_old = 
  map (\ i -> perm_old !! i) perm_new

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

findType :: (HasScope SOACS m) => SubExp -> m Type
findType (Constant pval) = pure $ Prim $ primValueType pval
findType (Var vnm) = lookupType vnm

findPType :: (HasScope SOACS m, Monad m) => SubExp -> m PrimType
findPType se = do
  tp <- findType se
  case tp of
    Prim ptp -> pure ptp
    _ -> error ("In findPType subexp: " ++ prettyString se ++
                " has type "++prettyString tp++", which is not PrimType")

peFromSe :: TopEnv -> PrimType -> SubExp -> PrimExp VName
peFromSe _ _ (Constant pv) = ValueExp pv
peFromSe env ptp (Var vnm) =
  case M.lookup vnm (scalars env) of
    Just pe -> pe
    Nothing -> LeafExp vnm ptp

