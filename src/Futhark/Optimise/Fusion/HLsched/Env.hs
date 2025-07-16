{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched.Env
  ( FuseEnv (..)
  , FwdTileTab
  , InvTileTab
  , Env (..)
  , emptyEnv
  , addTileBinding
  , addTransf2Env
  , addBinOp2Env
  , tileFuns
  , expandSE
  , eqPEs
  , addPes
  , mulPes
  , minPes
  , invPerm
  , getStmNode
  , findType
  , findPType
  , peFromSe
  , addInpDeps2Env
  )
where

--import Control.Monad
import Data.List qualified as L
import Data.Maybe
import Data.Map.Strict qualified as M
import Data.Graph.Inductive.Graph qualified as G
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.Tools
import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Optimise.Fusion.GraphRep

--import Debug.Trace

-----------------------------------
--- names of special functions
-----------------------------------

tileFuns :: [String]
tileFuns = ["strip1", "strip2"]


-----------------------------------
--- Environment
-----------------------------------

data FuseEnv m = FEnv
  {  fuseInLambda:: (LocalScope SOACS m, MonadFreshNames m) =>
                     Lambda SOACS -> m (Lambda SOACS, Bool)
  }
-- dummy is: (\lam -> pure (lam, False))

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

instance Pretty FwdTileTab where
  pretty = pretty . M.toList

instance Pretty InvTileTab where
  pretty = pretty . M.toList

data Env = Env
  { appTilesFwd :: FwdTileTab,
    appTilesInv :: InvTileTab,
    iotas       :: M.Map VName (SubExp, PrimExp VName), -- , Stms SOACS
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

-------------------------------------------------------
--- Simple Utilities
-------------------------------------------------------

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

getStmNode :: NodeT -> Maybe (Stm SOACS)
getStmNode (StmNode stm) = Just stm
getStmNode _ = Nothing

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

peFromSe :: Env -> PrimType -> SubExp -> PrimExp VName
peFromSe _ _ (Constant pv) = ValueExp pv
peFromSe env ptp (Var vnm) =
  case M.lookup vnm (scalars env) of
    Just pe -> pe
    Nothing -> LeafExp vnm ptp

-------------------------------------------------------------------------
--- Adding bindings to env for SOAC deps caused by iotas and scalars
-------------------------------------------------------------------------

addInpDeps2Env ::
  Env ->
  DepNode ->
  DepGraph ->
  Env
addInpDeps2Env env0 (nId, _nT) DepGraph{dgGraph = g} = do
  -- SoacNode{} <- soac_nT,
  let (_out_deps, _, _, inp_deps) = G.context g nId
  foldl addInpDep env0 inp_deps
  where
    se0 = Constant $ IntValue $ Int64Value 0
    se1 = Constant $ IntValue $ Int64Value 1
    i64ptp = IntType Int64
    --
    addInpDep env edgeid =
      let (_, _, node, inp_deps0) = G.context g (snd edgeid)
          inp_deps = filter (isInd . fst) inp_deps0
          env' = foldl addInpDep env inp_deps
      in case getStmNode node of
           Just (Let pat _aux (Apply fnm arg_diets _ _)) ->
             processTileFCall env' (pat, fnm, arg_diets)
           Just (Let (Pat [pat_el]) _aux e) ->
             processExp env' (patElemName pat_el, patElemDec pat_el) e
           _ -> env'
    --
    processTileFCall env (pat, fnm, arg_diets)
      | any (`L.isPrefixOf` (nameToString fnm)) tileFuns,
        [(size_se, _)] <- arg_diets,
        ptp:_ <- map (elemType . patElemDec)  $ patElems pat =
      addTileBinding env (peFromSe env ptp size_se) $
        map patElemName $ patElems pat
    processTileFCall env _ = env
    --
    processExp env (pat_nm, _) (BasicOp (Iota w start stride Int64))
      | start == se0 && stride == se1 =
      let pe_w = peFromSe env i64ptp w
      in  env { iotas = M.insert pat_nm (w, pe_w) (iotas env) }
    processExp env (pat_nm, Prim ptp) (BasicOp (BinOp bop s1 s2)) =
      let (p1, p2) = (peFromSe env ptp s1, peFromSe env ptp s2)
      in  env { scalars = M.insert pat_nm (BinOpExp bop p1 p2) (scalars env) }
    processExp env (pat_nm, Prim ptp) (BasicOp (UnOp unop se)) =
      let pe = peFromSe env ptp se
      in  env { scalars = M.insert pat_nm (UnOpExp unop pe) (scalars env) }
    processExp env _ _ = env

