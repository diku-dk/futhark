{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched.Utils
  ( FwdTileTab
  , InvTileTab
  , Env (..)
  , HLSched (..)
  , addTileBinding
  , getStmNode
  , expandScalar
  , parseHLSched
  )
where

import Data.List qualified as L
import Control.Monad
import Data.Graph.Inductive.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
-- import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
-- import Futhark.IR.SOACS qualified as F
import Futhark.Optimise.Fusion.GraphRep
import Futhark.Tools
-- import Futhark.Transform.Rename
-- import Futhark.Transform.Substitute
-- import Futhark.Analysis.PrimExp
import Futhark.Util.Pretty hiding (line, sep, (</>))
-- import Futhark.Analysis.PrimExp.Convert

import Debug.Trace

-----------------------------------
--- names of special functions
-----------------------------------

tileFuns :: [String]
tileFuns = ["strip1", "strip2"]

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
    appTilesInv :: InvTileTab
  } deriving Show

addTileBinding :: Env -> PrimExp VName -> [VName] -> Env
addTileBinding env pe tile_nms =
  let nms = namesFromList tile_nms
      fwdenv = M.insert pe nms $ appTilesFwd env
      bwdenv = foldl (foldfun nms) (appTilesInv env) tile_nms
  in  Env { appTilesFwd = fwdenv, appTilesInv = bwdenv }
  where
    foldfun nms env_cur nm = M.insert nm (pe, nms) env_cur

instance Pretty FwdTileTab where
  pretty = pretty . M.toList

instance Pretty InvTileTab where
  pretty = pretty . M.toList

-------------------------------------------------------
--- LMAD-Like Representation of a High-Level Schedule
-------------------------------------------------------

data HLSched = HLS
  { offset  :: PrimExp VName
  , dimlens :: [PrimExp VName]
  , strides :: [PrimExp VName]
  , origids :: [Int]
  , sigma   :: [Int]
  , signals :: [Int]
  }

instance Pretty HLSched where
  pretty sched =
    "{\n\tOffest: " <+> pretty (offset  sched) <> 
    "\n\tDimLens: " <+> pretty (dimlens sched) <>
    "\n\tOrigIds: " <+> pretty (origids sched) <>
    "\n\tDimPerm: " <+> pretty (sigma   sched) <>
    "\n\tSignals: " <+> pretty (signals sched) <>
    "\n\tStrides: " <+> pretty (strides sched) <>
    "   }"

-------------------------------------------------------
--- Simple Utilities
-------------------------------------------

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

--------------------------------------------------
--- Parsing Scalars, i.e., creating PrimExp for
--    them by following the program dependencies
--  Similar for array literals.
--------------------------------------------------

expandScalar :: (HasScope SOACS m, Monad m) =>
    Env -> DepGraph -> [(EdgeT,Int)] -> SubExp -> m (Env, PrimExp VName)
expandScalar env _ _ (Constant pval) =
  pure $ (env, ValueExp pval)
expandScalar env _ inp_deps (Var nm)
  | Nothing <- L.find (\ e -> nm == getName (fst e)) inp_deps = do
  ptp <- findPType (Var nm)
  pure $ (env, LeafExp nm ptp)
expandScalar env dg@DepGraph{dgGraph = g} inp_deps (Var nm) 
  | Just edgeid <- L.find (\e -> nm == getName (fst e)) inp_deps,
    edge_ctx  <- G.context g (snd edgeid),
    (_, _, node, inp_deps') <- edge_ctx,
    Just stm <- getStmNode node,
    (Let pat _aux e) <- stm = do
    case e of
      (Apply fnm arg_diets _ _) ->
        expandScalarFunCall env dg inp_deps' (pat, fnm, arg_diets) nm
      (BasicOp (BinOp bop se1 se2)) -> do
        (env1, pe1) <- expandScalar env  dg inp_deps' se1
        (env', pe2) <- expandScalar env1 dg inp_deps' se2
        pure $ (env', BinOpExp bop pe1 pe2)
      (BasicOp (UnOp unop se)) -> do
        (env', pe) <- expandScalar env dg inp_deps' se
        pure $ (env', UnOpExp unop pe)
      _ -> error ("Unimplemented case in `expandScalar` for stm: "++prettyString stm)
--
expandScalar _ _ _ (Var nm) =
  error ("In function expandScalar: should not reach here, var-name: " ++ prettyString nm)

expandScalarFunCall :: (HasScope SOACS m, Monad m) =>
    Env ->
    DepGraph ->
    [(EdgeT,Int)] ->
    (Pat (LetDec SOACS), Name, [(SubExp, Diet)]) ->
    VName ->
    m (Env, PrimExp VName)
expandScalarFunCall env dg inp_deps (pat, fnm, arg_diets) nm
  | any (`L.isPrefixOf` (nameToString fnm)) tileFuns,
    -- ^ handling of special tiling functions
    [(size_se, _)] <- arg_diets,
    res_nms <- map patElemName $ patElems pat,
    Just _ <- L.find (== nm) res_nms = do
  ptp <- findPType (Var nm)
  (env', size_pe) <- expandScalar env dg inp_deps size_se
  let env'' = addTileBinding env' size_pe res_nms
  pure (env'', LeafExp nm ptp)
expandScalarFunCall env _ _ _ nm = do
  -- ^ no interprocedural support for regular function calls
  ptp <- findPType (Var nm)
  pure (env, LeafExp nm ptp)

expandSymArrLit :: (HasScope SOACS m, Monad m) =>
    Env -> DepGraph -> [(EdgeT,Int)] -> SubExp -> m (Env, [PrimExp VName])
expandSymArrLit env dg@DepGraph{dgGraph = g} inp_deps (Var arr_nm)
  | Just edgeid <- L.find (\e -> arr_nm == getName (fst e)) inp_deps,
    edge_ctx  <- G.context g (snd edgeid),
    (_, _, node, inp_deps') <- edge_ctx,
    Just stm <- getStmNode node,
    (Let pat _aux e) <- stm = do
    case e of
      (BasicOp (ArrayLit arg_ses _arr_tp)) ->
        foldM (foldfun inp_deps') (env, []) arg_ses
      _ -> error ("This was supposed to be an array literal and is not!" ++ 
                  " ArrName: " ++ prettyString arr_nm ++
                  " Pattern: " ++ prettyString pat ++
                  " Expression: " ++ prettyString e)
  where
    foldfun deps (env_c, pes) se = do
      (env1, pe) <- expandScalar env_c dg deps se
      pure (env1, pes++[pe])
--
expandSymArrLit _ _ _ _ =
  error "Something went wrong in parsing an array literal (part of HLsched)"
  -- error "An array literal cannot be inlined in function call"

--------------------------------------------------------
--- Parsing High-Level Schedule, by pattern matching
---   special-function calls and filling in the
---   HLSched data-structure.
--------------------------------------------------------

parseHLSched ::
  (HasScope SOACS m, Monad m) =>
  DepNode ->
  DepGraph ->
  m (Maybe (NodeT, HLSched))
parseHLSched node_to_fuse dg@DepGraph {dgGraph = g}
  | sched_nodeT <- snd node_to_fuse,
    sched_node_id <- nodeFromLNode node_to_fuse,
    StmNode stmt <- sched_nodeT,
    Let pat _aux e <- stmt,
    [pat_el] <- patElems pat, 
    Apply fnm args_diet _rtp _ <- e,
    (args, _diet) <- unzip args_diet,
    strides_arg : signals_arg : sigma_arg : orig_arg : ns_arg : offs : soac_res : _ <- L.reverse args,
    trace ("Debug HLSched: "++ prettyString fnm ++
           "\n\t res-arr:  "++ prettyString (patElemName pat_el) ++
           "\n\t res_type: "++ prettyString (patElemDec  pat_el) ++
           "\n\targs: "++prettyString args ++ 
           "\n\t stmt: "++prettyString stmt ++
           "\n\t soac res name: "++ prettyString soac_res
          ) True,
    --
    sched_ctx <- G.context g sched_node_id,
    (out_deps, _, _, inp_deps) <- sched_ctx,
    indrc_deps <- filter (isInd   . fst) inp_deps,
    indrc_ctxs <- map (G.context g . snd) indrc_deps,
    indrc_ndTs <- map getNodeTfromCtx indrc_ctxs,
    ([soac_nT], arrlit_nTs) <- L.partition isSOACNodeT indrc_ndTs,
    SoacNode node_out_trsfs soac_pat soac _soac_aux <- soac_nT,
    H.nullTransforms node_out_trsfs,
    [soac_patel] <- patElems soac_pat,
    soac_res == Var (patElemName soac_patel),
    stmts <- mapMaybe getStmNode arrlit_nTs,
    trace ("soac:\nlet " ++ prettyString soac_pat ++ " inp-deps: "++show inp_deps) True,
    trace ("other " ++ show (length stmts) ++ " stms:\n" ++ prettyString stmts) True
    -- trace("indrct-deps: "++show indrc_deps) True
    --trace("graph: "++show g) True
  = do
  -- scope <- askScope
  let env0 = Env mempty mempty
  (_,    signals_pes) <- expandSymArrLit env0 dg inp_deps signals_arg
  (_,    origids_pes) <- expandSymArrLit env0 dg inp_deps orig_arg
  (_,    sigma_pes  ) <- expandSymArrLit env0 dg inp_deps sigma_arg
  -- 
  (env1, lmad_off_pe) <- expandScalar env0 dg inp_deps offs
  (env2, strides_pes) <- expandSymArrLit env1 dg inp_deps strides_arg
  (env , dimlens_pes) <- expandSymArrLit env2 dg inp_deps ns_arg
  let sched = HLS { offset  = lmad_off_pe
                  , dimlens = dimlens_pes
                  , strides = strides_pes
                  , origids = toArrInt origids_pes
                  , sigma   = toArrInt   sigma_pes
                  , signals = toArrInt signals_pes
                  }
  trace ("\nFwdTileTab: " ++ prettyString (appTilesFwd env) ++ 
         "\nInvTileTab: " ++ prettyString (appTilesInv env) ++
         "\nHLSchedule: " ++ prettyString sched
--         "\n\t Dimlens: " ++ prettyString dimlens_pes ++
--         "\n\t Strides: " ++ prettyString strides_pes
        ) $
    pure $ Just (soac_nT, sched)
  where
    getNodeTfromCtx (_, _, nT, _) = nT
    toArrInt ct_pes = map toInt ct_pes
    toInt :: PrimExp VName -> Int
    toInt (ValueExp (IntValue iv)) = valueIntegral iv
    toInt pe =
      error ("This was supposed to be a constant int value; instead is "++prettyString pe)
      
parseHLSched _ _ = do
  pure Nothing
