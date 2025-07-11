{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched.SchedUtils
  ( HLSched (..)
  , ParMode (..)
  , splitAtSched
  , parseHLSched
  , expandScalar
  , parMode
  , eqPEs
  , addPes
  , mulPes
  , minPes
  , invPerm
  , mkRegIndStrides
--  , getStmNode
  )
where

import Data.List qualified as L
import Control.Monad
import Data.Graph.Inductive.Graph qualified as G
--import Data.Map.Strict qualified as M
--import Data.Maybe
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
import Futhark.Optimise.Fusion.HLsched.Env

--import Debug.Trace

-----------------------------------
--- names of special functions
-----------------------------------

tileFuns :: [String]
tileFuns = ["strip1", "strip2"]

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

splitAtSched :: Int -> HLSched -> (HLSched, HLSched)
splitAtSched k sched =
  let (dimlens', dimlens'') = splitAt k (dimlens sched)
      (strides', strides'') = splitAt k (strides sched)
      (origids', origids'') = splitAt k (origids sched)
      (signals', signals'') = splitAt k (signals sched)
      (sigma',   sigma''  ) = splitAt k (sigma   sched)
  in ( HLS (offset sched) dimlens'  strides'  origids'  sigma'  signals'
     , HLS (offset sched) dimlens'' strides'' origids'' sigma'' signals''
     )

instance Pretty HLSched where
  pretty sched =
    "{\n\tOffest: " <+> pretty (offset  sched) <> 
    "\n\tDimLens: " <+> pretty (dimlens sched) <>
    "\n\tOrigIds: " <+> pretty (origids sched) <>
    "\n\tDimPerm: " <+> pretty (sigma   sched) <>
    "\n\tSignals: " <+> pretty (signals sched) <>
    "\n\tStrides: " <+> pretty (strides sched) <>
    "   }"

data ParMode = Par | Macc | Seq | Flip deriving Eq

parMode :: Int -> ParMode
parMode signal =
  case signal `rem` 4 of
    0 -> Par
    1 -> Macc
    2 -> Seq
    3 -> Flip
    _ -> error "Impossible case reached!"

mkRegIndStrides :: HLSched -> [PrimExp VName]
mkRegIndStrides sched =
  let modes = map parMode $ signals sched
      mul_carries = scanl f pe1 $ zip modes (dimlens sched)
      strides = reverse $ scanl mulPes pe1 $ reverse (dimlens sched)
  in  map g $ zip3 modes mul_carries strides
  where
    f carry (Flip, n) = mulPes n carry
    f carry _ = carry
    g (Flip, carry, _) = carry
    g (_, carry, strd) = mulPes strd carry
 
--------------------------------------------------------
--- Parsing High-Level Schedule, by pattern matching
---   special-function calls and filling in the
---   HLSched data-structure.
--------------------------------------------------------

parseHLSched ::
  (HasScope SOACS m, Monad m) =>
  DepNode ->
  DepGraph ->
  m (Maybe (Env, NodeT, HLSched, PatElem (LetDec SOACS)))
parseHLSched node_to_fuse dg@DepGraph {dgGraph = g}
  | sched_nodeT <- snd node_to_fuse,
    sched_node_id <- nodeFromLNode node_to_fuse,
    StmNode stmt <- sched_nodeT,
    Let pat _aux e <- stmt,
    [pat_el] <- patElems pat,
    Apply fnm args_diet _rtp _ <- e,
    L.isPrefixOf "hlSched2D" $ nameToString fnm,
    (args, _diet) <- unzip args_diet,
    strides_arg : signals_arg : sigma_arg : orig_arg : ns_arg : offs : soac_res : _ <- L.reverse args,
--    trace ("Debug HLSched: "++ prettyString fnm ++
--           "\n\t res-arr:  "++ prettyString (patElemName pat_el) ++
--           "\n\t res_type: "++ prettyString (patElemDec  pat_el) ++
--           "\n\targs: "++prettyString args ++ 
--           "\n\t stmt: "++prettyString stmt ++
--           "\n\t soac res name: "++ prettyString soac_res
--          ) True,
    --
    sched_ctx <- G.context g sched_node_id,
    (_out_deps, _, _, inp_deps) <- sched_ctx,
    indrc_deps <- filter (isInd   . fst) inp_deps,
    indrc_ctxs <- map (G.context g . snd) indrc_deps,
    indrc_ndTs <- map getNodeTfromCtx indrc_ctxs,
    ([soac_nT], _arrlit_nTs) <- L.partition isSOACNodeT indrc_ndTs,
    SoacNode node_out_trsfs soac_pat _soac _soac_aux <- soac_nT,
    H.nullTransforms node_out_trsfs,
    -- ^ simplification: no out-transform supported so far
    [soac_patel] <- patElems soac_pat,
    -- ^ simplification: soac must return one result
    soac_res == Var (patElemName soac_patel)
    -- ^ the argument of the HL-Sched must be the SOAC's result
--    stmts <- mapMaybe getStmNode arrlit_nTs
--    trace ("soac:\nlet " ++ prettyString soac_pat ++ " inp-deps: "++show inp_deps) True,
--    trace ("other " ++ show (length stmts) ++ " stms:\n" ++ prettyString stmts) True
    -- trace("indrct-deps: "++show indrc_deps) True
    --trace("graph: "++show g) True
  = do
  -- scope <- askScope
  let env0 = emptyEnv
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
--  trace ("\nFwdTileTab: " ++ prettyString (appTilesFwd env) ++ 
--         "\nInvTileTab: " ++ prettyString (appTilesInv env) ++
--         "\nHLSchedule: " ++ prettyString sched
----         "\n\t Dimlens: " ++ prettyString dimlens_pes ++
----         "\n\t Strides: " ++ prettyString strides_pes
--        ) $
  pure $ Just (env, soac_nT, sched, pat_el)
  where
    getNodeTfromCtx (_, _, nT, _) = nT
    toArrInt ct_pes = map toInt ct_pes
    toInt :: PrimExp VName -> Int
    toInt (ValueExp (IntValue iv)) = valueIntegral iv
    toInt pe =
      error ("This was supposed to be a constant int value; instead is "++prettyString pe)
      
parseHLSched _ _ = do
  pure Nothing

--------------------------------------------------
--- Parsing Scalars, i.e., creating PrimExp for
--     them by following the program dependencies
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
        (env1, pexp1) <- expandScalar env  dg inp_deps' se1
        (env', pexp2) <- expandScalar env1 dg inp_deps' se2
        pure $ (env', BinOpExp bop pexp1 pexp2)
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
