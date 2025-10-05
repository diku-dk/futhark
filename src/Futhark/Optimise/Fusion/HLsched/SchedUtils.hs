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
  , parMode
  , mkRegIndStrides
  , identityPerm
  , headOfSched
  , tailOfSched
  , sortByPerm
  , append2Sched
  , oneFullyConsumedMapRed
  , equivLambdas
  , fromFParam
  , toFParam
  )
where

import Data.List qualified as L
--import Control.Monad
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
--    3 -> Flip
    _ -> error "Impossible case reached!"

mkRegIndStrides :: HLSched -> [PrimExp VName]
mkRegIndStrides sched =
  let modes = map parMode $ signals sched
      mul_carries = scanl f pe1 $ zip modes (dimlens sched)
      strides = reverse $ scanl mulPes pe1 $ reverse (dimlens sched)
  in  map g $ zip3 modes mul_carries strides
  where
    pe1 = ValueExp $ IntValue $ Int64Value 1
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
  m (Maybe (Env, (Int, NodeT), HLSched, PatElem (LetDec SOACS)))
parseHLSched (sched_nId, sched_nT) dg@DepGraph {dgGraph = g}
  | StmNode stmt <- sched_nT,
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
    sched_ctx <- G.context g sched_nId,
    (_out_deps, _, _, inp_deps) <- sched_ctx,
    indrc_deps <- filter (isInd . fst) inp_deps,
    indrc_ctxs <- map (G.context g . snd) indrc_deps,
    indrc_ndTs <- map getNodeIdTfromCtx indrc_ctxs,
    ([(soac_nId, soac_nT)], _arrlit_nTs) <- L.partition (isSOACNodeT . snd) indrc_ndTs,
    SoacNode node_out_trsfs soac_pat _soac _soac_aux <- soac_nT,
    H.nullTransforms node_out_trsfs,
    -- ^ simplification: no out-transform supported so far
    [soac_patel] <- patElems soac_pat,
    -- ^ simplification: soac must return one result
    soac_res == Var (patElemName soac_patel)
    -- ^ the argument of the HL-Sched must be the SOAC's result
  = do
  ptp_offs <- findPType offs
  let env = addInpDeps2Env emptyEnv (sched_nId, sched_nT) dg
      lmad_off_pe = peFromSe env ptp_offs offs
      signals_pes = parseArrLitEdge env dg inp_deps signals_arg
      origids_pes = parseArrLitEdge env dg inp_deps orig_arg
      sigma_pes   = parseArrLitEdge env dg inp_deps sigma_arg
      dimlens_pes = parseArrLitEdge env dg inp_deps ns_arg
      strides_pes = parseArrLitEdge env dg inp_deps strides_arg
      --
      sched = HLS { offset  = lmad_off_pe
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
  pure $ Just (env, (soac_nId, soac_nT), sched, pat_el)
  where
    getNodeIdTfromCtx (_, nId, nT, _) = (nId, nT)
    toArrInt ct_pes = map toInt ct_pes
    toInt :: PrimExp VName -> Int
    toInt (ValueExp (IntValue iv)) = valueIntegral iv
    toInt pe = error ("This was supposed to be a constant int value; instead is "++prettyString pe)
--
parseHLSched _ _ = do
  pure Nothing


parseArrLitEdge :: Env -> DepGraph -> [(EdgeT,Int)] -> SubExp -> [PrimExp VName]
parseArrLitEdge env DepGraph{dgGraph = g} inp_deps (Var arr_nm)
  | Just edgeid <- L.find (\e -> arr_nm == getName (fst e)) inp_deps,
    edge_ctx  <- G.context g (snd edgeid),
    (_, _, node, _inp_deps') <- edge_ctx,
    Just stm <- getStmNode node,
    Let pat _aux (BasicOp (ArrayLit arg_ses _arr_tp)) <- stm,
    [pat_el] <- patElems pat = do
  map (peFromSe env (elemType $ patElemDec pat_el)) arg_ses
--
parseArrLitEdge _ _ _ _ =
  error "Something went wrong in parsing an array literal (part of HLsched)"

-------------------------------
--- Other Utility Functions ---
-------------------------------

identityPerm :: [Int] -> Bool
identityPerm [] = True
identityPerm perm = perm == [0 .. length perm - 1]

nullSched :: HLSched -> Bool
nullSched sched =
  null (dimlens sched) || null (strides sched) ||
  null (origids sched) || null (signals sched) || null (sigma sched)

headOfSched :: HLSched -> (PrimExp VName, PrimExp VName, Int, Int, Int)
headOfSched sched
  | nullSched sched =
    error ("Illegal Schedule passed as argument to headOfSched: "++prettyString sched)
headOfSched sched =
  ( head (dimlens sched)
  , head (strides sched)
  , head (origids sched)
  , head (sigma   sched)
  , head (signals sched)
  )

tailOfSched :: HLSched -> HLSched
tailOfSched sched
  | nullSched sched =
    error ("Illegal Schedule passed as argument to tailOfSched: "++prettyString sched)
tailOfSched sched =
  sched { dimlens = tail (dimlens sched)
        , strides = tail (strides sched)
        , origids = tail (origids sched)
        , sigma   = tail (sigma   sched)
        , signals = tail (signals sched)
        }

sortByPerm :: HLSched -> HLSched
sortByPerm sched =
  let lst = L.zip5 (dimlens sched) (strides sched) (origids sched) (sigma sched) (signals sched)
      (lens, strds, oids, sigm, signs)= L.unzip5 $ L.sortBy sortGT lst
  in  sched { dimlens = lens, strides = strds, origids = oids, sigma = sigm, signals = signs }
  where
    sortGT (_,_,_,p1,_) (_,_,_,p2,_)
      | p1 < p2 = LT
      | p1 > p2 = GT
      | True = GT

append2Sched :: (PrimExp VName, PrimExp VName, Int, Int, Int) -> HLSched -> HLSched
append2Sched (l, s, o, p, d) sched =
  sched { dimlens = l : (dimlens sched)
        , strides = s : (strides sched)
        , origids = o : (origids sched)
        , sigma   = p : (sigma   sched)
        , signals = d : (signals sched)
        }

oneFullyConsumedMapRed :: ScremaForm SOACS -> Maybe (Lambda SOACS)
oneFullyConsumedMapRed (ScremaForm map_lam [] [Reduce _com red_lam _ne])
  | lambdaReturnType red_lam == lambdaReturnType map_lam = Just red_lam
oneFullyConsumedMapRed _ = Nothing

-- | ToDo: extend for map-nest on top of the same binary operator.
equivLambdas :: (Lambda SOACS) -> (Lambda SOACS) -> Bool 
equivLambdas lam1 lam2
  | lambdaReturnType lam1 == lambdaReturnType lam2,
    [Let (Pat [pat1]) _ (BasicOp (BinOp bop1 se11 se12))] <- stmsToList $ bodyStms $ lambdaBody lam1,
    [Let (Pat [pat2]) _ (BasicOp (BinOp bop2 se21 se22))] <- stmsToList $ bodyStms $ lambdaBody lam1,
    [se11, se12] == map (Var . paramName) (lambdaParams lam1),
    [se21, se22] == map (Var . paramName) (lambdaParams lam2) =
  let res12  = concat $ map (bodyResult . lambdaBody) [lam1, lam2]
      res12' = map (subExpRes . Var . patElemName) [pat1, pat2]
  in  bop1 == bop2 && res12 == res12' 
equivLambdas _ _ = False

toFParam :: LParam SOACS -> FParam SOACS
toFParam p = Param (paramAttrs p) (paramName p) $ toDecl (paramDec p) Unique

fromFParam :: FParam SOACS -> LParam SOACS
fromFParam p = Param (paramAttrs p) (paramName p) $ fromDecl (paramDec p)

-----------------------------------------------------------
--  GARBAGE CODE BELOW, i.e., replaced with simpler one
-----------------------------------------------------------
--  Parsing Scalars, i.e., creating PrimExp for
--     them by following the program dependencies
--  Similar for array literals.
-----------------------------------------------------------

{-- -- from the implem of `parseHLSched`
  let env0 = emptyEnv
  (_,    signals_pes) <- expandSymArrLit env0 dg inp_deps signals_arg
  (_,    origids_pes) <- expandSymArrLit env0 dg inp_deps orig_arg
  (_,    sigma_pes  ) <- expandSymArrLit env0 dg inp_deps sigma_arg
  -- 
  (env1, lmad_off_pe) <- expandScalar env0 dg inp_deps offs
  (env2, strides_pes) <- expandSymArrLit env1 dg inp_deps strides_arg
  (env , dimlens_pes) <- expandSymArrLit env2 dg inp_deps ns_arg
--}


{--
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
--}
