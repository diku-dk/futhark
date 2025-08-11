{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched
  ( applyHLsched
  )
where

--import Data.List qualified as L
import Control.Monad
--import Data.Graph.Inductive.Graph qualified as G
--import Data.Map.Strict qualified as M
--import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
import Futhark.Optimise.Fusion.GraphRep
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Transform.Substitute
--import Futhark.Analysis.PrimExp
import Futhark.Optimise.Fusion.HLsched.Env
import Futhark.Optimise.Fusion.HLsched.SchedUtils
import Futhark.Optimise.Fusion.HLsched.Stripmine
import Futhark.Optimise.Fusion.HLsched.Permute
--import Futhark.Util.Pretty hiding (line, sep, (</>))
-- import Futhark.Analysis.PrimExp.Convert

import Debug.Trace

-----------------------------------
--- core function
-----------------------------------

-- Redundant: HasScope SOACS m, 

applyHLsched ::
  (LocalScope SOACS m, MonadFreshNames m) =>
  (Lambda SOACS -> m (Lambda SOACS, Bool)) ->
  DepNode ->
  DepGraph ->
  m (Maybe DepGraph)
applyHLsched fuseInLam node_to_fuse dg = do
  let fenv = FEnv fuseInLam
  mb_sched <- parseHLSched node_to_fuse dg
  case mb_sched of
    Just (env0, soac_node@(_, SoacNode out_trsfs soac_pat soac soac_aux), sched, patel_sched) -> do
      let (_valid_soac, exact_result) = isSupportedSOAC soac
          env = addInpDeps2Env env0 soac_node dg
      stripmined_soac <- applyStripmining fenv env sched (soac_pat, soac_aux, soac)
      _permuted_res_stms <- applyPermute fenv env sched soac_aux stripmined_soac
      (_prologue, _epilogue) <-
        if exact_result
        then pure (mempty,mempty)
        else do
          let [soac_res] = patElems soac_pat
          mkProEpilogue soac_res out_trsfs sched patel_sched
      --
      trace ("\nFwdTileTab: " ++ prettyString (appTilesFwd env) ++ 
             "\nInvTileTab: " ++ prettyString (appTilesInv env) ++
             "\nHL-Sched: "++prettyString sched++
             "\nSOAC: out-transfs: "++show out_trsfs++
             " pattern: "++prettyString soac_pat++" = "++
             "\n" ++ prettyString soac
            ) $ pure Nothing
    _ -> pure Nothing


isSupportedSOAC :: H.SOAC SOACS -> (Bool, Bool)
isSupportedSOAC soac
  | H.Screma _ _ form <- soac,
    ScremaForm _ [] [] <- form = do
    (True, False)
isSupportedSOAC _ = (False, False)


-------------------------------------------------------------------------
-------------------------------------------------------------------------

-- | Fusion entrypoint for permuting the dimensions of a SOAC nest,
--     i.e., HORep.SOAC argument
applyPermute :: 
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched -> StmAux (ExpDec SOACS) ->
    Maybe (H.SOAC SOACS) -> m (Stms SOACS)
applyPermute fenv env sched aux msoac
  | Just (H.Screma mm inps (H.ScremaForm map_lam [] reduces)) <- msoac,
    emptyOrFullyConsumedReduce map_lam reduces,
    not (any hasTransform inps) = do
  -- ^ ToDo: please relax the input-transforms later
  let soac = F.Screma mm (map nameFromInp inps) $ F.ScremaForm map_lam [] reduces
      inp_soac_type = findSOACtype soac
  strip_params <- forM inp_soac_type $ \ tp -> newParam ("tmp_stripmined_res") tp
  let strip_tmp_pat = Pat $ map (\ (Param _ nm dec) -> PatElem nm dec) strip_params
      sched' = sched { sigma = invPerm (sigma sched) }
  perm_soac_stms <- permuteNest fenv env sched' $ Let strip_tmp_pat aux $ Op soac
  trace ( "Stripmined soac:\n" ++ prettyString soac ++
          "Permuted   soac:\n" ++ prettyString perm_soac_stms
        ) $
      pure perm_soac_stms
  where
    hasTransform (H.Input transf _ _) = not $ H.nullTransforms transf
    nameFromInp (H.Input _ nm _) = nm
    --
    emptyOrFullyConsumedReduce _map_lam [] = True
    emptyOrFullyConsumedReduce map_lam reduces
      | Just _ <- oneFullyConsumedMapRed map_lam reduces = True
    emptyOrFullyConsumedReduce _ _ = False
    --
    oneFullyConsumedMapRed map_lam [Reduce _ red_lam _]
      | lambdaReturnType red_lam == lambdaReturnType map_lam =
      Just red_lam
    oneFullyConsumedMapRed _ _ = Nothing
    --
    findSOACtype (F.Screma mm _inp_nms (F.ScremaForm map_lam [] [])) =
      map (`arrayOfRow` mm) $ lambdaReturnType map_lam
    findSOACtype (F.Screma _mm _inp_nms (F.ScremaForm map_lam [] reduces))
      | Just red_lam <- oneFullyConsumedMapRed map_lam reduces = lambdaReturnType red_lam
    findSOACtype soac =
      error ("Unsupported soac in function findType of HLSched: "++prettyString soac)
--
--
applyPermute _ _ _ _ Nothing     =
  error ("Illegal (Nothing) argument to applyPermute in HLSched, i.e., stripmining must have failed!")
applyPermute _ _ _ _ (Just hsoac)     =
  error ("Unsupported argument to applyPermute in HLsched:\n"++prettyString hsoac)

-------------------------------------------------------------------------
-------------------------------------------------------------------------

mkProEpilogue ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    PatElem (LetDec SOACS) ->
    H.ArrayTransforms ->
    HLSched ->
    PatElem (LetDec SOACS) ->
    m (Stms SOACS, Stms SOACS)
mkProEpilogue _patel_res _out_trsfs _sched _patel_sched = do
  pure (mempty, mempty)



-------------------------------------------------------------------------
--- Previous code
-------------------------------------------------------------------------

{--  
    sched_nodeT <- snd node_to_fuse,
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
  (env, lmad_off_pe) <- expandScalar env0 dg inp_deps offs
  trace ("lmad offset is: "++ prettyString lmad_off_pe ++ 
         "\n\t FwdTileTab: " ++ prettyString (appTilesFwd env) ++ 
         "\n\t InvTileTab: " ++ prettyString (appTilesInv env)
        ) $
    pure Nothing
  where
    getNodeTfromCtx (_, _, nT, _) = nT

--
applyHLsched _ _ = do
  pure Nothing
--}

