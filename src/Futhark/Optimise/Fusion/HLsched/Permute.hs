{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.Permute
  ( permuteNest
  )
where

import Data.List qualified as L
--import Control.Monad
--import Data.Graph.Inductive.Graph qualified as G
import Data.Sequence qualified as Sq
import Data.Map.Strict qualified as M
--import Data.Set qualified as S
import Data.Maybe
--import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
--import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS
--import Futhark.IR.SOACS qualified as F
--import Futhark.Optimise.Fusion.GraphRep
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Transform.Substitute
--import Futhark.Analysis.PrimExp
import Futhark.Transform.Rename (renameLambda)
import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.Fusion.HLsched.Env
import Futhark.Optimise.Fusion.HLsched.SchedUtils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
--import Futhark.Analysis.PrimExp.Convert
--import Futhark.Optimise.TileLoops.Shared

import Debug.Trace

-- | Assumptions:
--   1. The results of each intermediate recurrence in the nest is
--        either of scalar type or it constitutes the body result
--   2. 
--   ToDo: 
--   1. Add sanity checks, e.g., that the permutation length <= the depth of the soac nest
--   2. Treat special functions such as pad1D as recurrences, e.g., in `separateLastRec`,
--        `mkRecWithBody`, `transposeTopOfNest`
--
permuteNest ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched ->
    Stm SOACS -> m (Stms SOACS)
permuteNest fenv env sched orig_nest_stm@(Let _pat _aux e_soac)
  | identityPerm (sigma sched) = trace ("Permute Base Case (Identity)") $ pure $ oneStm orig_nest_stm
  -- ^ if the permutation is the identity, the task has been accomplished
  --   the empty list is considered identity as well
  --
  | [_] <- sigma sched = trace ("Permute Base Case (Size 1)") $ pure $ oneStm orig_nest_stm
  -- ^ one element schedule is always permuted in the right way
  --
  | not (null (sigma sched)) && 0 == head (sigma sched),
  -- ^ if the first element of the permutation is 0 then it comes
  --   down to permuting the last recurrence of the current soac. 
    Just (code_bef, rec_stm, code_after, res_ses) <- sep3LastRec e_soac = do
    -- Let _pat_rec _aux_rec e_rec <- rec_stm = do
    -- ^ check that either `_pat_rec` is of scalar type or it is not used by code_after
    let sched_tail = tailOfSched sched
        sched' = sched_tail { sigma = map (\x -> x - 1) (sigma sched_tail) }
    last_rec_stms <- trace ("Permute, Case head==0, tail-sched: "++show (sigma sched')++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env sched' rec_stm
    mkRecWithBody orig_nest_stm (code_bef, last_rec_stms, code_after, res_ses) >>= pure . oneStm
    -- ^ this also need to fix the code_after to use the result of last_rec'
    --     which is potentially of a different shape now (?)
    --
  | s0 : s1 : _rest <- sigma sched,
    s0 < s1,
    Just (code_bef, rec_stm, code_after, res_ses) <- sep3LastRec e_soac = do
  -- ^ s0 cannot be directly interchanged. Treated in two steps:
  --   1. build a permutation for (s1:_rest) nest by subtracting 1 from each
  --       element greater than s0; call the resulted permutation `sigma_rec`
  --   2. recursively permute the (s1:_rest) according to `sigma_rec`
  --   3. place s0 at the correct position by recursively applying the
  --        permutation s0 ++ sort(s1:_rest) to the whole nest
    let tail_sched = tailOfSched sched
        sigma_rec = map (\x -> if x > s0 then x-1 else x) (sigma tail_sched)
        sched_rec = tail_sched { sigma = sigma_rec}
    last_rec_stms <- trace ("Permute Case 1.0, s0 < s1, rec-sched: "++show (sigma sched_rec)++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env sched_rec rec_stm
    rec_stm' <- mkRecWithBody orig_nest_stm (code_bef, last_rec_stms, code_after, res_ses)
    --
    let sched' = append2Sched (headOfSched sched) $ sortByPerm $ tailOfSched sched
    trace ("Permute Case 1.1, s0 < s1, repositionaing first dim: "++show (sigma sched')++" orig_sched: "++show (sigma sched)++" sorted sched: "++show (sigma $ sortByPerm $ tailOfSched sched)) $
      permuteNest fenv env sched' rec_stm'
  --
  | s0 : s1 : _rest <- sigma sched,
    s0 > s1 = do
  -- ^ interchange the front dimensions, then call recursively on the top soac
    -- let (code_bef, last_rec, code_after) = sep3LastRec soac
    (before_stms, last_rec_stm, after_stms) <- trace ("Permute Case 2.0, s0 > s1, transpose top"++" orig_sched: "++show (sigma sched)) $
      transposeTopOfNest fenv env sched orig_nest_stm
    last_rec_stms' <- trace ("Permute Case 2.0, s0 > s1, recurse on sched: "++show (sigma (interchangeTop sched))++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env (interchangeTop sched) last_rec_stm
    pure $ before_stms <> last_rec_stms' <> after_stms
    -- ^ something is not OK here: how do you identify the result?
    where
      interchangeTop schd =
        append2Sched (headOfSched (tailOfSched schd)) $
        append2Sched (headOfSched schd) $
        (tailOfSched (tailOfSched schd))
permuteNest _ _ _ nest_stm =
  error ("Unsupported (nest) statement in permuteNest: "++prettyString nest_stm)

--permuteNest fenv env sched (F.Screma mm inps (F.ScremaForm map_lam [] reduces)) = do
--  
--permuteNest _ _ _ soac =
--  error ("permuteNest support only fully-fused map-reduce SOACs; given: "++prettyString soac)


------------------------------------------------------------------
--- Core Function that performs one transposition
------------------------------------------------------------------

type LftTab = M.Map VName (VName, Type)

-- | For now we support only outer maps that are interchanged inwards
--   To be extended later at least with redomap, if not also with loops
transposeTopOfNest :: (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched -> Stm SOACS -> m (Stms SOACS, Stm SOACS, Stms SOACS)
transposeTopOfNest _fenv _env _sched outer_rec_stm
  | Let pat _aux (Op (Screma _m _inp_nms (ScremaForm map_lam [] []))) <- outer_rec_stm,
    (parts, res_ses) <- sep5LastRecBody (lambdaBody map_lam),
    PartStms bef_sched msched aft_sched (Just rec_stm) aft_rec <- parts = do
  let ((inl_nms_bef, inl_stms_bef), (inl_nms_aft, inl_stms_aft)) =
        findCheapStms2Inline (freeIn $ lambdaBody map_lam) parts
      res_nms = namesFromList $ mapMaybe se2VName res_ses
      fv3 = freeInStms aft_rec
      fv2 = fv3 <> freeIn rec_stm
      fv1 = fv2 <> freeInStms aft_sched
      --
      res_nms_bef_sched = namesIntersection (getPatNames bef_sched) $ res_nms <> (namesSubtract fv1 inl_nms_bef)
      res_nms_aft_sched = namesIntersection (getPatNames aft_sched) $ res_nms <> (namesSubtract fv2 inl_nms_aft)
      res_nms_aft_rec   = namesIntersection (getPatNames aft_rec) res_nms
  --
      tab0 = M.fromList $ zip (namesToList res_nms) $ map (\p -> (patElemName p, patElemType p)) $ patElems pat
  (tab_bef, map_stms_bef) <- distributeMapOnStmts tab0 outer_rec_stm (bef_sched, mempty) res_nms_bef_sched
  --
  let tab1 = tab0 `M.union` tab_bef
  (tab_schd, sched_stms)  <- distributeMapOnSched tab1 outer_rec_stm (msched, inl_nms_bef) res_nms
  --
  let tab2 = tab_bef `M.union` tab_schd
  (tab_aft, map_stms_aft) <- distributeMapOnStmts tab2 outer_rec_stm (aft_sched, inl_stms_bef) res_nms_aft_sched
  let tab3 = tab2 `M.union` tab_aft
  --
  (tab_rec, map_stm_rec) <- interchangeInwards tab3 outer_rec_stm (rec_stm, inl_stms_aft) res_nms
  let tab4 = tab3 `M.union` tab_rec
  --
  -- ToDo: we need to transpose the result or/and to substitute in the `aft_rec` statements;
  --       otherwise the interchange will break the typing
  --
  (_tab_fin, map_stms_fin) <- distributeMapOnStmts tab4 outer_rec_stm (aft_rec, inl_stms_aft) res_nms_aft_rec
  -- let tab5 = tab4 `M.union` tab_fin
  --
  pure (map_stms_bef <> sched_stms <> map_stms_aft, map_stm_rec, map_stms_fin)
  --
  where
    se2VName (Var nm) = Just nm
    se2VName _ = Nothing
  -- map (Var . patElemName) $ patElems pat
transposeTopOfNest _fenv _env _sched outer_rec_stm =
  error ("Yet unsuported case for function transposeTopOfNest in Permute.hs, stmt: "++prettyString outer_rec_stm)

distributeMapOnStmts :: (LocalScope SOACS m, MonadFreshNames m) =>
    LftTab -> Stm SOACS -> (Stms SOACS, Stms SOACS) -> Names -> m (LftTab, Stms SOACS)
distributeMapOnStmts _ _ (inner_stms, _) _
  | null inner_stms = pure (mempty, mempty)
distributeMapOnStmts tab outer_stm (inner_stms, to_inline) res_nms
  | Let _pat aux (Op (Screma m inp_nms (ScremaForm map_lam [] []))) <- outer_stm = do
  --
  let lam_pars = lambdaParams map_lam
      fvs0 = namesSubtract (freeInStms inner_stms) $ getPatNames to_inline
      fvs1 = namesSubtract fvs0 $ freeIn (lambdaBody map_lam)
      fvs  = namesSubtract fvs1 $ namesFromList $ map paramName lam_pars
      tab_nms = namesFromList $ M.keys tab
      (new_pars, arr_nms, arr_tps) =
        if null $ namesToList $ namesSubtract fvs tab_nms
        then let com_nms = namesToList $ namesIntersection fvs tab_nms
                 (_arrnms, arrtps) = unzip $ mapMaybe (`M.lookup` tab) com_nms
                 params  = zipWith (\p_nm arr_tp -> Param mempty p_nm (stripArray 1 arr_tp)) com_nms arrtps
             in  (params, arr_nms, arr_tps)
        else error ("Error in distributeMapOnStmts: " ++
                    "names in tab do not cover all interm arrays! " ++
                    prettyString (namesSubtract fvs tab_nms)
                   )
  scope <- askScope
  new_lam <-
    runLambdaBuilder (lam_pars ++ new_pars) $ localScope scope $ do
      addStms to_inline; addStms inner_stms; pure (map (subExpRes . Var) (namesToList res_nms))
  new_lam' <- renameLambda new_lam >>= simplifyLambda 0
  
  -- this should be a sort of fold so that we can accumulate in tab
  tmp <- mapM (mkPatElm m) $ namesToList res_nms  
  let (mb_bindings, new_pat_elms) = unzip tmp
      new_tab = M.fromList $ mapMaybe id mb_bindings
      new_pat = Pat new_pat_elms
      map_stm = Let new_pat aux $ Op $ Screma m (inp_nms ++ arr_nms) $ ScremaForm new_lam' [] []
  pure (new_tab, Sq.singleton map_stm)
  --
  where
    mkPatElm w nm
      | Just (arr_nm, arr_tp) <- M.lookup nm tab = pure (Nothing, PatElem arr_nm arr_tp)
      | True = do
      let pat_el_tp = findPatElemInStms nm $ stmsToList inner_stms
          arr_tp    = arrayOfRow pat_el_tp w
      arr_nm <- newVName (baseString nm ++ "lifted")
      pure ( Just (nm, (arr_nm, arr_tp)), PatElem arr_nm arr_tp )
    --
    findPatElemInStms nm [] =
      error ("PatElem for name "++show nm++" not found!")
    findPatElemInStms nm ((Let pat _ _ ):stms) =
      case L.find (\ p -> patElemName p == nm) (patElems pat) of
        Nothing -> findPatElemInStms nm stms
        Just pel-> patElemType pel
    
      
      
--
distributeMapOnStmts _ outer_stm _ _ =
  error ("Compiler shortcoming: distribution of statements other than map-reduce " ++
         "is not currently supported. Current Stm is: \n" ++ prettyString outer_stm ++ "\n\n")

distributeMapOnSched :: (LocalScope SOACS m, MonadFreshNames m) =>
    LftTab -> Stm SOACS -> (Maybe (Names, Stm SOACS), Names) -> Names -> m (LftTab, Stms SOACS)
distributeMapOnSched tab outer_rec_stm (msched, inl_nms_bef) res_nms =
  pure (mempty, mempty)

-- | ToDo: this should support a special case of a redomap perfectly
--         nested inside a redomap, both featuring the same assoc &
--         commutative reduce operator, in which case we can just
--         interchange the maps. Hence the result should be expanded
--         to indicate whether the outer reduce was already performed!
--
interchangeInwards :: (LocalScope SOACS m, MonadFreshNames m) =>
    LftTab -> Stm SOACS -> (Stm SOACS, Stms SOACS) -> Names -> m (LftTab, Stm SOACS)
interchangeInwards tab outer_rec_stm (rec_stm, inl_stms_aft) res_nms =
  pure (mempty, rec_stm)

--------------------------------------------------------------
--- Simple Helper functions: sep3/5LastRec & mkRecWithBody ---
--------------------------------------------------------------

getPatNames :: Stms SOACS -> Names
getPatNames = foldl mergeWithPatNames mempty
 where
   mergeWithPatNames acc (Let pat _ _) =
     acc <> (namesFromList $ map patElemName $ patElems pat)

freeInStms :: Stms SOACS -> Names
freeInStms stms = namesSubtract (freeIn stms) $ getPatNames stms

data PartStms = PartStms
  { bef_sched :: Stms SOACS,
    lst_sched :: Maybe (Names, Stm SOACS),
    aft_sched :: Stms SOACS,
    tgt_recstm:: Maybe (Stm SOACS),
    aft_tgtrec:: Stms SOACS
  } deriving Show

sep3LastRec :: Exp SOACS -> Maybe (Stms SOACS, Stm SOACS, Stms SOACS, [SubExp])
sep3LastRec e =
  case e of
    Op (Screma _mm _inps (ScremaForm map_lam [] _reds)) ->
      sep3LastRecBody $ lambdaBody map_lam
    Loop _ ForLoop{} body ->
      sep3LastRecBody body
    _ ->
      error ("Unimplementated case for function sepLastRec3Ways in Permute.hs, expression: "++prettyString e)
  where
    -- sep3LastRecBody Body SOACS -> Maybe (Stms SOACS, Stm SOACS, Stms SOACS, [SubExp])
    sep3LastRecBody body =
      let (parts, se_res) = sep5LastRecBody body 
          sched = case lst_sched parts of
                    Nothing  -> mempty
                    Just (_,stm) -> Sq.singleton stm
      in  case tgt_recstm parts of
            Just rec -> Just ( bef_sched parts <> sched <> aft_sched parts, rec, aft_tgtrec parts, se_res)
            Nothing  -> Nothing

sep5LastRecBody :: Body SOACS -> (PartStms, [SubExp])
sep5LastRecBody body =
  ( sepLastRecStms (Sq.reverse (bodyStms body))
  , map resSubExp $ bodyResult body
  )
  where
    sepLastRecStms rev_stms =
      foldl processStm (PartStms mempty Nothing mempty Nothing mempty) rev_stms
    processStm acc curr_stm
      | isNothing (tgt_recstm acc) && not (isSupportedRec curr_stm) =
          acc { aft_tgtrec = curr_stm Sq.<| (aft_tgtrec acc) }
      | isNothing (tgt_recstm acc) && isSupportedRec curr_stm =
          acc { tgt_recstm = Just curr_stm }
      | isJust (tgt_recstm acc) && isNothing (lst_sched acc) && isNothing (isSchedStm curr_stm) =
          acc { aft_sched  = curr_stm Sq.<| (aft_sched acc) }
      | isJust (tgt_recstm acc) && isNothing (lst_sched acc),
        Just sched_info <- isSchedStm curr_stm =
          acc { lst_sched  = Just sched_info }
      | isJust (tgt_recstm acc) && isJust (lst_sched acc) =
          acc { bef_sched  = curr_stm Sq.<| (bef_sched acc) }
      | True = error "Unreachable Case Reached in sep5LastRecBody.processStm!"
    -- 
    isSupportedRec (Let _ _ (Loop _ ForLoop{} _)) = True
    isSupportedRec (Let _ _ (Op (Screma _ _ (ScremaForm _ [] [])))) = True
    isSupportedRec (Let _ _ (Op (Screma _ _ form))) =
      isJust $ oneFullyConsumedMapRed form
    isSupportedRec _ = False
    --
    isSchedStm stm@(Let pat _ (Apply fnm arg_diets _ _)) =
      if L.isPrefixOf "hlSched" $ nameToString fnm
      then let num_res = length $ patElems pat
               arr_nms = map getVName $ take num_res $ fst $ unzip arg_diets
           in  Just (namesFromList arr_nms, stm)
      else Nothing
      where
        getVName (Var nm) = nm
        getVName _ = error ("Target array argument to HL-schedule is a constant!")
    isSchedStm _ = Nothing

-- | This is supposed to make a body from `stms_bef`, `new_stms_rec` and
--     `stms_aft`, where `new_stms_rec` are the statements generated by
--     a `permuteNest` call on an inner recurrence `stm_rec`.
--   The current implementation is WRONG because it does not adjust
--     `stms_aft` and the body result to the type of the permuted nest.
--   To fix: pass also the schedule as an argument, or at least the
--     permutation of the schdule and perform the necessary substitutions
--     in `stms_aft` and the body result.
--   
mkRecWithBody :: (LocalScope SOACS m, MonadFreshNames m) =>
    Stm SOACS -> (Stms SOACS, Stms SOACS, Stms SOACS, [SubExp]) -> m (Stm SOACS)
mkRecWithBody orig_nest_stm (stms_bef, stms_perm_rec, stms_aft, res_ses) -- body_stms
  | Let pat aux (Op (Screma m inp_nms (ScremaForm map_lam [] reduces))) <- orig_nest_stm = do
  scope <- askScope
  map_lam' <- runLambdaBuilder (lambdaParams map_lam) $ localScope scope $ do
    addStms (stms_bef <> stms_perm_rec <> stms_aft); pure $ map subExpRes res_ses
  pure $ Let pat aux $ Op $ Screma m inp_nms $ ScremaForm map_lam' [] reduces
mkRecWithBody orig_nest_stm (stms_bef, stms_perm_rec, stms_aft, res_ses)
  | Let pat aux (Loop par_inis (ForLoop i inttp mm) _body) <- orig_nest_stm,
    loop_pars <- map fromFParam (fst (unzip par_inis)) = do
  let i_param = Param (Attrs mempty) i $ Prim $ IntType inttp
  scope <- askScope
  loop_body' <- runBodyBuilder $ localScope (scope <> scopeOfLParams (i_param:loop_pars))$ do
    addStms (stms_bef <> stms_perm_rec <> stms_aft); pure $ map subExpRes res_ses
  pure $ Let pat aux $ Loop par_inis (ForLoop i inttp mm) loop_body'
--
mkRecWithBody orig_nest_stm _ =
  error ("Yet unsuported case for function mkRecWithBody in Permute.hs, stmt: "++prettyString orig_nest_stm)


findCheapStms2Inline :: Names -> PartStms -> ((Names, Stms SOACS), (Names, Stms SOACS))
findCheapStms2Inline fvs_body parts
  | isJust (tgt_recstm parts) =
  let (inl_nms_bef, inl_stms_bef) = foldl findCheapStm (mempty, mempty) $ bef_sched parts
      safe_sched =
        case lst_sched parts of
          Nothing -> True
          Just (tgt_nms, (Let _ _ e)) ->
            all (\ fnm -> nameIn fnm fvs_body || nameIn fnm inl_nms_bef) $
                namesToList $ namesSubtract (freeIn e) tgt_nms
      (inl_nms_aft, inl_stms_aft) = foldl findCheapStm (inl_nms_bef, inl_stms_bef) $ aft_sched parts
  in  if safe_sched
      then ((inl_nms_bef, inl_stms_bef), (inl_nms_aft, inl_stms_aft))
      else error "Illegal Inner Schedule detected in inlineCheapStms!"
  -- sched_info <- sched_smt Sq.|>  inl_stms_bef
  -- aft_sched  <- inl_stms_bef <> aft_sched  parts
  -- aft_tgtrec <- inl_stms_aft <> aft_tgtrec parts
  where
    findCheapStm :: (Names, Stms SOACS) -> Stm SOACS -> (Names, Stms SOACS)
    findCheapStm (cheap_nms, cheap_stms) stm@(Let pat _ e) =
      let ok_to_add =
           case e of
             BasicOp (SubExp (Constant _)) -> True
             BasicOp (SubExp (Var nm)) ->
               nameIn nm cheap_nms || nameIn nm fvs_body
             _ -> False
      in  if ok_to_add
          then (addPat2Names pat cheap_nms, cheap_stms Sq.|> stm)
          else (cheap_nms, cheap_stms)
    addPat2Names pat nms =
      nms <> (namesFromList $ map patElemName $ patElems pat)
findCheapStms2Inline _ _ = ((mempty, mempty), (mempty, mempty))
      
