{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.Stripmine
  ( applyStripmining
  )
where

import Control.Monad
--import Data.Graph.Inductive.Graph qualified as G
import Data.Sequence qualified as Sq
import Data.Map.Strict qualified as M
import Data.Set qualified as S
--import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
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
import Futhark.Analysis.PrimExp.Convert
--import Futhark.Optimise.TileLoops.Shared

import Debug.Trace

-- Redundant: HasScope SOACS m 

-- | ToDos:
--   1. treat iota and scalar dependencies from outside target SOAC                 -- CHECK!
--   2. need to generate the code for the iota inputs to various soacs              -- partially CHECK
--   3. perhaps extend the target SOAC to support redomaps, in addition to maps (?)
--   4. parse the TARGET SOAC and map the recurrences corresponding to the result
--   5.   at this stage allow and treat the case of out-transforms on SOAC's result
--   6. place enough restrictions to make it safe, including Tile2D, padding, e.g.,
--        treat the case of imprecise tiling of maps that are part of result!
--        certify that mapaccums give the soac's result and are inserted at outermost level
--        what happens if the soac has multiple results?
--   7. apply fusion recursively at inner level, rename and simplify lambdas        -- CHECK!
applyStripmining ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched ->
    (Pat Type, StmAux (ExpDec SOACS), H.SOAC SOACS) ->
    m (Maybe (H.SOAC SOACS))
applyStripmining fenv env sched (pat, aux, H.Screma mm inps (H.ScremaForm map_lam [] []))
  | not (any hasTransform inps) = do
    let soac = F.Screma mm (map nameFromInp inps) $ F.ScremaForm map_lam [] [] 
    mstmsres <- stripmineStmt fenv 0 env sched (Let pat aux (Op soac))
    case mstmsres of
      Just (prologue_stms, _env', ( (Let _ _ soac'@(Op (F.Screma m' [nm] form'))) Sq.:<| _ )) -> do
        let tp = Array i64ptp (Shape [m']) NoUniqueness
            hinp = H.Input H.noTransforms nm tp
        trace ("stripmined soac: \n" ++ prettyString soac' ++
             "\n prologues-stms:\n"++ prettyString prologue_stms ) $
          pure $ Just $ H.Screma m' [hinp] form'
      _ -> pure Nothing
    where
      hasTransform (H.Input transf _ _) = not $ H.nullTransforms transf 
      nameFromInp (H.Input _ nm _) = nm
applyStripmining _ _ _ _ = pure Nothing

--------------------------------------------------------------------------
--------------------------------------------------------------------------

exactTiling :: Env -> SubExp -> [PrimExp VName] -> Bool
exactTiling env m dims =
  eqPEs (expandSE env m i64ptp) (foldl mulPes pe1 dims)

validStripForDim :: Env -> Int -> SubExp -> HLSched -> Maybe (HLSched, HLSched)
validStripForDim env k m sched
  | q <- countSame $ origids sched,
    (sched', sched'') <- splitAtSched q sched,
    validSize (dimlens sched') =
  Just (sched', sched'')
  where
    countSame [] = 0
    countSame (i:ctarr) =
      if k == i
      then 1 + countSame ctarr
      else 0
    validSize dims
      | m_pe <- peFromSe env i64ptp m,
        Just nms <- M.lookup m_pe (appTilesFwd env),
        penms <- S.fromList $ map (`LeafExp` i64ptp) $ namesToList nms,
        length dims == S.size penms,
        penms == S.fromList dims = True
    validSize dims =
      if eqPEs (expandSE env m i64ptp) (foldl mulPes pe1 dims)
      then True
      else error ( "\n\n\nvalidSize FAILS! expanded size: " ++
                   prettyString (expandSE env m i64ptp) ++
                   " dims: " ++
                   prettyString (foldl mulPes pe1 dims) ++
                   "env-sizes: " ++ show (scalars env) ++
                   "\n\n\n"
                 ) -- False
--
validStripForDim _ _ _ _ = Nothing

----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- | ToDos: needs to be improved, for example to:
--   1. return an expression, not a SOAC, otherwise loops cannot
--      be stripmined; question is how do I get the types in recuCase???
--   2. populate the environment, for example bind the PrimExp
--      of i_flat
--   3. not here, at the very beginning of the pass, need to
--      track all dependencies of the original SOAC and populate
--      the symbol table with bindings for:
--        (a) PrimExp of integral scalar variables
--        (b) transformation on arrays, e.g., transpose
--   4. Remember to rename lambdas!
stripmineMap :: (LocalScope SOACS m, MonadFreshNames m) =>
    (Int, [(VName,Type)], SubExp) ->
    Env -> [PrimExp VName] ->
    PrimExp VName -> Lambda SOACS ->
    m (Maybe (Stms SOACS, Env, F.SOAC SOACS))
stripmineMap (k, inp_nm_tps, mm) env (n:ns) cur_ind lam = do
  i_p <- newParam ("i"++show k) $ Prim i64ptp
  let i_pe = LeafExp (paramName i_p) i64ptp
      i_mul_ns = mulPes i_pe $ foldl mulPes pe1 ns
      cur_ind' = addPes cur_ind i_mul_ns
  -- build the iota for this screma
  scope <- askScope
  iota_info <- mkIota k n
  if null ns
  then baseCase scope i_p cur_ind' iota_info
  else recuCase scope i_p cur_ind' iota_info
  where
    baseCase scope i_p cur_ind' ((w,iotnm), prologue_stms) = do
      new_lam <-
        runLambdaBuilder [i_p] $ localScope scope $ do
          let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowWrap) (peFromSe env i64ptp mm) pe1 
          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
          forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, inp_tp)) -> do
            let index_e = mkArrIndexingExp env inp_nm inp_tp i_flat
            letBind (Pat [PatElem (paramName lp) (paramDec lp)]) index_e
                    -- BasicOp $ Index inp_nm (Slice [DimFix i_flat])
          addStms $ bodyStms $ lambdaBody lam
          pure $ bodyResult $ lambdaBody lam
      new_lam' <- renameLambda new_lam >>= simplifyLambda 0
      let soac = F.Screma w [iotnm] $ ScremaForm new_lam' [] []
      pure $ Just (prologue_stms, env, soac)
    --
    recuCase scope i_p cur_ind' ((w,iotnm), prologue_stms) = do
      rec_res <- stripmineMap (k, inp_nm_tps, mm) env ns cur_ind' lam
      case rec_res of
        Just (prev_stms, prev_env, prev_soac) -> do
          new_lam <-
            runLambdaBuilder [i_p] $ localScope scope $ do
              prev_soac_res <- letTupExp' ("map"++show k++"strip") $ Op prev_soac
              pure $ map subExpRes prev_soac_res
          new_lam' <- renameLambda new_lam >>= simplifyLambda 0
          let soac = F.Screma w [iotnm] $ ScremaForm new_lam' [] []
          pure $ Just (prologue_stms <> prev_stms, prev_env, soac)
        -- end ^
        _ -> pure Nothing
stripmineMap _ _ [] _ _ =
  error "stripmineMap: unreachable case reached!"

----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- | Performs stripmining on the body of a lambda `lam` as dictated
--     by a high-level schedule `sched`. The input arrays and types
--     are given in `inp_nm_tps`, and the input arrays are assumed
--     to not be transformed.
stripmineLambda :: (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Int -> Env -> HLSched -> Lambda SOACS ->
    m (Maybe (Stms SOACS, Env, Lambda SOACS))
stripmineLambda _ _k env sched lam
  | null (dimlens sched) = pure $ Just (mempty, env, lam)
stripmineLambda fenv k env sched lam0 = do
  scope <- askScope
  (lam, _) <- (fuseInLambda fenv) lam0
  mbdyres <- localScope (scope <> scopeOf lam) $
    stripmineBody fenv k env sched (lambdaBody lam)
  case mbdyres of
    Nothing -> pure Nothing
    Just (prologue_stms, new_env, new_bdy) -> do
      new_lam <-
        runLambdaBuilder (lambdaParams lam) $ localScope scope $ do
          addStms $ bodyStms new_bdy
          pure $ bodyResult new_bdy
      new_lam' <- renameLambda new_lam >>= simplifyLambda 0
      pure $ Just (prologue_stms, new_env, new_lam')

stripmineBody :: (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Int -> Env -> HLSched  -> Body SOACS ->
    m (Maybe (Stms SOACS, Env, Body SOACS))
stripmineBody fenv k env sched bdy = do
  scope <- askScope
  (localScope (scope <> scopeOf (bodyStms bdy)) $
     foldM f (Just (mempty, env, mempty)) $ bodyStms bdy)
     >>= (pure . g)
  where
    f Nothing _ = pure Nothing
    f (Just (acc_prologue, acc_env, acc_stms)) stm = do
      mresstm <- stripmineStmt fenv k acc_env sched stm
      case mresstm of
        Nothing -> pure Nothing
        Just (new_prologue, new_env, new_stms) ->
          pure $ Just (acc_prologue <> new_prologue, new_env, acc_stms <> new_stms)
    g Nothing = Nothing
    g (Just (prol', env', stms')) = Just (prol', env', bdy { bodyStms = stms' })

stripmineStmt :: (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Int -> Env -> HLSched -> Stm SOACS ->
    m (Maybe (Stms SOACS, Env, Stms SOACS))
stripmineStmt _ _ env sched stm
  | null (dimlens sched) = pure $ Just (Sq.empty, env, Sq.singleton stm)
  -- ^ no schedule to apply, just return statement
stripmineStmt _ k env sched (Let _ _ e)
  | Op (F.Screma mm _ _) <- e,
    Nothing <- validStripForDim env k mm sched =
  pure Nothing
  -- ^ Illegal Stripmining of Screma
stripmineStmt fenv k env sched (Let pat aux
      (Op (F.Screma mm inp_nms (ScremaForm map_lam [] []))))
  | Just (sched', sched'') <- validStripForDim env k mm sched = do
  -- ^ Stripmining a map statement
  stripmineLambda fenv (k+1) env sched'' map_lam >>=
    maybe (pure Nothing) (mkResFromMLam sched')
  where
    mkResFromMLam schd (prologue_lam, env1, lam1) = do
      inp_tps <- mapM lookupType inp_nms
      mres_nest <- stripmineMap (k,zip inp_nms inp_tps,mm) env1 (dimlens schd) pe0 lam1
      case mres_nest of
        Just (prologue_stms2, env2, soac'@(F.Screma m' _nms (F.ScremaForm map_lam' [] []))) -> do
        -- ^ by construction the resulting soac has one iota-array arg
          let rts   = map (`arrayOfRow` m') $ lambdaReturnType map_lam'
              pels' = zipWith adjustPatElType (patElems pat) rts
              stm'  = Let (Pat pels') aux (Op soac')
          pure $ Just (prologue_stms2 <> prologue_lam, env2, Sq.singleton stm')
        _ -> pure Nothing
    adjustPatElType patel tp = patel { patElemDec = tp}
---------------------------------------------
-- Stripmining a Normalized DO Loop
---------------------------------------------
stripmineStmt fenv k env sched (Let pat aux (Loop fpar_inis (ForLoop liter inttp lcount) loop_body))
  | Just (sched', sched'') <- validStripForDim env k lcount sched = do
  stripmineBody fenv (k+1) env sched'' loop_body >>=
    maybe (pure Nothing) (mkResFromMBody sched')
  where
    mkResFromMBody schd (prologue_lam, env1, body1) = do
      let (fpars, inis) = unzip fpar_inis
          dim_lens = (dimlens schd)
      fpars' <- forM fpars $ \ fpar -> do
        let base_par_str = baseString $ paramName fpar
        newParam (base_par_str++show (0::Int)) (paramDec fpar)
      (prol, env2, stm) <- recStripLoop dim_lens env1 dim_lens body1 (pat,fpars',inis) pe0
      pure $ Just (prol <> prologue_lam, env2, Sq.singleton stm)
    -- Core Function for Stripmining a Normalized DO Loop
    recStripLoop _ _ [] _ _ _ =
      error "Unreachable case reached in loop stripmining"
    recStripLoop dlens env1 (w:ws) body1 (lpat, fpars, inis) cur_ind = do
      scope <- askScope
      i_p <- newParam ("i_strip"++show k) (Prim $ IntType inttp)
      localScope (scope <> scopeOfLParams [i_p]) $ do
        let i_p_nm = paramName i_p
            cur_ind' = addPes cur_ind $ mulPes (LeafExp i_p_nm i64ptp) $ foldl mulPes pe1 ws
        (w_se, w_stms) <- runBuilder $ localScope scope $ letSubExp ("w"++show k) =<< toExp w
        (prologue, env2, loop_e) <-
          case null ws of
            True  -> do
              let fparso = fst $ unzip fpar_inis
              loop_body' <- mkStripminedBody dlens body1 cur_ind'
              pure (Sq.empty, env1, Loop (zip fparso inis) (ForLoop i_p_nm inttp w_se) loop_body')
            False -> do
              let pat_strs = map (baseString . patElemName) $ patElems pat 
              pat_nms <- forM pat_strs $ \str -> newVName (str ++ show k)
              fpars' <- forM fpars $ \fpar -> newParam (baseString (paramName fpar) ++ show k) $ paramDec fpar
              let lpat' = Pat $ map (\(nm,fpar) -> PatElem nm $ fromDecl $ paramDec fpar) $ zip pat_nms fpars
                  inis'= map (Var . paramName) fpars
              (rec_prologue, rec_env, rec_stm) <- recStripLoop dlens env1 ws body1 (lpat', fpars', inis') cur_ind'
              let rec_stm_res= map (subExpRes . Var . patElemName) $ patElems $ stmPat rec_stm
                  loop_body' = Body (bodyDec loop_body) (Sq.singleton rec_stm) rec_stm_res
              pure (rec_prologue, rec_env, Loop (zip fpars inis) (ForLoop i_p_nm inttp w_se) loop_body')
        pure (prologue <> w_stms, env2, Let lpat aux loop_e)
    --
    mkStripminedBody dim_lens body1 cur_ind = do
      scope <- askScope
      runBodyBuilder $ localScope scope $ do
        i_flat_se <- letSubExp ("i"++show k++"flat") =<< toExp cur_ind
        letBind (Pat [PatElem liter (Prim $ IntType inttp)]) $ BasicOp $ SubExp i_flat_se
        if exactTiling env lcount dim_lens
        then do
          addStms $ bodyStms body1
          pure $ bodyResult body1
        else do
          if_se <-
            letTupExp' "if_res"
              =<< eIf
                   ( pure $ BasicOp $ CmpOp (CmpSlt inttp) (Var liter) lcount )
                   ( pure body1 )
                   ( resultBodyM $ map (Var . paramName) $ fst $ unzip fpar_inis )
          pure $ map subExpRes if_se
-- other kinds of loops are not currently supported 
stripmineStmt _ _ _ _ (Let _pat _aux (Loop{})) =
  trace ("Only normalized for loops are currently supported!") $ pure Nothing
--------------------------------------------------------------------------
--- Stripmining a Red o Map
--------------------------------------------------------------------------
--
-- | The case of a redomap statement => sequentialize as loop then stripmine the loop
--   ToDo: 1. check that all results of map are consumed, or record the non-consumed results
--         2. it will not find the size of Redomap in the `appTilesFwd` SymTab, hence the
--              `validStripForDim` will fail
--         3. verify that the mapaccums are at the very top,
--              i.e., loop-mapaccum-loop is ilegal as it would
--                    need lifting the computation for the top loop.
stripmineStmt fenv k env sched (Let pat aux
    (Op (F.Screma mm inp_nms (ScremaForm map_lam [] [Reduce com red_lam nes]))))
  |  Just (sched', sched'') <- validStripForDim env k mm sched,
     -- strides  <- mkRegIndStrides sched',
     sig_lens <- zip (signals sched') (dimlens sched') = do
  stripmineLambda fenv (k+1) env sched'' map_lam >>=
    maybe (pure Nothing) (mkResFromMLam sig_lens)
  where
    mkResFromMLam sig_lens (prologue_stms1, env1, map_lam1) = do
      let pat_nms = map patElemName $ patElems pat
      mres <- recStripRedomap env1 sig_lens map_lam1 (pat_nms,nes) pe0
      maybe (pure Nothing) (joinMbRes prologue_stms1) mres
    -- Core Function for Stripmining a Red o Map: Base Case
    recStripRedomap env1 [] map_lam1 _ cur_ind = do
      let (red_acc, red_els) = splitAt (length (lambdaReturnType red_lam)) $ lambdaParams red_lam
      scope <- askScope
      inp_tps <- mapM lookupType inp_nms
      loop_body <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams red_acc) $ do
          -- let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowWrap) (peFromSe env i64ptp mm) pe1 
          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp cur_ind
          forM_ (zip3 inp_nms inp_tps (lambdaParams map_lam1)) $ \ (arr, tp, arg) ->
            -- codegen of map's lambda arguments
            letBind (Pat [PatElem (paramName arg) (paramDec arg)]) $
                mkArrIndexingExp env arr tp i_flat
          addStms $ bodyStms $ lambdaBody map_lam1
          forM_ (zip red_els $ bodyResult $ lambdaBody map_lam1) $ \ (rlam_p, map_res) ->
            -- codegen of copy stamts connecting the map's lambda res with reduce's inp
            letBind (Pat [PatElem (paramName rlam_p) (paramDec rlam_p)]) $
                BasicOp $ SubExp $ resSubExp map_res
          addStms $ bodyStms $ lambdaBody red_lam
          pure $ bodyResult $ lambdaBody red_lam
      pure $ Just (Sq.empty, env1, bodyStms loop_body)
    --
    -- Core Function for Stripmining a Red o Map: Recursive Case
    recStripRedomap env1 ((sig,dlen): schd') map_lam1 (pat_nms, acc_ini) cur_ind = do
      scope <- askScope
      i_p <- newParam ("i"++show k) $ Prim i64ptp
      pat_nms' <- forM (patElems pat) $ \_ -> newVName ("stripRedpat"++show k) 
      let cur_ind' = addPes cur_ind $ mulPes (LeafExp (paramName i_p) i64ptp) $ foldl mulPes pe1 $ map snd schd'
          num_vars = length $ lambdaReturnType red_lam
      -- the innermost level uses the two halves of reduce's lambda args as loop accumulator and
      --   array elements, respectively && also uses the vars of reduce's lambda result as result.
      -- the other levels use fresh names for accumulators && the previous level result as result.
      (red_acc', rec_res') <-
        if null schd'
        then pure (take num_vars $ lambdaParams red_lam, bodyResult $ lambdaBody red_lam)
        else do acc' <- mapM (newParam ("stripacc"++show k) . patElemDec) $ patElems pat
                pure (acc', map (subExpRes . Var) pat_nms')
      let next_inis = if parMode sig == Seq then map (Var . paramName) red_acc' else nes
      rec_res <- recStripRedomap env1 schd' map_lam1 (pat_nms', next_inis) cur_ind'
      case (parMode sig, rec_res) of
        -- The case of a mapaccum subdimension, i.e., generating a map:
        (Macc, Just (rec_prologue, rec_env, rec_stms)) -> do
          ((w,iotnm), new_prologue) <- mkIota k dlen
          new_lam <-
            runLambdaBuilder [i_p] $ localScope (scope <> scopeOfLParams red_acc') $ do
              addStms rec_stms; pure rec_res'
          new_lam' <- renameLambda new_lam >>= simplifyLambda 0
          let map_stm = soacStm (pat_nms, map (`arrayOfRow` w) $ lambdaReturnType red_lam)
                (w,iotnm) $ ScremaForm new_lam' [] []
          pure $ Just (new_prologue <> rec_prologue, rec_env, Sq.singleton map_stm)
        -- The case of a parallel dimension, i.e., generating a redomap
        (_, Just (rec_prologue, rec_env, rec_stms)) -> do
          ((w,iotnm), new_prologue) <- mkIota k dlen
          new_lam <-
            runLambdaBuilder [i_p] $ localScope (scope <> scopeOfLParams red_acc') $ do
              addStms rec_stms; pure rec_res'
          new_lam' <- renameLambda new_lam >>= simplifyLambda 0
          red_lam' <- renameLambda red_lam
          new_stms <- flip runBuilderT_ scope $ do
              soac_res <- forM pat_nms $ \_ -> newVName $ "redomap_res" ++ show k
              addStm $ soacStm (soac_res, lambdaReturnType red_lam) (w,iotnm) $
                ScremaForm new_lam' [] [Reduce com red_lam' nes]
              applyRedLam (red_lam, nes) acc_ini (map Var soac_res) pat_nms >>= addStms
          pure $ Just (new_prologue <> rec_prologue, rec_env, new_stms)
{--
        -- This is commented out because we do not want to sequentialize at this stage,
        --   e.g., for SpMV we would like to stripmine and then to interchange!
        -- The case of a sequential subdimension, i.e., generating a loop:
        (Seq, Just (rec_prologue, rec_env, rec_stms)) -> do
          (w, w_stms) <- runBuilder $ localScope scope $ letSubExp ("w"++show k) =<< toExp dlen
          loop_body <-
            runBodyBuilder $ localScope (scope <> scopeOfLParams (i_p: red_acc')) $ do
              addStms rec_stms; pure rec_res'
          let loop_form  = ForLoop (paramName i_p) Int64 w
              loop_fargs = map toFParam red_acc'
              pat_res    = Pat $ zipWith PatElem pat_nms $ lambdaReturnType red_lam
              loop_stm   = Let pat_res aux $ Loop (zip loop_fargs acc_ini) loop_form loop_body
          pure $ Just (w_stms <> rec_prologue, rec_env, Sq.singleton loop_stm)
--}
        _ -> pure Nothing
    soacStm (pat_nms,tps) (w, iotnm) form =
      let pat_res = Pat $ zipWith PatElem pat_nms tps
      in  Let pat_res aux $ Op $ F.Screma w [iotnm] form
--------------------------------------------------------------------------
--- Stripmining Other Statements
--------------------------------------------------------------------------
 
stripmineStmt _ _k env _sched stm@(Let pat aux e) = do
  let env' =
       case (patElems pat, e) of
        ([p_el], BasicOp (Rearrange vnm sigma)) -> do
        -- ^ handling transpositions
          addTransf2Env env (patElemName p_el) vnm $
            H.noTransforms H.|> (H.Rearrange aux sigma)
        ([p_el], BasicOp (BinOp binop e1 e2)) -> do
          addBinOp2Env env (patElemName p_el,(patElemDec p_el)) binop e1 e2
        _ -> env
  pure $ Just (mempty, env', Sq.singleton stm)

--------------------------------------------------------------------------
--- Helpers
--------------------------------------------------------------------------

--emptyCerts :: Certs
--emptyCerts = Certs []
se0 :: SubExp
se0 = Constant $ IntValue $ Int64Value 0

se1 :: SubExp
se1 = Constant $ IntValue $ Int64Value 1

pe0 :: PrimExp VName
pe0 = ValueExp $ IntValue $ Int64Value 0

pe1 :: PrimExp VName
pe1 = ValueExp $ IntValue $ Int64Value 1

i64ptp :: PrimType
i64ptp = IntType Int64

{--
toFParam :: LParam SOACS -> FParam SOACS
toFParam p = Param (paramAttrs p) (paramName p) $ toDecl (paramDec p) Unique

fromFParam :: FParam SOACS -> LParam SOACS
fromFParam p = Param (paramAttrs p) (paramName p) $ fromDecl (paramDec p)
--}

mkArrIndexingExp :: Env -> VName -> Type -> SubExp -> Exp SOACS
mkArrIndexingExp env inp_nm inp_tp i_flat
  | Just _ <- M.lookup inp_nm (iotas env),
    elemType inp_tp == IntType Int64 = BasicOp $ SubExp i_flat
  -- ^ if a iota array, simply return `i_flat`
mkArrIndexingExp env inp_nm inp_tp i_flat
  | Just (base_arr, trsf) <- M.lookup inp_nm (arrTransf env),
    (H.Rearrange _aux sigma) H.:< empty_trsfs <- H.viewf trsf,
    H.nullTransforms empty_trsfs,
    Array _ptp shp _u <- inp_tp,
    _d:ds <- shapeDims shp =
    let slice_dims = (DimFix i_flat) : map (\d -> DimSlice se0 d se1) ds
        perm_slice = map (\i -> slice_dims !! i) $ invPerm sigma
        index_exp  = BasicOp $ Index base_arr $ Slice perm_slice
    in  index_exp
mkArrIndexingExp _ inp_nm _ i_flat =
  BasicOp $ Index inp_nm (Slice [DimFix i_flat])

joinMbRes :: (Applicative m) =>
  Stms SOACS -> (Stms SOACS, a, b) -> m (Maybe (Stms SOACS, a, b))
joinMbRes stms1 (stms2, x, y) = pure $ Just (stms1 <> stms2, x, y)

mkIota :: (HasScope SOACS m, MonadFreshNames m) =>
    Int -> PrimExp VName -> m ((SubExp, VName), Stms SOACS)
mkIota k n = do
  scope <- askScope
  runBuilder $ localScope scope $ do
    w <- letSubExp ("w"++show k) =<< toExp n
    iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
    pure (w, iotnm)

mkCopyStms :: (LocalScope SOACS m, MonadFreshNames m) =>
    [VName] -> [SubExp] -> [Type] -> m (Stms SOACS)
mkCopyStms res_nms ses tps = do
  scope <- askScope 
  flip runBuilderT_ scope $ do
    forM_ (zip3 ses res_nms tps) $ \ (se, nm, tp) ->
      letBind (Pat [PatElem nm tp]) $ BasicOp $ SubExp se

applyRedLam :: (LocalScope SOACS m, MonadFreshNames m) =>
    (Lambda SOACS, [SubExp]) -> [SubExp] -> [SubExp] -> [VName] -> m (Stms SOACS)
applyRedLam (lam, nes) ses1 ses2 res
  | nes == ses1 = mkCopyStms res ses2 $ lambdaReturnType lam
  | nes == ses2 = mkCopyStms res ses1 $ lambdaReturnType lam
applyRedLam (lam, _) ses1 ses2 res = do
  scope <- askScope
  lam' <- renameLambda lam
  let (lam_pars, lam_rtps) = (lambdaParams lam', lambdaReturnType lam')
      lam_res_ses = map resSubExp $ bodyResult $ lambdaBody lam'
  flip runBuilderT_ (scope <> scopeOfLParams lam_pars) $ do
    mkCopyStms (map paramName lam_pars) (ses1 ++ ses2) (lam_rtps++lam_rtps) >>= addStms
    addStms $ bodyStms $ lambdaBody lam'
    mkCopyStms res lam_res_ses lam_rtps >>= addStms

--------------------------------------------------------------------------
--- GARBAGE CODE
--------------------------------------------------------------------------


