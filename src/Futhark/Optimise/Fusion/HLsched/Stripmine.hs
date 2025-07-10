{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.Stripmine
  ( applyStripmining
  )
where

import Data.List qualified as L
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
import Futhark.Optimise.Fusion.HLsched.Env
import Futhark.Optimise.Fusion.HLsched.SchedUtils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Analysis.PrimExp.Convert
--import Futhark.Optimise.TileLoops.Shared

import Debug.Trace

-- Redundant: HasScope SOACS m 

-- | ToDos:
--   1. need to treat SOAC's inputs that are iota arrays
--   2. need to generate the code for the iota inputs to various soacs
applyStripmining ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    Env -> HLSched -> (Pat Type, StmAux (ExpDec SOACS), H.SOAC SOACS) ->
    m (Maybe (H.SOAC SOACS))
applyStripmining env sched (pat, aux, H.Screma mm inps (H.ScremaForm map_lam [] []))
  | not (any hasTransform inps) = do
    let soac = F.Screma mm (map nameFromInp inps) $ F.ScremaForm map_lam [] [] 
    mstmsres <- stripmineStmt 0 env sched (Let pat aux (Op soac))
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
applyStripmining _ _ _ = pure Nothing

--------------------------------------------------------------------------
--------------------------------------------------------------------------

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
      | Just nms <- M.lookup (peFromSeI64 m) (appTilesFwd env),
        penms <- S.fromList $ map (`LeafExp` i64ptp) $ namesToList nms,
        length dims == S.size penms,
        penms == S.fromList dims = True
    validSize dims = expandSE env m i64ptp == foldl mulPes pe1 dims
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
  iota_info <-
    runBuilder $ localScope scope $ do
      w <- letSubExp ("w"++show k) =<< toExp n
      iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
  if null ns
  then baseCase scope i_p cur_ind' iota_info
  else recuCase scope i_p cur_ind' iota_info
  where
    baseCase scope i_p cur_ind' ((w,iotnm), prologue_stms) = do
      new_lam <-
        runLambdaBuilder [i_p] $ localScope scope $ do
          let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
          forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, inp_tp)) -> do
            let index_e = mkArrIndexingExp env inp_nm inp_tp i_flat
            letBind (Pat [PatElem (paramName lp) (paramDec lp)]) index_e
                    -- BasicOp $ Index inp_nm (Slice [DimFix i_flat])
          addStms $ bodyStms $ lambdaBody lam
          pure $ bodyResult $ lambdaBody lam
      let soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
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
          let soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
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
    Int -> Env -> HLSched -> Lambda SOACS ->
    m (Maybe (Stms SOACS, Env, Lambda SOACS))
stripmineLambda _k env sched lam
  | null (dimlens sched) = pure $ Just (mempty, env, lam)
stripmineLambda k env sched lam = do
  scope <- askScope
  mbdyres <- localScope (scope <> scopeOf lam) $
    stripmineBody k env sched (lambdaBody lam)
  case mbdyres of
    Nothing -> pure Nothing
    Just (prologue_stms, new_env, new_bdy) -> do
      new_lam <-
        runLambdaBuilder (lambdaParams lam) $ localScope scope $ do
          addStms $ bodyStms new_bdy
          pure $ bodyResult new_bdy
      pure $ Just (prologue_stms, new_env, new_lam)

stripmineBody :: (LocalScope SOACS m, MonadFreshNames m) =>
    Int -> Env -> HLSched  -> Body SOACS ->
    m (Maybe (Stms SOACS, Env, Body SOACS))
stripmineBody k env sched bdy = do
  scope <- askScope
  (localScope (scope <> scopeOf (bodyStms bdy)) $
     foldM f (Just (mempty, env, mempty)) $ bodyStms bdy)
     >>= (pure . g)
  where
    f Nothing _ = pure Nothing
    f (Just (acc_prologue, acc_env, acc_stms)) stm = do
      mresstm <- stripmineStmt k acc_env sched stm
      case mresstm of
        Nothing -> pure Nothing
        Just (new_prologue, new_env, new_stms) ->
          pure $ Just (acc_prologue <> new_prologue, new_env, acc_stms <> new_stms)
    g Nothing = Nothing
    g (Just (prol', env', stms')) = Just (prol', env', bdy { bodyStms = stms' })

stripmineStmt :: (LocalScope SOACS m, MonadFreshNames m) =>
    Int -> Env -> HLSched -> Stm SOACS ->
    m (Maybe (Stms SOACS, Env, Stms SOACS))
stripmineStmt _ env sched stm
  | null (dimlens sched) = pure $ Just (Sq.empty, env, Sq.singleton stm)
  -- ^ no schedule to apply, just return statement
stripmineStmt k env sched (Let _ _ e)
  | Op (F.Screma mm _ _) <- e,
    Nothing <- validStripForDim env k mm sched =
  pure Nothing
  -- ^ Illegal Stripmining of Screma
stripmineStmt k env sched (Let pat aux
      (Op (F.Screma mm inp_nms (ScremaForm map_lam [] []))))
  | Just (sched', sched'') <- validStripForDim env k mm sched = do
  -- ^ Stripmining a map statement
  stripmineLambda (k+1) env sched'' map_lam >>=
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
stripmineStmt k env sched (Let pat aux
    (Op (F.Screma mm inp_nms (ScremaForm map_lam [] [Reduce _ red_lam nes]))))
  |  Just (sched', sched'') <- validStripForDim env k mm sched,
     sig_lens <- zip (signals sched') (dimlens sched') = do
  stripmineLambda (k+1) env sched'' map_lam >>=
    maybe (pure Nothing) (mkResFromMLam sig_lens)
  where
    mkResFromMLam sig_lens (prologue_stms1, env1, map_lam1) = do
      mres <- recStripRedomap env1 sig_lens map_lam1 (pat,nes) pe0
      maybe (pure Nothing) (joinMbRes prologue_stms1) mres
    -- Core Function for Stripmining a Red o Map: Base Case
    recStripRedomap env1 [] map_lam1 _ cur_ind = do
      let (red_acc, red_els) = splitAt (length (lambdaReturnType red_lam)) $ lambdaParams red_lam
      scope <- askScope
      inp_tps <- mapM lookupType inp_nms
      loop_body <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams red_acc) $ do
          -- let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
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
    recStripRedomap env1 ((sig,dlen): schd') map_lam1 (pat_res, acc_ini) cur_ind = do
      scope <- askScope
      i_p <- newParam ("i"++show k) $ Prim i64ptp
      pat_ps  <- mapM (newParam ("stripRedpat"++show k) . patElemDec) $ patElems pat
      let pat_res' = Pat $ zipWith PatElem (map paramName pat_ps) (map paramDec pat_ps)
          cur_ind' = addPes cur_ind $ mulPes (LeafExp (paramName i_p) i64ptp) $ foldl mulPes pe1 $ map snd schd'
      red_acc' <- if null schd'
                  then pure $ take (length (lambdaReturnType red_lam)) $ lambdaParams red_lam
                  else mapM (newParam ("stripacc"++show k) . patElemDec) $ patElems pat
      rec_res <- recStripRedomap env1 schd' map_lam1 (pat_res', map (Var . paramName) red_acc') cur_ind'
      case (sig, rec_res) of
        (3, Just (rec_prologue, rec_env, rec_stms)) -> do
        -- ^ the case of a sequential subdimension, i.e., generating a loop
          (w, w_stms) <- runBuilder $ localScope scope $ letSubExp ("w"++show k) =<< toExp dlen
          loop_body <-
            runBodyBuilder $ localScope (scope <> scopeOfLParams (i_p: red_acc')) $ do
              addStms rec_stms
              pure $ if null schd' then bodyResult $ lambdaBody red_lam
                     else map (subExpRes . Var . patElemName) $ patElems pat_res'
          let loop_form  = ForLoop (paramName i_p) Int64 w
              loop_fargs = map toFParam red_acc'
              loop_stm   = Let pat_res aux $ Loop (zip loop_fargs acc_ini) loop_form loop_body
          pure $ Just (rec_prologue, rec_env, w_stms Sq.|> loop_stm)
        _ -> pure Nothing

--------------------------------------------------------------------------
--- Stripmining Other Statements
--------------------------------------------------------------------------
 
stripmineStmt _k env _sched stm@(Let pat aux e) = do
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

toFParam :: LParam SOACS -> FParam SOACS
toFParam p = Param (paramAttrs p) (paramName p) $ toDecl (paramDec p) Unique

peFromSeI64 :: SubExp -> PrimExp VName
peFromSeI64 (Constant pv) = ValueExp pv
peFromSeI64 (Var vnm) = LeafExp vnm i64ptp

addPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
addPes e1 e2 | e1 == pe0 = e2
addPes e1 e2 | e2 == pe0 = e1
addPes e1 e2 = BinOpExp (Add Int64 OverflowUndef) e1 e2

mulPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
mulPes e1 e2 | e1 == pe1 = e2
mulPes e1 e2 | e2 == pe1 = e1
mulPes e1 e2 = BinOpExp (Mul Int64 OverflowUndef) e1 e2 

minPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
minPes e1 e2 | e1 == e2 = e1
minPes e1 e2 = BinOpExp (SMin Int64) e1 e2

invPerm :: [Int] -> [Int]
invPerm xs =
  map f [0..length xs-1]
  where
    f i | Just ind <- L.elemIndex i xs = ind
    f _ = error "Violation of assumed permutation semantics"

mkArrIndexingExp :: Env -> VName -> Type -> SubExp -> Exp SOACS
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


--------------------------------------------------------------------------
--- GARBAGE CODE
--------------------------------------------------------------------------

{--
applyStripmining env sched (H.Screma m inps form) = do
  soac <- stripmineRec 0 env sched $ F.Screma m (map selVName inps) form
  case soac of
    Just (F.Screma sz [iot] form') -> do
      let tp = Array i64ptp (Shape [sz]) NoUniqueness
          hinps = H.Input H.noTransforms iot tp
      pure $ Just $ H.Screma sz [hinps] form'
    _ -> pure Nothing
  where
    selVName (H.Input _ vnm _) = vnm
--}


{--
addNewMap :: (HasScope SOACS m, MonadFreshNames m) => Env -> PrimExp VName -> m (VName, SubExp, Env)
addNewMap len_pe = do
  scope <- askScope
  ((w,iotnm), stms) <-
    runBuilder $ localScope scope $ do
      w <- letSubExp "sz" =<< toExp len_pe
      iotnm <- letExp "iota" . BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
  let iotas' = M.insert iotanm (len_pe, w, stms) $ iotas env
  pure ( iotnm, w, Env { iotas = iotas' } )
--}


{--
stripmineRec ::
    (HasScope SOACS m, MonadFreshNames m) =>
    Int -> Env -> HLSched -> F.SOAC SOACS -> m (Maybe (F.SOAC SOACS))
stripmineRec k env sched soac@(F.Screma m inps form)
  | q <- countSame $ origids sched,
    (sched', sched'') <- splitAtSched q sched, 
    validSize (dimlens sched') = do
  case form of
    -- 1. the common case of map stripmining
    H.ScremaForm map_lam [] [] -> do
      
      env' <- foldM addNewIotaToEnv env (dimlens sched')
      pure $ Just soac
    _ -> error "Stripmining anything other than map is not implemented yet!"
  where
    peFromSe (Constant pv) = ValueExp pv
    peFromSe (Var vnm) = LeafExp vnm i64ptp
    countSame [] = 0
    countSame (i:ctarr) =
      if k == i
      then 1 + countSame ctarr
      else 0
    validSize dims
      | Just nms <- M.lookup (peFromSe m) (appTilesFwd env),
        penms <- S.fromList $ map (`LeafExp` i64ptp) $ namesToList nms,
        length dims == S.size penms,
        penms == S.fromList dims = True
    validSize _ = False
--
stripmineRec _ _ _ _ = pure Nothing

addNewIotaToEnv :: (HasScope SOACS m, MonadFreshNames m) => Env -> PrimExp VName -> m (VName, SubExp, Env)
addNewIotaToEnv len_pe = do
  scope <- askScope
  ((w,iotnm), stms) <-
    runBuilder $ localScope scope $ do
      w <- letSubExp "sz" =<< toExp len_pe
      iotnm <- letExp "iota" . BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
  let iotas' = M.insert iotanm (len_pe, w, stms) $ iotas env
  pure ( iotnm, w, Env { iotas = iotas' } )
--}

{--
buildMapNest (k, inp_nm_tps, mm) env [n] cur_ind lam = do
  i_p <- newParam ("i"++show k) $ Prim i64ptp
  let i_pe = LeafExp (paramName i_p) i64ptp
      cur_ind' = addPes cur_ind i_pe
  scope <- askScope
  ((w,iotnm), prologue_stms) <-
    runBuilder $ localScope scope $ do
      w <- letSubExp ("w"++show k) =<< toExp n
      iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
      -- 1. build a body that calls the screma
  lam_bdy <-
    runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
      let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
      i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
      forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, _inp_tp)) -> do
        letBind (Pat [PatElem (paramName lp) (paramDec lp)]) $
                  BasicOp $ Index inp_nm (Slice [DimFix i_flat])
      addStms $ bodyStms $ lambdaBody lam
      pure $ bodyResult $ lambdaBody lam
  -- build the lambda and the result screma
  let new_lam = Lambda [i_p] (lambdaReturnType lam) lam_bdy
      soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
  pure $ Just (prologue_stms, env, soac)
--
buildMapNest (k, inp_nm_tps, mm) env (n:m:ms) cur_ind lam = do
  let ns = m:ms
  i_p <- newParam ("i"++show k) $ Prim i64ptp
  let i_pe = LeafExp (paramName i_p) i64ptp
      i_mul_ns = mulPes i_pe $ foldl mulPes pe1 ns
      cur_ind' = addPes cur_ind i_mul_ns
  rec_res <- buildMapNest (k, inp_nm_tps, mm) env ns cur_ind' lam
  case rec_res of
    Just (prev_stms, prev_env, prev_soac@(F.Screma prev_w _ (ScremaForm prev_lam _ _))) -> do
      scope <- askScope
      -- 0. build the iota for this screma
      ((w,iotnm), prologue_stms) <-
        runBuilder $ localScope scope $ do
          w <- letSubExp ("w"++show k) =<< toExp n
          iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
          pure (w, iotnm)
      -- 1. build a body that calls the screma
      lam_bdy <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
          prev_soac_res <- letTupExp' ("map"++show k++"strip") $ Op prev_soac
          pure $ map subExpRes prev_soac_res
      -- 2. build the resulting screma
      let lam_rts = map (`arrayOfRow` prev_w) $ lambdaReturnType prev_lam
          new_lam = Lambda [i_p] lam_rts lam_bdy
          soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
      pure $ Just (prologue_stms <> prev_stms, prev_env, soac)
    -- end ^
    _ -> pure Nothing
--
-- there is still a lot of common code, clean it up please!!!
--
buildMapNest _ _ [] _ _ =
  error "buildMapNest: unreachable case reached!"
--}

{--
          -- 1. build a body that calls the screma
          lam_bdy <-
            runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
              prev_soac_res <- letTupExp' ("map"++show k++"strip") $ Op prev_soac
              lam_res_tps <- mapM getTypeSE prev_soac_res
              trace ("\nDEBUG: \n"++prettyString lam_res_tps) $
                pure $ map subExpRes prev_soac_res
          -- 2. build the lambda and the resulting screma
          -- lam_rts <- localScope scope $ do
          --              mapM getTypeSE $ map resSubExp $ bodyResult lam_bdy
          let lam_rts = map (`arrayOfRow` prev_w) $ lambdaReturnType prev_lam
              new_lam = Lambda [i_p] lam_rts lam_bdy
              soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
          pure $ Just (prologue_stms <> prev_stms, prev_env, soac)
--}

{--
      -- 1. adjust the lambda body
      lam_bdy <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
          let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
          forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, _inp_tp)) -> do
            letBind (Pat [PatElem (paramName lp) (paramDec lp)]) $
                      BasicOp $ Index inp_nm (Slice [DimFix i_flat])
          addStms $ bodyStms $ lambdaBody lam
          pure $ bodyResult $ lambdaBody lam
      -- 2. build the lambda and the result screma
      let new_lam = Lambda [i_p] (lambdaReturnType lam) lam_bdy
          soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
      pure $ Just (prologue_stms, env, soac)
--}


{--
    getTypeSE (Constant pval) = pure (Prim (primValueType pval))
    getTypeSE (Var nm) = lookupType nm     
--}

{--
applyStripmining env sched (pat, aux, H.Screma mm inps (H.ScremaForm map_lam [] []))
  | Just (sched', sched'') <- validStripForDim env 0 mm sched,
    not (any hasTransform inps) = do
  --
  let inp_arr_nm_tps = map extractNmTp inps
  mres_lam <- stripmineLambda 1 env sched'' map_lam
  case mres_lam of
    Just (_prologue_stms1, env1, lam') -> do
      mres_nest <- stripmineMap (0,inp_arr_nm_tps,mm) env1 (dimlens sched') pe0 lam'
      case mres_nest of
        Just (prologue_stms2, _env2, soac@(F.Screma m' [nm] form'@(F.ScremaForm _ [] []))) -> do
        -- ^ by construction the resulting soac has one iota-array arg
          let tp = Array i64ptp (Shape [m']) NoUniqueness
              hinp = H.Input H.noTransforms nm tp
          trace ("stripmined soac: \n" ++ prettyString soac ++
                 "\n prologues-stms:\n"++ prettyString prologue_stms2 ) $
           pure $ Just $ H.Screma m' [hinp] form'
        _ -> pure Nothing
    _ -> pure Nothing
  where
    hasTransform (H.Input transf _ _) = not $ H.nullTransforms transf
    extractNmTp (H.Input _ nm tp) = (nm,tp)
--}

--
{--
    baseCase map_lam1 (i_p, red_accs, red_els) cur_ind' (prologue_stms, env1) = do
      scope <- askScope
      inp_tps <- mapM lookupType inp_nms
      loop_body <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams (i_p: red_accs)) $ do
--          let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
--          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
          forM_ (zip3 inp_nms inp_tps (lambdaParams map_lam1)) $ \ (arr, tp, arg) ->
            -- codegen of map's lambda arguments
            letBind (Pat [PatElem (paramName arg) (paramDec arg)]) $
                mkArrIndexingExp env arr tp $ Var $ paramName i_p
          addStms $ bodyStms $ lambdaBody map_lam1
          forM_ (zip red_els $ bodyResult $ lambdaBody map_lam1) $ \ (rlam_p, map_res) ->
            -- codegen of copy stamts connecting the map's lambda res with reduce's inp
            letBind (Pat [PatElem (paramName rlam_p) (paramDec rlam_p)]) $
                BasicOp $ SubExp $ resSubExp map_res
          addStms $ bodyStms $ lambdaBody red_lam
          pure $ bodyResult $ lambdaBody red_lam
      let loop_form  = ForLoop (paramName i_p) Int64 mm
          loop_fargs = map toFParam red_accs
          loop_stm   = Let pat aux $ Loop (zip loop_fargs nes) loop_form loop_body
      pure $ Just (prologue_stms, env1, Sq.singleton loop_stm)
--}


