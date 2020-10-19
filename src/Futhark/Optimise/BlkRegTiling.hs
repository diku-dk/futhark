{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform a restricted form of block+register tiling corresponding to
--   the following pattern:
--     * a redomap is quasi-perfectly nested inside a kernel with at
--       least two parallel dimension (the perfectly nested restriction
--       is relaxed a bit to allow for SGEMM);
--     * all streamed arrays of redomap are one dimensional;
--     * all streamed arrays are variant to exacly one of the two
--       innermost parallel dimensions, and conversely for each of
--       the two innermost parallel dimensions, there is at least
--       one streamed array variant to it;
--     * the stream's result is a tuple of scalar values, which are
--       also the "thread-in-space" return of the kernel.
--     * We have further restrictions that in principle can be relaxed:
--          the redomap has exactly two array input
--          the redomap produces one scalar result
--          the kernel produces one scalar result
module Futhark.Optimise.BlkRegTiling
       ( mm_BlkRegTiling )
       where
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe
import Debug.Trace

import Futhark.MonadFreshNames
import Futhark.IR.Kernels
import Futhark.Tools
import Futhark.Transform.Rename

type TileM = ReaderT (Scope Kernels) (State VNameSource)
type VarianceTable = M.Map VName Names

mm_BlkRegTiling :: Stm Kernels -> TileM (Maybe (Stms Kernels, Stm Kernels))
mm_BlkRegTiling (Let pat aux (Op (SegOp (SegMap SegThread{} seg_space ts old_kbody))))
  | KernelBody () kstms [Returns ResultMaySimplify (Var res_nm)] <- old_kbody,
    -- check kernel has one result of primitive type
    res_tp : [] <- ts,
    primType res_tp,

    -- build the variance table, that records, for
    -- each variable name, the variables it depends on
    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,

    -- check that the code fits the pattern having:
    -- some `code1`, followed by one Screma SOAC, followed by some `code2`
    (code1, Just screma_stmt, code2) <- matchCodeStreamCode kstms,

    Let pat_redomap _ (Op _) <- screma_stmt,

    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (common_dim, arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,

    -- check that exactly two 1D arrays are streamed thorugh redomap,
    -- and the result of redomap is one scalar
    length arrs == 2 && length red_nes == 1,
    inp_A  : inp_B  : [] <- arrs,
    map_t1t : map_t2t : [] <- map paramDec $ lambdaParams map_lam,
    red_t1 : _ : [] <- map paramDec $ lambdaParams red_lam,
    primType map_t1t && primType map_t2t && primType red_t1,
    map_t1 <- elemType map_t1t,
    map_t2 <- elemType map_t2t,
--    map_t1 : map_t2 : [] <- map (elemType . paramDec) (lambdaParams map_lam),

    -- checks that the input arrays to redomap are variant to
    -- exactly one of the two innermost dimensions of the kernel
    Just _ <- isInvarTo1of2InnerDims mempty seg_space variance arrs,

    -- get the variables on which the first result of redomap depends on
    redomap_orig_res : [] <- patternValueElements pat_redomap,
    Just res_red_var <- M.lookup (patElemName redomap_orig_res) variance, -- variance of the reduce result

    -- we furthermore check that code1 is only formed by
    -- 1. statements that slice some globally-declared arrays
    --    to produce the input for the redomap, and
    -- 2. potentially some statements on which the redomap
    --    is independent; these are recorded in `code2''`
    Just (code2'', tab_inv_stm) <- foldl(processIndirections (namesFromList arrs) res_red_var)
                                        (Just (Seq.empty, M.empty)) code1,
    -- identify load_A, load_B
    tmp_stms <- mapMaybe (\nm -> M.lookup nm tab_inv_stm) arrs,
    length tmp_stms == length arrs,
    load_A : load_B : [] <- tmp_stms,

    code1' <- stmsFromList $ (stmsToList code1) \\ (stmsToList code2''),
    code2' <- code2'' <> code2,
    trace ("Cosmin debug: code1':\n"++pretty code1'++"\ncode 2:\n"++pretty code2++
           "\ncode2': \n"++pretty code2'++"\n load_A: "++pretty load_A++
           "\n load_B: "++pretty load_B++"\n redomap orig result"++pretty redomap_orig_res++
           "\n Cosmin Kernel return: "++pretty res_nm++" type: "++pretty ts) $
      True, -- TODO: remove the need for these assumptions !

    -- we get the global-thread id for the two inner dimensions,
    --   as we are probably going to use it in code generation
    (gtid_x, width_B) : (gtid_y, height_A) : rem_outer_dims_rev <- reverse $ unSegSpace seg_space,
    rem_outer_dims <- reverse rem_outer_dims_rev,

    -- null rem_outer_dims, -- TODO: remove the need for this assumption !

    -- sanity check that the reduce part is not missing
    not $ null red_nes = do
      let red_ne : _ = red_nes
      red_t <- subExpType red_ne
      let one_se = Constant $ IntValue $ Int32Value 1

      ---- in this binder: host code and outer seggroup (ie. the new kernel) ----
      (new_kernel, host_stms) <- runBinder $ do -- host code

        tk_name    <- nameFromString . pretty <$> newVName "Tk"
        tx_name    <- nameFromString . pretty <$> newVName "Tx"
        ty_name    <- nameFromString . pretty <$> newVName "Ty"
        rx_name    <- nameFromString . pretty <$> newVName "Rx"
        ry_name    <- nameFromString . pretty <$> newVName "Ry"
        tk         <- letSubExp "Tk" $ Op $ SizeOp $ GetSize tk_name SizeTile
        tx         <- letSubExp "Tx" $ Op $ SizeOp $ GetSize tx_name SizeTile
        ty         <- letSubExp "Ty" $ Op $ SizeOp $ GetSize ty_name SizeTile
        rx         <- letSubExp "Rx" $ Op $ SizeOp $ GetSize rx_name SizeRegTile
        ry         <- letSubExp "Ry" $ Op $ SizeOp $ GetSize ry_name SizeRegTile

        tk_div_tx  <- letSubExp "tk_div_tx" =<< ceilDiv tk tx
        tk_div_ty  <- letSubExp "tk_div_ty" =<< ceilDiv tk ty

        tx_rx      <- letSubExp "TxRx" =<< toExp (primFromSe tx * primFromSe rx)
        ty_ry      <- letSubExp "TyRy" =<< toExp (primFromSe ty * primFromSe ry)

        a_loc_sz   <- letSubExp "a_loc_sz" =<<
                         toExp (primFromSe ty * primFromSe ry * primFromSe tk)

        b_loc_sz   <- letSubExp "b_loc_sz" =<<
                        toExp (primFromSe tk * primFromSe tx * primFromSe rx)

        gridDim_x  <- letSubExp "gridDim_x"  =<< ceilDiv width_B  tx_rx
        gridDim_y  <- letSubExp "gridDim_y"  =<< ceilDiv height_A ty_ry
        let gridxy_pexp = primFromSe gridDim_y * primFromSe gridDim_x
        let grid_pexp = foldl (\ x d -> primFromSe d * x) gridxy_pexp $
                              map snd rem_outer_dims_rev
        grid_size  <- letSubExp "grid_size"  =<< toExp grid_pexp
        group_size <- letSubExp "group_size" =<< toExp (primFromSe ty * primFromSe tx)
        let segthd_lvl = SegThread (Count grid_size) (Count group_size) SegNoVirtFull

        gid_x      <- newVName "gid_x"
        gid_y      <- newVName "gid_y"
        gid_flat   <- newVName "gid_flat"

        ---- in this binder: outer seggroup ----
        (ret_seggroup, stms_seggroup) <- runBinder $ do

          iii <- letExp "iii" =<< toExp (LeafExp gid_y int32 * primFromSe ty_ry)
          jjj <- letExp "jjj" =<< toExp (LeafExp gid_x int32 * primFromSe tx_rx)

          -- initialize register mem with neutral elements.
          cssss_list <- segMap2D "cssss" segthd_lvl ResultPrivate (ty, tx) $ \_ -> do
            css_init <- scratch "css_init" (elemType red_t) [ry, rx]
            css <- forLoop ry [css_init] $ \i [css_merge] -> do
              css' <- forLoop rx [css_merge] $ \j [css_merge'] -> do
                css'' <- update' "css" css_merge' [i, j] red_ne
                resultBodyM [Var css'']
              resultBodyM [Var css']
            return [Var css]
          let [cssss] = cssss_list

          a_loc_init <- scratch "A_loc" map_t1 [a_loc_sz]
          b_loc_init <- scratch "B_loc" map_t2 [b_loc_sz]

          let kkLoopBody kk0 (thd_res_merge, a_loc_init', b_loc_init') epilogue = do
               kk <- letExp "kk" =<< toExp (LeafExp kk0 int32 * primFromSe tk)
               a_loc <- forLoop ry [a_loc_init'] $ \i0 [a_loc_merge] -> do
                 loop_a_loc <- forLoop tk_div_tx [a_loc_merge] $ \k0 [a_loc_merge'] -> do

                   scatter_a_loc <- segScatter2D "A_glb2loc" a_loc_sz a_loc_merge'
                                      segthd_lvl (ty, tx) $ \(thd_y, thd_x) -> do

                       k <- letExp "k" =<< toExp (LeafExp thd_x int32 +
                              LeafExp k0 int32 * primFromSe tx)
                       i <- letExp "i" =<< toExp (LeafExp thd_y int32 +
                              LeafExp i0 int32 * primFromSe ty)

                       letBindNames [gtid_y] =<< toExp (LeafExp iii int32 + LeafExp i int32)
                       a_col_idx <- letExp "A_col_idx" =<< toExp (LeafExp kk int32 + LeafExp k int32)

                       a_elem <- letSubExp "A_elem" =<<
                                eIf (toExp $ LeafExp gtid_y int32 .<. primFromSe height_A .&&.
                                             if epilogue then
                                               LeafExp a_col_idx int32 .<. primFromSe common_dim
                                             else true)
                                    (do addStm load_A
                                        res <- index "A_elem" inp_A [a_col_idx]
                                        resultBodyM [Var res])
                                    (eBody [eBlank $ Prim map_t1])
                       a_loc_ind <- letSubExp "a_loc_ind" =<<
                                eIf (toExp $ LeafExp k int32 .<. primFromSe tk)
                                    (toExp (LeafExp k int32 + LeafExp i int32 * primFromSe tk)
                                      >>= letTupExp' "loc_fi" >>= resultBodyM)
                                    (eBody [pure $ BasicOp $ SubExp $ intConst Int32 (-1)])
                       return (a_elem, a_loc_ind)
                   resultBodyM $ map Var scatter_a_loc
                 resultBodyM [Var loop_a_loc]

               -- copy B from global to shared memory
               b_loc <- forLoop tk_div_ty [b_loc_init'] $ \k0 [b_loc_merge] -> do
                 loop_b_loc <- forLoop rx [b_loc_merge] $ \j0 [b_loc_merge'] -> do
                   scatter_b_loc <- segScatter2D "B_glb2loc" b_loc_sz b_loc_merge'
                         segthd_lvl (ty, tx) $ \(thd_y, thd_x) -> do

                     k <- letExp "k" =<< toExp (LeafExp thd_y int32 +
                            LeafExp k0 int32 * primFromSe ty)
                     j <- letExp "j" =<< toExp (LeafExp thd_x int32 +
                            LeafExp j0 int32 * primFromSe tx)

                     letBindNames [gtid_x]  =<< toExp (LeafExp jjj int32 + LeafExp j int32)
                     b_row_idx <- letExp "B_row_idx" =<< toExp (LeafExp kk int32 + LeafExp k int32)

                     b_elem <- letSubExp "B_elem" =<<
                                eIf (toExp $ LeafExp gtid_x int32 .<. primFromSe width_B .&&.
                                             if epilogue then
                                               LeafExp b_row_idx int32 .<. primFromSe common_dim
                                             else true)
                                    (do addStm load_B
                                        res <- index "B_elem" inp_B [b_row_idx]
                                        resultBodyM [Var res])
                                    (eBody [eBlank $ Prim map_t2])

                     b_loc_ind <- letSubExp "b_loc_ind" =<<
                              eIf (toExp $ LeafExp k int32 .<. primFromSe tk)
                                  (toExp (LeafExp j int32 + LeafExp k int32 * primFromSe tx_rx)
                                     >>= letTupExp' "loc_fi" >>= resultBodyM)
                                  (eBody [pure $ BasicOp $ SubExp $ intConst Int32 (-1)])
                     return (b_elem, b_loc_ind)
                   resultBodyM $ map Var scatter_b_loc
                 resultBodyM [Var loop_b_loc]

               -- inner loop updating this thread's accumulator (loop k in mmm_kernels).
               thd_acc <- forLoop tk [thd_res_merge] $ \k [acc_merge] -> do
                 resultBodyM =<< letTupExp' "foo" =<<
                   eIf (toExp $ if epilogue then LeafExp kk int32 + LeafExp k int32
                                                 .<. primFromSe common_dim
                                else true) -- if in prologue, always compute redomap.
                       (do reg_mem <- segMap2D "reg_mem" segthd_lvl
                                        ResultPrivate (ty, tx) $ \(ltid_y, ltid_x) -> do
                             asss_init <- scratch "asss_init" map_t1 [ry]
                             bsss_init <- scratch "bsss_init" map_t2 [rx]

                             asss <- forLoop ry [asss_init] $ \i [asss_merge] -> do

                               a_loc_ind <- letExp "a_loc_ind" =<< toExp (LeafExp k int32 +
                                              (LeafExp ltid_y int32 * primFromSe ry +
                                               LeafExp i int32) * primFromSe tk)

                               asss <- index "A_loc_elem" a_loc [a_loc_ind]
                                         >>= update "asss" asss_merge [i]
                               resultBodyM [Var asss]

                             bsss <- forLoop rx [bsss_init] $ \j [bsss_merge] -> do

                               b_loc_ind <- letExp "b_loc_ind" =<< toExp (LeafExp j int32 +
                                              LeafExp k int32 * primFromSe tx_rx +
                                              LeafExp ltid_x int32 * primFromSe rx)

                               bsss <- index "B_loc_elem" b_loc [b_loc_ind]
                                         >>= update "bsss" bsss_merge [j]
                               resultBodyM [Var bsss]
                             return $ map Var [asss, bsss]

                           let [asss, bsss] = reg_mem

                           -- the actual redomap.
                           redomap_res <- segMap2D "redomap_res" segthd_lvl
                                            ResultPrivate (ty, tx) $ \(ltid_y, ltid_x) -> do

                             as <- index "as" asss [ltid_y, ltid_x]
                             bs <- index "bs" bsss [ltid_y, ltid_x]
                             css_init <- index "css_init" acc_merge [ltid_y, ltid_x]

                             css <- forLoop ry [css_init] $ \i [css_merge] -> do

                               css <- forLoop rx [css_merge] $ \j [css_merge'] -> do
                                 resultBodyM =<< letTupExp' "foo" =<<
                                   eIf ( toExp $ LeafExp iii int32 + LeafExp i int32 +
                                                   primFromSe ry * LeafExp ltid_y int32
                                                   .<. primFromSe height_A .&&.
                                                 LeafExp jjj int32 + LeafExp j int32 +
                                                   primFromSe rx * LeafExp ltid_x int32
                                                   .<. primFromSe width_B
                                       )

                                       ( do a <- index "a" as [i]
                                            b <- index "b" bs [j]
                                            c <- index "c" css_merge' [i, j]
                                            
                                            map_res  <- newVName "map_res"
                                            map_lam' <- renameLambda map_lam
                                            red_lam' <- renameLambda red_lam
                                            
                                            addStms $ rebindLambda map_lam' [a, b] map_res
                                                   <> rebindLambda red_lam' [c, map_res] c
                                            
                                            css <- update "css" css_merge' [i, j] c
                                            
                                            resultBodyM [Var css])
                                       ( resultBodyM [Var css_merge'] )
                               resultBodyM [Var css]
                             return [Var css]

                           resultBodyM $ map Var redomap_res
                       )
                       (resultBodyM [Var acc_merge])
               return [thd_acc, a_loc, b_loc]

          -- build prologue.
          full_tiles <- letExp "full_tiles" $ BasicOp $ BinOp (SQuot Int32) common_dim tk
          prologue_res_list <-
            forLoop' (Var full_tiles) [cssss, a_loc_init, b_loc_init] $
            \kk0 [thd_res_merge, a_loc_merge, b_loc_merge] -> do

            process_full_tiles <-
              kkLoopBody kk0 (thd_res_merge, a_loc_merge, b_loc_merge) False

            resultBodyM $ map Var process_full_tiles

          let prologue_res : a_loc_reuse : b_loc_reuse : _ = prologue_res_list

          -- build epilogue.
          epilogue_res_list <- kkLoopBody full_tiles (prologue_res, a_loc_reuse, b_loc_reuse) True

          let redomap_res : _ = epilogue_res_list

          -- support for non-empty code2'
          --  segmap (ltid_y < ty, ltid_x < tx) {
          --    for i < ry do
          --      for j < rx do
          --        res = if (iii+ltid_y*ry+i < height_A && jjj+ltid_x*rx+j < width_B)
          --              then code2' else dummy
          --        final_res[i,j] = res
          epilogue_res <-
            if patElemName redomap_orig_res == res_nm
            then do return redomap_res -- epilogue_res_list
            else do
              rssss_list <- segMap2D "rssss" segthd_lvl ResultPrivate (ty, tx) $ \(ltid_y, ltid_x) -> do
                rss_init <- scratch "rss_init" (elemType res_tp) [ry, rx]
                css <- index "redomap_thd" redomap_res [ltid_y, ltid_x]
                ii <- letExp "ii" =<< toExp (LeafExp iii int32 + LeafExp ltid_y int32 * primFromSe ry)
                jj <- letExp "jj" =<< toExp (LeafExp jjj int32 + LeafExp ltid_x int32 * primFromSe rx)
                rss <- forLoop ry [rss_init] $ \i [rss_merge] -> do
                  rss' <- forLoop rx [rss_merge] $ \j [rss_merge'] -> do
                    c <- index "redomap_elm" css [i, j]
                    cpy_stm <- mkLetNamesM [patElemName redomap_orig_res] $ BasicOp $ SubExp $ Var c
                    addStm cpy_stm
                    letBindNames [gtid_y] =<< toExp (LeafExp ii int32 + LeafExp i int32)
                    letBindNames [gtid_x] =<< toExp (LeafExp jj int32 + LeafExp j int32)

                    res_el <- letSubExp "res_elem" =<<
                                eIf (toExp $ LeafExp gtid_y int32 .<. primFromSe height_A .&&.
                                             LeafExp gtid_x int32 .<. primFromSe width_B
                                    )
                                    (do addStms code2'
                                        resultBodyM [Var res_nm])
                                    (eBody [eBlank $ res_tp])
                    rss'' <- update' "rss" rss_merge' [i, j] res_el
                    resultBodyM [Var rss'']
                  resultBodyM [Var rss']
                return [Var rss]
              let rssss : _ = rssss_list
              return rssss
          -- Reshape (ShapeChange SubExp) VName
          -- buckets'' <- certifying c $ letExp (baseString buckets') $
          -- I.BasicOp $ I.Reshape (reshapeOuter [DimCoercion w_img] 1 b_shape) buckets'
          let shp_chng = map (\_ -> DimNew one_se) rem_outer_dims ++ [DimNew ty, DimNew tx] ++
                         map (\_ -> DimNew one_se) rem_outer_dims ++ [DimNew ry, DimNew rx]
          epilogue_res_reshaped <- letExp "res_reshaped" $ BasicOp $ Reshape shp_chng epilogue_res
          let regtile_ret_dims =
                ( map (\(_, sz) -> (sz, one_se, one_se)) rem_outer_dims ) ++
                [(height_A, ty, ry), (width_B, tx, rx)]
          -- TODO: RegTileReturns is still missing boundary checks.
          return [RegTileReturns regtile_ret_dims epilogue_res_reshaped]

        let level' = SegGroup (Count grid_size) (Count group_size) SegNoVirt
            space' = SegSpace gid_flat (rem_outer_dims ++ [(gid_y, gridDim_y), (gid_x, gridDim_x)])
            kbody' = KernelBody () stms_seggroup ret_seggroup
        return $ Let pat aux $ Op $ SegOp $ SegMap level' space' ts kbody'

      --trace ("COSMIN kernel: "++pretty new_kernel) $
      return $ Just (host_stms, new_kernel)

mm_BlkRegTiling _ = do return Nothing

primFromSe :: SubExp -> PrimExp VName
primFromSe se = primExpFromSubExp int32 se

ceilDiv :: MonadBinder m => SubExp -> SubExp -> m (Exp (Lore m))
ceilDiv x y = eDivRoundingUp Int32 (eSubExp x) (eSubExp y)

scratch :: MonadBinder m => String -> PrimType -> [SubExp] -> m VName
scratch se_name t shape = letExp se_name $ BasicOp $ Scratch t shape

-- index an array with indices given in outer_indices; any inner
-- dims of arr not indexed by outer_indices are sliced entirely
index :: MonadBinder m => String -> VName -> [VName] -> m VName
index se_desc arr outer_indices = do
  arr_t <- lookupType arr
  let shape = arrayShape arr_t

  let inner_dims = shapeDims $ stripDims (length outer_indices) shape
  let inner_slices = map (\inner_dim -> DimSlice  (intConst Int32 0)
                                        inner_dim (intConst Int32 1)) inner_dims

  let indices = map (DimFix . Var) outer_indices ++ inner_slices
  letExp se_desc $ BasicOp $ Index arr indices

update :: MonadBinder m => String -> VName -> [VName] -> VName -> m VName
update se_desc arr indices new_elem = update' se_desc arr indices (Var new_elem)

update' :: MonadBinder m => String -> VName -> [VName] -> SubExp -> m VName
update' se_desc arr indices new_elem =
  letExp se_desc $ BasicOp $ Update arr (map (DimFix . Var) indices) new_elem

forLoop' :: SubExp            -- loop var
        -> [VName]            -- loop inits
        -> (VName -> [VName]  -- (loop var -> loop inits -> loop body)
             -> Binder Kernels (Body Kernels))
        -> Binder Kernels [VName]
forLoop' i_bound merge body = do
  i <- newVName "i"     -- could give this as arg to the function

  let loop_form = ForLoop i Int32 i_bound []

  merge_ts   <- mapM lookupType merge
  loop_inits <- mapM (\merge_t -> newParam "merge" $ toDecl merge_t Unique) merge_ts

  loop_body <- runBodyBinder $ inScopeOf loop_form $
    localScope (scopeOfFParams loop_inits) $ body i $ map paramName loop_inits

  letTupExp "loop" $ DoLoop [] (zip loop_inits $ map Var merge)
                            loop_form loop_body

forLoop :: SubExp -> [VName] -> (VName -> [VName] -> Binder Kernels (Body Kernels))
        -> Binder Kernels VName
forLoop i_bound merge body = do
  res_list <- forLoop' i_bound merge body
  return $ head res_list


-- given a lambda "lam", a list "new_params" of new
-- parameters which should be applied to the lambda,
-- and a VName "res_name" which the lambda result should
-- be bound to:
--   creates Stms corresponding to binding of new_params,
--   lambda body, and binding of lambda result to res_name.
rebindLambda :: Lambda Kernels
             -> [VName]
             -> VName
             -> Stms Kernels
rebindLambda lam new_params res_name =
  (stmsFromList $
    map (\(ident, new_param) ->
              mkLet [] [ident] $ BasicOp $ SubExp $ Var new_param)
        $ zip idents new_params)
  <> bodyStms lam_body
  <> oneStm (mkLet [] [Ident res_name lam_ret_type] $ BasicOp $ SubExp lam_res)
  where
    (lam_params, lam_body, lam_ret_type : _) =
      (lambdaParams lam, lambdaBody lam, lambdaReturnType lam)
    idents = map (\param -> Ident (paramName param) (paramDec param))
                 lam_params
    lam_res : _ = bodyResult lam_body

-- | Tries to identify the following pattern:
--   code followed by some Screma followed by more code.
matchCodeStreamCode :: Stms Kernels ->
                       (Stms Kernels, Maybe (Stm Kernels), Stms Kernels)
matchCodeStreamCode kstms =
  let (code1, screma, code2) = foldl (\acc stmt ->
                case (acc, stmt) of
                  ((cd1, Nothing, cd2), Let _ _ (Op (OtherOp (Screma _ _ _)))) ->
                   (cd1, Just stmt, cd2)

                  ((cd1, Nothing, cd2), _) ->
                   (cd1 ++ [stmt], Nothing, cd2)

                  ((cd1, Just strm, cd2), _) ->
                   (cd1, Just strm, cd2 ++ [stmt])
            ) ([], Nothing, []) (stmsToList kstms)
  in (stmsFromList code1, screma, stmsFromList code2)


isTileableRedomap :: Stm Kernels
         -> Maybe (SubExp, [VName],
                   (Commutativity, Lambda Kernels, [SubExp], Lambda Kernels))
isTileableRedomap stm
  | Op (OtherOp (Screma w form arrs)) <- stmExp stm,
    Just (reds, map_lam)              <- isRedomapSOAC form,
    Reduce red_comm red_lam red_nes   <- singleReduce reds,
    all (primType . rowType . paramType) $ lambdaParams red_lam,
    all (primType . rowType . paramType) $ lambdaParams map_lam,
    lambdaReturnType map_lam == lambdaReturnType red_lam, -- No mapout arrays.
    not (null arrs),
    all primType $ lambdaReturnType map_lam,
    all (primType . paramType) $ lambdaParams map_lam =
      Just (w, arrs, (red_comm, red_lam, red_nes, map_lam))
  | otherwise =
      Nothing


-- | Checks that all streamed arrays are variant to exacly one of
--   the two innermost parallel dimensions, and conversely, for
--   each of the two innermost parallel dimensions, there is at
--   least one streamed array variant to it. The result is the
--   number of the only variant parallel dimension for each array.
isInvarTo1of2InnerDims :: Names -> SegSpace -> VarianceTable -> [VName]
                       -> Maybe [Int]
isInvarTo1of2InnerDims branch_variant kspace variance arrs =
  let inner_perm0 = map varToOnly1of2InnerDims arrs
      inner_perm  = catMaybes inner_perm0
      ok1 = elem 0 inner_perm && elem 1 inner_perm
      ok2 = length inner_perm0 == length inner_perm
  in  if ok1 && ok2 then Just inner_perm else Nothing
  where varToOnly1of2InnerDims :: VName -> Maybe Int
        varToOnly1of2InnerDims arr = do
          (j, _) : (i, _) : _ <- Just $ reverse $ unSegSpace kspace
          let variant_to       = M.findWithDefault mempty arr variance
              branch_invariant = not $ nameIn j branch_variant ||
                                       nameIn i branch_variant
          if not branch_invariant then Nothing -- if i or j in branch_variant; return nothing
          else if nameIn i variant_to && not (nameIn j variant_to) then Just 0
          else if nameIn j variant_to && not (nameIn i variant_to) then Just 1
          else Nothing


varianceInStms :: VarianceTable -> Stms Kernels -> VarianceTable
varianceInStms = foldl varianceInStm

-- just in case you need the Screma being treated differently than
-- by default; previously Cosmin had to enhance it when dealing with stream.
varianceInStm :: VarianceTable -> Stm Kernels -> VarianceTable
varianceInStm v0 bnd@(Let _ _ (Op (OtherOp (Screma _ _ _))))
  | Just (_, arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap bnd =
    let v = defVarianceInStm v0 bnd
        red_args  = lambdaParams red_lam
        map_args  = lambdaParams map_lam
        card_red  = length red_nes
        acc_lam_f = take (card_red `quot` 2) red_args
        arr_lam_f = drop (card_red `quot` 2) red_args
        stm_lam   = (bodyStms $ lambdaBody map_lam) <> (bodyStms $ lambdaBody red_lam)

        v' = foldl' (\vacc (v_a, v_fm, v_fr_acc, v_fr_var) ->
                      let vrc   = oneName v_a <> M.findWithDefault mempty v_a vacc
                          vacc' = M.insert v_fm vrc vacc
                          vrc'  = oneName v_fm <> vrc
                      in  M.insert v_fr_acc (oneName v_fr_var <> vrc') $ M.insert v_fr_var vrc' vacc'
                    ) v $ zip4 arrs (map paramName map_args) (map paramName acc_lam_f) (map paramName arr_lam_f)
    in varianceInStms v' stm_lam
  | otherwise = defVarianceInStm v0 bnd

varianceInStm v0 bnd = defVarianceInStm v0 bnd

defVarianceInStm :: VarianceTable -> Stm Kernels -> VarianceTable
defVarianceInStm variance bnd =
  foldl' add variance $ patternNames $ stmPattern bnd
  where add variance' v = M.insert v binding_variance variance'
        look variance' v = oneName v <> M.findWithDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ namesToList (freeIn bnd)

-- alternatively, import TileLoops?
segMap2D :: String           -- desc
         -> SegLevel         -- lvl
         -> ResultManifest   -- manifest
         -> (SubExp, SubExp) -- (dim_x, dim_y)
         -> ((VName, VName)  -- f
             -> Binder Kernels [SubExp])
         -> Binder Kernels [VName]
segMap2D desc lvl manifest (dim_x, dim_y) f = do
  ltid_x    <- newVName "ltid_x"
  ltid_y    <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_x, ltid_y)
    ts  <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl segspace ts $ KernelBody () stms' $ map (Returns manifest) res'

segScatter2D :: String   -- desc
             -> SubExp   -- arr_size
             -> VName
             -> SegLevel -- lvl
             -> (SubExp, SubExp) -- (dim_y, dim_x)
             -> ((VName, VName) -> Binder Kernels (SubExp, SubExp)) -- f
             -> Binder Kernels [VName]
segScatter2D desc arr_size updt_arr lvl (dim_x, dim_y) f = do
  ltid_x <- newVName "ltid_x"
  ltid_y <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((t_v, res_v, res_i), stms) <- runBinder $ do
    (res_v, res_i) <- f (ltid_x, ltid_y)
    t_v <- subExpType res_v
    return (t_v, res_v, res_i)

  Body _ stms' res' <- renameBody $ mkBody stms [res_i, res_v]
  let [res_i', res_v'] = res'
  let ret  = WriteReturns [arr_size] updt_arr [([DimFix res_i'], res_v')]
  let body = KernelBody () stms' [ret]

  letTupExp desc $ Op $ SegOp $ SegMap lvl segspace [t_v] body
{--
processIndirections :: Names   -- input arrays to redomap
                    -> Names   -- variables on which the result of redomap depends on.
                    -> Maybe (Stms Kernels, M.Map VName (VName, Slice SubExp, Type))
                    -> Stm Kernels
                    -> Maybe (Stms Kernels, M.Map VName (VName, Slice SubExp, Type))
processIndirections arrs _ acc (Let patt _ (BasicOp (Index arr_nm slc)))
  | Just (ss, tab) <- acc,
    [p] <- patternValueElements patt,
    (p_nm, p_tp) <- (patElemName p, patElemType p),
    nameIn p_nm arrs,
    Array _ (Shape [_]) _ <- p_tp =
      Just (ss, M.insert p_nm (arr_nm, slc, p_tp) tab)

processIndirections _ res_red_var acc stm'@(Let patt _ _)
  | Just (ss, tab) <- acc,
    ps <- patternValueElements patt,
    all (\p -> not (nameIn (patElemName p) res_red_var)) ps =
      Just (ss Seq.|> stm', tab)
  | otherwise = Nothing
--}

processIndirections :: Names   -- input arrays to redomap
                    -> Names   -- variables on which the result of redomap depends on.
                    -> Maybe (Stms Kernels, M.Map VName (Stm Kernels))
                    -> Stm Kernels
                    -> Maybe (Stms Kernels, M.Map VName (Stm Kernels))
processIndirections arrs _ acc stm@(Let patt _ (BasicOp (Index _ _)))
  | Just (ss, tab) <- acc,
    [p] <- patternValueElements patt,
    p_nm <- patElemName p,
    nameIn p_nm arrs =
      Just (ss, M.insert p_nm stm tab)

processIndirections _ res_red_var acc stm'@(Let patt _ _)
  | Just (ss, tab) <- acc,
    ps <- patternValueElements patt,
    all (\p -> not (nameIn (patElemName p) res_red_var)) ps =
      Just (ss Seq.|> stm', tab)
  | otherwise = Nothing
