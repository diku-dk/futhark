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
module Futhark.Optimise.BlkRegTiling (mmBlkRegTiling, doRegTiling3D, matchCodeStreamCode) where

import Control.Monad
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Futhark.IR.GPU
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.MonadFreshNames
import Futhark.Optimise.TileLoops.Shared
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (splitFromEnd, takeLast)

se0 :: SubExp
se0 = intConst Int64 0

se1 :: SubExp
se1 = intConst Int64 1

se2 :: SubExp
se2 = intConst Int64 2

se4 :: SubExp
se4 = intConst Int64 4

se8 :: SubExp
se8 = intConst Int64 8

-- | Main helper function for Register-and-Block Tiling
kkLoopBody ::
  Env ->
  ( (SubExp, SubExp, SubExp, SubExp, SubExp, SubExp, SubExp, SubExp),
    SegLevel,
    [Int],
    (VName, SubExp, VName, SubExp, SubExp),
    (VName, VName),
    (Stm GPU, VName, PrimType, Stm GPU, VName, PrimType),
    (Lambda GPU, Lambda GPU)
  ) ->
  VName ->
  (VName, VName, VName) ->
  Bool ->
  Builder GPU [VName]
kkLoopBody
  env
  ( (rx, ry, tx, ty, tk, tk_div_tx, _tk_div_ty, tx_rx),
    segthd_lvl,
    var_dims,
    (gtid_x, width_B, gtid_y, height_A, common_dim),
    (iii, jjj),
    (load_A, inp_A, pt_A, load_B, inp_B, pt_B),
    (map_lam, red_lam)
    )
  kk0
  (thd_res_merge, a_shr_init', b_shr_init')
  epilogue = do
    let (map_t1, map_t2) = (pt_A, pt_B)
    kk <- letExp "kk" =<< toExp (le64 kk0 * pe64 tk)
    -- copy A to shared memory
    (a_shr, aCopyShr2Reg) <-
      copyGlb2ShMem kk (gtid_y, iii, map_t1, height_A, inp_A, load_A, a_shr_init')

    -- copy B from global to shared memory
    (b_shr, bCopyShr2Reg) <-
      copyGlb2ShMem kk (gtid_x, jjj, map_t2, width_B, inp_B, load_B, b_shr_init')

    -- inner loop updating this thread's accumulator (loop k in mmm_kernels).
    thd_acc <- forLoop_ tk [thd_res_merge] $ \k [acc_merge] ->
      letTupExp "foo"
        =<< eIf
          ( toExp $
              if epilogue
                then le64 kk + le64 k .<. pe64 common_dim
                else true -- if in prologue, always compute redomap.
          )
          ( do
              reg_mem <- segMapND "reg_mem" segthd_lvl ResultPrivate [ty, tx] $
                \[ltid_y, ltid_x] -> do
                  -- copy A from shared memory to registers
                  asss <- aCopyShr2Reg k ltid_y
                  -- copy B from shared memory to registers
                  bsss <- bCopyShr2Reg k ltid_x
                  pure $ varsRes [asss, bsss]
              let [asss, bsss] = reg_mem
              mkRedomapOneTileBody acc_merge asss bsss True
          )
          (resultBodyM [Var acc_merge])
    pure [thd_acc, a_shr, b_shr]
    where
      mk_ik True (thd_y, thd_x) (i0, k0) = do
        -- not-transposed case (i.e., already coalesced)
        let (t_par, t_seq) = (tx, tk)
        k <- letExp "k" =<< toExp (le64 thd_x + le64 k0 * pe64 t_par)
        i <- letExp "i" =<< toExp (le64 thd_y + le64 i0 * pe64 t_par)
        -- we have padded to minimize bank conflicts,
        -- hence the length of inner dim is (t_seq + 1)
        let e = le64 k + le64 i * (pe64 t_seq + pe64 se1)
        pure (i, k, e)
      mk_ik False (thd_y, thd_x) (i0, k0) = do
        -- matrix is transposed case (i.e., uncoalesced):
        let (t_par, tr_par) = (tx, tx_rx)
        k <- letExp "k" =<< toExp (le64 thd_y + le64 k0 * pe64 t_par)
        i <- letExp "i" =<< toExp (le64 thd_x + le64 i0 * pe64 t_par)
        -- we have padded to minimize bank conflicts,
        -- hence the length of inner dim is (tr_par + 1)
        let e = le64 i + le64 k * (pe64 tr_par + pe64 se1)
        pure (i, k, e)
      isInnerCoal :: Env -> VName -> Stm GPU -> Bool
      isInnerCoal (_, ixfn_env) slc_X (Let pat _ (BasicOp (Index x _)))
        | [slc_X'] <- patNames pat,
          slc_X == slc_X',
          Nothing <- M.lookup x ixfn_env =
            True -- if not in the table, we assume not-transposed!
      isInnerCoal (_, ixfn_env) slc_X (Let pat _ (BasicOp (Index x _)))
        | [slc_X'] <- patNames pat,
          slc_X == slc_X',
          Just lmad <- M.lookup x ixfn_env =
            innerHasStride1 lmad
      isInnerCoal _ _ _ =
        error "kkLoopBody.isInnerCoal: not an error, but I would like to know why!"
      innerHasStride1 lmad =
        let lmad_dims = LMAD.dims lmad
            stride = LMAD.ldStride $ last lmad_dims
         in stride == pe64 (intConst Int64 1)
      --
      mkRedomapOneTileBody acc_merge asss bsss fits_ij = do
        -- the actual redomap.
        redomap_res <- segMapND "redomap_res" segthd_lvl ResultPrivate [ty, tx] $
          \[ltid_y, ltid_x] -> do
            as <- index "as" asss [ltid_y, ltid_x]
            bs <- index "bs" bsss [ltid_y, ltid_x]
            css_init <- index "css_init" acc_merge [ltid_y, ltid_x]

            css <- forLoop_ ry [css_init] $ \i [css_merge] ->
              forLoop rx [css_merge] $ \j [css_merge'] ->
                letTupExp "foo"
                  =<< eIf
                    ( toExp $
                        if fits_ij
                          then true
                          else -- this condition is never needed because
                          -- if i and j are out of range than css[i,j]
                          -- is garbage anyways and should not be written.
                          -- so fits_ij should be always true!!!

                            le64 iii
                              + le64 i
                              + pe64 ry
                                * le64 ltid_y
                              .<. pe64 height_A
                              .&&. le64 jjj
                                + le64 j
                                + pe64 rx
                                  * le64 ltid_x
                                .<. pe64 width_B
                    )
                    ( do
                        a <- index "a" as [i]
                        b <- index "b" bs [j]
                        c <- index "c" css_merge' [i, j]

                        map_lam' <- renameLambda map_lam
                        red_lam' <- renameLambda red_lam

                        -- the inputs to map are supposed to be permutted with the
                        -- inverted permutation, so as to reach the original position;
                        -- it just so happens that the inverse of [a,b] is [b,a]
                        let map_inp_reg = if var_dims == [0, 1] then [a, b] else [b, a]

                        map_res <- eLambda map_lam' (map (eSubExp . Var) map_inp_reg)
                        ~[red_res] <- eLambda red_lam' (map eSubExp $ Var c : map resSubExp map_res)
                        css <- update "css" css_merge' [i, j] (resSubExp red_res)

                        resultBodyM [Var css]
                    )
                    (resultBodyM [Var css_merge'])
            pure [varRes css]
        resultBodyM $ map Var redomap_res
      --
      copyGlb2ShMem ::
        VName ->
        (VName, VName, PrimType, SubExp, VName, Stm GPU, VName) ->
        Builder GPU (VName, VName -> VName -> Builder GPU VName)
      copyGlb2ShMem kk (gtid, ii, ptp_X_el, parlen_X, inp_X, load_X, x_shr_init') = do
        let (t_par, r_par, tseq_div_tpar) = (tx, rx, tk_div_tx)
            is_inner_coal = isInnerCoal env inp_X load_X
            str_A = baseString inp_X
        x_shr <-
          segScatter2D (str_A ++ "_glb2shr") x_shr_init' [r_par, tseq_div_tpar] (t_par, t_par) $
            scatterFun is_inner_coal

        pure (x_shr, copyShr2Reg is_inner_coal str_A x_shr)
        where
          copyShr2Reg ::
            Bool ->
            String ->
            VName ->
            VName ->
            VName ->
            Builder GPU VName
          copyShr2Reg is_inner_coal str_A x_shr k ltid_yx = do
            let (r_par, t_seq, tr_par) = (rx, tk, tx_rx)
            xsss_init <- scratch (str_A ++ "_init_regs") ptp_X_el [r_par]
            forLoop_ r_par [xsss_init] $ \ij [xsss_merge] -> do
              x_shr_ind <-
                letExp (str_A ++ "_shr_ind")
                  =<< toExp
                    ( if is_inner_coal
                        then le64 k + (le64 ltid_yx * pe64 r_par + le64 ij) * (pe64 t_seq + pe64 se1)
                        else le64 ij + le64 ltid_yx * pe64 r_par + le64 k * (pe64 tr_par + pe64 se1)
                    )
              xsss <-
                update (str_A ++ "_regs") xsss_merge [ij] . Var
                  =<< index (str_A ++ "_shr_elem") x_shr [x_shr_ind]
              pure [xsss]

          --
          scatterFun ::
            Bool ->
            [VName] ->
            (VName, VName) ->
            Builder GPU (SubExp, SubExp)
          scatterFun is_inner_coal [i0, k0] (thd_y, thd_x) = do
            let str_A = baseString inp_X
                t_seq = tk
            (i, k, epx_shr_fi) <- mk_ik is_inner_coal (thd_y, thd_x) (i0, k0)
            letBindNames [gtid] =<< toExp (le64 ii + le64 i)
            a_seqdim_idx <- letExp (str_A ++ "_seqdim_idx") =<< toExp (le64 kk + le64 k)

            a_elem <-
              letSubExp (str_A ++ "_elem")
                =<< eIf
                  ( toExp $
                      le64 gtid
                        .<. pe64 parlen_X
                        .&&. if epilogue
                          then le64 a_seqdim_idx .<. pe64 common_dim
                          else true
                  )
                  ( do
                      addStm load_X
                      res <- index "A_elem" inp_X [a_seqdim_idx]
                      resultBodyM [Var res]
                  )
                  (eBody [eBlank $ Prim ptp_X_el])

            a_shr_ind <-
              letSubExp (str_A ++ "_shr_ind")
                =<< eIf
                  (toExp $ le64 k .<. pe64 t_seq)
                  (eBody [toExp epx_shr_fi])
                  (eBody [eSubExp $ intConst Int64 (-1)])
            pure (a_elem, a_shr_ind)
          scatterFun _ _ _ = do
            error "Function scatterFun in Shared.hs: 2nd arg should be an array with 2 elements!"

-- ToDo: we need tx == ty (named t_par), and rx == ry (named r_par)
--       in order to handle all the cases without transpositions.
--       additionally, of course, we need that tk is a multiple of t_par.
mmBlkRegTiling :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
mmBlkRegTiling env stm = do
  res <- mmBlkRegTilingAcc env stm
  case res of
    Nothing -> mmBlkRegTilingNrm env stm
    _ -> pure res

mmBlkRegTilingAcc :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
mmBlkRegTilingAcc env (Let pat aux (Op (SegOp (SegMap SegThread {} seg_space ts old_kbody))))
  | KernelBody () kstms [Returns ResultMaySimplify certs (Var res_nm)] <- old_kbody,
    certs == mempty,
    -- check kernel has one result of primitive type
    [res_tp] <- ts,
    isAcc res_tp,
    -- we get the global-thread id for the two inner dimensions,
    --   as we are probably going to use it in code generation
    (rem_outer_dims, [(gtid_y, height_A), (gtid_x, width_B)]) <-
      splitFromEnd 2 $ unSegSpace seg_space,
    Just
      ( code2',
        (load_A, inp_A, map_t1, load_B, inp_B, map_t2),
        common_dim,
        var_dims,
        (map_lam, red_lam, red_ne, redomap_orig_res, red_t)
        ) <-
      matchesBlkRegTile seg_space kstms,
    checkAccumulatesRedomapRes res_nm code2' redomap_orig_res = do
      -- Here we start the implementation --
      ---- in this binder: host code and outer seggroup (ie. the new kernel) ----
      (new_kernel, host_stms) <- runBuilder $ do
        -- host code
        (rx, ry, tx, ty, tk, tk_div_tx, tk_div_ty, tx_rx, ty_ry, a_shr_sz, b_shr_sz) <-
          mkTileMemSizes height_A width_B common_dim

        rk <- letSubExp "rk" $ BasicOp $ SubExp $ intConst Int64 8 -- 16 and 8 seem good values
        tk_rk <- letSubExp "tk_rk" =<< toExp (pe64 tk * pe64 rk)

        gridDim_t <- letSubExp "gridDim_t" =<< ceilDiv common_dim tk_rk
        gridDim_y <- letSubExp "gridDim_y" =<< ceilDiv height_A ty_ry
        gridDim_x <- letSubExp "gridDim_x" =<< ceilDiv width_B tx_rx

        let gridxyt_pexp = pe64 gridDim_y * pe64 gridDim_x * pe64 gridDim_t
            grid_pexp = gridxyt_pexp * product (map (pe64 . snd) rem_outer_dims)

        (grid_size, tblock_size, segthd_lvl) <- mkNewSegthdLvl tx ty grid_pexp
        (gid_x, gid_y, gid_flat) <- mkGidsXYF
        gid_t <- newVName "gid_t"

        ---- in this binder: outer seggroup ----
        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          iii <- letExp "iii" =<< toExp (le64 gid_y * pe64 ty_ry)
          jjj <- letExp "jjj" =<< toExp (le64 gid_x * pe64 tx_rx)
          ttt <- letExp "ttt" =<< toExp (le64 gid_t * pe64 tk_rk)

          -- initialize register mem with neutral elements and create shmem
          (cssss, a_shr_init, b_shr_init) <-
            initRegShmem
              (rx, tx, ry, ty, a_shr_sz, b_shr_sz)
              (map_t1, map_t2, red_t)
              segthd_lvl
              red_ne

          -- build prologue.
          elems_on_t <- letSubExp "elems_on_t" =<< toExp (pe64 common_dim - le64 ttt)
          tiles_on_t <- letSubExp "tiles_on_t" $ BasicOp $ BinOp (SQuot Int64 Unsafe) elems_on_t tk
          full_tiles <- letExp "full_tiles" $ BasicOp $ BinOp (SMin Int64) rk tiles_on_t

          let ct_arg =
                ( (rx, ry, tx, ty, tk, tk_div_tx, tk_div_ty, tx_rx),
                  segthd_lvl,
                  var_dims,
                  (gtid_x, width_B, gtid_y, height_A, common_dim),
                  (iii, jjj),
                  (load_A, inp_A, map_t1, load_B, inp_B, map_t2),
                  (map_lam, red_lam)
                )

          prologue_res_list <-
            forLoop (Var full_tiles) [cssss, a_shr_init, b_shr_init] $
              \kk0 [thd_res_merge, a_shr_merge, b_shr_merge] -> do
                off_t <- letExp "off_t" =<< toExp (pe64 rk * le64 gid_t + le64 kk0)
                kkLoopBody env ct_arg off_t (thd_res_merge, a_shr_merge, b_shr_merge) False

          let prologue_res : a_shr_reuse : b_shr_reuse : _ = prologue_res_list

          redomap_res_lst <-
            letTupExp "redomap_res_if"
              =<< eIf
                ( toExp $
                    le64 full_tiles
                      .==. pe64 rk
                      .||. pe64 common_dim
                        .==. (pe64 tk * le64 full_tiles + le64 ttt)
                )
                (resultBodyM $ map Var prologue_res_list)
                ( do
                    off_t <- letExp "off_t" =<< toExp (pe64 rk * le64 gid_t + le64 full_tiles)
                    process_sprs_tile <-
                      kkLoopBody env ct_arg off_t (prologue_res, a_shr_reuse, b_shr_reuse) True

                    resultBodyM $ map Var process_sprs_tile
                )
          let redomap_res : _ = redomap_res_lst

          -- support for non-empty code2'
          --  segmap (ltid_y < ty, ltid_x < tx) {
          --    for i < ry do
          --      for j < rx do
          --        res = if (iii+ltid_y*ry+i < height_A && jjj+ltid_x*rx+j < width_B)
          --              then code2' else dummy
          --        final_res[i,j] = res
          mkEpilogueAccRes
            segthd_lvl
            (redomap_orig_res, redomap_res)
            (res_nm, res_tp)
            (ty, tx, ry, rx)
            (iii, jjj)
            (gtid_y, gtid_x)
            (height_A, width_B, rem_outer_dims)
            code2'

        let grid = KernelGrid (Count grid_size) (Count tblock_size)
            level' = SegBlock SegNoVirt (Just grid)
            space' = SegSpace gid_flat (rem_outer_dims ++ [(gid_t, gridDim_t), (gid_y, gridDim_y), (gid_x, gridDim_x)])
            kbody' = KernelBody () stms_seggroup ret_seggroup
        pure $ Let pat aux $ Op $ SegOp $ SegMap level' space' ts kbody'
      pure $ Just (host_stms, new_kernel)
  where
    sameAccType acc_sglton (Acc sglton _ _ _) =
      acc_sglton == sglton
    sameAccType _ _ = False
    getAccumFV (Acc singleton _shp [_eltp] _) = do
      let fvs = namesToList $ freeIn old_kbody -- code
      tps <- localScope (scopeOfSegSpace seg_space) $ do
        mapM lookupType fvs
      let (acc_0s, _) = unzip $ filter (sameAccType singleton . snd) $ zip fvs tps
      case acc_0s of
        [acc_0] -> pure acc_0
        _ -> error "Impossible case reached when treating accumulators!"
    getAccumFV tp = error ("Should be an accumulator type at this point, given: " ++ prettyString tp)
    --
    -- checks that the redomap result is used directly as the accumulated value,
    -- in which case it is safe to parallelize the innermost dimension (of tile tk)
    checkAccumulatesRedomapRes res_nm acc_code redomap_orig_res = do
      foldl getAccumStm False $ reverse $ stmsToList acc_code
      where
        getAccumStm True _ = True
        getAccumStm False (Let (Pat [pat_el]) _aux (BasicOp (UpdateAcc _acc_nm _ind vals)))
          | [v] <- vals,
            patElemName pat_el == res_nm =
              v == Var redomap_orig_res
        getAccumStm False _ = False
    --
    -- epilogue for accumulator result type
    mkEpilogueAccRes
      segthd_lvl
      (redomap_orig_res, redomap_res)
      (res_nm, res_tp)
      (ty, tx, ry, rx)
      (iii, jjj)
      (gtid_y, gtid_x)
      (height_A, width_B, _rem_outer_dims)
      code2' = do
        rss_init <- getAccumFV res_tp
        epilogue_res_acc <- segMapND_ "rssss" segthd_lvl ResultMaySimplify [ty, tx] $ \[ltid_y, ltid_x] -> do
          (css, ii, jj) <- getThdRedomapRes (rx, ry) (ltid_x, ltid_y) (iii, jjj, redomap_res)
          rss <- forLoop_ ry [rss_init] $ \i [rss_merge] -> do
            forLoop rx [rss_merge] $ \j [rss_merge'] -> do
              prereqAddCode2 (gtid_x, gtid_y) (ii, i, jj, j) (css, redomap_orig_res)
              let code2_subs = substituteNames (M.singleton rss_init rss_merge') code2'

              letTupExp "res_elem"
                =<< eIf
                  ( toExp $
                      le64 gtid_y
                        .<. pe64 height_A
                        .&&. le64 gtid_x
                          .<. pe64 width_B
                  )
                  ( do
                      addStms code2_subs
                      resultBodyM [Var res_nm]
                  )
                  (resultBodyM [Var rss_merge'])
          pure [varRes rss]
        pure [Returns ResultMaySimplify (Certs []) $ Var epilogue_res_acc]
mmBlkRegTilingAcc _ _ = pure Nothing

--------------------------
--------------------------

mmBlkRegTilingNrm :: Env -> Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
mmBlkRegTilingNrm env (Let pat aux (Op (SegOp (SegMap SegThread {} seg_space ts old_kbody))))
  | KernelBody () kstms [Returns ResultMaySimplify certs (Var res_nm)] <- old_kbody,
    certs == mempty,
    -- check kernel has one result of primitive type
    [res_tp] <- ts,
    primType res_tp,
    -- we get the global-thread id for the two inner dimensions,
    --   as we are probably going to use it in code generation
    (rem_outer_dims, [(gtid_y, height_A), (gtid_x, width_B)]) <-
      splitFromEnd 2 $ unSegSpace seg_space,
    Just
      ( code2',
        (load_A, inp_A, map_t1, load_B, inp_B, map_t2),
        common_dim,
        var_dims,
        (map_lam, red_lam, red_ne, redomap_orig_res, red_t)
        ) <-
      matchesBlkRegTile seg_space kstms = do
      -- Here we start the implementation
      ---- in this binder: host code and outer seggroup (ie. the new kernel) ----
      (new_kernel, host_stms) <- runBuilder $ do
        -- host code
        (rx, ry, tx, ty, tk, tk_div_tx, tk_div_ty, tx_rx, ty_ry, a_shr_sz, b_shr_sz) <-
          mkTileMemSizes height_A width_B common_dim

        gridDim_x <- letSubExp "gridDim_x" =<< ceilDiv width_B tx_rx
        gridDim_y <- letSubExp "gridDim_y" =<< ceilDiv height_A ty_ry
        let gridxy_pexp = pe64 gridDim_y * pe64 gridDim_x
        let grid_pexp = gridxy_pexp * product (map (pe64 . snd) rem_outer_dims)
        (grid_size, tblock_size, segthd_lvl) <- mkNewSegthdLvl tx ty grid_pexp

        (gid_x, gid_y, gid_flat) <- mkGidsXYF

        ---- in this binder: outer seggroup ----
        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          iii <- letExp "iii" =<< toExp (le64 gid_y * pe64 ty_ry)
          jjj <- letExp "jjj" =<< toExp (le64 gid_x * pe64 tx_rx)

          -- initialize register mem with neutral elements and create shmem
          (cssss, a_shr_init, b_shr_init) <-
            initRegShmem
              (rx, tx, ry, ty, a_shr_sz, b_shr_sz)
              (map_t1, map_t2, red_t)
              segthd_lvl
              red_ne

          -- build prologue.
          full_tiles <-
            letExp "full_tiles" $
              BasicOp $
                BinOp (SQuot Int64 Unsafe) common_dim tk

          let ct_arg =
                ( (rx, ry, tx, ty, tk, tk_div_tx, tk_div_ty, tx_rx),
                  segthd_lvl,
                  var_dims,
                  (gtid_x, width_B, gtid_y, height_A, common_dim),
                  (iii, jjj),
                  (load_A, inp_A, map_t1, load_B, inp_B, map_t2),
                  (map_lam, red_lam)
                )

          prologue_res_list <-
            forLoop (Var full_tiles) [cssss, a_shr_init, b_shr_init] $
              \kk0 [thd_res_merge, a_shr_merge, b_shr_merge] -> do
                kkLoopBody env ct_arg kk0 (thd_res_merge, a_shr_merge, b_shr_merge) False

          let prologue_res : a_shr_reuse : b_shr_reuse : _ = prologue_res_list

          -- build epilogue.
          redomap_res <-
            head
              <$> kkLoopBody
                env
                ct_arg
                full_tiles
                (prologue_res, a_shr_reuse, b_shr_reuse)
                True

          -- support for non-empty code2'
          --  segmap (ltid_y < ty, ltid_x < tx) {
          --    for i < ry do
          --      for j < rx do
          --        res = if (iii+ltid_y*ry+i < height_A && jjj+ltid_x*rx+j < width_B)
          --              then code2' else dummy
          --        final_res[i,j] = res
          mkEpiloguePrimRes
            segthd_lvl
            (redomap_orig_res, redomap_res)
            (res_nm, res_tp)
            (ty, tx, ry, rx)
            (iii, jjj)
            (gtid_y, gtid_x)
            (height_A, width_B, rem_outer_dims)
            code2'

        let grid = KernelGrid (Count grid_size) (Count tblock_size)
            level' = SegBlock SegNoVirt (Just grid)
            space' = SegSpace gid_flat (rem_outer_dims ++ [(gid_y, gridDim_y), (gid_x, gridDim_x)])
            kbody' = KernelBody () stms_seggroup ret_seggroup
        pure $ Let pat aux $ Op $ SegOp $ SegMap level' space' ts kbody'
      pure $ Just (host_stms, new_kernel)
  where
    mkEpiloguePrimRes
      segthd_lvl
      (redomap_orig_res, redomap_res)
      (res_nm, res_tp)
      (ty, tx, ry, rx)
      (iii, jjj)
      (gtid_y, gtid_x)
      (height_A, width_B, rem_outer_dims)
      code2' = do
        epilogue_res <-
          if redomap_orig_res == res_nm
            then pure redomap_res -- epilogue_res_list
            else segMapND_ "rssss" segthd_lvl ResultPrivate [ty, tx] $ \[ltid_y, ltid_x] -> do
              rss_init <- scratch "rss_init" (elemType res_tp) [ry, rx]
              (css, ii, jj) <- getThdRedomapRes (rx, ry) (ltid_x, ltid_y) (iii, jjj, redomap_res)
              rss <- forLoop_ ry [rss_init] $ \i [rss_merge] -> do
                forLoop rx [rss_merge] $ \j [rss_merge'] -> do
                  prereqAddCode2 (gtid_x, gtid_y) (ii, i, jj, j) (css, redomap_orig_res)

                  res_el <-
                    letSubExp "res_elem"
                      =<< eIf
                        ( toExp $
                            le64 gtid_y
                              .<. pe64 height_A
                              .&&. le64 gtid_x
                                .<. pe64 width_B
                        )
                        ( do
                            addStms code2'
                            resultBodyM [Var res_nm]
                        )
                        (eBody [eBlank res_tp])
                  rss'' <- update "rss" rss_merge' [i, j] res_el
                  pure [rss'']
              pure [varRes rss]

        let regtile_ret_dims =
              map (\(_, sz) -> (sz, se1, se1)) rem_outer_dims
                ++ [(height_A, ty, ry), (width_B, tx, rx)]

        -- Add dummy dimensions to tile to reflect the outer dimensions.
        epilogue_res' <-
          if null rem_outer_dims
            then pure epilogue_res
            else do
              epilogue_t <- lookupType epilogue_res
              let (block_dims, rest_dims) = splitAt 2 $ arrayDims epilogue_t
                  ones = map (const $ intConst Int64 1) rem_outer_dims
                  new_shape = Shape $ concat [ones, block_dims, ones, rest_dims]
              letExp "res_reshaped" . BasicOp $
                Reshape ReshapeArbitrary new_shape epilogue_res

        pure [RegTileReturns mempty regtile_ret_dims epilogue_res']
mmBlkRegTilingNrm _ _ = pure Nothing

-- pattern match the properties of the code that we look to
-- tile: a redomap whose two input arrays are each invariant
-- to one of the last two (innermost) parallel dimensions.
matchesBlkRegTile ::
  SegSpace ->
  Stms GPU ->
  Maybe
    ( Stms GPU,
      (Stm GPU, VName, PrimType, Stm GPU, VName, PrimType),
      SubExp,
      [Int],
      (Lambda GPU, Lambda GPU, SubExp, VName, PrimType)
    )
matchesBlkRegTile seg_space kstms
  | -- build the variance table, that records, for
    -- each variable name, the variables it depends on
    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,
    -- check that the code fits the pattern having:
    -- some `code1`, followed by one Screma SOAC, followed by some `code2`
    Just (code1, screma_stmt, code2) <- matchCodeStreamCode kstms,
    Let pat_redomap _ (Op _) <- screma_stmt,
    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (common_dim, arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,
    -- check that exactly two 1D arrays are streamed thorugh redomap,
    -- and the result of redomap is one scalar
    -- !!!I need to rearrange this whole thing!!! including inp_A and inp_B
    length arrs == 2,
    [red_ne] <- red_nes,
    [map_t1t, map_t2t] <- map paramDec $ lambdaParams map_lam,
    [red_t1, _] <- map paramDec $ lambdaParams red_lam,
    primType map_t1t && primType map_t2t && primType red_t1,
    map_t1_0 <- elemType map_t1t,
    map_t2_0 <- elemType map_t2t,
    -- checks that the input arrays to redomap are variant to
    -- exactly one of the two innermost dimensions of the kernel
    Just var_dims <- isInvarTo1of2InnerDims mempty seg_space variance arrs,
    -- get the variables on which the first result of redomap depends on
    [redomap_orig_res] <- patNames pat_redomap,
    Just res_red_var <- M.lookup redomap_orig_res variance, -- variance of the reduce result

    -- we furthermore check that code1 is only formed by
    -- 1. statements that slice some globally-declared arrays
    --    to produce the input for the redomap, and
    -- 2. potentially some statements on which the redomap
    --    is independent; these are recorded in `code2''`
    Just (code2'', tab_inv_stm) <-
      foldl
        (processIndirections (namesFromList arrs) res_red_var)
        (Just (Seq.empty, M.empty))
        code1,
    -- identify load_A, load_B
    tmp_stms <- mapMaybe (`M.lookup` tab_inv_stm) arrs,
    length tmp_stms == length arrs =
      let zip_AB = zip3 tmp_stms arrs [map_t1_0, map_t2_0]
          [(load_A, inp_A, map_t1), (load_B, inp_B, map_t2)] =
            if var_dims == [0, 1]
              then zip_AB
              else reverse zip_AB
          code2' = code2'' <> code2
       in myDebug
            ( "Matches BlkRegTile pattern!\n"
                ++ "kstms:\n"
                ++ prettyString kstms
                ++ "\nseg_space:\n"
                ++ prettyString seg_space
                ++ "\nvariance:\n"
                ++ show variance
            )
            $ Just
              ( code2',
                (load_A, inp_A, map_t1, load_B, inp_B, map_t2),
                common_dim,
                var_dims,
                (map_lam, red_lam, red_ne, redomap_orig_res, elemType red_t1)
              )
matchesBlkRegTile space kstms =
  myDebug
    ( "Does NOT match BlkRegTile pattern: \n"
        ++ "kstms:\n"
        ++ prettyString kstms
        ++ "\nseg_space:\n"
        ++ prettyString space
    )
    Nothing

mkTileMemSizes ::
  SubExp ->
  SubExp ->
  SubExp ->
  Builder
    GPU
    ( SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp,
      SubExp
    )
mkTileMemSizes height_A width_B common_dim = do
  tk_name <- baseName <$> newVName "Tk"
  tx_name <- baseName <$> newVName "Tx"
  ty_name <- baseName <$> newVName "Ty"
  rx_name <- baseName <$> newVName "Rx"
  ry_name <- baseName <$> newVName "Ry"

  (ty, ry) <- getParTiles ("Ty", "Ry") (ty_name, ry_name) height_A
  (tx, rx) <- getParTiles ("Tx", "Rx") (tx_name, rx_name) width_B
  tk <- getSeqTile "Tk" tk_name common_dim tx ty

  tk_div_tx <- letSubExp "tk_div_tx" =<< ceilDiv tk tx
  tk_div_ty <- letSubExp "tk_div_ty" =<< ceilDiv tk ty

  tx_rx <- letSubExp "TxRx" =<< toExp (pe64 tx * pe64 rx)
  ty_ry <- letSubExp "TyRy" =<< toExp (pe64 ty * pe64 ry)

  let pad_term = sMax64 (pe64 tk) (pe64 ty * pe64 ry)
  -- if A not transposed, its shmem should be [ty*ry][tk]
  -- we pad to [ty*ry][tk+1] size to minimize bank conflicts
  a_shr_sz <-
    letSubExp "a_shr_sz"
      =<< toExp (pe64 ty * pe64 ry * pe64 tk + pad_term)
  -- if B is transposed, its shmem should be [tk][tx*rx]
  -- we pad as above, by assuming tx*rx == ty*ry >= tk
  -- ToDo: we can decrease the size by checking at this
  --       point whether A and B are transposed (or not).
  b_shr_sz <-
    letSubExp "b_shr_sz"
      =<< toExp (pe64 tx * pe64 rx * pe64 tk + pad_term) -- (pe64 tk * pe64 tx * pe64 rx)
  pure (rx, ry, tx, ty, tk, tk_div_tx, tk_div_ty, tx_rx, ty_ry, a_shr_sz, b_shr_sz)

mkNewSegthdLvl ::
  SubExp ->
  SubExp ->
  TPrimExp Int64 VName ->
  Builder GPU (SubExp, SubExp, SegLevel)
mkNewSegthdLvl tx ty grid_pexp = do
  grid_size <- letSubExp "grid_size" =<< toExp grid_pexp
  tblock_size <- letSubExp "tblock_size" =<< toExp (pe64 ty * pe64 tx)
  let segthd_lvl = SegThreadInBlock (SegNoVirtFull (SegSeqDims []))
  pure (grid_size, tblock_size, segthd_lvl)

mkGidsXYF :: Builder GPU (VName, VName, VName)
mkGidsXYF = do
  gid_y <- newVName "gid_y"
  gid_x <- newVName "gid_x"
  gid_flat <- newVName "gid_flat"
  pure (gid_x, gid_y, gid_flat)

initRegShmem ::
  (SubExp, SubExp, SubExp, SubExp, SubExp, SubExp) ->
  (PrimType, PrimType, PrimType) ->
  SegLevel ->
  SubExp ->
  Builder GPU (VName, VName, VName)
initRegShmem
  (rx, tx, ry, ty, a_shr_sz, b_shr_sz)
  (map_t1, map_t2, red_t)
  segthd_lvl
  red_ne = do
    -- initialize register mem with neutral elements.
    cssss <- segMapND_ "cssss" segthd_lvl ResultPrivate [ty, tx] $ \_ -> do
      css_init <- scratch "css_init" red_t [ry, rx]
      css <- forLoop_ ry [css_init] $ \i [css_merge] -> do
        forLoop rx [css_merge] $ \j [css_merge'] -> do
          css'' <- update "css" css_merge' [i, j] red_ne
          pure [css'']
      pure [varRes css]
    -- scratch shared memory
    a_shr_init <- scratch "A_shr" map_t1 [a_shr_sz]
    b_shr_init <- scratch "B_shr" map_t2 [b_shr_sz]
    pure (cssss, a_shr_init, b_shr_init)

getThdRedomapRes ::
  (SubExp, SubExp) ->
  (VName, VName) ->
  (VName, VName, VName) ->
  Builder GPU (VName, VName, VName)
getThdRedomapRes (rx, ry) (ltid_x, ltid_y) (iii, jjj, redomap_res) = do
  css <- index "redomap_thd" redomap_res [ltid_y, ltid_x]
  ii <- letExp "ii" =<< toExp (le64 iii + le64 ltid_y * pe64 ry)
  jj <- letExp "jj" =<< toExp (le64 jjj + le64 ltid_x * pe64 rx)
  pure (css, ii, jj)

prereqAddCode2 ::
  (VName, VName) ->
  (VName, VName, VName, VName) ->
  (VName, VName) ->
  Builder GPU ()
prereqAddCode2 (gtid_x, gtid_y) (ii, i, jj, j) (css, redomap_orig_res) = do
  c <- index "redomap_elm" css [i, j]
  cpy_stm <- mkLetNamesM [redomap_orig_res] $ BasicOp $ SubExp $ Var c
  addStm cpy_stm
  letBindNames [gtid_y] =<< toExp (le64 ii + le64 i)
  letBindNames [gtid_x] =<< toExp (le64 jj + le64 j)

matchCodeStreamCode :: Stms GPU -> Maybe (Stms GPU, Stm GPU, Stms GPU)
matchCodeStreamCode kstms = do
  idx_first_screma <- L.findIndex isScremaStm kstms'
  let (code1, (screma : code2)) = splitAt idx_first_screma kstms'
  pure (stmsFromList code1, screma, stmsFromList code2)
  where
    kstms' = stmsToList kstms
    isScremaStm (Let _ _ (Op (OtherOp Screma {}))) = True
    isScremaStm _ = False

-- | Checks that all streamed arrays are variant to exacly one of
--   the two innermost parallel dimensions, and conversely, for
--   each of the two innermost parallel dimensions, there is at
--   least one streamed array variant to it. The result is the
--   number of the only variant parallel dimension for each array.
isInvarTo1of2InnerDims ::
  Names ->
  SegSpace ->
  VarianceTable ->
  [VName] ->
  Maybe [Int]
isInvarTo1of2InnerDims branch_variant segspace =
  let n_dims = 2
      inner_dims = takeLast n_dims $ map fst $ unSegSpace segspace
   in isInvarToAllButOneInnerDim n_dims branch_variant inner_dims

isInvarToAllButOneInnerDim ::
  Int ->
  Names ->
  [VName] ->
  VarianceTable ->
  [VName] ->
  Maybe [Int]
isInvarToAllButOneInnerDim n_dims branch_variant dims variance arrs
  | length dims /= n_dims =
      error $
        "isInvarToAllButOneInnerDim error: Expected "
          ++ show n_dims
          ++ " dims, but got "
          ++ show (length dims)
  | all (`notNameIn` branch_variant) dims,
    Just inner_perm <- mapM arrIsVarToExactly1InnerDim arrs,
    all (`elem` inner_perm) [0 .. n_dims - 1] =
      pure inner_perm
  | otherwise = Nothing
  where
    arrIsVarToExactly1InnerDim :: VName -> Maybe Int
    arrIsVarToExactly1InnerDim arr
      | [variant_dim_idx] <- L.findIndices (`nameIn` variants) dims =
          pure variant_dim_idx
      | otherwise = Nothing
      where
        variants = M.findWithDefault mempty arr variance

processIndirections ::
  Names -> -- input arrays to redomap
  Names -> -- variables on which the result of redomap depends on.
  Maybe (Stms GPU, M.Map VName (Stm GPU)) ->
  Stm GPU ->
  Maybe (Stms GPU, M.Map VName (Stm GPU))
processIndirections arrs _ acc stm@(Let patt _ (BasicOp (Index _ _)))
  | Just (ss, tab) <- acc,
    [p] <- patElems patt,
    p_nm <- patElemName p,
    p_nm `nameIn` arrs =
      Just (ss, M.insert p_nm stm tab)
processIndirections _ res_red_var acc stm'@(Let patt _ _)
  | Just (ss, tab) <- acc,
    ps <- patElems patt,
    all (\p -> patElemName p `notNameIn` res_red_var) ps =
      Just (ss Seq.|> stm', tab)
  | otherwise = Nothing

getParTiles :: (String, String) -> (Name, Name) -> SubExp -> Builder GPU (SubExp, SubExp)
getParTiles (t_str, r_str) (t_name, r_name) len_dim =
  case len_dim of
    Constant (IntValue (Int64Value 8)) ->
      pure (se8, se1)
    Constant (IntValue (Int64Value 16)) ->
      pure (se8, se2)
    Constant (IntValue (Int64Value 32)) ->
      pure (se8, se4)
    _ -> do
      t <- letSubExp t_str $ Op $ SizeOp $ GetSize t_name SizeTile
      r <- letSubExp r_str $ Op $ SizeOp $ GetSize r_name SizeRegTile
      pure (t, r)

getSeqTile :: String -> Name -> SubExp -> SubExp -> SubExp -> Builder GPU SubExp
getSeqTile tk_str tk_name len_dim tx ty =
  case (tx, ty) of
    (Constant (IntValue (Int64Value v_x)), Constant (IntValue (Int64Value v_y))) ->
      letSubExp tk_str . BasicOp . SubExp . constant $
        case len_dim of
          Constant (IntValue (Int64Value v_d)) -> min v_d $ min v_x v_y
          _ -> min v_x v_y
    _ ->
      letSubExp tk_str $ Op $ SizeOp $ GetSize tk_name SizeTile

----------------------------------------------------------------------------------------------
--- 3D Tiling (RegTiling for the outermost dimension & Block tiling for the innermost two) ---
----------------------------------------------------------------------------------------------

maxRegTile :: Int64
maxRegTile = 30

mkRegTileSe :: Int64 -> SubExp
mkRegTileSe = constant

variantToDim :: VarianceTable -> VName -> VName -> Bool
variantToDim variance gid_outer nm =
  gid_outer == nm || nameIn gid_outer (M.findWithDefault mempty nm variance)

-- | Checks that all streamed arrays are variant to exacly one of
--   the three innermost parallel dimensions, and conversely, for
--   each of the three innermost parallel dimensions, there is at
--   least one streamed array variant to it. The result is the
--   number of the only variant parallel dimension for each array.
isInvarTo2of3InnerDims :: Names -> [VName] -> VarianceTable -> [VName] -> Maybe [Int]
isInvarTo2of3InnerDims = isInvarToAllButOneInnerDim 3

-- | Expects a kernel statement as argument.
--   CONDITIONS for 3D tiling optimization to fire are:
--     1. a) The kernel body can be broken into
--              scalar-code-1 ++ [Redomap stmt] ++ scalar-code-2.
--        b) The kernels has a per-thread result, and obviously
--              the result is variant to the 3rd dimension
--              (counted from innermost to outermost)
--     2. For the Redomap:
--          a) the streamed arrays are one dimensional
--          b) each of the array arguments of Redomap are variant
--              to exactly one of the three innermost-parallel dimension
--              of the kernel. This condition can be relaxed by interchanging
--              kernel dimensions whenever possible.
--     3. For scalar-code-1:
--          a) each of the statements is a slice that produces one of the
--             streamed arrays
--
-- mmBlkRegTiling :: Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
-- mmBlkRegTiling (Let pat aux (Op (SegOp (SegMap SegThread{} seg_space ts old_kbody))))
doRegTiling3D :: Stm GPU -> TileM (Maybe (Stms GPU, Stm GPU))
doRegTiling3D (Let pat aux (Op (SegOp old_kernel)))
  | SegMap SegThread {} space kertp (KernelBody () kstms kres) <- old_kernel,
    -- build the variance table, that records, for
    -- each variable name, the variables it depends on
    initial_variance <- M.map mempty $ scopeOfSegSpace space,
    variance <- varianceInStms initial_variance kstms,
    -- we get the global-thread id for the two inner dimensions,
    --   as we are probably going to use it in code generation
    all_dims <- unSegSpace space,
    (rem_outer_dims, inner_dims@[(gtid_z, d_M), (gtid_y, d_Ky), (gtid_x, d_Kx)]) <-
      splitFromEnd 3 $ all_dims,
    -- check that the code fits the pattern having:
    -- some `code1`, followed by one Screma SOAC, followed by some `code2`
    Just (code1, screma_stmt, code2) <- matchCodeStreamCode kstms,
    Let pat_redomap _ (Op _) <- screma_stmt,
    -- checks that the Screma SOAC is actually a redomap and normalize it
    Just (common_dim, inp_soac_arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,
    not (null red_nes),
    -- assuming we have a budget of maxRegTile registers, we distribute
    -- that budget across the result of redomap and the kernel result
    num_res <- max (length red_nes) (length kres),
    reg_tile <- maxRegTile `quot` fromIntegral num_res,
    reg_tile_se <- mkRegTileSe reg_tile,
    -- check that the element-type of the map and reduce are scalars:
    all (primType . paramDec) $ lambdaParams map_lam,
    red_res_tps <- map paramDec $ take (length red_nes) $ lambdaParams red_lam,
    all primType red_res_tps,
    -- checks that the input arrays to redomap are variant to
    -- exactly one of the two innermost dimensions of the kernel
    Just _ <- isInvarTo2of3InnerDims mempty (map fst inner_dims) variance inp_soac_arrs,
    -- get the free variables on which the result of redomap depends on
    redomap_orig_res <- patElems pat_redomap,
    res_red_var <- -- variance of the reduce result
      mconcat $ mapMaybe ((`M.lookup` variance) . patElemName) redomap_orig_res,
    res_red_var /= mempty,
    -- we furthermore check that code1 is only formed by
    -- 1. statements that slice some globally-declared arrays
    --    to produce the input for the redomap, and
    -- 2. potentially some statements on which the redomap
    --    is independent; these are recorded in `code2''`
    Just (code2'', arr_tab0) <-
      foldl
        (processIndirections (namesFromList inp_soac_arrs) res_red_var)
        (Just (Seq.empty, M.empty))
        code1,
    -- check that code1 contains exacly one slice for each of the input array to redomap
    tmp_stms <- mapMaybe (`M.lookup` arr_tab0) inp_soac_arrs,
    length tmp_stms == length inp_soac_arrs,
    -- code1' <- stmsFromList $ stmsToList code1 \\ stmsToList code2'',
    code2' <- code2'' <> code2,
    -- we assume the kernel results are variant to the thrid-outer parallel dimension
    -- (for sanity sake, they should be)
    ker_res_nms <- mapMaybe getResNm kres,
    length ker_res_nms == length kres,
    all primType kertp,
    all (variantToDim variance gtid_z) ker_res_nms = do
      -- HERE STARTS THE IMPLEMENTATION:
      (new_kernel, host_stms) <- runBuilder $ do
        -- host code
        -- process the z-variant arrays that need transposition;
        -- these "manifest" statements should come before the kernel
        (tab_inn, tab_out) <-
          foldM
            (insertTranspose variance (gtid_z, d_M))
            (M.empty, M.empty)
            $ M.toList arr_tab0

        tx_name <- baseName <$> newVName "Tx"
        ty_name <- baseName <$> newVName "Ty"

        tx0 <- letSubExp "Tx" $ Op $ SizeOp $ GetSize tx_name SizeTile
        ty0 <- letSubExp "Ty" $ Op $ SizeOp $ GetSize ty_name SizeTile
        ty <- limitTile "Ty" ty0 d_Ky
        tx <- limitTile "Tx" tx0 d_Kx
        let rz = reg_tile_se

        gridDim_x <- letSubExp "gridDim_x" =<< ceilDiv d_Kx tx
        gridDim_y <- letSubExp "gridDim_y" =<< ceilDiv d_Ky ty
        gridDim_z <- letSubExp "gridDim_z" =<< ceilDiv d_M rz
        let gridxyz_pexp = pe64 gridDim_z * pe64 gridDim_y * pe64 gridDim_x
        let grid_pexp = gridxyz_pexp * product (map (pe64 . snd) rem_outer_dims)
        grid_size <- letSubExp "grid_size_tile3d" =<< toExp grid_pexp
        tblock_size <- letSubExp "tblock_size_tile3d" =<< toExp (pe64 ty * pe64 tx)
        let segthd_lvl = SegThreadInBlock (SegNoVirtFull (SegSeqDims []))

        count_shmem <- letSubExp "count_shmem" =<< ceilDiv rz tblock_size

        gid_x <- newVName "gid_x"
        gid_y <- newVName "gid_y"
        gid_z <- newVName "gid_z"
        gid_flat <- newVName "gid_flat"

        ---- in this binder: outer seggroup ----
        (ret_seggroup, stms_seggroup) <- runBuilder $ do
          ii <- letExp "ii" =<< toExp (le64 gid_z * pe64 rz)
          jj1 <- letExp "jj1" =<< toExp (le64 gid_y * pe64 ty)
          jj2 <- letExp "jj2" =<< toExp (le64 gid_x * pe64 tx)

          -- initialize the register arrays corresponding to the result of redomap;
          reg_arr_nms <- segMapND "res" segthd_lvl ResultPrivate [ty, tx] $ \_ ->
            forM (zip red_nes red_res_tps) $ \(red_ne, red_t) -> do
              css_init <- scratch "res_init" (elemType red_t) [rz]
              css <- forLoop_ rz [css_init] $ \i [css_merge] -> do
                css' <- update "css" css_merge [i] red_ne
                pure [css']
              pure $ varRes css

          -- scratch the shared-memory arrays corresponding to the arrays that are
          --   input to the redomap and are invariant to the outermost parallel dimension.
          shr_arr_nms <- forM (M.toList tab_out) $ \(nm, (ptp, _)) ->
            scratch (baseString nm ++ "_shr") ptp [rz]

          prologue_res_list <-
            forLoop common_dim (reg_arr_nms ++ shr_arr_nms) $
              \q var_nms -> do
                let (reg_arr_merge_nms, shr_arr_merge_nms) = splitAt (length red_nes) var_nms

                -- collective copy from global to shared memory
                shr_arr_nms' <-
                  forLoop count_shmem shr_arr_merge_nms $ \tt shr_arr_merge2_nms -> do
                    shr_arr_merge2_nms' <-
                      forM (zip shr_arr_merge2_nms (M.toList tab_out)) $ \(shr_Y_nm, (glb_Y_nm, (ptp_Y, load_Y))) -> do
                        ltid_flat <- newVName "ltid_flat"
                        ltid <- newVName "ltid"
                        let segspace = SegSpace ltid_flat [(ltid, tblock_size)]
                        ((res_v, res_i), stms) <- runBuilder $ do
                          offs <- letExp "offs" =<< toExp (pe64 tblock_size * le64 tt)
                          shr_ind <- letExp "shr_ind" =<< toExp (le64 ltid + le64 offs)
                          letBindNames [gtid_z] =<< toExp (le64 ii + le64 shr_ind)
                          let glb_ind = gtid_z
                          y_elm <-
                            letSubExp "y_elem"
                              =<< eIf
                                (toExp $ le64 glb_ind .<. pe64 d_M)
                                ( do
                                    addStm load_Y
                                    res <- index "Y_elem" glb_Y_nm [q]
                                    resultBodyM [Var res]
                                )
                                (eBody [eBlank $ Prim ptp_Y])
                          y_ind <-
                            letSubExp "y_shr_ind"
                              =<< eIf
                                (toExp $ le64 shr_ind .<. pe64 rz)
                                (toExp shr_ind >>= letTupExp' "shr_fi" >>= resultBodyM)
                                (eBody [pure $ BasicOp $ SubExp $ intConst Int64 (-1)])
                          -- y_tp  <- subExpType y_elm
                          pure (y_elm, y_ind)

                        let ret = WriteReturns mempty shr_Y_nm [(Slice [DimFix res_i], res_v)]
                        let body = KernelBody () stms [ret]
                        shr_Y_nm_t <- lookupType shr_Y_nm

                        fmap head $
                          letTupExp "Y_glb2shr" <=< renameExp $
                            Op . SegOp $
                              SegMap segthd_lvl segspace [shr_Y_nm_t] body
                    pure shr_arr_merge2_nms'

                redomap_res <-
                  segMapND "redomap_res" segthd_lvl ResultPrivate [ty, tx] $
                    \[ltid_y, ltid_x] -> do
                      letBindNames [gtid_y] =<< toExp (le64 jj1 + le64 ltid_y)
                      letBindNames [gtid_x] =<< toExp (le64 jj2 + le64 ltid_x)
                      reg_arr_merge_nms_slc <- forM reg_arr_merge_nms $ \reg_arr_nm ->
                        index "res_reg_slc" reg_arr_nm [ltid_y, ltid_x]
                      fmap subExpsRes . letTupExp' "redomap_guarded"
                        =<< eIf
                          (toExp $ le64 gtid_y .<. pe64 d_Ky .&&. le64 gtid_x .<. pe64 d_Kx)
                          ( do
                              inp_scals_invar_outer <-
                                forM (M.toList tab_inn) $ \(inp_arr_nm, load_stm) -> do
                                  addStm load_stm
                                  index (baseString inp_arr_nm) inp_arr_nm [q]
                              -- build the loop of count R whose body is semantically the redomap code
                              reg_arr_merge_nms' <-
                                forLoop rz reg_arr_merge_nms_slc $ \i reg_arr_mm_nms -> do
                                  letBindNames [gtid_z] =<< toExp (le64 ii + le64 i)
                                  letTupExp "redomap_lam"
                                    =<< eIf
                                      (toExp $ le64 gtid_z .<. pe64 d_M)
                                      ( do
                                          -- read from shared memory
                                          ys <- forM shr_arr_nms' $ \shr_arr_nm ->
                                            index "inp_reg_var2z" shr_arr_nm [i]
                                          cs <- forM reg_arr_mm_nms $ \reg_arr_nm ->
                                            index "res_reg_var2z" reg_arr_nm [i]
                                          -- here we need to put in order the scalar inputs to map:
                                          let tab_scals =
                                                M.fromList $
                                                  zip (map fst $ M.toList tab_out) ys
                                                    ++ zip (map fst $ M.toList tab_inn) inp_scals_invar_outer
                                          map_inp_scals <- forM inp_soac_arrs $ \arr_nm ->
                                            case M.lookup arr_nm tab_scals of
                                              Nothing -> error "Impossible case reached in tiling3D\n"
                                              Just nm -> pure nm
                                          map_lam' <- renameLambda map_lam
                                          red_lam' <- renameLambda red_lam
                                          map_res_scals <- eLambda map_lam' (map (eSubExp . Var) map_inp_scals)
                                          red_res <- eLambda red_lam' (map eSubExp (map Var cs ++ map resSubExp map_res_scals))
                                          css <- forM (zip reg_arr_mm_nms red_res) $ \(reg_arr_nm, c) ->
                                            update (baseString reg_arr_nm) reg_arr_nm [i] (resSubExp c)
                                          resultBodyM $ map Var css
                                      )
                                      (resultBodyM $ map Var reg_arr_mm_nms)
                              resultBodyM $ map Var reg_arr_merge_nms'
                          )
                          (resultBodyM $ map Var reg_arr_merge_nms_slc)
                pure $ redomap_res ++ shr_arr_nms'

          -- support for non-empty code2'
          --  segmap (ltid_y < ty, ltid_x < tx) {
          --    for i < rz do
          --        res = if (ii+i < d_M && jj1+ltid_y < d_Ky && jj2 + ltid_x < d_Kx)
          --              then code2' else dummy
          --        final_res[i] = res
          let redomap_res = take (length red_nes) prologue_res_list
          epilogue_res <-
            if length redomap_orig_res == length ker_res_nms
              && ker_res_nms == map patElemName redomap_orig_res
              then segMapND "rssss" segthd_lvl ResultPrivate [se1, ty, tx] $ \[_ltid_z, ltid_y, ltid_x] ->
                forM (zip kertp redomap_res) $ \(res_tp, res) -> do
                  rss_init <- scratch "rss_init" (elemType res_tp) [rz, se1, se1]
                  fmap varRes $
                    forLoop_ rz [rss_init] $ \i [rss] -> do
                      let slice = Slice [DimFix $ Var i, DimFix se0, DimFix se0]
                      thread_res <- index "thread_res" res [ltid_y, ltid_x, i]
                      letTupExp "rss" $ BasicOp $ Update Unsafe rss slice $ Var thread_res
              else segMapND "rssss" segthd_lvl ResultPrivate [se1, ty, tx] $ \[_ltid_z, ltid_y, ltid_x] -> do
                letBindNames [gtid_y] =<< toExp (le64 jj1 + le64 ltid_y)
                letBindNames [gtid_x] =<< toExp (le64 jj2 + le64 ltid_x)
                rss_init <- forM kertp $ \res_tp ->
                  scratch "rss_init" (elemType res_tp) [rz, se1, se1]
                rss <- forLoop rz rss_init $ \i rss_merge -> do
                  letBindNames [gtid_z] =<< toExp (le64 ii + le64 i)
                  forM_ (zip redomap_orig_res redomap_res) $ \(o_res, n_res) -> do
                    c <- index "redomap_thd" n_res [ltid_y, ltid_x, i]
                    letBindNames [patElemName o_res] =<< toExp (le64 c)
                    pure c
                  res_els <-
                    letTupExp' "res_elem"
                      =<< eIf
                        ( toExp $
                            le64 gtid_y
                              .<. pe64 d_Ky
                              .&&. le64 gtid_x
                                .<. pe64 d_Kx
                              .&&. le64 gtid_z
                                .<. pe64 d_M
                        )
                        ( do
                            addStms code2'
                            resultBodyM $ map Var ker_res_nms
                        )
                        (eBody $ map eBlank kertp)
                  forM (zip res_els rss_merge) $ \(res_el, rs_merge) -> do
                    let slice = Slice [DimFix $ Var i, DimFix se0, DimFix se0]
                    letExp "rss" $ BasicOp $ Update Unsafe rs_merge slice res_el
                pure $ varsRes rss

          ----------------------------------------------------------------
          -- Finally, reshape the result arrays for the RegTileReturn  ---
          ----------------------------------------------------------------
          let regtile_ret_dims =
                map (\(_, sz) -> (sz, se1, se1)) rem_outer_dims
                  ++ [(d_M, se1, rz), (d_Ky, ty, se1), (d_Kx, tx, se1)]

          epilogue_res' <- forM epilogue_res $ \res ->
            if null rem_outer_dims
              then pure res
              else do
                -- Add dummy dimensions to tile to reflect the outer dimensions
                res_tp' <- lookupType res
                let (block_dims, rest_dims) = splitAt 2 $ arrayDims res_tp'
                    ones = map (const se1) rem_outer_dims
                    new_shape = Shape $ concat [ones, block_dims, ones, rest_dims]
                letExp "res_reshaped" . BasicOp $
                  Reshape ReshapeArbitrary new_shape res

          pure $ map (RegTileReturns mempty regtile_ret_dims) epilogue_res'
        -- END (ret_seggroup, stms_seggroup) <- runBuilder $ do
        let grid = KernelGrid (Count grid_size) (Count tblock_size)
            level' = SegBlock SegNoVirt (Just grid)
            space' = SegSpace gid_flat (rem_outer_dims ++ [(gid_z, gridDim_z), (gid_y, gridDim_y), (gid_x, gridDim_x)])
            kbody' = KernelBody () stms_seggroup ret_seggroup

        pure $ Let pat aux $ Op $ SegOp $ SegMap level' space' kertp kbody'
      -- END (new_kernel, host_stms) <- runBuilder $ do
      pure $ Just (host_stms, new_kernel)
  where
    getResNm (Returns ResultMaySimplify _ (Var res_nm)) = Just res_nm
    getResNm _ = Nothing

    limitTile :: String -> SubExp -> SubExp -> Builder GPU SubExp
    limitTile t_str t d_K = letSubExp t_str $ BasicOp $ BinOp (SMin Int64) t d_K
    insertTranspose ::
      VarianceTable ->
      (VName, SubExp) ->
      (M.Map VName (Stm GPU), M.Map VName (PrimType, Stm GPU)) ->
      (VName, Stm GPU) ->
      Builder GPU (M.Map VName (Stm GPU), M.Map VName (PrimType, Stm GPU))
    insertTranspose variance (gidz, _) (tab_inn, tab_out) (p_nm, stm@(Let patt yy (BasicOp (Index arr_nm slc))))
      | [p] <- patElems patt,
        ptp <- elemType $ patElemType p,
        p_nm == patElemName p =
          case L.findIndices (variantSliceDim variance gidz) (unSlice slc) of
            [] -> pure (M.insert p_nm stm tab_inn, tab_out)
            i : _ -> do
              arr_tp <- lookupType arr_nm
              let perm = [i + 1 .. arrayRank arr_tp - 1] ++ [0 .. i]
              let arr_tr_str = baseString arr_nm ++ "_transp"
              arr_tr_nm <- letExp arr_tr_str $ BasicOp $ Manifest perm arr_nm
              let e_ind' = BasicOp $ Index arr_tr_nm slc
              let stm' = Let patt yy e_ind'
              pure (tab_inn, M.insert p_nm (ptp, stm') tab_out)
    insertTranspose _ _ _ _ = error "\nUnreachable case reached in insertTranspose case, doRegTiling3D\n"

    variantSliceDim :: VarianceTable -> VName -> DimIndex SubExp -> Bool
    variantSliceDim variance gidz (DimFix (Var vnm)) = variantToDim variance gidz vnm
    variantSliceDim _ _ _ = False
doRegTiling3D _ = pure Nothing
