{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform a restricted form of block+register tiling corresponding to
--   the following pattern:
--     * a redomap is quasi-perfectly nested inside a kernel with at
--       least two parallel dimension (the perfectly nested restriction
--       is relaxed a bit to allow for SGEMM);
--     * all streamed arrays are one dimensional;
--     * all streamed arrays are variant to exacly one of the two
--       innermost parallel dimensions, and conversely for each of
--       the two innermost parallel dimensions, there is at least
--       one streamed array variant to it;
--     * the stream's result is a tuple of scalar values, which are
--       also the "thread-in-space" return of the kernel.
--   Test code can be found in "tests/mmm/sgemm.fut".
module Futhark.Optimise.BlkRegTiling
       ( mm_BlkRegTiling )
       where
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe
import Data.Tuple

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Tools
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Representation.AST.Attributes.Names

import Debug.Trace
import Futhark.Util.Pretty



type TileM = ReaderT (Scope Kernels) (State VNameSource)
type VarianceTable = M.Map VName Names

mm_BlkRegTiling :: Stm Kernels -> TileM (Maybe (Stms Kernels, Stm Kernels))
mm_BlkRegTiling stm@(Let pat aux (Op (SegOp (SegMap SegThread{} seg_space ts old_kbody))))
  | KernelBody () kstms kres <- old_kbody,

    -- build the variance table, that records, for
    -- each variable name, the variables it depends on
    initial_variance <- M.map mempty $ scopeOfSegSpace seg_space,
    variance <- varianceInStms initial_variance kstms,

    -- check that the code fits the pattern having:
    -- some `code1`, followed by one Screma SOAC, followed by some `code2`
    (code1, Just screma_stmt, code2)   <- matchCodeStreamCode kstms,

    Let pat_redomap aux_redomap (Op _) <- screma_stmt,

    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (common_dim, arrs, (red_comm, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,

    -- checks that the input arrays to redomap are variant to
    -- exactly one of the two innermost dimensions of the kernel
    Just arr_var_dims <- isInvarTo1of2InnerDims mempty seg_space variance arrs,

    -- get the variables on which the first result of redomap depends on
    fst_res : _      <- patternValueElements pat_redomap,
    Just res_red_var <- M.lookup (patElemName fst_res) variance, -- variance of the reduce result

    -- we furthermore check that code1 is only formed by
    -- 1. statements that slice some globally-declared arrays
    --    to produce the input for the redomap, and
    -- 2. potentially some statements on which the redomap
    --    is independent; these are recorded in `code2'`
    Just (code2', arr_tab0) <- foldl (processIndirections (namesFromList arrs) res_red_var)
                                     (Just (Seq.empty, M.empty)) code1,

    -- null code2 && null code2', -- FIXME: remove the need for these assumptions

    -- we get the global-thread id for the two inner dimensions,
    --   as we are probably going to use it in code generation
    (gtid_x, width_B) : (gtid_y, height_A) : rem_outer_dims <- reverse $ unSegSpace seg_space,

    -- sanity check that the reduce part is not missing
    not (null red_nes) = do
      let load_A : load_B : _ = stmsToList code1 -- TODO: probably not safe in general, since first
                                                 -- two elements of code1 may be something else.
      let inp_A  : inp_B  : _ = arrs
      let map_t1 : map_t2 : _ = map (elemType . paramAttr) (lambdaParams map_lam)
      let red_ne : _ = red_nes
      red_t <- subExpType red_ne

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

        tx_rx      <- letSubExp "TxRx" =<< toExp (primFromSe tx * primFromSe rx)
        ty_ry      <- letSubExp "TyRy" =<< toExp (primFromSe ty * primFromSe ry)
        tk_div_tx  <- letSubExp "tk_div_tx" =<< ceilDiv tk tx
        tk_div_ty  <- letSubExp "tk_div_ty" =<< ceilDiv tk ty

        a_loc_sz  <- letSubExp "a_loc_sz" =<<
                        toExp (primFromSe ty * primFromSe ry * primFromSe tk)
                   
        b_loc_sz  <- letSubExp "b_loc_sz" =<<
                        toExp (primFromSe tk * primFromSe tx * primFromSe rx)

        gridDim_x  <- letSubExp "gridDim_x"  =<< ceilDiv width_B  tx_rx
        gridDim_y  <- letSubExp "gridDim_y"  =<< ceilDiv height_A ty_ry
        grid_size  <- letSubExp "grid_size"  =<< toExp (primFromSe gridDim_x * primFromSe gridDim_y)
        group_size <- letSubExp "group_size" =<< toExp (primFromSe ty * primFromSe tx)

        gid_x      <- newVName "gid_x"
        gid_y      <- newVName "gid_y"
        gid_flat   <- newVName "gid_flat"

        ---- in this binder: outer seggroup ----
        (ret_seggroup, stms_seggroup) <- runBinder $ do
          iii <- letExp "iii" =<< toExp (LeafExp gid_y int32 * primFromSe ty_ry)
          jjj <- letExp "jjj" =<< toExp (LeafExp gid_x int32 * primFromSe tx_rx)

          -- initialize register mem with neutral elements
          cssss_list <- segMap2D "cssss" (segThread grid_size group_size)
                          ResultPrivate (ty, tx) $ \(_,_) -> do

            css_init <- scratch "css_init" (elemType red_t) [ry, rx]
            css <- forLoop ry css_init $ \i css_merge -> do
              css' <- forLoop rx css_merge $ \j css_merge' -> do
                css'' <- updateSe "css" css_merge' [i, j] red_ne
                return $ resultBody [Var css'']
              return $ resultBody [Var css']

            return [css]
          let [cssss] = cssss_list

          full_tiles <- letSubExp "full_tiles" $ BasicOp $ BinOp (SQuot Int32) common_dim tk
          prologue_res <- forLoop full_tiles cssss $ \kk0 thd_res_merge -> do

            kk <- letExp "kk" =<< toExp (LeafExp kk0 int32 * primFromSe tk)

            -- Cosmin: copy A from global to shared memory
            a_loc_init <- scratch "A_loc" map_t1 [a_loc_sz]

            a_loc <- forLoop ry a_loc_init $ \i0 a_loc_merge -> do
              loop_a_loc <- forLoop tk_div_tx a_loc_merge $ \k0 a_loc_merge' -> do
                -- segScatter desc arr_size updt_arr lvl f
                scatter_a_loc <- segScatter "A_glb2loc" a_loc_sz a_loc_merge'
                                   (segThread grid_size group_size) $ \lid -> do
                    lid_y <- letExp "lid_y" =<< toExp (BinOpExp (SQuot Int32)
                               (LeafExp lid int32) (primFromSe tx))
                    lid_x <- letExp "lid_x" =<< toExp (BinOpExp (SRem Int32)
                               (LeafExp lid int32) (primFromSe tx))

                    k <- letExp "k" =<< toExp (LeafExp lid_x int32 +
                           LeafExp k0 int32 * primFromSe tx)
                    i <- letExp "i" =<< toExp (LeafExp lid_y int32 +
                           LeafExp i0 int32 * primFromSe ty)

                    letBindNames_ [gtid_y] =<< toExp (LeafExp iii int32 + LeafExp i int32)
                    a_col_idx <- letExp "A_col_idx" =<< toExp (LeafExp kk int32 + LeafExp k int32)

                    glb_v <- letSubExp "glb_v" =<<
                             eIf (toExp $ LeafExp gtid_y int32 .<. primFromSe height_A) -- .&&.
                                    -- LeafExp a_col_idx int32  .<. primFromSe common_dim)
                                 (do addStm load_A
                                     res <- index "A_elem" inp_A [a_col_idx]
                                     resultBodyM [Var res])
                                 (eBody $ map eBlank [Prim map_t1])
                    -- a_loc_ind <- letSubExp "a_loc_ind" =<< toExp (LeafExp k int32 +
                    --              LeafExp i int32 * primFromSe tk)
                    a_loc_ind <- letSubExp "a_loc_ind" =<<
                             eIf (toExp $ LeafExp k int32 .<. primFromSe tk)
                                 (do res <- letExp "loc_fi" =<< toExp (LeafExp k int32 +
                                            LeafExp i int32 * primFromSe tk)
                                     resultBodyM [Var res])
                                 (eBody [pure $ BasicOp $ SubExp $ intConst Int32 (-1)])
                    return (glb_v, a_loc_ind)
                let [a_loc'] = scatter_a_loc
                return $ resultBody [Var a_loc']
              return $ resultBody [Var loop_a_loc]

            -- Cosmin: copy B from global to shared memory
            b_loc_init <- scratch "B_loc" map_t2 [b_loc_sz]
            b_loc <- forLoop tk_div_ty b_loc_init $ \k0 b_loc_merge -> do
              loop_b_loc <- forLoop rx b_loc_merge $ \j0 b_loc_merge' -> do
                scatter_b_loc <- segScatter "B_glb2loc" b_loc_sz b_loc_merge'
                      (segThread grid_size group_size) $ \lid -> do

                  lid_y <- letExp "lid_y" $ BasicOp $ BinOp (SQuot Int32) (Var lid) tx
                  lid_x <- letExp "lid_x" $ BasicOp $ BinOp (SRem Int32)  (Var lid) tx

                  k <- letExp "k" =<< toExp (LeafExp lid_y int32 +
                         LeafExp k0 int32 * primFromSe ty)
                  j <- letExp "j" =<< toExp (LeafExp lid_x int32 +
                         LeafExp j0 int32 * primFromSe tx)

                  letBindNames_ [gtid_x]  =<< toExp (LeafExp jjj int32 + LeafExp j int32)
                  b_row_idx <- letExp "B_row_idx" =<< toExp (LeafExp kk int32 + LeafExp k int32)

                  glb_v <- letSubExp "glb_v" =<<
                             eIf (toExp $
                                    LeafExp gtid_x int32    .<. primFromSe width_B) -- .&&.
                                    -- LeafExp b_row_idx int32 .<. primFromSe common_dim)
                                 (do addStm load_B
                                     res <- index "B_elem" inp_B [b_row_idx]
                                     resultBodyM [Var res])
                                 (eBody $ map eBlank [Prim map_t2])

                  -- b_loc_ind <- letSubExp "b_loc_ind" =<< toExp (LeafExp j int32 +
                  --              LeafExp k int32 * primFromSe tx_rx)
                  b_loc_ind <- letSubExp "b_loc_ind" =<<
                           eIf (toExp $ LeafExp k int32 .<. primFromSe tk)
                               (do res <- letExp "loc_fi" =<< toExp (LeafExp j int32 +
                                            LeafExp k int32 * primFromSe tx_rx)
                                   resultBodyM [Var res])
                               (eBody [pure $ BasicOp $ SubExp $ intConst Int32 (-1)])
                  return (glb_v, b_loc_ind)
                let [b_loc'] = scatter_b_loc
                return $ resultBody [Var b_loc']
              return $ resultBody [Var loop_b_loc]
            -- ===============================
            -- inner loop updating this thread's accumulator (loop k in mmm_kernels)
            thd_acc <- forLoop tk thd_res_merge $ \k acc_merge -> do

              -- before the redomap, write from local to register mem
              reg_mem <- segMap2D "reg_mem" (segThread grid_size group_size)
                           ResultPrivate (ty, tx) $ \(thd_y, thd_x) -> do

                asss_init <- scratch "asss_init" map_t1 [ry]
                bsss_init <- scratch "bsss_init" map_t2 [rx]

                asss <- forLoop ry asss_init $ \i asss_merge -> do
                  -- COSMIN re-wrote this
                  a_loc_ind <- letExp "a_loc_ind" =<< toExp (LeafExp k int32 +
                                 (LeafExp thd_y int32 * primFromSe ry +
                                  LeafExp i int32) * primFromSe tk)

                  asss <- update "asss" asss_merge [i] =<< index "A_loc_elem" a_loc [a_loc_ind]
                  return $ resultBody [Var asss]

                -- TODO: similarly for B_loc?
                bsss <- forLoop rx bsss_init $ \j bsss_merge -> do
                  -- COSMIN re-wrote this
                  b_loc_ind <- letExp "b_loc_ind" =<< toExp (LeafExp j int32 +
                                 LeafExp k int32 * primFromSe tx_rx +
                                 LeafExp thd_x int32 * primFromSe rx)

                  bsss <- update "bsss" bsss_merge [j] =<< index "B_loc_elem" b_loc [b_loc_ind]
                  return $ resultBody [Var bsss]
                return [asss, bsss]

              let [asss, bsss] = reg_mem

              -- the actual redomap
              redomap_res <- segMap2D "redomap_res" (segThread grid_size group_size)
                               ResultPrivate (ty, tx) $ \(thd_y, thd_x) -> do

                as <- indexSubArr "as" asss [thd_y, thd_x] [ry]
                bs <- indexSubArr "bs" bsss [thd_y, thd_x] [rx]
                css_init <- indexSubArr "css_init" acc_merge [thd_y, thd_x] [ry, rx]

                css <- forLoop ry css_init $ \i css_merge -> do

                  a <- index "a" as [i]
                  css <- forLoop rx css_merge $ \j css_merge' -> do

                    b <- index "b" bs [j]
                    c <- index "c" css_merge' [i, j]

                    map_res  <- newVName "map_res"
                    map_lam' <- renameLambda map_lam
                    red_lam' <- renameLambda red_lam

                    addStms $ rebindLambda map_lam' [a, b] map_res
                           <> rebindLambda red_lam' [c, map_res] c

                    css <- update "css" css_merge' [i, j] c

                    return $ resultBody [Var css]
                  return $ resultBody [Var css]
                return [css]

              -- TODO: where to put code2..? ie. code following the redomap
              return $ resultBody $ map Var redomap_res
            --------------- END inner k loop ----------------

            return $ resultBody [Var thd_acc]
          --------------- END outer kk0 loop ---------------

          --------------- START epilogue ---------------
          -- TODO: build the epilogue here. will probably be copy paste.
          -- TODO: should we guard the epilogue with an if statement?

          epilogue_res <- do

            t0 <- letSubExp "t0" $ BasicOp $ BinOp (SQuot Int32) common_dim tk
            kk <- letExp "kk" =<< toExp (primFromSe t0 * primFromSe tk)

            a_loc_init <- scratch "A_loc" map_t1 [a_loc_sz]
            -- Cosmin: copy A from global to local memory
            a_loc <- forLoop ry a_loc_init $ \i0 a_loc_merge -> do
              loop_a_loc <- forLoop tk_div_tx a_loc_merge $ \k0 a_loc_merge' -> do
                -- segScatter desc arr_size updt_arr lvl f
                scatter_a_loc <- segScatter "A_glb2loc" a_loc_sz a_loc_merge'
                                   (segThread grid_size group_size) $ \lid -> do
                    lid_y <- letExp "lid_y" =<< toExp (BinOpExp (SQuot Int32)
                               (LeafExp lid int32) (primFromSe tx))
                    lid_x <- letExp "lid_x" =<< toExp (BinOpExp (SRem Int32)
                               (LeafExp lid int32) (primFromSe tx))

                    k <- letExp "k" =<< toExp (LeafExp lid_x int32 +
                           LeafExp k0 int32 * primFromSe tx)
                    i <- letExp "i" =<< toExp (LeafExp lid_y int32 +
                           LeafExp i0 int32 * primFromSe ty)

                    letBindNames_ [gtid_y]  =<< toExp (LeafExp iii int32 + LeafExp i int32)
                    a_col_idx <- letExp "A_col_idx" =<< toExp (LeafExp kk int32  + LeafExp k int32)

                    glb_v <- letSubExp "glb_v" =<<
                             eIf (toExp $
                                    LeafExp gtid_y int32 .<. primFromSe height_A .&&.
                                    LeafExp a_col_idx int32 .<. primFromSe common_dim)
                                 (do addStm load_A
                                     res <- index "A_elem" inp_A [a_col_idx]
                                     resultBodyM [Var res])
                                 (eBody $ map eBlank [Prim map_t1])
                    ind <- letSubExp "ind" =<<
                             eIf (toExp $ LeafExp k int32 .<. primFromSe tk)
                                 (do res <- letExp "loc_fi" =<< toExp (LeafExp k int32 +
                                            LeafExp i int32 * primFromSe tk)
                                     resultBodyM [Var res])
                                 (eBody [pure $ BasicOp $ SubExp $ intConst Int32 (-1)])
                    return (glb_v, ind)
                let [a_loc'] = scatter_a_loc
                return $ resultBody [Var a_loc']
              return $ resultBody [Var loop_a_loc]

            b_loc_init <- scratch "B_loc" map_t2 [b_loc_sz]
            -- Cosmin: copy B from global to shared memory
            b_loc <- forLoop tk_div_ty b_loc_init $ \k0 b_loc_merge -> do
              loop_b_loc <- forLoop rx b_loc_merge $ \j0 b_loc_merge' -> do
                scatter_b_loc <- segScatter "B_glb2loc" b_loc_sz b_loc_merge'
                      (segThread grid_size group_size) $ \lid -> do

                  lid_y <- letExp "lid_y" =<< toExp (BinOpExp (SQuot Int32)
                             (LeafExp lid int32) (primFromSe tx))
                  lid_x <- letExp "lid_x" =<< toExp (BinOpExp (SRem Int32)
                             (LeafExp lid int32) (primFromSe tx))

                  k <- letExp "k" =<< toExp (LeafExp lid_y int32 +
                         LeafExp k0 int32 * primFromSe ty)
                  j <- letExp "j" =<< toExp (LeafExp lid_x int32 +
                         LeafExp j0 int32 * primFromSe tx)

                  letBindNames_ [gtid_x]  =<< toExp (LeafExp jjj int32 + LeafExp j int32)
                  b_row_idx <- letExp "B_row_idx" =<< toExp (LeafExp kk int32 + LeafExp k int32)

                  glb_v <- letSubExp "glb_v" =<<
                             eIf (toExp $
                                    LeafExp gtid_x int32 .<. primFromSe width_B .&&.
                                    LeafExp b_row_idx int32  .<. primFromSe common_dim)
                                 (do addStm load_B
                                     res <- index "B_elem" inp_B [b_row_idx]
                                     resultBodyM [Var res])
                                 (eBody $ map eBlank [Prim map_t2])
                  loc_ind <- letSubExp "loc_ind" =<<
                           eIf (toExp $ LeafExp k int32 .<. primFromSe tk)
                               (do res <- letExp "loc_fi" =<< toExp (LeafExp j int32 +
                                            LeafExp k int32 * primFromSe tx_rx)
                                   resultBodyM [Var res])
                               (eBody [pure $ BasicOp $ SubExp $ intConst Int32 (-1)])
                  return (glb_v, loc_ind)
                let [b_loc'] = scatter_b_loc
                return $ resultBody [Var b_loc']
              return $ resultBody [Var loop_b_loc]

            -- inner loop updating this thread's accumulator (loop k in mmm_kernels)
            thd_acc <- forLoop tk prologue_res $ \k acc_merge -> do

              -- before the redomap, write from shared to register mem
              reg_mem <- segMap2D "reg_mem" (segThread grid_size group_size)
                           ResultPrivate (ty, tx) $ \(thd_y, thd_x) -> do

                -- copy from shared mem to register mem
                asss_init <- scratch "asss_init" map_t1 [ry]
                bsss_init <- scratch "bsss_init" map_t2 [rx]

                asss <- forLoop ry asss_init $ \i asss_merge -> do
                  -- COSMIN re-wrote this
                  a_loc_ind <- letExp "a_loc_ind" =<< toExp (LeafExp k int32 +
                                 (LeafExp thd_y int32 * primFromSe ry +
                                  LeafExp i int32) * primFromSe tk)

                  asss <- update "asss" asss_merge [i] =<< index "A_loc_elem" a_loc [a_loc_ind]
                  return $ resultBody [Var asss]

                -- TODO: similarly for B_loc?
                bsss <- forLoop rx bsss_init $ \j bsss_merge -> do
                  -- COSMIN re-wrote this
                  b_loc_ind <- letExp "b_loc_ind" =<< toExp (LeafExp j int32 +
                                 LeafExp k int32 * primFromSe tx_rx +
                                 LeafExp thd_x int32 * primFromSe rx)

                  bsss <- update "bsss" bsss_merge [j] =<< index "B_loc_elem" b_loc [b_loc_ind]
                  return $ resultBody [Var bsss]
                return [asss, bsss]

              let [asss, bsss] = reg_mem

              -- the actual redomap
              redomap_res <- segMap2D "redomap_res" (segThread grid_size group_size)
                               ResultPrivate (ty, tx) $ \(thd_y, thd_x) -> do

                as <- indexSubArr "as" asss [thd_y, thd_x] [ry]
                bs <- indexSubArr "bs" bsss [thd_y, thd_x] [rx]
                css_init <- indexSubArr "css_init" acc_merge [thd_y, thd_x] [ry, rx]

                css <- forLoop ry css_init $ \i css_merge -> do

                  a <- index "a" as [i]
                  css <- forLoop rx css_merge $ \j css_merge' -> do

                    b <- index "b" bs [j]
                    c <- index "c" css_merge' [i, j]

                    map_res  <- newVName "map_res"
                    map_lam' <- renameLambda map_lam
                    red_lam' <- renameLambda red_lam

                    addStms $ rebindLambda map_lam' [a, b] map_res
                           <> rebindLambda red_lam' [c, map_res] c

                    css <- update "css" css_merge' [i, j] c

                    return $ resultBody [Var css]
                  return $ resultBody [Var css]
                return [css]

              -- TODO: where to put code2..? ie. code following the redomap
              return $ resultBody $ map Var redomap_res
            --------------- END inner k loop ----------------

            return thd_acc -- $ resultBody [Var thd_acc]
          --------------- END outer kk0 loop ---------------


          --------------- END epilogue -----------------
          -- TODO: RegTileReturns still a work in progress.
          return [RegTileReturns [(height_A, ty, ry), (width_B, tx, rx)]
                                 epilogue_res]
        --------------- END outer seggroup ---------------


        let level' = SegGroup (Count grid_size) (Count group_size) SegNoVirt
            space' = SegSpace gid_flat [(gid_y, gridDim_y), (gid_x, gridDim_x)]
            kbody' = KernelBody () stms_seggroup ret_seggroup
        return $ Let pat aux $ Op $ SegOp $ SegMap level' space' ts kbody'

      -- return $ Just (host_stms, new_kernel)
      trace (pretty host_stms ++ "\n" ++ pretty new_kernel) $ return $ Just (host_stms, new_kernel)

  where -- | There are two supported cases here:
        --   1. the statement is a slice that produces one of the
        --      arrays that are input to redomap. Also the streamed
        --      array is one dimensional. This info is accumulated
        --      in a table for later use.
        --   2. the redomap does not depend on this statement, hence
        --      this statement may as well come after redomap.
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

        primFromSe :: SubExp -> PrimExp VName
        primFromSe se = primExpFromSubExp int32 se

        ceilDiv :: MonadBinder m => SubExp -> SubExp -> m (Exp (Lore m))
        ceilDiv x y = eDivRoundingUp Int32 (eSubExp x) (eSubExp y)

        segThread :: SubExp -> SubExp -> SegLevel
        segThread grid_size group_size =
          SegThread (Count grid_size) (Count group_size) SegNoVirtFull

        scratch :: MonadBinder m => String -> PrimType -> [SubExp] -> m VName
        scratch se_name t shape = letExp se_name $ BasicOp $ Scratch t shape


        -- index a subarray given by outer_indices, slicing the entirety of
        -- any inner dimensions given in inner_dims.
        indexSubArr :: MonadBinder m => String -> VName -> [VName] -> [SubExp] -> m VName
        indexSubArr se_desc arr outer_indices inner_dims =
          letExp se_desc $ BasicOp $ Index arr indices
          where
            inner_slices = map (\inner_dim -> DimSlice (intConst Int32 0)
                                             inner_dim (intConst Int32 1))
                               inner_dims
            indices = map (DimFix . Var) outer_indices ++ inner_slices

        index :: MonadBinder m => String -> VName -> [VName] -> m VName
        index se_desc arr indices = indexSubArr se_desc arr indices []

        update :: MonadBinder m => String -> VName -> [VName] -> VName -> m VName
        update se_desc arr indices new_elem =
          letExp se_desc $ BasicOp $ Update arr (map (DimFix . Var) indices) (Var new_elem)

        updateSe :: MonadBinder m => String -> VName -> [VName] -> SubExp -> m VName
        updateSe se_desc arr indices new_elem =
          letExp se_desc $ BasicOp $ Update arr (map (DimFix . Var) indices) new_elem

        forLoop :: SubExp
                -> VName
                -> (VName -> VName  -- loop var -> loop init -> loop body
                    -> Binder Kernels (Body Kernels))
                -> Binder Kernels VName
        forLoop i_bound merge body = do
          i <- newVName "i"     -- could give this as arg to the function
          let desc = "loop" -- "loop_" ++ baseString i

          let loop_form = ForLoop i Int32 i_bound []

          merge_t <- lookupType merge
          loop_init <- newParam "merge" $ toDecl merge_t Unique
          loop_body <- runBodyBinder $ inScopeOf loop_form $
            localScope (scopeOfFParams [loop_init]) $ body i (paramName loop_init)

          return =<< letExp desc $ DoLoop [] [(loop_init, Var merge)]
                                          loop_form loop_body

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
            idents = map (\param -> Ident (paramName param) (paramAttr param))
                         lam_params
            lam_res : _ = bodyResult lam_body

mm_BlkRegTiling _ = do
  traceM "nej"
  return Nothing

---------------
--- HELPERS ---
---------------

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
          if not branch_invariant then Nothing     -- if i or j in branch_variant; return nothing
          else if nameIn i variant_to && not (nameIn j variant_to) then Just 0
          else if nameIn j variant_to && not (nameIn i variant_to) then Just 1
          else Nothing


varianceInStms :: VarianceTable -> Stms Kernels -> VarianceTable
varianceInStms = foldl varianceInStm

-- variantToOuterDim :: VarianceTable -> VName -> VName -> Bool
-- variantToOuterDim variance gid_outer nm =
--   gid_outer == nm || (nameIn gid_outer $ M.findWithDefault mempty nm variance)

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
             -> Binder Kernels [VName])
         -> Binder Kernels [VName]
segMap2D desc lvl manifest (dim_y, dim_x) f = do
  ltid_x    <- newVName "ltid_x"
  ltid_y    <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid_y, dim_y), (ltid_x, dim_x)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_y, ltid_x)
    ts  <- mapM lookupType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms $ map Var res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl segspace ts $ KernelBody () stms' $ map (Returns manifest) res'


{--
segMap1D :: String
         -> SegLevel -> ResultManifest
         -> (VName -> Binder Kernels [SubExp])
         -> Binder Kernels [VName]
segMap1D desc lvl manifest f = do
  ltid <- newVName "ltid"
  ltid_flat <- newVName "ltid_flat"
  let space = SegSpace ltid_flat [(ltid, unCount $ segGroupSize lvl)]

  ((ts, res), stms) <- runBinder $ do
    res <- f ltid
    ts <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl space ts $ KernelBody () stms' $ map (Returns manifest) res'


segScatter "A_glb2shr"
                        (segThread grid_size group_size) group_size $ \lid

WriteReturns
                    [SubExp] -- Size of array.  Must match number of dims.
                    VName -- Which array
                    [([SubExp], SubExp)]

--}

segScatter :: String   -- desc
           -> SubExp
           -> VName
           -> SegLevel -- lvl
           -> (VName -> Binder Kernels (SubExp, SubExp)) -- f
           -> Binder Kernels [VName]
segScatter desc arr_size updt_arr lvl f = do
  ltid <- newVName "ltid"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid, unCount $ segGroupSize lvl)]

  ((t_v, res_v, res_i), kstms) <- runBinder $ do
    (res_v, res_i) <- f ltid
    t_v <- subExpType res_v
    return (t_v, res_v, res_i)

  Body _ kstms' res' <- renameBody $ mkBody kstms [res_i, res_v]
  let [res_i', res_v'] = res'
  let krets' = WriteReturns [arr_size] updt_arr [ ([res_i'], res_v') ]
  let body   = KernelBody () kstms' [krets']
  --body <- renameBody $ mkBody kstms' krets
  letTupExp desc $ Op $ SegOp $ SegMap lvl segspace [t_v] $ body
