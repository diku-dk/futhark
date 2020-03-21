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
       ( mmmTiling2D )
       where
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe

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
_pretty :: Pretty a => a -> String
_pretty = (++ "\n\n====================="
           ++ "========================="
           ++ "========================="
           ++ "=====================\n\n") . pretty


-- mmmTiling2D lvl space ts kbody
mmmTiling2D :: Stm Kernels -> TileM (Maybe (Stms Kernels, Stm Kernels))
mmmTiling2D stm@(Let pat aux (Op (SegOp (SegMap lvl@SegThread{} space ts old_kbody))))
  | KernelBody () kstms kres <- old_kbody,

    -- build the variance table, that records, for
    -- each variable name, the variables it depends on
    initial_variance <- M.map mempty $ scopeOfSegSpace space,
    variance <- varianceInStms initial_variance kstms,

    -- check that the code fits the pattern having:
    -- some `code1`, followed by one Screma SOAC, followed by some `code2`
    (code1, Just screma_stmt, code2)   <- matchCodeStreamCode kstms,
    Let pat_redomap aux_redomap (Op _) <- screma_stmt,

    -- checks that the Screma SOAC is actually a redomap and normalizes it
    Just (common_dim, arrs, (red_comm, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,

    -- checks that the input arrays to redomap are variant to
    -- exactly one of the two innermost dimensions of the kernel
    Just arr_var_dims <- isInvarTo1of2InnerDims mempty space variance arrs,

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

    -- we get the global-thread id for the two inner dimensions,
    --   as we are probably going to use it in code generation
    (gid_x, width_B) : (gid_y, height_A) : rem_outer_dims <- reverse $ unSegSpace space,

    -- sanity check that the reduce part is not missing
    not (null red_nes) = do
      let inp_A : inp_B : _ = arrs
      let red_ne : _ = red_nes

      -----------------------------------------------------------------------
      -- in this binder: host code and outer seggroup (ie. the new kernel) --
      -----------------------------------------------------------------------
      (new_kernel, host_stms) <- runBinder $ do -- host code

        ty_name <- nameFromString . pretty <$> newVName "Ty"
        tx_name <- nameFromString . pretty <$> newVName "Tx"
        tk_name <- nameFromString . pretty <$> newVName "Tk"
        ry_name <- nameFromString . pretty <$> newVName "Ry"
        rx_name <- nameFromString . pretty <$> newVName "Rx"
        tk <- letSubExp "Tk" $ Op $ SizeOp $ GetSize ty_name SizeTile
        ty <- letSubExp "Ty" $ Op $ SizeOp $ GetSize tx_name SizeTile
        tx <- letSubExp "Tx" $ Op $ SizeOp $ GetSize tk_name SizeTile
        ry <- letSubExp "Ry" $ Op $ SizeOp $ GetSize ry_name SizeTile
        rx <- letSubExp "Rx" $ Op $ SizeOp $ GetSize rx_name SizeTile

        tx_rx <- letSubExp "TxRx" $ BasicOp $ BinOp (Mul Int32) tx rx
        ty_ry <- letSubExp "TyRy" $ BasicOp $ BinOp (Mul Int32) ty ry
        group_size <- letSubExp "group_size" $ BasicOp $ BinOp (Mul Int32) ty tx

        grid_dimy <- letSubExp "grid_dimy" =<< ceilDiv height_A ty_ry
        grid_dimx <- letSubExp "grid_dimx" =<< ceilDiv width_B tx_rx
        grid_size <- letSubExp "grid_size" $ BasicOp $ BinOp (Mul Int32) grid_dimx grid_dimy

        gtid_y   <- newVName "gtid_y"
        gtid_x   <- newVName "gtid_x"
        gid_flat <- newVName "gid_flat"

        -----------------------------------------------
        -- in this binder: the entire outer seggroup --
        -----------------------------------------------
        (ret_seggroup, stms_seggroup) <- runBinder $ do

          acc : _ <- segMap2D "cssss" (segThread grid_size group_size)
                               ResultPrivate (ty, tx) $ \(_, _) -> do
            css <- letSubExp "css" $ BasicOp $ Replicate (Shape [ry, rx]) red_ne
            return [css]

          -------------------------
          -- build outer kk loop --
          -------------------------
          addStms code1 -- FIXME: temp hack to bring arrays into scope before kk loop.
          kk       <- newVName "kk"
          kk_bound <- letSubExp "kk_bound" =<< ceilDiv common_dim tk
          loop_kk  <- buildForLoop acc kk kk_bound $ inScopeOf code1 $ do   -- TODO: inScopeOf here..?

            a_shr : _ <- segMap2D "A_shr" (segThread grid_size group_size)
                           ResultNoSimplify (ty, tk) $ \(x, y) -> do

              -- TODO: scratch a 1D array of size Ry. risky, in that compiler
              -- might put innermost array in stupid place such as global mem
              garbage <- letExp "garbage" $ BasicOp $ Scratch float64 [ry]  -- TODO: make generic

              reconstructGtids2D ty_ry (gtid_x, gtid_y) (gid_x, gid_y) (x, y)

              i <- newVName "i"
              loop_a_shr <- buildForLoop garbage i ry $ do
                var_y <- varFromVName y
                foo <- letSubExp "foo" $ BasicOp $ SubExp $ intConst Int32 22
                a_shr <- letSubExp "a_shr" $ BasicOp $ Update garbage [DimFix var_y] foo
                return $ resultBody [a_shr]

              return [loop_a_shr]


            b_shr : _ <- segMap2D "B_shr" (segThread grid_size group_size)
                           ResultNoSimplify (tk, tx_rx) $ \(x, y) -> do
              reconstructGtids2D tx_rx (gtid_x, gtid_y) (gid_x, gid_y) (x, y)

              slice_b <- letSubExp "slice_b" $ BasicOp $ SubExp $ Var gtid_x      -- dummy
              b_shr   <- letSubExp "b_shr" $ BasicOp $ Index inp_B [DimFix slice_b]
              return [b_shr]

            ------------------------
            -- build inner k loop --
            ------------------------
            k <- newVName "k"
            let k_bound = tk
            loop_k <- buildForLoop acc k k_bound $ do
              asss : bsss : _ <- segMap2D "asss_bsss" (segThread grid_size group_size)
                                   ResultPrivate (ty, tx) $ \(thd_x, thd_y) -> do

                slice_asss <- slice2D a_shr ry (thd_y, k)
                slice_bsss <- slice2D b_shr rx (thd_x, k)

                asss <- letSubExp "asss" $ BasicOp $ Index a_shr slice_asss
                bsss <- letSubExp "bsss" $ BasicOp $ Index b_shr $ reverse slice_bsss

                return [asss, bsss]

              acc' : _ <- segMap2D "acc'" (segThread grid_size group_size)
                            ResultPrivate (ty, tx) $ \(thd_x, thd_y) -> do

                y <- varFromVName thd_y
                x <- varFromVName thd_x

                as <- letSubExp "as" $ BasicOp $ Index asss [DimFix y, DimFix x]
                bs <- letSubExp "bs" $ BasicOp $ Index bsss [DimFix x, DimFix y]

                foo <- letSubExp "foo" $ BasicOp $ SubExp $ intConst Int32 42
                -- TODO: two nested for loops here ...

                return [foo]

              acc <- varFromVName acc   -- dummy
              return $ resultBody [acc] -- dummy

            return $ resultBody [loop_k]
          --------------- END inner k loop ----------------

          return [Returns ResultNoSimplify loop_kk]
        --------------- END outer kk loop ---------------

        return $ Let pat aux $ Op $ SegOp $
                   SegMap (SegGroup (Count grid_size) (Count group_size) SegNoVirt)
                          (SegSpace gid_flat [(gid_y, grid_dimy), (gid_x, grid_dimx)])
                          ts
                          (KernelBody () stms_seggroup ret_seggroup)
      ------------------------------------------------------------------------

      trace (
                ">> host_stms:\n"  ++ _pretty host_stms
             ++ ">> new_kernel:\n" ++ _pretty new_kernel
             ++ ">> new_kernel:\n" ++ _pretty new_kernel
            ) $ return $ Just (host_stms, new_kernel)


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

        ceilDiv :: MonadBinder m => SubExp -> SubExp -> m (Exp (Lore m))
        ceilDiv x y = eDivRoundingUp Int32 (eSubExp x) (eSubExp y)

        segThread :: SubExp -> SubExp -> SegLevel
        segThread grid_size group_size =
          SegThread (Count grid_size) (Count group_size) SegNoVirt

        segGroup :: SubExp -> SubExp -> SegLevel
        segGroup grid_size group_size =
          SegGroup (Count grid_size) (Count group_size) SegNoVirt

        varFromVName :: VName -> Binder Kernels SubExp
        varFromVName name = do
          foo <- letSubExp "foo" $ BasicOp $ SubExp $ Var name
          return foo

        slice2D :: VName           -- array to slice
                -> SubExp          -- tile size
                -> (VName, VName)  -- ltid_x, ltid_y
                -> Binder Kernels (Slice SubExp)
        slice2D arr tile_size (x, y) = do
          var_x <- varFromVName x
          var_y <- varFromVName y
          i <- letSubExp "i" $ BasicOp $ BinOp (Mul Int32) var_x tile_size
          return [DimFix i, DimFix var_y]

        buildForLoop :: VName                         -- loop merge vname
                     -> VName                         -- loop variable
                     -> SubExp                        -- loop variable bound
                     -> Binder Kernels (Body Kernels) -- loop body
                     -- -> (SubExp ->                    -- loop merge init
                     --      Binder Kernels (Body Kernels)) -- loop body
                     -> Binder Kernels SubExp
        buildForLoop merge i i_bound body = do
          let name = "loop_" ++ baseString i
          merge_t  <- lookupType merge
          merge_se <- letSubExp "merge_se" $ BasicOp $ SubExp $ Var merge

          let loop_form = ForLoop i Int32 i_bound []

          loop_init <- newParam (name ++ "_init") $ toDecl merge_t Unique

          loop_body <- runBodyBinder $ inScopeOf loop_form body -- $ body merge_se

          loop <- letSubExp name $ DoLoop [] [(loop_init, merge_se)]
                                          loop_form loop_body
          return loop

mmmTiling2D _ = trace "nej" $ return Nothing

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
                   (cd1, Just strm, cd2++[stmt])
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

variantToOuterDim :: VarianceTable -> VName -> VName -> Bool
variantToOuterDim variance gid_outer nm =
  gid_outer == nm || (nameIn gid_outer $ M.findWithDefault mempty nm variance)

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
  let space = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_x, ltid_y)
    ts  <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl space ts $ KernelBody () stms' $ map (Returns manifest) res'


-- Reconstruct the original gtids from group and local IDs.
reconstructGtids2D :: SubExp -> (VName, VName) -> (VName, VName) -> (VName, VName)
                   -> Binder Kernels ()
reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y) = do
  -- Reconstruct the original gtids from gid_x/gid_y and ltid_x/ltid_y.
  letBindNames_ [gtid_x] =<<
    toExp (LeafExp gid_x int32 * primExpFromSubExp int32 tile_size +
           LeafExp ltid_x int32)
  letBindNames_ [gtid_y] =<<
    toExp (LeafExp gid_y int32 * primExpFromSubExp int32 tile_size +
           LeafExp ltid_y int32)


-- | Translates an LParam to an FParam
-- translLParamToFParam :: LParam Kernels -> FParam Kernels
-- translLParamToFParam = fmap (`toDecl` Nonunique)

-- sufficientGroups :: MonadBinder m =>
--                     [(VName, SubExp, VName, SubExp)] -- gspace
--                  -> SubExp                           -- group_size
--                  -> m (SubExp, SubExp)               -- (x, y) grid dimensions?
-- sufficientGroups gspace group_size = do
--
--   groups_in_dims <- forM gspace $ \(_, gd, _, ld) ->
--                       letSubExp "groups_in_dim" =<<
--                       eDivRoundingUp Int32 (eSubExp gd) (eSubExp ld)
--
--   num_groups <- letSubExp "num_groups" =<<
--                 foldBinOp (Mul Int32) (constant (1::Int32)) groups_in_dims
--
--   num_threads <- letSubExp "num_threads" (BasicOp (BinOp (Mul Int32) num_groups group_size))
--
--   return (num_threads, num_groups)
