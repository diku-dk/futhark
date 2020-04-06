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
    -- Just arr_var_dims <- isInvarTo1of2InnerDims mempty space variance arrs,

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
    (bid_x, width_B) : (bid_y, height_A) : rem_outer_dims <- reverse $ unSegSpace space,

    -- sanity check that the reduce part is not missing
    not (null red_nes) = do
      let inp_A : inp_B : _ = arrs
      let red_ne : _ = red_nes

      -----------------------------------------------------------------------
      -- in this binder: host code and outer seggroup (ie. the new kernel) --
      -----------------------------------------------------------------------
      (new_kernel, host_stms) <- runBinder $ do -- host code

        tk_name    <- nameFromString . pretty <$> newVName "Tk"
        tx_name    <- nameFromString . pretty <$> newVName "Tx"
        ty_name    <- nameFromString . pretty <$> newVName "Ty"
        rx_name    <- nameFromString . pretty <$> newVName "Rx"
        ry_name    <- nameFromString . pretty <$> newVName "Ry"
        tk         <- letSubExp "Tk" $ Op $ SizeOp $ GetSize ty_name SizeTile
        tx         <- letSubExp "Tx" $ Op $ SizeOp $ GetSize tk_name SizeTile
        ty         <- letSubExp "Ty" $ Op $ SizeOp $ GetSize tx_name SizeTile
        rx         <- letSubExp "Rx" $ Op $ SizeOp $ GetSize rx_name SizeTile
        ry         <- letSubExp "Ry" $ Op $ SizeOp $ GetSize ry_name SizeTile

        tx_rx      <- letSubExp "TxRx"       $ BasicOp $ BinOp (Mul Int32) tx rx
        ty_ry      <- letSubExp "TyRy"       $ BasicOp $ BinOp (Mul Int32) ty ry
        block_size <- letSubExp "block_size" $ BasicOp $ BinOp (Mul Int32) ty tx

        gridDim_x  <- letSubExp "gridDim_x" =<< ceilDiv width_B  tx_rx
        gridDim_y  <- letSubExp "gridDim_y" =<< ceilDiv height_A ty_ry
        grid_size  <- letSubExp "grid_size" $ BasicOp $ BinOp (Mul Int32) gridDim_x gridDim_y

        -- gtid_x     <- newVName "gtid_x"
        -- gtid_y     <- newVName "gtid_y"
        bid_flat   <- newVName "bid_flat"

        -----------------------------------------------
        -- in this binder: the entire outer seggroup --
        -----------------------------------------------
        (ret_seggroup, stms_seggroup) <- runBinder $ do
          addStms code1 -- TODO: temp hack to bring arrays into scope before kk loop.

          [cssss] <- segMap2D "cssss" (segThread grid_size block_size)
                       ResultPrivate (tx, ty) $ \(_, _) -> do
            css <- letSubExp "css" $ BasicOp $ Replicate (Shape [rx, ry]) red_ne
            return [css]

          -------------------------
          -- build outer kk loop --
          -------------------------
          kk_bound <- letSubExp "kk_bound" =<< ceilDiv common_dim tk
                                                                    -- TODO: is inScopeOf enough..?
          loop_kk <- forLoop kk_bound cssss $ \kk -> inScopeOf code1 $ do
            kk' <- letExp "kk'" $ BasicOp $ BinOp (Mul Int32) (Var kk) tk

            iii <- letSubExp "iii" $ BasicOp $ BinOp (Mul Int32) (Var bid_y) ty_ry
            jjj <- letSubExp "jjj" $ BasicOp $ BinOp (Mul Int32) (Var bid_x) tx_rx

            [a_shr] <- segMap2D "A_shr" (segThread grid_size block_size)
                         ResultNoSimplify (tk, ty) $ \(thd_x, thd_y) -> do

              a_shr_init <- scratch "A_shr_init" float64 [ry]  -- TODO: make type generic
              loop_a_shr <- forLoop ry a_shr_init $ \i -> do
                t0 <- letSubExp "t0" $ BasicOp $ BinOp (Mul Int32) ty (Var i)
                i' <- letSubExp "i'" $ BasicOp $ BinOp (Add Int32) t0 (Var thd_y)

                -- a_row <- letSubExp "a_row" $ BasicOp $ BinOp (Add Int32) iii i'
                a_col <- letSubExp "a_col" $ BasicOp $ BinOp (Add Int32) (Var kk') (Var thd_x)

                a_elem <- indexSingle "A_elem" inp_A [{-a_row, -}a_col] -- TODO: how to index input arrays?
                a_shr  <- update "A_shr" a_shr_init [Var i] a_elem
                return $ resultBody [a_shr]

              return [loop_a_shr]

            [b_shr] <- segMap2D "B_shr" (segThread grid_size block_size)
                         ResultNoSimplify (tk, tx) $ \(thd_x, thd_y) -> do

              b_shr_init <- scratch "B_shr_init" float64 [rx] -- TODO: make type generic
              loop_b_shr <- forLoop rx b_shr_init $ \j -> do
                t0 <- letSubExp "t0" $ BasicOp $ BinOp (Mul Int32) tx (Var j)
                j' <- letSubExp "j'" $ BasicOp $ BinOp (Add Int32) t0 (Var thd_x)

                -- b_row <- letSubExp "b_row" $ BasicOp $ BinOp (Add Int32) jjj j'
                b_col <- letSubExp "b_col" $ BasicOp $ BinOp (Add Int32) (Var kk') (Var thd_y)

                b_elem <- indexSingle "B_elem" inp_B [{-b_row, -}b_col] -- TODO: how to index input arrays?
                b_shr  <- update "B_shr" b_shr_init [Var j] b_elem
                return $ resultBody [b_shr]

              return [loop_b_shr]

            ------------------------
            -- build inner k loop --
            ------------------------
            loop_k <- forLoop tk cssss $ \k -> do
              [asss, bsss] <- segMap2D "asss_bsss" (segThread grid_size block_size)
                                ResultPrivate (tx, ty) $ \(thd_x, thd_y) -> do

                asss_init <- scratch "asss_init" float64 [ry] -- TODO: make type generic
                bsss_init <- scratch "bsss_init" float64 [rx] -- TODO: make type generic

                asss <- forLoop ry asss_init $ \i -> do
                  -- in kernel code: A_shr :: [Ty][Tk][Ry]
                  -- indexed as:     A_shr[thd_y][thd_x][i]
                  --
                  -- here:           A_shr :: [Tk][Ty][Ry]
                  -- indexed as:     A_shr[thd_x][thd_y][i]

                  a_shr_elem <- indexSingle "A_shr_elem" a_shr $ map Var [thd_x, thd_y, i] -- TODO: dummy index
                  asss       <- update "asss" asss_init [Var i] a_shr_elem
                  return $ resultBody [asss]


                bsss <- forLoop rx bsss_init $ \j -> do

                  b_shr_elem <- indexSingle "B_shr_elem" b_shr $ map Var [thd_x, thd_y, j] -- TODO: dummy index
                  bsss       <- update "bsss" bsss_init [Var j] b_shr_elem
                  return $ resultBody [bsss]

                return [asss, bsss]

              [res] <- segMap2D "res" (segThread grid_size block_size)
                         ResultPrivate (tx, ty) $ \(thd_x, thd_y) -> do

                let (x, y) = (Var thd_x, Var thd_y)

                as <- index "as" asss [x, y] [ry]
                bs <- index "bs" bsss [x, y] [rx]
                css_init <- index "css_init" cssss [x, y] [rx, ry]

                css <- forLoop ry css_init $ \i -> do

                  as_elem <- indexSingle "as_elem" as [Var i]

                  css <- forLoop rx css_init $ \j -> do

                    bs_elem  <- indexSingle "bs_elem" bs [Var j]
                    css_elem <- indexSingle "css_elem" css_init [Var i, Var j]

                    map_res  <- newVName "map_res"
                    map_lam' <- renameLambda map_lam
                    red_lam' <- renameLambda red_lam

                    rebound_map_stms <- rebindLambda map_lam' ([as_elem,  bs_elem], map_res)
                    rebound_red_stms <- rebindLambda red_lam' ([css_elem, map_res], css_elem)

                    addStms rebound_map_stms
                    addStms rebound_red_stms

                    css <- update "css" css_init [Var i, Var j] css_elem

                    return $ resultBody [css] -- TODO: what should be returned in these three?
                  return $ resultBody [css]
                return [css]

              return $ resultBody [Var res] -- dummy
            --------------- END inner k loop ----------------

            return $ resultBody [loop_k]
          --------------- END outer kk loop ---------------

          return [Returns ResultNoSimplify loop_kk]
        --------------- END outer seggroup ---------------

        return $ Let pat aux $ Op $ SegOp $
                   SegMap (SegGroup (Count grid_size) (Count block_size) SegNoVirt)
                          (SegSpace bid_flat [(bid_x, gridDim_x), (bid_y, gridDim_y)])
                          ts
                          (KernelBody () stms_seggroup ret_seggroup)

      ------------------------------------------------------------------------

      trace (
              _pretty old_kbody  ++ "\n" ++
               pretty host_stms  ++ "\n" ++
              _pretty new_kernel ++
            "") $ return $ Just (host_stms, new_kernel)


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
        segThread grid_size block_size =
          SegThread (Count grid_size) (Count block_size) SegNoVirt


        -- segGroup :: SubExp -> SubExp -> SegLevel
        -- segGroup grid_size block_size =
        --   SegGroup (Count grid_size) (Count block_size) SegNoVirt


        -- slice2D :: VName           -- array to slice
        --         -> SubExp          -- tile size
        --         -> (VName, VName)  -- ltid_x, ltid_y
        --         -> Binder Kernels [SubExp]
        -- slice2D arr tile_size (x, y) = do
        --   x' <- letSubExp (baseString x) $ BasicOp $ BinOp (Mul Int32) (Var x) tile_size
        --   return [x', Var y]


        scratch :: MonadBinder m => String -> PrimType -> [SubExp] -> m VName
        -- scratch se_name t shape = do
        --   letExp se_name $ BasicOp $ Scratch t shape
        scratch se_name t shape = letExp se_name $ BasicOp $ Scratch t shape


        -- index an array, slicing the entirety of some inner dimensions.
        index :: MonadBinder m => String -> VName -> [SubExp] -> [SubExp] -> m VName
        index se_desc arr outer_indices inner_dims =
          letExp se_desc $ BasicOp $ Index arr indices
          where
            inner_slices = map (\inner_dim -> DimSlice  (intConst Int32 0)
                                              inner_dim (intConst Int32 1))
                               inner_dims
            indices = map DimFix outer_indices ++ inner_slices


        indexSingle :: MonadBinder m => String -> VName -> [SubExp] -> m VName
        indexSingle se_desc arr indices = index se_desc arr indices []


        -- update :: MonadBinder m => String -> VName -> [SubExp] -> SubExp -> m SubExp
        -- update se_desc arr indices new_elem =
        --   letSubExp se_desc $ BasicOp $ Update arr (map DimFix indices) new_elem
        update :: MonadBinder m => String -> VName -> [SubExp] -> VName -> m SubExp
        update se_desc arr indices new_elem =
          letSubExp se_desc $ BasicOp $ Update arr (map DimFix indices) (Var new_elem)


        forLoop :: SubExp    -- loop var bound
                -> VName     -- merge
                -> (VName -> Binder Kernels (Body Kernels)) -- loop var -> loop body
                -> Binder Kernels SubExp
        forLoop i_bound merge body = do
          i <- newVName "i"

          let desc = "loop_" ++ baseString i

          merge_t  <- lookupType merge
          merge_se <- letSubExp "merge_se" $ BasicOp $ SubExp $ Var merge

          let loop_form = ForLoop i Int32 i_bound []
          loop_init <- newParam (desc ++ "_init") $ toDecl merge_t Unique
          loop_body <- runBodyBinder $ inScopeOf loop_form $ body i

          return =<< letSubExp desc $ DoLoop [] [(loop_init, merge_se)]
                                             loop_form loop_body

        -- given a lambda "lam", a list "new_params" of new
        -- parameters which should be applied to the lambda,
        -- and a VName "res_name" which the lambda result should
        -- be bound to:
        --   creates Stms corresponding to binding of new_params,
        --   lambda body, and binding of lambda result to res_name.
        rebindLambda :: MonadBinder m =>
                        Lambda Kernels
                     -> ([VName], VName)
                     -> m (Stms Kernels)
        rebindLambda lam (new_params, res_name) = return $
          (stmsFromList $
            map (\(ident, new_param) ->
                      mkLet [] [ident] $ BasicOp $ SubExp $ Var new_param)
                (zip idents new_params))
          <> bodyStms lam_body
          <> oneStm (mkLet [] [Ident res_name lam_ret_type] $ BasicOp $ SubExp lam_res)
          where
            (lam_params, lam_body, lam_ret_type : _) =
              (lambdaParams lam, lambdaBody lam, lambdaReturnType lam)
            idents = map (\param -> Ident (paramName param) (paramAttr param))
                         lam_params
            lam_res : _ = bodyResult lam_body



mmmTiling2D _ = do trace "nej" $ return Nothing

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
-- isInvarTo1of2InnerDims :: Names -> SegSpace -> VarianceTable -> [VName]
--                        -> Maybe [Int]
-- isInvarTo1of2InnerDims branch_variant kspace variance arrs =
--   let inner_perm0 = map varToOnly1of2InnerDims arrs
--       inner_perm  = catMaybes inner_perm0
--       ok1 = elem 0 inner_perm && elem 1 inner_perm
--       ok2 = length inner_perm0 == length inner_perm
--   in  if ok1 && ok2 then Just inner_perm else Nothing
--   where varToOnly1of2InnerDims :: VName -> Maybe Int
--         varToOnly1of2InnerDims arr = do
--           (j, _) : (i, _) : _ <- Just $ reverse $ unSegSpace kspace
--           let variant_to       = M.findWithDefault mempty arr variance
--               branch_invariant = not $ nameIn j branch_variant ||
--                                        nameIn i branch_variant
--           if not branch_invariant then Nothing     -- if i or j in branch_variant; return nothing
--           else if nameIn i variant_to && not (nameIn j variant_to) then Just 0
--           else if nameIn j variant_to && not (nameIn i variant_to) then Just 1
--           else Nothing


varianceInStms :: VarianceTable -> Stms Kernels -> VarianceTable
varianceInStms = foldl varianceInStm

-- variantToOuterDim :: VarianceTable -> VName -> VName -> Bool
-- variantToOuterDim variance bid_outer nm =
--   bid_outer == nm || (nameIn bid_outer $ M.findWithDefault mempty nm variance)

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
  let space_ = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_x, ltid_y)
    ts  <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl space_ ts $ KernelBody () stms' $ map (Returns manifest) res'


-- Reconstruct the original gtid_s from block and thread indices.
-- reconstructGtids :: SubExp -> (VName, VName) -> (VName, VName) -> (VName, VName)
--                     -> Binder Kernels ()
-- reconstructGtids tile_size (gtid_x, gtid_y) (bid_x, bid_y) (ltid_x, ltid_y) = do
--   -- Reconstruct the original gtids from bid_x/bid_y and ltid_x/ltid_y.
--   letBindNames_ [gtid_x] =<<
--     toExp (LeafExp bid_x int32 * primExpFromSubExp int32 tile_size +
--            LeafExp ltid_x int32)
--   letBindNames_ [gtid_y] =<<
--     toExp (LeafExp bid_y int32 * primExpFromSubExp int32 tile_size +
--            LeafExp ltid_y int32)


-- we can only generate normalized loops, so this is
-- used to reconstruct loop variables in anormal loops.
-- reconstructLoopVar :: VName  -- loop variable
--                    -> SubExp -- tile size
--                    -> VName  -- thread index
--                    -> Binder Kernels ()
-- reconstructLoopVar i tile_size tid =
--   letBindNames_ [i] =<<
--     toExp (LeafExp i int32 * primExpFromSubExp int32 tile_size +
--            LeafExp tid int32)


-- | Translates an LParam to an FParam
-- translLParamToFParam :: LParam Kernels -> FParam Kernels
-- translLParamToFParam = fmap (`toDecl` Nonunique)

-- sufficientGroups :: MonadBinder m =>
--                     [(VName, SubExp, VName, SubExp)] -- gspace
--                  -> SubExp                           -- block_size
--                  -> m (SubExp, SubExp)               -- (x, y) grid dimensions?
-- sufficientGroups gspace block_size = do
--
--   groups_in_dims <- forM gspace $ \(_, gd, _, ld) ->
--                       letSubExp "groups_in_dim" =<<
--                       eDivRoundingUp Int32 (eSubExp gd) (eSubExp ld)
--
--   num_groups <- letSubExp "num_groups" =<<
--                 foldBinOp (Mul Int32) (constant (1::Int32)) groups_in_dims
--
--   num_threads <- letSubExp "num_threads" (BasicOp (BinOp (Mul Int32) num_groups block_size))
--
--   return (num_threads, num_groups)


{-
from A to A_shr:
  in the kernel code:
                      A[blockIdx.y * Ty * Ry + i][kk + k]

  assuming normalized loop variables i and kk, and assuming Tk == Tx == Ty:
                      A[blockIdx.y * Ty * Ry + i * Ty + threadIdx.y][kk * Tk + threadIdx.x]

  setting iii := bid_y * Ty * Ry, i' := i * Ty + threadIdx.y, and kk' := kk * Tk:
                      A[iii + i'][kk' + thd_x]

  t0 := i * ty
  i'  := t0 + thd_y

  iii   = bid_y * ty_ry
  a_row = iii + i'

  a_col = kk' + thd_x, where kk' == kk * Tk


from B to B_shr, assuming B is transposed in global memory:
  in the kernel code:
                      B[blockIdx.x * Tx * Rx + j][kk + k]

  assuming normalized loop variables j and kk, and assuming Tk == Tx == Ty:
                      B[blockIdx.x * Tx * Rx + j * Tx + threadIdx.x][kk + threadIdx.y]
  setting jjj := blockIdx.x * Tx * Rx, j' := j * Tx + threadIdx.x, and kk' := kk * Ty
                      B[jjj + j'][kk' + thd_y]

  t0 := j * tx
  j'  := t0 + thd_x

  jjj   := bid_x * tx_rx
  b_row := jjj + j'
  b_col := kk' + thd_y, where kk' := kk * Tk is set earlier
-}


  {-
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
    -- Just arr_var_dims <- isInvarTo1of2InnerDims mempty space variance arrs,

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
    (bid_x, width_B) : (bid_y, height_A) : rem_outer_dims <- reverse $ unSegSpace space,

    -- sanity check that the reduce part is not missing
    not (null red_nes) = do
      -}
