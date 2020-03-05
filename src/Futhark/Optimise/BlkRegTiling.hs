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
    Just (w, arrs, (red_comm, red_lam, red_nes, map_lam)) <- isTileableRedomap screma_stmt,

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
    (gidx, width_B) : (gidy, height_A) : rem_outer_dims <- reverse $ unSegSpace space,

    -- sanity check that the reduce part is not missing

    not (null red_nes) = do
      (stm', stms) <- runBinder $ do

        -- ty_name  <- nameFromString . pretty <$> newVName "Ty"
        -- tx_name  <- nameFromString . pretty <$> newVName "Tx"
        -- ry_name  <- nameFromString . pretty <$> newVName "Ry"
        -- rx_name  <- nameFromString . pretty <$> newVName "Rx"
        gid_x    <- newVName "gid_x"
        gid_y    <- newVName "gid_y"
        gid_flat <- newVName "gid_flat"

        ty <- letSubExp "Ty" $ BasicOp $ SubExp $ intConst Int32 16 -- Op $ SizeOp $ GetSize ty_name SizeTile
        tx <- letSubExp "Tx" $ BasicOp $ SubExp $ intConst Int32 8  -- Op $ SizeOp $ GetSize tx_name SizeTile
        ry <- letSubExp "Ry" $ BasicOp $ SubExp $ intConst Int32 4  -- Op $ SizeOp $ GetSize ty_name SizeTile
        rx <- letSubExp "Rx" $ BasicOp $ SubExp $ intConst Int32 4  -- Op $ SizeOp $ GetSize tx_name SizeTile

        tx_rx <- letSubExp "tx_rx" $ BasicOp $ BinOp (Mul Int32) tx rx -- tx rx 
        ty_ry <- letSubExp "ty_ry" $ BasicOp $ BinOp (Mul Int32) ty ry -- ty ry 

        group_size <- letSubExp "group_size" $ BasicOp $ BinOp (Mul Int32) ty tx

        grid_dimy <- letSubExp "grid_dimy" =<< eDivRoundingUp Int32 (eSubExp height_A) (eSubExp ty_ry)
        grid_dimx <- letSubExp "grid_dimx" =<< eDivRoundingUp Int32 (eSubExp width_B)  (eSubExp tx_rx)

        num_groups_top <- letSubExp "num_groups_top" $ BasicOp $ BinOp (Mul Int32) grid_dimx grid_dimy

        --
        -- create seggroup with segthreadprivate inside
        --
        let lvl'   = SegGroup (Count num_groups_top) (Count group_size) SegNoVirt
        let space' = SegSpace gid_flat [(gid_x, grid_dimx), (gid_y, grid_dimy)]

        tmp_zero <- letSubExp "tmp_zero" $ BasicOp $ SubExp $ intConst Int32 0
        tmp_rep  <- letSubExp "tmp_rep"  $ BasicOp $ Replicate (Shape [rx]) tmp_zero
        zeroes   <- letSubExp "zeroes" $ BasicOp (Replicate (Shape [ry]) tmp_rep)


        -- for now, just fill in the original kernel body

        result <- letSubExp "zeroes" $ Op $ SegOp $ SegMap lvl' space' ts $ old_kbody

        return $ Let pat aux $ Op $ SegOp $ SegMap lvl' space ts old_kbody

      trace (
             -- ">> pat:\n"                      ++ _pretty pat          ++
             -- ">> space:\n"                    ++ _pretty space        ++
             -- ">> old_kbody:\n"                ++ _pretty old_kbody ++
             -- ">> code2':\n"                   ++ _pretty code2'       ++
             -- ">> Variant arrays:\n"           ++ _pretty arr_var_dims ++
             -- ">> Variance table:\n"           ++ _pretty variance     ++
             -- ">> reduce result variance:\n"   ++ _pretty res_red_var  ++
             -- ">> indirect-slice table:\n"     ++ _pretty arr_tab0     ++
             -- ">> (gidx, m_X) : (gidy, m_Y)\n" ++ _pretty [(gidx, width_B), (gidy, height_A)] ++
             ">> stms:\n"                     ++ _pretty stms ++
             ">> stm':\n"                     ++ _pretty stm')
             $ return $ Just (stms, stm')

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
              Just (ss, M.insert p_nm (arr_nm,slc,p_tp) tab)

        processIndirections _ res_red_var acc stm'@(Let patt _ _)
          | Just (ss, tab) <- acc,
            ps <- patternValueElements patt,
            all (\p -> not (nameIn (patElemName p) res_red_var)) ps =
              Just (ss Seq.|> stm', tab)
          | otherwise = Nothing

mmmTiling2D _ = trace "nej" $ return Nothing

---------------
--- HELPERS ---
---------------

-- | Translates an LParam to an FParam
translParamToFParam :: LParam Kernels -> FParam Kernels
translParamToFParam = fmap (`toDecl` Nonunique)

-- | Tries to identify the following pattern:
--   code followed by some Screma followed by more code.
matchCodeStreamCode :: Stms Kernels ->
                       ([Stm Kernels], Maybe (Stm Kernels), [Stm Kernels])
matchCodeStreamCode kstms =
  foldl (\acc stmt ->
            case (acc, stmt) of
              ((cd1, Nothing, cd2), Let _ _ (Op (OtherOp (Screma _ _ _)))) ->
               (cd1, Just stmt, cd2)

              ((cd1, Nothing, cd2), _) ->
               (cd1 ++ [stmt], Nothing, cd2)

              ((cd1, Just strm, cd2), _) ->
               (cd1, Just strm, cd2++[stmt])
        ) ([], Nothing, []) (stmsToList kstms)

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

variantToOuterDim :: VarianceTable -> VName -> VName -> Bool
variantToOuterDim variance gid_outer nm =
  gid_outer == nm || (nameIn gid_outer $ M.findWithDefault mempty nm variance)

varianceInStms :: VarianceTable -> Stms Kernels -> VarianceTable
varianceInStms = foldl varianceInStm

-- just in case you need the Screma being treated differently than
-- by default; previously Cosmin had to enhance it when dealing with stream.
varianceInStm :: VarianceTable -> Stm Kernels -> VarianceTable
varianceInStm v0 bnd@(Let pat _ (Op (OtherOp (Screma _ _ _))))
  | Just (w, arrs, (red_comm, red_lam, red_nes, map_lam)) <- isTileableRedomap bnd =
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

-- sufficientGroups :: [(VName, SubExp, VName, SubExp)] -- gspace
sufficientGroups :: MonadBinder m =>
                    [(VName, SubExp, VName, SubExp)] -- gspace
                 -> SubExp                           -- group_size
                 -> m (SubExp, SubExp)               -- (x, y) grid dimensions?
sufficientGroups gspace group_size = do

  groups_in_dims <- forM gspace $ \(_, gd, _, ld) ->
                      letSubExp "groups_in_dim" =<<
                      eDivRoundingUp Int32 (eSubExp gd) (eSubExp ld)

  num_groups <- letSubExp "num_groups" =<<
                foldBinOp (Mul Int32) (constant (1::Int32)) groups_in_dims

  num_threads <- letSubExp "num_threads" (BasicOp (BinOp (Mul Int32) num_groups group_size))

  traceM ("num_threads:\n" ++ _pretty num_threads ++
          "num_groups:\n"  ++ _pretty num_groups)

  return (num_threads, num_groups)




    -- not (null red_nes) = do

      -- ty_name <- nameFromString . pretty <$> newVName "Ty"
      -- ty_name <- newVName "Ty"
      -- tx_name <- newVName "Tx"
      -- ry_name <- newVName "Ry"
      -- rx_name <- newVName "Rx"
      -- ty_ry_name <- newVName "TyRy"
      -- tx_rx_name <- newVName "TxRx"
      -- group_size_name <- newVName "group_size"
      --
      -- let ty = mkLet [] [Ident ty_name $ Prim $ IntType Int32] $ BasicOp $ SubExp $ constant (16 :: Int32)
      -- let tx = mkLet [] [Ident tx_name $ Prim $ IntType Int32] $ BasicOp $ SubExp $ constant (16 :: Int32)
      -- let ry = mkLet [] [Ident ry_name $ Prim $ IntType Int32] $ BasicOp $ SubExp $ constant (4  :: Int32)
      -- let rx = mkLet [] [Ident rx_name $ Prim $ IntType Int32] $ BasicOp $ SubExp $ constant (4  :: Int32)
      --
      -- let group_size = mkLet [] [Ident ty_name $ Prim $ IntType Int32] -- ty*tx
      -- let ty_ry      = mkLet [] [Ident tx_name $ Prim $ IntType Int32] -- ty*ry
      -- let tx_rx      = mkLet [] [Ident ry_name $ Prim $ IntType Int32] -- tx*rx
      --
      -- let statements = [ty, tx, ry, rx]
      -- -- SegGroup
      -- trace (pretty $ stmsFromList statements) $ return $ Just (stmsFromList statements, stm)

          -- group_size = mkLet [] [Ident group_size_name $ Prim Int32]
          --                (primExpFromSubExp (Prim Int32) ty * primExpFromSubExp (Prim Int32) tx)
          --
          -- tx_rx      = mkLet [] [Ident tx_rx_name $ Prim Int32]
          --                (primExpFromSubExp (Prim Int32) tx * primExpFromSubExp (Prim Int32) rx)
          --
          -- tx = mkLet [] [Ident tx_name $ Prim $ IntType Int32] IntValue $ Int32Value 8
          -- ry = mkLet [] [Ident ry_name $ Prim $ IntType Int32] IntValue $ Int32Value 4
          -- rx = mkLet [] [Ident rx_name $ Prim $ IntType Int32] IntValue $ Int32Value 4
          -- ty_ry      = mkLet [] [Ident ty_ry_name $ Prim Int32]
          --                (primExpFromSubExp (Prim Int32) ty * primExpFromSubExp (Prim Int32) ry)



      -- let a = constant (3 :: Int32)
          -- b = constant (4 :: Int32)
          -- x = (primExpFromSubExp (IntType Int32) a * primExpFromSubExp (IntType Int32) b)
