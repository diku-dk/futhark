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

type TileM = ReaderT (Scope Kernels) (State VNameSource)
type VarianceTable = M.Map VName Names

mmmTiling2D :: Stm Kernels -> TileM (Maybe (Stms Kernels, Stm Kernels))
mmmTiling2D stm@(Let pat aux (Op (SegOp (SegMap lvl@SegThread{} space ts old_ker_body))))
  | KernelBody () kstms kres <- old_ker_body,

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
    (gidx, m_X) : (gidy, m_Y) : _ <- reverse $ unSegSpace space,


    -- sanity check that the reduce part is not missing (it shouldn't be)
    not (null red_nes) =
      let let_binding = Let (Pattern {patternContextElements = [],
                                      patternValueElements =
                                      [PatElem {patElemName = (VName (nameFromString "foo") 7),
                                                patElemAttr = Prim (IntType Int32)}]})
                            aux (BasicOp (SubExp (Constant (IntValue (Int32Value 3)))))
 
      in trace ("pat:\n"                              ++ pretty pat ++
           "\n\n-----\n\nspace:\n"                    ++ pretty space ++
           "\n\n-----\n\nold_ker_body:\n"             ++ pretty old_ker_body ++
           "\n\n-----\n\ncode2':\n"                   ++ pretty code2' ++
           "\n\n-----\n\nVariant arrays:\n"           ++ pretty arr_var_dims ++
           "\n\n-----\n\nVariance table:\n"           ++ pretty variance ++
           "\n\n-----\n\nreduce result variance:\n"   ++ pretty res_red_var ++
           "\n\n-----\n\nindirect-slice table:\n"     ++ pretty arr_tab0 ++
           "\n\n-----\n\n(gidx, m_X) : (gidy, m_Y)\n" ++ pretty (reverse $ unSegSpace space) ++
           "\n\n\nstm:\n" ++ pretty stm)

           $ return (Just (oneStm let_binding, stm))

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

        processIndirections _ res_red_var acc stm@(Let patt _ _)
          | Just (ss, tab) <- acc,
            ps <- patternValueElements patt,
            all (\p -> not (nameIn (patElemName p) res_red_var)) ps =
              Just (ss Seq.|> stm, tab)
          | otherwise = Nothing

mmmTiling2D _ = return Nothing

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

sufficientGroups :: MonadBinder m =>
                    [(VName, SubExp, VName, SubExp)] -> SubExp
                 -> m (SubExp, SubExp)
sufficientGroups gspace group_size = do
  groups_in_dims <- forM gspace $ \(_, gd, _, ld) ->
    letSubExp "groups_in_dim" =<< eDivRoundingUp Int32 (eSubExp gd) (eSubExp ld)
  num_groups <- letSubExp "num_groups" =<<
                foldBinOp (Mul Int32) (constant (1::Int32)) groups_in_dims
  num_threads <- letSubExp "num_threads" $
                 BasicOp $ BinOp (Mul Int32) num_groups group_size
  return (num_threads, num_groups)
