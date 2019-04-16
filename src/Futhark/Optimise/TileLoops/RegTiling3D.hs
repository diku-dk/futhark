{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform a restricted form of register tiling corresponding to
--   the following pattern:
--     * a stream is perfectly nested inside a kernel with at least
--       three parallel dimension (the perfectly nested restriction
--       can be relaxed a bit);
--     * all streamed arrays are one dimensional;
--     * all streamed arrays are variant to exacly one of the three
--       innermost parallel dimensions, and conversly for each of
--       the three innermost parallel dimensions, there is at least
--       one streamed array variant to it;
--     * the stream's result is a tuple of scalar values, which are
--       also the "thread-in-space" return of the kernel.
--   Target code can be found in "tests/reg-tiling/reg-tiling-3d.fut".
module Futhark.Optimise.TileLoops.RegTiling3D
       ( doRegTiling3D )
       where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Tools
import Futhark.Transform.Substitute
import Futhark.Transform.Rename

type TileM = ReaderT (Scope Kernels) (State VNameSource)
type VarianceTable = M.Map VName Names

maxRegTile :: Int32
maxRegTile = 30

mkRegTileSe :: Int32 -> SubExp
mkRegTileSe = constant

-- | Expects a kernel statement as argument.
--   CONDITIONS for 3D tiling optimization to fire are:
--     1. a) The kernel body can be broken into
--              scalar-code-1 ++ [GroupStream stmt] ++ scalar-code-2.
--        b) The kernels has a "ThreadsReturn ThreadsInSpace" result,
--              and obviously the result is variant to the 3rd dimension
--              (counter from innermost to outermost)
--     2. For the GroupStream (morally StreamSeq):
--          a) the arrays' outersize must equal the maximal chunk size
--          b) the streamed arrays are one dimensional
--          c) each of the array arguments of GroupStream are variant
--              to exactly one of the three innermost-parallel dimension
--              of the kernel. This condition can be relaxed by interchanging
--              kernel dimensions whenever possible.
--     3. For scalar-code-1:
--          a) each of the statements is a slice that produces one of the
--             streamed arrays
--     4. For simplicity assume scalar-code-2 is empty!
--        (To be extended later.)
--   ASSUME the initial kernel is (as in tests/reg-tiling/reg-tiling-3d.fut):
--
--     kernel map(num groups: num_groups, group size: group_size,
--                num threads: num_threads, global TID -> global_tid,
--                local TID -> local_tid, group ID -> group_id)
--                (gtid_z < size_z, gtid_y < size_xy,
--                gtid_x < size_xy) : {f32} {
--        let {[size_com]f32 flags} = <empty_or_match_cert_6685>fss_6664[gtid_z,
--                                                                   0i32:+size_com*1i32]
--        let {[size_com]f32 ass} = ass_6662[gtid_y, 0i32:+size_com*1i32]
--        let {[size_com]f32 bss} = res_6687[gtid_x, 0i32:+size_com*1i32]
--        let {f32 res_ker} =
--         stream(size_com, size_com,
--                fn (int chunk_size_out, int chunk_offset_6736, f32 acc_out,
--                    [chunk_size_out]f32 flags_chunk_out,
--                    [chunk_size_out]f32 ass_chunk_out,
--                    [chunk_size_out]f32 bss_chunk_out) =>
--                  let {f32 res_out} =
--                    stream(chunk_size_out, 1i32,
--                           fn (int chunk_size_in, int i_6743, f32 acc_in,
--                               [chunk_size_in]f32 flags_chunk_in,
--                               [chunk_size_in]f32 ass_chunk_in,
--                               [chunk_size_in]f32 bss_chunk_in) =>
--                             let {f32 f} = flags_chunk_in[0i32]
--                             let {f32 a} = ass_chunk_in[0i32]
--                             let {f32 b} = bss_chunk_in[0i32]
--                             let {bool cond} = lt32(f, 9.0f32)
--                             let {f32 tmp} =
--                               if cond
--                               then {
--                                 let {f32 tmp1} = fmul32(a, b)
--                                 in {tmp1}
--                               } else {0.0f32}
--                             let {f32 res_in} = fadd32(acc_in, tmp)
--                             in {res_in},
--                           {acc_out},
--                           flags_chunk_out, ass_chunk_out, bss_chunk_out)
--                  in {res_out},
--                {0.0f32},
--                flags, ass, bss)
--        return {thread in space returns res_ker}
--     }
--
doRegTiling3D :: Stm Kernels -> TileM (Maybe (Stms Kernels, Stm Kernels))
doRegTiling3D (Let pat aux (Op (HostOp old_kernel)))
  | Kernel kerhint space kertp (KernelBody () kstms kres) <- old_kernel,
    FlatThreadSpace gspace <- spaceStructure space,
    initial_variance <- M.map mempty $ scopeOfKernelSpace space,
    variance <- varianceInStms initial_variance kstms,
    local_tid <- spaceLocalId space,
    (_,_) : (_,_) : (gidz,m_M) : _ <- reverse $ spaceDimensions space,
    (code1, Just stream_stmt, code2) <- matchCodeStreamCode kstms,
    Let pat_strm aux_strm (Op (GroupStream w w0 lam accs arrs)) <- stream_stmt,
    not (null accs),
    reg_tile <- maxRegTile `quot` fromIntegral (length accs),
    reg_tile_se <- mkRegTileSe reg_tile,
    w == w0,
    arr_chunk_params <- groupStreamArrParams lam,
    Just _ <- is3dTileable mempty space variance
                           arrs arr_chunk_params,
    Just arr_tab0 <- foldl (processIndirections $ S.fromList arrs)
                           (Just M.empty) code1,
    -- for simplicity, assume a single result, which is variant to
    -- the outer parallel dimension (for sanity sake, it should be)
    ker_res_nms <- mapMaybe retThreadInSpace kres,
    length ker_res_nms == length kres,
    Pattern [] ker_patels  <- pat,
    all primType kertp,
    all (variantToOuterDim variance gidz) ker_res_nms = do
  mm <- newVName "mm"
  mask <- newVName "mask"

  -- let mm = gidz * regTile
  let mm_stmt = mkInKerIntMulStmt mm (Var gidz) reg_tile_se
  let mask_stm= mkLet [] [Ident mask $ Prim int32] $ BasicOp $
                         BinOp (Shl Int32)
                          (Constant $ IntValue $ Int32Value 1 )
                          (Constant $ IntValue $ Int32Value 31)

  -- process the z-variant arrays that need transposition;
  -- these "manifest" statements should come before the kernel
  (arr_tab,trnsp_tab) <- foldM (insertTranspose variance gidz)
                                (M.empty, M.empty) $ M.toList arr_tab0
  let manif_stms = map(\ (a_t, (a,i,tp)) ->
                        let perm = [i+1..arrayRank tp-1] ++ [0..i]
                        in  mkLet [] [Ident a_t tp] $
                                  BasicOp $ Manifest perm a
                      ) $ M.toList trnsp_tab

  -- adjust the kernel space for 3d register tiling.
  (space_stms, space_struct, tiled_group_size, num_threads, num_groups) <-
        mkKerSpaceExtraStms reg_tile gspace
  let kspace' = space { spaceStructure  = space_struct
                      , spaceGroupSize  = tiled_group_size
                      , spaceNumThreads = num_threads
                      , spaceNumGroups  = num_groups
                      }

  -- most everything happans here!
  mb_myloop <- translateStreamsToLoop (reg_tile,mask,gidz,m_M,mm,local_tid,tiled_group_size)
                                      variance arr_tab w lam accs arrs $
                                      patternValueElements pat_strm

  -- ToDo: adjust the new kernel with
  --       1. in-place update return: for this you will need to `scratch`
  --          the result array before the kernel
  --       2. adjust the range of gidz to `(m_M + TILE_REG -1)/ TILE_REG`
  --       3. transpose the array invariant to the third-inner dim
  case mb_myloop of
    Nothing -> return Nothing
    Just (myloop, strm_res_inv, strm_res_var) -> do
      -- make loop statement
      loop_var_res <- forM strm_res_var $ \(PatElem nm attr) -> do
        clone_patel_nms <- replicateM (fromIntegral reg_tile) $ newVName $ baseString nm
        return $ map (`PatElem` attr) clone_patel_nms
      let pat_loop = Pattern [] $ strm_res_inv ++ concat loop_var_res
      let stm_loop = Let pat_loop aux_strm myloop

      -- get variant ker-results and corresponding pattern elements
      let ker_var_res_patels =
            filter (\(r,_) -> variantToOuterDim variance gidz r) $
                   zip ker_res_nms ker_patels
          (ker_var_res, ker_var_patels) = unzip ker_var_res_patels
          (code2_var, code2_inv) =
            partition (variantToOuterDim variance gidz . patElemName .
                       head . patternValueElements . stmPattern) code2

      -- make the scratch statements for kernel results variant to the z-parallel dimension
      scratch_nms_stms <- mapM mkScratchStm ker_var_patels
      let (scratch_nms, scratch_stms) = unzip scratch_nms_stms
          loop_var_nms_tr = transpose $ map (map patElemName) loop_var_res

      -- clone the statements in code2 variant to the z-parallel dimension,
      -- by encapsulating them inside if-then-else in which the then-body
      -- terminates with an in-place update corresponding to the result!
          strm_var_nms = map patElemName strm_res_var
      (ip_out_nms, unrolled_code) <-
          foldM (cloneVarCode2 mm space strm_var_nms ker_var_res_patels code2_var)
                (scratch_nms, []) $ zip [0..reg_tile-1] loop_var_nms_tr

      -- replace the `ThreadsInSpace` kernel return to an `InPlace` return
      -- for the z-variant kernel results
      let ker_res_ip_tp_tab = M.fromList $ zip ker_var_res ip_out_nms
          (kres', kertp') = unzip $
            zipWith (\r tp -> case M.lookup r ker_res_ip_tp_tab of
                                Nothing -> (ThreadsReturn ThreadsInSpace (Var r), tp)
                                Just (dims, arr, ivs) -> (WriteReturn dims arr ivs, tp)
                    ) ker_res_nms kertp

      -- finally, put everything together
          kstms' = stmsFromList $ mask_stm : mm_stmt : stm_loop : (code2_inv ++ unrolled_code)
          ker_body = KernelBody () kstms' kres'
          new_ker = Op $ HostOp $ Kernel kerhint kspace' kertp' ker_body
          extra_stms = space_stms <> stmsFromList (scratch_stms ++ manif_stms)
      return $ Just (extra_stms, Let pat aux new_ker)

  where -- | Checks that the statement is a slice that produces one of the
        --   streamed arrays. Also that the streamed array is one dimensional.
        --   Accumulates the information in a table for later use.
        processIndirections :: S.Set VName
                            -> Maybe (M.Map VName (VName, Slice SubExp, Type))
                            -> Stm InKernel
                            -> Maybe (M.Map VName (VName, Slice SubExp, Type))
        processIndirections arrs acc (Let patt _ (BasicOp (Index arr_nm slc))) =
          case (acc, patternValueElements patt) of
              (Nothing,    _) -> Nothing
              (Just tab, [p]) -> do
                  let (p_nm, p_tp) = (patElemName p, patElemType p)
                  case (S.member p_nm arrs, p_tp) of
                    (True, Array _ (Shape [_]) _) ->
                      Just $ M.insert p_nm (arr_nm,slc,p_tp) tab
                    _ -> Nothing
              (_, _) -> Nothing
        processIndirections _ _ _ = Nothing

        -- |   The second Map accumulator keeps tracks of the arrays that
        --       are variant to the z-parallel dimension and need to be transposed;
        --       the `Int` field refers to the index of the z-variant dimension, and
        --       the `Type` field refers to the type of the original global array.
        --     The first accumulator table is updated to refer to the transposed-array
        --       name, whenever such a case is discovered; otherwise it just accumulates.
        insertTranspose :: VarianceTable -> VName
                        -> (M.Map VName (VName, Slice SubExp, Type), M.Map VName (VName,Int,Type))
                        -> (VName, (VName, Slice SubExp, Type))
                        -> TileM (M.Map VName (VName, Slice SubExp, Type), M.Map VName (VName,Int,Type))
        insertTranspose variance gidz (tab, trnsp) (p_nm, (arr_nm,slc,p_tp)) =
          case findIndex (variantSliceDim variance gidz) slc of
            Nothing -> return (M.insert p_nm (arr_nm,slc,p_tp) tab, trnsp)
            Just  i -> do
              arr_tp <- lookupType arr_nm
              arr_tr_nm <- newVName $ baseString arr_nm ++ "_transp"
              let tab'   = M.insert p_nm (arr_tr_nm,slc,p_tp) tab
              let trnsp' = M.insert arr_tr_nm (arr_nm, i, arr_tp) trnsp
              return (tab', trnsp')

        variantSliceDim :: VarianceTable -> VName -> DimIndex SubExp -> Bool
        variantSliceDim variance gidz (DimFix (Var vnm)) = variantToOuterDim variance gidz vnm
        variantSliceDim _ _ _ = False

        mkInKerIntMulStmt :: VName -> SubExp -> SubExp -> Stm InKernel
        mkInKerIntMulStmt res_nm0 op1_se op2_se =
            mkLet [] [Ident res_nm0 $ Prim int32] $
              BasicOp $ BinOp (Mul Int32) op1_se op2_se

        retThreadInSpace (ThreadsReturn ThreadsInSpace (Var r)) = Just r
        retThreadInSpace _ = Nothing

doRegTiling3D _ = return Nothing

translateStreamsToLoop :: (Int32,VName,VName,SubExp,VName,VName,SubExp) ->
                          VarianceTable ->
                          M.Map VName (VName, Slice SubExp, Type) ->
                          SubExp -> GroupStreamLambda InKernel ->
                          [SubExp] -> [VName] -> [PatElem InKernel]
                       -> TileM (Maybe (Exp InKernel, [PatElem InKernel], [PatElem InKernel]))
translateStreamsToLoop (reg_tile, mask,gidz,m_M,mm,local_tid, group_size) variance
                       arr_tab w_o lam_o accs_o_p arrs_o_p strm_ress
  | -- 1. We assume the inner stream (of chunk 1) is directly nested
    --    inside the outer stream and also takes its arguments (array
    --    and accumulators) from the outer stream (all checked).
    --    Also all accumulators have primitive types (otherwise
    --    they cannot be efficiently stored in registers anyway).
    accs_o_f <- groupStreamAccParams lam_o,
    arrs_o_f <- groupStreamArrParams lam_o,
    [Let _ _ (Op (GroupStream _ ct1i32 lam_i accs_i_p arrs_i_p))] <-
        stmsToList $ bodyStms $ groupStreamLambdaBody lam_o,
    ct1i32 == (Constant $ IntValue $ Int32Value 1),
    accs_i_f <- groupStreamAccParams lam_i,
    arrs_i_f <- groupStreamArrParams lam_i,
    and $ zipWith (==) (map subExpVar accs_i_p) (map (Just . paramName) accs_o_f),
    and $ zipWith (==) arrs_i_p $ map paramName arrs_o_f,
    all (primType . paramType) accs_o_f,
    -- 2. The intent is to flatten the two streams into a loop, so
    --    we reuse the index of the inner stream for the result-loop index,
    --    and we will modify the body of the inner lambda `body_i` for the
    --    result loop.
    loop_ind_nm <- groupStreamChunkOffset lam_i,
    body_i <- groupStreamLambdaBody lam_i,
    -- 3. We transfer the slicing information (from sclar-code-1) to
    --    the array-formal arguments of the inner stream.
    arr_tab' <- foldl (\ tab (a_o_p, a_o_f, a_i_p, a_i_f) ->
                        case (paramName a_o_f == a_i_p, M.lookup a_o_p tab) of
                          (True, Just info) -> M.insert (paramName a_i_f) info tab
                          _ -> tab
                      ) arr_tab $ zip4 arrs_o_p arrs_o_f arrs_i_p arrs_i_f,
    -- 4. We translate the inner stream's accumulator to a FParam, required for
    --    mapping it as a result-loop variant variable.
    accs_i_f' <- map translParamToFParam accs_i_f,
    -- 5. We break the "loop" statements into two parts:
    --      a) the ones invariant to the z parallel dimension `invar_out_stms`,
    --      b) the ones variant   to the z parallel dimension `var_out_stms`, and
    --      c) the ones corresponding to indexing operations on variant arrays `var_ind_stms`.
    (invar_out_stms, var_ind_stms, var_out_stms) <-
      foldl (\ (acc_inv, acc_inds, acc_var) stmt ->
                let nm = patElemName $ head $ patternValueElements $ stmPattern stmt
                in  if not $ variantToOuterDim variance gidz nm
                    then (stmt : acc_inv,acc_inds,acc_var)
                    else case stmt of
                           Let _ _ (BasicOp (Index arr_nm [DimFix _])) ->
                             case M.lookup arr_nm arr_tab' of
                                Just _  -> (acc_inv,stmt:acc_inds,acc_var)
                                Nothing -> (acc_inv,acc_inds,stmt:acc_var)
                           _ -> (acc_inv,acc_inds,stmt:acc_var)
            ) ([],[],[]) $ reverse $ stmsToList $ bodyStms body_i,
    -- 6. We check that the variables used in the index statements referring to
    --    streamed arrays that are variant to the z parallel dimension (`var_ind_stms`)
    --    depend only on variables defined in the invariant stms to the z parallel dimension.
    var_nms <- concatMap (patternNames . stmPattern) var_out_stms,
    null $ S.intersection (S.fromList var_nms) $
                          S.unions (map freeInStm var_ind_stms),
    -- 7. We assume (check) for simplicity that all accumulator initializers
    --     of the outer stream are invariant to the z parallel dimension.
    loop_ini_vs <- subExpVars accs_o_p,
    all (not . variantToOuterDim variance gidz) loop_ini_vs,
    -- 8. We assume that all results of the inner-stream body are variables
    --    (for simplicity); they should have been simplified anyways if not!
    loop_res0 <- bodyResult body_i,
    loop_res  <- subExpVars loop_res0,
    length loop_res == length loop_res0 = do
  -- I. After all these conditions, we finally start by partitioning
  --    the stream's accumulators and results into the ones that are
  --    variant to the z-parallel dimension and the ones that are not.
  let (loop_var_p_i_r, loop_inv_p_i_r) =
        partition (\(_,_,r,_) -> variantToOuterDim variance gidz r) $
                  zip4 accs_i_f' accs_o_p loop_res strm_ress
  -- II. Transform the statements invariant to the z-parallel dimension
  --     so that they perform indexing in the global arrays rather than
  --     in the streamed arrays, i.e., eliminate the indirection.
  inv_stms0 <- mapM (transfInvIndStm arr_tab' loop_ind_nm) invar_out_stms
  let inv_stms = concat inv_stms0
  -- III. the index-statements variant to the z-parallel dimension are
  --      transformed to combined regions.
  m <- newVName "m"
  ind_stms0 <- foldM (transfVarIndStm arr_tab' (reg_tile,loop_ind_nm,local_tid,group_size,m,m_M))
                      (Just ([],M.empty)) $ reverse var_ind_stms
  case ind_stms0 of
    Nothing -> return Nothing
    Just (ind_stms, subst_tab) -> do
      -- IV. Add statement `let m = mm + local_tid`
      --     Then perform the substitution `gidz -> m` on the combine regions.
      let m_stmt = mkLet [] [Ident m $ Prim int32] $
                BasicOp $ BinOp (Add Int32) (Var mm) (Var local_tid)
          tab_z_m_comb = M.insert gidz m M.empty
          ind_stms' = m_stmt : map (substituteNames tab_z_m_comb) ind_stms

      -- V. We clone the variant statements regTile times and enclose
      --    each one in a if-then-else testing whether `mm + local_id < m_M`
      --    TODO: check that the statements do not involve In-Place updates!
      let loop_var_p_i_r' = map (\(x,y,z,_)->(x,y,z)) loop_var_p_i_r
      if_ress <- mapM (cloneVarStms subst_tab (mask,loop_ind_nm,mm,m_M,gidz)
                                     loop_var_p_i_r' var_out_stms) [0..reg_tile-1]
      -- VI. build the loop-variant vars/res/inis
      let (if_stmt_clones0, var_ress_pars) = unzip if_ress
          if_stmt_clones = concat if_stmt_clones0
          (_, var_ini, _, strm_var_res) = unzip4 loop_var_p_i_r
          var_inis = concat $ replicate (fromIntegral reg_tile) var_ini
          (var_ress, var_pars) = unzip $ concat var_ress_pars
          (inv_pars, inv_inis, inv_ress, strm_inv_res) = unzip4 loop_inv_p_i_r
          loop_form_acc = inv_pars ++ var_pars
          loop_inis_acc = inv_inis ++ var_inis
          loop_ress     = inv_ress ++ var_ress
      -- VII. Finally build the loop body and return it!
      --      Insert an extra barrier at the begining of the loop; make
      --        it dependent on the loop index so it cannot be hoisted!
      ind_bar <- newVName "loop_ind"
      let bar_stmt = mkLet [] [Ident loop_ind_nm $ Prim int32] $ Op (Barrier [Var ind_bar])
          stms_body_i' = bar_stmt : inv_stms ++ ind_stms' ++ if_stmt_clones
          form = ForLoop ind_bar Int32 w_o []
          body_i' = Body (bodyAttr body_i)
                         (stmsFromList stms_body_i') $
                         map Var loop_ress
          myloop = DoLoop [] (zip loop_form_acc loop_inis_acc) form body_i'
          free_in_body = freeInBody body_i'
          elim_vars = S.fromList $ arrs_i_p ++ arrs_o_p ++
                                   map paramName arrs_i_f ++
                                   map paramName accs_o_f
      if null $ S.intersection free_in_body elim_vars
      then return $ Just (myloop, strm_inv_res, strm_var_res)
      else return Nothing
translateStreamsToLoop _ _ _ _ _ _ _ _ = return Nothing

-- | Clone the variant statements, by creating a new if-then-else
--   big statement that cheks that `mm + i < m_M` for `i = 0...regTile-1`
--   Return the if-then-else statement together with the result variables
--   so that the body of the loop and the loop results and paramters can
--   be constructed.
--   In order to disallow hoisting from the loop we will generate:
--   let zero = mask & loop_ind
--   let mmpi = zero + mm + i
cloneVarStms :: M.Map VName (VName,Type) -> (VName, VName, VName, SubExp, VName)
              -> [(FParam InKernel, SubExp, VName)] -> [Stm InKernel]
              -> Int32 -> TileM ([Stm InKernel], [(VName,FParam InKernel)])
cloneVarStms subst_tab (mask,loop_ind,mm,m_M,gidz) loop_info var_out_stms i = do
  let (loop_par_origs, loop_inis, body_res_origs) = unzip3 loop_info
  body_res_clones <- mapM (\x -> newVName $ baseString x ++ "_clone") body_res_origs
  loop_par_nm_clones <- mapM (\x -> newVName $ baseString (paramName x) ++ "_clone") loop_par_origs
  m <- newVName "m"
  z <- newVName "zero"
  ii<- newVName "unroll_ct"
  let loop_par_clones = zipWith (\ p nm -> p { paramName = nm })
                                loop_par_origs loop_par_nm_clones
      res_types = map paramType loop_par_origs
      i_se = Constant $ IntValue $ Int32Value i

      stmt_zero = mkLet [] [Ident z  $ Prim int32] $
                        BasicOp $ BinOp (And Int32) (Var mask) (Var loop_ind)
      stmt_ii   = mkLet [] [Ident ii $ Prim int32] $
                        BasicOp $ BinOp (Add Int32) (Var z) i_se
      m_stmt_other =
        mkLet [] [Ident m $ Prim int32] $
              BasicOp $ BinOp (Add Int32) (Var mm) (Var ii)
      read_sh_stms =
        map (\ (scal,(sh_arr, el_tp)) ->
                  mkLet [] [Ident scal el_tp] $
                        BasicOp $ Index sh_arr [DimFix i_se]
            ) $ M.toList subst_tab
      tab_z_m_other = foldl (\tab (old,new) -> M.insert (paramName old) new tab)
                            (M.insert gidz m M.empty) $
                            zip loop_par_origs loop_par_nm_clones
      var_out_stms' = map (substituteNames tab_z_m_other) $
                           read_sh_stms ++ var_out_stms
  cond_nm <- newVName "out3_inbounds"
  -- if the statements are simple, i.e., "safe", then do not
  -- encapsulate them in an if-then-else; this will result in
  -- significant performance gains.
  let simple = all simpleStm var_out_stms
  let cond_stm  = if simple
                  then mkLet [] [Ident cond_nm $ Prim Bool] $
                          BasicOp $ SubExp (Constant $ BoolValue True)
                  else mkCondStmt m_M m cond_nm
      -- TODO: we need to uniquely rename the then/else bodies!
  then_body <- renameBody $ Body () (stmsFromList var_out_stms') (map Var body_res_origs)
  let else_body = Body () mempty loop_inis
      if_stmt = mkLet [] (zipWith Ident body_res_clones res_types) $
                  If (Var cond_nm) then_body else_body $
                     IfAttr (staticShapes res_types) IfFallback
  -- we will substitute later the original loop formal-param names
  -- with the newly created ones in the body
  return ( [stmt_zero, stmt_ii, m_stmt_other, cond_stm, if_stmt]
         , zip body_res_clones loop_par_clones )

mkCondStmt :: SubExp -> VName -> VName -> Stm InKernel
mkCondStmt m_M m cond_nm =
  mkLet [] [Ident cond_nm $ Prim Bool] $
        BasicOp $ CmpOp (CmpSlt Int32) (Var m) m_M

simpleStm :: Stm InKernel -> Bool
simpleStm (Let _ _ e) = safeExp e

mkScratchStm :: PatElem Kernels -> TileM (([SubExp], VName, [([SubExp], SubExp)]),
                                          Stm Kernels)
mkScratchStm ker_patel = do
  let (unique_arr_tp, res_arr_nm0) = (patElemType ker_patel, patElemName ker_patel)
      ptp = elemType unique_arr_tp
  scrtch_arr_nm <- newVName $ baseString res_arr_nm0 ++ "_0"
  let scratch_stm = mkLet [] [Ident scrtch_arr_nm unique_arr_tp] $
                          BasicOp $ Scratch ptp $ arrayDims unique_arr_tp
  return ((arrayDims unique_arr_tp, scrtch_arr_nm, []), scratch_stm)

-- | Arguments are:
--     1. @mm@ this is the length of z-parallel dimension divided by reg_tile
--     2. @space@: the kernel space
--     3. @strm_res_nms@: the z-variant results of the original stream
--     4. @keres_patels@: the kernel result names tupled with the corresponding
--                        pattern elements of the kernel statement.
--     5. @code2_var@: the z-variant statements of the code after the stream.
--     6. @ip_writes@: the "current" argument to a 'WriteReturn'.
--        @unroll_code@: the current unrolled code. Both form a `foldM` accumulator.
--     7. @k@ the "current" clone number;
--        @loop_res_nms@ the names of the loop result corresponding to the current clone.
--   Result:
--     1. the argument for the current in-place update result,
--     2. a new if-statement is added to the unrolled-code accumulator which actually
--        perform the in-place update.
cloneVarCode2 :: VName -> KernelSpace -> [VName]
              -> [(VName, PatElem InKernel)] -> [Stm InKernel]
              -> ([([SubExp], VName, [([SubExp], SubExp)])],
                  [Stm InKernel])
              -> (Int32, [VName])
              -> TileM ([([SubExp], VName, [([SubExp], SubExp)])],
                        [Stm InKernel])
cloneVarCode2 mm space strm_res_nms keres_patels code2_var
              (writes, unroll_code) (k, loop_res_nms) = do
  let (ker_nms, pat_els) = unzip keres_patels
      root_strs = map (baseString . patElemName) pat_els
  ip_out_nms <- mapM (\s -> newVName $ s ++ "_out_" ++ pretty (k+1)) root_strs
  m <- newVName "m"
  -- make in-place update statements
  let (gidx,_) : (gidy,_) : (gidz,m_M) : rev_outer_dims = reverse $ spaceDimensions space
      (outer_dims, _) = unzip $ reverse rev_outer_dims
      strip_dims = length $ outer_dims++[m,gidy,gidx]
      ts = map (stripArray strip_dims . patElemType) pat_els
  -- make if
  cond_nm <- newVName "m_cond"
  let i_se = Constant $ IntValue $ Int32Value k
      m_stm = mkLet [] [Ident m $ Prim int32] $
                    BasicOp $ BinOp (Add Int32) (Var mm) i_se
      c_stm = mkCondStmt m_M m cond_nm
      strm_loop_tab = M.fromList $ (gidz, m) : zip strm_res_nms loop_res_nms
  then_body <- renameBody $ substituteNames strm_loop_tab $
               Body () (stmsFromList code2_var) $ map Var ker_nms
  let else_body = Body () mempty $ map blank ts
      if_stm = mkLet [] (zipWith Ident ip_out_nms ts) $
                     If (Var cond_nm) then_body else_body  $
                     IfAttr (staticShapes ts) IfFallback
      addWritePair (dims, arr, current) ker_nm =
        (dims, arr, current ++ [(map Var $ outer_dims++[m,gidy,gidx], Var ker_nm)])
  return (zipWith addWritePair writes ip_out_nms, unroll_code ++ [m_stm, c_stm, if_stm])
  where blank (Prim t) = Constant $ blankPrimValue t
        blank t = error $ "cloneVarCode2: cannot tile non-prim type " ++ pretty t

helper3Stms :: VName -> SubExp -> SubExp -> Slice SubExp
             -> VName -> Stm InKernel -> TileM [Stm InKernel]
helper3Stms loop_ind strd beg par_slc par_arr (Let ptt att _) = do
  tmp1 <- newVName "tmp"
  tmp2 <- newVName "ind"
  let stmt1 = mkLet [] [Ident tmp1 $ Prim int32] $
                BasicOp $ BinOp (Mul Int32) (Var loop_ind) strd
      stmt2 = mkLet [] [Ident tmp2 $ Prim int32] $
                BasicOp $ BinOp (Add Int32) beg (Var tmp1)
      ndims = length par_slc
      ind_exp = BasicOp (Index par_arr (take (ndims-1) par_slc ++ [DimFix $ Var tmp2]))
      stmt3 = Let ptt att ind_exp
  return [stmt1,stmt2,stmt3]

-- | Insert the necessary translations for a statement that is indexing
--   in one of the streamed arrays, which is invariant to the z-parallel
--   dimension. The index is necessarily `0` at this point, and we use `tab`
--   to figure out to what global array does the streamed array actually
--   refers to, and to compute the global index.
transfInvIndStm :: M.Map VName (VName, Slice SubExp, Type)
                -> VName -> Stm InKernel
                -> TileM [Stm InKernel]
transfInvIndStm tab loop_ind stm@(Let _ _ (BasicOp (Index arr_nm [DimFix _])))
  | Just (par_arr, par_slc@(_:_), _) <- M.lookup arr_nm tab,
    DimSlice beg _ strd <- last par_slc =
  helper3Stms loop_ind strd beg par_slc par_arr stm
transfInvIndStm _ _ stm = return [stm]

-- | Insert the necessary translations for a statement that is indexing
--   inside one of the streamed arrays, which is variant to the outermost
--   parallel dimension.
transfVarIndStm :: M.Map VName (VName, Slice SubExp, Type)
                -> (Int32,VName,VName,SubExp,VName,SubExp)
                -> Maybe ([Stm InKernel],M.Map VName (VName,Type))
                -> Stm InKernel
                -> TileM (Maybe ([Stm InKernel],M.Map VName (VName,Type)))
transfVarIndStm tab (reg_tile,loop_ind,local_tid,group_size,m,m_M) acc
                    stm@(Let ptt _ (BasicOp (Index arr_nm [DimFix _])))
  | Just (tstms,stab) <- acc,
    Just (par_arr, par_slc@(_:_), _) <- M.lookup arr_nm tab,
    DimSlice beg _ strd <- last par_slc,
    [pat_el] <- patternValueElements ptt,
    el_tp <- patElemType pat_el,
    pat_el_nm <- patElemName pat_el,
    Prim _ <- el_tp = do
  -- compute the index into the global array
  stms3 <- helper3Stms loop_ind strd beg par_slc par_arr stm
  let glb_ind_stms = stmsFromList stms3
  -- set up the combine part
  sh_arr_1d <- newVName $ baseString par_arr ++ "_sh_1d"
  cid <- newVName "cid"
  let block_cspace = combineSpace [(cid,group_size)]
      comb_exp = Op $ Combine block_cspace [el_tp]
                    [(local_tid, mkRegTileSe reg_tile), (m,m_M)] $
                    Body () glb_ind_stms [Var pat_el_nm]
      sh_arr_pe = PatElem sh_arr_1d $
                    arrayOfShape el_tp $ Shape [group_size]
      write_sh_arr_stmt =
         Let (Pattern [] [sh_arr_pe]) (defAux ()) comb_exp
  return $ Just (write_sh_arr_stmt:tstms, M.insert pat_el_nm (sh_arr_1d,el_tp) stab)
transfVarIndStm _ _ _ _ = return Nothing

--------------
--- HELPES ---
--------------

-- | translates an LParam to an FParam
translParamToFParam :: LParam InKernel -> FParam InKernel
translParamToFParam = fmap (`toDecl` Nonunique)

-- | Tries to identified the following pattern:
--   code folowed by a group stream followed by
--   another code.
matchCodeStreamCode :: Stms InKernel ->
                       ([Stm InKernel], Maybe (Stm InKernel), [Stm InKernel])
matchCodeStreamCode kstms =
  foldl (\acc stmt ->
            case (acc,stmt) of
                ( (cd1,Nothing,cd2), Let _ _ (Op GroupStream{})) ->
                    (cd1, Just stmt, cd2)
                ( (cd1, Nothing, cd2), _) -> (cd1++[stmt], Nothing, cd2)
                ( (cd1,Just strm,cd2), _) -> (cd1,Just strm,cd2++[stmt])
        ) ([],Nothing,[]) (stmsToList kstms)

-- | Checks that all streamed arrays are variant to exacly one of
--   the three innermost parallel dimensions, and conversly for
--   each of the three innermost parallel dimensions, there is at
--   least one streamed array variant to it. The result is the
--   the number of the only variant parallel dimension for each array.
is3dTileable :: Names -> KernelSpace -> VarianceTable -> [VName]
             -> [LParam InKernel] -> Maybe [Int]
is3dTileable branch_variant kspace variance arrs block_params =
  let ok1 = all (primType . rowType . paramType) block_params
      inner_perm0 = map variantOnlyToOneOfThreeInnerDims arrs
      inner_perm = catMaybes inner_perm0
      ok2 = elem 0 inner_perm && elem 1 inner_perm && elem 2 inner_perm
      ok3 = length inner_perm0 == length inner_perm
      ok = ok1 && ok2 && ok3
  in if ok then Just inner_perm else Nothing
  where variantOnlyToOneOfThreeInnerDims :: VName -> Maybe Int
        variantOnlyToOneOfThreeInnerDims arr = do
          (k,_) : (j,_) : (i,_) : _ <- Just $ reverse $ spaceDimensions kspace
          let variant_to = M.findWithDefault mempty arr variance
              branch_invariant = not $  S.member k branch_variant ||
                                        S.member j branch_variant ||
                                        S.member i branch_variant
          if not branch_invariant
          then Nothing
          else if      i `S.member` variant_to && not (j `S.member` variant_to) && not (k `S.member` variant_to) then Just 0
               else if not (i `S.member` variant_to) && j `S.member` variant_to && not (k `S.member` variant_to) then Just 1
               else if not (i `S.member` variant_to) && not (j `S.member` variant_to) && k `S.member` variant_to then Just 2
               else Nothing

mkKerSpaceExtraStms :: Int32 -> [(VName, SubExp)]
                    -> TileM (Stms Kernels, SpaceStructure, SubExp, SubExp, SubExp)
mkKerSpaceExtraStms reg_tile gspace = do
  dim_z_nm <- newVName "gidz_range"
  tmp <- newVName "tmp"
  let tmp_stm = mkLet [] [Ident tmp $ Prim int32] $
                      BasicOp $ BinOp (Add Int32) m_M $
                      Constant $ IntValue $ Int32Value (reg_tile-1)
      rgz_stm = mkLet [] [Ident dim_z_nm $ Prim int32] $
                      BasicOp $ BinOp (SQuot Int32) (Var tmp) $
                      Constant $ IntValue $ Int32Value reg_tile
      (gidx,sz_x) : (gidy,sz_y) : (gidz,m_M) : untiled_gspace = reverse gspace

  ((tile_size_x, tile_size_y, tiled_group_size), tile_size_bnds) <- runBinder $ do
      tile_size_key <- nameFromString . pretty <$> newVName "tile_size"
      tile_ct_size  <- letSubExp "tile_size" $ Op $ GetSize tile_size_key SizeTile
      tile_size_x   <- letSubExp "tile_size_x" $ BasicOp $
                                 BinOp (SMin Int32) tile_ct_size sz_x
      tile_size_y   <- letSubExp "tile_size_y" $ BasicOp $
                                 BinOp (SMin Int32) tile_ct_size sz_y
      tiled_group_size <- letSubExp "tiled_group_size" $
                                 BasicOp $ BinOp (Mul Int32) tile_size_x tile_size_y
      return (tile_size_x, tile_size_y, tiled_group_size)
      -- Play with reversion to ensure we get increasing IDs for
      -- ltids.  This affects readability of generated code.
  untiled_gspace' <- fmap reverse $ forM (reverse untiled_gspace) $ \(gtid,gdim) -> do
      ltid <- newVName "ltid"
      return (gtid, gdim, ltid, constant (1::Int32))
  ltidz <- newVName "ltid"
  let dim_z = (gidz, Var dim_z_nm, ltidz, constant (1::Int32))
  ltidy <- newVName "ltid"
  let dim_y = (gidy, sz_y, ltidy, tile_size_y)
  ltidx <- newVName "ltid"
  let dim_x = (gidx, sz_x, ltidx, tile_size_x)
      gspace' = reverse $ dim_x : dim_y : dim_z : untiled_gspace'
  -- We have to recalculate number of workgroups and
  -- number of threads to fit the new workgroup size.
  ((num_threads, num_groups), num_bnds) <-
        runBinder $ sufficientGroups gspace' tiled_group_size

  let extra_stms = oneStm tmp_stm <> oneStm rgz_stm <> tile_size_bnds <> num_bnds
  return ( extra_stms, NestedThreadSpace gspace'
         , tiled_group_size, num_threads, num_groups )


variantToOuterDim :: VarianceTable -> VName -> VName -> Bool
variantToOuterDim variance gid_outer nm =
  gid_outer == nm || gid_outer `S.member` M.findWithDefault mempty nm variance

varianceInStms :: VarianceTable -> Stms InKernel -> VarianceTable
varianceInStms = foldl varianceInStm

varianceInStm :: VarianceTable -> Stm InKernel -> VarianceTable
varianceInStm v0 bnd@(Let _ _ (Op (GroupStream _ _ lam accs arrs))) =
  let v = defVarianceInStm v0 bnd
      acc_lam_f = groupStreamAccParams lam
      arr_lam_f = groupStreamArrParams lam
      bdy_lam   = groupStreamLambdaBody lam
      stm_lam   = bodyStms   bdy_lam

      v' = foldl' (\vacc (v_a, v_f) ->
                    let vrc = S.insert v_a $ M.findWithDefault mempty v_a vacc
                    in  M.insert v_f vrc vacc
                  ) v $ zip arrs $ map paramName arr_lam_f
      v''= foldl' (\vacc (v_se, v_f) ->
                    case v_se of
                      Var v_a ->
                        let vrc = S.insert v_a $ M.findWithDefault mempty v_a vacc
                        in  M.insert v_f vrc vacc
                      Constant _ -> vacc
                  ) v' $ zip accs $ map paramName acc_lam_f
  in varianceInStms v'' stm_lam
varianceInStm variance bnd = defVarianceInStm variance bnd

defVarianceInStm :: VarianceTable -> Stm InKernel -> VarianceTable
defVarianceInStm variance bnd =
  foldl' add variance $ patternNames $ stmPattern bnd
  where add variance' v = M.insert v binding_variance variance'
        look variance' v = S.insert v $ M.findWithDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ S.toList (freeInStm bnd)

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
