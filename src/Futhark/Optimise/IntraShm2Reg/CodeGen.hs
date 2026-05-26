-- | This module performs the Code Generation for
--     the IntraShm2Reg transformations, which attempts
--     to allocate intermediate arrays produced in
--     intra-group kernels in Register memory
--     (rather than shared memory).
module Futhark.Optimise.IntraShm2Reg.CodeGen
  ( updateStm, genKerReg2Shm, genKerShm2Reg, fixRegKerResults )
  where

import Data.Maybe
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Futhark.IR.GPU
import Futhark.Tools
import Futhark.Optimise.IntraShm2Reg.SymTabs

i64ptp :: PrimType
i64ptp = IntType Int64

-- | Arguments:
--     @bu_env@ the bottom-up environment that keeps
--              track of the register-mapping safety
--     @nm@ a program variable name
--   Returns a boolean denoting whether the input variable
--           can be safely mapped to register memory.
--   The safety of register mapping comes down to verifying
--     that said variable is either not used in any of the
--     remaining kernel statements or it is used in a
--     compatible (@Compat@) way with the register mapping.
regMapSucceeds :: BotEnv -> VName -> Bool
regMapSucceeds bu_env nm
  | Just etry <- M.lookup nm $ regArrays bu_env,
    kind <- foldl unifyAccK None $ map snd (bindings etry) =
    kind == None || kind == Compat
regMapSucceeds _ _ = False

-- | Arguments:
--    @(td_env, bu_env)@ the top-down and bottom-up environments
--    @stm@ a program statement
--   The result is a sequence of statements:
--    If the input statement was verified to be safe to keep some
--      of its results to register (rather than shared) memory,
--      then the resulted statements perform the neccessary
--      adjustments, e.g., a @segmap@ kernel would adjust the
--      corresponding results to be thread-private, while a
--      @manifest@ would generate a @segmap@ that translates
--      the input from shared to register memory (by having
--      thread-private results).
--    Otherwise, it conservatively returns the input statement.
--
updateStm :: Env -> Stm GPU -> Shm2RegM (Stms GPU)
updateStm (_td_env, bu_env) (Let (Pat patels) aux e)
  -- the case of an Inner SegMap:
  | Op (SegOp sgmap) <- e,
    SegMap (SegThreadInBlock vrt) inner_space ts kbody <- sgmap,
    bdyres <- bodyResult kbody,
    length patels == length bdyres = do
  let bdyres'= map fff $ zip3 patels bdyres ts
      kbody' = kbody { bodyResult = bdyres' }
      e' = Op $ SegOp $ SegMap (SegThreadInBlock vrt) inner_space ts kbody'
      aux' = aux { stmAuxAttrs = removeAttr2RegMem (stmAuxAttrs aux) }
  pure $ Sq.singleton $ Let (Pat patels) aux' e'
  where
    fff (_, res, Acc{}) = res
    fff (pel, Returns ResultMaySimplify certs se, _)
      | regMapSucceeds bu_env (patElemName pel) =
      Returns ResultPrivate certs se
    fff (_, res, _) = res
--
-- a successful opaque is translated to a copy stmt:
updateStm (_td_env, bu_env) (Let (Pat [patel]) aux e)
  | BasicOp (Opaque OpaqueNil (Var orig_nm)) <- e,
    regMapSucceeds bu_env orig_nm =
  -- ^ the access kinds were merged into @orig_nm@ by @onBotUpStm@,
  --   hence we simply verify the original name subjecto to @opaque@ 
  pure $ Sq.singleton $ Let (Pat [patel]) aux $ BasicOp $ SubExp $ Var orig_nm
--
-- a successful Manifest target to "glb2reg_only" whose root
--   array is indeed in global memory is translated to a
--   copying segmap:
updateStm (td_env, bu_env) (Let (Pat [pel]) aux e)
  | BasicOp (Manifest arrnm _) <- e,
    regMapSucceeds bu_env (patElemName pel),
    Just _ <- intOfAttrGlb2RegOnly (stmAuxAttrs aux),
    -- check that the root of @arrnm@ is in global memory:
    glbnm <- fromMaybe arrnm (M.lookup arrnm (rootSlcArr td_env)),
    nameIn glbnm (freeVars bu_env),
    -- get its entry
    Just etry <- M.lookup (patElemName pel) (regArrays bu_env),
    n_tot <- length (shpdims etry),
    n_par <- length (pardims etry),
    n_par > 0 && n_par < n_tot = do
  let par_size_ses = map fst $ take n_par $ shpdims etry
  scope <- askScope
  (shm_nm, stms_manifest) <-
    runBuilder $ localScope scope $ do
      letExp (nameFromString "arr_shm") e
  e_sgmap <- genKerShm2Reg par_size_ses (patElemDec pel) shm_nm
  let aux' = aux { stmAuxAttrs = removeAttrGlb2RegOnly (stmAuxAttrs aux) }
  pure $ stms_manifest Sq.|> Let (Pat [pel]) aux' e_sgmap
--
-- A Manifest that was target to "glb2reg_only" BUT
--   whose root array is NOT in global memory gets
--   transformed into a trivial copy statement
updateStm (td_env, bu_env) (Let (Pat [pel]) aux e)
  | BasicOp (Manifest arrnm perm) <- e,
    isIdentityPerm perm,
    regMapSucceeds bu_env (patElemName pel),
    Just _ <- intOfAttrGlb2RegOnly (stmAuxAttrs aux),
    -- check that the root of @arrnm@ is in global memory:
    glbnm <- fromMaybe arrnm (M.lookup arrnm (rootSlcArr td_env)),
    not (nameIn glbnm (freeVars bu_env)) = do
  let aux' = aux { stmAuxAttrs = removeAttrGlb2RegOnly (stmAuxAttrs aux) }
  pure $ Sq.singleton $ Let (Pat [pel]) aux' $ BasicOp $ SubExp $ Var arrnm
{--
-- This is not needed any more since we decided to use MANIFEST!!!
--
-- slicing a global memory array: we assume the parallel space
--   can be found as part of the corresponding entry in @bu_env@
-- we generte a manifest statement to map the slice to shared memory,
--   then we use a segmap kernel with thread-private return to map
--   it to register memory.
updateStm (_td_env, bu_env) (Let (Pat [pel]) aux e)
  | BasicOp (Index{}) <- e,
    regMapSucceeds bu_env (patElemName pel),
    Array _ptp shp_res _u <- patElemDec pel,
    Just etry <- M.lookup (patElemName pel) (regArrays bu_env),
    n_tot <- length (shapeDims shp_res),
    n_par <- length (pardims etry),
    n_par > 0 && n_par < n_tot = do
  scope <- askScope
  (slc_shm_nm, stms_manifest) <-
    runBuilder $ localScope scope $ do
      new_slc_nm <- letExp (nameFromString "arr_slice") e
      let e_ind = BasicOp $ Manifest new_slc_nm [0 .. n_tot-1]
      letExp (nameFromString "shm_slice") e_ind
  --
  let par_size_ses = map fst $ take n_par $ shpdims etry
  e_sgmap <- genKerShm2Reg par_size_ses (patElemDec pel) slc_shm_nm
  pure $ stms_manifest Sq.|> Let (Pat [pel]) aux e_sgmap
--}
--
-- default case: don't change a thing!
updateStm _ stm =
  pure $ Sq.singleton stm

--------------------------------------
--- Code Generation Helpers
--------------------------------------

-- | Generates a deep-copy loop; it assumes it is called from inside a builder.
--   Arguments:
--    1. the unique type of the result array (target to in-place updates)
--    2. a list of @SubExp@ denoting the result-array dimensions
--    3. the enclosing-kernel thread ids as Params
--    4. the indices of the enclosing loops (generated so far) as Params
--    5. the variable name of the result array; initially it should be
--       the initializer of the loop variant, then each recursive invocation
--       constructs a new name.
--    6. the name of the input array, i.e., from where to copy
--   Result: a @Body@ containing the code for the copying loop and
--           whaterver prologue (scratch) and epilogue are needed
genCopyLoop :: (LocalScope GPU m, MonadFreshNames m) =>
               DeclType -> [SubExp] -> [Param Type] ->
               [Param Type] -> VName -> VName -> m (Body GPU)
-- the base case generates the body of the innermost loop:
genCopyLoop _ [] keridx_ps loop_ind_ps arr_nm slc_shm_nm = do
      scope <- askScope
      runBodyBuilder $ localScope (scope <> scopeOfLParams (keridx_ps ++ loop_ind_ps)) $ do
          let dimfix_ind = map (DimFix . Var . paramName) $ keridx_ps ++  loop_ind_ps
              e_ind = BasicOp $ Index slc_shm_nm $ Slice dimfix_ind
          el_se <- letSubExp (nameFromString "element") e_ind
          let dimfix_upd = map (DimFix . Var . paramName) loop_ind_ps
              e_upd = BasicOp $ Update Unsafe arr_nm (Slice dimfix_upd) el_se
          res_se <- letSubExp (nameFromString "reg_arr") e_upd
          pure [subExpRes res_se]
-- the recursive case generates one additional loop of the target nest:
genCopyLoop arrtp (d_se:dims) keridx_ps loop_ind_ps loop_ini_nm slc_shm_nm = do
      scope <- askScope
      ind_p <- newParam (nameFromString "loop_ind") (Prim i64ptp)
      arr_p <- newParam (nameFromString "loop_arr") arrtp
      let scope_params = scopeOfLParams (keridx_ps ++ [ind_p]) <> scopeOfFParams [arr_p]
      runBodyBuilder $ localScope (scope <> scope_params) $ do
        rec_body <-
          genCopyLoop arrtp dims keridx_ps (loop_ind_ps ++ [ind_p]) (paramName arr_p) slc_shm_nm
        let fpar_ses  = [(arr_p, Var loop_ini_nm)]
            loop_form = ForLoop (paramName ind_p) Int64 d_se
        res_se <- letSubExp (nameFromString "loop_res") $ Loop fpar_ses loop_form rec_body
        pure [subExpRes res_se]

genKerShm2Reg :: [SubExp] -> Type -> VName -> Shm2RegM (Exp GPU)
genKerShm2Reg par_size_ses res_tp slc_shm_nm =
  genKerShmReg ResultPrivate par_size_ses res_tp slc_shm_nm

genKerReg2Shm :: [SubExp] -> Type -> VName -> Shm2RegM (Exp GPU)
genKerReg2Shm par_size_ses res_tp slc_shm_nm =
  genKerShmReg ResultMaySimplify par_size_ses res_tp slc_shm_nm

-- | Arguments:
--     1. @ker_res_kind@: the @ResultManifest@, e.g.,
--          the shared-to-register direction should use @ResultPrivate@,
--          the reigster-to-shared direction should use @ResultMaySimplify@
--     2. @par_size_ses@ the parallel sizes of the generated kernel (as SubExp)
--     3. @res_tp@ the type of the result expression
--     4. @slc_shm_nm@ the name of the corresponding shared memory
--          (from which to copy)
--   Result: a SegMap expression that performs the copy from shared
--           memory to a register-allocated array.
--
genKerShmReg :: ResultManifest -> [SubExp] -> Type -> VName -> Shm2RegM (Exp GPU)
genKerShmReg ker_res_kind par_size_ses res_tp slc_shm_nm
  | Array ptp shp_res u <- res_tp = do
  let n_par = length par_size_ses
      slc_shm_par = Param mempty slc_shm_nm res_tp 
      kerid_strs = map (\i -> "tid" ++ show i) [0 .. n_par-1]
      priv_dims = drop n_par $ shapeDims shp_res
      priv_arr_tp = Array ptp (Shape priv_dims) u
      uniq_arr_tp = Array ptp (Shape priv_dims) Unique
  scope <- askScope
  keridx_ps <- mapM ( (`newParam` (Prim i64ptp)) . nameFromString ) kerid_strs
  ker_body <-
    runBodyBuilder $ localScope (scope <> scopeOfLParams (slc_shm_par : keridx_ps)) $ do
      let e_scratch = BasicOp $ Scratch ptp priv_dims
      nm_scratch <- letExp (nameFromString "arr_scratch") e_scratch
      body_with_loop <-
        genCopyLoop uniq_arr_tp priv_dims keridx_ps [] nm_scratch slc_shm_nm
      mapM_ addStm $ bodyStms body_with_loop
      pure $ map ( (Returns ker_res_kind (Certs [])) . resSubExp ) $ bodyResult body_with_loop
  flat_tid_nm <- newVName $ nameFromString "flat_tid"  
  let lvl = SegThreadInBlock SegNoVirt
      inner_space = SegSpace flat_tid_nm $ zip (map paramName keridx_ps) par_size_ses
  pure $ Op $ SegOp $ SegMap lvl inner_space [priv_arr_tp] ker_body
--
genKerShmReg _ _ res_tp _ =
  error ("In genKerShmReg: the result is not of array type: " ++ prettyString res_tp)

----------------------------------------------------------------
--- Copying the results of the Block Kernel
---   back to Shared-Memory whenever register-mapping suceeded
----------------------------------------------------------------

fixOneResult :: BotEnv -> KernelResult ->
                Shm2RegM (Stms GPU, KernelResult)
fixOneResult bu_env (Returns mnfs certs (Var reg_nm))
-- ^ check nmfs so that it is not Private!!!
  | regMapSucceeds bu_env reg_nm,
    Just etry <- M.lookup reg_nm (regArrays bu_env),
    n_par <- length (pardims etry),
    (shp_arr, _) <- unzip (shpdims etry),
    n_par > 0 && length shp_arr > n_par = do
  scope <- askScope
  let par_size_ses = take n_par shp_arr
      res_tp = case M.lookup reg_nm scope of
                 Just (LetName tp) -> tp
                 _ -> error "Result is not a let binding!"
  --
  e_sgmap <- genKerReg2Shm par_size_ses res_tp reg_nm
  (shm_nm, stms) <-
    runBuilder $ localScope scope $ do
      let shm_str = nameToString (baseName reg_nm) ++ "_to_shm"
      
      letExp (nameFromString shm_str) e_sgmap
  pure (stms, Returns mnfs certs (Var shm_nm))
-- otherwise do nothing
fixOneResult _ kres =
  pure (Sq.empty, kres)

fixRegKerResults :: BotEnv -> [KernelResult] ->
                    Shm2RegM (Stms GPU, [KernelResult])
fixRegKerResults bu_env kress = do
  stms_ress <- mapM (fixOneResult bu_env) kress
  let (stmss, kress') = unzip stms_ress
  pure (foldl (<>) Sq.empty stmss, kress')

