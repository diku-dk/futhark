module Futhark.Optimise.IntraShm2Reg.IntraBndAn
  ( shm2RegOnIntraStms ) where

--import Control.Monad
--import Control.Monad.Reader
--import Control.Monad.State hiding (state)
--import Control.Monad (forM)
-- import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
--import Futhark.Builder
import Futhark.IR.GPU
--import Futhark.IR.GPU.Simplify (simplifyGPU)
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
--import Futhark.IR.Mem.LMAD qualified as LMAD
--import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
--import Futhark.Optimise.IntraShm2Reg.OutBndAn
import Futhark.Optimise.IntraShm2Reg.SymTabs
import Futhark.Optimise.IntraShm2Reg.CodeGen(updateStm)
import Futhark.Optimise.IntraShm2Reg.IntraRecBodyAn
import Futhark.Util.Pretty
import Debug.Trace

i64ptp :: PrimType
i64ptp = IntType Int64

-----------------------------------------
--- Analysis
-----------------------------------------

-- | Traversal structure. ToDos:
--   1. This should probably be changed to entirely top-down
--        traversal with code generation included.
--   2. If "A" is mapped to registers and the next statement
--        is non-trivial---i.e., creating one or multiple shared
--        memory buffers---then "A" should be remapped to another
--        shared-memory buffer whenever necessary after this statement.
--   3. To handle global-to-reg remappings, we need to find the
--        parallel dimensions of the so-called principle inner kernel,
--        and attempt to remap based on it.
--   4. For simplicity, we can manifest to the same shared-memorybuffer
--        across whole `if` or `loops`
--   5. Register mapping should be automatically propagated to loops,
--        via the initializer (if sizes are consistent) and to the
--        results of `if-then-else`, similarly, if sizes permit.
--   6. We need to handle opaque!!!
shm2RegOnIntraStms :: Env -> Stms GPU -> Shm2RegM (BotEnv, Stms GPU)
shm2RegOnIntraStms env0 stmts = do
  ( (_, bu_env), stms') <- traverseStms env0 stmts
  -- trace (prettyString bu_env ++ "\nKernel Stms:\n" ++ prettyString stms') $
  pure (bu_env, stms')

traverseStms :: Env -> Stms GPU -> Shm2RegM (Env, Stms GPU)
-- base case:
traverseStms env Sq.Empty = pure (env, Sq.Empty)
-- Special cases amenable to simple preprocessing:
-- A @manifest@ with attribute @glb2reg_only@ which
--   does not have its base array in global memory
--   is translated to a simple copy statement.
traverseStms (td_env, bu_env) (stm Sq.:<| stms)
  | Let pat aux (BasicOp (Manifest arrnm perm)) <- stm,
    isIdentityPerm perm,
    Just _ <- intOfAttrGlb2RegOnly (stmAuxAttrs aux),
    glbnm <- fromMaybe arrnm (M.lookup arrnm (rootSlcArr td_env)),
    not (nameIn glbnm (freeVars bu_env)) = do
  let aux' = aux { stmAuxAttrs = removeAttrGlb2RegOnly (stmAuxAttrs aux) }
      stm' = Let pat aux' $ BasicOp $ SubExp $ Var arrnm
  traverseStms (td_env,bu_env) $ stm' Sq.<| stms
--
--traverseStms (td_env, bu_env) (stm Sq.:<| stms)
--  | Let pat aux (BasicOp (Manifest arrnm perm)) <- stm,
--    isIdentityPerm perm,
--    Just _ <- intOfAttrIgnore (stmAuxAttrs aux) = do
--  let aux' = aux { stmAuxAttrs = removeAttrIgnore (stmAuxAttrs aux) }
--      stm' = Let pat aux' $ BasicOp $ SubExp $ Var arrnm
--  traverseStms (td_env,bu_env) $ stm' Sq.<| stms
--
-- Loop Case is handled separately by function @traverseLoop@
  | Let _ _ Loop{} <- stm =
    traverseLoop traverseStms (td_env, bu_env) $ stm Sq.<| stms
--
-- Match Case is handled separately by function @traverseIf@
  | Let _ _ Match{} <- stm =
    traverseIf traverseStms (td_env, bu_env) $ stm Sq.<| stms
--
-- WithAcc Case is handled separately by function @traverseWithAcc@
  | Let _ _ WithAcc{} <- stm =
    traverseWithAcc traverseStms (td_env, bu_env) $ stm Sq.<| stms
--
-- General Traversal Structure:
traverseStms (td_env, bu_env) (stm Sq.:<| stms) = do
  -- Compute @td_env@ top down
  let td_env' = updateTopdownEnv td_env stm
  -- Add potential target for bottom-up analysis (verification)
  let bu_env' = addTargetForAn (td_env, bu_env) stm
  -- Compute @bu_env@ bottom up
  ((_, bu_env''), stms') <- traverseStms (td_env', bu_env') stms
  -- let bu_env'' = updateBotmupEnv td_env' bu_env' stm
  -- stm' <- shm2RegOnStm (td_env', bu_env'') stm
  bu_env''' <- onBotUpStm (td_env', bu_env'') stm
  let env' = (td_env', bu_env''')
  curr_stms <- updateStm env' stm
  pure ( env', curr_stms <> stms' )

-- | Applies the analysis in a body of statements:
--   1. sets the body result in the top-down env and
--   2. processes the statements by means of @shm2RegOnIntraStms@
shm2RegOnIntraBody :: Env -> Body GPU -> Shm2RegM (Body GPU)
shm2RegOnIntraBody env body = do
  let td_env' = (fst env) { bdy_res = map resSubExp $ bodyResult body}
  scope <- askScope
  (_bu_env', stms') <- localScope (scope <> scopeOf (bodyStms body)) $
    shm2RegOnIntraStms (td_env', snd env) $ bodyStms body
  pure $ body { bodyStms = stms' }

-- | Applies the analysis in a lambda, essentially by calling
--     @shm2RegOnIntraBody@
applySchedOnLambda :: Env -> Lambda GPU -> Shm2RegM (Lambda GPU)
applySchedOnLambda env lam = do
  scope <- askScope
  bdy <- localScope (scope <> scopeOf lam) $
           shm2RegOnIntraBody env $ lambdaBody lam
  pure $ lam { lambdaBody = bdy }

-----------------------------------
--- Adding a target for analysis:
-----------------------------------
-- | Arguments:
--     1. the top-down and bottom-up environments
--     2. the target statement
--   Result:
--     A new bottom up environment, which potentially
--     contains a new entry corresponding to the result
--     of the current statement being mapped to register
--     memory (if safe and if so indicated by user, or if
--     we are in cases such as loop/if where we deemed
--     to be always beneficial to use register mapping)
--     
-- To DO:
--   (1) check that the sizes of the register-allocated slice
--       only contains constants or user-defined params; the
--       top-down environment has already been extended in
--       this sense the field @userParams@.
addTargetForAn :: (TopEnv, BotEnv) -> Stm GPU -> BotEnv
-- the case of a manifest having the attribute "inform_pardim_only":
addTargetForAn (td_env, bu_env) (Let pat aux e)
  | BasicOp (Manifest arrnm perm) <- e,
    isIdentityPerm perm,
    -- check it has a "inform_pardim_only" attribute; get its int field:    
    Just num_par_dims <- intOfAttrParDimOnly (stmAuxAttrs aux),
    [pel] <- patElems pat,
    Array _ptp shp_res _u <- patElemDec pel,
    num_par_dims < length (shapeDims shp_res) =
  let bu_env' = mkManifestEntry bu_env arrnm num_par_dims shp_res
  in  mkManifestEntry bu_env' (patElemName pel) num_par_dims shp_res
--
-- the case of a manifest having the attribute "glb2reg_only":
  | BasicOp (Manifest arrnm perm) <- e,
    isIdentityPerm perm,
    -- check that the root of @arrnm@ is in global memory:
    glbnm <- fromMaybe arrnm (M.lookup arrnm (rootSlcArr td_env)),
    nameIn glbnm (freeVars bu_env),
    -- check it has a "glbtoreg_only" attribute; get its int field:    
    attrs <- stmAuxAttrs aux,
    Just num_par_dims <- intOfAttrGlb2RegOnly attrs,
    [pel] <- patElems pat,
    Array _ptp shp_res _u <- patElemDec pel,
    num_par_dims < length (shapeDims shp_res) =
  mkManifestEntry bu_env (patElemName pel) num_par_dims shp_res
  where
    mkManifestEntry bot_env nm num_par_dims shp_res =
      let shp_res_ses = shapeDims shp_res
          shp_res_pes = map (peFromSe td_env i64ptp) shp_res_ses
          par_dim_pes = take num_par_dims shp_res_pes
          entry = initEntry (zip shp_res_ses shp_res_pes) par_dim_pes
          entry'= validEntryForBotEnv nm bot_env entry
      in  --trace ("Target: Manifest annotated, pat-el: " ++ prettyString nm ++
          --       " shape_ses: "++prettyString shp_res_ses ++
          --       " shape_pes: " ++ prettyString shp_res_pes ) $
          bot_env { regArrays = M.insert nm entry' (regArrays bot_env) }
--
-- the case of an inner-map kernel that can be changed to private result!
addTargetForAn (td_env, bu_env) (Let pat aux (Op (SegOp sgmap)))
  | SegMap (SegThreadInBlock {}) inner_space _ts kbody <- sgmap,
    (_idxs, ker_dim_ses) <- unzip (unSegSpace inner_space),
    ker_dim_pes <- map (peFromSe td_env i64ptp) ker_dim_ses,
    attrs <- stmAuxAttrs aux,
    Just _ <- intOfAttr2RegMem attrs,
    bdyres <- bodyResult kbody,
    length bdyres == length (patElems pat) =
  let entry_space = ker_dim_pes -- zip ker_dim_ses ker_dim_pes
      nm_entries = mapMaybe (mkEntry entry_space) $ zip (patElems pat) bdyres
      regArrays' = foldl addEntry (regArrays bu_env) nm_entries
  in  --trace ("TARGET: Annotated Map, pat_els " ++ prettyString pat ++
      --       " par-space: " ++ prettyString entry_space) $
      bu_env { regArrays = regArrays' }
{--
-- We switched to supporting manifest, hence the hack below is NOT NEEDED
-- The case of taking the slice of an array declared outside the scope
--   of the intra-group kernel, i.e., an array allocated in global memory
-- ToDo: we should also check a corresponding annotation.
addTargetForAn (td_env, bu_env) (Let pat _aux e)
  | BasicOp (Index arrnm slice) <- e,
    [pel] <- patElems pat,
    Array _ptp shp_res _u <- patElemDec pel,
    _dims <- unSlice slice,
    nameIn arrnm (freeVars bu_env) =
  let shp_res_ses = shapeDims shp_res -- :: [(SubExp, PrimExp VName)],
      shp_res_pes = map (peFromSe td_env i64ptp) shp_res_ses
      entry = initEntry1 (zip shp_res_ses shp_res_pes)
      patel_nm = patElemName pel
  in  trace ("Target: Glob-Mem Slice, pat-el: "++prettyString patel_nm ++
             " shape_ses: "++prettyString shp_res_ses ++
             " shape_pes: " ++ prettyString shp_res_pes ) $
        bu_env { regArrays = M.insert patel_nm entry (regArrays bu_env) }

--}
  where
    addEntry tab (nm, entry) =
      M.insert nm (validEntryForBotEnv nm bu_env entry) tab
    eql (x, y) = x == y
    mkEntry entry_space (patel, bdyres)
      | Returns ResultMaySimplify _cert _se <- bdyres,
        Array _ptp shp_res _u <- patElemDec patel,
        shp_res_ses <- shapeDims shp_res,
        shp_res_pes <- map (peFromSe td_env i64ptp) shp_res_ses,
        shp_dims <- zip shp_res_ses shp_res_pes,
        -- sanity check: the outer dimensions should equal
        --   the kernel dimensions:
        length entry_space <= length shp_dims,
        all eql $ zip entry_space (map snd shp_dims) =
      let entry = initEntry (zip shp_res_ses shp_res_pes) entry_space
      in  Just (patElemName patel, entry) 
    --
    mkEntry _ _ = Nothing
--
-- a copy statement creates a new entry which is merged
-- in the base-array entry by the bottom-up processing
addTargetForAn (_td_env, bu_env) (Let pat _aux e)
  | BasicOp (SubExp (Var orig_nm)) <- e,
    Just orig_entry <- M.lookup orig_nm (regArrays bu_env), 
    [pel] <- patElems pat,
    pel_nm<- patElemName pel,
    Array{} <- patElemDec pel =
  let new_entry = orig_entry { bindings = mempty }
      new_entry'= validEntryForBotEnv pel_nm bu_env new_entry
      regArrays'= M.insert pel_nm new_entry' $ regArrays bu_env
  in bu_env { regArrays = regArrays' }
--
-- opaque is treated similarly to a copy statement
addTargetForAn (_td_env, bu_env) (Let pat _aux e)
  | BasicOp (Opaque OpaqueNil (Var orig_nm)) <- e,
    Just orig_entry <- M.lookup orig_nm (regArrays bu_env), 
    [pel] <- patElems pat,
    pel_nm<- patElemName pel,
    Array{} <- patElemDec pel =
  let new_entry = orig_entry { bindings = mempty }
      new_entry'= validEntryForBotEnv pel_nm bu_env new_entry
      regArrays'= M.insert pel_nm new_entry' $ regArrays bu_env
  in bu_env { regArrays = regArrays' }
{--
--
-- the case of a manifest having the attribute "glb2reg_only",
--   but which is not applied to a global-memory array: behaves
--   just as a copy statement (as in Opaque above)
addTargetForAn (td_env, bu_env) (Let pat aux e)
  | BasicOp (Manifest arrnm perm) <- e,
    isIdentityPerm perm,
    -- check that the root of @arrnm@ is NOT in global memory:
    glbnm <- fromMaybe arrnm (M.lookup arrnm (rootSlcArr td_env)),
    not (nameIn glbnm (freeVars bu_env)),
    -- check it has a "glbtoreg_only" attribute; get its int field:    
    attrs <- stmAuxAttrs aux,
    Just _ <- intOfAttrGlb2RegOnly attrs,
    [pel] <- patElems pat,
    Just orig_entry <- M.lookup arrnm (regArrays bu_env),
    Array{} <- patElemDec pel =
  let new_entry = orig_entry { bindings = mempty }
      regArrays'= M.insert (patElemName pel) new_entry $ regArrays bu_env
  in bu_env { regArrays = regArrays' }
--}
--
addTargetForAn (_, bu_env) _ = bu_env

--------------------------------------------
--- Code Generation for Register Mapping
--------------------------------------------


----------------------------
--- Bottom-Up Traversal
----------------------------

data InnerEnv = InnerEnv
  { ker_idxs :: [VName]
  -- ^ the thread indices of the inner kernel
  , ker_dims :: [PrimExp VName]
  -- ^ the dimensions of the inner kernel
  , fvs      :: Names
  -- ^ the free variables of the current kernel
  , ind_kind :: M.Map VName AccessKind
  -- ^ maps and array name to its access kind,
  --   denoting compatibility with register mapping
  --   (None, Irreg, Compat)
  , indirect :: M.Map VName (VName, [Int])
  -- ^ E.g., a = b[tid0, :, tid2] results in binding `a |-> (b, [1, 0, 1])`.
  --   More precisely, the @Int@ in the binding list has semantics:
  --     `0`  means full slice on the corresponding dimension
  --     `1`  means it was indexed with the correct threadId
  --     `-1` means unanalysable (incompatible) slice or index
  }
  
instance Pretty InnerEnv where
    pretty inn_env =
     "InnerEnv {\n\tKerIdxs: " <+> pretty (ker_idxs inn_env) <>
     "\n\tKerDims: " <+> pretty (ker_dims inn_env) <>
     "\n\tIndKind: "<+> pretty (M.toList (ind_kind inn_env)) <>
     "   }"

freshInnerEnv :: [VName] -> [PrimExp VName] -> Names -> InnerEnv
freshInnerEnv thids par_dims fvs =
  InnerEnv thids par_dims fvs ind_kind mempty
  where
    ind_kind = M.fromList $ map (\nm -> (nm,Compat)) $ namesToList fvs

-- | Summarize the uses of the variables intended to be mapped to
--     register memory in each statement into Campatible, Irregular,
--     or None. These uses are recorded in the @bindings@ field of
--     @RegEntry@. We treat several cases of high interest below
--     (namely an inner-kernels, manifest, opaque, shallow copy stmts)
--     and for the rest we apply a very conservative treatment that
--     disqualifies all free names in the target statement. 
--   Please note that "Loops" and "Ifs" have specialized treatment
--     implemented in IntraRecBodyAn.
onBotUpStm :: Env -> Stm GPU -> Shm2RegM BotEnv
onBotUpStm (top_env, bu_env) stm@(Let (Pat (pel:_)) aux e)
  -- the target case: a segmented-map operation:
  | Op (SegOp sgmap) <- e,
    SegMap (SegThreadInBlock {}) inner_space _ts kbody <- sgmap,
    (ker_idxs, ker_dim_ses) <- unzip (unSegSpace inner_space),
    ker_dim_pes <- map (peFromSe top_env i64ptp) ker_dim_ses,
    fvs <- freeIn stm,
    _bdyres <- bodyResult kbody = do
  let inn_env0 = freshInnerEnv ker_idxs ker_dim_pes fvs
      (_, inn_env) = onBotUpInnerStms (top_env, inn_env0) $ bodyStms kbody
  pure $ updateBotEnv (patElemName pel) inn_env bu_env
  --
  -- The case of a shallow-copy: just transfer the entry to the pattent
  | BasicOp (SubExp (Var orig_nm)) <- e,
    Just curr_entry <- M.lookup (patElemName pel) (regArrays bu_env),
    Just orig_entry <- M.lookup orig_nm (regArrays bu_env),
    Array{} <- patElemDec pel = do
  let new_entry = mergeBindings orig_entry curr_entry
      regArrays'= M.insert orig_nm new_entry $ regArrays bu_env
  pure $ bu_env { regArrays = regArrays' }
  --
  -- The case of Opaque: similar to shallow-copy statement
  | BasicOp (Opaque OpaqueNil (Var orig_nm)) <- e,
    Just curr_entry <- M.lookup (patElemName pel) (regArrays bu_env),
    Just orig_entry <- M.lookup orig_nm (regArrays bu_env),
    Array{} <- patElemDec pel = do
  let new_entry = mergeBindings orig_entry curr_entry
      regArrays'= M.insert orig_nm new_entry $ regArrays bu_env
  pure $ bu_env { regArrays = regArrays' }
  --
  -- The case of Manifest with "inform_pardim_only" attribute:
  --   is equivalent with a shallow copy
  | BasicOp (Manifest orig_nm perm) <- e,
    isIdentityPerm perm,
    -- check it has a "inform_pardim_only" attribute; get its int field:    
    Just _ <- intOfAttrParDimOnly (stmAuxAttrs aux),
    Just curr_entry <- M.lookup (patElemName pel) (regArrays bu_env),
    Just orig_entry <- M.lookup orig_nm (regArrays bu_env),
    Array{} <- patElemDec pel = do
  let new_entry = mergeBindings orig_entry curr_entry
      regArrays'= M.insert orig_nm new_entry $ regArrays bu_env
  pure $ bu_env { regArrays = regArrays' }
{--
  -- the case of a Manifest with "glb2reg_only" attribute who is
  --   not from global memory; just trasfer the entry to the parrent 
  | BasicOp (Manifest arrnm perm) <- e,
    -- check that the root of @arrnm@ is in not global memory:
    glbnm <- fromMaybe arrnm (M.lookup arrnm (rootSlcArr top_env)),
    isIdentityPerm perm && not (nameIn glbnm (freeVars bu_env)),
    -- check it has a "glbtoreg_only" attribute; get its int field:    
    Just _ <- intOfAttrGlb2RegOnly (stmAuxAttrs aux),
    Just curr_entry <- M.lookup (patElemName pel) (regArrays bu_env),
    Just orig_entry <- M.lookup arrnm (regArrays bu_env),
    Array{} <- patElemDec pel = do
  let new_entry = mergeBindings orig_entry curr_entry
      regArrays'= M.insert arrnm new_entry $ regArrays bu_env
  pure $ bu_env { regArrays = regArrays' }
--}
  -- conservative case: mark irregular all accesses of free vars in this stm
  | True = do
  let ind_kind= M.fromList $ map (\nm -> (nm,Irreg)) $ namesToList $ freeIn stm
      inn_env = InnerEnv [] [] (freeIn stm) ind_kind mempty
  pure $ updateBotEnv (patElemName pel) inn_env bu_env
  where
    -- Helper that updates the BottomUp Environment
    updateBotEnv pel_nm inn_env bot_env =
      let regArrays' = M.mapWithKey (ff inn_env pel_nm) (regArrays bot_env)
      in  bot_env { regArrays = regArrays' }
    ff inn_env pel_nm key (RegEntry sdims pdims bnds)
      | null pdims && not (null (ker_dims inn_env)),
        Just Compat <- M.lookup key (ind_kind inn_env) =
      RegEntry sdims (ker_dims inn_env) $ (pel_nm, Compat) : bnds
    --
    ff inn_env pel_nm key (RegEntry sdims pdims bnds) =
      let kind = fromMaybe None $ M.lookup key $ ind_kind inn_env
          inn_par_dims = ker_dims inn_env
          inn_par_dims_valid = not $ null inn_par_dims
          kind'= if kind == Compat && inn_par_dims_valid && not (pdims == inn_par_dims)
                 then Irreg else kind
      in  RegEntry sdims pdims $ (pel_nm, kind') : bnds
    -- opaque helper functions
    mergeBindings etry1 etry2
      | (bnds1, bnds2) <- (bindings etry1, bindings etry2),
        length bnds1 == length bnds2,
        map fst bnds1 == map fst bnds2 =
      let bnds' = zipWith (\(nm,k1) (_,k2) -> (nm, unifyAccK k1 k2)) bnds1 bnds2
          pardims' = if null (pardims etry1) then pardims etry2 else pardims etry1
      in  etry1 { pardims = pardims', bindings = bnds' }
    mergeBindings etry_long etry_short =
      error $ "Opaque: inconsistent bindings:\n\t" ++
              prettyString etry_long ++ 
              "\n\t" ++ prettyString etry_short
-- Otherwise error: pattern matching should imply empty Pat
onBotUpStm _env stm =
  error ("Unreachable case reached in onBotUpStm for stm: " ++ prettyString stm)

onBotUpInnerStms :: (TopEnv, InnerEnv) -> Stms GPU -> (TopEnv, InnerEnv)
onBotUpInnerStms env stms =
  foldl topbotpass env $ stmsToList stms
  where
    topbotpass (top_env, bot_env) stm =
      let top_env' = updateTopdownEnv top_env stm
          bot_env' = onBotUpInnerStm (top_env', bot_env) stm
      in  (top_env', bot_env')

onBotUpInnerStm :: (TopEnv, InnerEnv) -> Stm GPU -> InnerEnv
onBotUpInnerStm (_,inn_env) (Let _ _ (BasicOp (Index nm _)))
  -- index uses an array already known to be incompatible; nothing to do
  | nm' <- fst $ fromMaybe (nm,[]) $
           M.lookup nm (indirect inn_env),
    Just kind <- M.lookup nm' (ind_kind inn_env),
    kind == Irreg = inn_env
--
-- A screma in which all inputs are already known to be
-- compatible and no free-vars exist in the form then OK!
onBotUpInnerStm (_top_env, inn_env) (Let _pat _aux e)
  | Op (OtherOp (Screma _w arrs form)) <- e,
    all alreadyCompat arrs && noFreeVarsInForm form =
  inn_env
  where
    alreadyCompat arrnm =
      let k = length $ ker_idxs inn_env
          (arrnm', dims) = fromMaybe (arrnm,replicate k 0) $ M.lookup arrnm $ indirect inn_env
          kind = fromMaybe Irreg $ M.lookup arrnm' $ ind_kind inn_env
      in  all (==1) dims || kind == Compat --   || not (nameIn arrnm' (fvs inn_env))
      --  ^ is the last term of the disjunction safe???
    noFreeVarsInForm (ScremaForm lam1 scans reds lam2) =
      mempty == freeIn lam1 && mempty == freeIn lam2 &&
      mempty == map fvsScan scans && mempty == map fvsRed reds
    fvsScan :: Scan GPU -> Names
    fvsScan (Scan lam nes) = foldl (<>) (freeIn lam) $ map freeIn nes
    fvsRed  :: Reduce GPU -> Names
    fvsRed  (Reduce _ lam nes) = foldl (<>) (freeIn lam) $ map freeIn nes
--
-- The index corrresponds to a partially-indexed global array that
--   was not already proven to be incompatible with register mapping;
-- This is the case on which we can find regular accesses that allow
--   mapping to register memory.
onBotUpInnerStm (top_env, inn_env) (Let pat _aux e)
  | BasicOp (Index arrnm slice) <- e,
    Pat [patel] <- pat =
  let k = length $ ker_idxs inn_env
      (arrnm', dimslcs) =
        fromMaybe (arrnm, replicate k 0) $
        M.lookup arrnm  (indirect inn_env)
      inds_fslc= getIndsOfFullSlice dimslcs
      new_slcs = zipWith analyseDim inds_fslc $ unSlice slice
      dimslcs' = scatterMon dimslcs $ zip inds_fslc new_slcs
  in case (all (==1) dimslcs', any (== (-1)) dimslcs') of
       (True,_) -> inn_env -- index compatible with register mapping
       (_,True) -> -- discovered register-mapping incompatiblity
         disqualify (namesFromList [arrnm']) inn_env
       (False,False) -> -- some full slices still exist; update @indirect@ tab
         let patelnm = patElemName patel
             indirect' = M.insert patelnm (arrnm', dimslcs') $ indirect inn_env
         in  inn_env { indirect = indirect' }
  where
    getIndsOfFullSlice dimslcs =
      foldl ff [] $ zip dimslcs [0 .. length dimslcs - 1]
    ff acc (0, i) = acc ++ [i]
    ff acc _ = acc
    analyseDim i_fslc (DimFix se)
      | LeafExp vnm _ptp <- removeTpConv (peFromSe top_env i64ptp se),
        vnm == (ker_idxs inn_env) !! i_fslc = 1
      -- ^ found dimemsion indexed by the correct gid
    analyseDim _ (DimFix _) = -1
      -- ^ if reached here: the index is incompatible with register mapping
    analyseDim _ (DimSlice beg _n stride)
      | beg == se0 && stride == se1 = 0
      -- ^ a slice starting from 0 with stride 1 can be further analysed
    analyseDim _ DimSlice{} = -1 -- un-analysable slice
    scatterMon :: [Int] -> [(Int,Int)] -> [Int]
    scatterMon dimslc [] = dimslc
    scatterMon dimslc ((i,_):ivs)
      | i < 0 || i >= length dimslc =
      scatterMon dimslc ivs
    scatterMon dimslc ((i,v):ivs) =
      scatterMon (take i dimslc ++ [v] ++ drop (i+1) dimslc) ivs
    se0 = Constant $ IntValue $ Int64Value 0
    se1 = Constant $ IntValue $ Int64Value 1
--
-- Case If-Then-Else: simply recurse on branches
-- ^ ToDo: implement me!
-- Case loops: simply recruse on loop body
onBotUpInnerStm env (Let _pat _aux e)
  | Loop fpar_ses _form loop_body <- e,
    (_fpars, init_ses) <- unzip fpar_ses =
    let (_, inn_env) = handleBody env loop_body
    -- also conservatively disqualify the inits
        disq_vars = namesFromList $ mapMaybe nameFromSe $ init_ses 
    in  disqualify disq_vars inn_env
  | Match _ses cases def_body _dec_match <- e =
    snd $ foldl handleBody env $ def_body : (map caseBody cases)
  | WithAcc inps lam <- e =
    let (_, inn_env) = handleBody env $ lambdaBody lam
    -- also conservatively disqualify the withacc inputs
        inp_nms = namesFromList $ concat $ map (\(_,nms,_) -> nms) inps
    in  disqualify inp_nms inn_env
  where
    nameFromSe (Var nm) = Just nm
    nameFromSe _ = Nothing
    handleBody env1 body =
      let (top_env, inn_env) = onBotUpInnerStms env1 $ bodyStms body
          res_nms = mapMaybe nameFromSe $ map resSubExp $ bodyResult body
      in  (top_env, disqualify (namesFromList res_nms) inn_env)
      

-- Conservative treatment: mark all free variables with irregular access.
onBotUpInnerStm (_, inn_env) stm =
  disqualify (freeIn stm) inn_env

-- | Args:
--     @fvs@ a set of names
--     @inn_env@ an InnerEnv 
--   Seamantics:
--     expands the input names @fvs@ by means of the @indirect@ filed of 
--     the environment @inn_env@ and then marks each of the resulted names
--     as having @Irreg@ accesses in the @ind_kind@ filed of @inn_env@.
disqualify :: Names -> InnerEnv -> InnerEnv
disqualify fvs inn_env =
  let fvs' = expandFVbyIndirect fvs
      ind_kind' = M.mapWithKey (fkey fvs') $ ind_kind inn_env
  in  inn_env { ind_kind = ind_kind' }
  where
    expandFVbyIndirect = namesFromList . (map liftArrName) . namesToList
    liftArrName nm =
      case M.lookup nm (indirect inn_env) of
        Just (gnm,_) -> gnm
        Nothing      -> nm
    fkey fvnms nm kind =
      if nameIn nm fvnms then Irreg else kind

