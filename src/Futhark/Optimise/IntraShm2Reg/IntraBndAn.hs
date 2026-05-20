module Futhark.Optimise.IntraShm2Reg.IntraBndAn
  ( shm2RegOnIntraStms ) where

--import Control.Monad
--import Control.Monad.Reader
--import Control.Monad.State hiding (state)
--import Control.Monad (forM)
import Data.List qualified as L
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
--import Futhark.Builder
import Futhark.IR.GPU
--import Futhark.IR.GPU.Simplify (simplifyGPU)
-- import Futhark.Optimise.TileLoops.Shared
--import Futhark.Pass
import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
--import Futhark.IR.Mem.LMAD qualified as LMAD
--import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
--import Futhark.Optimise.IntraShm2Reg.OutBndAn
import Futhark.Optimise.IntraShm2Reg.SymTabs
import Debug.Trace

i64ptp :: PrimType
i64ptp = IntType Int64

-----------------------------------------
--- Analysis
-----------------------------------------

-- | Traversal structure
shm2RegOnIntraStms :: Env -> Stms GPU -> Shm2RegM (Stms GPU)
shm2RegOnIntraStms env0 stmts = do
  ( (_, bu_env), stms') <- traverseStms env0 stmts
  let keys = M.keys $ regArrays bu_env
  trace ("Keys: " ++ prettyString keys) $
    pure stms'
  where
    traverseStms env Sq.Empty = pure (env, Sq.Empty)
    traverseStms (td_env, bu_env) (stm Sq.:<| stms) = do
      -- Compute @td_env@ top down
      let td_env' = updateTopdownEnv td_env stm
      -- Add potential target for bottom-up analysis (verification)
      let bu_env' = addTargetForAn (td_env, bu_env) stm
      -- Compute @bu_env@ bottom up
      ((_, bu_env''), stms') <- traverseStms (td_env', bu_env') stms
      -- let bu_env'' = updateBotmupEnv td_env' bu_env' stm
      -- stm' <- shm2RegOnStm (td_env', bu_env'') stm
      (bu_env''', stm') <- onBotUpStm (td_env', bu_env'') stm
      let env' = (td_env', bu_env''')
      pure ( env', stm' Sq.:<| stms')

-- | Applies the analysis in a body of statements:
--   1. sets the body result in the top-down env and
--   2. processes the statements by means of @shm2RegOnIntraStms@
shm2RegOnIntraBody :: Env -> Body GPU -> Shm2RegM (Body GPU)
shm2RegOnIntraBody env body = do
  let td_env' = (fst env) { bdy_res = map resSubExp $ bodyResult body}
  scope <- askScope
  stms' <- localScope (scope <> scopeOf (bodyStms body)) $
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



----------------------------------------
--- CORE FUNCTION is applySchedOnStm ---
----------------------------------------

{--

-- | Core function that does the replacement of array
--     mappings from shared memory to registers 
shm2RegOnStm :: Env -> Stm GPU -> Shm2RegM (Stm GPU)
shm2RegOnStm env stm@( Let pat aux ( Op ( SegOp ( SegMap lvl space ts kbody ) ) ) )
  | L.length (unSegSpace space) == 1,
    SegBlock virt (Just grid) <- lvl,
    Body yyy stms kres <- kbody = do
    -- shouldSequentialize (stmAuxAttrs aux) = do
  -- implementation starts here
  let attrs = stmAuxAttrs aux
  stms' <- trace ("Attributes Intragroup Ker: " ++ prettyString attrs) $ onIntraStms env stms
  let kbody' = Body yyy stms' kres
  pure $ Let pat aux ( Op ( SegOp ( SegMap lvl space ts kbody' ) ) )
--
shm2RegOnStm (_, bu_env) stm =
  pure stm
--}

{--
onIntraStms :: Env -> Stms GPU -> Shm2RegM (Stms GPU)
onIntraStms env stms = do
  stms' <- mapM (onIntraStm env) $ stmsToList stms 
  pure $ stmsFromList stms'
--}

onIntraStm :: Env -> Stm GPU -> Shm2RegM (Stm GPU)
onIntraStm _env stm@(Let _pat aux _e)
  |  attrs <- stmAuxAttrs aux,
     not (attrs == mempty) = do 
  -- Just attr <- hasAttr2RegMem (stmAuxAttrs aux) = do
  -- trace ("Found ATTRIBUTE FROM GLOBAL TO FAST: " ++ prettyString attrs ++ "\nstm:\n" ++ prettyString stm) $
    pure stm
--
onIntraStm _env stm@(Let _at aux _e) =
  -- trace ("some statement: " ++ prettyString stm ++ "\nAttributes: " ++ prettyString (stmAuxAttrs aux) ++ "\n\n") $
    pure stm

-----------------------------------
--- Adding a target for analysis:
-----------------------------------

addTargetForAn :: (TopEnv, BotEnv) -> Stm GPU -> BotEnv
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
--
-- the case of an inner-map kernel that can be changed to private result!
addTargetForAn (td_env, bu_env) (Let pat aux (Op (SegOp sgmap)))
  | SegMap (SegThreadInBlock {}) inner_space _ts kbody <- sgmap,
    (_idxs, ker_dim_ses) <- unzip (unSegSpace inner_space),
    ker_dim_pes <- map (peFromSe td_env i64ptp) ker_dim_ses,
    attrs <- stmAuxAttrs aux,
    Just _ <- hasAttr2RegMem attrs,
    bdyres <- bodyResult kbody,
    length bdyres == length (patElems pat) =
  let entry_space = zip ker_dim_ses ker_dim_pes
      nm_entries = mapMaybe (mkEntry entry_space) $ zip (patElems pat) bdyres
      regArrays' = foldl addEntry (regArrays bu_env) nm_entries
  in  trace ("TARGET: Annotated Map, pat_els " ++ prettyString pat ++
             " par-space: " ++ prettyString entry_space) $
        bu_env { regArrays = regArrays' }
  where
    addEntry tab (nm, entry) = M.insert nm entry tab
    mkEntry entry_space (patel, bdyres)
      | Returns ResultMaySimplify _cert _se <- bdyres,
        Array _ptp shp_res _u <- patElemDec patel,
        shp_res_ses <- shapeDims shp_res,
        shp_res_pes <- map (peFromSe td_env i64ptp) shp_res_ses,
        shp_dims <- zip shp_res_ses shp_res_pes,
        -- sanity check: the outer dimensions should equal
        --   the kernel dimensions:
        length entry_space <= length shp_dims,
        all (\(x,y) -> x == y) (zip shp_dims entry_space) =
      let entry = initEntry2 (zip shp_res_ses shp_res_pes) entry_space
      in  Just (patElemName patel, entry) 
    --
    mkEntry _ _ = Nothing
--
addTargetForAn (_, bu_env) _ = bu_env



----------------------------
--- Bottom-Up Traversal
----------------------------

onBotUpStm :: Env -> Stm GPU -> Shm2RegM (BotEnv, Stm GPU)
onBotUpStm env stm@(Let (Pat (pel:_)) _aux (Op (SegOp sgmap)))
  | SegMap (SegThreadInBlock {}) inner_space _ts kbody <- sgmap,
    (ker_idxs, ker_dim_ses) <- unzip (unSegSpace inner_space),
    ker_dim_pes <- map (peFromSe (fst env) i64ptp) ker_dim_ses,
    _bdyres <- bodyResult kbody = do
  let inn_env0 = InnerEnv ker_idxs ker_dim_pes mempty mempty
      (_,inn_env) = onBotUpInnerStms (fst env, inn_env0) $ bodyStms kbody
      reg_inds'= M.filter (== ker_idxs) $ reg_inds inn_env
      inn_env' = inn_env { reg_inds = reg_inds' }
      pel_nm   = patElemName pel
      bu_env'  = updateBotEnv pel_nm inn_env' (freeIn stm) (snd env)
  pure (bu_env', stm)
-- default, conservative case
onBotUpStm (_, bu_env) stm@(Let (Pat (pel:_)) _aux e) = do
  let inn_env = InnerEnv [] [] mempty mempty
      pat_el_nm = patElemName pel
      bu_env' = updateBotEnv pat_el_nm inn_env (freeIn e) bu_env
  pure (bu_env', stm)
-- otherwise error: pattern matching should imply empty Pat
onBotUpStm _env stm =
  error ("Unreachable case reached in onBotUpStm for stm: " ++ prettyString stm)

data InnerEnv = InnerEnv
  { ker_idxs :: [VName]
  -- ^ the thread indices of the inner kernel
  , ker_dims :: [PrimExp VName]
  -- ^ the dimensions of the inner kernel
  , reg_inds :: M.Map VName [VName]
  -- ^ array-name |-> the outer indices, which are supposed
  --   to be indexed with the tids, to allow register mapping.
  , indirect :: M.Map VName (VName, [VName])
  -- ^ a = b[tid1, tid2] is represented as a |-> (b, [tid1,tid2])
  }

-- | Helper that updates the BottomUp Environment
updateBotEnv :: VName -> InnerEnv -> Names -> BotEnv -> BotEnv
updateBotEnv pel_nm inn_env fvs bu_env =
  let reg_nms = namesFromList $ M.keys $ reg_inds inn_env
      regArrays' = M.mapWithKey (ff reg_nms) (regArrays bu_env)
  in  bu_env { regArrays = regArrays' }
  where
    ff regs key (RegEntry sdims pdims bnds) =
      let kind = case (nameIn key regs, nameIn key fvs) of
                   (True,_) -> Compat
                   (False,True) -> Irreg
                   _ -> None
      in  RegEntry sdims pdims $ (pel_nm, kind) : bnds

onBotUpInnerStms :: (TopEnv, InnerEnv) -> Stms GPU -> (TopEnv, InnerEnv)
onBotUpInnerStms env stms = foldl onBotUpInnerStm env $ stmsToList stms

onBotUpInnerStm :: (TopEnv, InnerEnv) -> Stm GPU -> (TopEnv, InnerEnv)
onBotUpInnerStm (top_env, inn_env) _stm =
  (top_env, inn_env)

{--
updateBotmupEnv :: TopEnv -> BotEnv -> Stm GPU -> BotEnv
updateBotmupEnv td_env bu_env _stm =
  bu_env
--}

----------------------------------------------------
--- Utility Functions, e.g., parsing attributes
----------------------------------------------------

hasAttr2RegMem :: Attrs -> Maybe Attr
hasAttr2RegMem (Attrs attrs) =
  let attrs' = S.toList attrs
   in case L.findIndex isFromGlb2Fast attrs' of
        Just i -> Just $ attrs' !! i
        Nothing -> Nothing
  where
    isFromGlb2Fast :: Attr -> Bool
    isFromGlb2Fast (AttrComp "tofastmem" [AttrInt 1]) = True
    isFromGlb2Fast _ = False

{--
findSeqAttr :: Attrs -> Maybe Attr
findSeqAttr (Attrs attrs) =
  let attrs' = S.toList attrs
   in case L.findIndex isSeqFactor attrs' of
        Just i -> Just $ attrs' !! i
        Nothing -> Nothing
  where
    isSeqFactor :: Attr -> Bool
    isSeqFactor (AttrComp "seq_factor" [AttrInt _]) = True
    isSeqFactor _ = False

getSeqFactor :: Attrs -> SubExp
getSeqFactor attrs =
  case findSeqAttr attrs of
    Just i' ->
      let (AttrComp _ [AttrInt x]) = i'
       in intConst Int64 x
    Nothing -> intConst Int64 4

shouldSequentialize :: Attrs -> Bool
shouldSequentialize attrs =
  case findSeqAttr attrs of
    Just _ -> True
    Nothing -> False

--}
