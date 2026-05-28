-- | This modules handles the analysis of intra statement
--   with recursive bodies, such as Loop and Match
module Futhark.Optimise.IntraShm2Reg.IntraRecBodyAn
  ( traverseIf, traverseLoop ) where

import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
import Futhark.IR.GPU
import Futhark.Tools
import Futhark.Optimise.IntraShm2Reg.SymTabs
import Futhark.Optimise.IntraShm2Reg.CodeGen(genKerReg2Shm,genKerShm2Reg)

-- | ToDo: the implementation assumes that manifests were placed
--         on the result of Match with user annotations that specify
--         the parallel dimension of the register-allocated kernels.
--    Please implement said annotated manifests on the bottom-up pass.
traverseIf :: (Env -> Stms GPU -> Shm2RegM (Env, Stms GPU)) ->
              Env -> Stms GPU -> Shm2RegM (Env, Stms GPU)
traverseIf _ _ Sq.Empty =
  error ("traverseIf applied to empty stms!")
traverseIf traverseStms (td_env, bu_env) (stm Sq.:<| stms)
  | Let (Pat pels) aux (Match ses cases def_bdy match_dec) <- stm,
    pel : _ <- pels = do
  let pat_nms = namesFromList $ map patElemName pels
      pat_tps = map patElemType pels
      mentries = map (mkIfEntries pat_nms) pels
      aft_entries = M.fromList $ mkEntries mentries $ map (Var . patElemName) pels
      bu_env' = bu_env { regArrays = M.union (regArrays bu_env) aft_entries }
  -- travers the remaining statements in the outer scope of the Match
  ((_, bu_env''), stms') <- traverseStms (td_env, bu_env') stms
  -- fix the results of the whole Match: if unsafe in register,
  -- then map them to shared memory just after the Match:
  --   @pels'@ are the new pattern elements of the Match
  --   @fix_aft_stms@ need to be inserted just after the Match
  scope <- askScope
  fix_out_res <- mapM (fixRegOrShmAlloc scope True bu_env'') $
                   zip (map (Var . patElemName) pels) (map patElemDec pels)
  let pels' = zipWith updPatElemName pels $ map fst fix_out_res
      fix_aft_stms = foldl (<>) mempty $ map snd fix_out_res
  -- Now, we recursively analyze each of the Match bodies:
  let bodies = map caseBody cases ++ [def_bdy]
  bodies_buenvs <- mapM (analyzeBody scope bu_env'' pat_tps mentries) bodies
  let (bodies', bot_envs) = unzip bodies_buenvs
      bu_env''' = foldl (mergeInFstBotEnv (patElemName pel)) bu_env'' bot_envs
      cases' = zipWith (\c bdy -> c { caseBody = bdy }) cases bodies'
      stm' = Let (Pat pels') aux $ Match ses cases' (last bodies') match_dec
      all_stms = Sq.singleton stm' <> fix_aft_stms <> stms'
  pure ((td_env, bu_env'''), all_stms)
  -- 
  where
    mkIfEntries pat_nms pel
      | Just entry <- M.lookup (patElemName pel) (regArrays bu_env),
        Array _ptp shape _u <- patElemType pel,
        fvs_shp <- namesToList (freeIn shape),
        all (\x -> not (nameIn x pat_nms)) fvs_shp,
        shp_res_ses <- shapeDims shape,
        shp_res_pes <- map (peFromSe td_env (IntType Int64)) shp_res_ses,
        -- sanity check:
        shpdims entry == zip shp_res_ses shp_res_pes =
      Just $ entry { bindings = [] }
    mkIfEntries _ _ = Nothing
    --
    analyzeBody scope bot_env pat_tps mentries body = do
      let bdy_res_ses = map resSubExp $ bodyResult body 
          new_entries = M.fromList $ mkEntries mentries $ bdy_res_ses
          bot_env' = bot_env { regArrays = M.union (regArrays bot_env) new_entries }
          scope' = scope <> scopeOf (bodyStms body)
      ((_, bot_env''), inner_stms) <- localScope scope' $
        traverseStms (td_env, bot_env') $ bodyStms body
      -- if a body result could not be mapped to register than
      --   fix it by mapping to register just before the end of its body
      fix_inn_res <- mapM (fixRegOrShmAlloc scope' False bot_env'') $
                     zip bdy_res_ses pat_tps
      let bdy_res_ses' = map fst fix_inn_res
          fix_res_stms = foldl (<>) mempty $ map snd fix_inn_res
      -- construct new body:
          new_body_stms = inner_stms <> fix_res_stms
          new_body_res  = zipWith updResSubExp (bodyResult body) bdy_res_ses'
          body' = body { bodyStms = new_body_stms, bodyResult = new_body_res }
      pure (body', bot_env'')
--
traverseIf _ _ (stm Sq.:<| _) =
  error ("In traverseIf, current statement is not a Match stm: " ++ prettyString stm)

-- | Specialized implementation of the traversal for the case of Loop
--   ToDos:
--     fix bug: currently if the size of the loop variant is
--              not equal to the initializer; the entry in bu_env
--              is not invalidated! It should because otherwise
--              they are in compatible memories: share mem vs regs
traverseLoop :: (Env -> Stms GPU -> Shm2RegM (Env, Stms GPU)) ->
                Env -> Stms GPU -> Shm2RegM (Env, Stms GPU)
traverseLoop _ _ Sq.Empty =
  error ("traverseLoop applied to empty stms!")
traverseLoop traverseStms (td_env, bu_env) (stm Sq.:<| stms)
  | Let (Pat pels) aux (Loop fpar_ini_ses lform lbody) <- stm,
    pel : _ <- pels,
    (fpars, ini_ses) <- unzip fpar_ini_ses,
    res_ses <- map resSubExp (bodyResult lbody),
    length pels == length fpars = do
  scope <- askScope
  let tups = zip4 pels fpars ini_ses res_ses
      mentries = map (mkLoopEntries scope) tups
  -- add the entries for the result of the loop (essentially addTargetForAn)
      aft_entries = M.fromList $ mkEntries mentries $ map (Var . patElemName) pels
      bu_env' = bu_env { regArrays = M.union (regArrays bu_env) aft_entries }
  -- travers the remaining statements in the outer scope of the loop
  ((_, bu_env''), stms') <- traverseStms (td_env, bu_env') stms
  -- Now, we recursively analyze the loop body:
  let old_entries = M.map (\etry -> etry {bindings = []}) (regArrays bu_env)
      inn_entries_p = mkEntries mentries $ map (Var . paramName) fpars
      inn_entries_r = mkEntries mentries res_ses
      new_inn_entries = M.fromList $ inn_entries_p ++ inn_entries_r
      inner_entries = M.union old_entries new_inn_entries
      -- processing the loop body
      inner_bu_env= bu_env { regArrays = inner_entries }
      inner_scope = scope <> scopeOfFParams fpars <> scopeOf (bodyStms lbody)
  ((_, inner_bu_env'), inner_stms) <- localScope inner_scope $
    traverseStms (td_env, inner_bu_env) $ bodyStms lbody
  -- we merge the results from @bu_env''@ and @inner_bu_env'@
  let bu_env''' = mergeInFstBotEnv (patElemName pel) bu_env'' inner_bu_env'
  -- fix the results of the whole loop: if unsafe in register,
  -- then map them to shared memory just after the loop:
  --   @pels'@ are the new pattern elements of the loop
  --   @fix_aft_stms@ need to be inserted just after the loop
  fix_out_res <- mapM (fixRegOrShmAlloc scope True bu_env''') $
                   zip (map (Var . patElemName) pels) (map patElemDec pels)
  let pels' = zipWith updPatElemName pels $ map fst fix_out_res
      fix_aft_stms = foldl (<>) mempty $ map snd fix_out_res
  -- fix the fpars which are not safe to be in register, i.e., keep
  --   them in registers but move them to shared memory at iteration's start:
  fix_inn_arg <- mapM (fixRegOrShmAlloc inner_scope True inner_bu_env') $
                   zip (map (Var . paramName) fpars) $ map patElemDec pels
  let fpars' = zipWith updFParamName fpars $ map fst fix_inn_arg
      fix_arg_stms = foldl (<>) mempty $ map snd fix_inn_arg
  -- fix the body results which are not safe to be in register, i.e.,
  --   move them from shared memory to registers at iteration's end:
  fix_inn_res <- mapM (fixRegOrShmAlloc inner_scope False inner_bu_env') $
                   zip res_ses $ map patElemDec pels
  let res_ses' = map fst fix_inn_res
      fix_res_stms = foldl (<>) mempty $ map snd fix_inn_res
  -- construct result statements
      new_loop_stms = fix_arg_stms <> inner_stms <> fix_res_stms
      new_body_res  = zipWith updResSubExp (bodyResult lbody) res_ses'
      lbody' = lbody { bodyStms = new_loop_stms, bodyResult = new_body_res }
  -- fix the intializers: always copy them to shared memory
  fix_loop_ini<- mapM (forceToReg scope bu_env''') $ zip ini_ses $ map patElemDec pels
  let ini_ses' = map fst fix_loop_ini
      fix_ini_stms = foldl (<>) mempty $ map snd fix_loop_ini
      loop_stm = Let (Pat pels') aux (Loop (zip fpars' ini_ses') lform lbody')
      all_stms = fix_ini_stms <> Sq.singleton loop_stm <> fix_aft_stms <> stms'
  pure ((td_env, bu_env'''), all_stms)
  where
    updFParamName fpar (Var nm) = fpar { paramName = nm }
    updFParamName fpar _ = fpar
    --
    -- a valid entry is created for a tuple whose types match
    --   the one of the loop initializer, and the loop initializer
    --   is planned to be hold in register memory
    mkLoopEntries scope (y, x_arg, Var x_ini, Var _x_res)
      | Just etry_ini <- M.lookup x_ini (regArrays bu_env), 
        Just (LetName tp_ini) <- M.lookup x_ini scope,
        Array ptp1 shp1 _u <- tp_ini,
        Array ptp2 shp2 _u <- paramDec x_arg,
        tp_ini == patElemDec y && ptp1 == ptp2 && shp1 == shp2 =
      Just $ etry_ini { bindings = [] }
    mkLoopEntries _ _ = Nothing
    --
    forceToReg :: Scope GPU -> BotEnv -> (SubExp, Type) ->
                  Shm2RegM (SubExp, Stms GPU)
    forceToReg scope bot_env (Var penm, petp)
      | Just entry <- M.lookup penm (regArrays bot_env),
        n_par <- length (pardims entry),
        par_size_ses <- map fst (take n_par (shpdims entry)),
        not (null par_size_ses) = do
      let scope' = scope <> scopeOfLParams [Param (Attrs mempty) penm petp]
      e_sgmap <- genKerShm2Reg par_size_ses petp penm
      (new_nm, sgmap_stms) <-
        runBuilder $ localScope scope' $ do
          letExp (nameFromString "tmp") e_sgmap
      pure (Var new_nm, sgmap_stms)
    forceToReg _ _ (se, _) = pure (se, mempty)
--
traverseLoop _ _ (stm Sq.:<| _) =
  error ("In traverseLoop, current statement not a loop: " ++ prettyString stm)

----------------------------------------
--- Helper Functions
----------------------------------------

zip4 :: [t1] -> [t2] -> [t3] -> [t4] -> [(t1,t2,t3,t4)]
zip4 as bs cs ds =
  zipWith (\(a,b) (c,d) -> (a,b,c,d)) (zip as bs) (zip cs ds)

-- | Helper: return var names paired with corresponding register entry
mkEntries :: [Maybe RegEntry] -> [SubExp] -> [(VName, RegEntry)]
mkEntries mentries ses =
  mapMaybe addMEntry $ zip mentries ses
  where
    addMEntry (Just etry, Var nm) = Just (nm, etry)
    addMEntry _ = Nothing

-- | Helper to update a pattern element with a new name
updPatElemName :: PatElem (LetDec GPU) -> SubExp -> PatElem (LetDec GPU)
updPatElemName patel (Var nm) = patel { patElemName = nm }
updPatElemName patel _ = patel

-- | Helper to update a body result with a new name
updResSubExp :: SubExpRes -> SubExp -> SubExpRes
updResSubExp res_se se = res_se { resSubExp = se }

--
-- | Arguments:
--     @in_shm@ put the result in shared memory
--     @aux@ an aux to construct the statement
--     @bot_env@ the bottom up environment
--     @(se, ptp)@ a subexpression tupled with its @Type@
-- Result: a SubExp tupled with  sequence of statements
-- Semantics:
--   In case @se = Var penm@ and @penm@ is unsafely to
--   be allocated in register memory, then it generates
--   the segmap statement that transfers it either to shared
--   memory or to register memory, as dictated by @in_shm@.
--   The result of segmap is stored in @penm@, and the
--   first result is a new variable name that will replace
--   the result of the loop (body) or the fparam of the loop.
--
fixRegOrShmAlloc :: Scope GPU -> Bool -> BotEnv ->
                    (SubExp, Type) -> Shm2RegM (SubExp, Stms GPU)
fixRegOrShmAlloc scope in_shm bot_env (Var penm, petp)
  | not (regMapSucceeds bot_env penm),
    Just entry <- M.lookup penm (regArrays bot_env),
    n_par <- length (pardims entry),
    par_size_ses <- map fst (take n_par (shpdims entry)),
    not (null par_size_ses) = do
  new_penm <- newVName $ nameFromString $ nameToString (baseName penm) ++ "_reg"
  let genKerFun = if in_shm then genKerReg2Shm else genKerShm2Reg
      scope' = scope <> scopeOfLParams [Param (Attrs mempty) penm petp]
  e_sgmap <- genKerFun par_size_ses petp new_penm
  (_, sgmap_stms) <-
    runBuilder $ localScope scope' $ do
      letExp (nameFromString "tmp") e_sgmap
  let sgmap_stm = updStmt penm sgmap_stms
  pure (Var new_penm,  Sq.singleton sgmap_stm)
  where
    updStmt nm ( (Let (Pat [patel]) aux e) Sq.:<| stmts )
      | Sq.null stmts =
      let patel' = patel { patElemName = nm }
      in  Let (Pat [patel']) aux e
    updStmt _ stmts =
      error("Argument stms is not a single stmt: " ++ prettyString stmts)
--
-- no fix needed if already in registers or not target to register
fixRegOrShmAlloc _ _ _ (se, _) = pure (se, mempty)
