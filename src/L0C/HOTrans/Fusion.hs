{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.HOTrans.Fusion ( fuseProg )
  where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
--import Control.Monad.Writer

import Data.Loc

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

--import Language.L0
import L0C.L0
import L0C.FreshNames
import L0C.EnablingOpts.EnablingOptDriver
import L0C.HOTrans.Composing

data FusionGEnv = FusionGEnv {
                    soacs      :: M.Map VName ([VName], Exp)
                  , arrsInScope:: S.Set VName -- M.Map VName (TupIdent tf)
                  , fusedRes   :: FusedRes --M.Map VName (FunDec Type)
                  , program    :: Prog
                  }


newtype FusionGM a = FusionGM (StateT VNameSource (ReaderT FusionGEnv (Either EnablingOptError)) a)
    deriving (  MonadState VNameSource,
                MonadReader FusionGEnv,
                Monad, Applicative, Functor )


------------------------------------------------------------------------
--- Monadic Helpers: bind/new/runOptimPass, etc                      ---
------------------------------------------------------------------------


-- | Binds an array name to the set of used-array vars
bindVar :: FusionGEnv -> VName -> FusionGEnv
bindVar env name =
  env { arrsInScope = S.insert name $ arrsInScope env }

bindVars :: FusionGEnv -> [VName] -> FusionGEnv
bindVars = foldl bindVar

binding :: TupIdent -> FusionGM a -> FusionGM a
binding pat = local (`bindVars` map identName (filter (\x->case identType x of
                                                                Array{} -> True
                                                                _       -> False)
                                                       (getIdents pat)
                                               )
                    )

-- | Binds an array name to the set of soac-produced vars
bindPatVar :: ([VName],Exp) -> FusionGEnv -> VName -> FusionGEnv
bindPatVar soac env nm = env { soacs       = M.insert nm soac $ soacs env
                             , arrsInScope = S.insert nm      $ arrsInScope env
                             }

bindPatVars :: ([VName], Exp) -> FusionGEnv -> [VName] -> FusionGEnv
bindPatVars soac = foldl (bindPatVar soac)

bindPat :: TupIdent -> Exp -> FusionGM a -> FusionGM a
bindPat pat soac = do
    let nms = map identName $ getIdents pat
    let xxx = bindPatVars (nms, soac)
    local ( `xxx` nms )

-- | Binds the fusion result to the environment.
bindRes :: FusedRes -> FusionGM a -> FusionGM a
bindRes rrr = local (\x -> x { fusedRes = rrr })

-- | The fusion transformation runs in this monad.  The mutable
-- state refers to the fresh-names engine.
-- The reader hides the vtable that associates ... to ... (fill in, please).
-- The 'Either' monad is used for error handling.
runFusionGatherM :: Prog -> FusionGM a -> FusionGEnv -> Either EnablingOptError a
runFusionGatherM prog (FusionGM a) =
    runReaderT (evalStateT a (newNameSourceForProg prog))

badFusionGM :: EnablingOptError -> FusionGM a
badFusionGM = FusionGM . lift . lift . Left

-- | Return a fresh, unique name.  The @VName@ is prepended to the
-- name.
new :: String -> FusionGM VName
new = state . flip newVName

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

fuseProg :: Prog -> Either EnablingOptError (Bool, Prog) -- (M.Map VName FusedRes)
fuseProg prog = do
    let env = FusionGEnv { soacs = M.empty, arrsInScope = S.empty, fusedRes = mkFreshFusionRes, program = prog }
    let funs= progFunctions prog
    ks <- runFusionGatherM prog (mapM fusionGatherFun funs) env
    let ks'    = map cleanFusionResult ks
    -- let succc = ( foldl (||) False . map (\x-> rsucc x) )   ks
    let succc = L.any rsucc ks'
    if not succc
    then return (False, prog)
    else do funs' <- runFusionGatherM prog (mapM fuseInFun (zip ks' funs)) env
            return (True, Prog funs')
{-- INCORRECT -- some things are left unfused!!!
            ks'   <- runFusionGatherM prog (mapM fuseInKerResLams ks)      env
            funs' <- runFusionGatherM prog (mapM fuseInFun (zip ks' funs)) env
            return (True, Prog funs')
    where
        fuseInKerResLams :: FusedRes -> FusionGM FusedRes
        fuseInKerResLams res = do
            kernels' <- bindRes res $
                           mapM (\(knm,ker) -> do let (oid, soac) = fsoac ker
                                                  soac' <- oneStepLam soac
                                                  return $ (knm, ker { fsoac = (oid, soac') })
                                ) (M.toList $ kernels res)
            return $ res { kernels = M.fromList kernels' }
        oneStepLam :: Exp -> FusionGM Exp
        oneStepLam fused_soac = do
            lam  <- getLamSOAC fused_soac
            lam' <- fuseInLambda lam
            nmsrc <- get
            case normCopyOneLambda prog nmsrc lam' of
                Left err             -> badFusionGM err
                Right (nmsrc', lam'') -> do
                    put nmsrc'
                    (_, nfres) <- fusionGatherLam (S.empty, mkFreshFusionRes) lam''
                    let nfres' =  cleanFusionResult nfres
                    lam'''     <- bindRes nfres' $ fuseInLambda lam''
                    updateLamSOAC lam''' fused_soac
--}


fusionGatherFun :: FunDec -> FusionGM FusedRes
fusionGatherFun (_, _, _, body, _) = fusionGatherExp mkFreshFusionRes body


fuseInFun :: (FusedRes, FunDec) -> FusionGM FunDec
fuseInFun (res, (fnm, rtp, idds, body, pos)) = do
    body' <- bindRes res $ fuseInExp body
    return (fnm, rtp, idds, body', pos)


---------------------------------------------------
---------------------------------------------------
---- RESULT's Data Structure
---------------------------------------------------
---------------------------------------------------

data FusedKer = FusedKer {
                  fsoac      :: (TupIdent, Exp)
                -- ^ the fused SOAC statement, e.g.,
                -- (z,w) = map2( f(a,b), x, y )

                , inp        :: S.Set Ident
                -- ^ the input arrays used in the `soac'
                -- stmt, i.e., `x', `y'.

                --, used       :: S.Set Ident
                -- -- ^ the set of variables used in
                -- -- the closure of the `soac' stmt, i.e., `a', `b'.

                , inplace    :: S.Set VName
                -- ^ every kernel maintains a set of variables
                -- that alias vars used in in-place updates,
                -- such that fusion is prevented to move
                -- a use of an

                , fusedVars :: [Ident]
                -- ^ whether at least a fusion has been performed.
                }

data FusedRes = FusedRes {
    rsucc :: Bool
  -- ^ Whether we have fused something anywhere.

  , outArr     :: M.Map VName VName
  -- ^ associates an array to the name of the
  -- SOAC kernel that has produced it.

  , inpArr     :: M.Map VName (S.Set VName)
  -- ^ associates an array to the names of the
  -- SOAC kernels that uses it. These sets include
  -- only the SOAC input arrays used as full variables, i.e., no `a[i]'.

  , unfusable  :: S.Set VName
  -- ^ the (names of) arrays that are not fusable, i.e.,
  --  (i) they are either used other than input to SOAC kernels, or
  -- (ii) are used as input to at least two different kernels that
  --      are not located on disjoint control-flow branches, or
  --(iii) are used in the lambda expression of SOACs

  , kernels    :: M.Map VName FusedKer
  -- ^ The hashtable recording the uses
  }

isOmapKer :: FusedKer -> Bool
isOmapKer ker =
    let (_, soac) = fsoac ker
    in case soac of
        Reduce2 {} -> True
        Redomap2{} -> True
        Map2    {} -> True
        _          -> False


isInpArrInResModKers :: FusedRes -> S.Set VName -> VName -> Bool
isInpArrInResModKers ress kers nm =
    case M.lookup nm (inpArr ress) of
        Nothing -> False
        Just s  -> not (S.null $ s `S.difference` kers)

getKersWithInpArrs :: FusedRes -> [VName] -> S.Set VName
getKersWithInpArrs ress =
    foldl (\s nm -> case M.lookup nm (inpArr ress) of
                        Nothing -> s
                        Just ns -> s `S.union` ns
          ) S.empty

-- | extend the set of names to include all the names
--     produced via SOACs (by querring the vtable's soac)
expandSoacInpArr :: [VName] -> FusionGM [VName]
expandSoacInpArr =
    foldM (\y nm -> do bnd <- asks $ M.lookup nm . soacs
                       case bnd of
                        Nothing     -> return (y++[nm])
                        Just (nns,_)-> return (y++nns )
          ) []

----------------------------------------------------------------------
----------------------------------------------------------------------

addNewKer :: FusedRes -> (TupIdent, Exp) -> FusionGM FusedRes
addNewKer res (idd, soac) = do
    (inp_idds, other_idds) <- getInpArrSOAC soac >>= getIdentArr
    let (inp_nms0,other_nms0) = (map identName inp_idds, map identName other_idds)
    inp_nms   <- expandSoacInpArr   inp_nms0
    other_nms <- expandSoacInpArr other_nms0

    let used_inps = filter (isInpArrInResModKers res S.empty) inp_nms
    let ufs' =  unfusable res        `S.union`
                S.fromList used_inps `S.union`
                S.fromList other_nms

    let new_ker = FusedKer (idd, soac) (S.fromList inp_idds) S.empty []
    nm_ker  <- new "ker"

    let out_nms = map identName (getIdents idd)

    let os' = foldl (\x arr -> M.insert arr nm_ker x)
                    (outArr res) out_nms

    let is' = foldl (\x arr -> M.insertWith' S.union arr (S.singleton nm_ker) x)
                    (inpArr res) inp_nms0

    return $ FusedRes (rsucc res) os' is' ufs'
                      (M.insert nm_ker new_ker (kernels res))


-- map, reduce, redomap
greedyFuse:: Bool -> S.Set VName -> FusedRes -> (TupIdent, Exp) -> FusionGM FusedRes
greedyFuse is_repl lam_used_nms res (idd, soac) = do
    -- Assumtion: the free vars in lambda are already in `unfusable'

    -- E.g., with `map2(f, a, b[i])', `a' belongs to `inp_idds' and
    --        `b' belongs to `other_idds'
    (inp_idds, other_idds) <- getInpArrSOAC soac >>= getIdentArr
    let (inp_nms0,other_nms0) = (map identName inp_idds, map identName other_idds)
    inp_nms   <- expandSoacInpArr   inp_nms0
    other_nms <- expandSoacInpArr other_nms0

    let out_idds     = getIdents idd
    let out_nms      = map identName out_idds
    -- Conditions for fusion:
    --   (i) none of `out_idds' belongs to the unfusable set, i.e., `ufs'
    --  (ii) there are some kernels that use some of `out_idds' as inputs
    let not_unfusable  = is_repl || all (notUnfusable res) out_nms
    let to_fuse_knmSet = getKersWithInpArrs res out_nms
    let to_fuse_knms   = S.toList to_fuse_knmSet
    to_fuse_kers <- mapM (\x-> case M.lookup x (kernels res) of
                                Nothing -> badFusionGM $ EnablingOptError (srclocOf soac)
                                                          ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                                           ++ "kernel name not found in kernels field!")
                                Just ker-> return ker
                         ) to_fuse_knms

    -- all kernels has to be compatible for fusion, e.g., if
    -- the kernel is a map, and the current soac is a filter,
    -- then they cannot be fused
    ok_kers_compat <- foldM (\b ker -> do b1 <- isCompatibleKer (out_nms,soac) ker
                                          return (b1 && b)
                            ) True to_fuse_kers

    -- check whether fusing @soac@ will violate any in-place update
    --    restriction, e.g., would move an input array past its in-place update.
    let all_used_names = foldl (flip S.insert) lam_used_nms (inp_nms++other_nms)

    --let kers_cap = map (\k-> S.intersection (inplace k) all_used_names) to_fuse_kers
    --let ok_inplace = L.all S.null kers_cap -- foldl (&&) True (map (\s -> S.null s) kers_cap)
    -- Replace with:
    let ok_inplace = L.all (\ker-> not $ L.any (\x->S.member x $ inplace ker) (S.toList all_used_names)) to_fuse_kers


    -- compute whether @soac@ is fusable or not
    let is_fusable = not_unfusable && not (null to_fuse_kers) && ok_inplace && ok_kers_compat

    -- DEBUG STMT (delete it!)
    --to_fuse_kers' <- trace ("ker: "++ppTupId idd++" fusable?: "++show is_fusable++" to_fuse_kers_num: "++show (length to_fuse_kers) ++ " inpArrs keys: " ++ concatMap nameToString (M.keys (inpArr res))) (return to_fuse_kers)

    --  (i) inparr ids other than vars will be added to unfusable list,
    -- (ii) will also become part of the unfusable set the inparr vars
    --         that also appear as inparr of another kernel,
    --         BUT which said kernel is not the one we are fusing with (now)!
    let mod_kerS  = if is_fusable then to_fuse_knmSet else S.empty
    let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
    let ufs'      = unfusable res `S.union` S.fromList used_inps `S.union` S.fromList other_nms

    if not is_fusable && is_repl
    then return res
    else
     if not is_fusable
     then do -- nothing to fuse, add a new soac kernel to the result
            let new_ker = FusedKer (idd, soac) (S.fromList inp_idds) S.empty []
            nm_ker  <- new "ker"
            let os' = foldl (\x arr -> M.insert arr nm_ker x)
                            (outArr res) out_nms
            let is' = foldl (\x arr -> M.insertWith' S.union arr (S.singleton nm_ker) x)
                            (inpArr res) inp_nms0
            return $ FusedRes (rsucc res) os' is' ufs'
                              (M.insert nm_ker new_ker (kernels res))
     else do -- ... fuse current soac into to_fuse_kers ...
            fused_kers <- mapM (fuseSOACwithKer (out_idds, soac)) to_fuse_kers
            -- Need to suitably update `inpArr':
            --   (i) first remove the inpArr bindings of the old kernel
            --  (ii) then add the inpArr bindings of the new kernel
            let inpArr' = foldl (\ inpa (kold, knew, knm)->
                                    let inpa' =
                                         foldl (\ inpp nm->case M.lookup nm inpp of
                                                             Nothing -> inpp
                                                             Just s  -> let new_set = S.delete knm s
                                                                        in if S.null new_set
                                                                           then M.delete nm         inpp
                                                                           else M.insert nm new_set inpp
                                               )
                                               inpa (map identName (S.toList (inp kold)))
                                    in foldl   (\ inpp nm->case M.lookup nm inpp of
                                                             Nothing -> M.insert nm (S.singleton knm) inpp
                                                             Just s  -> M.insert nm (S.insert  knm s) inpp
                                               )
                                               inpa' (map identName (S.toList (inp knew)))
                                )
                                (inpArr res) (zip3 to_fuse_kers fused_kers to_fuse_knms)
            -- Update the kernels map
            let kernels' = foldl (\ kers (knew, knm) -> M.insert knm knew kers )
                                 (kernels res) (zip fused_kers to_fuse_knms)

            -- nothing to do for `outArr' (since we have not added a new kernel)
            return $ FusedRes True (outArr res) inpArr' ufs' kernels'
    where
        notUnfusable :: FusedRes -> VName -> Bool
        notUnfusable ress nm = not $ S.member nm $ unfusable ress

fuseSOACwithKer :: ([Ident], Exp) -> FusedKer -> FusionGM FusedKer
fuseSOACwithKer (out_ids1, soac1) ker = do
  let (out_ids2, soac2) = fsoac ker
  cs1      <- getCertsSOAC soac1
  cs2      <- getCertsSOAC soac2
  case (soac2, soac1) of
      -- first get rid of the cases that can be solved by
      -- a bit of soac rewriting.
    (Reduce2 _ lam ne arrs rwtps loc, Map2   {}) -> do
      let soac2' = Redomap2 (cs1++cs2) lam lam ne arrs rwtps loc
          ker'   = ker { fsoac = (out_ids2, soac2') }
      fuseSOACwithKer (out_ids1, soac1) ker'
    _ -> do -- treat the complicated cases!
            inp1_arr <- getInpArrSOAC soac1
            inp2_arr <- getInpArrSOAC soac2

            lam1     <- getLamSOAC soac1
            lam2     <- getLamSOAC soac2

            (res_soac, res_inp) <-
              case (soac2,soac1) of
                ----------------------------------------------------
                -- The Fusions that are semantically map fusions:
                ----------------------------------------------------
                (Map2 _ _ _ _ pos, Map2    {}) -> do
                  let (res_lam, new_inp) = fuseMaps lam1 inp1_arr out_ids1 lam2 inp2_arr
                  return (Map2 (cs1++cs2) res_lam new_inp (mkElType new_inp) pos, new_inp)
                (Redomap2 _ lam21 _ ne _ _ pos, Map2 {})-> do
                  let (res_lam, new_inp) = fuseMaps lam1 inp1_arr out_ids1 lam2 inp2_arr
                  return (Redomap2 (cs1++cs2) lam21 res_lam ne new_inp (mkElType new_inp) pos, new_inp)

                ----------------------------------------------------
                -- The Fusions that are semantically filter fusions:
                ----------------------------------------------------
                (Reduce2 _ _ ne _ eltp pos, Filter2 {}) -> do
                  (res_lam, new_inp) <- fuseRedFilt inp1_arr lam1 out_ids1 inp2_arr lam2
                  return (Reduce2 (cs1++cs2) res_lam ne new_inp eltp pos, new_inp)
                (Redomap2 _ lam21 _ nes _ eltp pos, Filter2 {}) -> do
                  name <- new "check"
                  let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr out_ids1 lam2 inp2_arr name
                  return (Redomap2 (cs1++cs2) lam21 res_lam nes new_inp eltp pos, new_inp)
                (Filter2 _ _ _ pos, Filter2 _ _ _ pos1) -> do
                  (res_lam, new_inp) <- fuseMapFilt inp1_arr lam1 (Literal (LogVal False) pos1) inp2_arr lam2
                  return (Filter2 (cs1++cs2) res_lam new_inp pos, new_inp)

                ----------------------------------------------------
                -- Unfusable: should not have reached here!!!
                ----------------------------------------------------
                _ -> badFusionGM $ EnablingOptError (srclocOf soac1)
                                    ("In Fusion.hs, fuseSOACwithKer: fusion not supported "
                                     ++ "(soac2,soac1): (" ++ ppExp soac2++", "++ppExp soac1)
            -- END CASE(soac2,soac1)

            let inp_new = foldl (\lst e -> case e of
                                            Var idd -> lst++[idd]
                                            _       -> lst
                                ) [] res_inp

            -- soac_new1<- trace ("\nFUSED KERNEL: " ++ (nameToString . identName) (head out_ids1) ++ " : " ++ ppExp res_soac ++ " Input arrs: " ++ concatMap (nameToString . identName) inp_new) (return res_soac)

            let fusedVars_new = fusedVars ker++out_ids1
            return $ FusedKer (out_ids2, res_soac) (S.fromList inp_new) (inplace ker) fusedVars_new

    where mkElType  = map (stripArray 1 . typeOf)



------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
--- Fusion Gather for EXPRESSIONS, i.e., where work is being done:   ---
---    i) bottom-up AbSyn traversal (backward analysis)              ---
---   ii) soacs are fused greedily iff does not duplicate computation---
--- E.g., (y1, y2, y3) = map2(f, x1, x2[i])                          ---
---       (z1, z2)     = map2(g1, y1, y2)                            ---
---       (q1, q2)     = map2(g2, y3, z1, a, y3)                     ---
---       res          = reduce(op, ne, q1, q2, z2, y1, y3)          ---
--- can be fused if y1,y2,y3, z1,z2, q1,q2 are not used elsewhere:   ---
---       res = redomap(op, \(x1,x2i,a)->                            ---
---                             let (y1,y2,y3) = f (x1, x2i)       in---
---                             let (z1,z2)    = g1(y1, y2)        in---
---                             let (q1,q2)    = g2(y3, z1, a, y3) in---
---                             (q1, q2, z2, y1, y3)                 ---
---                     x1, x2[i], a)                                ---
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
fusionGatherExp :: FusedRes -> Exp -> FusionGM FusedRes

----------------------------------------------
--- Let-Pattern: most important construct
----------------------------------------------

-- bnd <- asks $ M.lookup (identName src) . envVtable
-- body' <- binding bnds $ tupleNormExp  body


fusionGatherExp fres (LetPat pat soac@(Map2 _ lam _ _ _) body _) = do
    bres  <- bindPat pat soac $ fusionGatherExp fres body
    (used_lam, blres) <- fusionGatherLam (S.empty, bres) lam
    greedyFuse False used_lam blres (pat, soac)

fusionGatherExp fres (LetPat pat soac@(Replicate n el pos) body _) = do
    bres <- bindPat pat soac $ fusionGatherExp fres body
    -- Implemented inplace: gets the variables in `n` and `el`
    (used_set, bres')<- getUnfusableSet pos bres [n,el]
    repl_idnm <- new "repl_x"
    let repl_id = Ident repl_idnm (Elem Int) pos
        (lame, rwt) = case typeOf el of
                        Elem (Tuple ets) -> (el, ets)
                        t                -> (TupLit [el] pos, [t])
        repl_lam = TupleLambda [toParam repl_id] lame (map toDecl rwt) pos
        soac_repl= Map2 [] repl_lam [Iota n pos] rwt pos
    greedyFuse True used_set bres' (pat, soac_repl)

fusionGatherExp fres (LetPat pat soac@(Filter2 _ lam _ _) body _) = do
    bres  <- bindPat pat soac $ fusionGatherExp fres body
    (used_lam, blres) <- fusionGatherLam (S.empty, bres) lam
    greedyFuse False used_lam blres (pat, soac)
    --bres' <- foldM fusionGatherExp bres arrs
    --(_, lres) <- fusionGatherLam (S.empty, bres') lam
    --return lres

fusionGatherExp fres (LetPat pat soac@(Reduce2 _ lam nes _ _ loc) body _) = do
    -- a reduce always starts a new kernel
    bres  <- bindPat pat soac $ fusionGatherExp fres body
    bres' <- fusionGatherExp bres $ TupLit nes loc
    (_, blres) <- fusionGatherLam (S.empty, bres') lam
    addNewKer blres (pat, soac)

fusionGatherExp fres (LetPat pat soac@(Redomap2 _ outer_red inner_red ne _ _ loc) body _) = do
    -- a redomap always starts a new kernel
    (_, lres)  <- foldM fusionGatherLam (S.empty, fres) [outer_red, inner_red]
    bres  <- bindPat pat soac $ fusionGatherExp lres body -- binding pat $
    bres' <- fusionGatherExp bres $ TupLit ne loc
    addNewKer bres' (pat, soac)

fusionGatherExp fres (LetPat pat (Scan2 _ lam nes arrs _ _) body _) = do
    -- NOT FUSABLE
    (_, lres)  <- fusionGatherLam (S.empty, fres) lam
    bres  <- binding pat $ fusionGatherExp lres body
    foldM fusionGatherExp bres (nes++arrs)

fusionGatherExp fres (LetPat pat e body _) = do
    let pat_vars = map Var $ getIdents pat
    bres <- binding pat $ fusionGatherExp fres body
    foldM fusionGatherExp bres (e:pat_vars)


-----------------------------------------
---- Var/Index/LetWith/Do-Loop/If    ----
-----------------------------------------

fusionGatherExp fres (Var idd) =
    -- IF idd is an array THEN ADD it to the unfusable set!
    case identType idd of
        Array{} -> return fres { unfusable = S.insert (identName idd) (unfusable fres) }
        _       -> return fres

fusionGatherExp fres (Index _ idd _ inds _ _) =
    foldM fusionGatherExp fres (Var idd : inds)

fusionGatherExp fres (LetWith _ id1 id0 inds elm body _) = do
    bres  <- binding (Id id1) $ fusionGatherExp fres body

    let pat_vars = [Var id0, Var id1]
    fres' <- foldM fusionGatherExp bres (elm : inds ++ pat_vars)
    let (ker_nms, kers) = unzip $ M.toList $ kernels fres'

    -- Now add the aliases of id0 (itself included) to the `inplace' field
    --     of any existent kernel. ToDo: extend the list to contain the
    --     whole set of aliases (not available now!)
    let inplace_aliases = S.toList $ aliases $ typeOf $ Var id0
    let kers' = map (\ k -> let inplace' = foldl (flip S.insert) (inplace k) inplace_aliases
                            in  k { inplace = inplace' }
                    ) kers
    let new_kernels = M.fromList $ zip ker_nms kers'
    return $ fres' { kernels = new_kernels }

fusionGatherExp fres (DoLoop merge_pat ini_val _ ub loop_body let_body _) = do
    letbres <- binding merge_pat $ fusionGatherExp fres let_body

    let pat_vars = map Var (getIdents merge_pat)
    fres' <- foldM fusionGatherExp letbres (ini_val:ub:pat_vars)

    let null_res = mkFreshFusionRes
    new_res <- binding merge_pat $ fusionGatherExp null_res loop_body
    -- make the inpArr unfusable, so that they
    -- cannot be fused from outside the loop:
    let (inp_arrs, _) = unzip $ M.toList $ inpArr new_res
    -- let unfus = (unfusable new_res) `S.union` (S.fromList inp_arrs)
    let new_res' = new_res { unfusable = foldl (flip S.insert) (unfusable new_res) inp_arrs }
    -- merge new_res with fres'
    return $ unionFusionRes new_res' fres'


-- bnds <- asks $ arrsInScope

fusionGatherExp fres (If cond e_then e_else _ _) = do
    let null_res = mkFreshFusionRes
    then_res <- fusionGatherExp null_res e_then
    else_res <- fusionGatherExp null_res e_else
    let both_res = unionFusionRes then_res else_res
    fres'    <- fusionGatherExp fres cond
    mergeFusionRes fres' both_res

------WRONG IMPLEM
--    let null_res = mkFreshFusionRes
--    then_res <- fusionGatherExp null_res e_then
--    else_res <- fusionGatherExp null_res e_else
--    fres'    <- fusionGatherExp fres cond
--    return $ unionFusionRes fres' (unionFusionRes then_res else_res)

-----------------------------------------------------------------------------------
--- Errors: all SOACs, both the regular ones (because they cannot appear in prg)---
---         and the 2 ones (because normalization ensures they appear directly  ---
---         in let exp, i.e., let x = e)
-----------------------------------------------------------------------------------

fusionGatherExp _ (Map      _ _ _     pos) = errorIllegal "map"     pos
fusionGatherExp _ (Reduce   _ _ _ _   pos) = errorIllegal "reduce"  pos
fusionGatherExp _ (Scan     _ _ _ _   pos) = errorIllegal "scan"    pos
fusionGatherExp _ (Filter   _ _ _     pos) = errorIllegal "filter"  pos
fusionGatherExp _ (Redomap  _ _ _ _ _ pos) = errorIllegal "redomap"  pos

fusionGatherExp _ (Map2     _ _ _ _     pos) = errorIllegal "map2"    pos
fusionGatherExp _ (Reduce2  _ _ _ _ _   pos) = errorIllegal "reduce2" pos
fusionGatherExp _ (Scan2    _ _ _ _ _   pos) = errorIllegal "scan2"   pos
fusionGatherExp _ (Filter2  _ _ _       pos) = errorIllegal "filter2" pos
fusionGatherExp _ (Redomap2 _ _ _ _ _ _ pos) = errorIllegal "redomap2" pos

-----------------------------------
---- Generic Traversal         ----
-----------------------------------

fusionGatherExp fres e = do
    let foldstct = identityFolder { foldOnExp = fusionGatherExp }
    foldExpM foldstct fres e

----------------------------------------------
--- Lambdas create a new scope. Disalow fusing
---   from outside lambda by adding inp_arrs
---   to the unfusable set.
----------------------------------------------

fusionGatherLam :: (S.Set VName, FusedRes) -> TupleLambda -> FusionGM (S.Set VName, FusedRes)
fusionGatherLam (u_set,fres) (TupleLambda idds body _ pos) = do
    let null_res = mkFreshFusionRes
    new_res <- binding (TupId (map (Id . fromParam) idds) pos) $ fusionGatherExp null_res body
    -- make the inpArr unfusable, so that they
    -- cannot be fused from outside the lambda:
    let inp_arrs = S.fromList $ M.keys (inpArr new_res) -- unzip $ M.toList $ inpArr new_res
    let unfus' = unfusable new_res `S.union` inp_arrs
    -- foldl (\r x -> S.insert x r) (unfusable new_res) inp_arrs
    bnds <- asks arrsInScope
    let unfus  = unfus' `S.intersection` bnds
    -- merge fres with new_res'
    let new_res' = new_res { unfusable = unfus }
    -- merge new_res with fres'
    return (u_set `S.union` unfus, unionFusionRes new_res' fres)

getUnfusableSet :: SrcLoc -> FusedRes -> [Exp] -> FusionGM (S.Set VName, FusedRes)
getUnfusableSet pos fres args = do
    -- assuming program is normalized then args
    -- can only contribute to the unfusable set
    let null_res = mkFreshFusionRes
    new_res <- foldM fusionGatherExp null_res args
    if not (M.null (outArr  new_res)) || not (M.null (inpArr new_res)) ||
       not (M.null (kernels new_res)) || rsucc new_res
    then badFusionGM $ EnablingOptError pos $
                        "In Fusion.hs, getUnfusableSet, broken invariant!"
                        ++ " Unnormalized program: " ++ concatMap ppExp args
    else return ( unfusable new_res,
                  fres { unfusable = unfusable fres `S.union` unfusable new_res }
                )

-------------------------------------------------------------
-------------------------------------------------------------
--- FINALLY, Substitute the kernels in function
-------------------------------------------------------------
-------------------------------------------------------------

fuseInExp :: Exp -> FusionGM Exp

----------------------------------------------
--- Let-Pattern: most important construct
----------------------------------------------

fuseInExp (LetPat pat soac@(Map2 {}) body pos) = do
    body' <- fuseInExp body
    soac' <- replaceSOAC pat soac
    return $ LetPat pat soac' body' pos

fuseInExp (LetPat pat soac@(Filter2 {}) body pos) = do
    body' <- fuseInExp body
    soac' <- replaceSOAC pat soac
    return $ LetPat pat soac' body' pos

fuseInExp (LetPat pat soac@(Reduce2 {}) body pos) = do
    body' <- fuseInExp body
    soac' <- replaceSOAC pat soac
    return $ LetPat pat soac' body' pos

fuseInExp (LetPat pat soac@(Redomap2 {}) body pos) = do
    body' <- fuseInExp body
    soac' <- replaceSOAC pat soac
    return $ LetPat pat soac' body' pos

fuseInExp (LetPat pat soac@(Scan2 {}) body pos) = do
    -- NOT FUSABLE
    body' <- fuseInExp body
    soac' <- fuseInExp soac
    return $ LetPat pat soac' body' pos

fuseInExp (LetPat pat e body pos) = do
    body' <- fuseInExp body
    e'    <- fuseInExp e
    return $ LetPat pat e' body' pos


-----------------------------------------------------------------------------------
--- Errors: all SOACs, both the regular ones (because they cannot appear in prg)---
---         and the 2 ones (because normalization ensures they appear directly  ---
---         in let exp, i.e., let x = e)
-----------------------------------------------------------------------------------

fuseInExp (Map      _ _ _     pos) = errorIllegalFus "map"     pos
fuseInExp (Reduce   _ _ _ _   pos) = errorIllegalFus "reduce"  pos
fuseInExp (Scan     _ _ _ _   pos) = errorIllegalFus "scan"    pos
fuseInExp (Filter   _ _ _     pos) = errorIllegalFus "filter"  pos
fuseInExp (Redomap  _ _ _ _ _ pos) = errorIllegalFus "redomap"  pos
{-
fuseInExp (Map2     _ _ _     pos) = errorIllegalFus "map2"    pos
fuseInExp (Reduce2  _ _ _ _   pos) = errorIllegalFus "reduce2" pos
fuseInExp (Scan2    _ _ _ _   pos) = errorIllegalFus "scan2"   pos
fuseInExp (Filter2  _ _       pos) = errorIllegalFus "filter2" pos
fuseInExp (Redomap2 _ _ _ _ _ pos) = errorIllegalFus "redomap2" pos
-}

-------------------------------------------------------
-------------------------------------------------------
---- Pattern Match The Rest of the Implementation! ----
----          NOT USED !!!!!                       ----
-------------------------------------------------------
-------------------------------------------------------


fuseInExp e = mapExpM fuseIn e
  where fuseIn = identityMapper {
                   mapOnExp         = fuseInExp
                 , mapOnTupleLambda = fuseInLambda
                 }

fuseInLambda :: TupleLambda -> FusionGM TupleLambda
fuseInLambda (TupleLambda params body rtp pos) = do
  body' <- fuseInExp body
  return $ TupleLambda params body' rtp pos

--fuseInExp e = do return e


replaceSOAC :: TupIdent -> Exp -> FusionGM Exp
replaceSOAC pat soac = do
    fres  <- asks fusedRes
    let pos     = srclocOf soac
    let pat_nm  = identName $ head $ getIdents pat
    case M.lookup pat_nm (outArr fres) of
        Nothing -> fuseInExp soac
        Just knm-> case M.lookup knm (kernels fres) of
                    Nothing -> badFusionGM $ EnablingOptError pos
                                                ("In Fusion.hs, replaceSOAC, outArr in ker_name "
                                                 ++"which is not in Res: "++textual knm)
                    Just ker-> do
                        let (pat', new_soac) = fsoac ker
                        if pat /= pat'
                        then badFusionGM $ EnablingOptError pos
                                            ("In Fusion.hs, replaceSOAC, "
                                             ++" pat does not match kernel's pat: "++ppTupId pat)
                        else if L.null $ fusedVars ker
                             then badFusionGM $ EnablingOptError pos
                                                 ("In Fusion.hs, replaceSOAC, unfused kernel "
                                                  ++"still in result: "++ppTupId pat)
                             -- then fuseInExp soac
                             else do -- return new_soac
                                     -- TRY MOVE THIS TO OUTER LEVEL!!!
                                     lam   <- getLamSOAC new_soac
                                     nmsrc <- get
                                     prog  <- asks program
                                     case normCopyOneTupleLambda prog nmsrc lam of
                                        Left err             -> badFusionGM err
                                        Right (nmsrc', lam') -> do
                                            put nmsrc'
                                            (_, nfres) <- fusionGatherLam (S.empty, mkFreshFusionRes) lam'
                                            let nfres' =  cleanFusionResult nfres
                                            lam''      <- bindRes nfres' $ fuseInLambda lam'
                                            updateLamSOAC lam'' new_soac
---------------------------------------------------
---------------------------------------------------
---- HELPERS
---------------------------------------------------
---------------------------------------------------

-- | Get a new fusion result, i.e., for when entering a new scope,
--   e.g., a new lambda or a new loop.
mkFreshFusionRes :: FusedRes
mkFreshFusionRes =
    FusedRes { rsucc     = False,   outArr = M.empty, inpArr  = M.empty,
               unfusable = S.empty, kernels = M.empty }

unionFusionRes :: FusedRes -> FusedRes -> FusedRes
unionFusionRes res1 res2 =
    FusedRes  (rsucc     res1       ||      rsucc     res2)
              (outArr    res1    `M.union`  outArr    res2)
              (M.unionWith S.union (inpArr res1) (inpArr res2) )
              (unfusable res1    `S.union`  unfusable res2)
              (kernels   res1    `M.union`  kernels   res2)

mergeFusionRes :: FusedRes -> FusedRes -> FusionGM FusedRes
mergeFusionRes res1 res2 = do
    let ufus_mres = unfusable res1 `S.union` unfusable res2
    inp_both     <- expandSoacInpArr $ M.keys $ inpArr res1 `M.intersection` inpArr res2
    let m_unfus   = foldl (flip S.insert) ufus_mres inp_both
    return $ FusedRes  (rsucc     res1       ||      rsucc     res2)
                       (outArr    res1    `M.union`  outArr    res2)
                       (M.unionWith S.union (inpArr res1) (inpArr res2) )
                       m_unfus
                       (kernels   res1    `M.union`  kernels   res2)


-- | Returns the list of identifiers of a pattern.
getIdents :: TupIdent -> [Ident]
getIdents (Wildcard _ _) = []
getIdents (Id idd)       = [idd]
getIdents (TupId tis _)  = concatMap getIdents tis

getLamSOAC :: Exp -> FusionGM TupleLambda
getLamSOAC (Map2     _ lam _    _ _    ) = return lam
getLamSOAC (Reduce2  _ lam _    _ _ _  ) = return lam
getLamSOAC (Filter2  _ lam _    _      ) = return lam
getLamSOAC (Redomap2 _ _   lam2 _ _ _ _) = return lam2
getLamSOAC ee = badFusionGM $ EnablingOptError
                                (srclocOf ee)
                                ("In Fusion.hs, getLamSOAC, broken invariant: "
                                 ++" argument not a SOAC! "++ppExp ee)

updateLamSOAC :: TupleLambda -> Exp -> FusionGM Exp
updateLamSOAC lam (Map2     cs      _    arrs eltp loc) =
  return $ Map2     cs          lam    arrs eltp loc
updateLamSOAC lam (Reduce2  cs      _ ne arrs eltp loc) =
  return $ Reduce2  cs      lam ne arrs eltp loc
updateLamSOAC lam (Filter2  cs      _    arrs      loc) =
  return $ Filter2  cs      lam    arrs      loc
updateLamSOAC lam (Redomap2 cs lam1 _ ne arrs eltp loc) =
  return $ Redomap2 cs lam1 lam ne arrs eltp loc
updateLamSOAC _ ee = badFusionGM $ EnablingOptError
                                    (srclocOf ee)
                                    ("In Fusion.hs, updateLamSOAC, broken invariant: "
                                     ++" argument not a SOAC! "++ppExp ee)

-- | Returns the input arrays used in a SOAC.
--     If exp is not a SOAC then error.
getInpArrSOAC :: Exp -> FusionGM [Exp]
getInpArrSOAC (Map2 _     _     arrs _ _) = return arrs
getInpArrSOAC (Reduce2  _ _ _   arrs _ _) = return arrs
getInpArrSOAC (Filter2  _ _     arrs _  ) = return arrs
getInpArrSOAC (Redomap2 _ _ _ _ arrs _ _) = return arrs
getInpArrSOAC ee = badFusionGM $ EnablingOptError
                                (srclocOf ee)
                                ("In Fusion.hs, getInpArrSOAC, broken invariant: "
                                 ++" argument not a SOAC! "++ppExp ee)

-- | Returns the certificates used in a SOAC.
--     If exp is not a SOAC then error.
getCertsSOAC :: Exp -> FusionGM Certificates
getCertsSOAC (Map2     cs _     _ _ _) = return cs
getCertsSOAC (Reduce2  cs _ _   _ _ _) = return cs
getCertsSOAC (Filter2  cs _     _ _  ) = return cs
getCertsSOAC (Redomap2 cs _ _ _ _ _ _) = return cs
getCertsSOAC ee = badFusionGM $ EnablingOptError
                                (srclocOf ee)
                                ("In Fusion.hs, getCertsSOAC, broken invariant: "
                                 ++" argument not a SOAC! "++ppExp ee)

-- | The expression arguments are supposed to be array-type exps.
--   Returns a tuple, in which the arrays that are vars are in
--     the first element of the tuple, and the one which are
--     indexed should be in the second.
--     The normalization *SHOULD* ensure that input arrays in SOAC's
--     are only vars, indexed-vars, and iotas, hence error is raised
--     in all other cases.
-- E.g., for expression `map2(f, a, b[i])', the result should be `([a],[b])'
getIdentArr :: [Exp] -> FusionGM ([Ident], [Ident])
getIdentArr []             = return ([],[])
getIdentArr (Var idd:es) = do
    (vs, os) <- getIdentArr es
    case identType idd of
        Array {} -> return (idd:vs, os)
        _        -> -- return (vs, os)
            badFusionGM $ EnablingOptError
                            (srclocOf idd)
                            ("In Fusion.hs, getIdentArr, broken invariant: "
                             ++" argument not of array type! "++ppExp (Var idd))
getIdentArr (Index _ idd _ _ _ _:es) = do
    (vs, os) <- getIdentArr es
    case identType idd of
        Array {} -> return (vs, idd:os)
        _        ->
            badFusionGM $ EnablingOptError
                            (srclocOf idd)
                            ("In Fusion.hs, getIdentArr, broken invariant: "
                             ++" argument not of array type! "++ppExp (Var idd))
getIdentArr (Iota _ _:es) = getIdentArr es
getIdentArr (ee:_) =
    badFusionGM $ EnablingOptError
                    (srclocOf ee)
                    ("In Fusion.hs, getIdentArr, broken invariant: "
                     ++" argument not a (indexed) variable! "++ppExp ee)

cleanFusionResult :: FusedRes -> FusedRes
cleanFusionResult fres =
    let newks = M.filter (not . null . fusedVars)      (kernels fres)
        newoa = M.filter (`M.member` newks)            (outArr  fres)
        newia = M.map    (S.filter (`M.member` newks)) (inpArr fres)
    in fres { outArr = newoa, inpArr = newia, kernels = newks }


--------------
--- Errors ---
--------------

errorIllegal :: String -> SrcLoc -> FusionGM FusedRes
errorIllegal soac_name pos =
    badFusionGM $ EnablingOptError pos
                  ("In Fusion.hs, soac "++soac_name++" appears illegaly in pgm!")

errorIllegalFus :: String -> SrcLoc -> FusionGM Exp
errorIllegalFus soac_name pos =
    badFusionGM $ EnablingOptError pos
                  ("In Fusion.hs, soac "++soac_name++" appears illegaly in pgm!")

--------------------------------------------
--------------------------------------------
--- FUSING 2 COMPATIBLE SOACs:           ---
--------------------------------------------
--------------------------------------------


-- | Assumption: the order and number of elements matches between
--       the input to the Reduce and the input to Filter,
--       i.e., already checked in @isCompatibleKer@
fuseRedFilt :: [Exp] -> TupleLambda -> [Ident]
            -> [Exp] -> TupleLambda
            -> FusionGM (TupleLambda, [Exp])
fuseRedFilt inp1 lam1 _ inp2 lam2 = do
    let rtp2 = tupleLambdaReturnType lam2
    let pos2 = srclocOf lam2
    let pos1 = srclocOf lam1
    let (ids2, bdy2 ) = getUnmdParsAndBody lam2
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1

    let n   = length inp2
    let nd2 = n `div` 2
    (ids21, ids22) <- if n `mod` 2 == 0 && nd2 == length par1 && n == length ids2
                      then return (take nd2 ids2, drop nd2 ids2)
                      else badFusionGM $ EnablingOptError pos2
                                          ("In Fusion.hs, fuseRedFilt: uneven lambda params "
                                           ++ "of lam2 (lam1): " ++ show n ++ " " ++ show (length par1))

    let then_pat = TupId (map Id ids22) pos2
    let then_var = TupLit (map Var par1 ) pos1
    let then_exp = LetPat then_pat then_var  bdy2 pos2
    let else_exp = TupLit (map Var ids21) pos2
    let res_body = If call1 then_exp else_exp (typeOf then_exp) pos1
    return (TupleLambda (map toParam $ ids21++par1) res_body rtp2 pos2, inp1)


-- | Assumption: the order and number of elements matches
--       between the map and the reduce,
--       i.e., already checked in @isCompatibleKer@.
fuseMapFilt :: [Exp] -> TupleLambda -> Exp
            -> [Exp] -> TupleLambda
            -> FusionGM (TupleLambda, [Exp])
fuseMapFilt inp1 lam1 ne inp2 lam2 = do
    let rtp2 = tupleLambdaReturnType lam2
    let pos2 = srclocOf lam2
    let pos1 = srclocOf lam1
    let (ids2, bdy2 ) = getUnmdParsAndBody lam2
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1

    _ <- if length ids2 == length par1 && length par1 == length inp1 && length inp2 == length inp1
         then return Nothing
         else badFusionGM $ EnablingOptError pos2
                             ("In Fusion.hs, fuse2Filter: num of params of "
                              ++ "lam2, lam1 and inp1 do not agree: " ++ show (length ids2)
                              ++ " " ++ show (length par1) ++ " " ++ show (length inp1))

    let then_pat = TupId (map Id ids2) pos2
    let then_var = TupLit (map Var par1) pos1
    let then_exp = LetPat then_pat then_var bdy2 pos2
    check <- Ident <$> new "check" <*> pure (Elem Bool) <*> pure pos2
    let res_branch = If (Var check) then_exp ne (typeOf ne) pos1
    let res_body = LetPat (TupId [Id check] pos2) call1 res_branch pos2
    return (TupleLambda (map toParam par1) res_body rtp2 pos2, inp1)

{-
-- | Assumption: the order and number of elements matches between the two filters,
--     i.e., already checked in @isCompatibleKer@
fuse2Filter :: [Exp] -> Lambda -> [Ident]
                -> [Exp] -> Lambda
                -> FusionGM (Lambda, [Exp])
fuse2Filter inp1 lam1 _ inp2 lam2 = do
    let pos2 = (SrcLoc (locOf lam2))
    let pos1 = (SrcLoc (locOf lam1))
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1
    (ids2, bdy2 ) <- getUnmdParsAndBody inp2 lam2
    _ <- if length ids2 == length par1 && length par1 == length inp1 && length inp2 == length inp1
         then return 1
         else badFusionGM $ EnablingOptError pos2
                             ("In Fusion.hs, fuse2Filter: num of params of "
                              ++ "lam2, lam1 and inp1 do not agree: " ++ show (length ids2)
                              ++ " " ++ show (length par1) ++ " " ++ show (length inp1))
    let then_pat = TupId  (map (\x->Id  x) ids2) pos2
    let then_var = TupLit (map (\x->Var x) par1) pos1
    let then_exp = LetPat then_pat then_var  bdy2 pos2
    let res_body = If call1 then_exp (Literal (LogVal False) pos1) (Elem Bool) pos1
    return (AnonymFun par1 res_body (Elem Bool) pos2, inp1)
-}

-- | Recieves a list of types @tps@, which should match
--     the lambda's parameters, and a lambda expression.
--   Returns a list of identifiers, corresponding to
--     the `expanded' argument of lambda, and the lambda call.
buildCall :: [Type] -> TupleLambda -> FusionGM ([Ident], Exp)
buildCall tps (TupleLambda lam_args bdy _ pos)    = do
    let (ids, bdy') = (lam_args, bdy) -- getLamNormIdsAndBody lam_args bdy
    let bres = and $ zipWith subtypeOf tps $ map (identType . fromParam) ids
    if bres then return (map fromParam ids, bdy')
    else badFusionGM $ EnablingOptError pos
                       "In Fusion.hs, buildCall, types do NOT match!"

getUnmdParsAndBody :: TupleLambda -> ([Ident], Exp)
getUnmdParsAndBody (TupleLambda lam_args2 body2 _ _) =
  (map fromParam lam_args2, body2)

isCompatibleKer :: ([VName], Exp) -> FusedKer -> FusionGM Bool
isCompatibleKer (_,       Map2    {}) ker = return $ isOmapKer   ker
isCompatibleKer (out_nms, Filter2 {}) ker = do
    let (_, soac) = fsoac ker
    let ok = case soac of
                Reduce2 {} -> True
                Redomap2{} -> True
                Filter2 {} -> True
                _          -> False
    if not ok
    then return False
    else do -- check that the input-array  set of soac is included
            --         in the output-array set of soac_filt
            (inp_idds2, other_idds2) <- getInpArrSOAC soac >>= getIdentArr
            let inp_lst = map identName inp_idds2
            return $ L.null other_idds2 && inp_lst == out_nms -- inp_set `S.isSubsetOf` S.fromList out_nms
isCompatibleKer _ _ = return False
