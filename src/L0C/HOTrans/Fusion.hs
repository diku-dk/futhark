{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.HOTrans.Fusion ( fuseProg )
  where
 
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
--import Control.Monad.Writer

--import Data.Data
--import Data.Generics


import Data.Loc

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

--import Language.L0
import L0C.L0
import L0C.FreshNames
import L0C.EnablingOpts.EnablingOptDriver

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
binding pat = local (`bindVars` (map identName (filter (\x->case identType x of
                                                                Array{} -> True
                                                                _       -> False) 
                                                       (getIdents pat)
                                               )
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
new = state . newVName

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

fuseProg :: Prog -> Either EnablingOptError (Bool, Prog) -- (M.Map VName FusedRes)
fuseProg prog = do
    let env = FusionGEnv { soacs = M.empty, arrsInScope = S.empty, fusedRes = mkFreshFusionRes, program = prog }
    let funs= progFunctions prog
    ks0 <- runFusionGatherM prog (mapM fusionGatherFun funs) env
    let ks    = map ( \funres -> cleanFusionResult funres ) ks0
    -- let succc = ( foldl (||) False . map (\x-> rsucc x) )   ks
    let succc = L.any (\x-> rsucc x) ks
    if not succc
    then do return (False, prog)
    else do funs' <- runFusionGatherM prog (mapM fuseInFun (zip ks funs)) env
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
fusionGatherFun (_, _, _, body, _) = do
    --body' <- trace ("in function: "++fname++"\n") (tupleNormAbstrFun args body pos)
    fusionGatherExp mkFreshFusionRes body


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
                -- ^ the set of variables used in
                -- the closure of the `soac' stmt, i.e., `a', `b'.

                --, asserts    :: M.Map Ident Exp
                -- ^ the Map of `assertZip' stmts that involve
                -- arrays that are input to the kernel.

                , inplace    :: S.Set VName
                -- ^ every kernel maintains a set of variables
                -- that alias vars used in in-place updates,
                -- such that fusion is prevented to move
                -- a use of an 

                , fused_vars :: [Ident]
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

  , zips       :: M.Map VName (Int, S.Set Exp)
  -- ^ a map between the name of the boolean variable
  -- that holds the result of an assertZip function call
  -- AND the expression represention of the corresponding 
  -- function call.

  , inzips     :: M.Map VName [VName]
  -- ^ a map between an array name in a zip and
  -- its corresponding boolean value (in zips)

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
getKersWithInpArrs ress nms = 
    foldl (\s nm -> case M.lookup nm (inpArr ress) of
                        Nothing -> s
                        Just ns -> s `S.union` ns
          ) S.empty nms

-- | extend the set of names to include all the names
--     produced via SOACs (by querring the vtable's soac)
expandSoacInpArr :: [VName] -> FusionGM [VName]
expandSoacInpArr nms = do
    foldM (\y nm -> do bnd <- asks $ M.lookup nm . soacs
                       case bnd of
                        Nothing     -> return (y++[nm])
                        Just (nns,_)-> return (y++nns )
          ) [] nms

----------------------------------------------------------------------
----------------------------------------------------------------------

addNewKer :: FusedRes -> (TupIdent, Exp) -> FusionGM FusedRes
addNewKer res (idd, soac) = do
    (inp_idds, other_idds) <- getInpArrSOAC soac >>= getIdentArr
    let (inp_nms0,other_nms0) = (map identName inp_idds, map identName other_idds)
    inp_nms   <- expandSoacInpArr   inp_nms0
    other_nms <- expandSoacInpArr other_nms0

    let used_inps = filter (isInpArrInResModKers res S.empty) inp_nms
    let ufs' =  (unfusable res)      `S.union` 
                S.fromList used_inps `S.union` 
                S.fromList other_nms

    let new_ker = FusedKer (idd, soac) (S.fromList inp_idds) S.empty []    
    nm_ker  <- new "ker"

    let out_nms = map identName (getIdents idd)

    let os' = foldl (\x arr -> M.insert arr nm_ker x) 
                    (outArr res) out_nms 

    let is' = foldl (\x arr -> M.insertWith' (\a b -> a `S.union` b) arr (S.singleton nm_ker) x) 
                            (inpArr res) inp_nms0

    return $ FusedRes (rsucc res) os' is' ufs' (zips res) (inzips res) 
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
    let not_unfusable  = is_repl || ( ( foldl (&&) True . map (notUnfusable res) ) out_nms ) 
    let to_fuse_knmSet = getKersWithInpArrs res out_nms
    let to_fuse_knms   = S.toList to_fuse_knmSet
    to_fuse_kers <- mapM (\x-> case M.lookup x (kernels res) of
                                Nothing -> badFusionGM $ EnablingOptError (SrcLoc (locOf soac))
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
    let all_used_names = foldl (\y x -> S.insert x y) lam_used_nms (inp_nms++other_nms)

    --let kers_cap = map (\k-> S.intersection (inplace k) all_used_names) to_fuse_kers
    --let ok_inplace = L.all S.null kers_cap -- foldl (&&) True (map (\s -> S.null s) kers_cap)  
    -- Replace with:
    let ok_inplace = L.all (\ker-> not $ L.any (\x->S.member x $ inplace ker) (S.toList all_used_names)) to_fuse_kers


    -- If @soac@ is a Filter2, then check that none of its out idds 
    --    are used in a size call. 
    let ok_sz=case soac of
                Filter2{} -> 
                     let bs = map (\x->case M.lookup x (inzips res) of
                                    Nothing  -> True
                                    Just znms-> foldl (\acc znm -> 
                                                          case M.lookup znm (zips res) of
                                                            Nothing   -> acc -- error
                                                            Just (i,_)-> acc && not (getBuiltinFunName i == "shape")
                                                      ) True znms
                                  ) out_nms
                     in foldl (&&) True bs
                _ -> True

    -- compute whether @soac@ is fusable or not
    let is_fusable = not_unfusable && not (null to_fuse_kers) && ok_inplace && ok_kers_compat && ok_sz

    -- DEBUG STMT (delete it!)
    --to_fuse_kers' <- trace ("ker: "++ppTupId idd++" fusable?: "++show is_fusable++" to_fuse_kers_num: "++show (length to_fuse_kers) ++ " inpArrs keys: " ++ concatMap nameToString (M.keys (inpArr res))) (return to_fuse_kers)

    --  (i) inparr ids other than vars will be added to unfusable list, 
    -- (ii) will also become part of the unfusable set the inparr vars 
    --         that also appear as inparr of another kernel,    
    --         BUT which said kernel is not the one we are fusing with (now)!
    let mod_kerS  = if is_fusable then to_fuse_knmSet else S.empty
    let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
    let ufs'      = (unfusable res) `S.union` S.fromList used_inps `S.union` S.fromList other_nms

    if not is_fusable && is_repl
    then do return res
    else 
     if not is_fusable 
     then do -- nothing to fuse, add a new soac kernel to the result
            let new_ker = FusedKer (idd, soac) (S.fromList inp_idds) S.empty []
            nm_ker  <- new "ker"
            let os' = foldl (\x arr -> M.insert arr nm_ker x) 
                            (outArr res) out_nms 
            let is' = foldl (\x arr -> M.insertWith' (\a b -> a `S.union` b) arr (S.singleton nm_ker) x) 
                            (inpArr res) inp_nms0
            return $ FusedRes (rsucc res) os' is' ufs' (zips res) (inzips res) 
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
 
            -- TODO: fixing the zipAsserts: 
            --       (i) get the ``best'' substitution candidate from the input arrays 
            inp_rep <- getInpArrSOAC soac >>= getBestInpArr 

            --      (ii) remove the fused arrays from the `inzips' Map 
            --           and update the ``best'' substitution candidate
            let (zip_nms, inzips') = 
                    foldl (\(l,m) x -> case M.lookup x m of
                                    Nothing -> ([] :l, m)
                                    Just lst-> (lst:l, M.delete x m) 
                          ) ([], inzips res) (reverse out_nms) 
            let inzips'' = case inp_rep of
                            Var x -> case M.lookup (identName x) inzips' of
                                        Nothing -> M.insert (identName x)
                                                            (concat zip_nms)
                                                            inzips'
                                        Just l  -> M.insert (identName x)
                                                            (L.nub $ concat zip_nms++l)
                                                            inzips'
                            _     -> inzips'

            
            --     (iii) update the zips Map, i.e., perform the substitution
            --           with the best candidate
            --_ <- trace (ppExp inp_rep) (return Nothing)
            --_ <- trace (ppTupId idd) (return Nothing)
            let zips''= foldl (\mm (nm,bnms) -> 
                                    foldl (\m bnm -> 
                                            case M.lookup bnm m of
                                                Nothing         -> m --error
                                                Just (fnm,arrs) -> M.insert bnm (fnm, substCand inp_rep nm arrs) m 
                                          ) mm bnms 
                              ) (zips res) (zip out_nms zip_nms)

            -- nothing to do for `outArr' (since we have not added a new kernel) 
            return $ FusedRes True (outArr res) inpArr' ufs' zips'' inzips'' kernels'
    where 
        notUnfusable :: FusedRes -> VName -> Bool
        notUnfusable ress nm = not $ S.member nm $ unfusable ress

        getBestInpArr :: [Exp] -> FusionGM Exp
        getBestInpArr arrs = do
            foldM (\b x -> case x of
                               Iota{} -> return x
                               Var {} -> case b of
                                            Iota{} -> return b
                                            _      -> return x
                               Index{}-> return b
                               _     -> badFusionGM $ EnablingOptError (SrcLoc (locOf b))
                                                        "In Fusion.hs, getBestInpArr error!"
                  ) (head arrs) (tail arrs)

        substCand :: Exp -> VName -> S.Set Exp -> S.Set Exp
        substCand rep nm arrs = 
            let rres = foldl (\l arr -> case arr of
                                Var x -> if nm == (identName x)
                                         then (rep:l) else (arr:l)
                                _     -> (arr:l)
                             ) [] (reverse $ S.toList arrs)
            in S.fromList rres

fuseSOACwithKer :: ([Ident], Exp) -> FusedKer -> FusionGM FusedKer
fuseSOACwithKer (out_ids1, soac1) ker = do
  let (out_ids2, soac2) = fsoac ker

  case (soac2, soac1) of
      -- first get rid of the cases that can be solved by
      -- a bit of soac rewriting.
    (Reduce2{}, Map2   {}) -> do
      soac2' <- reduce2Redomap soac2
      let ker'   = ker { fsoac = (out_ids2, soac2') }
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
                (Map2      _ _ _ pos, Map2    {}) -> do
                  (res_lam, new_inp) <- fuse2SOACs inp1_arr lam1 out_ids1 inp2_arr lam2
                  return (Map2    res_lam new_inp (mkElType new_inp) pos, new_inp)
                (Redomap2 lam21 _ ne _ _ pos, Map2 {})-> do
                  (res_lam, new_inp) <- fuse2SOACs inp1_arr lam1 out_ids1 inp2_arr lam2
                  return (Redomap2 lam21 res_lam ne new_inp (mkElType new_inp) pos, new_inp)

                ----------------------------------------------------
                -- The Fusions that are semantically filter fusions:
                ----------------------------------------------------
                (Reduce2 _ ne _ eltp pos, Filter2 _ _ _) -> do
                  (res_lam, new_inp) <- fuseRedFilt inp1_arr lam1 out_ids1 inp2_arr lam2
                  return (Reduce2 res_lam ne new_inp eltp pos, new_inp)
                (Redomap2 lam21 _ nes _ eltp pos, Filter2 _ _ _) -> do
                  (res_lam, new_inp) <- fuseMapFilt inp1_arr lam1 (tuplit nes pos) inp2_arr lam2
                  return (Redomap2 lam21 res_lam nes new_inp eltp pos, new_inp)
                (Filter2 _ _ pos, Filter2 _ _ pos1) -> do
                  (res_lam, new_inp) <- fuseMapFilt inp1_arr lam1 (Literal (LogVal False) pos1) inp2_arr lam2
                  return (Filter2 res_lam new_inp pos, new_inp)

                ----------------------------------------------------
                -- Unfusable: should not have reached here!!!
                ----------------------------------------------------
                _ -> badFusionGM $ EnablingOptError (SrcLoc (locOf soac1))
                                    ("In Fusion.hs, fuseSOACwithKer: fusion not supported " 
                                     ++ "(soac2,soac1): (" ++ ppExp soac2++", "++ppExp soac1)
            -- END CASE(soac2,soac1)

            let inp_new = foldl (\lst e -> case e of
                                            Var idd -> lst++[idd]
                                            _       -> lst
                                ) [] res_inp
    
            -- soac_new1<- trace ("\nFUSED KERNEL: " ++ (nameToString . identName) (head out_ids1) ++ " : " ++ ppExp res_soac ++ " Input arrs: " ++ concatMap (nameToString . identName) inp_new) (return res_soac)

            let fused_vars_new    = (fused_vars ker)++out_ids1
            return $ FusedKer (out_ids2, res_soac) (S.fromList inp_new) (inplace ker) fused_vars_new

    where
        mkElType :: [Exp] -> Type
        mkElType arrs = do
            if length arrs == 1 
            then stripArray 1 (typeOf $ head arrs)
            else Elem $ Tuple $ map (stripArray 1) (map typeOf arrs)



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


fusionGatherExp fres (LetPat pat soac@(Map2 lam _ _ _) body _) = do
    bres  <- bindPat pat soac $ fusionGatherExp fres body   -- binding pat $ 
    (used_lam, blres) <- fusionGatherLam (S.empty, bres) lam
    greedyFuse False used_lam blres (pat, soac)

fusionGatherExp fres (LetPat pat soac@(Replicate n el pos) body _) = do
    bres <- bindPat pat soac $ fusionGatherExp fres body
    -- Implemented inplace: gets the variables in `n` and `el`
    (used_set, bres')<- getUnfusableSet pos bres [n,el] 
    repl_idnm <- new "repl_x"
    let repl_id = Ident repl_idnm (Elem Int) pos
    let repl_lam = AnonymFun [toParam repl_id] el (toDecl $ typeOf el) pos
    let soac_repl= Map2 repl_lam [Iota n pos] (typeOf el) pos
    greedyFuse True used_set bres' (pat, soac_repl)

fusionGatherExp fres (LetPat pat soac@(Filter2 lam _ _) body _) = do
    bres  <- bindPat pat soac $ fusionGatherExp fres body
    (used_lam, blres) <- fusionGatherLam (S.empty, bres) lam
    greedyFuse False used_lam blres (pat, soac)
    --bres' <- foldM fusionGatherExp bres arrs
    --(_, lres) <- fusionGatherLam (S.empty, bres') lam
    --return lres

fusionGatherExp fres (LetPat pat soac@(Reduce2 lam nes _ _ loc) body _) = do
    -- a reduce always starts a new kernel
    bres  <- bindPat pat soac $ fusionGatherExp fres body
    bres' <- fusionGatherExp bres $ tuplit nes loc
    (_, blres) <- fusionGatherLam (S.empty, bres') lam
    addNewKer blres (pat, soac)

fusionGatherExp fres (LetPat pat soac@(Redomap2 lam_red lam_map ne _ _ loc) body _) = do
    -- a redomap always starts a new kernel
    (_, lres)  <- foldM fusionGatherLam (S.empty, fres) [lam_red, lam_map]
    bres  <- bindPat pat soac $ fusionGatherExp lres body -- binding pat $ 
    bres' <- fusionGatherExp bres $ tuplit ne loc
    addNewKer bres' (pat, soac)

fusionGatherExp fres (LetPat pat (Scan2 lam nes arrs _ _) body _) = do
    -- NOT FUSABLE
    (_, lres)  <- fusionGatherLam (S.empty, fres) lam
    bres  <- binding pat $ fusionGatherExp lres body
    foldM fusionGatherExp bres (nes++arrs)

fusionGatherExp fres (LetPat pat (Apply fname zip_args _ _) body _) 
  | "assertZip" <- nameToString fname = do
    bres  <- fusionGatherExp fres body
    let nm = identName $ head $ getIdents pat

    let iddnms  = foldl (\s x-> case x of
                                  Var idd -> S.insert (identName idd) s
                                  _       -> s
                        ) S.empty zip_args
    let inzips' = foldl (\m x -> case M.lookup x m of
                                    Nothing -> M.insert x [nm]     m
                                    Just lst-> M.insert x (nm:lst) m
                        ) (inzips bres) (S.toList iddnms)
    let i = getBuiltinFunIdent "assertZip"
    return $ bres { zips   = M.insert nm (i, S.fromList zip_args) (zips bres), inzips = inzips' }

fusionGatherExp fres (LetPat pat (Shape arr _) body _) = do
    -- Fix implementation such that if the arg of size is not from a map, then it is parsed,
    -- i.e., if the argument is coming from a filter then it should inhibit fusion!
    bres  <- fusionGatherExp fres body
    let nm = identName $ head $ getIdents pat
    (inzips',bres') <- 
        case arr of
            Var idd -> case M.lookup (identName idd) (inzips bres) of
                        Nothing -> return ( M.insert (identName idd) [nm]     (inzips bres), bres )
                        Just lst-> return ( M.insert (identName idd) (nm:lst) (inzips bres), bres )
            _       -> do new_res <- fusionGatherExp bres arr
                          return (inzips bres, new_res)
    let i  = getBuiltinFunIdent "shape"
    return $ bres' { zips   = M.insert nm (i, S.singleton arr) (zips bres'), inzips = inzips' }


fusionGatherExp fres (LetPat pat e body _) = do
    let pat_vars = map (\x->Var x) (getIdents pat)
    bres <- binding pat $ fusionGatherExp fres body
    foldM fusionGatherExp bres (e:pat_vars)


-----------------------------------------
---- Var/Index/LetWith/Do-Loop/If    ----
-----------------------------------------

fusionGatherExp fres (Var idd) = do
    -- IF idd is an array THEN ADD it to the unfusable set!
    case identType idd of
        Array{} -> do return $ fres { unfusable = S.insert (identName idd) (unfusable fres) }
        _       -> do return $ fres

fusionGatherExp fres (Index idd inds _ _) = do 
    foldM fusionGatherExp fres ((Var idd) : inds)

fusionGatherExp fres (LetWith id1 id0 inds elm body _) = do 
    bres  <- binding (Id id1) $ fusionGatherExp fres body

    let pat_vars = map (\x->Var x) [id0, id1]
    fres' <- foldM fusionGatherExp bres (elm : inds ++ pat_vars)
    let (ker_nms, kers) = unzip $ M.toList $ kernels fres'

    -- Now add the aliases of id0 (itself included) to the `inplace' field 
    --     of any existent kernel. ToDo: extend the list to contain the 
    --     whole set of aliases (not available now!)
    let inplace_aliases = S.toList $ aliases $ typeOf $ Var id0
    let kers' = map (\ k -> let inplace' = foldl (\x y->S.insert y x) (inplace k) inplace_aliases
                            in  k { inplace = inplace' }
                    ) kers
    let new_kernels = M.fromList $ zip ker_nms kers'
    return $ fres' { kernels = new_kernels }

fusionGatherExp fres (DoLoop merge_pat ini_val _ ub loop_body let_body _) = do
    letbres <- binding merge_pat $ fusionGatherExp fres let_body

    let pat_vars = map (\x->Var x) (getIdents merge_pat)
    fres' <- foldM fusionGatherExp letbres (ini_val:ub:pat_vars)

    let null_res = mkFreshFusionRes
    new_res <- binding merge_pat $ fusionGatherExp null_res loop_body
    -- make the inpArr unfusable, so that they 
    -- cannot be fused from outside the loop:
    let (inp_arrs, _) = unzip $ M.toList $ inpArr new_res
    -- let unfus = (unfusable new_res) `S.union` (S.fromList inp_arrs)
    let new_res' = new_res { unfusable = foldl (\r x -> S.insert x r) (unfusable new_res) inp_arrs }
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

fusionGatherExp _ (Map2     _ _ _     pos) = errorIllegal "map2"    pos
fusionGatherExp _ (Reduce2  _ _ _ _   pos) = errorIllegal "reduce2" pos
fusionGatherExp _ (Scan2    _ _ _ _   pos) = errorIllegal "scan2"   pos
fusionGatherExp _ (Filter2  _ _       pos) = errorIllegal "filter2" pos
fusionGatherExp _ (Redomap2 _ _ _ _ _ pos) = errorIllegal "redomap2" pos

fusionGatherExp _ (Apply fname _ _ pos)
  | "assertZip" <- nameToString fname = 
    badFusionGM $ EnablingOptError pos
                  ("In Fusion.hs, assertZip found outside a let binding! ")
fusionGatherExp _ (Apply fname _ _ pos)
  | "size" <- nameToString fname = 
    badFusionGM $ EnablingOptError pos
                  ("In Fusion.hs, size found outside a let binding! ")


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

fusionGatherLam :: (S.Set VName, FusedRes) -> Lambda -> FusionGM (S.Set VName, FusedRes)
fusionGatherLam (u_set,fres) (AnonymFun idds body _ pos) = do
    let null_res = mkFreshFusionRes
    new_res <- binding (TupId (map (Id . fromParam) idds) pos) $ fusionGatherExp null_res body
    -- make the inpArr unfusable, so that they 
    -- cannot be fused from outside the lambda:
    let inp_arrs = S.fromList $ M.keys (inpArr new_res) -- unzip $ M.toList $ inpArr new_res
    let unfus' = (unfusable new_res) `S.union` inp_arrs
    -- foldl (\r x -> S.insert x r) (unfusable new_res) inp_arrs 
    bnds <- asks $ arrsInScope
    let unfus  = unfus' `S.intersection` bnds
    -- merge fres with new_res'
    let new_res' = new_res { unfusable = unfus }
    -- merge new_res with fres'
    return $ (u_set `S.union` unfus, unionFusionRes new_res' fres)
fusionGatherLam (u_set1, fres) (CurryFun _ args _ pos) = do
    (u_set2, fres') <- getUnfusableSet pos fres args
    return (u_set1 `S.union` u_set2, fres')


getUnfusableSet :: SrcLoc -> FusedRes -> [Exp] -> FusionGM (S.Set VName, FusedRes)
getUnfusableSet pos fres args = do
    -- assuming program is normalized then args 
    -- can only contribute to the unfusable set
    let null_res = mkFreshFusionRes
    new_res <- foldM fusionGatherExp null_res args
    if not (M.null (outArr  new_res)) || not (M.null (inpArr new_res)) || 
       not (M.null (kernels new_res)) || not (M.null (zips   new_res)) || (rsucc new_res)
    then badFusionGM $ EnablingOptError pos 
                        ("In Fusion.hs, getUnfusableSet, broken invariant!"
                         ++" Unnormalized program: " ++ concatMap (ppExp) args )
    else return $ ( unfusable new_res, 
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

fuseInExp (LetPat pat (Apply fname _ rtp p1) body pos) 
  | "assertZip" <- nameToString fname = do
    body' <- fuseInExp body
    fres  <- asks $ fusedRes
    let nm = identName $ head $ getIdents pat
    case M.lookup nm (zips fres) of
        Nothing -> badFusionGM $ EnablingOptError pos
                                   ("In Fusion.hs, fuseInExp LetPat of assertZip, "
                                    ++" assertZip not in Res: "++textual nm)
        Just (i,ss)-> do let arrs = S.toList ss
                         let fnm  = getBuiltinFunName i
                         _ <- if fnm == "assertZip" 
                              then return Nothing
                              else badFusionGM $ EnablingOptError pos
                                                  ("In Fusion.hs, fuseInExp LetPat of assertZip, "
                                                   ++" builtin funname not assertZip: "++fnm)
                         if length arrs > 1
                         then return $ LetPat pat (Apply fname arrs rtp p1) body' pos
                         else return $ body' -- i.e., assertZip eliminated

fuseInExp (LetPat pat (Shape _ p1) body pos) = do
    body' <- fuseInExp body
    fres  <- asks $ fusedRes
    let nm = identName $ head $ getIdents pat
    case M.lookup nm (zips fres) of
        Nothing -> badFusionGM $ EnablingOptError pos
                                   ("In Fusion.hs, fuseInExp LetPat of shape, "
                                    ++" shape not in Res: "++textual nm)
        Just (i,ss)-> do let arrs = S.toList ss
                         let fnm  = getBuiltinFunName i
                         _ <- if fnm == "shape" && length arrs == 1
                              then return Nothing
                              else badFusionGM $ EnablingOptError pos
                                                  ("In Fusion.hs, fuseInExp LetPat of shape, "
                                                   ++" builtin funname not shape: "++fnm)
                         case (head arrs) of
                            Iota n_exp _ -> return $ LetPat pat n_exp body' pos -- shape eliminated
                            _            -> return $ LetPat pat (Shape (head arrs) p1) body' pos

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
fuseInExp (Apply fname _ _ pos)
  | "assertZip" <- nameToString fname = 
    badFusionGM $ EnablingOptError pos
                  ("In Fusion.hs, assertZip found outside a let binding! ")

-------------------------------------------------------
-------------------------------------------------------
---- Pattern Match The Rest of the Implementation! ----
----          NOT USED !!!!!                       ----
-------------------------------------------------------        
-------------------------------------------------------


fuseInExp e = mapExpM fuseIn e
  where fuseIn = identityMapper {
                      mapOnExp    = fuseInExp
                    , mapOnLambda = fuseInLambda
                    }

fuseInLambda :: Lambda -> FusionGM Lambda
fuseInLambda (AnonymFun params body rtp pos) = do  
    body' <- fuseInExp body
    return $ AnonymFun params body' rtp pos

fuseInLambda (CurryFun fname exps rtp pos) = do
    exps'  <- mapM fuseInExp exps
    return $ CurryFun fname exps' rtp pos 

--fuseInExp e = do return e


replaceSOAC :: TupIdent -> Exp -> FusionGM Exp
replaceSOAC pat soac = do
    fres  <- asks $ fusedRes
    let pos     = SrcLoc (locOf soac)
    let pat_nm  = identName $ head $ getIdents pat
    case M.lookup pat_nm (outArr fres) of
        Nothing -> fuseInExp soac
        Just knm-> case M.lookup knm (kernels fres) of
                    Nothing -> badFusionGM $ EnablingOptError pos
                                                ("In Fusion.hs, replaceSOAC, outArr in ker_name "
                                                 ++"which is not in Res: "++textual knm)
                    Just ker-> do
                        let (pat', new_soac) = fsoac ker 
                        if not (pat == pat')
                        then badFusionGM $ EnablingOptError pos 
                                            ("In Fusion.hs, replaceSOAC, "
                                             ++" pat does not match kernel's pat: "++ppTupId pat)
                        else if L.null $ fused_vars ker
                             then badFusionGM $ EnablingOptError pos 
                                                 ("In Fusion.hs, replaceSOAC, unfused kernel "
                                                  ++"still in result: "++ppTupId pat)
                             -- then fuseInExp soac
                             else do -- return new_soac
                                     -- TRY MOVE THIS TO OUTER LEVEL!!!
                                     lam   <- getLamSOAC new_soac
                                     nmsrc <- get
                                     prog  <- asks $ program
                                     case normCopyOneLambda prog nmsrc lam of
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
               unfusable = S.empty, zips   = M.empty, inzips  = M.empty, kernels = M.empty }

unionFusionRes :: FusedRes -> FusedRes -> FusedRes
unionFusionRes res1 res2 = 
    FusedRes  (rsucc     res1       ||      rsucc     res2)
              (outArr    res1    `M.union`  outArr    res2)
              (M.unionWith S.union (inpArr res1) (inpArr res2) )
              (unfusable res1    `S.union`  unfusable res2)
              (zips      res1    `M.union`  zips      res2)
              (M.unionWith (++) (inzips res1) (inzips res2) )
              (kernels   res1    `M.union`  kernels   res2)

mergeFusionRes :: FusedRes -> FusedRes -> FusionGM FusedRes
mergeFusionRes res1 res2 = do
    let ufus_mres = unfusable res1 `S.union` unfusable res2
    inp_both     <- expandSoacInpArr $ M.keys $ inpArr res1 `M.intersection` inpArr res2
    let m_unfus   = foldl (\s x -> S.insert x s) ufus_mres inp_both
    return $ FusedRes  (rsucc     res1       ||      rsucc     res2)
                       (outArr    res1    `M.union`  outArr    res2)
                       (M.unionWith S.union (inpArr res1) (inpArr res2) )
                       m_unfus
                       (zips      res1    `M.union`  zips      res2)
                       (M.unionWith (++) (inzips res1) (inzips res2) )
                       (kernels   res1    `M.union`  kernels   res2)
 
     
-- | Returns the list of identifiers of a pattern.
getIdents :: TupIdent -> [Ident]
getIdents (Id idd)      = [idd]
getIdents (TupId tis _) = concatMap getIdents tis

getLamSOAC :: Exp -> FusionGM Lambda
getLamSOAC (Map2     lam _ _ _  ) = return lam
getLamSOAC (Reduce2  lam _ _ _ _) = return lam
getLamSOAC (Filter2  lam _ _    ) = return lam
getLamSOAC (Redomap2 _ lam2 _ _ _ _) = return lam2
getLamSOAC ee = badFusionGM $ EnablingOptError 
                                (SrcLoc (locOf ee)) 
                                ("In Fusion.hs, getLamSOAC, broken invariant: "
                                 ++" argument not a SOAC! "++ppExp ee)

updateLamSOAC :: Lambda -> Exp -> FusionGM Exp
updateLamSOAC lam (Map2          _    arrs eltp pos) = return $ Map2          lam    arrs eltp pos
updateLamSOAC lam (Reduce2       _ ne arrs eltp pos) = return $ Reduce2       lam ne arrs eltp pos
updateLamSOAC lam (Filter2       _    arrs      pos) = return $ Filter2       lam    arrs      pos
updateLamSOAC lam (Redomap2 lam1 _ ne arrs eltp pos) = return $ Redomap2 lam1 lam ne arrs eltp pos
updateLamSOAC _ ee = badFusionGM $ EnablingOptError 
                                    (SrcLoc (locOf ee)) 
                                    ("In Fusion.hs, updateLamSOAC, broken invariant: "
                                     ++" argument not a SOAC! "++ppExp ee)

-- | Returns the input arrays used in a SOAC.
--     If exp is not a SOAC then error.
getInpArrSOAC :: Exp -> FusionGM [Exp]
getInpArrSOAC (Map2     _ arrs _ _  ) = return arrs
getInpArrSOAC (Reduce2  _ _ arrs _ _) = return arrs
getInpArrSOAC (Filter2  _ arrs _    ) = return arrs
getInpArrSOAC (Redomap2 _ _ _ arrs _ _) = return arrs
getInpArrSOAC ee = badFusionGM $ EnablingOptError 
                                (SrcLoc (locOf ee)) 
                                ("In Fusion.hs, getInpArrSOAC, broken invariant: "
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
getIdentArr ((Var idd):es) = do
    (vs, os) <- getIdentArr es 
    case identType idd of
        Array _ _ _ _ -> return (idd:vs, os)
        _             -> -- return (vs, os)
            badFusionGM $ EnablingOptError 
                            (identSrcLoc idd) 
                            ("In Fusion.hs, getIdentArr, broken invariant: "
                             ++" argument not of array type! "++ppExp (Var idd))
getIdentArr ((Index idd _ _ _):es) = do
    (vs, os) <- getIdentArr es 
    case identType idd of
        Array _ _ _ _ -> return (vs, idd:os)
        _             -> -- return (vs, os)
            badFusionGM $ EnablingOptError 
                            (identSrcLoc idd) 
                            ("In Fusion.hs, getIdentArr, broken invariant: "
                             ++" argument not of array type! "++ppExp (Var idd))
getIdentArr ((Iota _ _):es) = getIdentArr es
getIdentArr (ee:_) = 
    badFusionGM $ EnablingOptError 
                    (SrcLoc (locOf ee)) 
                    ("In Fusion.hs, getIdentArr, broken invariant: "
                     ++" argument not a (indexed) variable! "++ppExp ee)
 

reduce2Redomap :: Exp -> FusionGM Exp
reduce2Redomap (Reduce2 lam ne arrs eltp pos) = do
    let num_arrs= length arrs
    let lam_tps = map (stripArray 1 . typeOf) arrs
    lam_nms <- mapM new (replicate num_arrs "tmp_lam")
    let lam_pos = replicate num_arrs pos
    let lam_ids = map (\(nm,tp,p)-> Ident nm tp p) (zip3 lam_nms lam_tps lam_pos)
    let lam_body= if length lam_ids == 1
                  then Var (head lam_ids)
                  else tuplit (map Var lam_ids) pos
    let map_lam = AnonymFun (map toParam lam_ids) lam_body (toDecl eltp) pos
    return $ Redomap2 lam map_lam ne arrs eltp pos
reduce2Redomap eee = 
    badFusionGM $ EnablingOptError (SrcLoc (locOf eee))
                   ("In Fusion.hs, reduce2Redomap: argument not a reduce2: " 
                    ++ ppExp eee)


cleanFusionResult :: FusedRes -> FusedRes
cleanFusionResult fres = 
    let newks = M.filter ( \ker -> not (null (fused_vars ker)) )              (kernels fres)
        newoa = M.filter ( \knm -> M.member knm newks          )              (outArr  fres)
        newia = M.map    ( \knms-> S.filter (\knm->M.member knm newks) knms)  (inpArr  fres)
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


getBuiltinFunName  :: Int -> String
getBuiltinFunName i = case i of
                        1 -> "assertZip"
                        2 -> "shape"
                        _ -> "unknownBuiltInFun"  
getBuiltinFunIdent :: String -> Int
getBuiltinFunIdent "assertZip" = 1
getBuiltinFunIdent "shape"     = 2
getBuiltinFunIdent _           = 33

--------------------------------------------
--------------------------------------------
--- FUSING 2 COMPATIBLE SOACs:           ---
--------------------------------------------
--------------------------------------------


-- | Assumption: the order and number of elements matches between 
--       the input to the Reduce and the input to Filter,
--       i.e., already checked in @isCompatibleKer@
fuseRedFilt :: [Exp] -> Lambda -> [Ident]
                -> [Exp] -> Lambda
                -> FusionGM (Lambda, [Exp])
fuseRedFilt inp1 lam1 _ inp2 lam2 = do
    let rtp2 = lambdaReturnType lam2
    let pos2 = (SrcLoc (locOf lam2))
    let pos1 = (SrcLoc (locOf lam1))
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1
    (ids2, bdy2 ) <- getUnmdParsAndBody inp2 lam2

    let n   = length inp2
    let nd2 = n `div` 2
    (ids21, ids22) <- if n `mod` 2 == 0 && nd2 == length par1 && n == length ids2  
                      then return (take nd2 ids2, drop nd2 ids2)
                      else badFusionGM $ EnablingOptError pos2
                                          ("In Fusion.hs, fuseRedFilt: uneven lambda params " 
                                           ++ "of lam2 (lam1): " ++ show n ++ " " ++ show (length par1))
    
    let then_pat = tuppat (map Id ids22) pos2
    let then_var = tuplit (map Var par1 ) pos1
    let then_exp = LetPat then_pat then_var  bdy2 pos2
    let else_exp = tuplit (map Var ids21) pos2
    let res_body = If call1 then_exp else_exp (fromDecl rtp2) pos1
    return (AnonymFun (map toParam $ ids21++par1) res_body rtp2 pos2, inp1)
    

-- | Assumption: the order and number of elements matches 
--       between the map and the reduce, 
--       i.e., already checked in @isCompatibleKer@.
fuseMapFilt :: [Exp] -> Lambda -> Exp
                -> [Exp] -> Lambda
                -> FusionGM (Lambda, [Exp])
fuseMapFilt inp1 lam1 ne inp2 lam2 = do
    let rtp2 = lambdaReturnType lam2
    let pos2 = (SrcLoc (locOf lam2))
    let pos1 = (SrcLoc (locOf lam1))
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1
    (ids2, bdy2 ) <- getUnmdParsAndBody inp2 lam2

    _ <- if length ids2 == length par1 && length par1 == length inp1 && length inp2 == length inp1
         then return Nothing
         else badFusionGM $ EnablingOptError pos2
                             ("In Fusion.hs, fuse2Filter: num of params of " 
                              ++ "lam2, lam1 and inp1 do not agree: " ++ show (length ids2) 
                              ++ " " ++ show (length par1) ++ " " ++ show (length inp1))

    let then_pat = tuppat (map Id ids2) pos2
    let then_var = tuplit (map Var par1) pos1
    let then_exp = LetPat then_pat then_var bdy2 pos2
    let res_body = If call1 then_exp ne (fromDecl rtp2) pos1
    return (AnonymFun (map toParam par1) res_body rtp2 pos2, inp1)
    
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

----------------------------------------------------------------------------------            
-- | Params:
--    @inp1@ is the input of the (first) to-be-fused SOAC
--    @lam1@ is the function of the (first) to-be-fused SOAC
--    @out1@ are the result arrays (identifiers) of the first SOAC.
--    @inp2@ is the input to the second SOAC.
--    @lam2@ is the function of the second SOAC.
--   Result: the anonymous function of the result SOAC, tupled with
--           the input of the result SOAC. 
--           (The output of the result SOAC == the one of the second SOAC)
----------------------------------------------------------------------------------
fuse2SOACs :: [Exp] -> Lambda -> [Ident]
           -> [Exp] -> Lambda
           -> FusionGM (Lambda, [Exp])
fuse2SOACs inp1 lam1 out1 inp2 lam2 = do
          -- inp2 (AnonymFun lam_args2 body2 rtp2 pos2) = do
    let rtp2 = lambdaReturnType lam2
    let pos2 = SrcLoc (locOf lam2)
    -- SOAC's lambda have been normalized, hence they accept an 
    --   arbitrary number of non-tuple arguments.
    (ids2, bdy2) <- getUnmdParsAndBody inp2 lam2

    -- get Map that bridge the out_arrays of SOAC1 
    -- with the input_lambda_parameters of SOAC2 
    let out_m1 = map identName out1
    out_m2 <- buildOutMaps out1 ids2 inp2 

    -- compute: (inp2 \\ out1, ids2 \\ out1)
    let inp_par2 = 
         filter (\(x,_)-> case x of
                            Var idd -> not $ elem (identName idd) out_m1  -- M.member 
                            _       -> True
                )   (zip inp2 ids2)
    let (inp2', par2') = unzip inp_par2
    let inp_res = inp1 ++ inp2'

    -- Construct the body of the resulting lambda!
    (ids_out1, bdy2') <- 
              foldM (\(idds,body) i -> do
                      let args = case M.lookup i out_m2 of
                                  Nothing   -> []
                                  Just (_,y)-> y
                      if null args 
                      then badFusionGM $ EnablingOptError 
                                          (SrcLoc (locOf body)) 
                                          ("In Fusion.hs, fuse2SOACs, foldM's lambda1 "
                                           ++" broken invariant: param index not in Map!")
                      else return $ (idds++[(head args)], mkCopies (head args) (tail args) body)
                    ) ([],bdy2) [0..(length out1)-1]

    let call_idd = if length ids_out1 == 1 then Id (head ids_out1)
                   else TupId (map (\x->Id x) ids_out1) (SrcLoc (locOf lam1))

    -- Build the call to the first SOAC function
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1
    let bdy2'' = LetPat call_idd call1 bdy2' (SrcLoc (locOf lam1))
    let par = par1 ++ par2'

    -- remove duplicate inputs
    (inp_res', par', bdy2''') <- removeDupInps inp_res par bdy2''

    return (AnonymFun (map toParam par') bdy2''' rtp2 pos2, inp_res')
    
    where
        -- | @arrs@ are the input arrays of a SOAC call
        --   @idds@ are the args (ids) of the SOAC's unnamed function
        --   @lam_bdy@ is the body of the lambda function.
        --   Result: eliminates the duplicate input arrays (and lambda args),
        --           by inserting copy statement in the body of the lambda.
        --   E.g., originally: map(\(a,b,c) -> body,              x[i], y, x[i])
        --         results in: map(\(b,c)   -> let a = b in body,       y, x[i])
        removeDupInps :: [Exp] -> [Ident] -> Exp -> FusionGM ([Exp], [Ident], Exp)
        removeDupInps [] [] lam_bdy = return ([], [], lam_bdy)
        removeDupInps [] _  lam_bdy = badFusionGM $ EnablingOptError (SrcLoc (locOf lam_bdy)) 
                                                     ("In Fusion.hs, removeDupInps, arr inps "
                                                      ++" AND lam pars size do not match!")
        removeDupInps _  [] lam_bdy = badFusionGM $ EnablingOptError (SrcLoc (locOf lam_bdy)) 
                                                     ("In Fusion.hs, removeDupInps, arr inps "
                                                      ++" AND lam pars size do not match!")
        removeDupInps (arr:arrs) (idd:idds) lam_bdy = do
            case L.elemIndex arr arrs of
                Nothing -> do (rec_arr, rec_idds, rec_bdy) <- removeDupInps arrs idds lam_bdy
                              return (arr:rec_arr, idd:rec_idds, rec_bdy)
                Just ind-> do -- eliminate the current so you can advance!
                              let rec_bdy = LetPat (Id idd) (Var (idds !! ind)) lam_bdy (identSrcLoc idd)
                              removeDupInps arrs idds rec_bdy
        


        -- | @mkCopies x [y,z,t] body@ results in
        --   @let y = x in let z = x in let t = x in body
        mkCopies :: Ident -> [Ident] -> Exp -> Exp
        mkCopies idd idds body = 
            foldl (\ bbdy ii -> LetPat (Id ii) (Var idd) bbdy (identSrcLoc idd) )
                  body idds
             
        -- | @out_arr1@ are the array result of a (first) SOAC (call)
        --   @inp_par2@ are the (expanded lambda) param of another (second) SOAC, 
        --              which is to be fused with the first SOAC.
        --   @inp_arr2@ are the input arrays of the second SOAC.
        --   The result is a Map that ASSOCIATES:
        --     (i) the position in which vars of @inp_par2@ appear in @out_arr1@ 
        --    (ii) with a tuple formed by (1) the corresponding 
        --            element in the @out_arr1@ and (2) the set of vars in 
        --            @inp_par2@, i.e., lambda args, which correspond to (1).
        -- E.g., let (x1,x2,x3) = map(f, a1, a2)
        --       let res        = map(fn type (a,b,c,d) => ..., x3, x1, y1, x1)
        --    Result is: { 1 -> ("x1", ["b","d"]), 2 -> ("",["new_tmp"], 3 -> ("x3", ["a"])} 
        buildOutMaps :: [Ident] -> [Ident] -> [Exp] ->
                        FusionGM (M.Map Int (VName, [Ident]))
        buildOutMaps out_arr1 inp_par2 inp_arr2 = do
            -- let inp_pnm = map identName inp_par2
            let out_anm = map identName out_arr1
            let m2 = foldl (\ m (arr, par) -> 
                                case arr of
                                    Var idd -> case L.elemIndex (identName idd) out_anm of 
                                                Nothing -> m
                                                Just i  -> case M.lookup i m of
                                                            Nothing    -> M.insert i (identName idd, [par] ) m
                                                            Just (_,ps)-> M.insert i (identName idd, ps++[par]) m
                                    _       -> m    
                           )   M.empty (zip inp_arr2 inp_par2)
            -- add the unused vars from out_arr1
            m3 <- foldM  (\ m i -> 
                              case M.lookup i m2 of
                                Nothing   -> do new_nm <- new "tmp"
                                                let ppp    = out_arr1 !! i
                                                let new_tp = stripArray 1 (identType ppp) 
                                                let new_id = Ident new_nm new_tp (identSrcLoc ppp)
                                                -- CHECKME: Empty name?  This will definitely cause trouble.
                                                return $ M.insert i (varName "" Nothing, [new_id]) m
                                Just(_,_)-> return m
                            )
                            m2 [0..(length out_anm)-1] 
            return m3    



-- | Recieves a list of types @tps@, which should match
--     the lambda's parameters, and a lambda expression.
--   Returns a list of identifiers, corresponding to
--     the `expanded' argument of lambda, and the lambda call. 
buildCall :: [Type] -> Lambda -> FusionGM ([Ident], Exp)
buildCall tps (AnonymFun lam_args bdy _ pos)    = do
    let (ids, bdy') = (lam_args, bdy) -- getLamNormIdsAndBody lam_args bdy
    let bres = and $ zipWith subtypeOf tps $ map (identType . fromParam) ids
    if bres then return (map fromParam ids, bdy')
    else badFusionGM $ EnablingOptError pos
                        ("In Fusion.hs, buildCall, types do NOT match! ")
buildCall tps (CurryFun fnm aargs rtp pos) = do
    ids <- mapM (\ tp -> do new_nm <- new "tmp"
                            return $ Ident new_nm tp pos
                ) tps
    -- let tup = TupLit (map (\x -> Var x) ids) pos
    let new_vars = map Var ids
    return (ids, Apply fnm (aargs++new_vars) rtp pos) -- [tup]


getUnmdParsAndBody :: [Exp] -> Lambda -> FusionGM ([Ident], Exp)
getUnmdParsAndBody inpp lamm = do
    case lamm of
        AnonymFun lam_args2 body2 _ _ -> return (map fromParam lam_args2, body2)
        CurryFun fnm aargs rtp pos -> do
            pnms <- mapM new $ replicate (length inpp) "lam_arg"
            let new_ids = map (\(nm,tp) -> Ident nm tp pos)
                              ( zip pnms (map (stripArray 1 . typeOf) inpp) )
            let es = map Var new_ids
            let call2  = Apply fnm (aargs++es) rtp pos
            return (new_ids, call2) 

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
    then do return False
    else do -- check that the input-array  set of soac is included
            --         in the output-array set of soac_filt
            (inp_idds2, other_idds2) <- getInpArrSOAC soac >>= getIdentArr
            let inp_lst = map identName inp_idds2
            return $ (L.null other_idds2) && inp_lst == out_nms -- inp_set `S.isSubsetOf` S.fromList out_nms 
isCompatibleKer _ _ = return False 

tuplit :: [Exp] -> SrcLoc -> Exp
tuplit [e] _ = e
tuplit e loc = TupLit e loc

tuppat :: [TupIdent] -> SrcLoc -> TupIdent
tuppat [p] _ = p
tuppat pats loc = TupId pats loc
