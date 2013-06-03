{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.HOTrans.Fusion ( fuseProg )
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

import L0.AbSyn
import L0.FreshNames
import L0.EnablingOpts.EnablingOptErrors


data FusionGEnv = FusionGEnv {   
                    envVtable  :: S.Set Name -- M.Map Name (TupIdent tf)
                  , envFtable  :: M.Map Name (FunDec Type)  
                  }


newtype FusionGM a = FusionGM (StateT NameSource (ReaderT FusionGEnv (Either EnablingOptError)) a)
    deriving (  MonadState NameSource, 
                MonadReader FusionGEnv,
                Monad, Applicative, Functor )
{-

-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: TupNormEnv tf -> (Name, TupIdent tf) -> TupNormEnv tf
bindVar env (name,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: TupNormEnv tf -> [(Name, TupIdent tf)] -> TupNormEnv tf
bindVars = foldl bindVar

binding :: [(Name, TupIdent tf)] -> TupNormM tf a -> TupNormM tf a
binding bnds = local (`bindVars` bnds)

-}


-- | The fusion transformation runs in this monad.  The mutable
-- state refers to the fresh-names engine. 
-- The reader hides the vtable that associates ... to ... (fill in, please).
-- The 'Either' monad is used for error handling.
runFusionGatherM :: Prog Type -> FusionGM a -> FusionGEnv -> Either EnablingOptError a
runFusionGatherM prog (FusionGM a) =
    runReaderT (evalStateT a (newNameSourceForProg prog))

badFusionGM :: EnablingOptError -> FusionGM a
badFusionGM = FusionGM . lift . lift . Left

-- | Return a fresh, unique name.  The @Name@ is prepended to the
-- name.
new :: Name -> FusionGM Name
new = state . newName

---------------------------------------------------
---------------------------------------------------
---- RESULT's Data Structure
---------------------------------------------------
---------------------------------------------------
 
data FusedKer = FusedKer { 
                  fsoac      :: (TupIdent Type, Exp Type)
                -- ^ the fused SOAC statement, e.g.,
                -- (z,w) = map2( f(a,b), x, y )

                , inp        :: S.Set (Ident Type)
                -- ^ the input arrays used in the `soac' 
                -- stmt, i.e., `x', `y'.

                --, used       :: S.Set (Ident Type)
                -- ^ the set of variables used in
                -- the closure of the `soac' stmt, i.e., `a', `b'.

                , asserts    :: M.Map (Ident Type) (Exp Type)
                -- ^ the Map of `assertZip' stmts that involve
                -- arrays that are input to the kernel.

                , fused_vars :: [(Ident Type)]
                -- ^ whether at least a fusion has been performed.
                }

data FusedRes = FusedRes {
    rsucc :: Bool
  -- ^ Whether we have fused something anywhere.

  , outArr     :: M.Map Name Name
  -- ^ associates an array to the name of the
  -- SOAC kernel that has produced it.

  , inpArr     :: M.Map Name (S.Set Name)
  -- ^ associates an array to the names of the
  -- SOAC kernels that uses it. These sets include
  -- only the SOAC input arrays used as full variables, i.e., no `a[i]'. 

  , unfusable  :: S.Set Name
  -- ^ the (names of) arrays that are not fusable, i.e.,
  --  (i) they are either used other than input to SOAC kernels, or 
  -- (ii) are used as input to at least two different kernels that
  --      are not located on disjoint control-flow branches, or
  --(iii) are used in the lambda expression of SOACs

  , kernels    :: M.Map Name FusedKer
  -- ^ The hashtable recording the uses
  }

isInpArrInKers :: FusedRes -> Name -> Bool
isInpArrInKers ress nm = 
    case M.lookup nm (inpArr ress) of
        Nothing -> False
        Just s  -> not (S.null s)
 
getKersWithInpArrs :: FusedRes -> [Name] -> S.Set Name
getKersWithInpArrs ress nms = 
    foldl (\s nm -> case M.lookup nm (inpArr ress) of
                        Nothing -> s
                        Just ns -> s `S.union` ns
          ) S.empty nms


----------------------------------------------------------------------
----------------------------------------------------------------------

-- map, reduce, redomap
fuseSOAC :: FusedRes -> (TupIdent Type, Exp Type) -> FusionGM FusedRes 
fuseSOAC res (idd, soac) = do   -- r@(FusedRes s os is ufs ks)
    -- Assumtion: the free vars in lambda are already in `unfusable'

    -- E.g., with `map2(f, a, b[i])', `a' belongs to `inp_idds' and
    --        `b' belongs to `other_idds' 
    (inp_idds, other_idds) <- getInpArrSOAC soac >>= getIdentArr
    let (inp_nms,other_nms) = (map identName inp_idds, map identName other_idds)

    --  (i) inparr ids other than vars will be added to unfusable list, 
    -- (ii) inparr vars who also appear as inparr of another kernel 
    --        will also become part of the unfusable set
    let used_inps = filter (isInpArrInKers res) inp_nms
    --let used_inps = filter  ( \x -> foldl (||) False (map (hasKerInpArr x) ks) ) inp_nms
    let ufs' =  (unfusable res) `S.union` S.fromList used_inps `S.union` S.fromList other_nms
    
    -- Conditions for fusion:
    --   (i) none of `out_idds' belongs to the unfusable set, i.e., `ufs'
    --  (ii) there are some kernels that use some of `out_idds' as inputs 
    let out_idds     = getIdents idd
    let out_nms      = map identName out_idds
    let is_fusable   = ( foldl (&&) True . map (notUnfusable res) ) out_nms   
    let to_fuse_knms = S.toList $ getKersWithInpArrs res out_nms
    let to_fuse_kers = concatMap (\x -> case M.lookup x (kernels res) of
                                            Nothing -> []
                                            Just ker-> [ker] ) 
                                 to_fuse_knms

    if (not is_fusable) || (null to_fuse_kers) 
    then do -- nothing to fuse, add a new soac kernel to the result
            let new_ker = FusedKer (idd, soac) (S.fromList inp_idds) M.empty []
            nm_ker  <- new (nameFromString "ker")
            let os' = foldl (\x arr -> M.insert arr nm_ker x) 
                            (outArr res) out_nms 
            let is' = foldl (\x arr -> M.adjust (\y -> S.insert nm_ker y) arr x) 
                            (inpArr res) inp_nms
            return $ FusedRes (rsucc res) os' is' ufs' (M.insert nm_ker new_ker (kernels res)) 
    else do -- ... fuse current soac into to_fuse_kers ...
            fused_kers <- mapM (fuseKerWithSOAC (out_idds, soac)) to_fuse_kers
            -- Need to suitably update `inpArr': 
            --   (i) first remove the inpArr bindings of the old kernel
            --  (ii) then add the inpArr bindings of the new kernel
            let inpArr' = foldl (\ inpa (kold, knew, knm)-> 
                                    let inpa' = 
                                         foldl (\ inp nm -> case M.lookup nm inp of
                                                             Nothing -> inp
                                                             Just s  -> M.insert nm (S.delete knm s) inp   ) 
                                               inpa (map identName (S.toList (inp kold))) 
                                    in foldl   (\ inp nm -> case M.lookup nm inp of
                                                             Nothing -> inp
                                                             Just s  -> M.insert nm (S.insert knm s) inp   ) 
                                               inpa' (map identName (S.toList (inp knew)))
                                )
                                (inpArr res) (zip3 to_fuse_kers fused_kers to_fuse_knms)
            -- Update the kernels map
            let kernels' = foldl (\ kers (knew, knm) -> M.insert knm knew kers )
                                 (kernels res) (zip fused_kers to_fuse_knms) 
            -- nothing to do for `outArr' (since we have not added a new kernel) 
            return $ FusedRes True (outArr res) inpArr' ufs' kernels'
    where 
        notUnfusable :: FusedRes -> Name -> Bool
        notUnfusable ress nm = not (S.member nm (unfusable ress))

            
fuseKerWithSOAC :: ([Ident Type], Exp Type) -> FusedKer -> FusionGM FusedKer
fuseKerWithSOAC (ids, soac) ker = do
    
    return ker


fuseSOACs :: [Exp Type] -> (Lambda Type) -> [Ident Type] ->
             [Exp Type] -> (Lambda Type) -> 
             FusionGM (Lambda Type, [Exp Type])
fuseSOACs inp1 lam1 out1
          inp2 (AnonymFun ids2 bdy2 rtp2 pos2) = do
    -- get the two maps that bridge the out_arrs of SOAC1 
    -- with the inp_lam_params of SOAC2 
    (out_m1, out_m2) <- buildOutMaps out1 ids2 inp2 

    -- compute: (inp2 \\ out1, ids2 \\ out1)
    let inp_par2 = 
         filter (\(x,y)-> case x of
                            Var idd -> not $ M.member (identName idd) out_m1 
                            _       -> True
                )   (zip inp2 ids2)
    let (inp2', par2') = unzip inp_par2
    let inp = inp1 ++ inp2'

    -- Construct the body of the resulting lambda!
    bdy2' <- foldM (\ body i -> do
                      let args = case M.lookup i out_m2 of
                                  Nothing   -> []
                                  Just (_,y)-> y
                      if null args 
                      then badFusionGM $ EnablingOptError 
                                          (SrcLoc (locOf body)) 
                                          ("In Fusion.hs, fuseSOACs, foldM's lambda1 "
                                           ++" broken invariant: param index not in Map!")
                      else return $ mkCopies (head args) (tail args) body
                   )   
                   bdy2 [0..(length out1)-1]
    ids_out1<-mapM (\ ii -> do 
                      let args = case M.lookup ii out_m2 of
                                  Nothing   -> []
                                  Just (_,y)-> y
                      if null args 
                      then badFusionGM $ EnablingOptError 
                                          (SrcLoc (locOf lam1))
                                          ("In Fusion.hs, fuseSOACs, foldM's lambda2 "
                                           ++" broken invariant: param index not in Map!")
                      else return $ head args
                    ) [0..(length out1)-1]
    let call_idd = if length ids_out1 == 1 then Id (head ids_out1)
                   else TupId (map (\x->Id x) ids_out1) (SrcLoc (locOf lam1))

    -- Build the call to the first SOAC function
    (par1, call1) <- buildCall (map (stripArray 1 . typeOf) inp1) lam1
    let par = par1 ++ par2'

    let bdy2'' = LetPat call_idd call1 bdy2' (SrcLoc (locOf lam1))
    return $ (AnonymFun par bdy2'' rtp2 pos2, inp)
    
    where
        mkCopies :: Ident Type -> [Ident Type] -> Exp Type -> Exp Type
        mkCopies idd idds body = 
            foldl (\ bbdy ii -> LetPat (Id ii) (Var idd) bbdy (identSrcLoc idd) )
                  body idds
 
        buildCall :: [Type] -> Lambda Type -> FusionGM ([Ident Type], Exp Type)
        buildCall tps (AnonymFun ids bdy _ pos)    = do
            let bres = foldl (&&) True (zipWith (==)  tps (map identType ids))
            if bres then return (ids, bdy)
            else badFusionGM $ EnablingOptError pos
                                ("In Fusion.hs, buildCall, types do NOT match! ")
        buildCall tps (CurryFun fnm aargs rtp pos) = do
            ids <- mapM (\ tp -> do new_nm <- new (nameFromString "tmp")
                                    return $ Ident new_nm tp pos
                        ) tps
            let tup = TupLit (map (\x -> Var x) ids) pos
            return (ids, Apply fnm (aargs++[tup]) rtp pos)
            

        buildOutMaps :: [Ident Type] -> [Ident Type] -> [Exp Type] ->
                        FusionGM ( M.Map Name Int, M.Map Int (Name, [Ident Type]) )
        buildOutMaps out_arr1 inp_par2 inp_arr2 = do
            let inp_pnm = map identName inp_par2
            let out_anm = map identName out_arr1
            let m1 = M.fromList (zip out_anm [0..(length out_anm)-1])
            let m2 = foldl (\ m (arr, par) -> 
                                case arr of
                                    Var idd -> case M.lookup (identName idd) m1 of
                                                Nothing -> m
                                                Just i  -> case M.lookup i m of
                                                            Nothing    -> M.insert i (identName idd, [par] ) m
                                                            Just (t,ps)-> M.insert i (identName idd, par:ps) m
                                    _       -> m    
                           )   M.empty (zip inp_arr2 inp_par2)
            -- add the unused vars from out_arr1
            m3 <- foldM  (\ m i -> 
                              case M.lookup i m2 of
                                Nothing   -> do new_nm <- new (nameFromString "tmp")
                                                let ppp    = out_arr1 !! i
                                                let new_tp = stripArray 1 (identType ppp) 
                                                let new_id = Ident new_nm new_tp (identSrcLoc ppp)
                                                return $ M.insert i (nameFromString "", [new_id]) m
                                Just(t,ps)-> return m
                            )
                            m2 [0..(length out_anm)-1] 
            return (m1, m3)

fuseSOACs inp1 lam1 out1
          inp2 (CurryFun fnm2 aargs2 rtp2 pos2) = do
        
------------------------------------------------------------------------
--- Fusion Gather Entry Point: gathers to-be-fused kernels@pgm level ---
------------------------------------------------------------------------

fuseProg :: Prog Type -> Either EnablingOptError (Prog Type) -- (M.Map Name FusedRes)
fuseProg prog = do
    let env = FusionGEnv { envVtable = S.empty, envFtable = M.empty }
    case runFusionGatherM prog (mapM fusionGatherFun prog) env of
        Left  a  -> Left a
        Right ks -> Right (map fuseInFun (zip ks prog))

fusionGatherFun :: FunDec Type -> FusionGM FusedRes
fusionGatherFun (fname, rettype, args, body, pos) = do
    --body' <- trace ("in function: "++fname++"\n") (tupleNormAbstrFun args body pos)
    fusionGatherExp body


fusionGatherExp :: Exp Type -> FusionGM FusedRes
fusionGatherExp ee = 
    return FusedRes { rsucc = False, outArr = M.empty, inpArr = M.empty, unfusable = S.empty, kernels = M.empty }



-------------------------------------------------------------
--- Substitute the kernels in function
-------------------------------------------------------------
fuseInFun :: (FusedRes, FunDec Type) -> FunDec Type
fuseInFun (kers, fundec) = fundec


---------------------------------------------------
---- HELPERS
---------------------------------------------------
     

getIdents :: TupIdent Type -> [Ident Type]
getIdents (Id idd)      = [idd]
getIdents (TupId tis _) = concatMap getIdents tis

getLamSOAC :: Exp Type -> FusionGM (Lambda Type)
getLamSOAC (Map2     lam _ _ _  ) = return lam
getLamSOAC (Reduce2  lam _ _ _ _) = return lam
getLamSOAC (Filter2  lam _ _ _  ) = return lam
getLamSOAC (Mapall2  lam _ _ _ _) = return lam
getLamSOAC (Redomap2 lam1 lam2 _ _ _ _ _) = return lam2
getLamSOAC ee = badFusionGM $ EnablingOptError 
                                (SrcLoc (locOf ee)) 
                                ("In Fusion.hs, getLamSOAC, broken invariant: "
                                 ++" argument not a SOAC! "++ppExp 0 ee)


getInpArrSOAC :: Exp Type -> FusionGM [Exp Type]
getInpArrSOAC (Map2     _ arrs _ _  ) = return arrs
getInpArrSOAC (Reduce2  _ _ arrs _ _) = return arrs
getInpArrSOAC (Filter2  _ arrs _ _  ) = return arrs
getInpArrSOAC (Mapall2  _ arrs _ _ _) = return arrs
getInpArrSOAC (Redomap2 _ _ _ arrs _ _ _) = return arrs
getInpArrSOAC ee = badFusionGM $ EnablingOptError 
                                (SrcLoc (locOf ee)) 
                                ("In Fusion.hs, getInpArrSOAC, broken invariant: "
                                 ++" argument not a SOAC! "++ppExp 0 ee)

getIdentArr :: [Exp Type] -> FusionGM ([Ident Type], [Ident Type])
getIdentArr []             = return ([],[])
getIdentArr ((Var idd):es) = do
    (vs, os) <- getIdentArr es 
    case identType idd of
        Array _ _ _ -> return (idd:vs, os)
        _           -> return (vs, os)
getIdentArr ((Index idd _ _ _ _):es) = do
    (vs, os) <- getIdentArr es 
    case identType idd of
        Array _ _ _ -> return (vs, idd:os)
        _           -> return (vs, os)
getIdentArr (ee:_) = 
    badFusionGM $ EnablingOptError 
                    (SrcLoc (locOf ee)) 
                    ("In Fusion.hs, getIdentArr, broken invariant: "
                     ++" argument not a variable or array! "++ppExp 0 ee)
 
