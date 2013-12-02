{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.HOTrans.Fusion ( fuseProg )
  where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Loops (allM)

import Data.Maybe
import Data.Loc

import qualified Data.Map  as M
import qualified Data.Set  as S

import L0C.L0
import L0C.FreshNames
import L0C.EnablingOpts.EnablingOptDriver
import L0C.HOTrans.Composing

data FusionGEnv = FusionGEnv {
    soacs      :: M.Map VName [VName]
  -- ^ Mapping from variable name to its entire family.
  , arrsInScope:: S.Set VName
  , fusedRes   :: FusedRes
  , program    :: Prog
  }

newtype FusionGM a = FusionGM (StateT VNameSource (ReaderT FusionGEnv (Either EnablingOptError)) a)
    deriving (  MonadState VNameSource,
                MonadReader FusionGEnv,
                Monad, Applicative, Functor )


------------------------------------------------------------------------
--- Monadic Helpers: bind/new/runFusionGatherM, etc                      ---
------------------------------------------------------------------------


-- | Binds an array name to the set of used-array vars
bindVar :: FusionGEnv -> VName -> FusionGEnv
bindVar env name =
  env { arrsInScope = S.insert name $ arrsInScope env }

bindVars :: FusionGEnv -> [VName] -> FusionGEnv
bindVars = foldl bindVar

bindingIdents :: [Ident] -> FusionGM a -> FusionGM a
bindingIdents idds = local (`bindVars` namesOfArrays idds)
  where namesOfArrays = map identName . filter (not . basicType . identType)

binding :: TupIdent -> FusionGM a -> FusionGM a
binding = bindingIdents . S.toList . patIdents

-- | Binds an array name to the set of soac-produced vars
bindPatVar :: [VName] -> FusionGEnv -> VName -> FusionGEnv
bindPatVar faml env nm = env { soacs       = M.insert nm faml $ soacs env
                             , arrsInScope = S.insert nm      $ arrsInScope env
                             }

bindPatVars :: [VName] -> FusionGEnv -> FusionGEnv
bindPatVars names env = foldl (bindPatVar names) env names

bindPat :: TupIdent -> FusionGM a -> FusionGM a
bindPat pat = do
  let nms = map identName $ S.toList $ patIdents pat
  local $ bindPatVars nms

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

-- | Return a fresh, unique name.  The given @String@ is prepended to
-- the name.
new :: String -> FusionGM VName
new = state . flip newVName

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

fuseProg :: Prog -> Either EnablingOptError (Bool, Prog)
fuseProg prog = do
  let env = FusionGEnv { soacs = M.empty, arrsInScope = S.empty, fusedRes = mkFreshFusionRes, program = prog }
  let funs= progFunctions prog
  ks <- runFusionGatherM prog (mapM fusionGatherFun funs) env
  let ks'    = map cleanFusionResult ks
  let succc = any rsucc ks'
  if not succc
  then return (False, prog)
  else do funs' <- runFusionGatherM prog (zipWithM fuseInFun ks' funs) env
          return (True, Prog funs')

fusionGatherFun :: FunDec -> FusionGM FusedRes
fusionGatherFun (_, _, _, body, _) = fusionGatherExp mkFreshFusionRes body

fuseInFun :: FusedRes -> FunDec -> FusionGM FunDec
fuseInFun res (fnm, rtp, idds, body, pos) = do
  body' <- bindRes res $ fuseInExp body
  return (fnm, rtp, idds, body', pos)


---------------------------------------------------
---------------------------------------------------
---- RESULT's Data Structure
---------------------------------------------------
---------------------------------------------------

-- | A type used for (hopefully) uniquely referring a producer SOAC.
-- The uniquely identifying value is the name of the first array
-- returned from the SOAC.
newtype KernName = KernName { unKernName :: VName }
  deriving (Eq, Ord, Show)

data SOAC = SOACMap2 Certificates TupleLambda [Exp] [Type] SrcLoc
          | SOACReduce2  Certificates TupleLambda [Exp] [Exp] [Type] SrcLoc
          | SOACScan2 Certificates TupleLambda [Exp] [Exp] [Type] SrcLoc
          | SOACFilter2 Certificates TupleLambda [Exp] SrcLoc
          | SOACRedomap2 Certificates TupleLambda TupleLambda [Exp] [Exp] [Type] SrcLoc

instance Located SOAC where
  locOf (SOACMap2 _ _ _ _ loc) = locOf loc
  locOf (SOACReduce2 _ _ _ _ _ loc) = locOf loc
  locOf (SOACScan2 _ _ _ _ _ loc) = locOf loc
  locOf (SOACFilter2 _ _ _ loc) = locOf loc
  locOf (SOACRedomap2 _ _ _ _ _ _ loc) = locOf loc

-- | Returns the input arrays used in a SOAC.
getInputsSOAC :: SOAC -> [Exp]
getInputsSOAC (SOACMap2 _     _     arrs _ _) = arrs
getInputsSOAC (SOACReduce2  _ _ _   arrs _ _) = arrs
getInputsSOAC (SOACScan2    _ _ _   arrs _ _) = arrs
getInputsSOAC (SOACFilter2  _ _     arrs _  ) = arrs
getInputsSOAC (SOACRedomap2 _ _ _ _ arrs _ _) = arrs

getLamSOAC :: SOAC -> TupleLambda
getLamSOAC (SOACMap2     _ lam _    _ _    ) = lam
getLamSOAC (SOACReduce2  _ lam _    _ _ _  ) = lam
getLamSOAC (SOACScan2    _ lam _    _ _ _  ) = lam
getLamSOAC (SOACFilter2  _ lam _    _      ) = lam
getLamSOAC (SOACRedomap2 _ _   lam2 _ _ _ _) = lam2

updateLamSOAC :: TupleLambda -> SOAC -> SOAC
updateLamSOAC lam (SOACMap2     cs         _    arrs eltp loc) =
  SOACMap2     cs          lam    arrs eltp loc
updateLamSOAC lam (SOACReduce2  cs         _ ne arrs eltp loc) =
  SOACReduce2  cs      lam ne arrs eltp loc
updateLamSOAC lam (SOACScan2    cs         _ ne arrs eltp loc) =
  SOACScan2  cs      lam ne arrs eltp loc
updateLamSOAC lam (SOACFilter2  cs         _    arrs      loc) =
  SOACFilter2  cs      lam    arrs      loc
updateLamSOAC lam (SOACRedomap2 cs lam1    _ ne arrs eltp loc) =
  SOACRedomap2 cs lam1 lam ne arrs eltp loc

-- | Returns the certificates used in a SOAC.
getCertsSOAC :: SOAC -> Certificates
getCertsSOAC (SOACMap2     cs _     _ _ _) = cs
getCertsSOAC (SOACReduce2  cs _ _   _ _ _) = cs
getCertsSOAC (SOACScan2    cs _ _   _ _ _) = cs
getCertsSOAC (SOACFilter2  cs _     _ _  ) = cs
getCertsSOAC (SOACRedomap2 cs _ _ _ _ _ _) = cs

soacToExp :: SOAC -> Exp
soacToExp (SOACMap2 cs l as t loc) = Map2 cs l as t loc
soacToExp (SOACReduce2 cs l es as ts loc) = Reduce2 cs l es as ts loc
soacToExp (SOACScan2 cs l es as ts loc) = Scan2 cs l es as ts loc
soacToExp (SOACFilter2 cs l es loc) = Filter2 cs l es loc
soacToExp (SOACRedomap2 cs l1 l2 es as ts loc) = Redomap2 cs l1 l2 es as ts loc

expToSOAC :: Exp -> Maybe SOAC
expToSOAC (Map2 cs l as t loc) = Just $ SOACMap2 cs l as t loc
expToSOAC (Reduce2 cs l es as ts loc) = Just $ SOACScan2 cs l es as ts loc
expToSOAC (Scan2 cs l es as ts loc) = Just $ SOACScan2 cs l es as ts loc
expToSOAC (Filter2 cs l es loc) = Just $ SOACFilter2 cs l es loc
expToSOAC (Redomap2 cs l1 l2 es as ts loc) = Just $ SOACRedomap2 cs l1 l2 es as ts loc
expToSOAC _ = Nothing

data FusedKer = FusedKer {
                  fsoac      :: (TupIdent, SOAC)
                -- ^ the fused SOAC statement, e.g.,
                -- (z,w) = map2( f(a,b), x, y )

                , inp        :: S.Set Ident
                -- ^ the input arrays used in the `soac'
                -- stmt, i.e., `x', `y'.

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

  , outArr     :: M.Map VName KernName
  -- ^ Associates an array to the name of the
  -- SOAC kernel that has produced it.

  , inpArr     :: M.Map VName (S.Set KernName)
  -- ^ Associates an array to the names of the
  -- SOAC kernels that uses it. These sets include
  -- only the SOAC input arrays used as full variables, i.e., no `a[i]'.

  , unfusable  :: S.Set VName
  -- ^ the (names of) arrays that are not fusable, i.e.,
  --
  --   1. they are either used other than input to SOAC kernels, or
  --
  --   2. are used as input to at least two different kernels that
  --      are not located on disjoint control-flow branches, or
  --
  --   3. are used in the lambda expression of SOACs

  , kernels    :: M.Map KernName FusedKer
  -- ^ The map recording the uses
  }

isOmapKer :: FusedKer -> Bool
isOmapKer ker =
    let (_, soac) = fsoac ker
    in case soac of
        SOACReduce2 {} -> True
        SOACRedomap2{} -> True
        SOACMap2    {} -> True
        _              -> False


isInpArrInResModKers :: FusedRes -> S.Set KernName -> VName -> Bool
isInpArrInResModKers ress kers nm =
  case M.lookup nm (inpArr ress) of
    Nothing -> False
    Just s  -> not $ S.null $ s `S.difference` kers

getKersWithInpArrs :: FusedRes -> [VName] -> S.Set KernName
getKersWithInpArrs ress =
  S.unions . mapMaybe (`M.lookup` inpArr ress)

-- | extend the set of names to include all the names
--     produced via SOACs (by querring the vtable's soac)
expandSoacInpArr :: [VName] -> FusionGM [VName]
expandSoacInpArr =
    foldM (\y nm -> do bnd <- asks $ M.lookup nm . soacs
                       case bnd of
                         Nothing  -> return (y++[nm])
                         Just nns -> return (y++nns )
          ) []

----------------------------------------------------------------------
----------------------------------------------------------------------

soacInputs :: SOAC -> FusionGM ([VName], [VName])
soacInputs soac = do
  (inp_idds, other_idds) <- getIdentArr $ getInputsSOAC soac
  let (inp_nms0,other_nms0) = (map identName inp_idds, map identName other_idds)
  inp_nms   <- expandSoacInpArr   inp_nms0
  other_nms <- expandSoacInpArr other_nms0
  return (inp_nms, other_nms)

addNewKer :: FusedRes -> (TupIdent, SOAC) -> FusionGM FusedRes
addNewKer res (idd, soac) = do
  (inp_nms, other_nms) <- soacInputs soac

  let used_inps = filter (isInpArrInResModKers res S.empty) inp_nms
  let ufs = S.unions [unfusable res, S.fromList used_inps, S.fromList other_nms]

  addNewKerWithUnfusable res (idd, soac) ufs

addNewKerWithUnfusable :: FusedRes -> (TupIdent, SOAC) -> S.Set VName -> FusionGM FusedRes
addNewKerWithUnfusable res (idd, soac) ufs = do
  (inp_idds, _) <- getIdentArr $ getInputsSOAC soac
  nm_ker <- KernName <$> new "ker"
  let inp_nms0 = map identName inp_idds
      new_ker = FusedKer (idd, soac) (S.fromList inp_idds) S.empty []
      out_nms = patNames idd
      os' = S.foldl (\x arr -> M.insert arr nm_ker x)
            (outArr res) out_nms
      is' = foldl (\x arr -> M.insertWith' S.union arr (S.singleton nm_ker) x)
            (inpArr res) inp_nms0
  return $ FusedRes (rsucc res) os' is' ufs
           (M.insert nm_ker new_ker (kernels res))

-- map, reduce, redomap
greedyFuse :: Bool -> S.Set VName -> FusedRes -> (TupIdent, SOAC) -> FusionGM FusedRes
greedyFuse is_repl lam_used_nms res (idd, soac) = do
    -- Assumption: the free vars in lambda are already in
    -- 'unfusable res'.
    (inp_nms, other_nms) <- soacInputs soac

    let out_idds     = S.toList $ patIdents idd
    let out_nms      = map identName out_idds
    -- Conditions for fusion:
    --   (i) none of `out_idds' belongs to the unfusable set.
    --  (ii) there are some kernels that use some of `out_idds' as inputs
    let isUnfusable    = (`S.member` unfusable res)
        not_unfusable  = is_repl || not (any isUnfusable out_nms)
        to_fuse_knmSet = getKersWithInpArrs res out_nms
        to_fuse_knms   = S.toList to_fuse_knmSet
        lookup_kern k  = case M.lookup k (kernels res) of
                           Nothing  -> badFusionGM $ EnablingOptError (srclocOf soac)
                                       ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                        ++ "kernel name not found in kernels field!")
                           Just ker -> return ker

    to_fuse_kers <- mapM lookup_kern to_fuse_knms

    -- all kernels has to be compatible for fusion, e.g., if
    -- the kernel is a map, and the current soac is a filter,
    -- then they cannot be fused
    ok_kers_compat <- allM (isCompatibleKer (out_nms,soac)) to_fuse_kers

    -- check whether fusing @soac@ will violate any in-place update
    --    restriction, e.g., would move an input array past its in-place update.
    let all_used_names = S.toList $ S.unions [lam_used_nms, S.fromList inp_nms, S.fromList other_nms]
        has_inplace ker = any (`S.member` inplace ker) all_used_names
        ok_inplace = not $ any has_inplace to_fuse_kers

    -- compute whether @soac@ is fusable or not
    let is_fusable = not_unfusable && not (null to_fuse_kers) && ok_inplace && ok_kers_compat

    --  (i) inparr ids other than vars will be added to unfusable list,
    -- (ii) will also become part of the unfusable set the inparr vars
    --         that also appear as inparr of another kernel,
    --         BUT which said kernel is not the one we are fusing with (now)!
    let mod_kerS  = if is_fusable then to_fuse_knmSet else S.empty
    let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
    let ufs       = S.unions [unfusable res, S.fromList used_inps, S.fromList other_nms]
    let comb      = M.unionWith S.union

    if not is_fusable then
      if is_repl then return res
      else -- nothing to fuse, add a new soac kernel to the result
        addNewKerWithUnfusable res (idd, soac) ufs
     else do -- ... fuse current soac into to_fuse_kers ...
       fused_kers <- mapM (fuseSOACwithKer (out_idds, soac)) to_fuse_kers
       -- Need to suitably update `inpArr':
       --   (i) first remove the inpArr bindings of the old kernel
       --  (ii) then add the inpArr bindings of the new kernel
       let inpArr' =
             foldl (\inpa (kold, knew, knm) ->
                      let inpa' =
                            S.foldl (\inpp nm ->
                                     case M.lookup nm inpp of
                                       Nothing -> inpp
                                       Just s  -> let new_set = S.delete knm s
                                                  in if S.null new_set
                                                     then M.delete nm         inpp
                                                     else M.insert nm new_set inpp
                                    )
                            inpa $ S.map identName $ inp kold
                      in M.fromList [ (k, S.singleton knm)
                                        | k <- S.toList $ S.map identName (inp knew) ]
                           `comb` inpa')
             (inpArr res) (zip3 to_fuse_kers fused_kers to_fuse_knms)
       -- Update the kernels map
       let kernels' = M.fromList (zip to_fuse_knms fused_kers)
                      `M.union` kernels res

       -- nothing to do for `outArr' (since we have not added a new kernel)
       return $ FusedRes True (outArr res) inpArr' ufs kernels'

fuseSOACwithKer :: ([Ident], SOAC) -> FusedKer -> FusionGM FusedKer
fuseSOACwithKer (out_ids1, soac1) ker = do
  -- We are fusing soac1 into soac2, i.e, the output of soac1 is going
  -- into soac2.
  let (out_ids2, soac2) = fsoac ker
      cs1      = getCertsSOAC soac1
      cs2      = getCertsSOAC soac2
  case (soac2, soac1) of
      -- first get rid of the cases that can be solved by
      -- a bit of soac rewriting.
    (SOACReduce2 _ lam ne arrs rwtps loc, SOACMap2   {}) -> do
      let soac2' = SOACRedomap2 (cs1++cs2) lam lam ne arrs rwtps loc
          ker'   = ker { fsoac = (out_ids2, soac2') }
      fuseSOACwithKer (out_ids1, soac1) ker'
    _ -> do -- treat the complicated cases!
            let inp1_arr = getInputsSOAC soac1
                inp2_arr = getInputsSOAC soac2
                lam1     = getLamSOAC soac1
                lam2     = getLamSOAC soac2

            (res_soac, res_inp) <-
              case (soac2,soac1) of
                ----------------------------------------------------
                -- The Fusions that are semantically map fusions:
                ----------------------------------------------------
                (SOACMap2 _ _ _ _ pos, SOACMap2    {}) -> do
                  let (res_lam, new_inp) = fuseMaps lam1 inp1_arr out_ids1 lam2 inp2_arr
                  return (SOACMap2 (cs1++cs2) res_lam new_inp (mkElType new_inp) pos, new_inp)
                (SOACRedomap2 _ lam21 _ ne _ _ pos, SOACMap2 {})-> do
                  let (res_lam, new_inp) = fuseMaps lam1 inp1_arr out_ids1 lam2 inp2_arr
                  return (SOACRedomap2 (cs1++cs2) lam21 res_lam ne new_inp (mkElType new_inp) pos, new_inp)

                ----------------------------------------------------
                -- The Fusions that are semantically filter fusions:
                ----------------------------------------------------
                (SOACReduce2 _ _ ne _ eltp pos, SOACFilter2 {}) -> do
                  name <- new "check"
                  let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr out_ids1 lam2 inp2_arr name
                  return (SOACReduce2 (cs1++cs2) res_lam ne new_inp eltp pos, new_inp)
                (SOACRedomap2 _ lam21 _ nes _ eltp pos, SOACFilter2 {}) -> do
                  name <- new "check"
                  let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr out_ids1 lam2 inp2_arr name
                  return (SOACRedomap2 (cs1++cs2) lam21 res_lam nes new_inp eltp pos, new_inp)
                (SOACFilter2 _ _ _ pos, SOACFilter2 {}) -> do
                  name <- new "check"
                  let (res_lam, new_inp) = fuseFilters lam1 inp1_arr out_ids1 lam2 inp2_arr name
                  return (SOACFilter2 (cs1++cs2) res_lam new_inp pos, new_inp)

                ----------------------------------------------------
                -- Unfusable: should not have reached here!!!
                ----------------------------------------------------
                _ -> badFusionGM $ EnablingOptError (srclocOf soac1)
                                    ("In Fusion.hs, fuseSOACwithKer: fusion not supported "
                                     ++ "(soac2,soac1): (" ++ ppExp (soacToExp soac2)++", "++ppExp (soacToExp soac1))
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

fusionGatherExp fres (LetPat pat e body _)
  | Just soac <- expToSOAC e =
      case soac of
        SOACMap2 _ lam _ _ _ -> do
          bres  <- bindPat pat $ fusionGatherExp fres body
          (used_lam, blres) <- fusionGatherLam (S.empty, bres) lam
          greedyFuse False used_lam blres (pat, soac)

        SOACFilter2 _ lam _ _ -> do
          bres  <- bindPat pat $ fusionGatherExp fres body
          (used_lam, blres) <- fusionGatherLam (S.empty, bres) lam
          greedyFuse False used_lam blres (pat, soac)

        SOACReduce2 _ lam nes _ _ loc -> do
          -- a reduce always starts a new kernel
          bres  <- bindPat pat $ fusionGatherExp fres body
          bres' <- fusionGatherExp bres $ TupLit nes loc
          (_, blres) <- fusionGatherLam (S.empty, bres') lam
          addNewKer blres (pat, soac)

        SOACRedomap2 _ outer_red inner_red ne _ _ loc -> do
          -- a redomap always starts a new kernel
          (_, lres)  <- foldM fusionGatherLam (S.empty, fres) [outer_red, inner_red]
          bres  <- bindPat pat $ fusionGatherExp lres body
          bres' <- fusionGatherExp bres $ TupLit ne loc
          addNewKer bres' (pat, soac)

        SOACScan2 _ lam nes arrs _ _ -> do
          -- NOT FUSABLE
          (_, lres)  <- fusionGatherLam (S.empty, fres) lam
          bres  <- binding pat $ fusionGatherExp lres body
          foldM fusionGatherExp bres (nes++arrs)

fusionGatherExp fres (LetPat pat (Replicate n el loc) body _) = do
    bres <- bindPat pat $ fusionGatherExp fres body
    -- Implemented inplace: gets the variables in `n` and `el`
    (used_set, bres') <- getUnfusableSet loc bres [n,el]
    repl_idnm <- new "repl_x"
    let repl_id = Ident repl_idnm (Elem Int) loc
        (lame, rwt) = case typeOf el of
                        Elem (Tuple ets) -> (el, ets)
                        t                -> (TupLit [el] loc, [t])
        repl_lam = TupleLambda [toParam repl_id] lame (map toDecl rwt) loc
        soac_repl= SOACMap2 [] repl_lam [Iota n loc] rwt loc
    greedyFuse True used_set bres' (pat, soac_repl)

fusionGatherExp fres (LetPat pat e body _) = do
    let pat_vars = map Var $ S.toList $ patIdents pat
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
    bres  <- bindingIdents [id1] $ fusionGatherExp fres body

    let pat_vars = [Var id0, Var id1]
    fres' <- foldM fusionGatherExp bres (elm : inds ++ pat_vars)
    let (ker_nms, kers) = unzip $ M.toList $ kernels fres'

    -- Now add the aliases of id0 (itself included) to the `inplace'
    -- field of any existent kernel.
    let inplace_aliases = S.toList $ aliases $ typeOf $ Var id0
    let kers' = map (\ k -> let inplace' = foldl (flip S.insert) (inplace k) inplace_aliases
                            in  k { inplace = inplace' }
                    ) kers
    let new_kernels = M.fromList $ zip ker_nms kers'
    return $ fres' { kernels = new_kernels }

fusionGatherExp fres (DoLoop merge_pat ini_val _ ub loop_body let_body _) = do
    letbres <- binding merge_pat $ fusionGatherExp fres let_body

    let pat_vars = map Var $ S.toList $ patIdents merge_pat
    fres' <- foldM fusionGatherExp letbres (ini_val:ub:pat_vars)

    let null_res = mkFreshFusionRes
    new_res <- binding merge_pat $ fusionGatherExp null_res loop_body
    -- make the inpArr unfusable, so that they
    -- cannot be fused from outside the loop:
    let (inp_arrs, _) = unzip $ M.toList $ inpArr new_res
    let new_res' = new_res { unfusable = foldl (flip S.insert) (unfusable new_res) inp_arrs }
    -- merge new_res with fres'
    return $ unionFusionRes new_res' fres'

fusionGatherExp fres (If cond e_then e_else _ _) = do
    let null_res = mkFreshFusionRes
    then_res <- fusionGatherExp null_res e_then
    else_res <- fusionGatherExp null_res e_else
    let both_res = unionFusionRes then_res else_res
    fres'    <- fusionGatherExp fres cond
    mergeFusionRes fres' both_res

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

-- Lambdas create a new scope.  Disallow fusing from outside lambda by
-- adding inp_arrs to the unfusable set.
fusionGatherLam :: (S.Set VName, FusedRes) -> TupleLambda -> FusionGM (S.Set VName, FusedRes)
fusionGatherLam (u_set,fres) (TupleLambda idds body _ _) = do
    let null_res = mkFreshFusionRes
    new_res <- bindingIdents (map fromParam idds) $ fusionGatherExp null_res body
    -- make the inpArr unfusable, so that they
    -- cannot be fused from outside the lambda:
    let inp_arrs = S.fromList $ M.keys $ inpArr new_res
    let unfus = unfusable new_res `S.union` inp_arrs
    bnds <- asks arrsInScope
    let unfus'  = unfus `S.intersection` bnds
    -- merge fres with new_res'
    let new_res' = new_res { unfusable = unfus' }
    -- merge new_res with fres'
    return (u_set `S.union` unfus', unionFusionRes new_res' fres)

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

fuseInExp (LetPat pat e body pos) =
  case expToSOAC e of
    Just (SOACScan2 {}) -> do
      -- NOT FUSABLE
      body' <- fuseInExp body
      soac' <- fuseInExp e
      return $ LetPat pat soac' body' pos
    Just soac -> do
      body' <- fuseInExp body
      soac' <- replaceSOAC pat soac
      return $ LetPat pat soac' body' pos
    _ -> do
      body' <- fuseInExp body
      e'    <- fuseInExp e
      return $ LetPat pat e' body' pos

-- Errors: regular SOAC, because they cannot appear in prg.  The
-- tuple-SOACs can appear if they are not used in fusion
fuseInExp (Map      _ _ _     pos) = errorIllegalFus "map"     pos
fuseInExp (Reduce   _ _ _ _   pos) = errorIllegalFus "reduce"  pos
fuseInExp (Scan     _ _ _ _   pos) = errorIllegalFus "scan"    pos
fuseInExp (Filter   _ _ _     pos) = errorIllegalFus "filter"  pos
fuseInExp (Redomap  _ _ _ _ _ pos) = errorIllegalFus "redomap"  pos

fuseInExp e = mapExpM fuseIn e
  where fuseIn = identityMapper {
                   mapOnExp         = fuseInExp
                 , mapOnTupleLambda = fuseInLambda
                 }

fuseInLambda :: TupleLambda -> FusionGM TupleLambda
fuseInLambda (TupleLambda params body rtp pos) = do
  body' <- fuseInExp body
  return $ TupleLambda params body' rtp pos

replaceSOAC :: TupIdent -> SOAC -> FusionGM Exp
replaceSOAC pat soac = do
  fres  <- asks fusedRes
  let loc     = srclocOf soac
  let pat_nm  = identName $ head $ S.toList $ patIdents pat
  case M.lookup pat_nm (outArr fres) of
      Nothing  -> fuseInExp $ soacToExp soac
      Just knm ->
        case M.lookup knm (kernels fres) of
          Nothing  -> badFusionGM $ EnablingOptError loc
                                     ("In Fusion.hs, replaceSOAC, outArr in ker_name "
                                      ++"which is not in Res: "++textual (unKernName knm))
          Just ker -> do
            let (pat', new_soac) = fsoac ker
            if pat /= pat'
            then badFusionGM $ EnablingOptError loc
                                ("In Fusion.hs, replaceSOAC, "
                                 ++" pat does not match kernel's pat: "++ppTupId pat)
            else if null $ fusedVars ker
                 then badFusionGM $ EnablingOptError loc
                                     ("In Fusion.hs, replaceSOAC, unfused kernel "
                                      ++"still in result: "++ppTupId pat)
                 -- then fuseInExp soac
                 else do -- TRY MOVE THIS TO OUTER LEVEL!!!
                         let lam = getLamSOAC new_soac
                         nmsrc <- get
                         prog  <- asks program
                         case normCopyOneTupleLambda prog nmsrc lam of
                            Left err             -> badFusionGM err
                            Right (nmsrc', lam') -> do
                              put nmsrc'
                              (_, nfres) <- fusionGatherLam (S.empty, mkFreshFusionRes) lam'
                              let nfres' =  cleanFusionResult nfres
                              lam''      <- bindRes nfres' $ fuseInLambda lam'
                              return $ soacToExp $ updateLamSOAC lam'' new_soac

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
                  ("In Fusion.hs, soac "++soac_name++" appears illegally in pgm!")

errorIllegalFus :: String -> SrcLoc -> FusionGM Exp
errorIllegalFus soac_name pos =
    badFusionGM $ EnablingOptError pos
                  ("In Fusion.hs, soac "++soac_name++" appears illegally in pgm!")

--------------------------------------------
--------------------------------------------
--- FUSING 2 COMPATIBLE SOACs:           ---
--------------------------------------------
--------------------------------------------

isCompatibleKer :: ([VName], SOAC) -> FusedKer -> FusionGM Bool
isCompatibleKer (_,       SOACMap2    {}) ker = return $ isOmapKer ker
isCompatibleKer (out_nms, SOACFilter2 {}) ker = do
    let (_, soac) = fsoac ker
    let ok = case soac of
                SOACReduce2 {} -> True
                SOACRedomap2{} -> True
                SOACFilter2 {} -> True
                SOACMap2    {} -> False
                SOACScan2   {} -> False
    if not ok
    then return False
    else do -- check that the input-array set of consumer is included
            -- in the output-array set of producer.  That is, a
            -- filter-producer can only be fused if the consumer
            -- accepts input from no other source.
            (inp_idds2, other_idds2) <- getIdentArr $ getInputsSOAC soac
            let inp_lst = map identName inp_idds2
            return $ null other_idds2 && all (`elem` out_nms) inp_lst
isCompatibleKer _ _ = return False
