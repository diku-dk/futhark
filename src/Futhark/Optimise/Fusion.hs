{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Optimise.Fusion ( fuseProg )
  where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader

import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Loc

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Optimise.SimpleOpts
import Futhark.Optimise.Fusion.LoopKernel
import Futhark.HORepresentation.SOAC (SOAC)
import Futhark.Binder
import qualified Futhark.HORepresentation.SOAC as SOAC

data FusionGEnv = FusionGEnv {
    soacs      :: HM.HashMap VName [VName]
  -- ^ Mapping from variable name to its entire family.
  , arrsInScope:: HM.HashMap VName (Maybe (Ident, SOAC.ArrayTransforms))
  , fusedRes   :: FusedRes
  , program    :: Prog
  }

newtype FusionGM a = FusionGM (StateT VNameSource (ReaderT FusionGEnv (Either Error)) a)
  deriving (Monad, Applicative, Functor,
            MonadState VNameSource, MonadReader FusionGEnv)

instance MonadFreshNames FusionGM where
  getNameSource = get
  putNameSource = put

------------------------------------------------------------------------
--- Monadic Helpers: bind/new/runFusionGatherM, etc                      ---
------------------------------------------------------------------------

arrayTransforms :: Ident -> FusionGM (Ident, SOAC.ArrayTransforms)
arrayTransforms v = do
  v' <- asks $ HM.lookup (identName v) . arrsInScope
  case v' of Just (Just res) -> return res
             _               -> return (v, SOAC.noTransforms)

-- | Binds an array name to the set of used-array vars
bindVar :: FusionGEnv -> VName -> FusionGEnv
bindVar env name =
  env { arrsInScope = HM.insert name Nothing $ arrsInScope env }

bindVars :: FusionGEnv -> [VName] -> FusionGEnv
bindVars = foldl bindVar

binding :: [Ident] -> FusionGM a -> FusionGM a
binding idds = local (`bindVars` namesOfArrays idds)
  where namesOfArrays = map identName . filter (not . basicType . identType)

-- | Binds an array name to the set of soac-produced vars
bindingFamilyVar :: [VName] -> FusionGEnv -> VName -> FusionGEnv
bindingFamilyVar faml env nm =
  env { soacs       = HM.insert nm faml    $ soacs env
      , arrsInScope = HM.insert nm Nothing $ arrsInScope env
      }

bindingFamily :: [Ident] -> FusionGM a -> FusionGM a
bindingFamily pat = local $ \env -> foldl (bindingFamilyVar names) env names
  where names = map identName pat

bindingTransform :: Ident -> Ident -> SOAC.ArrayTransform -> FusionGM a -> FusionGM a
bindingTransform v src trns = local $ \env ->
  case HM.lookup srcname $ arrsInScope env of
    Just (Just (src', ts)) ->
      env { arrsInScope =
              HM.insert vname (Just (src', ts SOAC.|> trns)) $ arrsInScope env
          }
    Just Nothing ->
      env { arrsInScope =
              HM.insert vname (Just (src, SOAC.singleTransform trns))
                  $ arrsInScope env
          }
    _ -> bindVar env vname
  where vname   = identName v
        srcname = identName src

-- | Binds the fusion result to the environment.
bindRes :: FusedRes -> FusionGM a -> FusionGM a
bindRes rrr = local (\x -> x { fusedRes = rrr })

-- | The fusion transformation runs in this monad.  The mutable
-- state refers to the fresh-names engine.
-- The reader hides the vtable that associates ... to ... (fill in, please).
-- The 'Either' monad is used for error handling.
runFusionGatherM :: VNameSource -> FusionGM a -> FusionGEnv -> Either Error (a, VNameSource)
runFusionGatherM src (FusionGM a) =
  runReaderT (runStateT a src)

badFusionGM :: Error -> FusionGM a
badFusionGM = FusionGM . lift . lift . Left

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

fuseProg :: Prog -> Either Error Prog
fuseProg prog = do
  let env  = FusionGEnv { soacs = HM.empty
                        , arrsInScope = HM.empty
                        , fusedRes = mkFreshFusionRes
                        , program = prog
                        }
      funs = progFunctions prog
      src  = newNameSourceForProg prog
  (ks,src') <- runFusionGatherM src (mapM fusionGatherFun funs) env
  let ks'    = map cleanFusionResult ks
  let succc = any rsucc ks'
  if not succc
  then return prog
  else do (funs',_) <- runFusionGatherM src' (zipWithM fuseInFun ks' funs) env
          return $ Prog funs'

fusionGatherFun :: FunDec -> FusionGM FusedRes
fusionGatherFun (_, _, _, body, _) = fusionGatherBody mkFreshFusionRes body

fuseInFun :: FusedRes -> FunDec -> FusionGM FunDec
fuseInFun res (fnm, rtp, idds, body, loc) = do
  body' <- bindRes res $ fuseInBody body
  return (fnm, rtp, idds, body', loc)


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

instance Hashable KernName where
  hashWithSalt salt = hashWithSalt salt . unKernName

data FusedRes = FusedRes {
    rsucc :: Bool
  -- ^ Whether we have fused something anywhere.

  , outArr     :: HM.HashMap VName KernName
  -- ^ Associates an array to the name of the
  -- SOAC kernel that has produced it.

  , inpArr     :: HM.HashMap VName (HS.HashSet KernName)
  -- ^ Associates an array to the names of the
  -- SOAC kernels that uses it. These sets include
  -- only the SOAC input arrays used as full variables, i.e., no `a[i]'.

  , unfusable  :: HS.HashSet VName
  -- ^ the (names of) arrays that are not fusable, i.e.,
  --
  --   1. they are either used other than input to SOAC kernels, or
  --
  --   2. are used as input to at least two different kernels that
  --      are not located on disjoint control-flow branches, or
  --
  --   3. are used in the lambda expression of SOACs

  , kernels    :: HM.HashMap KernName FusedKer
  -- ^ The map recording the uses
  }

isInpArrInResModKers :: FusedRes -> HS.HashSet KernName -> VName -> Bool
isInpArrInResModKers ress kers nm =
  case HM.lookup nm (inpArr ress) of
    Nothing -> False
    Just s  -> not $ HS.null $ s `HS.difference` kers

getKersWithInpArrs :: FusedRes -> [VName] -> HS.HashSet KernName
getKersWithInpArrs ress =
  HS.unions . mapMaybe (`HM.lookup` inpArr ress)

-- | extend the set of names to include all the names
--     produced via SOACs (by querring the vtable's soac)
expandSoacInpArr :: [VName] -> FusionGM [VName]
expandSoacInpArr =
    foldM (\y nm -> do bnd <- asks $ HM.lookup nm . soacs
                       case bnd of
                         Nothing  -> return (y++[nm])
                         Just nns -> return (y++nns )
          ) []

----------------------------------------------------------------------
----------------------------------------------------------------------

soacInputs :: SOAC -> FusionGM ([VName], [VName])
soacInputs soac = do
  let (inp_idds, other_idds) = getIdentArr $ SOAC.inputs soac
      (inp_nms0,other_nms0)  = (map identName inp_idds, map identName other_idds)
  inp_nms   <- expandSoacInpArr   inp_nms0
  other_nms <- expandSoacInpArr other_nms0
  return (inp_nms, other_nms)

addNewKer :: FusedRes -> ([Ident], SOAC) -> FusionGM FusedRes
addNewKer res (idd, soac) = do
  (inp_nms, other_nms) <- soacInputs soac

  let used_inps = filter (isInpArrInResModKers res HS.empty) inp_nms
  let ufs = HS.unions [unfusable res, HS.fromList used_inps, HS.fromList other_nms]

  addNewKerWithUnfusable res (idd, soac) ufs

addNewKerWithUnfusable :: FusedRes -> ([Ident], SOAC) -> HS.HashSet VName -> FusionGM FusedRes
addNewKerWithUnfusable res (idd, soac) ufs = do
  nm_ker <- KernName <$> newVName "ker"
  let new_ker = newKernel idd soac
      out_nms = map identName idd
      comb    = HM.unionWith HS.union
      os' = HM.fromList [(arr,nm_ker) | arr <- out_nms]
            `HM.union` outArr res
      is' = HM.fromList [(identName arr,HS.singleton nm_ker)
                         | arr <- mapMaybe SOAC.inputArray $ SOAC.inputs soac]
            `comb` inpArr res
  return $ FusedRes (rsucc res) os' is' ufs
           (HM.insert nm_ker new_ker (kernels res))

inlineSOACInput :: SOAC.Input -> FusionGM SOAC.Input
inlineSOACInput (SOAC.Input ts (SOAC.Var v)) = do
  (v2, ts2) <- arrayTransforms v
  return $ SOAC.Input (ts2<>ts) (SOAC.Var v2)
inlineSOACInput input = return input

inlineSOACInputs :: SOAC -> FusionGM SOAC
inlineSOACInputs soac = do
  inputs' <- mapM inlineSOACInput $ SOAC.inputs soac
  return $ inputs' `SOAC.setInputs` soac

-- map, reduce, redomap
greedyFuse :: Bool -> HS.HashSet VName -> FusedRes -> ([Ident], SOAC) -> FusionGM FusedRes
greedyFuse is_repl lam_used_nms res (out_idds, orig_soac) = do
  soac <- inlineSOACInputs orig_soac
  -- Assumption: the free vars in lambda are already in
  -- 'unfusable res'.
  (inp_nms, other_nms) <- soacInputs soac

  let out_nms      = map identName out_idds
  -- Conditions for fusion:
  --   (i) none of `out_idds' belongs to the unfusable set.
  --  (ii) there are some kernels that use some of `out_idds' as inputs
  let isUnfusable    = (`HS.member` unfusable res)
      not_unfusable  = is_repl || not (any isUnfusable out_nms)
      to_fuse_knmSet = getKersWithInpArrs res out_nms
      to_fuse_knms   = HS.toList to_fuse_knmSet
      lookup_kern k  = case HM.lookup k (kernels res) of
                         Nothing  -> badFusionGM $ Error (srclocOf soac)
                                     ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                      ++ "kernel name not found in kernels field!")
                         Just ker -> return ker

  to_fuse_kers <- mapM lookup_kern to_fuse_knms

  -- all kernels has to be compatible for fusion, e.g., if
  -- the kernel is a map, and the current soac is a filter,
  -- then they cannot be fused
  (ok_kers_compat, fused_kers) <- do
    kers <- mapM (attemptFusion out_idds soac) to_fuse_kers
    case sequence kers of
      Nothing    -> return (False, [])
      Just kers' -> return (True, kers')

  -- check whether fusing @soac@ will violate any in-place update
  --    restriction, e.g., would move an input array past its in-place update.
  let all_used_names = HS.toList $ HS.unions [lam_used_nms, HS.fromList inp_nms, HS.fromList other_nms]
      has_inplace ker = any (`HS.member` inplace ker) all_used_names
      ok_inplace = not $ any has_inplace to_fuse_kers

  -- compute whether @soac@ is fusable or not
  let is_fusable = not_unfusable && not (null to_fuse_kers) && ok_inplace && ok_kers_compat

  --  (i) inparr ids other than vars will be added to unfusable list,
  -- (ii) will also become part of the unfusable set the inparr vars
  --         that also appear as inparr of another kernel,
  --         BUT which said kernel is not the one we are fusing with (now)!
  let mod_kerS  = if is_fusable then to_fuse_knmSet else HS.empty
  let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
  let ufs       = HS.unions [unfusable res, HS.fromList used_inps,
                             HS.fromList other_nms `HS.difference`
                             HS.fromList (map identName $ mapMaybe SOAC.inputArray $ SOAC.inputs soac)]
  let comb      = HM.unionWith HS.union

  if not is_fusable then
    if is_repl then return res
    else -- nothing to fuse, add a new soac kernel to the result
      addNewKerWithUnfusable res (out_idds, soac) ufs
   else do
     -- Need to suitably update `inpArr':
     --   (i) first remove the inpArr bindings of the old kernel
     --  (ii) then add the inpArr bindings of the new kernel
     let inpArr' =
           foldl (\inpa (kold, knew, knm) ->
                    let inpa' =
                          HS.foldl'
                              (\inpp nm ->
                                 case HM.lookup nm inpp of
                                   Nothing -> inpp
                                   Just s  -> let new_set = HS.delete knm s
                                              in if HS.null new_set
                                                 then HM.delete nm         inpp
                                                 else HM.insert nm new_set inpp
                              )
                          inpa $ HS.map identName $ arrInputs kold
                    in HM.fromList [ (k, HS.singleton knm)
                                      | k <- HS.toList $ HS.map identName (arrInputs knew) ]
                         `comb` inpa')
           (inpArr res) (zip3 to_fuse_kers fused_kers to_fuse_knms)
     -- Update the kernels map
     let kernels' = HM.fromList (zip to_fuse_knms fused_kers)
                    `HM.union` kernels res

     -- nothing to do for `outArr' (since we have not added a new kernel)
     return $ FusedRes True (outArr res) inpArr' ufs kernels'

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
--- Fusion Gather for EXPRESSIONS and BODIES,                        ---
--- i.e., where work is being done:                                  ---
---    i) bottom-up AbSyn traversal (backward analysis)              ---
---   ii) soacs are fused greedily iff does not duplicate computation---
--- E.g., (y1, y2, y3) = mapT(f, x1, x2[i])                          ---
---       (z1, z2)     = mapT(g1, y1, y2)                            ---
---       (q1, q2)     = mapT(g2, y3, z1, a, y3)                     ---
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

fusionGatherBody :: FusedRes -> Body -> FusionGM FusedRes

fusionGatherBody fres (Body (Let pat e:bnds) res)
  | Right soac <- SOAC.fromExp e =
      case soac of
        SOAC.Map _ lam _ _ -> do
          bres  <- bindingFamily pat $ fusionGatherBody fres body
          (used_lam, blres) <- fusionGatherLam (HS.empty, bres) lam
          greedyFuse False used_lam blres (pat, soac)

        SOAC.Filter _ lam _ _ _ -> do
          bres  <- bindingFamily pat $ fusionGatherBody fres body
          (used_lam, blres) <- fusionGatherLam (HS.empty, bres) lam
          greedyFuse False used_lam blres (pat, soac)

        SOAC.Reduce _ lam args _ -> do
          -- a reduce always starts a new kernel
          let nes = map fst args
          bres  <- bindingFamily pat $ fusionGatherBody fres body
          bres' <- foldM fusionGatherExp bres $ map SubExp nes
          (_, blres) <- fusionGatherLam (HS.empty, bres') lam
          addNewKer blres (pat, soac)

        SOAC.Redomap _ outer_red inner_red nes _ _ -> do
          -- a redomap always starts a new kernel
          (_, lres)  <- foldM fusionGatherLam (HS.empty, fres) [outer_red, inner_red]
          bres  <- bindingFamily pat $ fusionGatherBody lres body
          bres' <- foldM fusionGatherExp bres $ map SubExp nes
          addNewKer bres' (pat, soac)

        SOAC.Scan _ lam args _ -> do
          -- NOT FUSABLE (probably), but still add as kernel, as
          -- optimisations like ISWIM may make it fusable.
          let nes = map fst args
          bres  <- bindingFamily pat $ fusionGatherBody fres body
          (used_lam, blres) <- fusionGatherLam (HS.empty, bres) lam
          blres' <- foldM fusionGatherExp blres $ map SubExp nes
          greedyFuse False used_lam blres' (pat, soac)
  where body = Body bnds res

fusionGatherBody _ (Body (Let _ e:_) _)
  | Left (SOAC.InvalidArrayInput inpe) <- SOAC.fromExp e =
    badFusionGM $ Error (srclocOf e)
                  ("In Fusion.hs, "++ppSubExp inpe++" is not valid array input.")

fusionGatherBody fres (Body (Let [v] e:bnds) res)
  | Just (src,trns) <- SOAC.transformFromExp e =
    bindingTransform v src trns $ fusionGatherBody fres $ Body bnds res

fusionGatherBody fres (Body (Let pat (Replicate n el loc):bnds) res) = do
  bres <- bindingFamily pat $ fusionGatherBody fres $ Body bnds res
  -- Implemented inplace: gets the variables in `n` and `el`
  (used_set, bres') <- getUnfusableSet loc bres [SubExp n, SubExp el]
  repl_idnm <- newVName "repl_x"
  let repl_id = Ident repl_idnm (Basic Int) loc
      repl_lam = Lambda [toParam repl_id] (resultBody [] [el] loc)
                 [toConstType $ subExpType el] loc
      soac_repl= SOAC.Map [] repl_lam [SOAC.Input SOAC.noTransforms $ SOAC.Iota n] loc
  greedyFuse True used_set bres' (pat, soac_repl)

fusionGatherBody fres (Body (Let [id1] (Update _ id0 inds elm _):bnds) res) = do
  bres  <- binding [id1] $ fusionGatherBody fres $ Body bnds res

  let pat_vars = [Var id0, Var id1]
  fres' <- foldM fusionGatherSubExp bres (elm : inds ++ pat_vars)
  let (ker_nms, kers) = unzip $ HM.toList $ kernels fres'

  -- Now add the aliases of id0 (itself included) to the `inplace'
  -- field of any existent kernel.
  let inplace_aliases = HS.toList $ aliases $ subExpType $ Var id0
  let kers' = map (\ k -> let inplace' = foldl (flip HS.insert) (inplace k) inplace_aliases
                          in  k { inplace = inplace' }
                  ) kers
  let new_kernels = HM.fromList $ zip ker_nms kers'
  return $ fres' { kernels = new_kernels }

fusionGatherBody fres (Body (Let pat e:bnds) res) = do
    let pat_vars = map (SubExp . Var) pat
    bres <- binding pat $ fusionGatherBody fres $ Body bnds res
    foldM fusionGatherExp bres (e:pat_vars)

fusionGatherBody fres (Body [] _) =
  return fres

fusionGatherExp :: FusedRes -> Exp -> FusionGM FusedRes

-----------------------------------------
---- Index/If    ----
-----------------------------------------

fusionGatherExp fres (DoLoop _ merge _ ub loop_body _) = do
  let (merge_pat, ini_val) = unzip merge

  let pat_vars = map Var merge_pat
  fres' <- foldM fusionGatherSubExp fres (ini_val++ub:pat_vars)

  let null_res = mkFreshFusionRes
  new_res <- binding merge_pat $ fusionGatherBody null_res loop_body
  -- make the inpArr unfusable, so that they
  -- cannot be fused from outside the loop:
  let (inp_arrs, _) = unzip $ HM.toList $ inpArr new_res
  let new_res' = new_res { unfusable = foldl (flip HS.insert) (unfusable new_res) inp_arrs }
  -- merge new_res with fres'
  return $ unionFusionRes new_res' fres'

fusionGatherExp fres (Index _ idd inds _) =
  foldM fusionGatherSubExp fres (Var idd : inds)

fusionGatherExp fres (If cond e_then e_else _ _) = do
    let null_res = mkFreshFusionRes
    then_res <- fusionGatherBody null_res e_then
    else_res <- fusionGatherBody null_res e_else
    let both_res = unionFusionRes then_res else_res
    fres'    <- fusionGatherSubExp fres cond
    mergeFusionRes fres' both_res

-----------------------------------------------------------------------------------
--- Errors: all SOACs, (because normalization ensures they appear
--- directly in let exp, i.e., let x = e)
-----------------------------------------------------------------------------------

fusionGatherExp _ (Map     _ _ _     loc) = errorIllegal "map"    loc
fusionGatherExp _ (Reduce  _ _ _     loc) = errorIllegal "reduce" loc
fusionGatherExp _ (Scan    _ _ _     loc) = errorIllegal "scan"   loc
fusionGatherExp _ (Filter  _ _ _   _ loc) = errorIllegal "filter" loc
fusionGatherExp _ (Redomap _ _ _ _ _ loc) = errorIllegal "redomap" loc

-----------------------------------
---- Generic Traversal         ----
-----------------------------------

fusionGatherExp fres e = do
    let foldstct = identityFolder { foldOnExp = fusionGatherExp
                                  , foldOnSubExp = fusionGatherSubExp
                                  }
    foldExpM foldstct fres e

fusionGatherSubExp :: FusedRes -> SubExp -> FusionGM FusedRes
fusionGatherSubExp fres (Var idd) = addIdentToUnfusable fres idd
fusionGatherSubExp fres _         = return fres

addIdentToUnfusable :: FusedRes -> Ident -> FusionGM FusedRes
addIdentToUnfusable fres orig_idd
  | Array{} <- identType orig_idd = do
  (idd,_) <- arrayTransforms orig_idd
  return fres { unfusable = HS.insert (identName idd) (unfusable fres) }
addIdentToUnfusable fres _ =
  return fres

-- Lambdas create a new scope.  Disallow fusing from outside lambda by
-- adding inp_arrs to the unfusable set.
fusionGatherLam :: (HS.HashSet VName, FusedRes) -> Lambda -> FusionGM (HS.HashSet VName, FusedRes)
fusionGatherLam (u_set,fres) (Lambda idds body _ _) = do
    let null_res = mkFreshFusionRes
    new_res <- binding (map fromParam idds) $ fusionGatherBody null_res body
    -- make the inpArr unfusable, so that they
    -- cannot be fused from outside the lambda:
    let inp_arrs = HS.fromList $ HM.keys $ inpArr new_res
    let unfus = unfusable new_res `HS.union` inp_arrs
    bnds <- HM.keys <$> asks arrsInScope
    let unfus'  = unfus `HS.intersection` HS.fromList bnds
    -- merge fres with new_res'
    let new_res' = new_res { unfusable = unfus' }
    -- merge new_res with fres'
    return (u_set `HS.union` unfus', unionFusionRes new_res' fres)

getUnfusableSet :: SrcLoc -> FusedRes -> [Exp] -> FusionGM (HS.HashSet VName, FusedRes)
getUnfusableSet loc fres args = do
    -- assuming program is normalized then args
    -- can only contribute to the unfusable set
    let null_res = mkFreshFusionRes
    new_res <- foldM fusionGatherExp null_res args
    if not (HM.null (outArr  new_res)) || not (HM.null (inpArr new_res)) ||
       not (HM.null (kernels new_res)) || rsucc new_res
    then badFusionGM $ Error loc $
                        "In Fusion.hs, getUnfusableSet, broken invariant!"
                        ++ " Unnormalized program: " ++ concatMap ppExp args
    else return ( unfusable new_res,
                  fres { unfusable = unfusable fres `HS.union` unfusable new_res }
                )

-------------------------------------------------------------
-------------------------------------------------------------
--- FINALLY, Substitute the kernels in function
-------------------------------------------------------------
-------------------------------------------------------------

fuseInBody :: Body -> FusionGM Body

fuseInBody (Body (Let pat e:bnds) res) =
  case SOAC.fromExp e of
    Right soac ->
      replaceSOAC pat soac =<< fuseInBody (Body bnds res)
    _ -> do
      Body bnds' res' <- fuseInBody $ Body bnds res
      e'              <- fuseInExp e
      return $ Body (Let pat e':bnds') res'

fuseInBody b = mapBodyM fuseIn b

fuseInExp :: Exp -> FusionGM Exp
fuseInExp = mapExpM fuseIn

fuseIn :: Mapper FusionGM
fuseIn = identityMapper {
           mapOnExp    = fuseInExp
         , mapOnBody   = fuseInBody
         , mapOnLambda = fuseInLambda
         }

fuseInLambda :: Lambda -> FusionGM Lambda
fuseInLambda (Lambda params body rtp loc) = do
  body' <- fuseInBody body
  return $ Lambda params body' rtp loc

replaceSOAC :: [Ident] -> SOAC -> Body -> FusionGM Body
replaceSOAC [] _ body = return body
replaceSOAC names@(Ident pat_nm _ _ : _) soac body = do
  fres  <- asks fusedRes
  let loc    = srclocOf soac
  case HM.lookup pat_nm (outArr fres) of
    Nothing  -> do
      (e,f) <- runBinder' $ SOAC.toExp soac
      e'    <- fuseInExp e
      return $ f $ Let names e' `insertBinding` body
    Just knm ->
      case HM.lookup knm (kernels fres) of
        Nothing  -> badFusionGM $ Error loc
                                   ("In Fusion.hs, replaceSOAC, outArr in ker_name "
                                    ++"which is not in Res: "++textual (unKernName knm))
        Just ker -> do
          when (names /= outputs ker) $
            badFusionGM $ Error loc
                          ("In Fusion.hs, replaceSOAC, "
                           ++" pat does not match kernel's pat: "++ppTuple names)
          when (null $ fusedVars ker) $
            badFusionGM $ Error loc
                          ("In Fusion.hs, replaceSOAC, unfused kernel "
                          ++"still in result: "++ppTuple names)

          insertKerSOAC ker body

insertKerSOAC :: FusedKer -> Body -> FusionGM Body
insertKerSOAC ker body = do
  prog <- asks program
  let new_soac = fsoac ker
  lam' <- normCopyOneLambda prog $ SOAC.lambda new_soac
  (_, nfres) <- fusionGatherLam (HS.empty, mkFreshFusionRes) lam'
  let nfres' =  cleanFusionResult nfres
  lam''      <- bindRes nfres' $ fuseInLambda lam'
  runBinder $ do
    transformOutput (outputTransform ker) (outputs ker) $
                    SOAC.setLambda lam'' new_soac
    return body

---------------------------------------------------
---------------------------------------------------
---- HELPERS
---------------------------------------------------
---------------------------------------------------

-- | Get a new fusion result, i.e., for when entering a new scope,
--   e.g., a new lambda or a new loop.
mkFreshFusionRes :: FusedRes
mkFreshFusionRes =
    FusedRes { rsucc     = False,   outArr = HM.empty, inpArr  = HM.empty,
               unfusable = HS.empty, kernels = HM.empty }

unionFusionRes :: FusedRes -> FusedRes -> FusedRes
unionFusionRes res1 res2 =
    FusedRes  (rsucc     res1       ||      rsucc     res2)
              (outArr    res1    `HM.union`  outArr    res2)
              (HM.unionWith HS.union (inpArr res1) (inpArr res2) )
              (unfusable res1    `HS.union`  unfusable res2)
              (kernels   res1    `HM.union`  kernels   res2)

mergeFusionRes :: FusedRes -> FusedRes -> FusionGM FusedRes
mergeFusionRes res1 res2 = do
    let ufus_mres = unfusable res1 `HS.union` unfusable res2
    inp_both     <- expandSoacInpArr $ HM.keys $ inpArr res1 `HM.intersection` inpArr res2
    let m_unfus   = foldl (flip HS.insert) ufus_mres inp_both
    return $ FusedRes  (rsucc     res1       ||      rsucc     res2)
                       (outArr    res1    `HM.union`  outArr    res2)
                       (HM.unionWith HS.union (inpArr res1) (inpArr res2) )
                       m_unfus
                       (kernels   res1    `HM.union`  kernels   res2)


-- | The expression arguments are supposed to be array-type exps.
--   Returns a tuple, in which the arrays that are vars are in the
--   first element of the tuple, and the one which are indexed or
--   transposes (or otherwise transformed) should be in the second.
--
--   E.g., for expression `mapT(f, a, b[i])', the result should be
--   `([a],[b])'
getIdentArr :: [SOAC.Input] -> ([Ident], [Ident])
getIdentArr = foldl comb ([],[])
  where comb (vs,os) (SOAC.Input ts (SOAC.Var idd))
          | SOAC.nullTransforms ts = (idd:vs, os)
        comb (vs, os) inp =
          (vs, maybeToList (SOAC.inputArray inp)++os)

cleanFusionResult :: FusedRes -> FusedRes
cleanFusionResult fres =
    let newks = HM.filter (not . null . fusedVars)      (kernels fres)
        newoa = HM.filter (`HM.member` newks)            (outArr  fres)
        newia = HM.map    (HS.filter (`HM.member` newks)) (inpArr fres)
    in fres { outArr = newoa, inpArr = newia, kernels = newks }

--------------
--- Errors ---
--------------

errorIllegal :: String -> SrcLoc -> FusionGM FusedRes
errorIllegal soac_name loc =
    badFusionGM $ Error loc
                  ("In Fusion.hs, soac "++soac_name++" appears illegally in pgm!")
