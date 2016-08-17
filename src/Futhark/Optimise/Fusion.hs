{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Optimise.Fusion ( fuseSOACs )
  where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.Hashable
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.List         as L

import Prelude

import Futhark.Optimise.DeadVarElim (deadCodeElim)
import Futhark.Representation.SOACS hiding (SOAC(..))
import qualified Futhark.Representation.SOACS as Futhark
import Futhark.MonadFreshNames
import Futhark.Representation.SOACS.Simplify
import Futhark.Optimise.Fusion.LoopKernel
import Futhark.Binder
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import Futhark.Transform.Rename
import Futhark.Pass

data VarEntry = IsArray VName (NameInfo SOACS) SOAC.Input
              | IsNotArray VName (NameInfo SOACS)

varEntryType :: VarEntry -> NameInfo SOACS
varEntryType (IsArray _ attr _) =
  attr
varEntryType (IsNotArray _ attr) =
  attr

data FusionGEnv = FusionGEnv {
    soacs      :: HM.HashMap VName [VName]
  -- ^ Mapping from variable name to its entire family.
  , varsInScope:: HM.HashMap VName VarEntry
  , fusedRes   :: FusedRes
  }

arrsInScope :: FusionGEnv -> HM.HashMap VName SOAC.Input
arrsInScope = HM.fromList . mapMaybe asArray . HM.toList . varsInScope
  where asArray (name, IsArray _ _ input) =
          Just (name, input)
        asArray (_, IsNotArray{}) =
          Nothing

data Error = Error String

instance Show Error where
  show (Error msg) =
    "Fusion error:\n" ++ msg

newtype FusionGM a = FusionGM (ExceptT Error (StateT VNameSource (Reader FusionGEnv)) a)
  deriving (Monad, Applicative, Functor,
            MonadError Error,
            MonadState VNameSource,
            MonadReader FusionGEnv)

instance MonadFreshNames FusionGM where
  getNameSource = get
  putNameSource = put

instance HasScope SOACS FusionGM where
  askScope = toScope <$> asks varsInScope
    where toScope = HM.map varEntryType

------------------------------------------------------------------------
--- Monadic Helpers: bind/new/runFusionGatherM, etc
------------------------------------------------------------------------

-- | Binds an array name to the set of used-array vars
bindVar :: FusionGEnv -> Ident -> FusionGEnv
bindVar env (Ident name t) =
  env { varsInScope = HM.insert name entry $ varsInScope env }
  where entry = case t of
          Array {} -> IsArray name (LetInfo t) $ SOAC.identInput $ Ident name t
          _        -> IsNotArray name $ LetInfo t

bindVars :: FusionGEnv -> [Ident] -> FusionGEnv
bindVars = foldl bindVar

binding :: [Ident] -> FusionGM a -> FusionGM a
binding vs = local (`bindVars` vs)

gatherBindingPattern :: Pattern -> FusionGM FusedRes -> FusionGM FusedRes
gatherBindingPattern pat m = do
  res <- binding (patternIdents pat) m
  checkForUpdates pat res

bindingPat :: Pattern -> FusionGM a -> FusionGM a
bindingPat = binding . patternIdents

bindingFParams :: [FParam] -> FusionGM a -> FusionGM a
bindingFParams = binding  . map paramIdent

-- | Binds an array name to the set of soac-produced vars
bindingFamilyVar :: [VName] -> FusionGEnv -> Ident -> FusionGEnv
bindingFamilyVar faml env (Ident nm t) =
  env { soacs       = HM.insert nm faml $ soacs env
      , varsInScope = HM.insert nm (IsArray nm (LetInfo t) $
                                    SOAC.identInput $ Ident nm t) $
                      varsInScope env
      }

checkForUpdates :: Pattern -> FusedRes -> FusionGM FusedRes
checkForUpdates pat res = foldM checkForUpdate res $ patternElements pat
  where checkForUpdate res' (PatElem _ BindVar _) =
          return res'
        checkForUpdate res' (PatElem _ (BindInPlace _ src is) _) = do
          res'' <- foldM fusionGatherSubExp res' (Var src : is)
          let aliases = [src]
              inspectKer k =
                let inplace' = foldl (flip HS.insert) (inplace k) aliases
                in  k { inplace = inplace' }
          return $ res'' { kernels = HM.map inspectKer $ kernels res'' }

-- | Updates the environment: (i) the @soacs@ (map) by binding each pattern
--   element identifier to all pattern elements (identifiers) and (ii) the
--   @arrsInScope@ (map) by inserting each (pattern-array) name.
--   Finally, if the binding is an in-place update, then the @inplace@ field
--   of each (result) kernel is updated with the new in-place updates.
bindingFamily :: Pattern -> FusionGM FusedRes -> FusionGM FusedRes
bindingFamily pat m = do
  res <- local bind m
  checkForUpdates pat res
  where idents = patternIdents pat
        family = patternNames pat
        bind env = foldl (bindingFamilyVar family) env idents

bindingTransform :: PatElem -> VName -> SOAC.ArrayTransform -> FusionGM a -> FusionGM a
bindingTransform pe srcname trns = local $ \env ->
  case HM.lookup srcname $ varsInScope env of
    Just (IsArray src' _ input) ->
      env { varsInScope =
              HM.insert vname
              (IsArray src' (LetInfo attr) $ trns `SOAC.addTransform` input) $
              varsInScope env
          }
    _ -> bindVar env $ patElemIdent pe
  where vname = patElemName pe
        attr = patElemAttr pe

-- | Binds the fusion result to the environment.
bindRes :: FusedRes -> FusionGM a -> FusionGM a
bindRes rrr = local (\x -> x { fusedRes = rrr })

-- | The fusion transformation runs in this monad.  The mutable
-- state refers to the fresh-names engine.
-- The reader hides the vtable that associates ... to ... (fill in, please).
-- The 'Either' monad is used for error handling.
runFusionGatherM :: MonadFreshNames m =>
                    FusionGM a -> FusionGEnv -> m (Either Error a)
runFusionGatherM (FusionGM a) env =
  modifyNameSource $ \src -> runReader (runStateT (runExceptT a) src) env

badFusionGM :: Error -> FusionGM a
badFusionGM = throwError

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass { passName = "Fuse SOACs"
       , passDescription = "Perform higher-order optimisation, i.e., fusion."
       , passFunction = fuseProg
       }

fuseProg :: Prog -> PassM Prog
fuseProg prog = do
  let env  = FusionGEnv { soacs = HM.empty
                        , varsInScope = HM.empty
                        , fusedRes = mkFreshFusionRes
                        }
      funs = progFunctions prog
  ks <- liftEitherM $ runFusionGatherM (mapM fusionGatherFun funs) env
  let ks'    = map cleanFusionResult ks
  let succc = any rsucc ks'
  if not succc
  then return prog
  else do funs' <- liftEitherM $ runFusionGatherM (zipWithM fuseInFun ks' funs) env
          deadCodeElim <$> renameProg (Prog funs')

fusionGatherFun :: FunDef -> FusionGM FusedRes
fusionGatherFun fundec =
  bindingFParams (funDefParams fundec) $
  fusionGatherBody mkFreshFusionRes $ funDefBody fundec

fuseInFun :: FusedRes -> FunDef -> FusionGM FunDef
fuseInFun res fundec = do
  body' <- bindingFParams (funDefParams fundec) $
           bindRes res $
           fuseInBody $ funDefBody fundec
  return $ fundec { funDefBody = body' }

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

  , infusible  :: Names
  -- ^ the (names of) arrays that are not fusible, i.e.,
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
      (inp_nms0, other_nms0) = (inp_idds, other_idds)
  inp_nms   <- expandSoacInpArr   inp_nms0
  other_nms <- expandSoacInpArr other_nms0
  return (inp_nms, other_nms)

addNewKerWithInfusible :: FusedRes -> ([Ident], SOAC) -> Names -> FusionGM FusedRes
addNewKerWithInfusible res (idd, soac) ufs = do
  nm_ker <- KernName <$> newVName "ker"
  scope <- askScope
  let out_nms = map identName idd
      new_ker = newKernel soac out_nms scope
      comb    = HM.unionWith HS.union
      os' = HM.fromList [(arr,nm_ker) | arr <- out_nms]
            `HM.union` outArr res
      is' = HM.fromList [(arr,HS.singleton nm_ker)
                         | arr <- map SOAC.inputArray $ SOAC.inputs soac]
            `comb` inpArr res
  return $ FusedRes (rsucc res) os' is' ufs
           (HM.insert nm_ker new_ker (kernels res))

lookupInput :: VName -> FusionGM (Maybe SOAC.Input)
lookupInput name = asks $ HM.lookup name . arrsInScope

inlineSOACInput :: SOAC.Input -> FusionGM SOAC.Input
inlineSOACInput (SOAC.Input ts v t) = do
  maybe_inp <- lookupInput v
  case maybe_inp of
    Nothing ->
      return $ SOAC.Input ts v t
    Just (SOAC.Input ts2 v2 t2) ->
      return $ SOAC.Input (ts2<>ts) v2 t2

inlineSOACInputs :: SOAC -> FusionGM SOAC
inlineSOACInputs soac = do
  inputs' <- mapM inlineSOACInput $ SOAC.inputs soac
  return $ inputs' `SOAC.setInputs` soac


-- | Attempts to fuse between map(s), reduce(s), redomap(s). Input:
--   @rem_bnds@ are the bindings remaining in the current body after @orig_soac@.
--   @lam_used_nms@ the infusible names
--   @res@ the fusion result (before processing the current soac)
--   @orig_soac@ and @out_idds@ the current SOAC and its binding pattern
--   Output: a new Fusion Result (after processing the current SOAC binding)
greedyFuse :: [Binding] -> Names -> FusedRes -> (Pattern, SOAC) -> FusionGM FusedRes
greedyFuse rem_bnds lam_used_nms res (out_idds, orig_soac) = do
  soac <- inlineSOACInputs orig_soac
  (inp_nms, other_nms) <- soacInputs soac
  -- Assumption: the free vars in lambda are already in @infusible res@.
  let out_nms     = patternNames out_idds
      isInfusible = (`HS.member` infusible res)
      is_redomap_scanomap  = case orig_soac of
                        SOAC.Redomap{}  -> True
                        SOAC.Scanomap{} -> True
                        _               -> False
  --
  -- Conditions for fusion:
  -- If current soac is a replicate OR (current soac not a redomap AND
  --    (i) none of @out_idds@ belongs to the infusible set)
  -- THEN try applying producer-consumer fusion
  -- ELSE try applying horizontal        fusion
  -- (without duplicating computation in both cases)

  (ok_kers_compat, fused_kers, fused_nms, old_kers, oldker_nms) <-
        if   is_redomap_scanomap || any isInfusible out_nms
        then horizontGreedyFuse rem_bnds res (out_idds, soac)
        else prodconsGreedyFuse          res (out_idds, soac)
  --
  -- (ii) check whether fusing @soac@ will violate any in-place update
  --      restriction, e.g., would move an input array past its in-place update.
  let all_used_names = HS.toList $ HS.unions [lam_used_nms, HS.fromList inp_nms, HS.fromList other_nms]
      has_inplace ker = any (`HS.member` inplace ker) all_used_names
      ok_inplace = not $ any has_inplace old_kers
  --
  -- (iii)  there are some kernels that use some of `out_idds' as inputs
  -- (iv)   and producer-consumer or horizontal fusion succeeds with those.
  let fusible_ker = not (null old_kers) && ok_inplace && ok_kers_compat
  --
  -- Start constructing the fusion's result:
  --  (i) inparr ids other than vars will be added to infusible list,
  -- (ii) will also become part of the infusible set the inparr vars
  --         that also appear as inparr of another kernel,
  --         BUT which said kernel is not the one we are fusing with (now)!
  let mod_kerS  = if fusible_ker then HS.fromList oldker_nms else HS.empty
  let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
  let ufs       = HS.unions [infusible res, HS.fromList used_inps,
                             HS.fromList other_nms `HS.difference`
                             HS.fromList (map SOAC.inputArray $ SOAC.inputs soac)]
  let comb      = HM.unionWith HS.union

  if not fusible_ker then
    addNewKerWithInfusible res (patternIdents out_idds, soac) ufs
  else do
     -- Need to suitably update `inpArr':
     --   (i) first remove the inpArr bindings of the old kernel
     let inpArr' =
            foldl (\inpa (kold, knm) ->
                    HS.foldl'
                        (\inpp nm ->
                           case HM.lookup nm inpp of
                             Nothing -> inpp
                             Just s  -> let new_set = HS.delete knm s
                                        in if HS.null new_set
                                           then HM.delete nm         inpp
                                           else HM.insert nm new_set inpp
                        )
                    inpa $ arrInputs kold
                 )
            (inpArr res) (zip old_kers oldker_nms)
     --  (ii) then add the inpArr bindings of the new kernel
     let fused_ker_nms = zip fused_nms fused_kers
         inpArr''= foldl (\inpa' (knm, knew) ->
                             HM.fromList [ (k, HS.singleton knm)
                                         | k <- HS.toList $ arrInputs knew ]
                             `comb` inpa'
                         )
                   inpArr' fused_ker_nms
     -- Update the kernels map (why not delete the ones that have been fused?)
     let kernels' = HM.fromList fused_ker_nms `HM.union` kernels res
     -- nothing to do for `outArr' (since we have not added a new kernel)
     -- DO IMPROVEMENT: attempt to fuse the resulting kernel AGAIN until it fails,
     --                 but make sure NOT to add a new kernel!
     return $ FusedRes True (outArr res) inpArr'' ufs kernels'

prodconsGreedyFuse :: FusedRes -> (Pattern, SOAC)
                   -> FusionGM (Bool, [FusedKer], [KernName], [FusedKer], [KernName])
prodconsGreedyFuse res (out_idds, soac) = do
  let out_nms        = patternNames out_idds    -- Extract VNames from output patterns
      to_fuse_knmSet = getKersWithInpArrs res out_nms  -- Find kernels which consume outputs
      to_fuse_knms   = HS.toList to_fuse_knmSet
      lookup_kern k  = case HM.lookup k (kernels res) of
                         Nothing  -> badFusionGM $ Error
                                     ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                      ++ "kernel name not found in kernels field!")
                         Just ker -> return ker
  to_fuse_kers <- mapM lookup_kern to_fuse_knms -- Get all consumer kernels
  -- try producer-consumer fusion
  (ok_kers_compat, fused_kers) <- do
      kers <- forM to_fuse_kers $
                attemptFusion HS.empty (patternNames out_idds) soac
      case sequence kers of
        Nothing    -> return (False, [])
        Just kers' -> return (True, kers')
  return (ok_kers_compat, fused_kers, to_fuse_knms, to_fuse_kers, to_fuse_knms)

horizontGreedyFuse :: [Binding] -> FusedRes -> (Pattern, SOAC)
                   -> FusionGM (Bool, [FusedKer], [KernName], [FusedKer], [KernName])
horizontGreedyFuse rem_bnds res (out_idds, soac) = do
  (inp_nms, _) <- soacInputs soac
  let out_nms        = patternNames out_idds
      infusible_nms  = HS.fromList $ filter (`HS.member` infusible res) out_nms
      out_arr_nms    = case soac of
                        -- the accumulator result cannot be fused!
                        SOAC.Redomap _ _ _ _ _ nes _ -> drop (length nes) out_nms
                        SOAC.Scanomap _ _ _ _ nes _ -> drop (length nes) out_nms
                        SOAC.Stream  _ _ frm _ _ -> drop (length $ getStreamAccums frm) out_nms
                        _ -> out_nms
      to_fuse_knms1  = HS.toList $ getKersWithInpArrs res (out_arr_nms++inp_nms)
      to_fuse_knms2  = getKersWithSameInpSize (SOAC.width soac) res
      to_fuse_knms   = HS.toList $ HS.fromList $ to_fuse_knms1 ++ to_fuse_knms2
      lookup_kern k  = case HM.lookup k (kernels res) of
                         Nothing  -> badFusionGM $ Error
                                     ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                      ++ "kernel name not found in kernels field!")
                         Just ker -> return ker
  to_fuse_kers <- mapM lookup_kern to_fuse_knms
  -- for each kernel get the index in the bindings where the kernel is located
  -- and sort based on the index so that partial fusion may succeed.
  kernminds <- forM (zip to_fuse_knms to_fuse_kers) $ \(ker_nm, ker) -> do
                    let bnd_nms = map (patternNames . bindingPattern) rem_bnds
                        out_nm  = case fsoac ker of
                                    SOAC.Stream _ _ frm _ _
                                      | x:_ <- drop (length $ getStreamAccums frm) $ outNames ker ->
                                        x
                                    _ -> head $ outNames ker
                    case L.findIndex (elem out_nm) bnd_nms of
                      Nothing -> return Nothing
                      Just i  -> return $ Just (ker,ker_nm,i)
  scope <- askScope
  let kernminds' = L.sortBy (\(_,_,i1) (_,_,i2)->compare i1 i2) $ catMaybes kernminds
      soac_kernel = newKernel soac out_nms scope
  -- now try to fuse kernels one by one (in a fold); @ok_ind@ is the index of the
  -- kernel until which fusion succeded, and @fused_ker@ is the resulted kernel.
  (_,ok_ind,_,fused_ker,_) <-
      foldM (\(cur_ok,n,prev_ind,cur_ker,ufus_nms) (ker,_,bnd_ind) -> do
                -- check that we still try fusion and that the intermediate
                -- bindings do not use the results of cur_ker
                let curker_outnms  = outNames cur_ker
                    curker_outset  = HS.fromList curker_outnms
                    new_ufus_nms   = HS.fromList $ outNames ker ++ HS.toList ufus_nms
                    -- disable horizontal fusion in the case when an output array of
                    -- producer SOAC is a non-trivially transformed input of the consumer
                    out_transf_ok  = let ker_inp = SOAC.inputs $ fsoac ker
                                         unfuse1 = HS.fromList (map SOAC.inputArray ker_inp) `HS.difference`
                                                   HS.fromList (mapMaybe SOAC.isVarInput ker_inp)
                                         unfuse2 = HS.intersection curker_outset ufus_nms
                                     in  HS.null $ HS.intersection unfuse1 unfuse2
                    -- Disable horizontal fusion if consumer has any
                    -- output transforms.
                    cons_no_out_transf = SOAC.nullTransforms $ outputTransform ker
                consumer_ok   <- do let consumer_bnd   = rem_bnds !! bnd_ind
                                    maybesoac <- SOAC.fromExp $ bindingExp consumer_bnd
                                    case maybesoac of
                                      -- check that consumer's lambda body does not use
                                      -- directly the produced arrays (e.g., see noFusion3.fut).
                                      Right conssoac -> return $ HS.null $ HS.intersection curker_outset $
                                                                 freeInBody $ lambdaBody $ SOAC.lambda conssoac
                                      Left _         -> return True
                let interm_bnds_ok = cur_ok && consumer_ok && out_transf_ok && cons_no_out_transf &&
                      foldl (\ok bnd-> ok && -- hardwired to False after first fail
                                       -- (i) check that the in-between bindings do
                                       --     not use the result of current kernel OR
                                       HS.null ( HS.intersection curker_outset $
                                                      freeInExp (bindingExp bnd) ) ||
                                       --(ii) that the pattern-binding corresponds to
                                       --     the result of the consumer kernel; in the
                                       --     latter case it means it corresponds to a
                                       --     kernel that has been fused in the consumer,
                                       --     hence it should be ignored
                                       not ( null $ curker_outnms `L.intersect`
                                                         patternNames (bindingPattern bnd) )
                            ) True (drop (prev_ind+1) $ take bnd_ind rem_bnds)
                if not interm_bnds_ok then return (False,n,bnd_ind,cur_ker,HS.empty)
                else do new_ker <- attemptFusion ufus_nms (outNames cur_ker) (fsoac cur_ker) ker
                        case new_ker of
                          Nothing -> return (False, n,bnd_ind,cur_ker,HS.empty)
                          Just krn-> return (True,n+1,bnd_ind,krn,new_ufus_nms)
            ) (True,0,0,soac_kernel,infusible_nms) kernminds'
  let (to_fuse_kers',to_fuse_knms',_) = unzip3 $ take ok_ind kernminds'
      new_kernms = [to_fuse_knms' !! (ok_ind - 1) | ok_ind > 0]
  return (ok_ind>0, [fused_ker], new_kernms, to_fuse_kers', to_fuse_knms')
    where getKersWithSameInpSize :: SubExp -> FusedRes -> [KernName]
          getKersWithSameInpSize sz ress =
            map fst $ filter (\ (_,ker) -> sz == SOAC.width (fsoac ker)) $ HM.toList $ kernels ress

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

-- A reduce is translated to a redomap and treated from there.
fusionGatherBody fres (Body blore (Let pat bndtp (Op (Futhark.Reduce cs w comm lam args)):bnds) res) = do
  let (ne, arrs) = unzip args
      equivsoac = Futhark.Redomap cs w comm lam lam ne arrs
  fusionGatherBody fres $ Body blore (Let pat bndtp (Op equivsoac):bnds) res

fusionGatherBody fres (Body _ (bnd@(Let pat _ e):bnds) res) = do
  maybesoac <- SOAC.fromExp e
  case maybesoac of
    Right soac@(SOAC.Map _ _ lam _) ->
      mapLike soac lam

    Right soac@(SOAC.Write _cs _len lam _ivs _as) ->
      mapLike soac lam

    Right soac@(SOAC.Redomap _ _ _ outer_red inner_red nes _) ->
      -- a redomap does not neccessarily start a new kernel, e.g.,
      -- @let a = reduce(+,0,A) in ... bnds ... in let B = map(f,A)@
      -- can be fused into a redomap that replaces the @map@, if @a@
      -- and @B@ are defined in the same scope and @bnds@ does not uses @a@.
      -- a redomap always starts a new kernel
      reduceLike soac [outer_red, inner_red] nes

    Right soac@(SOAC.Scanomap  _ _ outer_red inner_red nes _) ->
      reduceLike soac [outer_red, inner_red] nes

    Right soac@(SOAC.Scan _ _ lam args) ->
      -- NOT FUSIBLE (probably), but still add as kernel, as
      -- optimisations like ISWIM may make it fusible.
      reduceLike soac [lam] $ map fst args

    Right soac@(SOAC.Stream _ _ form lam _) -> do
      -- a redomap does not neccessarily start a new kernel, e.g.,
      -- @let a= reduce(+,0,A) in ... bnds ... in let B = map(f,A)@
      -- can be fused into a redomap that replaces the @map@, if @a@
      -- and @B@ are defined in the same scope and @bnds@ does not uses @a@.
      -- a redomap always starts a new kernel
      let lambdas = case form of
                        RedLike _ _ lout _ -> [lout, lam]
                        _                  -> [lam]
      reduceLike soac lambdas $ getStreamAccums form

    Left (SOAC.InvalidArrayInput inpe) ->
      badFusionGM $ Error
      ("In Fusion.hs, "++pretty inpe++" is not valid array input.")

    _ | [pe] <- patternValueElements pat,
        Just (src,trns) <- SOAC.transformFromExp e ->
      bindingTransform pe src trns $ fusionGatherBody fres $ mkBody bnds res

    _ -> do
      let pat_vars = map (PrimOp . SubExp . Var) $ patternNames pat
      bres <- gatherBindingPattern pat $ fusionGatherBody fres body
      foldM fusionGatherExp bres (e:pat_vars)
  where body = mkBody bnds res
        rem_bnds = bnd : bnds

        reduceLike soac lambdas nes = do
          (used_lam, lres)  <- foldM fusionGatherLam (HS.empty, fres) lambdas
          bres  <- bindingFamily pat $ fusionGatherBody lres body
          bres' <- foldM fusionGatherSubExp bres nes
          greedyFuse rem_bnds used_lam bres' (pat, soac)

        mapLike soac lambda = do
          bres  <- bindingFamily pat $ fusionGatherBody fres body
          (used_lam, blres) <- fusionGatherLam (HS.empty, bres) lambda
          greedyFuse rem_bnds used_lam blres (pat, soac)

fusionGatherBody fres (Body _ [] res) =
  foldM fusionGatherExp fres $ map (PrimOp . SubExp) res

fusionGatherExp :: FusedRes -> Exp -> FusionGM FusedRes

-----------------------------------------
---- Index/If    ----
-----------------------------------------

fusionGatherExp fres (DoLoop ctx val form loop_body) = do
  let (merge_pat, ini_val) = unzip merge

  let pat_vars = map (Var . paramName)  merge_pat
  fres' <- foldM fusionGatherSubExp fres (ini_val++pat_vars)
  (fres'', form_idents) <- case form of
    ForLoop i bound ->
      (,) <$> fusionGatherSubExp fres' bound <*> pure [Ident i $ Prim int32]
    WhileLoop cond ->
      (,) <$> fusionGatherSubExp fres' (Var cond) <*> pure []

  let null_res = mkFreshFusionRes
  new_res <- binding (form_idents ++ map paramIdent merge_pat) $
    fusionGatherBody null_res loop_body
  -- make the inpArr infusible, so that they
  -- cannot be fused from outside the loop:
  let (inp_arrs, _) = unzip $ HM.toList $ inpArr new_res
  let new_res' = new_res { infusible = foldl (flip HS.insert) (infusible new_res) inp_arrs }
  -- merge new_res with fres''
  return $ unionFusionRes new_res' fres''
  where merge = ctx ++ val

fusionGatherExp fres (PrimOp (Index _ idd inds)) =
  foldM fusionGatherSubExp fres (Var idd : inds)

fusionGatherExp fres (If cond e_then e_else _) = do
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

fusionGatherExp _ (Op Futhark.Map{}) = errorIllegal "map"
fusionGatherExp _ (Op Futhark.Reduce{}) = errorIllegal "reduce"
fusionGatherExp _ (Op Futhark.Scan{}) = errorIllegal "scan"
fusionGatherExp _ (Op Futhark.Redomap{}) = errorIllegal "redomap"
fusionGatherExp _ (Op Futhark.Scanomap{}) = errorIllegal "scanomap"
fusionGatherExp _ (Op Futhark.Write{}) = errorIllegal "write"

-----------------------------------
---- Generic Traversal         ----
-----------------------------------

fusionGatherExp fres e = do
    let foldstct = identityFolder { foldOnBinding = \x -> fusionGatherExp x . bindingExp
                                  , foldOnSubExp = fusionGatherSubExp
                                  }
    foldExpM foldstct fres e

fusionGatherSubExp :: FusedRes -> SubExp -> FusionGM FusedRes
fusionGatherSubExp fres (Var idd) = addVarToInfusible fres idd
fusionGatherSubExp fres _         = return fres

addVarToInfusible :: FusedRes -> VName -> FusionGM FusedRes
addVarToInfusible fres name = do
  trns <- asks $ HM.lookup name . arrsInScope
  let name' = case trns of
        Nothing         -> name
        Just (SOAC.Input _ orig _) -> orig
  return fres { infusible = HS.insert name' $ infusible fres }

-- Lambdas create a new scope.  Disallow fusing from outside lambda by
-- adding inp_arrs to the infusible set.
fusionGatherLam :: (Names, FusedRes) -> Lambda -> FusionGM (HS.HashSet VName, FusedRes)
fusionGatherLam (u_set,fres) (Lambda idds body _) = do
    let null_res = mkFreshFusionRes
    new_res <- binding (map paramIdent idds) $
               fusionGatherBody null_res body
    -- make the inpArr infusible, so that they
    -- cannot be fused from outside the lambda:
    let inp_arrs = HS.fromList $ HM.keys $ inpArr new_res
    let unfus = infusible new_res `HS.union` inp_arrs
    bnds <- HM.keys <$> asks varsInScope
    let unfus'  = unfus `HS.intersection` HS.fromList bnds
    -- merge fres with new_res'
    let new_res' = new_res { infusible = unfus' }
    -- merge new_res with fres'
    return (u_set `HS.union` unfus', unionFusionRes new_res' fres)

-------------------------------------------------------------
-------------------------------------------------------------
--- FINALLY, Substitute the kernels in function
-------------------------------------------------------------
-------------------------------------------------------------

fuseInBody :: Body -> FusionGM Body

fuseInBody (Body _ (Let pat lore e:bnds) res) = do
  maybesoac <- SOAC.fromExp e
  case maybesoac of
    Right soac ->
      bindingPat pat (fuseInBody (mkBody bnds res)) >>=
      replaceSOAC pat soac
    _ -> do
      Body _ bnds' res' <- bindingPat pat $ fuseInBody $ mkBody bnds res
      e'                <- fuseInExp e
      return $ mkBody (Let pat lore e':bnds') res'

fuseInBody (Body () [] res) =
  return $ Body () [] res

fuseInExp :: Exp -> FusionGM Exp

-- Handle loop specially because we need to bind the types of the
-- merge variables.
fuseInExp (DoLoop ctx val form loopbody) =
  binding form_idents $
  bindingFParams (map fst $ ctx ++ val) $ do
    loopbody' <- fuseInBody loopbody
    return $ DoLoop ctx val form loopbody'
  where form_idents = case form of
          WhileLoop{} -> []
          ForLoop i _ -> [Ident i $ Prim int32]

fuseInExp e = mapExpM fuseIn e

fuseIn :: Mapper SOACS SOACS FusionGM
fuseIn = identityMapper {
           mapOnBody    = fuseInBody
         , mapOnOp      = mapSOACM identitySOACMapper
                          { mapOnSOACLambda = fuseInLambda
                          , mapOnSOACExtLambda = fuseInExtLambda
                          }
         }

fuseInLambda :: Lambda -> FusionGM Lambda
fuseInLambda (Lambda params body rtp) = do
  body' <- binding (map paramIdent params) $ fuseInBody body
  return $ Lambda params body' rtp

fuseInExtLambda :: ExtLambda -> FusionGM ExtLambda
fuseInExtLambda (ExtLambda params body rtp) = do
  body' <- binding (map paramIdent params) $ fuseInBody body
  return $ ExtLambda params body' rtp

replaceSOAC :: Pattern -> SOAC -> Body -> FusionGM Body
replaceSOAC (Pattern _ []) _ body = return body
replaceSOAC pat@(Pattern _ (patElem : _)) soac body = do
  fres  <- asks fusedRes
  let pat_nm = patElemName patElem
      names  = patternIdents pat
  case HM.lookup pat_nm (outArr fres) of
    Nothing  -> do
      (e,bnds) <- runBinder $ SOAC.toExp soac
      e'    <- fuseInExp e
      return $ insertBindings bnds $ mkLet' [] names e' `insertBinding` body
    Just knm ->
      case HM.lookup knm (kernels fres) of
        Nothing  -> badFusionGM $ Error
                                   ("In Fusion.hs, replaceSOAC, outArr in ker_name "
                                    ++"which is not in Res: "++textual (unKernName knm))
        Just ker -> do
          when (null $ fusedVars ker) $
            badFusionGM $ Error
            ("In Fusion.hs, replaceSOAC, unfused kernel "
             ++"still in result: "++pretty names)
          insertKerSOAC (outNames ker) ker body

insertKerSOAC :: [VName] -> FusedKer -> Body -> FusionGM Body
insertKerSOAC names ker body = do
  let new_soac = fsoac ker
      lam = SOAC.lambda new_soac
      args = replicate (length $ lambdaParams lam) Nothing
  lam' <- simplifyLambda lam Nothing args
  (_, nfres) <- fusionGatherLam (HS.empty, mkFreshFusionRes) lam'
  let nfres' =  cleanFusionResult nfres
  lam''      <- bindRes nfres' $ fuseInLambda lam'
  runBodyBinder $ do
    transformOutput (outputTransform ker) names $
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
               infusible = HS.empty, kernels = HM.empty }

unionFusionRes :: FusedRes -> FusedRes -> FusedRes
unionFusionRes res1 res2 =
    FusedRes  (rsucc     res1       ||      rsucc     res2)
              (outArr    res1    `HM.union`  outArr    res2)
              (HM.unionWith HS.union (inpArr res1) (inpArr res2) )
              (infusible res1    `HS.union`  infusible res2)
              (kernels   res1    `HM.union`  kernels   res2)

mergeFusionRes :: FusedRes -> FusedRes -> FusionGM FusedRes
mergeFusionRes res1 res2 = do
    let ufus_mres = infusible res1 `HS.union` infusible res2
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
getIdentArr :: [SOAC.Input] -> ([VName], [VName])
getIdentArr = foldl comb ([],[])
  where comb (vs,os) (SOAC.Input ts idd _)
          | SOAC.nullTransforms ts = (idd:vs, os)
        comb (vs, os) inp =
          (vs, SOAC.inputArray inp : os)

cleanFusionResult :: FusedRes -> FusedRes
cleanFusionResult fres =
    let newks = HM.filter (not . null . fusedVars)      (kernels fres)
        newoa = HM.filter (`HM.member` newks)            (outArr  fres)
        newia = HM.map    (HS.filter (`HM.member` newks)) (inpArr fres)
    in fres { outArr = newoa, inpArr = newia, kernels = newks }

--------------
--- Errors ---
--------------

errorIllegal :: String -> FusionGM FusedRes
errorIllegal soac_name =
    badFusionGM $ Error
                  ("In Fusion.hs, soac "++soac_name++" appears illegally in pgm!")
