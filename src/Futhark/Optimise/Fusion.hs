{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform horizontal and vertical fusion of SOACs.
module Futhark.Optimise.Fusion ( fuseSOACs )
  where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set      as S
import qualified Data.List         as L

import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.SOACS hiding (SOAC(..))
import qualified Futhark.Representation.Aliases as Aliases
import qualified Futhark.Representation.SOACS as Futhark
import Futhark.MonadFreshNames
import Futhark.Representation.SOACS.Simplify
import Futhark.Optimise.Fusion.LoopKernel
import Futhark.Construct
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Pass

data VarEntry = IsArray VName (NameInfo SOACS) Names SOAC.Input
              | IsNotArray VName (NameInfo SOACS)

varEntryType :: VarEntry -> NameInfo SOACS
varEntryType (IsArray _ attr _ _) =
  attr
varEntryType (IsNotArray _ attr) =
  attr

varEntryAliases :: VarEntry -> Names
varEntryAliases (IsArray _ _ x _) = x
varEntryAliases _ = mempty

data FusionGEnv = FusionGEnv {
    soacs      :: M.Map VName [VName]
  -- ^ Mapping from variable name to its entire family.
  , varsInScope:: M.Map VName VarEntry
  , fusedRes   :: FusedRes
  }

lookupArr :: VName -> FusionGEnv -> Maybe SOAC.Input
lookupArr v env = asArray =<< M.lookup v (varsInScope env)
  where asArray (IsArray _ _ _ input) = Just input
        asArray IsNotArray{}          = Nothing

newtype Error = Error String

instance Show Error where
  show (Error msg) = "Fusion error:\n" ++ msg

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
    where toScope = M.map varEntryType

------------------------------------------------------------------------
--- Monadic Helpers: bind/new/runFusionGatherM, etc
------------------------------------------------------------------------

-- | Binds an array name to the set of used-array vars
bindVar :: FusionGEnv -> (Ident, Names) -> FusionGEnv
bindVar env (Ident name t, aliases) =
  env { varsInScope = M.insert name entry $ varsInScope env }
  where entry = case t of
          Array {} -> IsArray name (LetInfo t) aliases' $ SOAC.identInput $ Ident name t
          _        -> IsNotArray name $ LetInfo t
        expand = maybe mempty varEntryAliases . flip M.lookup (varsInScope env)
        aliases' = aliases <> mconcat (map expand $ namesToList aliases)

bindVars :: FusionGEnv -> [(Ident, Names)] -> FusionGEnv
bindVars = foldl bindVar

binding :: [(Ident, Names)] -> FusionGM a -> FusionGM a
binding vs = local (`bindVars` vs)

gatherStmPattern :: Pattern -> Exp -> FusionGM FusedRes -> FusionGM FusedRes
gatherStmPattern pat e = binding $ zip idents aliases
  where idents = patternIdents pat
        aliases = replicate (length (patternContextNames pat)) mempty ++
                  expAliases (Alias.analyseExp e)

bindingPat :: Pattern -> FusionGM a -> FusionGM a
bindingPat = binding . (`zip` repeat mempty) . patternIdents

bindingParams :: Typed t => [Param t] -> FusionGM a -> FusionGM a
bindingParams = binding . (`zip` repeat mempty) . map paramIdent

-- | Binds an array name to the set of soac-produced vars
bindingFamilyVar :: [VName] -> FusionGEnv -> Ident -> FusionGEnv
bindingFamilyVar faml env (Ident nm t) =
  env { soacs       = M.insert nm faml $ soacs env
      , varsInScope = M.insert nm (IsArray nm (LetInfo t) mempty $
                                   SOAC.identInput $ Ident nm t) $
                      varsInScope env
      }

varAliases :: VName -> FusionGM Names
varAliases v = asks $ (oneName v<>) . maybe mempty varEntryAliases .
                      M.lookup v . varsInScope

varsAliases :: Names -> FusionGM Names
varsAliases = fmap mconcat . mapM varAliases . namesToList

checkForUpdates :: FusedRes -> Exp -> FusionGM FusedRes
checkForUpdates res (BasicOp (Update src is _)) = do
  res' <- foldM addVarToInfusible res $
          src : namesToList (mconcat $ map freeIn is)
  aliases <- varAliases src
  let inspectKer k = k { inplace = aliases <> inplace k }
  return res' { kernels = M.map inspectKer $ kernels res' }
checkForUpdates res _ = return res

-- | Updates the environment: (i) the @soacs@ (map) by binding each pattern
--   element identifier to all pattern elements (identifiers) and (ii) the
--   variables in scope (map) by inserting each (pattern-array) name.
--   Finally, if the binding is an in-place update, then the @inplace@ field
--   of each (result) kernel is updated with the new in-place updates.
bindingFamily :: Pattern -> FusionGM FusedRes -> FusionGM FusedRes
bindingFamily pat = local bind
  where idents = patternIdents pat
        family = patternNames pat
        bind env = foldl (bindingFamilyVar family) env idents

bindingTransform :: PatElem -> VName -> SOAC.ArrayTransform -> FusionGM a -> FusionGM a
bindingTransform pe srcname trns = local $ \env ->
  case M.lookup srcname $ varsInScope env of
    Just (IsArray src' _ aliases input) ->
      env { varsInScope =
              M.insert vname
              (IsArray src' (LetInfo attr) (oneName srcname <> aliases) $
               trns `SOAC.addTransform` input) $
              varsInScope env
          }
    _ -> bindVar env (patElemIdent pe, oneName vname)
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

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass { passName = "Fuse SOACs"
       , passDescription = "Perform higher-order optimisation, i.e., fusion."
       , passFunction = simplifySOACS <=< renameProg <=< intraproceduralTransformation fuseFun
       }

fuseFun :: FunDef SOACS -> PassM (FunDef SOACS)
fuseFun fun = do
  let env  = FusionGEnv { soacs = M.empty
                        , varsInScope = M.empty
                        , fusedRes = mempty
                        }
  k <- cleanFusionResult <$>
       liftEitherM (runFusionGatherM (fusionGatherFun fun) env)
  if not $ rsucc k
  then return fun
  else liftEitherM $ runFusionGatherM (fuseInFun k fun) env

fusionGatherFun :: FunDef SOACS -> FusionGM FusedRes
fusionGatherFun fundec =
  bindingParams (funDefParams fundec) $
  fusionGatherBody mempty $ funDefBody fundec

fuseInFun :: FusedRes -> FunDef SOACS -> FusionGM (FunDef SOACS)
fuseInFun res fundec = do
  body' <- bindingParams (funDefParams fundec) $
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

  , infusible  :: Names
  -- ^ the (names of) arrays that are not fusible, i.e.,
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

instance Semigroup FusedRes where
  res1 <> res2 =
    FusedRes (rsucc     res1       ||      rsucc     res2)
             (outArr    res1    `M.union`  outArr    res2)
             (M.unionWith S.union (inpArr res1) (inpArr res2) )
             (infusible res1    <>  infusible res2)
             (kernels   res1    `M.union`  kernels   res2)

instance Monoid FusedRes where
  mempty = FusedRes { rsucc     = False,   outArr = M.empty, inpArr  = M.empty,
                      infusible = mempty, kernels = M.empty }

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
  let (inp_idds, other_idds) = getIdentArr $ SOAC.inputs soac
      (inp_nms0, other_nms0) = (inp_idds, other_idds)
  inp_nms   <- expandSoacInpArr   inp_nms0
  other_nms <- expandSoacInpArr other_nms0
  return (inp_nms, other_nms)

addNewKerWithInfusible :: FusedRes -> ([Ident], Certificates, SOAC, Names) -> Names -> FusionGM FusedRes
addNewKerWithInfusible res (idd, cs, soac, consumed) ufs = do
  nm_ker <- KernName <$> newVName "ker"
  scope <- askScope
  let out_nms = map identName idd
      new_ker = newKernel cs soac consumed out_nms scope
      comb    = M.unionWith S.union
      os' = M.fromList [(arr,nm_ker) | arr <- out_nms]
            `M.union` outArr res
      is' = M.fromList [(arr,S.singleton nm_ker)
                         | arr <- map SOAC.inputArray $ SOAC.inputs soac]
            `comb` inpArr res
  return $ FusedRes (rsucc res) os' is' ufs
           (M.insert nm_ker new_ker (kernels res))

lookupInput :: VName -> FusionGM (Maybe SOAC.Input)
lookupInput name = asks $ lookupArr name

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


-- | Attempts to fuse between SOACs. Input:
--   @rem_bnds@ are the bindings remaining in the current body after @orig_soac@.
--   @lam_used_nms@ the infusible names
--   @res@ the fusion result (before processing the current soac)
--   @orig_soac@ and @out_idds@ the current SOAC and its binding pattern
--   @consumed@ is the set of names consumed by the SOAC.
--   Output: a new Fusion Result (after processing the current SOAC binding)
greedyFuse :: [Stm] -> Names -> FusedRes -> (Pattern, Certificates, SOAC, Names)
           -> FusionGM FusedRes
greedyFuse rem_bnds lam_used_nms res (out_idds, cs, orig_soac, consumed) = do
  soac <- inlineSOACInputs orig_soac
  (inp_nms, other_nms) <- soacInputs soac
  -- Assumption: the free vars in lambda are already in @infusible res@.
  let out_nms     = patternNames out_idds
      isInfusible = (`nameIn` infusible res)
      is_screma  = case orig_soac of
                       SOAC.Screma _ form _ ->
                         (isJust (isRedomapSOAC form) || isJust (isScanomapSOAC form)) &&
                         not (isJust (isReduceSOAC form) || isJust (isScanSOAC form))
                       _ -> False
  --
  -- Conditions for fusion:
  -- If current soac is a replicate OR (current soac a redomap/scanomap AND
  --    (i) none of @out_idds@ belongs to the infusible set)
  -- THEN try applying producer-consumer fusion
  -- ELSE try applying horizontal        fusion
  -- (without duplicating computation in both cases)

  (ok_kers_compat, fused_kers, fused_nms, old_kers, oldker_nms) <-
        if   is_screma || any isInfusible out_nms
        then horizontGreedyFuse rem_bnds res (out_idds, cs, soac, consumed)
        else prodconsGreedyFuse          res (out_idds, cs, soac, consumed)
  --
  -- (ii) check whether fusing @soac@ will violate any in-place update
  --      restriction, e.g., would move an input array past its in-place update.
  let all_used_names = namesToList $ mconcat [lam_used_nms, namesFromList inp_nms, namesFromList other_nms]
      has_inplace ker = any (`nameIn` inplace ker) all_used_names
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
  let mod_kerS  = if fusible_ker then S.fromList oldker_nms else mempty
  let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
  let ufs       = mconcat [infusible res, namesFromList used_inps,
                           namesFromList other_nms `namesSubtract`
                           namesFromList (map SOAC.inputArray $ SOAC.inputs soac)]
  let comb      = M.unionWith S.union

  if not fusible_ker then
    addNewKerWithInfusible res (patternIdents out_idds, cs, soac, consumed) ufs
  else do
     -- Need to suitably update `inpArr':
     --   (i) first remove the inpArr bindings of the old kernel
     let inpArr' =
            foldl (\inpa (kold, knm) ->
                    S.foldl'
                        (\inpp nm ->
                           case M.lookup nm inpp of
                             Nothing -> inpp
                             Just s  -> let new_set = S.delete knm s
                                        in if S.null new_set
                                           then M.delete nm         inpp
                                           else M.insert nm new_set inpp
                        )
                    inpa $ arrInputs kold
                 )
            (inpArr res) (zip old_kers oldker_nms)
     --  (ii) then add the inpArr bindings of the new kernel
     let fused_ker_nms = zip fused_nms fused_kers
         inpArr''= foldl (\inpa' (knm, knew) ->
                             M.fromList [ (k, S.singleton knm)
                                         | k <- S.toList $ arrInputs knew ]
                             `comb` inpa'
                         )
                   inpArr' fused_ker_nms
     -- Update the kernels map (why not delete the ones that have been fused?)
     let kernels' = M.fromList fused_ker_nms `M.union` kernels res
     -- nothing to do for `outArr' (since we have not added a new kernel)
     -- DO IMPROVEMENT: attempt to fuse the resulting kernel AGAIN until it fails,
     --                 but make sure NOT to add a new kernel!
     return $ FusedRes True (outArr res) inpArr'' ufs kernels'

prodconsGreedyFuse :: FusedRes -> (Pattern, Certificates, SOAC, Names)
                   -> FusionGM (Bool, [FusedKer], [KernName], [FusedKer], [KernName])
prodconsGreedyFuse res (out_idds, cs, soac, consumed) = do
  let out_nms        = patternNames out_idds    -- Extract VNames from output patterns
      to_fuse_knmSet = getKersWithInpArrs res out_nms  -- Find kernels which consume outputs
      to_fuse_knms   = S.toList to_fuse_knmSet
      lookup_kern k  = case M.lookup k (kernels res) of
                         Nothing  -> throwError $ Error
                                     ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                      ++ "kernel name not found in kernels field!")
                         Just ker -> return ker
  to_fuse_kers <- mapM lookup_kern to_fuse_knms -- Get all consumer kernels
  -- try producer-consumer fusion
  (ok_kers_compat, fused_kers) <- do
      kers <- forM to_fuse_kers $
              attemptFusion mempty (patternNames out_idds) soac consumed
      case sequence kers of
        Nothing    -> return (False, [])
        Just kers' -> return (True, map certifyKer kers')
  return (ok_kers_compat, fused_kers, to_fuse_knms, to_fuse_kers, to_fuse_knms)
  where certifyKer k = k { certificates = certificates k <> cs }

horizontGreedyFuse :: [Stm] -> FusedRes -> (Pattern, Certificates, SOAC, Names)
                   -> FusionGM (Bool, [FusedKer], [KernName], [FusedKer], [KernName])
horizontGreedyFuse rem_bnds res (out_idds, cs, soac, consumed) = do
  (inp_nms, _) <- soacInputs soac
  let out_nms        = patternNames out_idds
      infusible_nms  = namesFromList $ filter (`nameIn` infusible res) out_nms
      out_arr_nms    = case soac of
                        -- the accumulator result cannot be fused!
                        SOAC.Screma _ (ScremaForm (_, scan_nes) reds _) _ ->
                          drop (length scan_nes + redResults reds) out_nms
                        SOAC.Stream _ frm _ _ -> drop (length $ getStreamAccums frm) out_nms
                        _ -> out_nms
      to_fuse_knms1  = S.toList $ getKersWithInpArrs res (out_arr_nms++inp_nms)
      to_fuse_knms2  = getKersWithSameInpSize (SOAC.width soac) res
      to_fuse_knms   = S.toList $ S.fromList $ to_fuse_knms1 ++ to_fuse_knms2
      lookupKernel k  = case M.lookup k (kernels res) of
                          Nothing  -> throwError $ Error
                                      ("In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                                       ++ "kernel name not found in kernels field!")
                          Just ker -> return ker

  -- For each kernel get the index in the bindings where the kernel is
  -- located and sort based on the index so that partial fusion may
  -- succeed.  We use the last position where one of the kernel
  -- outputs occur.
  let bnd_nms = map (patternNames . stmPattern) rem_bnds
  kernminds <- forM to_fuse_knms $ \ker_nm -> do
    ker <- lookupKernel ker_nm
    case mapMaybe (\out_nm -> L.findIndex (elem out_nm) bnd_nms) (outNames ker) of
      [] -> return Nothing
      is -> return $ Just (ker,ker_nm,maximum is)

  scope <- askScope
  let kernminds' = L.sortBy (\(_,_,i1) (_,_,i2)->compare i1 i2) $ catMaybes kernminds
      soac_kernel = newKernel cs soac consumed out_nms scope

  -- now try to fuse kernels one by one (in a fold); @ok_ind@ is the index of the
  -- kernel until which fusion succeded, and @fused_ker@ is the resulting kernel.
  (_,ok_ind,_,fused_ker,_) <-
      foldM (\(cur_ok,n,prev_ind,cur_ker,ufus_nms) (ker, _ker_nm, bnd_ind) -> do
                -- check that we still try fusion and that the intermediate
                -- bindings do not use the results of cur_ker
                let curker_outnms  = outNames cur_ker
                    curker_outset  = namesFromList curker_outnms
                    new_ufus_nms   = namesFromList $ outNames ker ++ namesToList ufus_nms
                    -- disable horizontal fusion in the case when an output array of
                    -- producer SOAC is a non-trivially transformed input of the consumer
                    out_transf_ok  = let ker_inp = SOAC.inputs $ fsoac ker
                                         unfuse1 = namesFromList (map SOAC.inputArray ker_inp) `namesSubtract`
                                                   namesFromList (mapMaybe SOAC.isVarInput ker_inp)
                                         unfuse2 = namesIntersection curker_outset ufus_nms
                                     in not $ unfuse1 `namesIntersect` unfuse2
                    -- Disable horizontal fusion if consumer has any
                    -- output transforms.
                    cons_no_out_transf = SOAC.nullTransforms $ outputTransform ker

                consumer_ok   <- do let consumer_bnd   = rem_bnds !! bnd_ind
                                    maybesoac <- SOAC.fromExp $ stmExp consumer_bnd
                                    case maybesoac of
                                      -- check that consumer's lambda body does not use
                                      -- directly the produced arrays (e.g., see noFusion3.fut).
                                      Right conssoac -> return $ not $
                                                        curker_outset
                                                        `namesIntersect`
                                                        freeIn (lambdaBody $ SOAC.lambda conssoac)
                                      Left _         -> return True

                let interm_bnds_ok = cur_ok && consumer_ok && out_transf_ok && cons_no_out_transf &&
                      foldl (\ok bnd-> ok && -- hardwired to False after first fail
                                       -- (i) check that the in-between bindings do
                                       --     not use the result of current kernel OR
                                       not (curker_outset `namesIntersect` freeIn (stmExp bnd)) ||
                                       --(ii) that the pattern-binding corresponds to
                                       --     the result of the consumer kernel; in the
                                       --     latter case it means it corresponds to a
                                       --     kernel that has been fused in the consumer,
                                       --     hence it should be ignored
                                        not ( null $ curker_outnms `L.intersect`
                                              patternNames (stmPattern bnd))
                            ) True (drop (prev_ind+1) $ take bnd_ind rem_bnds)
                if not interm_bnds_ok then return (False,n,bnd_ind,cur_ker,mempty)
                else do new_ker <- attemptFusion ufus_nms (outNames cur_ker)
                                   (fsoac cur_ker) (fusedConsumed cur_ker) ker
                        case new_ker of
                          Nothing -> return (False, n,bnd_ind,cur_ker,mempty)
                          Just krn-> return (True,n+1,bnd_ind,krn,new_ufus_nms)
            ) (True,0,0,soac_kernel,infusible_nms) kernminds'

  -- Find the kernels we have fused into and the name of the last such
  -- kernel (if any).
  let (to_fuse_kers', to_fuse_knms',_) = unzip3 $ take ok_ind kernminds'
      new_kernms = drop (ok_ind-1) to_fuse_knms'

  return (ok_ind>0, [fused_ker], new_kernms, to_fuse_kers', to_fuse_knms')

  where getKersWithSameInpSize :: SubExp -> FusedRes -> [KernName]
        getKersWithSameInpSize sz ress =
            map fst $ filter (\ (_,ker) -> sz == SOAC.width (fsoac ker)) $ M.toList $ kernels ress

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
fusionGatherBody fres (Body _ stms res) =
  fusionGatherStms fres (stmsToList stms) res

fusionGatherStms :: FusedRes -> [Stm] -> Result -> FusionGM FusedRes

-- Some forms of do-loops can profitably be considered streamSeqs.  We
-- are careful to ensure that the generated nested loop cannot itself
-- be considered a stream, to avoid infinite recursion.
fusionGatherStms fres (Let (Pattern [] pes) bndtp
                       (DoLoop [] merge (ForLoop i it w loop_vars) body) : bnds) res
  | not $ null loop_vars = do
  let (merge_params,merge_init) = unzip merge
      (loop_params,loop_arrs) = unzip loop_vars
  chunk_size <- newVName "chunk_size"
  offset <- newVName "offset"
  let chunk_param = Param chunk_size $ Prim int32
      offset_param = Param offset $ Prim $ IntType it

  acc_params <- forM merge_params $ \p ->
    Param <$> newVName (baseString (paramName p) ++ "_outer") <*>
    pure (paramType p)

  chunked_params <- forM loop_vars $ \(p,arr) ->
    Param <$> newVName (baseString arr ++ "_chunk") <*>
    pure (paramType p `arrayOfRow` Futhark.Var chunk_size)

  let lam_params = chunk_param : acc_params ++ [offset_param] ++ chunked_params

  lam_body <- runBodyBinder $ localScope (scopeOfLParams lam_params) $ do
    let merge' = zip merge_params $ map (Futhark.Var . paramName) acc_params
    j <- newVName "j"
    loop_body <- runBodyBinder $ do
      forM_ (zip loop_params chunked_params) $ \(p,a_p) ->
        letBindNames_ [paramName p] $ BasicOp $ Index (paramName a_p) $
        fullSlice (paramType a_p) [DimFix $ Futhark.Var j]
      letBindNames_ [i] $ BasicOp $ BinOp (Add it) (Futhark.Var offset) (Futhark.Var j)
      return body
    eBody [pure $
           DoLoop [] merge' (ForLoop j it (Futhark.Var chunk_size) []) loop_body,
           pure $
           BasicOp $ BinOp (Add Int32) (Futhark.Var offset) (Futhark.Var chunk_size)]
  let lam = Lambda { lambdaParams = lam_params
                   , lambdaBody = lam_body
                   , lambdaReturnType = map paramType $ acc_params ++ [offset_param]
                   }
      stream = Futhark.Stream w (Sequential $ merge_init ++ [intConst it 0]) lam loop_arrs

  -- It is important that the (discarded) final-offset is not the
  -- first element in the pattern, as we use the first element to
  -- identify the SOAC in the second phase of fusion.
  discard <- newVName "discard"
  let discard_pe = PatElem discard $ Prim int32

  fusionGatherStms fres
    (Let (Pattern [] (pes<>[discard_pe])) bndtp (Op stream) : bnds) res

fusionGatherStms fres (bnd@(Let pat _ e):bnds) res = do
  maybesoac <- SOAC.fromExp e
  case maybesoac of
    Right soac@(SOAC.Scatter _len lam _ivs _as) -> do
      -- We put the variables produced by Scatter into the infusible
      -- set to force horizontal fusion.  It is not possible to
      -- producer/consumer-fuse Scatter anyway.
      fres' <- addNamesToInfusible fres $ namesFromList $ patternNames pat
      mapLike fres' soac lam

    Right soac@(SOAC.Hist _ _ lam _) -> do
      -- We put the variables produced by Hist into the infusible
      -- set to force horizontal fusion.  It is not possible to
      -- producer/consumer-fuse Hist anyway.
      fres' <- addNamesToInfusible fres $ namesFromList $ patternNames pat
      mapLike fres' soac lam

    Right soac@(SOAC.Screma _ (ScremaForm (scan_lam, scan_nes) reds map_lam) _) ->
      reduceLike soac (map redLambda reds ++ [scan_lam, map_lam]) $
      scan_nes <> concatMap redNeutral reds

    Right soac@(SOAC.Stream _ form lam _) -> do
      -- a redomap does not neccessarily start a new kernel, e.g.,
      -- @let a= reduce(+,0,A) in ... bnds ... in let B = map(f,A)@
      -- can be fused into a redomap that replaces the @map@, if @a@
      -- and @B@ are defined in the same scope and @bnds@ does not uses @a@.
      -- a redomap always starts a new kernel
      let lambdas = case form of
                        Parallel _ _ lout _ -> [lout, lam]
                        _                   -> [lam]
      reduceLike soac lambdas $ getStreamAccums form

    _ | [pe] <- patternValueElements pat,
        Just (src,trns) <- SOAC.transformFromExp (stmCerts bnd) e ->
          bindingTransform pe src trns $ fusionGatherStms fres bnds res
      | otherwise -> do
          let pat_vars = map (BasicOp . SubExp . Var) $ patternNames pat
          bres <- gatherStmPattern pat e $ fusionGatherStms fres bnds res
          bres' <- checkForUpdates bres e
          foldM fusionGatherExp bres' (e:pat_vars)

  where cs = stmCerts bnd
        rem_bnds = bnd : bnds
        consumed = consumedInExp $ Alias.analyseExp e

        reduceLike soac lambdas nes = do
          (used_lam, lres)  <- foldM fusionGatherLam (mempty, fres) lambdas
          bres  <- bindingFamily pat $ fusionGatherStms lres bnds res
          bres' <- foldM fusionGatherSubExp bres nes
          consumed' <- varsAliases consumed
          greedyFuse rem_bnds used_lam bres' (pat, cs, soac, consumed')

        mapLike fres' soac lambda = do
          bres  <- bindingFamily pat $ fusionGatherStms fres' bnds res
          (used_lam, blres) <- fusionGatherLam (mempty, bres) lambda
          consumed' <- varsAliases consumed
          greedyFuse rem_bnds used_lam blres (pat, cs, soac, consumed')

fusionGatherStms fres [] res =
  foldM fusionGatherExp fres $ map (BasicOp . SubExp) res

fusionGatherExp :: FusedRes -> Exp -> FusionGM FusedRes

-----------------------------------------
---- Index/If    ----
-----------------------------------------

fusionGatherExp fres (DoLoop ctx val form loop_body) = do
  fres' <- addNamesToInfusible fres $ freeIn form <> freeIn ctx <> freeIn val
  let form_idents =
        case form of
          ForLoop i _ _ loopvars ->
            Ident i (Prim int32) : map (paramIdent . fst) loopvars
          WhileLoop{} -> []

  new_res <- binding (zip (form_idents ++ map (paramIdent . fst) (ctx<>val)) $
                      repeat mempty) $
    fusionGatherBody mempty loop_body
  -- make the inpArr infusible, so that they
  -- cannot be fused from outside the loop:
  let (inp_arrs, _) = unzip $ M.toList $ inpArr new_res
  let new_res' = new_res { infusible = infusible new_res <> mconcat (map oneName inp_arrs) }
  -- merge new_res with fres'
  return $ new_res' <> fres'

fusionGatherExp fres (If cond e_then e_else _) = do
    then_res <- fusionGatherBody mempty e_then
    else_res <- fusionGatherBody mempty e_else
    let both_res = then_res <> else_res
    fres'    <- fusionGatherSubExp fres cond
    mergeFusionRes fres' both_res

-----------------------------------------------------------------------------------
--- Errors: all SOACs, (because normalization ensures they appear
--- directly in let exp, i.e., let x = e)
-----------------------------------------------------------------------------------

fusionGatherExp _ (Op Futhark.Screma{}) = errorIllegal "screma"
fusionGatherExp _ (Op Futhark.Scatter{}) = errorIllegal "write"

-----------------------------------
---- Generic Traversal         ----
-----------------------------------

fusionGatherExp fres e = addNamesToInfusible fres $ freeIn e

fusionGatherSubExp :: FusedRes -> SubExp -> FusionGM FusedRes
fusionGatherSubExp fres (Var idd) = addVarToInfusible fres idd
fusionGatherSubExp fres _         = return fres

addNamesToInfusible :: FusedRes -> Names -> FusionGM FusedRes
addNamesToInfusible fres = foldM addVarToInfusible fres . namesToList

addVarToInfusible :: FusedRes -> VName -> FusionGM FusedRes
addVarToInfusible fres name = do
  trns <- asks $ lookupArr name
  let name' = case trns of
        Nothing         -> name
        Just (SOAC.Input _ orig _) -> orig
  return fres { infusible = oneName name' <> infusible fres }

-- Lambdas create a new scope.  Disallow fusing from outside lambda by
-- adding inp_arrs to the infusible set.
fusionGatherLam :: (Names, FusedRes) -> Lambda -> FusionGM (Names, FusedRes)
fusionGatherLam (u_set,fres) (Lambda idds body _) = do
    new_res <- bindingParams idds $ fusionGatherBody mempty body
    -- make the inpArr infusible, so that they
    -- cannot be fused from outside the lambda:
    let inp_arrs = namesFromList $ M.keys $ inpArr new_res
    let unfus = infusible new_res <> inp_arrs
    bnds <- M.keys <$> asks varsInScope
    let unfus'  = unfus `namesIntersection` namesFromList bnds
    -- merge fres with new_res'
    let new_res' = new_res { infusible = unfus' }
    -- merge new_res with fres'
    return (u_set <> unfus', new_res' <> fres)

-------------------------------------------------------------
-------------------------------------------------------------
--- FINALLY, Substitute the kernels in function
-------------------------------------------------------------
-------------------------------------------------------------

fuseInBody :: Body -> FusionGM Body

fuseInBody (Body _ stms res)
  | Let pat aux e:bnds <- stmsToList stms = do
      body' <- bindingPat pat $ fuseInBody $ mkBody (stmsFromList bnds) res
      soac_bnds <- replaceSOAC pat aux e
      return $ insertStms soac_bnds body'
  | otherwise = return $ Body () mempty res

fuseInExp :: Exp -> FusionGM Exp

-- Handle loop specially because we need to bind the types of the
-- merge variables.
fuseInExp (DoLoop ctx val form loopbody) =
  binding (zip form_idents $ repeat mempty) $
  bindingParams (map fst $ ctx ++ val) $
  DoLoop ctx val form <$> fuseInBody loopbody
  where form_idents = case form of
          WhileLoop{} -> []
          ForLoop i it _ loopvars ->
            Ident i (Prim $ IntType it) :
            map (paramIdent . fst) loopvars

fuseInExp e = mapExpM fuseIn e

fuseIn :: Mapper SOACS SOACS FusionGM
fuseIn = identityMapper {
           mapOnBody = const fuseInBody
         , mapOnOp = mapSOACM identitySOACMapper { mapOnSOACLambda = fuseInLambda }
         }

fuseInLambda :: Lambda -> FusionGM Lambda
fuseInLambda (Lambda params body rtp) = do
  body' <- bindingParams params $ fuseInBody body
  return $ Lambda params body' rtp

replaceSOAC :: Pattern -> StmAux () -> Exp -> FusionGM (Stms SOACS)
replaceSOAC (Pattern _ []) _ _ = return mempty
replaceSOAC pat@(Pattern _ (patElem : _)) aux e = do
  fres  <- asks fusedRes
  let pat_nm = patElemName patElem
      names  = patternIdents pat
  case M.lookup pat_nm (outArr fres) of
    Nothing  ->
      oneStm . Let pat aux <$> fuseInExp e
    Just knm ->
      case M.lookup knm (kernels fres) of
        Nothing  -> throwError $ Error
                                   ("In Fusion.hs, replaceSOAC, outArr in ker_name "
                                    ++"which is not in Res: "++pretty (unKernName knm))
        Just ker -> do
          when (null $ fusedVars ker) $
            throwError $ Error
            ("In Fusion.hs, replaceSOAC, unfused kernel "
             ++"still in result: "++pretty names)
          insertKerSOAC (outNames ker) ker

insertKerSOAC :: [VName] -> FusedKer -> FusionGM (Stms SOACS)
insertKerSOAC names ker = do
  new_soac' <- finaliseSOAC $ fsoac ker
  runBinder_ $ do
    f_soac <- SOAC.toSOAC new_soac'
    -- The fused kernel may consume more than the original SOACs (see
    -- issue #224).  We insert copy expressions to fix it.
    f_soac' <- copyNewlyConsumed (fusedConsumed ker) $ addOpAliases f_soac
    validents <- zipWithM newIdent (map baseString names) $ SOAC.typeOf new_soac'
    letBind_ (basicPattern [] validents) $ Op f_soac'
    transformOutput (outputTransform ker) names validents

-- | Perform simplification and fusion inside the lambda(s) of a SOAC.
finaliseSOAC :: SOAC.SOAC SOACS -> FusionGM (SOAC.SOAC SOACS)
finaliseSOAC new_soac =
  case new_soac of
    SOAC.Screma w (ScremaForm (scan_lam, scan_nes) reds map_lam) arrs -> do
      scan_lam' <- simplifyAndFuseInLambda scan_lam
      reds' <- forM reds $ \(Reduce comm red_lam red_nes) -> do
        red_lam' <- simplifyAndFuseInLambda red_lam
        return $ Reduce comm red_lam' red_nes
      map_lam' <- simplifyAndFuseInLambda map_lam
      return $ SOAC.Screma w (ScremaForm (scan_lam', scan_nes) reds' map_lam') arrs
    SOAC.Scatter w lam inps dests -> do
      lam' <- simplifyAndFuseInLambda lam
      return $ SOAC.Scatter w lam' inps dests
    SOAC.Hist w ops lam arrs -> do
      lam' <- simplifyAndFuseInLambda lam
      return $ SOAC.Hist w ops lam' arrs
    SOAC.Stream w form lam inps -> do
      lam' <- simplifyAndFuseInLambda lam
      return $ SOAC.Stream w form lam' inps

simplifyAndFuseInLambda :: Lambda -> FusionGM Lambda
simplifyAndFuseInLambda lam = do
  let args = replicate (length $ lambdaParams lam) Nothing
  lam' <- simplifyLambda lam args
  (_, nfres) <- fusionGatherLam (mempty, mkFreshFusionRes) lam'
  let nfres' =  cleanFusionResult nfres
  bindRes nfres' $ fuseInLambda lam'

copyNewlyConsumed :: Names
                  -> Futhark.SOAC (Aliases.Aliases SOACS)
                  -> Binder SOACS (Futhark.SOAC SOACS)
copyNewlyConsumed was_consumed soac =
  case soac of
    Futhark.Screma w (Futhark.ScremaForm (scan_lam, scan_nes) reds map_lam) arrs -> do
      -- Copy any arrays that are consumed now, but were not in the
      -- constituents.
      arrs' <- mapM copyConsumedArr arrs
      -- Any consumed free variables will have to be copied inside the
      -- lambda, and we have to substitute the name of the copy for
      -- the original.
      map_lam' <- copyFreeInLambda map_lam
      let reds' = map (\red -> red { redLambda =
                                       Aliases.removeLambdaAliases
                                       (redLambda red)})
                  reds
      return $ Futhark.Screma w
        (Futhark.ScremaForm (Aliases.removeLambdaAliases scan_lam, scan_nes) reds' map_lam') arrs'

    _ -> return $ removeOpAliases soac
  where consumed = consumedInOp soac
        newly_consumed = consumed `namesSubtract` was_consumed

        copyConsumedArr a
          | a `nameIn` newly_consumed =
            letExp (baseString a <> "_copy") $ BasicOp $ Copy a
          | otherwise = return a

        copyFreeInLambda lam = do
          let free_consumed = consumedByLambda lam `namesSubtract`
                namesFromList (map paramName $ lambdaParams lam)
          (bnds, subst) <-
            foldM copyFree (mempty, mempty) $ namesToList free_consumed
          let lam' = Aliases.removeLambdaAliases lam
          return $ if null bnds
                   then lam'
                   else lam' { lambdaBody =
                                 insertStms bnds $
                                 substituteNames subst $ lambdaBody lam'
                             }

        copyFree (bnds, subst) v = do
          v_copy <- newVName $ baseString v <> "_copy"
          copy <- mkLetNamesM [v_copy] $ BasicOp $ Copy v
          return (oneStm copy<>bnds, M.insert v v_copy subst)

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
               infusible = mempty, kernels = M.empty }

mergeFusionRes :: FusedRes -> FusedRes -> FusionGM FusedRes
mergeFusionRes res1 res2 = do
    let ufus_mres = infusible res1 <> infusible res2
    inp_both <- expandSoacInpArr $ M.keys $ inpArr res1 `M.intersection` inpArr res2
    let m_unfus = ufus_mres <> mconcat (map oneName inp_both)
    return $ FusedRes  (rsucc     res1       ||      rsucc     res2)
                       (outArr    res1    `M.union`  outArr    res2)
                       (M.unionWith S.union (inpArr res1) (inpArr res2) )
                       m_unfus
                       (kernels   res1    `M.union`  kernels   res2)


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
    let newks = M.filter (not . null . fusedVars)      (kernels fres)
        newoa = M.filter (`M.member` newks)            (outArr  fres)
        newia = M.map    (S.filter (`M.member` newks)) (inpArr fres)
    in fres { outArr = newoa, inpArr = newia, kernels = newks }

--------------
--- Errors ---
--------------

errorIllegal :: String -> FusionGM FusedRes
errorIllegal soac_name =
    throwError $ Error
                  ("In Fusion.hs, soac "++soac_name++" appears illegally in pgm!")
