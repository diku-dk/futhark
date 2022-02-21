{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Perform horizontal and vertical fusion of SOACs.  See the paper
-- /A T2 Graph-Reduction Approach To Fusion/ for the basic idea (some
-- extensions discussed in /Design and GPGPU Performance of Futhark’s
-- Redomap Construct/).
module Futhark.Optimise.Fusion (fuseSOACs) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.Construct
import qualified Futhark.IR.Aliases as Aliases
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import Futhark.IR.SOACS.Simplify
import Futhark.Optimise.Fusion.LoopKernel
import Futhark.Pass
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (maxinum)

data VarEntry
  = IsArray VName (NameInfo SOACS) Names SOAC.Input
  | IsNotArray (NameInfo SOACS)

varEntryType :: VarEntry -> NameInfo SOACS
varEntryType (IsArray _ dec _ _) =
  dec
varEntryType (IsNotArray dec) =
  dec

varEntryAliases :: VarEntry -> Names
varEntryAliases (IsArray _ _ x _) = x
varEntryAliases _ = mempty

data FusionGEnv = FusionGEnv
  { -- | Mapping from variable name to its entire family.
    soacs :: M.Map VName [VName],
    varsInScope :: M.Map VName VarEntry,
    fusedRes :: FusedRes
  }

lookupArr :: VName -> FusionGEnv -> Maybe SOAC.Input
lookupArr v env = asArray =<< M.lookup v (varsInScope env)
  where
    asArray (IsArray _ _ _ input) = Just input
    asArray IsNotArray {} = Nothing

newtype Error = Error String

instance Show Error where
  show (Error msg) = "Fusion error:\n" ++ msg

newtype FusionGM a = FusionGM (ExceptT Error (StateT VNameSource (Reader FusionGEnv)) a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadError Error,
      MonadState VNameSource,
      MonadReader FusionGEnv
    )

instance MonadFreshNames FusionGM where
  getNameSource = get
  putNameSource = put

instance HasScope SOACS FusionGM where
  askScope = asks $ toScope . varsInScope
    where
      toScope = M.map varEntryType

------------------------------------------------------------------------
--- Monadic Helpers: bind/new/runFusionGatherM, etc
------------------------------------------------------------------------

-- | Binds an array name to the set of used-array vars
bindVar :: FusionGEnv -> (Ident, Names) -> FusionGEnv
bindVar env (Ident name t, aliases) =
  env {varsInScope = M.insert name entry $ varsInScope env}
  where
    entry = case t of
      Array {} -> IsArray name (LetName t) aliases' $ SOAC.identInput $ Ident name t
      _ -> IsNotArray $ LetName t
    expand = maybe mempty varEntryAliases . flip M.lookup (varsInScope env)
    aliases' = aliases <> mconcat (map expand $ namesToList aliases)

bindVars :: FusionGEnv -> [(Ident, Names)] -> FusionGEnv
bindVars = foldl bindVar

binding :: [(Ident, Names)] -> FusionGM a -> FusionGM a
binding vs = local (`bindVars` vs)

gatherStmPat :: Pat Type -> Exp SOACS -> FusionGM FusedRes -> FusionGM FusedRes
gatherStmPat pat e = binding $ zip idents aliases
  where
    idents = patIdents pat
    aliases = expAliases (Alias.analyseExp mempty e)

bindingPat :: Pat Type -> FusionGM a -> FusionGM a
bindingPat = binding . (`zip` repeat mempty) . patIdents

bindingParams :: Typed t => [Param t] -> FusionGM a -> FusionGM a
bindingParams = binding . (`zip` repeat mempty) . map paramIdent

-- | Binds an array name to the set of soac-produced vars
bindingFamilyVar :: [VName] -> FusionGEnv -> Ident -> FusionGEnv
bindingFamilyVar faml env (Ident nm t) =
  env
    { soacs = M.insert nm faml $ soacs env,
      varsInScope =
        M.insert
          nm
          ( IsArray nm (LetName t) mempty $
              SOAC.identInput $ Ident nm t
          )
          $ varsInScope env
    }

varAliases :: VName -> FusionGM Names
varAliases v =
  asks $
    (oneName v <>) . maybe mempty varEntryAliases
      . M.lookup v
      . varsInScope

varsAliases :: Names -> FusionGM Names
varsAliases = fmap mconcat . mapM varAliases . namesToList

updateKerInPlaces :: FusedRes -> ([VName], [VName]) -> FusionGM FusedRes
updateKerInPlaces res (ip_vs, other_infuse_vs) = do
  res' <- foldM addVarToInfusible res (ip_vs ++ other_infuse_vs)
  aliases <- mconcat <$> mapM varAliases ip_vs
  let inspectKer k = k {inplace = aliases <> inplace k}
  return res' {kernels = M.map inspectKer $ kernels res'}

checkForUpdates :: FusedRes -> Exp SOACS -> FusionGM FusedRes
checkForUpdates res (BasicOp (Update _ src slice _)) = do
  let ifvs = namesToList $ freeIn slice
  updateKerInPlaces res ([src], ifvs)
checkForUpdates res (BasicOp (FlatUpdate src slice _)) = do
  let ifvs = namesToList $ freeIn slice
  updateKerInPlaces res ([src], ifvs)
checkForUpdates res (Op (Futhark.Scatter _ _ _ written_info)) = do
  let updt_arrs = map (\(_, _, x) -> x) written_info
  updateKerInPlaces res (updt_arrs, [])
checkForUpdates res _ = return res

-- | Updates the environment: (i) the @soacs@ (map) by binding each pattern
--   element identifier to all pattern elements (identifiers) and (ii) the
--   variables in scope (map) by inserting each (pattern-array) name.
--   Finally, if the binding is an in-place update, then the @inplace@ field
--   of each (result) kernel is updated with the new in-place updates.
bindingFamily :: Pat Type -> FusionGM FusedRes -> FusionGM FusedRes
bindingFamily pat = local bind
  where
    idents = patIdents pat
    family = patNames pat
    bind env = foldl (bindingFamilyVar family) env idents

bindingTransform :: PatElem Type -> VName -> SOAC.ArrayTransform -> FusionGM a -> FusionGM a
bindingTransform pe srcname trns = local $ \env ->
  case M.lookup srcname $ varsInScope env of
    Just (IsArray src' _ aliases input) ->
      env
        { varsInScope =
            M.insert
              vname
              ( IsArray src' (LetName dec) (oneName srcname <> aliases) $
                  trns `SOAC.addTransform` input
              )
              $ varsInScope env
        }
    _ -> bindVar env (patElemIdent pe, oneName vname)
  where
    vname = patElemName pe
    dec = patElemDec pe

-- | Binds the fusion result to the environment.
bindRes :: FusedRes -> FusionGM a -> FusionGM a
bindRes rrr = local (\x -> x {fusedRes = rrr})

-- | The fusion transformation runs in this monad.  The mutable
-- state refers to the fresh-names engine.
-- The reader hides the vtable that associates ... to ... (fill in, please).
-- The 'Either' monad is used for error handling.
runFusionGatherM ::
  MonadFreshNames m =>
  FusionGM a ->
  FusionGEnv ->
  m (Either Error a)
runFusionGatherM (FusionGM a) env =
  modifyNameSource $ \src -> runReader (runStateT (runExceptT a) src) env

------------------------------------------------------------------------
--- Fusion Entry Points: gather the to-be-fused kernels@pgm level    ---
---    and fuse them in a second pass!                               ---
------------------------------------------------------------------------

-- | The pass definition.
fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass
    { passName = "Fuse SOACs",
      passDescription = "Perform higher-order optimisation, i.e., fusion.",
      passFunction = \prog ->
        simplifySOACS =<< renameProg
          =<< intraproceduralTransformationWithConsts
            (fuseConsts (freeIn (progFuns prog)))
            fuseFun
            prog
    }

fuseConsts :: Names -> Stms SOACS -> PassM (Stms SOACS)
fuseConsts used_consts consts =
  fuseStms mempty consts $ varsRes $ namesToList used_consts

fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun consts fun = do
  stms <-
    fuseStms
      (scopeOf consts <> scopeOfFParams (funDefParams fun))
      (bodyStms $ funDefBody fun)
      (bodyResult $ funDefBody fun)
  let body = (funDefBody fun) {bodyStms = stms}
  return fun {funDefBody = body}

fuseStms :: Scope SOACS -> Stms SOACS -> Result -> PassM (Stms SOACS)
fuseStms scope stms res = do
  let env =
        FusionGEnv
          { soacs = M.empty,
            varsInScope = mempty,
            fusedRes = mempty
          }
  k <-
    cleanFusionResult
      <$> liftEitherM
        ( runFusionGatherM
            (binding scope' $ fusionGatherStms mempty (stmsToList stms) res)
            env
        )
  if not $ rsucc k
    then return stms
    else liftEitherM $ runFusionGatherM (binding scope' $ bindRes k $ fuseInStms stms) env
  where
    scope' = map toBind $ M.toList scope
    toBind (k, t) = (Ident k $ typeOf t, mempty)

---------------------------------------------------
---------------------------------------------------
---- RESULT's Data Structure
---------------------------------------------------
---------------------------------------------------

-- | A type used for (hopefully) uniquely referring a producer SOAC.
-- The uniquely identifying value is the name of the first array
-- returned from the SOAC.
newtype KernName = KernName {unKernName :: VName}
  deriving (Eq, Ord, Show)

data FusedRes = FusedRes
  { -- | Whether we have fused something anywhere.
    rsucc :: Bool,
    -- | Associates an array to the name of the
    -- SOAC kernel that has produced it.
    outArr :: M.Map VName KernName,
    -- | Associates an array to the names of the
    -- SOAC kernels that uses it. These sets include
    -- only the SOAC input arrays used as full variables, i.e., no `a[i]'.
    inpArr :: M.Map VName (S.Set KernName),
    -- | the (names of) arrays that are not fusible, i.e.,
    --
    --   1. they are either used other than input to SOAC kernels, or
    --
    --   2. are used as input to at least two different kernels that
    --      are not located on disjoint control-flow branches, or
    --
    --   3. are used in the lambda expression of SOACs
    infusible :: Names,
    -- | The map recording the uses
    kernels :: M.Map KernName FusedKer
  }

instance Semigroup FusedRes where
  res1 <> res2 =
    FusedRes
      (rsucc res1 || rsucc res2)
      (outArr res1 `M.union` outArr res2)
      (M.unionWith S.union (inpArr res1) (inpArr res2))
      (infusible res1 <> infusible res2)
      (kernels res1 `M.union` kernels res2)

instance Monoid FusedRes where
  mempty =
    FusedRes
      { rsucc = False,
        outArr = M.empty,
        inpArr = M.empty,
        infusible = mempty,
        kernels = M.empty
      }

isInpArrInResModKers :: FusedRes -> S.Set KernName -> VName -> Bool
isInpArrInResModKers ress kers nm =
  case M.lookup nm (inpArr ress) of
    Nothing -> False
    Just s -> not $ S.null $ s `S.difference` kers

getKersWithInpArrs :: FusedRes -> [VName] -> S.Set KernName
getKersWithInpArrs ress =
  S.unions . mapMaybe (`M.lookup` inpArr ress)

-- | extend the set of names to include all the names
--     produced via SOACs (by querring the vtable's soac)
expandSoacInpArr :: [VName] -> FusionGM [VName]
expandSoacInpArr =
  foldM
    ( \y nm -> do
        stm <- asks $ M.lookup nm . soacs
        case stm of
          Nothing -> return (y ++ [nm])
          Just nns -> return (y ++ nns)
    )
    []

----------------------------------------------------------------------
----------------------------------------------------------------------

soacInputs :: SOAC -> FusionGM ([VName], [VName])
soacInputs soac = do
  let (inp_idds, other_idds) = getIdentArr $ SOAC.inputs soac
      (inp_nms0, other_nms0) = (inp_idds, other_idds)
  inp_nms <- expandSoacInpArr inp_nms0
  other_nms <- expandSoacInpArr other_nms0
  return (inp_nms, other_nms)

addNewKerWithInfusible :: FusedRes -> ([Ident], StmAux (), SOAC, Names) -> Names -> FusionGM FusedRes
addNewKerWithInfusible res (idd, aux, soac, consumed) ufs = do
  nm_ker <- KernName <$> newVName "ker"
  scope <- askScope
  let out_nms = map identName idd
      new_ker = newKernel aux soac consumed out_nms scope
      comb = M.unionWith S.union
      os' =
        M.fromList [(arr, nm_ker) | arr <- out_nms]
          `M.union` outArr res
      is' =
        M.fromList
          [ (arr, S.singleton nm_ker)
            | arr <- map SOAC.inputArray $ SOAC.inputs soac
          ]
          `comb` inpArr res
  return $
    FusedRes
      (rsucc res)
      os'
      is'
      ufs
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
      return $ SOAC.Input (ts2 <> ts) v2 t2

inlineSOACInputs :: SOAC -> FusionGM SOAC
inlineSOACInputs soac = do
  inputs' <- mapM inlineSOACInput $ SOAC.inputs soac
  return $ inputs' `SOAC.setInputs` soac

-- | Attempts to fuse between SOACs. Input:
--   @rem_stms@ are the bindings remaining in the current body after @orig_soac@.
--   @lam_used_nms@ the infusible names
--   @res@ the fusion result (before processing the current soac)
--   @orig_soac@ and @out_idds@ the current SOAC and its binding pattern
--   @consumed@ is the set of names consumed by the SOAC.
--   Output: a new Fusion Result (after processing the current SOAC binding)
greedyFuse ::
  [Stm SOACS] ->
  Names ->
  FusedRes ->
  (Pat Type, StmAux (), SOAC, Names) ->
  FusionGM FusedRes
greedyFuse rem_stms lam_used_nms res (out_idds, aux, orig_soac, consumed) = do
  soac <- inlineSOACInputs orig_soac
  (inp_nms, other_nms) <- soacInputs soac
  -- Assumption: the free vars in lambda are already in @infusible res@.
  let out_nms = patNames out_idds
      isInfusible = (`nameIn` infusible res)
      is_screma = case orig_soac of
        SOAC.Screma _ form _ ->
          (isJust (isRedomapSOAC form) || isJust (isScanomapSOAC form))
            && not (isJust (isReduceSOAC form) || isJust (isScanSOAC form))
        _ -> False
  --
  -- Conditions for fusion:
  -- If current soac is a replicate OR (current soac a redomap/scanomap AND
  --    (i) none of @out_idds@ belongs to the infusible set)
  -- THEN try applying producer-consumer fusion
  -- ELSE try applying horizontal        fusion
  -- (without duplicating computation in both cases)

  (ok_kers_compat, fused_kers, fused_nms, old_kers, oldker_nms) <-
    if is_screma || any isInfusible out_nms
      then horizontGreedyFuse rem_stms res (out_idds, aux, soac, consumed)
      else prodconsGreedyFuse res (out_idds, aux, soac, consumed)
  --
  -- (ii) check whether fusing @soac@ will violate any in-place update
  --      restriction, e.g., would move an input array past its in-place update.
  all_used_names <-
    fmap mconcat . mapM varAliases . namesToList $
      mconcat [lam_used_nms, namesFromList inp_nms, namesFromList other_nms]
  let has_inplace ker = inplace ker `namesIntersect` all_used_names
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
  let mod_kerS = if fusible_ker then S.fromList oldker_nms else mempty
  let used_inps = filter (isInpArrInResModKers res mod_kerS) inp_nms
  let ufs =
        mconcat
          [ infusible res,
            namesFromList used_inps,
            namesFromList other_nms
              `namesSubtract` namesFromList (map SOAC.inputArray $ SOAC.inputs soac)
          ]
  let comb = M.unionWith S.union

  if not fusible_ker
    then addNewKerWithInfusible res (patIdents out_idds, aux, soac, consumed) ufs
    else do
      -- Need to suitably update `inpArr':
      --   (i) first remove the inpArr bindings of the old kernel
      let inpArr' =
            foldl
              ( \inpa (kold, knm) ->
                  S.foldl'
                    ( \inpp nm ->
                        case M.lookup nm inpp of
                          Nothing -> inpp
                          Just s ->
                            let new_set = S.delete knm s
                             in if S.null new_set
                                  then M.delete nm inpp
                                  else M.insert nm new_set inpp
                    )
                    inpa
                    $ arrInputs kold
              )
              (inpArr res)
              (zip old_kers oldker_nms)
      --  (ii) then add the inpArr bindings of the new kernel
      let fused_ker_nms = zip fused_nms fused_kers
          inpArr'' =
            foldl
              ( \inpa' (knm, knew) ->
                  M.fromList
                    [ (k, S.singleton knm)
                      | k <- S.toList $ arrInputs knew
                    ]
                    `comb` inpa'
              )
              inpArr'
              fused_ker_nms
      -- Update the kernels map (why not delete the ones that have been fused?)
      let kernels' = M.fromList fused_ker_nms `M.union` kernels res
      -- nothing to do for `outArr' (since we have not added a new kernel)
      -- DO IMPROVEMENT: attempt to fuse the resulting kernel AGAIN until it fails,
      --                 but make sure NOT to add a new kernel!
      return $ FusedRes True (outArr res) inpArr'' ufs kernels'

prodconsGreedyFuse ::
  FusedRes ->
  (Pat Type, StmAux (), SOAC, Names) ->
  FusionGM (Bool, [FusedKer], [KernName], [FusedKer], [KernName])
prodconsGreedyFuse res (out_idds, aux, soac, consumed) = do
  let out_nms = patNames out_idds -- Extract VNames from output patterns
      to_fuse_knmSet = getKersWithInpArrs res out_nms -- Find kernels which consume outputs
      to_fuse_knms = S.toList to_fuse_knmSet
      lookup_kern k = case M.lookup k (kernels res) of
        Nothing ->
          throwError $
            Error
              ( "In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                  ++ "kernel name not found in kernels field!"
              )
        Just ker -> return ker
  to_fuse_kers <- mapM lookup_kern to_fuse_knms -- Get all consumer kernels
  -- try producer-consumer fusion
  (ok_kers_compat, fused_kers) <- do
    kers <-
      forM to_fuse_kers $
        attemptFusion mempty (patNames out_idds) soac consumed
    case sequence kers of
      Nothing -> return (False, [])
      Just kers' -> return (True, map certifyKer kers')
  return (ok_kers_compat, fused_kers, to_fuse_knms, to_fuse_kers, to_fuse_knms)
  where
    certifyKer k = k {kerAux = kerAux k <> aux}

horizontGreedyFuse ::
  [Stm SOACS] ->
  FusedRes ->
  (Pat Type, StmAux (), SOAC, Names) ->
  FusionGM (Bool, [FusedKer], [KernName], [FusedKer], [KernName])
horizontGreedyFuse rem_stms res (out_idds, aux, soac, consumed) = do
  (inp_nms, _) <- soacInputs soac
  let out_nms = patNames out_idds
      infusible_nms = namesFromList $ filter (`nameIn` infusible res) out_nms
      out_arr_nms = case soac of
        -- the accumulator result cannot be fused!
        SOAC.Screma _ (ScremaForm scans reds _) _ ->
          drop (scanResults scans + redResults reds) out_nms
        SOAC.Stream _ _ _ nes _ -> drop (length nes) out_nms
        _ -> out_nms
      to_fuse_knms1 = S.toList $ getKersWithInpArrs res (out_arr_nms ++ inp_nms)
      to_fuse_knms2 = getKersWithSameInpSize (SOAC.width soac) res
      to_fuse_knms = S.toList $ S.fromList $ to_fuse_knms1 ++ to_fuse_knms2
      lookupKernel k = case M.lookup k (kernels res) of
        Nothing ->
          throwError $
            Error
              ( "In Fusion.hs, greedyFuse, comp of to_fuse_kers: "
                  ++ "kernel name not found in kernels field!"
              )
        Just ker -> return ker

  -- For each kernel get the index in the bindings where the kernel is
  -- located and sort based on the index so that partial fusion may
  -- succeed.  We use the last position where one of the kernel
  -- outputs occur.
  let stm_nms = map (patNames . stmPat) rem_stms
  kernminds <- forM to_fuse_knms $ \ker_nm -> do
    ker <- lookupKernel ker_nm
    case mapMaybe (\out_nm -> L.findIndex (elem out_nm) stm_nms) (outNames ker) of
      [] -> return Nothing
      is -> return $ Just (ker, ker_nm, maxinum is)

  scope <- askScope
  let kernminds' = L.sortBy (\(_, _, i1) (_, _, i2) -> compare i1 i2) $ catMaybes kernminds
      soac_kernel = newKernel aux soac consumed out_nms scope

  -- now try to fuse kernels one by one (in a fold); @ok_ind@ is the index of the
  -- kernel until which fusion succeded, and @fused_ker@ is the resulting kernel.
  (_, ok_ind, _, fused_ker, _) <-
    foldM
      ( \(cur_ok, n, prev_ind, cur_ker, ufus_nms) (ker, _ker_nm, stm_ind) -> do
          -- check that we still try fusion and that the intermediate
          -- bindings do not use the results of cur_ker
          let curker_outnms = outNames cur_ker
              curker_outset = namesFromList curker_outnms
              new_ufus_nms = namesFromList $ outNames ker ++ namesToList ufus_nms
              -- disable horizontal fusion in the case when an output array of
              -- producer SOAC is a non-trivially transformed input of the consumer
              out_transf_ok =
                let ker_inp = SOAC.inputs $ fsoac ker
                    unfuse1 =
                      namesFromList (map SOAC.inputArray ker_inp)
                        `namesSubtract` namesFromList (mapMaybe SOAC.isVarInput ker_inp)
                    unfuse2 = namesIntersection curker_outset ufus_nms
                 in not $ unfuse1 `namesIntersect` unfuse2
              -- Disable horizontal fusion if consumer has any
              -- output transforms.
              cons_no_out_transf = SOAC.nullTransforms $ outputTransform ker

          -- check that consumer's lambda body does not use
          -- directly the produced arrays (e.g., see noFusion3.fut).
          let consumer_ok =
                not $
                  curker_outset
                    `namesIntersect` freeIn (lambdaBody $ SOAC.lambda $ fsoac ker)

          let interm_stms_ok =
                cur_ok && consumer_ok && out_transf_ok && cons_no_out_transf
                  && foldl
                    ( \ok stm ->
                        ok
                          && not (curker_outset `namesIntersect` freeIn (stmExp stm))
                          -- hardwired to False after first fail
                          -- (i) check that the in-between bindings do
                          --     not use the result of current kernel OR
                          ||
                          --(ii) that the pattern-binding corresponds to
                          --     the result of the consumer kernel; in the
                          --     latter case it means it corresponds to a
                          --     kernel that has been fused in the consumer,
                          --     hence it should be ignored
                          not
                            ( null $
                                curker_outnms
                                  `L.intersect` patNames (stmPat stm)
                            )
                    )
                    True
                    (drop (prev_ind + 1) $ take stm_ind rem_stms)
          if not interm_stms_ok
            then return (False, n, stm_ind, cur_ker, mempty)
            else do
              new_ker <-
                attemptFusion
                  ufus_nms
                  (outNames cur_ker)
                  (fsoac cur_ker)
                  (fusedConsumed cur_ker)
                  ker
              case new_ker of
                Nothing -> return (False, n, stm_ind, cur_ker, mempty)
                Just krn ->
                  let krn' = krn {kerAux = aux <> kerAux krn}
                   in return (True, n + 1, stm_ind, krn', new_ufus_nms)
      )
      (True, 0, 0, soac_kernel, infusible_nms)
      kernminds'

  -- Find the kernels we have fused into and the name of the last such
  -- kernel (if any).
  let (to_fuse_kers', to_fuse_knms', _) = unzip3 $ take ok_ind kernminds'
      new_kernms = drop (ok_ind - 1) to_fuse_knms'

  return (ok_ind > 0, [fused_ker], new_kernms, to_fuse_kers', to_fuse_knms')
  where
    getKersWithSameInpSize :: SubExp -> FusedRes -> [KernName]
    getKersWithSameInpSize sz ress =
      map fst $ filter (\(_, ker) -> sz == SOAC.width (fsoac ker)) $ M.toList $ kernels ress

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

fusionGatherBody :: FusedRes -> Body SOACS -> FusionGM FusedRes
fusionGatherBody fres (Body _ stms res) =
  fusionGatherStms fres (stmsToList stms) res

fusionGatherStms :: FusedRes -> [Stm SOACS] -> Result -> FusionGM FusedRes
-- Some forms of do-loops can profitably be considered streamSeqs.  We
-- are careful to ensure that the generated nested loop cannot itself
-- be considered a stream, to avoid infinite recursion.
fusionGatherStms
  fres
  (Let (Pat pes) stmtp (DoLoop merge (ForLoop i it w loop_vars) body) : stms)
  res
    | not $ null loop_vars = do
      let (merge_params, merge_init) = unzip merge
          (loop_params, loop_arrs) = unzip loop_vars
      chunk_param <- newParam "chunk_size" $ Prim int64
      offset_param <- newParam "offset" $ Prim $ IntType it
      let offset = paramName offset_param
          chunk_size = paramName chunk_param

      acc_params <- forM merge_params $ \p ->
        newParam (baseString (paramName p) ++ "_outer") (paramType p)

      chunked_params <- forM loop_vars $ \(p, arr) ->
        newParam
          (baseString arr ++ "_chunk")
          (paramType p `arrayOfRow` Futhark.Var chunk_size)

      let lam_params = chunk_param : acc_params ++ [offset_param] ++ chunked_params

      lam_body <- runBodyBuilder $
        localScope (scopeOfLParams lam_params) $ do
          let merge' = zip merge_params $ map (Futhark.Var . paramName) acc_params
          j <- newVName "j"
          loop_body <- runBodyBuilder $ do
            forM_ (zip loop_params chunked_params) $ \(p, a_p) ->
              letBindNames [paramName p] $
                BasicOp $
                  Index (paramName a_p) $
                    fullSlice (paramType a_p) [DimFix $ Futhark.Var j]
            letBindNames [i] $ BasicOp $ BinOp (Add it OverflowUndef) (Futhark.Var offset) (Futhark.Var j)
            return body
          eBody
            [ pure $
                DoLoop merge' (ForLoop j it (Futhark.Var chunk_size) []) loop_body,
              pure $
                BasicOp $ BinOp (Add Int64 OverflowUndef) (Futhark.Var offset) (Futhark.Var chunk_size)
            ]
      let lam =
            Lambda
              { lambdaParams = lam_params,
                lambdaBody = lam_body,
                lambdaReturnType = map paramType $ acc_params ++ [offset_param]
              }
          stream = Futhark.Stream w loop_arrs Sequential (merge_init ++ [intConst it 0]) lam

      -- It is important that the (discarded) final-offset is not the
      -- first element in the pattern, as we use the first element to
      -- identify the SOAC in the second phase of fusion.
      discard <- newVName "discard"
      let discard_pe = PatElem discard $ Prim int64

      fusionGatherStms
        fres
        (Let (Pat (pes <> [discard_pe])) stmtp (Op stream) : stms)
        res
fusionGatherStms fres (stm@(Let pat _ e) : stms) res = do
  maybesoac <- SOAC.fromExp e
  case maybesoac of
    Right soac@(SOAC.Scatter _len lam _ivs _as) -> do
      -- We put the variables produced by Scatter into the infusible
      -- set to force horizontal fusion.  It is not possible to
      -- producer/consumer-fuse Scatter anyway.
      fres' <- addNamesToInfusible fres $ namesFromList $ patNames pat
      fres'' <- mapLike fres' soac lam
      checkForUpdates fres'' e
    Right soac@(SOAC.Hist _ _ lam _) -> do
      -- We put the variables produced by Hist into the infusible
      -- set to force horizontal fusion.  It is not possible to
      -- producer/consumer-fuse Hist anyway.
      fres' <- addNamesToInfusible fres $ namesFromList $ patNames pat
      mapLike fres' soac lam
    Right soac@(SOAC.Screma _ (ScremaForm scans reds map_lam) _) ->
      reduceLike soac (map scanLambda scans <> map redLambda reds <> [map_lam]) $
        concatMap scanNeutral scans <> concatMap redNeutral reds
    Right soac@(SOAC.Stream _ form lam nes _) -> do
      -- a redomap does not neccessarily start a new kernel, e.g.,
      -- @let a= reduce(+,0,A) in ... stms ... in let B = map(f,A)@
      -- can be fused into a redomap that replaces the @map@, if @a@
      -- and @B@ are defined in the same scope and @stms@ does not uses @a@.
      -- a redomap always starts a new kernel
      let lambdas = case form of
            Parallel _ _ lout -> [lout, lam]
            Sequential -> [lam]
      reduceLike soac lambdas nes
    _
      | Pat [pe] <- pat,
        Just (src, trns) <- SOAC.transformFromExp (stmCerts stm) e ->
        bindingTransform pe src trns $ fusionGatherStms fres stms res
      | otherwise -> do
        let pat_vars = map (BasicOp . SubExp . Var) $ patNames pat
        bres <- gatherStmPat pat e $ fusionGatherStms fres stms res
        bres' <- checkForUpdates bres e
        foldM fusionGatherExp bres' (e : pat_vars)
  where
    aux = stmAux stm
    rem_stms = stm : stms
    consumed = consumedInExp $ Alias.analyseExp mempty e

    reduceLike soac lambdas nes = do
      (used_lam, lres) <- foldM fusionGatherLam (mempty, fres) lambdas
      bres <- bindingFamily pat $ fusionGatherStms lres stms res
      bres' <- foldM fusionGatherSubExp bres nes
      consumed' <- varsAliases consumed
      greedyFuse rem_stms used_lam bres' (pat, aux, soac, consumed')

    mapLike fres' soac lambda = do
      bres <- bindingFamily pat $ fusionGatherStms fres' stms res
      (used_lam, blres) <- fusionGatherLam (mempty, bres) lambda
      consumed' <- varsAliases consumed
      greedyFuse rem_stms used_lam blres (pat, aux, soac, consumed')
fusionGatherStms fres [] res =
  foldM fusionGatherExp fres $ map (BasicOp . SubExp . resSubExp) res

fusionGatherExp :: FusedRes -> Exp SOACS -> FusionGM FusedRes
fusionGatherExp fres (DoLoop merge form loop_body) = do
  fres' <- addNamesToInfusible fres $ freeIn form <> freeIn merge
  let form_idents =
        case form of
          ForLoop i it _ loopvars ->
            Ident i (Prim (IntType it)) : map (paramIdent . fst) loopvars
          WhileLoop {} -> []

  new_res <-
    binding (zip (form_idents ++ map (paramIdent . fst) merge) $ repeat mempty) $
      fusionGatherBody mempty loop_body
  -- make the inpArr infusible, so that they
  -- cannot be fused from outside the loop:
  let (inp_arrs, _) = unzip $ M.toList $ inpArr new_res
  let new_res' = new_res {infusible = infusible new_res <> mconcat (map oneName inp_arrs)}
  -- merge new_res with fres'
  return $ new_res' <> fres'
fusionGatherExp fres (If cond e_then e_else _) = do
  then_res <- fusionGatherBody mempty e_then
  else_res <- fusionGatherBody mempty e_else
  let both_res = then_res <> else_res
  fres' <- fusionGatherSubExp fres cond
  mergeFusionRes fres' both_res
fusionGatherExp fres (WithAcc inps lam) = do
  (_, fres') <- fusionGatherLam (mempty, fres) lam
  addNamesToInfusible fres' $ freeIn inps

-----------------------------------------------------------------------------------
--- Errors: all SOACs, (because normalization ensures they appear
--- directly in let exp, i.e., let x = e)
-----------------------------------------------------------------------------------

fusionGatherExp _ (Op Futhark.Screma {}) = errorIllegal "screma"
fusionGatherExp _ (Op Futhark.Scatter {}) = errorIllegal "write"
fusionGatherExp fres (Op (Futhark.VJP lam _ _)) =
  snd <$> fusionGatherLam (mempty, fres) lam
fusionGatherExp fres (Op (Futhark.JVP lam _ _)) =
  snd <$> fusionGatherLam (mempty, fres) lam
--
fusionGatherExp fres e = addNamesToInfusible fres $ freeIn e

fusionGatherSubExp :: FusedRes -> SubExp -> FusionGM FusedRes
fusionGatherSubExp fres (Var idd) = addVarToInfusible fres idd
fusionGatherSubExp fres _ = return fres

addNamesToInfusible :: FusedRes -> Names -> FusionGM FusedRes
addNamesToInfusible fres = foldM addVarToInfusible fres . namesToList

addVarToInfusible :: FusedRes -> VName -> FusionGM FusedRes
addVarToInfusible fres name = do
  trns <- asks $ lookupArr name
  let name' = case trns of
        Nothing -> name
        Just (SOAC.Input _ orig _) -> orig
  return fres {infusible = oneName name' <> infusible fres}

-- Lambdas create a new scope.  Disallow fusing from outside lambda by
-- adding inp_arrs to the infusible set.
fusionGatherLam :: (Names, FusedRes) -> Lambda SOACS -> FusionGM (Names, FusedRes)
fusionGatherLam (u_set, fres) (Lambda idds body _) = do
  new_res <- bindingParams idds $ fusionGatherBody mempty body
  -- make the inpArr infusible, so that they
  -- cannot be fused from outside the lambda:
  let inp_arrs = namesFromList $ M.keys $ inpArr new_res
  let unfus = infusible new_res <> inp_arrs
  stms <- asks $ M.keys . varsInScope
  let unfus' = unfus `namesIntersection` namesFromList stms
  -- merge fres with new_res'
  let new_res' = new_res {infusible = unfus'}
  -- merge new_res with fres'
  return (u_set <> unfus', new_res' <> fres)

-------------------------------------------------------------
-------------------------------------------------------------
--- FINALLY, Substitute the kernels in function
-------------------------------------------------------------
-------------------------------------------------------------

fuseInStms :: Stms SOACS -> FusionGM (Stms SOACS)
fuseInStms stms
  | Just (Let pat aux e, stms') <- stmsHead stms = do
    stms'' <- bindingPat pat $ fuseInStms stms'
    soac_stms <- replaceSOAC pat aux e
    pure $ soac_stms <> stms''
  | otherwise =
    pure mempty

fuseInBody :: Body SOACS -> FusionGM (Body SOACS)
fuseInBody (Body _ stms res) =
  Body () <$> fuseInStms stms <*> pure res

fuseInExp :: Exp SOACS -> FusionGM (Exp SOACS)
-- Handle loop specially because we need to bind the types of the
-- merge variables.
fuseInExp (DoLoop merge form loopbody) =
  binding (zip form_idents $ repeat mempty) $
    bindingParams (map fst merge) $
      DoLoop merge form <$> fuseInBody loopbody
  where
    form_idents = case form of
      WhileLoop {} -> []
      ForLoop i it _ loopvars ->
        Ident i (Prim $ IntType it) :
        map (paramIdent . fst) loopvars
fuseInExp e = mapExpM fuseIn e

fuseIn :: Mapper SOACS SOACS FusionGM
fuseIn =
  identityMapper
    { mapOnBody = const fuseInBody,
      mapOnOp = mapSOACM identitySOACMapper {mapOnSOACLambda = fuseInLambda}
    }

fuseInLambda :: Lambda SOACS -> FusionGM (Lambda SOACS)
fuseInLambda (Lambda params body rtp) = do
  body' <- bindingParams params $ fuseInBody body
  return $ Lambda params body' rtp

replaceSOAC :: Pat Type -> StmAux () -> Exp SOACS -> FusionGM (Stms SOACS)
replaceSOAC (Pat []) _ _ = return mempty
replaceSOAC pat@(Pat (patElem : _)) aux e = do
  fres <- asks fusedRes
  let pat_nm = patElemName patElem
      names = patIdents pat
  case M.lookup pat_nm (outArr fres) of
    Nothing ->
      oneStm . Let pat aux <$> fuseInExp e
    Just knm ->
      case M.lookup knm (kernels fres) of
        Nothing ->
          throwError $
            Error
              ( "In Fusion.hs, replaceSOAC, outArr in ker_name "
                  ++ "which is not in Res: "
                  ++ pretty (unKernName knm)
              )
        Just ker -> do
          when (null $ fusedVars ker) $
            throwError $
              Error
                ( "In Fusion.hs, replaceSOAC, unfused kernel "
                    ++ "still in result: "
                    ++ pretty names
                )
          insertKerSOAC aux (outNames ker) ker

insertKerSOAC :: StmAux () -> [VName] -> FusedKer -> FusionGM (Stms SOACS)
insertKerSOAC aux names ker = do
  new_soac' <- finaliseSOAC $ fsoac ker
  runBuilder_ $ do
    f_soac <- SOAC.toSOAC new_soac'
    -- The fused kernel may consume more than the original SOACs (see
    -- issue #224).  We insert copy expressions to fix it.
    f_soac' <- copyNewlyConsumed (fusedConsumed ker) $ addOpAliases mempty f_soac
    validents <- zipWithM newIdent (map baseString names) $ SOAC.typeOf new_soac'
    auxing (kerAux ker <> aux) $ letBind (basicPat validents) $ Op f_soac'
    transformOutput (outputTransform ker) names validents

-- | Perform simplification and fusion inside the lambda(s) of a SOAC.
finaliseSOAC :: SOAC.SOAC SOACS -> FusionGM (SOAC.SOAC SOACS)
finaliseSOAC new_soac =
  case new_soac of
    SOAC.Screma w (ScremaForm scans reds map_lam) arrs -> do
      scans' <- forM scans $ \(Scan scan_lam scan_nes) -> do
        scan_lam' <- simplifyAndFuseInLambda scan_lam
        return $ Scan scan_lam' scan_nes

      reds' <- forM reds $ \(Reduce comm red_lam red_nes) -> do
        red_lam' <- simplifyAndFuseInLambda red_lam
        return $ Reduce comm red_lam' red_nes

      map_lam' <- simplifyAndFuseInLambda map_lam

      return $ SOAC.Screma w (ScremaForm scans' reds' map_lam') arrs
    SOAC.Scatter w lam inps dests -> do
      lam' <- simplifyAndFuseInLambda lam
      return $ SOAC.Scatter w lam' inps dests
    SOAC.Hist w ops lam arrs -> do
      lam' <- simplifyAndFuseInLambda lam
      return $ SOAC.Hist w ops lam' arrs
    SOAC.Stream w form lam nes inps -> do
      lam' <- simplifyAndFuseInLambda lam
      return $ SOAC.Stream w form lam' nes inps

simplifyAndFuseInLambda :: Lambda SOACS -> FusionGM (Lambda SOACS)
simplifyAndFuseInLambda lam = do
  lam' <- simplifyLambda lam
  (_, nfres) <- fusionGatherLam (mempty, mkFreshFusionRes) lam'
  let nfres' = cleanFusionResult nfres
  bindRes nfres' $ fuseInLambda lam'

copyNewlyConsumed ::
  Names ->
  Futhark.SOAC (Aliases.Aliases SOACS) ->
  Builder SOACS (Futhark.SOAC SOACS)
copyNewlyConsumed was_consumed soac =
  case soac of
    Futhark.Screma w arrs (Futhark.ScremaForm scans reds map_lam) -> do
      -- Copy any arrays that are consumed now, but were not in the
      -- constituents.
      arrs' <- mapM copyConsumedArr arrs
      -- Any consumed free variables will have to be copied inside the
      -- lambda, and we have to substitute the name of the copy for
      -- the original.
      map_lam' <- copyFreeInLambda map_lam

      let scans' =
            map
              ( \scan ->
                  scan
                    { scanLambda =
                        Aliases.removeLambdaAliases
                          (scanLambda scan)
                    }
              )
              scans

      let reds' =
            map
              ( \red ->
                  red
                    { redLambda =
                        Aliases.removeLambdaAliases
                          (redLambda red)
                    }
              )
              reds

      return $ Futhark.Screma w arrs' $ Futhark.ScremaForm scans' reds' map_lam'
    _ -> return $ removeOpAliases soac
  where
    consumed = consumedInOp soac
    newly_consumed = consumed `namesSubtract` was_consumed

    copyConsumedArr a
      | a `nameIn` newly_consumed =
        letExp (baseString a <> "_copy") $ BasicOp $ Copy a
      | otherwise = return a

    copyFreeInLambda lam = do
      let free_consumed =
            consumedByLambda lam
              `namesSubtract` namesFromList (map paramName $ lambdaParams lam)
      (stms, subst) <-
        foldM copyFree (mempty, mempty) $ namesToList free_consumed
      let lam' = Aliases.removeLambdaAliases lam
      return $
        if null stms
          then lam'
          else
            lam'
              { lambdaBody =
                  insertStms stms $
                    substituteNames subst $ lambdaBody lam'
              }

    copyFree (stms, subst) v = do
      v_copy <- newVName $ baseString v <> "_copy"
      copy <- mkLetNamesM [v_copy] $ BasicOp $ Copy v
      return (oneStm copy <> stms, M.insert v v_copy subst)

---------------------------------------------------
---------------------------------------------------
---- HELPERS
---------------------------------------------------
---------------------------------------------------

-- | Get a new fusion result, i.e., for when entering a new scope,
--   e.g., a new lambda or a new loop.
mkFreshFusionRes :: FusedRes
mkFreshFusionRes =
  FusedRes
    { rsucc = False,
      outArr = M.empty,
      inpArr = M.empty,
      infusible = mempty,
      kernels = M.empty
    }

mergeFusionRes :: FusedRes -> FusedRes -> FusionGM FusedRes
mergeFusionRes res1 res2 = do
  let ufus_mres = infusible res1 <> infusible res2
  inp_both <- expandSoacInpArr $ M.keys $ inpArr res1 `M.intersection` inpArr res2
  let m_unfus = ufus_mres <> mconcat (map oneName inp_both)
  return $
    FusedRes
      (rsucc res1 || rsucc res2)
      (outArr res1 `M.union` outArr res2)
      (M.unionWith S.union (inpArr res1) (inpArr res2))
      m_unfus
      (kernels res1 `M.union` kernels res2)

-- | The expression arguments are supposed to be array-type exps.
--   Returns a tuple, in which the arrays that are vars are in the
--   first element of the tuple, and the one which are indexed or
--   transposes (or otherwise transformed) should be in the second.
--
--   E.g., for expression `mapT(f, a, b[i])', the result should be
--   `([a],[b])'
getIdentArr :: [SOAC.Input] -> ([VName], [VName])
getIdentArr = foldl comb ([], [])
  where
    comb (vs, os) (SOAC.Input ts idd _)
      | SOAC.nullTransforms ts = (idd : vs, os)
    comb (vs, os) inp =
      (vs, SOAC.inputArray inp : os)

cleanFusionResult :: FusedRes -> FusedRes
cleanFusionResult fres =
  let newks = M.filter (not . null . fusedVars) (kernels fres)
      newoa = M.filter (`M.member` newks) (outArr fres)
      newia = M.map (S.filter (`M.member` newks)) (inpArr fres)
   in fres {outArr = newoa, inpArr = newia, kernels = newks}

--------------
--- Errors ---
--------------

errorIllegal :: String -> FusionGM FusedRes
errorIllegal soac_name =
  throwError $
    Error
      ("In Fusion.hs, soac " ++ soac_name ++ " appears illegally in pgm!")
