{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Pass.ExtractKernels.Distribution
       (
         Target
       , Targets
       , ppTargets
       , singleTarget
       , outerTarget
       , innerTarget
       , pushInnerTarget
       , popInnerTarget
       , targetsScope

       , LoopNesting (..)
       , ppLoopNesting

       , Nesting (..)
       , Nestings
       , ppNestings
       , letBindInInnerNesting
       , singleNesting
       , pushInnerNesting

       , KernelNest
       , ppKernelNest
       , newKernel
       , pushKernelNesting
       , pushInnerKernelNesting
       , removeArraysFromNest
       , kernelNestLoops
       , kernelNestWidths
       , boundInKernelNest
       , boundInKernelNests
       , flatKernel
       , constructKernel

       , tryDistribute
       , tryDistributeStm
       )
       where

import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.Maybe
import Data.List

import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Util
import Futhark.Transform.Rename
import Futhark.Util.Log
import Futhark.Pass.ExtractKernels.BlockedKernel
  (mapKernel, KernelInput(..), readKernelInput, MkSegLevel)

type Target = (Pattern Kernels, Result)

-- | First pair element is the very innermost ("current") target.  In
-- the list, the outermost target comes first.  Invariant: Every
-- element of a pattern must be present as the result of the
-- immediately enclosing target.  This is ensured by 'pushInnerTarget'
-- by removing unused pattern elements.
data Targets = Targets { _innerTarget :: Target
                       , _outerTargets :: [Target]
                       }

ppTargets :: Targets -> String
ppTargets (Targets target targets) =
  unlines $ map ppTarget $ targets ++ [target]
  where ppTarget (pat, res) =
          pretty pat ++ " <- " ++ pretty res

singleTarget :: Target -> Targets
singleTarget = flip Targets []

outerTarget :: Targets -> Target
outerTarget (Targets inner_target []) = inner_target
outerTarget (Targets _ (outer_target : _)) = outer_target

innerTarget :: Targets -> Target
innerTarget (Targets inner_target _) = inner_target

pushOuterTarget :: Target -> Targets -> Targets
pushOuterTarget target (Targets inner_target targets) =
  Targets inner_target (target : targets)

pushInnerTarget :: Target -> Targets -> Targets
pushInnerTarget (pat, res) (Targets inner_target targets) =
  Targets (pat', res') (targets ++ [inner_target])
  where (pes', res') = unzip $ filter (used . fst) $ zip (patternElements pat) res
        pat' = Pattern [] pes'
        inner_used = freeIn $ snd inner_target
        used pe = patElemName pe `nameIn` inner_used

popInnerTarget :: Targets -> Maybe (Target, Targets)
popInnerTarget (Targets t ts) =
  case reverse ts of
    x:xs -> Just (t, Targets x $ reverse xs)
    []   -> Nothing

targetScope :: Target -> Scope Kernels
targetScope = scopeOfPattern . fst

targetsScope :: Targets -> Scope Kernels
targetsScope (Targets t ts) = mconcat $ map targetScope $ t : ts

data LoopNesting = MapNesting { loopNestingPattern :: Pattern Kernels
                              , loopNestingCertificates :: Certificates
                              , loopNestingWidth :: SubExp
                              , loopNestingParamsAndArrs :: [(Param Type, VName)]
                              }
                 deriving (Show)

instance Scoped Kernels LoopNesting where
  scopeOf = scopeOfLParams . map fst . loopNestingParamsAndArrs

ppLoopNesting :: LoopNesting -> String
ppLoopNesting (MapNesting _ _ _ params_and_arrs) =
  pretty (map fst params_and_arrs) ++
  " <- " ++
  pretty (map snd params_and_arrs)

loopNestingParams :: LoopNesting -> [LParam Kernels]
loopNestingParams  = map fst . loopNestingParamsAndArrs

instance FreeIn LoopNesting where
  freeIn' (MapNesting pat cs w params_and_arrs) =
    freeIn' pat <>
    freeIn' cs <>
    freeIn' w <>
    freeIn' params_and_arrs

data Nesting = Nesting { nestingLetBound :: Names
                       , nestingLoop :: LoopNesting
                       }
             deriving (Show)

letBindInNesting :: Names -> Nesting -> Nesting
letBindInNesting newnames (Nesting oldnames loop) =
  Nesting (oldnames <> newnames) loop

-- ^ First pair element is the very innermost ("current") nest.  In
-- the list, the outermost nest comes first.
type Nestings = (Nesting, [Nesting])

ppNestings :: Nestings -> String
ppNestings (nesting, nestings) =
  unlines $ map ppNesting $ nestings ++ [nesting]
  where ppNesting (Nesting _ loop) =
          ppLoopNesting loop

singleNesting :: Nesting -> Nestings
singleNesting = (,[])

pushInnerNesting :: Nesting -> Nestings -> Nestings
pushInnerNesting nesting (inner_nesting, nestings) =
  (nesting, nestings ++ [inner_nesting])

-- | Both parameters and let-bound.
boundInNesting :: Nesting -> Names
boundInNesting nesting =
  namesFromList (map paramName (loopNestingParams loop)) <>
  nestingLetBound nesting
  where loop = nestingLoop nesting

letBindInInnerNesting :: Names -> Nestings -> Nestings
letBindInInnerNesting names (nest, nestings) =
  (letBindInNesting names nest, nestings)


-- | Note: first element is *outermost* nesting.  This is different
-- from the similar types elsewhere!
type KernelNest = (LoopNesting, [LoopNesting])

ppKernelNest :: KernelNest -> String
ppKernelNest (nesting, nestings) =
  unlines $ map ppLoopNesting $ nesting : nestings

-- | Add new outermost nesting, pushing the current outermost to the
-- list, also taking care to swap patterns if necessary.
pushKernelNesting :: Target -> LoopNesting -> KernelNest -> KernelNest
pushKernelNesting target newnest (nest, nests) =
  (fixNestingPatternOrder newnest target (loopNestingPattern nest),
   nest : nests)

-- | Add new innermost nesting, pushing the current outermost to the
-- list.  It is important that the 'Target' has the right order
-- (non-permuted compared to what is expected by the outer nests).
pushInnerKernelNesting :: Target -> LoopNesting -> KernelNest -> KernelNest
pushInnerKernelNesting target newnest (nest, nests) =
  (nest, nests ++ [fixNestingPatternOrder newnest target (loopNestingPattern innermost)])
  where innermost = case reverse nests of
          []  -> nest
          n:_ -> n

fixNestingPatternOrder :: LoopNesting -> Target -> Pattern Kernels -> LoopNesting
fixNestingPatternOrder nest (_,res) inner_pat =
  nest { loopNestingPattern = basicPattern [] pat' }
  where pat = loopNestingPattern nest
        pat' = map fst fixed_target
        fixed_target = sortOn posInInnerPat $ zip (patternValueIdents pat) res
        posInInnerPat (_, Var v) = fromMaybe 0 $ elemIndex v $ patternNames inner_pat
        posInInnerPat _          = 0

-- | Remove these arrays from the outermost nesting, and all
-- uses of corresponding parameters from innermost nesting.
removeArraysFromNest :: [VName] -> KernelNest -> KernelNest
removeArraysFromNest orig_arrs (outer, inners) =
  let (arrs, outer') = remove (namesFromList orig_arrs) outer
      (_, inners') = mapAccumL remove arrs inners
  in (outer', inners')
  where remove arrs nest =
          let (discard, keep) = partition ((`nameIn` arrs) . snd) $ loopNestingParamsAndArrs nest
          in (namesFromList (map (paramName . fst) discard) <> arrs,
              nest { loopNestingParamsAndArrs = keep })

newKernel :: LoopNesting -> KernelNest
newKernel nest = (nest, [])

kernelNestLoops :: KernelNest -> [LoopNesting]
kernelNestLoops (loop, loops) = loop : loops

boundInKernelNest :: KernelNest -> Names
boundInKernelNest = mconcat . boundInKernelNests

boundInKernelNests :: KernelNest -> [Names]
boundInKernelNests = map (namesFromList .
                          map (paramName . fst) .
                          loopNestingParamsAndArrs) .
                     kernelNestLoops

kernelNestWidths :: KernelNest -> [SubExp]
kernelNestWidths = map loopNestingWidth . kernelNestLoops

constructKernel :: (MonadFreshNames m, LocalScope Kernels m) =>
                   MkSegLevel m -> KernelNest -> Body Kernels
                -> m (Stm Kernels, Stms Kernels)
constructKernel mk_lvl kernel_nest inner_body = runBinderT' $ do
  (ispace, inps) <- flatKernel kernel_nest
  let cs = loopNestingCertificates first_nest
      ispace_scope = M.fromList $ zip (map fst ispace) $ repeat $ IndexInfo Int32
      pat = loopNestingPattern first_nest
      rts = map (stripArray (length ispace)) $ patternTypes pat

  inner_body' <- fmap (uncurry (flip (KernelBody ()))) $ runBinder $
                 localScope ispace_scope $ do
    mapM_ readKernelInput $ filter inputIsUsed inps
    map Returns <$> bodyBind inner_body

  (segop, aux_stms) <- lift $ mapKernel mk_lvl ispace [] rts inner_body'

  addStms aux_stms

  return $ Let pat (StmAux cs ()) $ Op $ SegOp segop
  where
    first_nest = fst kernel_nest
    inputIsUsed input = kernelInputName input `nameIn` freeIn inner_body

-- | Flatten a kernel nesting to:
--
--  (1) The index space.
--
--  (2) The kernel inputs - note that some of these may be unused.
flatKernel :: MonadFreshNames m =>
              KernelNest
           -> m ([(VName, SubExp)],
                 [KernelInput])
flatKernel (MapNesting _ _ nesting_w params_and_arrs, []) = do
  i <- newVName "gtid"
  let inps = [ KernelInput pname ptype arr [Var i] |
               (Param pname ptype, arr) <- params_and_arrs ]
  return ([(i,nesting_w)], inps)

flatKernel (MapNesting _ _ nesting_w params_and_arrs, nest : nests) = do
  i <- newVName "gtid"
  (ispace, inps) <- flatKernel (nest, nests)

  let inps' = map fixupInput inps
      isParam inp =
        snd <$> find ((==kernelInputArray inp) . paramName . fst) params_and_arrs
      fixupInput inp
        | Just arr <- isParam inp =
            inp { kernelInputArray = arr
                , kernelInputIndices = Var i : kernelInputIndices inp }
        | otherwise =
            inp

  return ((i, nesting_w) : ispace, extra_inps i <> inps')
  where extra_inps i =
          [ KernelInput pname ptype arr [Var i] |
            (Param pname ptype, arr) <- params_and_arrs ]

-- | Description of distribution to do.
data DistributionBody = DistributionBody {
    distributionTarget :: Targets
  , distributionFreeInBody :: Names
  , distributionIdentityMap :: M.Map VName Ident
  , distributionExpandTarget :: Target -> Target
    -- ^ Also related to avoiding identity mapping.
  }

distributionInnerPattern :: DistributionBody -> Pattern Kernels
distributionInnerPattern = fst . innerTarget . distributionTarget

distributionBodyFromStms :: Attributes lore =>
                            Targets -> Stms lore -> (DistributionBody, Result)
distributionBodyFromStms (Targets (inner_pat, inner_res) targets) stms =
  let bound_by_stms = namesFromList $ M.keys $ scopeOf stms
      (inner_pat', inner_res', inner_identity_map, inner_expand_target) =
        removeIdentityMappingGeneral bound_by_stms inner_pat inner_res
  in (DistributionBody
      { distributionTarget = Targets (inner_pat', inner_res') targets
      , distributionFreeInBody = fold (fmap freeIn stms) `namesSubtract` bound_by_stms
      , distributionIdentityMap = inner_identity_map
      , distributionExpandTarget = inner_expand_target
      },
      inner_res')

distributionBodyFromStm :: Attributes lore =>
                           Targets -> Stm lore -> (DistributionBody, Result)
distributionBodyFromStm targets bnd =
  distributionBodyFromStms targets $ oneStm bnd

createKernelNest :: (MonadFreshNames m, HasScope t m) =>
                    Nestings
                 -> DistributionBody
                 -> m (Maybe (Targets, KernelNest))
createKernelNest (inner_nest, nests) distrib_body = do
  let Targets target targets = distributionTarget distrib_body
  unless (length nests == length targets) $
    error $ "Nests and targets do not match!\n" ++
    "nests: " ++ ppNestings (inner_nest, nests) ++
    "\ntargets:" ++ ppTargets (Targets target targets)
  runMaybeT $ fmap prepare $ recurse $ zip nests targets

  where prepare (x, _, z) = (z, x)
        bound_in_nest =
          mconcat $ map boundInNesting $ inner_nest : nests
        -- | Can something of this type be taken outside the nest?
        -- I.e. are none of its dimensions bound inside the nest.
        distributableType =
          (==mempty) . namesIntersection bound_in_nest . freeIn . arrayDims

        distributeAtNesting :: (HasScope t m, MonadFreshNames m) =>
                               Nesting
                            -> Pattern Kernels
                            -> (LoopNesting -> KernelNest, Names)
                            -> M.Map VName Ident
                            -> [Ident]
                            -> (Target -> Targets)
                            -> MaybeT m (KernelNest, Names, Targets)
        distributeAtNesting
          (Nesting nest_let_bound nest)
          pat
          (add_to_kernel, free_in_kernel)
          identity_map
          inner_returned_arrs
          addTarget = do
          let nest'@(MapNesting _ cs w params_and_arrs) =
                removeUnusedNestingParts free_in_kernel nest
              (params,arrs) = unzip params_and_arrs
              param_names = namesFromList $ map paramName params
              free_in_kernel' =
                (freeIn nest' <> free_in_kernel) `namesSubtract` param_names
              required_from_nest =
                free_in_kernel' `namesIntersection` nest_let_bound

          required_from_nest_idents <-
            forM (namesToList required_from_nest) $ \name -> do
              t <- lift $ lookupType name
              return $ Ident name t

          (free_params, free_arrs, bind_in_target) <-
            fmap unzip3 $
            forM (inner_returned_arrs++required_from_nest_idents) $
            \(Ident pname ptype) ->
              case M.lookup pname identity_map of
                Nothing -> do
                  arr <- newIdent (baseString pname ++ "_r") $
                         arrayOfRow ptype w
                  return (Param pname ptype,
                          arr,
                          True)
                Just arr ->
                  return (Param pname ptype,
                          arr,
                          False)

          let free_arrs_pat =
                basicPattern [] $ map snd $
                filter fst $ zip bind_in_target free_arrs
              free_params_pat =
                map snd $ filter fst $ zip bind_in_target free_params

              (actual_params, actual_arrs) =
                (params++free_params,
                 arrs++map identName free_arrs)
              actual_param_names =
                namesFromList $ map paramName actual_params

              nest'' =
                removeUnusedNestingParts free_in_kernel $
                MapNesting pat cs w $ zip actual_params actual_arrs

              free_in_kernel'' =
                (freeIn nest'' <> free_in_kernel) `namesSubtract` actual_param_names

          unless (all (distributableType . paramType) $
                  loopNestingParams nest'') $
            fail "Would induce irregular array"
          return (add_to_kernel nest'',

                  free_in_kernel'',

                  addTarget (free_arrs_pat, map (Var . paramName) free_params_pat))

        recurse :: (HasScope t m, MonadFreshNames m) =>
                   [(Nesting,Target)]
                -> MaybeT m (KernelNest, Names, Targets)
        recurse [] =
          distributeAtNesting
          inner_nest
          (distributionInnerPattern distrib_body)
          (newKernel,
           distributionFreeInBody distrib_body `namesIntersection` bound_in_nest)
          (distributionIdentityMap distrib_body)
          [] $
          singleTarget . distributionExpandTarget distrib_body

        recurse ((nest, (pat,res)) : nests') = do
          (kernel@(outer, _), kernel_free, kernel_targets) <- recurse nests'

          let (pat', res', identity_map, expand_target) =
                removeIdentityMappingFromNesting
                (namesFromList $ patternNames $ loopNestingPattern outer) pat res

          distributeAtNesting
            nest
            pat'
            (\k -> pushKernelNesting (pat',res') k kernel,
             kernel_free)
            identity_map
            (patternIdents $ fst $ outerTarget kernel_targets)
            ((`pushOuterTarget` kernel_targets) . expand_target)

removeUnusedNestingParts :: Names -> LoopNesting -> LoopNesting
removeUnusedNestingParts used (MapNesting pat cs w params_and_arrs) =
  MapNesting pat cs w $ zip used_params used_arrs
  where (params,arrs) = unzip params_and_arrs
        (used_params, used_arrs) =
          unzip $
          filter ((`nameIn` used) . paramName . fst) $
          zip params arrs

removeIdentityMappingGeneral :: Names -> Pattern Kernels -> Result
                             -> (Pattern Kernels,
                                 Result,
                                 M.Map VName Ident,
                                 Target -> Target)
removeIdentityMappingGeneral bound pat res =
  let (identities, not_identities) =
        mapEither isIdentity $ zip (patternElements pat) res
      (not_identity_patElems, not_identity_res) = unzip not_identities
      (identity_patElems, identity_res) = unzip identities
      expandTarget (tpat, tres) =
        (Pattern [] $ patternElements tpat ++ identity_patElems,
         tres ++ map Var identity_res)
      identity_map = M.fromList $ zip identity_res $
                      map patElemIdent identity_patElems
  in (Pattern [] not_identity_patElems,
      not_identity_res,
      identity_map,
      expandTarget)
  where isIdentity (patElem, Var v)
          | not (v `nameIn` bound) = Left (patElem, v)
        isIdentity x               = Right x

removeIdentityMappingFromNesting :: Names -> Pattern Kernels -> Result
                                 -> (Pattern Kernels,
                                     Result,
                                     M.Map VName Ident,
                                     Target -> Target)
removeIdentityMappingFromNesting bound_in_nesting pat res =
  let (pat', res', identity_map, expand_target) =
        removeIdentityMappingGeneral bound_in_nesting pat res
  in (pat', res', identity_map, expand_target)

tryDistribute :: (MonadFreshNames m, LocalScope Kernels m, MonadLogger m) =>
                 MkSegLevel m -> Nestings -> Targets -> Stms Kernels
              -> m (Maybe (Targets, Stms Kernels))
tryDistribute _ _ targets stms | null stms =
  -- No point in distributing an empty kernel.
  return $ Just (targets, mempty)
tryDistribute mk_lvl nest targets stms =
  createKernelNest nest dist_body >>=
  \case
    Just (targets', distributed) -> do
      (kernel_bnd, w_bnds) <-
        localScope (targetsScope targets') $
        constructKernel mk_lvl distributed $ mkBody stms inner_body_res
      distributed' <- renameStm kernel_bnd
      logMsg $ "distributing\n" ++
        unlines (map pretty $ stmsToList stms) ++
        pretty (snd $ innerTarget targets) ++
        "\nas\n" ++ pretty distributed' ++
        "\ndue to targets\n" ++ ppTargets targets ++
        "\nand with new targets\n" ++ ppTargets targets'
      return $ Just (targets', w_bnds <> oneStm distributed')
    Nothing ->
      return Nothing
  where (dist_body, inner_body_res) = distributionBodyFromStms targets stms

tryDistributeStm :: (MonadFreshNames m, HasScope t m, Attributes lore) =>
                    Nestings -> Targets -> Stm lore
                 -> m (Maybe (Result, Targets, KernelNest))
tryDistributeStm nest targets bnd =
  fmap addRes <$> createKernelNest nest dist_body
  where (dist_body, res) = distributionBodyFromStm targets bnd
        addRes (targets', kernel_nest) = (res, targets', kernel_nest)
