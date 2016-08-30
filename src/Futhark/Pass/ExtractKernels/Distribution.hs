{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.Distribution
       (
         Target
       , Targets
       , ppTargets
       , singleTarget
       , innerTarget
       , outerTarget
       , pushOuterTarget
       , pushInnerTarget

       , LoopNesting (..)
       , ppLoopNesting

       , Nesting (..)
       , Nestings
       , ppNestings
       , letBindInInnerNesting
       , singleNesting
       , pushInnerNesting

       , KernelNest
       , kernelNestLoops
       , kernelNestWidths
       , boundInKernelNest
       , boundInKernelNests
       , flatKernel

       , tryDistribute
       , tryDistributeBinding
       )
       where

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List
import Data.Ord

import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Util
import Futhark.Transform.Rename
import Futhark.Util.Log
import Futhark.Pass.ExtractKernels.BlockedKernel (mapKernel, KernelInput(..))

import Prelude

type Target = (Pattern Kernels, Result)

-- | First pair element is the very innermost ("current") target.  In
-- the list, the outermost target comes first.
type Targets = (Target, [Target])

ppTargets :: Targets -> String
ppTargets (target, targets) =
  unlines $ map ppTarget $ targets ++ [target]
  where ppTarget (pat, res) =
          pretty pat ++ " <- " ++ pretty res

singleTarget :: Target -> Targets
singleTarget = (,[])

innerTarget :: Targets -> Target
innerTarget = fst

outerTarget :: Targets -> Target
outerTarget (inner_target, []) = inner_target
outerTarget (_, outer_target : _) = outer_target

pushOuterTarget :: Target -> Targets -> Targets
pushOuterTarget target (inner_target, targets) =
  (inner_target, target : targets)

pushInnerTarget :: Target -> Targets -> Targets
pushInnerTarget target (inner_target, targets) =
  (target, targets ++ [inner_target])

data LoopNesting = MapNesting { loopNestingPattern :: Pattern Kernels
                              , loopNestingCertificates :: Certificates
                              , loopNestingWidth :: SubExp
                              , loopNestingParamsAndArrs :: [(Param Type, VName)]
                              }
                 deriving (Show)

ppLoopNesting :: LoopNesting -> String
ppLoopNesting (MapNesting _ _ _ params_and_arrs) =
  pretty (map fst params_and_arrs) ++
  " <- " ++
  pretty (map snd params_and_arrs)

loopNestingParams :: LoopNesting -> [LParam Kernels]
loopNestingParams  = map fst . loopNestingParamsAndArrs

instance FreeIn LoopNesting where
  freeIn (MapNesting pat cs w params_and_arrs) =
    freeIn pat <>
    freeIn cs <>
    freeIn w <>
    freeIn params_and_arrs

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
  HS.fromList (map paramName (loopNestingParams loop)) <>
  nestingLetBound nesting
  where loop = nestingLoop nesting

letBindInInnerNesting :: Names -> Nestings -> Nestings
letBindInInnerNesting names (nest, nestings) =
  (letBindInNesting names nest, nestings)


-- | Note: first element is *outermost* nesting.  This is different
-- from the similar types elsewhere!
type KernelNest = (LoopNesting, [LoopNesting])

-- | Add new outermost nesting, pushing the current outermost to the
-- list, also taking care to swap patterns if necessary.
pushKernelNesting :: Target -> LoopNesting -> KernelNest -> KernelNest
pushKernelNesting target newnest (nest, nests) =
  (fixNestingPatternOrder newnest target (loopNestingPattern nest),
   nest : nests)

fixNestingPatternOrder :: LoopNesting -> Target -> Pattern Kernels -> LoopNesting
fixNestingPatternOrder nest (_,res) inner_pat =
  nest { loopNestingPattern = basicPattern' [] pat' }
  where pat = loopNestingPattern nest
        pat' = map fst fixed_target
        fixed_target = sortBy (comparing posInInnerPat) $ zip (patternValueIdents pat) res
        posInInnerPat (_, Var v) = fromMaybe 0 $ elemIndex v $ patternNames inner_pat
        posInInnerPat _          = 0

newKernel :: LoopNesting -> KernelNest
newKernel nest = (nest, [])

kernelNestLoops :: KernelNest -> [LoopNesting]
kernelNestLoops (loop, loops) = loop : loops

boundInKernelNest :: KernelNest -> Names
boundInKernelNest = mconcat . boundInKernelNests

boundInKernelNests :: KernelNest -> [Names]
boundInKernelNests = map (HS.fromList .
                          map (paramName . fst) .
                          loopNestingParamsAndArrs) .
                     kernelNestLoops

kernelNestWidths :: KernelNest -> [SubExp]
kernelNestWidths = map loopNestingWidth . kernelNestLoops

constructKernel :: (MonadFreshNames m, HasScope Kernels m) =>
                   KernelNest -> KernelBody InKernel
                -> m ([Binding Kernels], Binding Kernels)
constructKernel kernel_nest inner_body = do
  (w_bnds, w, ispace, inps, rts) <- flatKernel kernel_nest
  let used_inps = filter inputIsUsed inps
      cs = loopNestingCertificates first_nest

  (ksize_bnds, k) <- mapKernel cs w ispace used_inps rts inner_body

  let kbnds = w_bnds ++ ksize_bnds
  return (kbnds,
          Let (loopNestingPattern first_nest) () $ Op k)
  where
    first_nest = fst kernel_nest
    inputIsUsed input = kernelInputName input `HS.member`
                        freeIn inner_body

-- | Flatten a kernel nesting to:
--
--  (0) Ancillary prologue bindings.
--
--  (1) The total number of threads, equal to the product of all
--  nesting widths, and equal to the product of the index space.
--
--  (2) The index space.
--
--  (3) The kernel inputs - not that some of these may be unused.
--
--  (4) The per-thread return type.
flatKernel :: MonadFreshNames m =>
              KernelNest
           -> m ([Binding Kernels],
                 SubExp,
                 [(VName, SubExp)],
                 [KernelInput],
                 [Type])
flatKernel (MapNesting pat _ nesting_w params_and_arrs, []) = do
  i <- newVName "gtid"
  let inps = [ KernelInput pname ptype arr [Var i] |
               (Param pname ptype, arr) <- params_and_arrs ]
  return ([], nesting_w, [(i,nesting_w)], inps,
          map rowType $ patternTypes pat)

flatKernel (MapNesting _ _ nesting_w params_and_arrs, nest : nests) = do
  i <- newVName "gtid"
  (w_bnds, w, ispace, inps, returns) <- flatKernel (nest, nests)

  w' <- newVName "nesting_size"
  let w_bnd = mkLet' [] [Ident w' $ Prim int32] $
              BasicOp $ BinOp (Mul Int32) w nesting_w

  let inps' = map fixupInput inps
      isParam inp =
        snd <$> find ((==kernelInputArray inp) . paramName . fst) params_and_arrs
      fixupInput inp
        | Just arr <- isParam inp =
            inp { kernelInputArray = arr
                , kernelInputIndices = Var i : kernelInputIndices inp }
        | otherwise =
            inp

  return (w_bnds++[w_bnd], Var w', (i, nesting_w) : ispace, extra_inps i <> inps', returns)
  where extra_inps i =
          [ KernelInput pname ptype arr [Var i] |
            (Param pname ptype, arr) <- params_and_arrs ]

-- | Description of distribution to do.
data DistributionBody = DistributionBody {
    distributionTarget :: Targets
  , distributionFreeInBody :: Names
  , distributionIdentityMap :: HM.HashMap VName Ident
  , distributionExpandTarget :: Target -> Target
    -- ^ Also related to avoiding identity mapping.
  }

distributionInnerPattern :: DistributionBody -> Pattern Kernels
distributionInnerPattern = fst . innerTarget . distributionTarget

distributionBodyFromBindings :: (Attributes lore, CanBeAliased (Op lore)) =>
                                Targets -> [Binding lore] -> (DistributionBody, Result)
distributionBodyFromBindings ((inner_pat, inner_res), targets) stms =
  let bound_by_stms = HS.fromList $ HM.keys $ scopeOf stms
      (inner_pat', inner_res', inner_identity_map, inner_expand_target) =
        removeIdentityMappingGeneral bound_by_stms inner_pat inner_res
  in (DistributionBody
      { distributionTarget = ((inner_pat', inner_res'), targets)
      , distributionFreeInBody = mconcat (map freeInBinding stms)
                                 `HS.difference` bound_by_stms
      , distributionIdentityMap = inner_identity_map
      , distributionExpandTarget = inner_expand_target
      },
      inner_res')

distributionBodyFromBinding :: (Attributes lore, CanBeAliased (Op lore)) =>
                               Targets -> Binding lore -> (DistributionBody, Result)
distributionBodyFromBinding targets bnd =
  distributionBodyFromBindings targets [bnd]

createKernelNest :: (MonadFreshNames m, HasScope t m) =>
                    Nestings
                 -> DistributionBody
                 -> m (Maybe (Targets, KernelNest))
createKernelNest (inner_nest, nests) distrib_body = do
  let (target, targets) = distributionTarget distrib_body
  unless (length nests == length targets) $
    fail $ "Nests and targets do not match!\n" ++
    "nests: " ++ ppNestings (inner_nest, nests) ++
    "\ntargets:" ++ ppTargets (target, targets)
  runMaybeT $ fmap prepare $ recurse $ zip nests targets

  where prepare (x, _, z) = (z, x)
        bound_in_nest =
          mconcat $ map boundInNesting $ inner_nest : nests
        -- | Can something of this type be taken outside the nest?
        -- I.e. are none of its dimensions bound inside the nest.
        distributableType =
          HS.null . HS.intersection bound_in_nest . freeIn . arrayDims

        distributeAtNesting :: (HasScope t m, MonadFreshNames m) =>
                               Nesting
                            -> Pattern Kernels
                            -> (LoopNesting -> KernelNest, Names)
                            -> HM.HashMap VName Ident
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
              param_names = HS.fromList $ map paramName params
              free_in_kernel' =
                (freeIn nest' <> free_in_kernel) `HS.difference` param_names
              required_from_nest =
                free_in_kernel' `HS.intersection` nest_let_bound

          required_from_nest_idents <-
            forM (HS.toList required_from_nest) $ \name -> do
              t <- lift $ lookupType name
              return $ Ident name t

          (free_params, free_arrs, bind_in_target) <-
            fmap unzip3 $
            forM (inner_returned_arrs++required_from_nest_idents) $
            \(Ident pname ptype) ->
              case HM.lookup pname identity_map of
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
                basicPattern [] $ map ((,BindVar) . snd) $
                filter fst $ zip bind_in_target free_arrs
              free_params_pat =
                map snd $ filter fst $ zip bind_in_target free_params

              (actual_params, actual_arrs) =
                (params++free_params,
                 arrs++map identName free_arrs)
              actual_param_names =
                HS.fromList $ map paramName actual_params

              nest'' =
                removeUnusedNestingParts free_in_kernel $
                MapNesting pat cs w $ zip actual_params actual_arrs

              free_in_kernel'' =
                (freeIn nest'' <> free_in_kernel) `HS.difference` actual_param_names

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
           distributionFreeInBody distrib_body `HS.intersection` bound_in_nest)
          (distributionIdentityMap distrib_body)
          [] $
          singleTarget . distributionExpandTarget distrib_body

        recurse ((nest, (pat,res)) : nests') = do
          (kernel@(outer, _), kernel_free, kernel_targets) <- recurse nests'

          let (pat', res', identity_map, expand_target) =
                removeIdentityMappingFromNesting
                (HS.fromList $ patternNames $ loopNestingPattern outer) pat res

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
          filter ((`HS.member` used) . paramName . fst) $
          zip params arrs

removeIdentityMappingGeneral :: Names -> Pattern Kernels -> Result
                             -> (Pattern Kernels,
                                 Result,
                                 HM.HashMap VName Ident,
                                 Target -> Target)
removeIdentityMappingGeneral bound pat res =
  let (identities, not_identities) =
        mapEither isIdentity $ zip (patternElements pat) res
      (not_identity_patElems, not_identity_res) = unzip not_identities
      (identity_patElems, identity_res) = unzip identities
      expandTarget (tpat, tres) =
        (Pattern [] $ patternElements tpat ++ identity_patElems,
         tres ++ map Var identity_res)
      identity_map = HM.fromList $ zip identity_res $
                      map patElemIdent identity_patElems
  in (Pattern [] not_identity_patElems,
      not_identity_res,
      identity_map,
      expandTarget)
  where isIdentity (patElem, Var v)
          | not (v `HS.member` bound) = Left (patElem, v)
        isIdentity x                  = Right x

removeIdentityMappingFromNesting :: Names -> Pattern Kernels -> Result
                                 -> (Pattern Kernels,
                                     Result,
                                     HM.HashMap VName Ident,
                                     Target -> Target)
removeIdentityMappingFromNesting bound_in_nesting pat res =
  let (pat', res', identity_map, expand_target) =
        removeIdentityMappingGeneral bound_in_nesting pat res
  in (pat', res', identity_map, expand_target)

tryDistribute :: (MonadFreshNames m, LocalScope Kernels m, MonadLogger m) =>
                 Nestings -> Targets -> [Binding InKernel]
              -> m (Maybe (Targets, [Binding Kernels]))
tryDistribute _ targets [] =
  -- No point in distributing an empty kernel.
  return $ Just (targets, [])
tryDistribute nest targets stms =
  createKernelNest nest dist_body >>=
  \case
    Just (targets', distributed) -> do
      let targets_scope = mconcat $ map (scopeOf . fst) $ uncurry (:) targets'
      (w_bnds, kernel_bnd) <- localScope targets_scope $
        constructKernel distributed inner_body
      distributed' <- renameBinding kernel_bnd
      logMsg $ "distributing\n" ++
        unlines (map pretty stms) ++
        pretty (snd $ innerTarget targets) ++
        "\nas\n" ++ pretty distributed' ++
        "\ndue to targets\n" ++ ppTargets targets ++
        "\nand with new targets\n" ++ ppTargets targets'
      return $ Just (targets', w_bnds ++ [distributed'])
    Nothing ->
      return Nothing
  where (dist_body, inner_body_res) = distributionBodyFromBindings targets stms
        inner_body = KernelBody () stms $
                     map (ThreadsReturn ThreadsInSpace) inner_body_res

tryDistributeBinding :: (MonadFreshNames m, HasScope t m,
                         Attributes lore, CanBeAliased (Op lore)) =>
                        Nestings -> Targets -> Binding lore
                     -> m (Maybe (Result, Targets, KernelNest))
tryDistributeBinding nest targets bnd =
  fmap addRes <$> createKernelNest nest dist_body
  where (dist_body, res) = distributionBodyFromBinding targets bnd
        addRes (targets', kernel_nest) = (res, targets', kernel_nest)
