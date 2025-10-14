{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.ExtractKernels.Distribution
  ( Target,
    Targets,
    ppTargets,
    singleTarget,
    outerTarget,
    innerTarget,
    pushInnerTarget,
    popInnerTarget,
    targetsScope,
    LoopNesting (..),
    ppLoopNesting,
    scopeOfLoopNesting,
    Nesting (..),
    Nestings,
    ppNestings,
    letBindInInnerNesting,
    singleNesting,
    pushInnerNesting,
    KernelNest,
    ppKernelNest,
    newKernel,
    innermostKernelNesting,
    pushKernelNesting,
    pushInnerKernelNesting,
    scopeOfKernelNest,
    kernelNestLoops,
    kernelNestWidths,
    boundInKernelNest,
    boundInKernelNests,
    flatKernel,
    constructKernel,
    tryDistribute,
    tryDistributeStm,
  )
where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import Data.Bifunctor (second)
import Data.Foldable
import Data.List (elemIndex, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR
import Futhark.IR.SegOp
import Futhark.MonadFreshNames
import Futhark.Pass.ExtractKernels.BlockedKernel
  ( DistRep,
    KernelInput (..),
    MkSegLevel,
    mapKernel,
    readKernelInput,
  )
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util
import Futhark.Util.Log

type Target = (Pat Type, Result)

-- | First pair element is the very innermost ("current") target.  In
-- the list, the outermost target comes first.  Invariant: Every
-- element of a pattern must be present as the result of the
-- immediately enclosing target.  This is ensured by 'pushInnerTarget'
-- by removing unused pattern elements.
data Targets = Targets
  { _innerTarget :: Target,
    _outerTargets :: [Target]
  }

ppTargets :: Targets -> String
ppTargets (Targets target targets) =
  unlines $ map ppTarget $ targets ++ [target]
  where
    ppTarget (pat, res) = prettyString pat ++ " <- " ++ prettyString res

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
  where
    (pes', res') = unzip $ filter (used . fst) $ zip (patElems pat) res
    pat' = Pat pes'
    inner_used = freeIn $ snd inner_target
    used pe = patElemName pe `nameIn` inner_used

popInnerTarget :: Targets -> Maybe (Target, Targets)
popInnerTarget (Targets t ts) =
  case reverse ts of
    x : xs -> Just (t, Targets x $ reverse xs)
    [] -> Nothing

targetScope :: (DistRep rep) => Target -> Scope rep
targetScope = scopeOfPat . fst

targetsScope :: (DistRep rep) => Targets -> Scope rep
targetsScope (Targets t ts) = mconcat $ map targetScope $ t : ts

data LoopNesting = MapNesting
  { loopNestingPat :: Pat Type,
    loopNestingAux :: StmAux (),
    loopNestingWidth :: SubExp,
    loopNestingParamsAndArrs :: [(Param Type, VName)]
  }
  deriving (Show)

scopeOfLoopNesting :: (LParamInfo rep ~ Type) => LoopNesting -> Scope rep
scopeOfLoopNesting = scopeOfLParams . map fst . loopNestingParamsAndArrs

ppLoopNesting :: LoopNesting -> String
ppLoopNesting (MapNesting _ _ _ params_and_arrs) =
  prettyString (map fst params_and_arrs)
    ++ " <- "
    ++ prettyString (map snd params_and_arrs)

loopNestingParams :: LoopNesting -> [Param Type]
loopNestingParams = map fst . loopNestingParamsAndArrs

instance FreeIn LoopNesting where
  freeIn' (MapNesting pat aux w params_and_arrs) =
    freeIn' pat <> freeIn' aux <> freeIn' w <> freeIn' params_and_arrs

data Nesting = Nesting
  { nestingLetBound :: Names,
    nestingLoop :: LoopNesting
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
  where
    ppNesting (Nesting _ loop) = ppLoopNesting loop

singleNesting :: Nesting -> Nestings
singleNesting = (,[])

pushInnerNesting :: Nesting -> Nestings -> Nestings
pushInnerNesting nesting (inner_nesting, nestings) =
  (nesting, nestings ++ [inner_nesting])

-- | Both parameters and let-bound.
boundInNesting :: Nesting -> Names
boundInNesting nesting =
  namesFromList (map paramName (loopNestingParams loop))
    <> nestingLetBound nesting
  where
    loop = nestingLoop nesting

letBindInInnerNesting :: Names -> Nestings -> Nestings
letBindInInnerNesting names (nest, nestings) =
  (letBindInNesting names nest, nestings)

-- | Note: first element is *outermost* nesting.  This is different
-- from the similar types elsewhere!
type KernelNest = (LoopNesting, [LoopNesting])

ppKernelNest :: KernelNest -> String
ppKernelNest (nesting, nestings) =
  unlines $ map ppLoopNesting $ nesting : nestings

-- | Retrieve the innermost kernel nesting.
innermostKernelNesting :: KernelNest -> LoopNesting
innermostKernelNesting (nest, nests) =
  fromMaybe nest $ maybeHead $ reverse nests

-- | Add new outermost nesting, pushing the current outermost to the
-- list, also taking care to swap patterns if necessary.
pushKernelNesting :: Target -> LoopNesting -> KernelNest -> KernelNest
pushKernelNesting target newnest (nest, nests) =
  ( fixNestingPatOrder newnest target (loopNestingPat nest),
    nest : nests
  )

-- | Add new innermost nesting, pushing the current outermost to the
-- list.  It is important that the 'Target' has the right order
-- (non-permuted compared to what is expected by the outer nests).
pushInnerKernelNesting :: Target -> LoopNesting -> KernelNest -> KernelNest
pushInnerKernelNesting target newnest (nest, nests) =
  (nest, nests ++ [fixNestingPatOrder newnest target (loopNestingPat innermost)])
  where
    innermost = case reverse nests of
      [] -> nest
      n : _ -> n

fixNestingPatOrder :: LoopNesting -> Target -> Pat Type -> LoopNesting
fixNestingPatOrder nest (_, res) inner_pat =
  nest {loopNestingPat = basicPat pat'}
  where
    pat = loopNestingPat nest
    pat' = map fst fixed_target
    fixed_target = sortOn posInInnerPat $ zip (patIdents pat) res
    posInInnerPat (_, SubExpRes _ (Var v)) = fromMaybe 0 $ elemIndex v $ patNames inner_pat
    posInInnerPat _ = 0

newKernel :: LoopNesting -> KernelNest
newKernel nest = (nest, [])

kernelNestLoops :: KernelNest -> [LoopNesting]
kernelNestLoops (loop, loops) = loop : loops

scopeOfKernelNest :: (LParamInfo rep ~ Type) => KernelNest -> Scope rep
scopeOfKernelNest = foldMap scopeOfLoopNesting . kernelNestLoops

boundInKernelNest :: KernelNest -> Names
boundInKernelNest = mconcat . boundInKernelNests

boundInKernelNests :: KernelNest -> [Names]
boundInKernelNests =
  map (namesFromList . map (paramName . fst) . loopNestingParamsAndArrs)
    . kernelNestLoops

kernelNestWidths :: KernelNest -> [SubExp]
kernelNestWidths = map loopNestingWidth . kernelNestLoops

constructKernel ::
  (DistRep rep, MonadFreshNames m, LocalScope rep m) =>
  MkSegLevel rep m ->
  KernelNest ->
  Body rep ->
  m (Stm rep, Stms rep)
constructKernel mk_lvl kernel_nest inner_body = runBuilderT' $ do
  (ispace, inps) <- flatKernel kernel_nest
  let aux = loopNestingAux first_nest
      ispace_scope = M.fromList $ map ((,IndexName Int64) . fst) ispace
      pat = loopNestingPat first_nest
      rts = map (stripArray (length ispace)) $ patTypes pat

  inner_body' <- fmap (uncurry (flip (Body ()))) $
    runBuilder . localScope ispace_scope $ do
      mapM_ readKernelInput $ filter inputIsUsed inps
      res <- bodyBind inner_body
      forM res $ \(SubExpRes cs se) -> pure $ Returns ResultMaySimplify cs se

  (segop, aux_stms) <- lift $ mapKernel mk_lvl ispace [] rts inner_body'

  addStms aux_stms

  pure $ Let pat aux $ Op $ segOp segop
  where
    first_nest = fst kernel_nest
    inputIsUsed input = kernelInputName input `nameIn` freeIn inner_body

-- | Flatten a kernel nesting to:
--
--  (1) The index space.
--
--  (2) The kernel inputs - note that some of these may be unused.
flatKernel ::
  (MonadFreshNames m) =>
  KernelNest ->
  m ([(VName, SubExp)], [KernelInput])
flatKernel (MapNesting _ _ nesting_w params_and_arrs, []) = do
  i <- newVName "gtid"
  let inps =
        [ KernelInput pname ptype arr [Var i]
          | (Param _ pname ptype, arr) <- params_and_arrs
        ]
  pure ([(i, nesting_w)], inps)
flatKernel (MapNesting _ _ nesting_w params_and_arrs, nest : nests) = do
  i <- newVName "gtid"
  (ispace, inps) <- flatKernel (nest, nests)

  let inps' = map fixupInput inps
      isParam inp =
        snd <$> find ((== kernelInputArray inp) . paramName . fst) params_and_arrs
      fixupInput inp
        | Just arr <- isParam inp =
            inp
              { kernelInputArray = arr,
                kernelInputIndices = Var i : kernelInputIndices inp
              }
        | otherwise =
            inp

  pure ((i, nesting_w) : ispace, extra_inps i <> inps')
  where
    extra_inps i =
      [ KernelInput pname ptype arr [Var i]
        | (Param _ pname ptype, arr) <- params_and_arrs
      ]

-- | Description of distribution to do.
data DistributionBody = DistributionBody
  { distributionTarget :: Targets,
    distributionFreeInBody :: Names,
    distributionIdentityMap :: M.Map VName Ident,
    -- | Also related to avoiding identity mapping.
    distributionExpandTarget :: Target -> Target
  }

distributionInnerPat :: DistributionBody -> Pat Type
distributionInnerPat = fst . innerTarget . distributionTarget

distributionBodyFromStms ::
  (ASTRep rep) =>
  Targets ->
  Stms rep ->
  (DistributionBody, Result)
distributionBodyFromStms (Targets (inner_pat, inner_res) targets) stms =
  let bound_by_stms = namesFromList $ M.keys $ scopeOf stms
      (inner_pat', inner_res', inner_identity_map, inner_expand_target) =
        removeIdentityMappingGeneral bound_by_stms inner_pat inner_res
      free =
        (foldMap freeIn stms <> freeIn (map resCerts inner_res))
          `namesSubtract` bound_by_stms
   in ( DistributionBody
          { distributionTarget = Targets (inner_pat', inner_res') targets,
            distributionFreeInBody = free,
            distributionIdentityMap = inner_identity_map,
            distributionExpandTarget = inner_expand_target
          },
        inner_res'
      )

distributionBodyFromStm ::
  (ASTRep rep) =>
  Targets ->
  Stm rep ->
  (DistributionBody, Result)
distributionBodyFromStm targets stm =
  distributionBodyFromStms targets $ oneStm stm

createKernelNest ::
  forall rep m.
  (MonadFreshNames m, HasScope rep m) =>
  Nestings ->
  DistributionBody ->
  m (Maybe (Targets, KernelNest))
createKernelNest (inner_nest, nests) distrib_body = do
  let Targets target targets = distributionTarget distrib_body
  unless (length nests == length targets) $
    error $
      "Nests and targets do not match!\n"
        ++ "nests: "
        ++ ppNestings (inner_nest, nests)
        ++ "\ntargets:"
        ++ ppTargets (Targets target targets)
  runMaybeT $ fmap prepare $ recurse $ zip nests targets
  where
    prepare (x, _, z) = (z, x)
    bound_in_nest = mconcat $ map boundInNesting $ inner_nest : nests
    distributableType =
      (== mempty) . namesIntersection bound_in_nest . freeIn . arrayDims

    distributeAtNesting ::
      Nesting ->
      Pat Type ->
      (LoopNesting -> KernelNest, Names) ->
      M.Map VName Ident ->
      [Ident] ->
      (Target -> Targets) ->
      MaybeT m (KernelNest, Names, Targets)
    distributeAtNesting
      (Nesting nest_let_bound nest)
      pat
      (add_to_kernel, free_in_kernel)
      identity_map
      inner_returned_arrs
      addTarget = do
        let nest'@(MapNesting _ aux w params_and_arrs) =
              removeUnusedNestingParts free_in_kernel nest
            (params, arrs) = unzip params_and_arrs
            param_names = namesFromList $ map paramName params
            free_in_kernel' =
              (freeIn nest' <> free_in_kernel) `namesSubtract` param_names
            required_from_nest =
              free_in_kernel' `namesIntersection` nest_let_bound

        required_from_nest_idents <-
          forM (namesToList required_from_nest) $ \name -> do
            t <- lift $ lookupType name
            pure $ Ident name t

        (free_params, free_arrs, bind_in_target) <-
          fmap unzip3 $
            forM (inner_returned_arrs ++ required_from_nest_idents) $
              \(Ident pname ptype) ->
                case M.lookup pname identity_map of
                  Nothing -> do
                    arr <-
                      newIdent (baseName pname <> "_r") $ arrayOfRow ptype w
                    pure
                      ( Param mempty pname ptype,
                        arr,
                        True
                      )
                  Just arr ->
                    pure
                      ( Param mempty pname ptype,
                        arr,
                        False
                      )

        let free_arrs_pat =
              basicPat $ map snd $ filter fst $ zip bind_in_target free_arrs
            free_params_pat =
              map snd $ filter fst $ zip bind_in_target free_params

            (actual_params, actual_arrs) =
              ( params ++ free_params,
                arrs ++ map identName free_arrs
              )
            actual_param_names =
              namesFromList $ map paramName actual_params

            nest'' =
              removeUnusedNestingParts free_in_kernel $
                MapNesting pat aux w $
                  zip actual_params actual_arrs

            free_in_kernel'' =
              (freeIn nest'' <> free_in_kernel) `namesSubtract` actual_param_names

        unless
          ( all (distributableType . paramType) $
              loopNestingParams nest''
          )
          $ fail "Would induce irregular array"
        pure
          ( add_to_kernel nest'',
            free_in_kernel'',
            addTarget (free_arrs_pat, varsRes $ map paramName free_params_pat)
          )

    recurse :: [(Nesting, Target)] -> MaybeT m (KernelNest, Names, Targets)
    recurse [] =
      distributeAtNesting
        inner_nest
        (distributionInnerPat distrib_body)
        ( newKernel,
          distributionFreeInBody distrib_body `namesIntersection` bound_in_nest
        )
        (distributionIdentityMap distrib_body)
        []
        $ singleTarget . distributionExpandTarget distrib_body
    recurse ((nest, (pat, res)) : nests') = do
      (kernel@(outer, _), kernel_free, kernel_targets) <- recurse nests'

      let (pat', res', identity_map, expand_target) =
            removeIdentityMappingFromNesting
              (namesFromList $ patNames $ loopNestingPat outer)
              pat
              res

      distributeAtNesting
        nest
        pat'
        ( \k -> pushKernelNesting (pat', res') k kernel,
          kernel_free
        )
        identity_map
        (patIdents $ fst $ outerTarget kernel_targets)
        ((`pushOuterTarget` kernel_targets) . expand_target)

removeUnusedNestingParts :: Names -> LoopNesting -> LoopNesting
removeUnusedNestingParts used (MapNesting pat aux w params_and_arrs) =
  MapNesting pat aux w $ zip used_params used_arrs
  where
    (params, arrs) = unzip params_and_arrs
    (used_params, used_arrs) =
      unzip $ filter ((`nameIn` used) . paramName . fst) $ zip params arrs

removeIdentityMappingGeneral ::
  Names ->
  Pat Type ->
  Result ->
  ( Pat Type,
    Result,
    M.Map VName Ident,
    Target -> Target
  )
removeIdentityMappingGeneral bound pat res =
  let (identities, not_identities) =
        mapEither isIdentity $ zip (patElems pat) res
      (not_identity_patElems, not_identity_res) = unzip not_identities
      (identity_patElems, identity_res) = unzip identities
      expandTarget (tpat, tres) =
        ( Pat $ patElems tpat ++ identity_patElems,
          tres ++ map (uncurry SubExpRes . second Var) identity_res
        )
      identity_map =
        M.fromList $ zip (map snd identity_res) $ map patElemIdent identity_patElems
   in ( Pat not_identity_patElems,
        not_identity_res,
        identity_map,
        expandTarget
      )
  where
    isIdentity (patElem, SubExpRes _ (Var v))
      | v `notNameIn` bound = Left (patElem, (mempty, v))
    isIdentity x = Right x

removeIdentityMappingFromNesting ::
  Names ->
  Pat Type ->
  Result ->
  ( Pat Type,
    Result,
    M.Map VName Ident,
    Target -> Target
  )
removeIdentityMappingFromNesting bound_in_nesting pat res =
  let (pat', res', identity_map, expand_target) =
        removeIdentityMappingGeneral bound_in_nesting pat res
   in (pat', res', identity_map, expand_target)

tryDistribute ::
  ( DistRep rep,
    MonadFreshNames m,
    LocalScope rep m,
    MonadLogger m
  ) =>
  MkSegLevel rep m ->
  Nestings ->
  Targets ->
  Stms rep ->
  m (Maybe (Targets, Stms rep))
tryDistribute _ _ targets stms
  | null stms =
      -- No point in distributing an empty kernel.
      pure $ Just (targets, mempty)
tryDistribute mk_lvl nest targets stms =
  createKernelNest nest dist_body
    >>= \case
      Just (targets', distributed) -> do
        (kernel_stm, w_stms) <-
          localScope (targetsScope targets') $
            constructKernel mk_lvl distributed $
              mkBody stms inner_body_res
        distributed' <- renameStm kernel_stm
        logMsg $
          "distributing\n"
            ++ unlines (map prettyString $ stmsToList stms)
            ++ prettyString (snd $ innerTarget targets)
            ++ "\nas\n"
            ++ prettyString distributed'
            ++ "\ndue to targets\n"
            ++ ppTargets targets
            ++ "\nand with new targets\n"
            ++ ppTargets targets'
        pure $ Just (targets', w_stms <> oneStm distributed')
      Nothing ->
        pure Nothing
  where
    (dist_body, inner_body_res) = distributionBodyFromStms targets stms

tryDistributeStm ::
  (MonadFreshNames m, HasScope t m, ASTRep rep) =>
  Nestings ->
  Targets ->
  Stm rep ->
  m (Maybe (Result, Targets, KernelNest))
tryDistributeStm nest targets stm =
  fmap addRes <$> createKernelNest nest dist_body
  where
    (dist_body, res) = distributionBodyFromStm targets stm
    addRes (targets', kernel_nest) = (res, targets', kernel_nest)
