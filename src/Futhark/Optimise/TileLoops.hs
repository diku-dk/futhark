{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Perform a restricted form of loop tiling within SegMaps.  We only
-- tile primitive types, to avoid excessive local memory use.
module Futhark.Optimise.TileLoops (tileLoops) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Futhark.Analysis.Alias as Alias
import Futhark.IR.GPU
import Futhark.IR.Prop.Aliases (consumedInStm)
import Futhark.MonadFreshNames
import Futhark.Optimise.BlkRegTiling
import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename
import Prelude hiding (quot)

-- | The pass definition.
tileLoops :: Pass GPU GPU
tileLoops =
  Pass "tile loops" "Tile stream loops inside kernels" $
    intraproceduralTransformation onStms
  where
    onStms scope stms =
      modifyNameSource $
        runState $
          runReaderT (optimiseStms stms) scope

optimiseBody :: Body GPU -> TileM (Body GPU)
optimiseBody (Body () stms res) =
  Body () <$> optimiseStms stms <*> pure res

optimiseStms :: Stms GPU -> TileM (Stms GPU)
optimiseStms stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM optimiseStm (stmsToList stms)

optimiseStm :: Stm GPU -> TileM (Stms GPU)
optimiseStm stm@(Let pat aux (Op (SegOp (SegMap lvl@SegThread {} space ts kbody)))) = do
  res3dtiling <- doRegTiling3D stm
  case res3dtiling of
    Just (extra_stms, stmt') -> return (extra_stms <> oneStm stmt')
    Nothing -> do
      blkRegTiling_res <- mmBlkRegTiling stm
      case blkRegTiling_res of
        Just (extra_stms, stmt') -> return (extra_stms <> oneStm stmt')
        Nothing -> localScope (scopeOfSegSpace space) $ do
          (host_stms, (lvl', space', kbody')) <- tileInKernelBody mempty initial_variance lvl space ts kbody
          return $ host_stms <> oneStm (Let pat aux $ Op $ SegOp $ SegMap lvl' space' ts kbody')
  where
    initial_variance = M.map mempty $ scopeOfSegSpace space
optimiseStm (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where
    optimise = identityMapper {mapOnBody = \scope -> localScope scope . optimiseBody}

tileInKernelBody ::
  Names ->
  VarianceTable ->
  SegLevel ->
  SegSpace ->
  [Type] ->
  KernelBody GPU ->
  TileM (Stms GPU, (SegLevel, SegSpace, KernelBody GPU))
tileInKernelBody branch_variant initial_variance lvl initial_kspace ts kbody
  | Just kbody_res <- mapM isSimpleResult $ kernelBodyResult kbody = do
    maybe_tiled <-
      tileInBody branch_variant initial_variance lvl initial_kspace ts $
        Body () (kernelBodyStms kbody) kbody_res
    case maybe_tiled of
      Just (host_stms, tiling, tiledBody) -> do
        (res', stms') <-
          runBuilder $ mapM (tilingTileReturns tiling) =<< tiledBody mempty mempty
        return
          ( host_stms,
            ( tilingLevel tiling,
              tilingSpace tiling,
              KernelBody () stms' res'
            )
          )
      Nothing ->
        return (mempty, (lvl, initial_kspace, kbody))
  | otherwise =
    return (mempty, (lvl, initial_kspace, kbody))
  where
    isSimpleResult (Returns _ cs se) = Just $ SubExpRes cs se
    isSimpleResult _ = Nothing

tileInBody ::
  Names ->
  VarianceTable ->
  SegLevel ->
  SegSpace ->
  [Type] ->
  Body GPU ->
  TileM (Maybe (Stms GPU, Tiling, TiledBody))
tileInBody branch_variant initial_variance initial_lvl initial_space res_ts (Body () initial_kstms stms_res) =
  descend mempty $ stmsToList initial_kstms
  where
    variance = varianceInStms initial_variance initial_kstms

    descend _ [] =
      return Nothing
    descend prestms (stm_to_tile : poststms)
      -- 2D tiling of redomap.
      | (gtids, kdims) <- unzip $ unSegSpace initial_space,
        Just (w, arrs, form) <- tileable stm_to_tile,
        Just inputs <-
          mapM (invariantToOneOfTwoInnerDims branch_variant variance gtids) arrs,
        not $ null $ tiledInputs inputs,
        gtid_y : gtid_x : top_gtids_rev <- reverse gtids,
        kdim_y : kdim_x : top_kdims_rev <- reverse kdims,
        (prestms', poststms') <-
          preludeToPostlude variance prestms stm_to_tile (stmsFromList poststms),
        used <- freeIn stm_to_tile <> freeIn poststms' <> freeIn stms_res =
        Just . injectPrelude initial_space variance prestms' used
          <$> tileGeneric
            (tiling2d $ reverse $ zip top_gtids_rev top_kdims_rev)
            initial_lvl
            res_ts
            (stmPat stm_to_tile)
            (gtid_x, gtid_y)
            (kdim_x, kdim_y)
            w
            form
            inputs
            poststms'
            stms_res
      -- 1D tiling of redomap.
      | (gtid, kdim) : top_space_rev <- reverse $ unSegSpace initial_space,
        Just (w, arrs, form) <- tileable stm_to_tile,
        inputs <- map (is1DTileable gtid variance) arrs,
        not $ null $ tiledInputs inputs,
        not $ gtid `nameIn` branch_variant,
        (prestms', poststms') <-
          preludeToPostlude variance prestms stm_to_tile (stmsFromList poststms),
        used <- freeIn stm_to_tile <> freeIn poststms' <> freeIn stms_res =
        Just . injectPrelude initial_space variance prestms' used
          <$> tileGeneric
            (tiling1d $ reverse top_space_rev)
            initial_lvl
            res_ts
            (stmPat stm_to_tile)
            gtid
            kdim
            w
            form
            inputs
            poststms'
            stms_res
      -- Tiling inside for-loop.
      | DoLoop merge (ForLoop i it bound []) loopbody <- stmExp stm_to_tile,
        not $ any ((`nameIn` freeIn merge) . paramName . fst) merge,
        (prestms', poststms') <-
          preludeToPostlude variance prestms stm_to_tile (stmsFromList poststms) = do
        let branch_variant' =
              branch_variant
                <> mconcat
                  ( map
                      (flip (M.findWithDefault mempty) variance)
                      (namesToList (freeIn bound))
                  )
            merge_params = map fst merge

        maybe_tiled <-
          localScope (M.insert i (IndexName it) $ scopeOfFParams merge_params) $
            tileInBody
              branch_variant'
              variance
              initial_lvl
              initial_space
              (map paramType merge_params)
              $ mkBody (bodyStms loopbody) (bodyResult loopbody)

        case maybe_tiled of
          Nothing -> next
          Just tiled ->
            Just
              <$> tileDoLoop
                initial_space
                variance
                prestms'
                (freeIn loopbody <> freeIn merge)
                tiled
                res_ts
                (stmPat stm_to_tile)
                (stmAux stm_to_tile)
                merge
                i
                it
                bound
                poststms'
                stms_res
      | otherwise = next
      where
        next =
          localScope (scopeOf stm_to_tile) $
            descend (prestms <> oneStm stm_to_tile) poststms

-- | Move statements from prelude to postlude if they are not used in
-- the tiled statement anyway.
preludeToPostlude ::
  VarianceTable ->
  Stms GPU ->
  Stm GPU ->
  Stms GPU ->
  (Stms GPU, Stms GPU)
preludeToPostlude variance prelude stm_to_tile postlude =
  (prelude_used, prelude_not_used <> postlude)
  where
    used_in_tiled = freeIn stm_to_tile

    used_in_stm_variant =
      (used_in_tiled <>) $
        mconcat $
          map (flip (M.findWithDefault mempty) variance) $
            namesToList used_in_tiled

    used stm =
      any (`nameIn` used_in_stm_variant) $
        patNames $ stmPat stm

    (prelude_used, prelude_not_used) =
      Seq.partition used prelude

-- | Partition prelude statements preceding a tiled loop (or something
-- containing a tiled loop) into three categories:
--
-- 1) Group-level statements that are invariant to the threads in the group.
--
-- 2) Thread-variant statements that should be computed once with a segmap_thread_scalar.
--
-- 3) Thread-variant statements that should be recomputed whenever
-- they are needed.
--
-- The third category duplicates computation, so we only want to do it
-- when absolutely necessary.  Currently, this is necessary for
-- results that are views of an array (slicing, rotate, etc) and which
-- results are used after the prelude, because these cannot be
-- efficiently represented by a scalar segmap (they'll be manifested
-- in memory).
partitionPrelude ::
  VarianceTable ->
  Stms GPU ->
  Names ->
  Names ->
  (Stms GPU, Stms GPU, Stms GPU)
partitionPrelude variance prestms private used_after =
  (invariant_prestms, precomputed_variant_prestms, recomputed_variant_prestms)
  where
    invariantTo names stm =
      case patNames (stmPat stm) of
        [] -> True -- Does not matter.
        v : _ -> not $ any (`nameIn` names) $ namesToList $ M.findWithDefault mempty v variance

    consumed v = v `nameIn` consumed_in_prestms
    consumedStm stm = any consumed (patNames (stmPat stm))

    later_consumed =
      namesFromList $
        concatMap (patNames . stmPat) $
          stmsToList $ Seq.filter consumedStm prestms

    groupInvariant stm =
      invariantTo private stm
        && not (any (`nameIn` later_consumed) (patNames (stmPat stm)))
        && invariantTo later_consumed stm
    (invariant_prestms, variant_prestms) =
      Seq.partition groupInvariant prestms

    consumed_in_prestms =
      foldMap consumedInStm $ fst $ Alias.analyseStms mempty prestms

    mustBeInlinedExp (BasicOp (Index _ slice)) = not $ null $ sliceDims slice
    mustBeInlinedExp (BasicOp Iota {}) = True
    mustBeInlinedExp (BasicOp Rotate {}) = True
    mustBeInlinedExp (BasicOp Rearrange {}) = True
    mustBeInlinedExp (BasicOp Reshape {}) = True
    mustBeInlinedExp _ = False
    mustBeInlined stm =
      mustBeInlinedExp (stmExp stm)
        && any (`nameIn` used_after) (patNames (stmPat stm))

    must_be_inlined =
      namesFromList $
        concatMap (patNames . stmPat) $
          stmsToList $ Seq.filter mustBeInlined variant_prestms
    recompute stm =
      any (`nameIn` must_be_inlined) (patNames (stmPat stm))
        || not (invariantTo must_be_inlined stm)
    (recomputed_variant_prestms, precomputed_variant_prestms) =
      Seq.partition recompute variant_prestms

-- Anything that is variant to the "private" names should be
-- considered thread-local.
injectPrelude ::
  SegSpace ->
  VarianceTable ->
  Stms GPU ->
  Names ->
  (Stms GPU, Tiling, TiledBody) ->
  (Stms GPU, Tiling, TiledBody)
injectPrelude initial_space variance prestms used (host_stms, tiling, tiledBody) =
  (host_stms, tiling, tiledBody')
  where
    tiledBody' private privstms = do
      let nontiled = (`notElem` unSegSpace (tilingSpace tiling))
          private' =
            private
              <> namesFromList (map fst (filter nontiled $ unSegSpace initial_space))
          ( invariant_prestms,
            precomputed_variant_prestms,
            recomputed_variant_prestms
            ) =
              partitionPrelude variance prestms private' used

      addStms invariant_prestms

      let live_set =
            namesToList $
              liveSet precomputed_variant_prestms $
                used <> freeIn recomputed_variant_prestms
      prelude_arrs <-
        inScopeOf precomputed_variant_prestms $
          doPrelude tiling privstms precomputed_variant_prestms live_set

      let prelude_privstms =
            PrivStms recomputed_variant_prestms $
              mkReadPreludeValues prelude_arrs live_set

      tiledBody private' (prelude_privstms <> privstms)

tileDoLoop ::
  SegSpace ->
  VarianceTable ->
  Stms GPU ->
  Names ->
  (Stms GPU, Tiling, TiledBody) ->
  [Type] ->
  Pat GPU ->
  StmAux (ExpDec GPU) ->
  [(FParam GPU, SubExp)] ->
  VName ->
  IntType ->
  SubExp ->
  Stms GPU ->
  Result ->
  TileM (Stms GPU, Tiling, TiledBody)
tileDoLoop initial_space variance prestms used_in_body (host_stms, tiling, tiledBody) res_ts pat aux merge i it bound poststms poststms_res = do
  let prestms_used = used_in_body <> freeIn poststms <> freeIn poststms_res
      ( invariant_prestms,
        precomputed_variant_prestms,
        recomputed_variant_prestms
        ) =
          partitionPrelude variance prestms tiled_kdims prestms_used

  let (mergeparams, mergeinits) = unzip merge

      -- Expand the loop merge parameters to be arrays.
      tileDim t = arrayOf t (tilingTileShape tiling) $ uniqueness t

      merge_scope = M.insert i (IndexName it) $ scopeOfFParams mergeparams

      tiledBody' private privstms = localScope (scopeOf host_stms <> merge_scope) $ do
        addStms invariant_prestms

        let live_set =
              namesToList $
                liveSet precomputed_variant_prestms $
                  freeIn recomputed_variant_prestms <> prestms_used

        prelude_arrs <-
          inScopeOf precomputed_variant_prestms $
            doPrelude tiling privstms precomputed_variant_prestms live_set

        mergeparams' <- forM mergeparams $ \(Param attrs pname pt) ->
          Param attrs <$> newVName (baseString pname ++ "_group") <*> pure (tileDim pt)

        let merge_ts = map paramType mergeparams

        let inloop_privstms =
              PrivStms recomputed_variant_prestms $
                mkReadPreludeValues prelude_arrs live_set

        mergeinit' <-
          fmap (map Var) $
            certifying (stmAuxCerts aux) $
              tilingSegMap tiling "tiled_loopinit" (scalarLevel tiling) ResultPrivate $
                \in_bounds slice ->
                  fmap varsRes $
                    protectOutOfBounds "loopinit" in_bounds merge_ts $ do
                      addPrivStms slice inloop_privstms
                      addPrivStms slice privstms
                      return $ subExpsRes mergeinits

        let merge' = zip mergeparams' mergeinit'

        let indexMergeParams slice =
              localScope (scopeOfFParams mergeparams') $
                forM_ (zip mergeparams mergeparams') $ \(to, from) ->
                  letBindNames [paramName to] . BasicOp . Index (paramName from) $
                    fullSlice (paramType from) slice

            private' =
              private <> namesFromList (map paramName mergeparams ++ map paramName mergeparams')

            privstms' =
              PrivStms mempty indexMergeParams <> privstms <> inloop_privstms

        loopbody' <-
          localScope (scopeOfFParams mergeparams') . runBodyBuilder $
            resultBody . map Var
              <$> tiledBody private' privstms'
        accs' <-
          letTupExp "tiled_inside_loop" $
            DoLoop merge' (ForLoop i it bound []) loopbody'

        postludeGeneric tiling (privstms <> inloop_privstms) pat accs' poststms poststms_res res_ts

  return (host_stms, tiling, tiledBody')
  where
    tiled_kdims =
      namesFromList $
        map fst $
          filter (`notElem` unSegSpace (tilingSpace tiling)) $
            unSegSpace initial_space

doPrelude :: Tiling -> PrivStms -> Stms GPU -> [VName] -> Builder GPU [VName]
doPrelude tiling privstms prestms prestms_live =
  -- Create a SegMap that takes care of the prelude for every thread.
  tilingSegMap tiling "prelude" (scalarLevel tiling) ResultPrivate $
    \in_bounds slice -> do
      ts <- mapM lookupType prestms_live
      fmap varsRes . protectOutOfBounds "pre" in_bounds ts $ do
        addPrivStms slice privstms
        addStms prestms
        pure $ varsRes prestms_live

liveSet :: FreeIn a => Stms GPU -> a -> Names
liveSet stms after =
  namesFromList (concatMap (patNames . stmPat) stms)
    `namesIntersection` freeIn after

tileable ::
  Stm GPU ->
  Maybe
    ( SubExp,
      [VName],
      (Commutativity, Lambda GPU, [SubExp], Lambda GPU)
    )
tileable stm
  | Op (OtherOp (Screma w arrs form)) <- stmExp stm,
    Just (reds, map_lam) <- isRedomapSOAC form,
    Reduce red_comm red_lam red_nes <- singleReduce reds,
    lambdaReturnType map_lam == lambdaReturnType red_lam, -- No mapout arrays.
    not $ null arrs,
    all primType $ lambdaReturnType map_lam,
    all (primType . paramType) $ lambdaParams map_lam =
    Just (w, arrs, (red_comm, red_lam, red_nes, map_lam))
  | otherwise =
    Nothing

-- | We classify the inputs to the tiled loop as whether they are
-- tileable (and with what permutation of the kernel indexes) or not.
-- In practice, we should have at least one tileable array per loop,
-- but this is not enforced in our representation.
data InputArray
  = InputTile [Int] VName
  | InputDontTile VName

tiledInputs :: [InputArray] -> [(VName, [Int])]
tiledInputs = mapMaybe f
  where
    f (InputTile perm arr) = Just (arr, perm)
    f InputDontTile {} = Nothing

-- | A tile (or an original untiled array).
data InputTile
  = InputTiled [Int] VName
  | InputUntiled VName

-- First VNames are the tiles, second are the untiled.
inputsToTiles :: [InputArray] -> [VName] -> [InputTile]
inputsToTiles (InputTile perm _ : inputs) (tile : tiles) =
  InputTiled perm tile : inputsToTiles inputs tiles
inputsToTiles (InputDontTile arr : inputs) tiles =
  InputUntiled arr : inputsToTiles inputs tiles
inputsToTiles _ _ = []

-- The atual tile size may be smaller for the last tile, so we have to
-- be careful now.
sliceUntiled ::
  MonadBuilder m =>
  VName ->
  SubExp ->
  SubExp ->
  SubExp ->
  m VName
sliceUntiled arr tile_id full_tile_size this_tile_size = do
  arr_t <- lookupType arr
  slice_offset <-
    letSubExp "slice_offset" =<< toExp (pe64 tile_id * pe64 full_tile_size)
  let slice = DimSlice slice_offset this_tile_size (intConst Int64 1)
  letExp "untiled_slice" $
    BasicOp $ Index arr $ fullSlice arr_t [slice]

-- | Statements that we insert directly into every thread-private
-- SegMaps.  This is for things that cannot efficiently be computed
-- once in advance in the prelude SegMap, primarily (exclusively?)
-- array slicing operations.
data PrivStms = PrivStms (Stms GPU) ReadPrelude

privStms :: Stms GPU -> PrivStms
privStms stms = PrivStms stms $ const $ return ()

addPrivStms :: [DimIndex SubExp] -> PrivStms -> Builder GPU ()
addPrivStms local_slice (PrivStms stms readPrelude) = do
  readPrelude local_slice
  addStms stms

instance Semigroup PrivStms where
  PrivStms stms_x readPrelude_x <> PrivStms stms_y readPrelude_y =
    PrivStms stms_z readPrelude_z
    where
      stms_z = stms_x <> stms_y
      readPrelude_z slice = readPrelude_x slice >> readPrelude_y slice

instance Monoid PrivStms where
  mempty = privStms mempty

type ReadPrelude = [DimIndex SubExp] -> Builder GPU ()

data ProcessTileArgs = ProcessTileArgs
  { processPrivStms :: PrivStms,
    processComm :: Commutativity,
    processRedLam :: Lambda GPU,
    processMapLam :: Lambda GPU,
    processTiles :: [InputTile],
    processAcc :: [VName],
    processTileId :: SubExp
  }

data ResidualTileArgs = ResidualTileArgs
  { residualPrivStms :: PrivStms,
    residualComm :: Commutativity,
    residualRedLam :: Lambda GPU,
    residualMapLam :: Lambda GPU,
    residualInput :: [InputArray],
    residualAcc :: [VName],
    residualInputSize :: SubExp,
    residualNumWholeTiles :: SubExp
  }

-- | Information about a loop that has been tiled inside a kernel, as
-- well as the kinds of changes that we would then like to perform on
-- the kernel.
data Tiling = Tiling
  { tilingSegMap ::
      String ->
      SegLevel ->
      ResultManifest ->
      (PrimExp VName -> [DimIndex SubExp] -> Builder GPU Result) ->
      Builder GPU [VName],
    -- The boolean PrimExp indicates whether they are in-bounds.

    tilingReadTile ::
      TileKind ->
      PrivStms ->
      SubExp ->
      [InputArray] ->
      Builder GPU [InputTile],
    tilingProcessTile ::
      ProcessTileArgs ->
      Builder GPU [VName],
    tilingProcessResidualTile ::
      ResidualTileArgs ->
      Builder GPU [VName],
    tilingTileReturns :: VName -> Builder GPU KernelResult,
    tilingSpace :: SegSpace,
    tilingTileShape :: Shape,
    tilingLevel :: SegLevel,
    tilingNumWholeTiles :: Builder GPU SubExp
  }

type DoTiling gtids kdims =
  SegLevel -> gtids -> kdims -> SubExp -> Builder GPU Tiling

scalarLevel :: Tiling -> SegLevel
scalarLevel tiling =
  SegThread (segNumGroups lvl) (segGroupSize lvl) SegNoVirt
  where
    lvl = tilingLevel tiling

protectOutOfBounds ::
  String ->
  PrimExp VName ->
  [Type] ->
  Builder GPU Result ->
  Builder GPU [VName]
protectOutOfBounds desc in_bounds ts m = do
  -- This is more complicated than you might expect, because we need
  -- to be able to produce a blank accumulator, which eBlank cannot
  -- do.  By the linear type rules of accumulators, the body returns
  -- an accumulator of type 'acc_t', then a unique variable of type
  -- 'acc_t' must also be free in the body.  This means we can find it
  -- based just on the type.
  m_body <- insertStmsM $ mkBody mempty <$> m
  let m_body_free = namesToList $ freeIn m_body
  t_to_v <-
    filter (isAcc . fst)
      <$> (zip <$> mapM lookupType m_body_free <*> pure m_body_free)
  let blank t = maybe (eBlank t) (pure . BasicOp . SubExp . Var) $ lookup t t_to_v
  letTupExp desc =<< eIf (toExp in_bounds) (pure m_body) (eBody $ map blank ts)
  where
    isAcc Acc {} = True
    isAcc _ = False

postludeGeneric ::
  Tiling ->
  PrivStms ->
  Pat GPU ->
  [VName] ->
  Stms GPU ->
  Result ->
  [Type] ->
  Builder GPU [VName]
postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts =
  tilingSegMap tiling "thread_res" (scalarLevel tiling) ResultPrivate $ \in_bounds slice -> do
    -- Read our per-thread result from the tiled loop.
    forM_ (zip (patNames pat) accs') $ \(us, everyone) -> do
      everyone_t <- lookupType everyone
      letBindNames [us] $ BasicOp $ Index everyone $ fullSlice everyone_t slice

    if poststms == mempty
      then do
        -- The privstms may still be necessary for the result.
        addPrivStms slice privstms
        return poststms_res
      else fmap varsRes $
        protectOutOfBounds "postlude" in_bounds res_ts $ do
          addPrivStms slice privstms
          addStms poststms
          return poststms_res

type TiledBody = Names -> PrivStms -> Builder GPU [VName]

tileGeneric ::
  DoTiling gtids kdims ->
  SegLevel ->
  [Type] ->
  Pat GPU ->
  gtids ->
  kdims ->
  SubExp ->
  (Commutativity, Lambda GPU, [SubExp], Lambda GPU) ->
  [InputArray] ->
  Stms GPU ->
  Result ->
  TileM (Stms GPU, Tiling, TiledBody)
tileGeneric doTiling initial_lvl res_ts pat gtids kdims w form inputs poststms poststms_res = do
  (tiling, tiling_stms) <- runBuilder $ doTiling initial_lvl gtids kdims w

  return (tiling_stms, tiling, tiledBody tiling)
  where
    (red_comm, red_lam, red_nes, map_lam) = form

    tiledBody :: Tiling -> Names -> PrivStms -> Builder GPU [VName]
    tiledBody tiling _private privstms = do
      let tile_shape = tilingTileShape tiling

      num_whole_tiles <- tilingNumWholeTiles tiling

      -- We don't use a Replicate here, because we want to enforce a
      -- scalar memory space.
      mergeinits <- tilingSegMap tiling "mergeinit" (scalarLevel tiling) ResultPrivate $ \in_bounds slice ->
        -- Constant neutral elements (a common case) do not need protection from OOB.
        if freeIn red_nes == mempty
          then return $ subExpsRes red_nes
          else fmap varsRes $
            protectOutOfBounds "neutral" in_bounds (lambdaReturnType red_lam) $ do
              addPrivStms slice privstms
              return $ subExpsRes red_nes

      merge <- forM (zip (lambdaParams red_lam) mergeinits) $ \(p, mergeinit) ->
        (,)
          <$> newParam
            (baseString (paramName p) ++ "_merge")
            (paramType p `arrayOfShape` tile_shape `toDecl` Unique)
          <*> pure (Var mergeinit)

      tile_id <- newVName "tile_id"
      let loopform = ForLoop tile_id Int64 num_whole_tiles []
      loopbody <- renameBody <=< runBodyBuilder $
        inScopeOf loopform $
          localScope (scopeOfFParams $ map fst merge) $ do
            -- Collectively read a tile.
            tile <- tilingReadTile tiling TilePartial privstms (Var tile_id) inputs

            -- Now each thread performs a traversal of the tile and
            -- updates its accumulator.
            let accs =
                  map (paramName . fst) merge
                tile_args =
                  ProcessTileArgs privstms red_comm red_lam map_lam tile accs (Var tile_id)
            resultBody . map Var <$> tilingProcessTile tiling tile_args

      accs <- letTupExp "accs" $ DoLoop merge loopform loopbody

      -- We possibly have to traverse a residual tile.
      red_lam' <- renameLambda red_lam
      map_lam' <- renameLambda map_lam
      let residual_args =
            ResidualTileArgs privstms red_comm red_lam' map_lam' inputs accs w num_whole_tiles
      accs' <- tilingProcessResidualTile tiling residual_args

      -- Create a SegMap that takes care of the postlude for every thread.
      postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts

mkReadPreludeValues :: [VName] -> [VName] -> ReadPrelude
mkReadPreludeValues prestms_live_arrs prestms_live slice =
  fmap mconcat $
    forM (zip prestms_live_arrs prestms_live) $ \(arr, v) -> do
      arr_t <- lookupType arr
      letBindNames [v] $ BasicOp $ Index arr $ fullSlice arr_t slice

tileReturns :: [(VName, SubExp)] -> [(SubExp, SubExp)] -> VName -> Builder GPU KernelResult
tileReturns dims_on_top dims arr = do
  let unit_dims = replicate (length dims_on_top) (intConst Int64 1)
  arr_t <- lookupType arr
  arr' <-
    if null dims_on_top || null (arrayDims arr_t) -- Second check is for accumulators.
      then return arr
      else do
        let new_shape = unit_dims ++ arrayDims arr_t
        letExp (baseString arr) $ BasicOp $ Reshape (map DimNew new_shape) arr
  let tile_dims = zip (map snd dims_on_top) unit_dims ++ dims
  return $ TileReturns mempty tile_dims arr'

is1DTileable :: VName -> M.Map VName Names -> VName -> InputArray
is1DTileable gtid variance arr
  | not $ nameIn gtid $ M.findWithDefault mempty arr variance =
    InputTile [0] arr
  | otherwise =
    InputDontTile arr

reconstructGtids1D ::
  Count GroupSize SubExp ->
  VName ->
  VName ->
  VName ->
  Builder GPU ()
reconstructGtids1D group_size gtid gid ltid =
  letBindNames [gtid]
    =<< toExp (le64 gid * pe64 (unCount group_size) + le64 ltid)

readTile1D ::
  SubExp ->
  VName ->
  VName ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  TileKind ->
  PrivStms ->
  SubExp ->
  [InputArray] ->
  Builder GPU [InputTile]
readTile1D tile_size gid gtid num_groups group_size kind privstms tile_id inputs =
  fmap (inputsToTiles inputs)
    . segMap1D "full_tile" lvl ResultNoSimplify
    $ \ltid -> do
      j <-
        letSubExp "j"
          =<< toExp (pe64 tile_id * pe64 tile_size + le64 ltid)

      reconstructGtids1D group_size gtid gid ltid
      addPrivStms [DimFix $ Var ltid] privstms

      let arrs = map fst $ tiledInputs inputs
      arr_ts <- mapM lookupType arrs
      let tile_ts = map rowType arr_ts
          w = arraysSize 0 arr_ts

      let readTileElem arr =
            -- No need for fullSlice because we are tiling only prims.
            letExp "tile_elem" (BasicOp $ Index arr $ Slice [DimFix j])
      fmap varsRes $
        case kind of
          TilePartial ->
            letTupExp "pre1d"
              =<< eIf
                (toExp $ pe64 j .<. pe64 w)
                (resultBody <$> mapM (fmap Var . readTileElem) arrs)
                (eBody $ map eBlank tile_ts)
          TileFull ->
            mapM readTileElem arrs
  where
    lvl = SegThread num_groups group_size SegNoVirt

processTile1D ::
  VName ->
  VName ->
  SubExp ->
  SubExp ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  ProcessTileArgs ->
  Builder GPU [VName]
processTile1D gid gtid kdim tile_size num_groups group_size tile_args = do
  let red_comm = processComm tile_args
      privstms = processPrivStms tile_args
      map_lam = processMapLam tile_args
      red_lam = processRedLam tile_args
      tiles = processTiles tile_args
      tile_id = processTileId tile_args
      accs = processAcc tile_args

  segMap1D "acc" lvl ResultPrivate $ \ltid -> do
    reconstructGtids1D group_size gtid gid ltid
    addPrivStms [DimFix $ Var ltid] privstms

    -- We replace the neutral elements with the accumulators (this is
    -- OK because the parallel semantics are not used after this
    -- point).
    thread_accs <- forM accs $ \acc ->
      letSubExp "acc" $ BasicOp $ Index acc $ Slice [DimFix $ Var ltid]
    let sliceTile (InputTiled _ arr) =
          pure arr
        sliceTile (InputUntiled arr) =
          sliceUntiled arr tile_id tile_size tile_size

    tiles' <- mapM sliceTile tiles

    let form' = redomapSOAC [Reduce red_comm red_lam thread_accs] map_lam
    fmap varsRes $
      letTupExp "acc"
        =<< eIf
          (toExp $ le64 gtid .<. pe64 kdim)
          (eBody [pure $ Op $ OtherOp $ Screma tile_size tiles' form'])
          (resultBodyM thread_accs)
  where
    lvl = SegThread num_groups group_size SegNoVirt

processResidualTile1D ::
  VName ->
  VName ->
  SubExp ->
  SubExp ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  ResidualTileArgs ->
  Builder GPU [VName]
processResidualTile1D gid gtid kdim tile_size num_groups group_size args = do
  -- The number of residual elements that are not covered by
  -- the whole tiles.
  residual_input <-
    letSubExp "residual_input" $
      BasicOp $ BinOp (SRem Int64 Unsafe) w tile_size

  letTupExp "acc_after_residual"
    =<< eIf
      (toExp $ pe64 residual_input .==. 0)
      (resultBodyM $ map Var accs)
      (nonemptyTile residual_input)
  where
    red_comm = residualComm args
    map_lam = residualMapLam args
    red_lam = residualRedLam args
    privstms = residualPrivStms args
    inputs = residualInput args
    accs = residualAcc args
    num_whole_tiles = residualNumWholeTiles args
    w = residualInputSize args

    nonemptyTile residual_input = runBodyBuilder $ do
      -- Collectively construct a tile.  Threads that are out-of-bounds
      -- provide a blank dummy value.
      full_tiles <-
        readTile1D
          tile_size
          gid
          gtid
          num_groups
          group_size
          TilePartial
          privstms
          num_whole_tiles
          inputs

      let sliceTile (InputUntiled arr) =
            pure $ InputUntiled arr
          sliceTile (InputTiled perm tile) = do
            let slice =
                  DimSlice (intConst Int64 0) residual_input (intConst Int64 1)
            InputTiled perm
              <$> letExp "partial_tile" (BasicOp $ Index tile $ Slice [slice])

      tiles <- mapM sliceTile full_tiles

      -- Now each thread performs a traversal of the tile and
      -- updates its accumulator.
      let tile_args =
            ProcessTileArgs privstms red_comm red_lam map_lam tiles accs num_whole_tiles
      resultBody . map Var
        <$> processTile1D gid gtid kdim residual_input num_groups group_size tile_args

tiling1d :: [(VName, SubExp)] -> DoTiling VName SubExp
tiling1d dims_on_top initial_lvl gtid kdim w = do
  gid <- newVName "gid"
  gid_flat <- newVName "gid_flat"

  (lvl, space) <-
    if null dims_on_top
      then
        return
          ( SegGroup (segNumGroups initial_lvl) (segGroupSize initial_lvl) $ segVirt initial_lvl,
            SegSpace gid_flat [(gid, unCount $ segNumGroups initial_lvl)]
          )
      else do
        group_size <-
          letSubExp "computed_group_size" $
            BasicOp $ BinOp (SMin Int64) (unCount (segGroupSize initial_lvl)) kdim

        -- How many groups we need to exhaust the innermost dimension.
        ldim <-
          letSubExp "ldim" $
            BasicOp $ BinOp (SDivUp Int64 Unsafe) kdim group_size

        num_groups <-
          letSubExp "computed_num_groups"
            =<< foldBinOp (Mul Int64 OverflowUndef) ldim (map snd dims_on_top)

        return
          ( SegGroup (Count num_groups) (Count group_size) SegNoVirt,
            SegSpace gid_flat $ dims_on_top ++ [(gid, ldim)]
          )
  let tile_size = unCount $ segGroupSize lvl

  return
    Tiling
      { tilingSegMap = \desc lvl' manifest f -> segMap1D desc lvl' manifest $ \ltid -> do
          letBindNames [gtid]
            =<< toExp (le64 gid * pe64 tile_size + le64 ltid)
          f (untyped $ le64 gtid .<. pe64 kdim) [DimFix $ Var ltid],
        tilingReadTile =
          readTile1D tile_size gid gtid (segNumGroups lvl) (segGroupSize lvl),
        tilingProcessTile =
          processTile1D gid gtid kdim tile_size (segNumGroups lvl) (segGroupSize lvl),
        tilingProcessResidualTile =
          processResidualTile1D gid gtid kdim tile_size (segNumGroups lvl) (segGroupSize lvl),
        tilingTileReturns = tileReturns dims_on_top [(kdim, tile_size)],
        tilingTileShape = Shape [tile_size],
        tilingNumWholeTiles =
          letSubExp "num_whole_tiles" $
            BasicOp $ BinOp (SQuot Int64 Unsafe) w tile_size,
        tilingLevel = lvl,
        tilingSpace = space
      }

invariantToOneOfTwoInnerDims ::
  Names ->
  M.Map VName Names ->
  [VName] ->
  VName ->
  Maybe InputArray
invariantToOneOfTwoInnerDims branch_variant variance dims arr = do
  j : i : _ <- Just $ reverse dims
  let variant_to = M.findWithDefault mempty arr variance
      branch_invariant = not $ nameIn j branch_variant || nameIn i branch_variant
  if branch_invariant && i `nameIn` variant_to && not (j `nameIn` variant_to)
    then Just $ InputTile [0, 1] arr
    else
      if branch_invariant && j `nameIn` variant_to && not (i `nameIn` variant_to)
        then Just $ InputTile [1, 0] arr
        else Just $ InputDontTile arr

-- Reconstruct the original gtids from group and local IDs.
reconstructGtids2D ::
  SubExp ->
  (VName, VName) ->
  (VName, VName) ->
  (VName, VName) ->
  Builder GPU ()
reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y) = do
  -- Reconstruct the original gtids from gid_x/gid_y and ltid_x/ltid_y.
  letBindNames [gtid_x]
    =<< toExp (le64 gid_x * pe64 tile_size + le64 ltid_x)
  letBindNames [gtid_y]
    =<< toExp (le64 gid_y * pe64 tile_size + le64 ltid_y)

readTile2D ::
  (SubExp, SubExp) ->
  (VName, VName) ->
  (VName, VName) ->
  SubExp ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  TileKind ->
  PrivStms ->
  SubExp ->
  [InputArray] ->
  Builder GPU [InputTile]
readTile2D (kdim_x, kdim_y) (gtid_x, gtid_y) (gid_x, gid_y) tile_size num_groups group_size kind privstms tile_id inputs =
  fmap (inputsToTiles inputs)
    . segMap2D
      "full_tile"
      (SegThread num_groups group_size (SegNoVirtFull (SegSeqDims [])))
      ResultNoSimplify
      (tile_size, tile_size)
    $ \(ltid_x, ltid_y) -> do
      i <-
        letSubExp "i"
          =<< toExp (pe64 tile_id * pe64 tile_size + le64 ltid_x)
      j <-
        letSubExp "j"
          =<< toExp (pe64 tile_id * pe64 tile_size + le64 ltid_y)

      reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)
      addPrivStms [DimFix $ Var ltid_x, DimFix $ Var ltid_y] privstms

      let arrs_and_perms = tiledInputs inputs

          readTileElem (arr, perm) =
            -- No need for fullSlice because we are tiling only prims.
            letExp
              "tile_elem"
              ( BasicOp . Index arr $
                  Slice [DimFix $ last $ rearrangeShape perm [i, j]]
              )

          readTileElemIfInBounds (arr, perm) = do
            arr_t <- lookupType arr
            let tile_t = rowType arr_t
                w = arraySize 0 arr_t
                idx = last $ rearrangeShape perm [i, j]
                othercheck =
                  last $
                    rearrangeShape
                      perm
                      [ le64 gtid_y .<. pe64 kdim_y,
                        le64 gtid_x .<. pe64 kdim_x
                      ]
            eIf
              (toExp $ pe64 idx .<. pe64 w .&&. othercheck)
              (eBody [return $ BasicOp $ Index arr $ Slice [DimFix idx]])
              (eBody [eBlank tile_t])

      fmap varsRes $
        case kind of
          TilePartial ->
            mapM (letExp "pre2d" <=< readTileElemIfInBounds) arrs_and_perms
          TileFull ->
            mapM readTileElem arrs_and_perms

findTileSize :: HasScope rep m => [InputTile] -> m SubExp
findTileSize tiles =
  case mapMaybe isTiled tiles of
    v : _ -> arraySize 0 <$> lookupType v
    [] -> pure $ intConst Int64 0
  where
    isTiled InputUntiled {} = Nothing
    isTiled (InputTiled _ tile) = Just tile

processTile2D ::
  (VName, VName) ->
  (VName, VName) ->
  (SubExp, SubExp) ->
  SubExp ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  ProcessTileArgs ->
  Builder GPU [VName]
processTile2D (gid_x, gid_y) (gtid_x, gtid_y) (kdim_x, kdim_y) tile_size num_groups group_size tile_args = do
  let privstms = processPrivStms tile_args
      red_comm = processComm tile_args
      red_lam = processRedLam tile_args
      map_lam = processMapLam tile_args
      tiles = processTiles tile_args
      accs = processAcc tile_args
      tile_id = processTileId tile_args

  -- Might be truncated in case of a partial tile.
  actual_tile_size <- findTileSize tiles

  segMap2D
    "acc"
    (SegThread num_groups group_size (SegNoVirtFull (SegSeqDims [])))
    ResultPrivate
    (tile_size, tile_size)
    $ \(ltid_x, ltid_y) -> do
      reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)

      addPrivStms [DimFix $ Var ltid_x, DimFix $ Var ltid_y] privstms

      -- We replace the neutral elements with the accumulators (this is
      -- OK because the parallel semantics are not used after this
      -- point).
      thread_accs <- forM accs $ \acc ->
        letSubExp "acc" $ BasicOp $ Index acc $ Slice [DimFix $ Var ltid_x, DimFix $ Var ltid_y]
      let form' = redomapSOAC [Reduce red_comm red_lam thread_accs] map_lam

          sliceTile (InputUntiled arr) =
            sliceUntiled arr tile_id tile_size actual_tile_size
          sliceTile (InputTiled perm tile) = do
            tile_t <- lookupType tile
            let idx = DimFix $ Var $ head $ rearrangeShape perm [ltid_x, ltid_y]
            letExp "tile" $
              BasicOp $ Index tile $ sliceAt tile_t (head perm) [idx]

      tiles' <- mapM sliceTile tiles

      fmap varsRes $
        letTupExp "acc"
          =<< eIf
            ( toExp $ le64 gtid_x .<. pe64 kdim_x .&&. le64 gtid_y .<. pe64 kdim_y
            )
            (eBody [pure $ Op $ OtherOp $ Screma actual_tile_size tiles' form'])
            (resultBodyM thread_accs)

processResidualTile2D ::
  (VName, VName) ->
  (VName, VName) ->
  (SubExp, SubExp) ->
  SubExp ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  ResidualTileArgs ->
  Builder GPU [VName]
processResidualTile2D
  gids
  gtids
  kdims
  tile_size
  num_groups
  group_size
  args = do
    -- The number of residual elements that are not covered by
    -- the whole tiles.
    residual_input <-
      letSubExp "residual_input" $
        BasicOp $ BinOp (SRem Int64 Unsafe) w tile_size

    letTupExp "acc_after_residual"
      =<< eIf
        (toExp $ pe64 residual_input .==. 0)
        (resultBodyM $ map Var accs)
        (nonemptyTile residual_input)
    where
      privstms = residualPrivStms args
      red_comm = residualComm args
      red_lam = residualRedLam args
      map_lam = residualMapLam args
      accs = residualAcc args
      inputs = residualInput args
      num_whole_tiles = residualNumWholeTiles args
      w = residualInputSize args

      nonemptyTile residual_input = renameBody <=< runBodyBuilder $ do
        -- Collectively construct a tile.  Threads that are out-of-bounds
        -- provide a blank dummy value.
        full_tile <-
          readTile2D
            kdims
            gtids
            gids
            tile_size
            num_groups
            group_size
            TilePartial
            privstms
            num_whole_tiles
            inputs

        let slice =
              DimSlice (intConst Int64 0) residual_input (intConst Int64 1)
        tiles <- forM full_tile $ \case
          InputTiled perm tile' ->
            InputTiled perm
              <$> letExp "partial_tile" (BasicOp $ Index tile' (Slice [slice, slice]))
          InputUntiled arr ->
            pure $ InputUntiled arr

        let tile_args =
              ProcessTileArgs privstms red_comm red_lam map_lam tiles accs num_whole_tiles

        -- Now each thread performs a traversal of the tile and
        -- updates its accumulator.
        resultBody . map Var
          <$> processTile2D
            gids
            gtids
            kdims
            tile_size
            num_groups
            group_size
            tile_args

tiling2d :: [(VName, SubExp)] -> DoTiling (VName, VName) (SubExp, SubExp)
tiling2d dims_on_top _initial_lvl (gtid_x, gtid_y) (kdim_x, kdim_y) w = do
  gid_x <- newVName "gid_x"
  gid_y <- newVName "gid_y"

  tile_size_key <- nameFromString . pretty <$> newVName "tile_size"
  tile_size <- letSubExp "tile_size" $ Op $ SizeOp $ GetSize tile_size_key SizeTile
  group_size <- letSubExp "group_size" $ BasicOp $ BinOp (Mul Int64 OverflowUndef) tile_size tile_size

  num_groups_x <-
    letSubExp "num_groups_x" $
      BasicOp $ BinOp (SDivUp Int64 Unsafe) kdim_x tile_size
  num_groups_y <-
    letSubExp "num_groups_y" $
      BasicOp $ BinOp (SDivUp Int64 Unsafe) kdim_y tile_size

  num_groups <-
    letSubExp "num_groups_top"
      =<< foldBinOp
        (Mul Int64 OverflowUndef)
        num_groups_x
        (num_groups_y : map snd dims_on_top)

  gid_flat <- newVName "gid_flat"
  let lvl = SegGroup (Count num_groups) (Count group_size) (SegNoVirtFull (SegSeqDims []))
      space =
        SegSpace gid_flat $
          dims_on_top ++ [(gid_x, num_groups_x), (gid_y, num_groups_y)]

  return
    Tiling
      { tilingSegMap = \desc lvl' manifest f ->
          segMap2D desc lvl' manifest (tile_size, tile_size) $ \(ltid_x, ltid_y) -> do
            reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)
            f
              ( untyped $
                  le64 gtid_x .<. pe64 kdim_x .&&. le64 gtid_y .<. pe64 kdim_y
              )
              [DimFix $ Var ltid_x, DimFix $ Var ltid_y],
        tilingReadTile = readTile2D (kdim_x, kdim_y) (gtid_x, gtid_y) (gid_x, gid_y) tile_size (segNumGroups lvl) (segGroupSize lvl),
        tilingProcessTile = processTile2D (gid_x, gid_y) (gtid_x, gtid_y) (kdim_x, kdim_y) tile_size (segNumGroups lvl) (segGroupSize lvl),
        tilingProcessResidualTile = processResidualTile2D (gid_x, gid_y) (gtid_x, gtid_y) (kdim_x, kdim_y) tile_size (segNumGroups lvl) (segGroupSize lvl),
        tilingTileReturns = tileReturns dims_on_top [(kdim_x, tile_size), (kdim_y, tile_size)],
        tilingTileShape = Shape [tile_size, tile_size],
        tilingNumWholeTiles =
          letSubExp "num_whole_tiles" $
            BasicOp $ BinOp (SQuot Int64 Unsafe) w tile_size,
        tilingLevel = lvl,
        tilingSpace = space
      }
