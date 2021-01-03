{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Perform a restricted form of loop tiling within SegMaps.  We only
-- tile primitive types, to avoid excessive local memory use.
module Futhark.Optimise.TileLoops (tileLoops) where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import Futhark.IR.Kernels
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename
import Prelude hiding (quot)

-- | The pass definition.
tileLoops :: Pass Kernels Kernels
tileLoops =
  Pass "tile loops" "Tile stream loops inside kernels" $
    intraproceduralTransformation onStms
  where
    onStms scope stms =
      modifyNameSource $
        runState $
          runReaderT (optimiseStms stms) scope

type TileM = ReaderT (Scope Kernels) (State VNameSource)

optimiseBody :: Body Kernels -> TileM (Body Kernels)
optimiseBody (Body () stms res) =
  Body () <$> optimiseStms stms <*> pure res

optimiseStms :: Stms Kernels -> TileM (Stms Kernels)
optimiseStms stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM optimiseStm (stmsToList stms)

optimiseStm :: Stm Kernels -> TileM (Stms Kernels)
optimiseStm (Let pat aux (Op (SegOp (SegMap lvl@SegThread {} space ts kbody)))) = do
  (host_stms, (lvl', space', kbody')) <- tileInKernelBody mempty initial_variance lvl space ts kbody
  return $
    host_stms
      <> oneStm (Let pat aux $ Op $ SegOp $ SegMap lvl' space' ts kbody')
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
  KernelBody Kernels ->
  TileM (Stms Kernels, (SegLevel, SegSpace, KernelBody Kernels))
tileInKernelBody branch_variant initial_variance lvl initial_kspace ts kbody
  | Just kbody_res <- mapM isSimpleResult $ kernelBodyResult kbody = do
    maybe_tiled <-
      tileInBody branch_variant mempty initial_variance lvl initial_kspace ts $
        Body () (kernelBodyStms kbody) kbody_res
    case maybe_tiled of
      Just (host_stms, tiling, tiledBody) -> do
        (res', stms') <-
          runBinder $ mapM (tilingTileReturns tiling) =<< tiledBody mempty
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
    isSimpleResult (Returns _ se) = Just se
    isSimpleResult _ = Nothing

tileInBody ::
  Names ->
  Names ->
  VarianceTable ->
  SegLevel ->
  SegSpace ->
  [Type] ->
  Body Kernels ->
  TileM (Maybe (Stms Kernels, Tiling, TiledBody))
tileInBody branch_variant private initial_variance initial_lvl initial_space res_ts (Body () initial_kstms stms_res) =
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
        Just . injectPrelude initial_space private variance prestms' used
          <$> tileGeneric
            (tiling2d $ reverse $ zip top_gtids_rev top_kdims_rev)
            initial_lvl
            res_ts
            (stmPattern stm_to_tile)
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
        Just . injectPrelude initial_space private variance prestms' used
          <$> tileGeneric
            (tiling1d $ reverse top_space_rev)
            initial_lvl
            res_ts
            (stmPattern stm_to_tile)
            gtid
            kdim
            w
            form
            inputs
            poststms'
            stms_res
      -- Tiling inside for-loop.
      | DoLoop [] merge (ForLoop i it bound []) loopbody <- stmExp stm_to_tile,
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
            private' = namesFromList $ map paramName merge_params

        maybe_tiled <-
          localScope (M.insert i (IndexName it) $ scopeOfFParams merge_params) $
            tileInBody
              branch_variant'
              private'
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
                (stmPattern stm_to_tile)
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
  Stms Kernels ->
  Stm Kernels ->
  Stms Kernels ->
  (Stms Kernels, Stms Kernels)
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
        patternNames $ stmPattern stm

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
  Stms Kernels ->
  Names ->
  Names ->
  (Stms Kernels, Stms Kernels, Stms Kernels)
partitionPrelude variance prestms private used_after =
  (invariant_prestms, precomputed_variant_prestms, recomputed_variant_prestms)
  where
    invariantTo names stm =
      case patternNames (stmPattern stm) of
        [] -> True -- Does not matter.
        v : _ ->
          not $
            any (`nameIn` names) $
              namesToList $
                M.findWithDefault mempty v variance
    (invariant_prestms, variant_prestms) =
      Seq.partition (invariantTo private) prestms

    mustBeInlinedExp (BasicOp (Index _ slice)) = not $ null $ sliceDims slice
    mustBeInlinedExp (BasicOp Rotate {}) = True
    mustBeInlinedExp (BasicOp Rearrange {}) = True
    mustBeInlinedExp (BasicOp Reshape {}) = True
    mustBeInlinedExp _ = False
    mustBeInlined stm =
      mustBeInlinedExp (stmExp stm)
        && any (`nameIn` used_after) (patternNames (stmPattern stm))

    must_be_inlined =
      namesFromList $
        concatMap (patternNames . stmPattern) $
          stmsToList $ Seq.filter mustBeInlined variant_prestms
    recompute stm =
      any (`nameIn` must_be_inlined) (patternNames (stmPattern stm))
        || not (invariantTo must_be_inlined stm)
    (recomputed_variant_prestms, precomputed_variant_prestms) =
      Seq.partition recompute variant_prestms

-- Anything that is variant to the "private" names should be
-- considered thread-local.
injectPrelude ::
  SegSpace ->
  Names ->
  VarianceTable ->
  Stms Kernels ->
  Names ->
  (Stms Kernels, Tiling, TiledBody) ->
  (Stms Kernels, Tiling, TiledBody)
injectPrelude initial_space private variance prestms used (host_stms, tiling, tiledBody) =
  (host_stms, tiling, tiledBody')
  where
    private' =
      private
        <> namesFromList
          ( map fst $
              filter (`notElem` unSegSpace (tilingSpace tiling)) $
                unSegSpace initial_space
          )

    tiledBody' privstms = do
      let ( invariant_prestms,
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

      tiledBody (prelude_privstms <> privstms)

tileDoLoop ::
  SegSpace ->
  VarianceTable ->
  Stms Kernels ->
  Names ->
  (Stms Kernels, Tiling, TiledBody) ->
  [Type] ->
  Pattern Kernels ->
  StmAux (ExpDec Kernels) ->
  [(FParam Kernels, SubExp)] ->
  VName ->
  IntType ->
  SubExp ->
  Stms Kernels ->
  Result ->
  TileM (Stms Kernels, Tiling, TiledBody)
tileDoLoop initial_space variance prestms used_in_body (host_stms, tiling, tiledBody) res_ts pat aux merge i it bound poststms poststms_res = do
  let prestms_used =
        used_in_body
          <> freeIn poststms
          <> freeIn poststms_res
      ( invariant_prestms,
        precomputed_variant_prestms,
        recomputed_variant_prestms
        ) =
          partitionPrelude variance prestms tiled_kdims prestms_used

  let (mergeparams, mergeinits) = unzip merge

      -- Expand the loop merge parameters to be arrays.
      tileDim t = arrayOf t (tilingTileShape tiling) $ uniqueness t

      merge_scope = M.insert i (IndexName it) $ scopeOfFParams mergeparams

      tiledBody' privstms = localScope (scopeOf host_stms <> merge_scope) $ do
        addStms invariant_prestms

        let live_set =
              namesToList $
                liveSet precomputed_variant_prestms $
                  freeIn recomputed_variant_prestms <> prestms_used

        prelude_arrs <-
          inScopeOf precomputed_variant_prestms $
            doPrelude tiling privstms precomputed_variant_prestms live_set

        mergeparams' <- forM mergeparams $ \(Param pname pt) ->
          Param <$> newVName (baseString pname ++ "_group") <*> pure (tileDim pt)

        let merge_ts = map paramType mergeparams

        let inloop_privstms =
              PrivStms recomputed_variant_prestms $
                mkReadPreludeValues prelude_arrs live_set

        mergeinit' <-
          fmap (map Var) $
            certifying (stmAuxCerts aux) $
              tilingSegMap tiling "tiled_loopinit" (scalarLevel tiling) ResultPrivate $
                \in_bounds slice ->
                  fmap (map Var) $
                    protectOutOfBounds "loopinit" in_bounds merge_ts $ do
                      addPrivStms slice inloop_privstms
                      addPrivStms slice privstms
                      return mergeinits

        let merge' = zip mergeparams' mergeinit'

        let indexMergeParams slice =
              localScope (scopeOfFParams mergeparams') $
                forM_ (zip mergeparams mergeparams') $ \(to, from) ->
                  letBindNames [paramName to] $
                    BasicOp $
                      Index (paramName from) $
                        fullSlice (paramType from) slice

        loopbody' <-
          runBodyBinder $
            resultBody . map Var
              <$> tiledBody (PrivStms mempty indexMergeParams <> privstms <> inloop_privstms)
        accs' <-
          letTupExp "tiled_inside_loop" $
            DoLoop [] merge' (ForLoop i it bound []) loopbody'

        postludeGeneric tiling (privstms <> inloop_privstms) pat accs' poststms poststms_res res_ts

  return (host_stms, tiling, tiledBody')
  where
    tiled_kdims =
      namesFromList $
        map fst $
          filter (`notElem` unSegSpace (tilingSpace tiling)) $
            unSegSpace initial_space

doPrelude :: Tiling -> PrivStms -> Stms Kernels -> [VName] -> Binder Kernels [VName]
doPrelude tiling privstms prestms prestms_live =
  -- Create a SegMap that takes care of the prelude for every thread.
  tilingSegMap tiling "prelude" (scalarLevel tiling) ResultPrivate $
    \in_bounds slice -> do
      ts <- mapM lookupType prestms_live
      fmap (map Var) $
        letTupExp "pre"
          =<< eIf
            (toExp in_bounds)
            ( do
                addPrivStms slice privstms
                addStms prestms
                resultBodyM $ map Var prestms_live
            )
            (eBody $ map eBlank ts)

liveSet :: FreeIn a => Stms Kernels -> a -> Names
liveSet stms after =
  namesFromList (concatMap (patternNames . stmPattern) stms)
    `namesIntersection` freeIn after

tileable ::
  Stm Kernels ->
  Maybe
    ( SubExp,
      [VName],
      (Commutativity, Lambda Kernels, [SubExp], Lambda Kernels)
    )
tileable stm
  | Op (OtherOp (Screma w form arrs)) <- stmExp stm,
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
  MonadBinder m =>
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
data PrivStms = PrivStms (Stms Kernels) ReadPrelude

privStms :: Stms Kernels -> PrivStms
privStms stms = PrivStms stms $ const $ return ()

addPrivStms :: Slice SubExp -> PrivStms -> Binder Kernels ()
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

type ReadPrelude = Slice SubExp -> Binder Kernels ()

data ProcessTileArgs = ProcessTileArgs
  { processPrivStms :: PrivStms,
    processComm :: Commutativity,
    processRedLam :: Lambda Kernels,
    processMapLam :: Lambda Kernels,
    processTiles :: [InputTile],
    processAcc :: [VName],
    processTileId :: SubExp
  }

data ResidualTileArgs = ResidualTileArgs
  { residualPrivStms :: PrivStms,
    residualComm :: Commutativity,
    residualRedLam :: Lambda Kernels,
    residualMapLam :: Lambda Kernels,
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
      (PrimExp VName -> Slice SubExp -> Binder Kernels [SubExp]) ->
      Binder Kernels [VName],
    -- The boolean PrimExp indicates whether they are in-bounds.

    tilingReadTile ::
      TileKind ->
      PrivStms ->
      SubExp ->
      [InputArray] ->
      Binder Kernels [InputTile],
    tilingProcessTile ::
      ProcessTileArgs ->
      Binder Kernels [VName],
    tilingProcessResidualTile ::
      ResidualTileArgs ->
      Binder Kernels [VName],
    tilingTileReturns :: VName -> Binder Kernels KernelResult,
    tilingSpace :: SegSpace,
    tilingTileShape :: Shape,
    tilingLevel :: SegLevel,
    tilingNumWholeTiles :: Binder Kernels SubExp
  }

type DoTiling gtids kdims =
  SegLevel -> gtids -> kdims -> SubExp -> Binder Kernels Tiling

scalarLevel :: Tiling -> SegLevel
scalarLevel tiling =
  SegThread (segNumGroups lvl) (segGroupSize lvl) SegNoVirt
  where
    lvl = tilingLevel tiling

protectOutOfBounds ::
  String ->
  PrimExp VName ->
  [Type] ->
  Binder Kernels [SubExp] ->
  Binder Kernels [VName]
protectOutOfBounds desc in_bounds ts m =
  letTupExp desc =<< eIf (toExp in_bounds) (resultBody <$> m) (eBody $ map eBlank ts)

postludeGeneric ::
  Tiling ->
  PrivStms ->
  Pattern Kernels ->
  [VName] ->
  Stms Kernels ->
  Result ->
  [Type] ->
  Binder Kernels [VName]
postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts =
  tilingSegMap tiling "thread_res" (scalarLevel tiling) ResultPrivate $ \in_bounds slice -> do
    -- Read our per-thread result from the tiled loop.
    forM_ (zip (patternNames pat) accs') $ \(us, everyone) -> do
      everyone_t <- lookupType everyone
      letBindNames [us] $ BasicOp $ Index everyone $ fullSlice everyone_t slice

    if poststms == mempty
      then do
        -- The privstms may still be necessary for the result.
        addPrivStms slice privstms
        return poststms_res
      else fmap (map Var) $
        protectOutOfBounds "postlude" in_bounds res_ts $ do
          addPrivStms slice privstms
          addStms poststms
          return poststms_res

type TiledBody = PrivStms -> Binder Kernels [VName]

tileGeneric ::
  DoTiling gtids kdims ->
  SegLevel ->
  [Type] ->
  Pattern Kernels ->
  gtids ->
  kdims ->
  SubExp ->
  (Commutativity, Lambda Kernels, [SubExp], Lambda Kernels) ->
  [InputArray] ->
  Stms Kernels ->
  Result ->
  TileM (Stms Kernels, Tiling, TiledBody)
tileGeneric doTiling initial_lvl res_ts pat gtids kdims w form inputs poststms poststms_res = do
  (tiling, tiling_stms) <- runBinder $ doTiling initial_lvl gtids kdims w

  return (tiling_stms, tiling, tiledBody tiling)
  where
    (red_comm, red_lam, red_nes, map_lam) = form

    tiledBody :: Tiling -> PrivStms -> Binder Kernels [VName]
    tiledBody tiling privstms = do
      let tile_shape = tilingTileShape tiling

      num_whole_tiles <- tilingNumWholeTiles tiling

      -- We don't use a Replicate here, because we want to enforce a
      -- scalar memory space.
      mergeinits <- tilingSegMap tiling "mergeinit" (scalarLevel tiling) ResultPrivate $ \in_bounds slice ->
        -- Constant neutral elements (a common case) do not need protection from OOB.
        if freeIn red_nes == mempty
          then return red_nes
          else fmap (map Var) $
            protectOutOfBounds "neutral" in_bounds (lambdaReturnType red_lam) $ do
              addPrivStms slice privstms
              return red_nes

      merge <- forM (zip (lambdaParams red_lam) mergeinits) $ \(p, mergeinit) ->
        (,)
          <$> newParam
            (baseString (paramName p) ++ "_merge")
            (paramType p `arrayOfShape` tile_shape `toDecl` Unique)
          <*> pure (Var mergeinit)

      tile_id <- newVName "tile_id"
      let loopform = ForLoop tile_id Int64 num_whole_tiles []
      loopbody <- renameBody <=< runBodyBinder $
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

      accs <- letTupExp "accs" $ DoLoop [] merge loopform loopbody

      -- We possibly have to traverse a residual tile.
      red_lam' <- renameLambda red_lam
      map_lam' <- renameLambda map_lam
      let residual_args =
            ResidualTileArgs privstms red_comm red_lam' map_lam' inputs accs w num_whole_tiles
      accs' <- tilingProcessResidualTile tiling residual_args

      -- Create a SegMap that takes care of the postlude for every thread.
      postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts

data TileKind = TilePartial | TileFull

mkReadPreludeValues :: [VName] -> [VName] -> ReadPrelude
mkReadPreludeValues prestms_live_arrs prestms_live slice =
  fmap mconcat $
    forM (zip prestms_live_arrs prestms_live) $ \(arr, v) -> do
      arr_t <- lookupType arr
      letBindNames [v] $ BasicOp $ Index arr $ fullSlice arr_t slice

tileReturns :: [(VName, SubExp)] -> [(SubExp, SubExp)] -> VName -> Binder Kernels KernelResult
tileReturns dims_on_top dims arr = do
  let unit_dims = replicate (length dims_on_top) (intConst Int64 1)
  arr' <-
    if null dims_on_top
      then return arr
      else do
        arr_t <- lookupType arr
        let new_shape = unit_dims ++ arrayDims arr_t
        letExp (baseString arr) $ BasicOp $ Reshape (map DimNew new_shape) arr
  let tile_dims = zip (map snd dims_on_top) unit_dims ++ dims
  return $ TileReturns tile_dims arr'

is1DTileable :: VName -> M.Map VName Names -> VName -> InputArray
is1DTileable gtid variance arr
  | not $ nameIn gtid $ M.findWithDefault mempty arr variance =
    InputTile [0] arr
  | otherwise =
    InputDontTile arr

segMap1D ::
  String ->
  SegLevel ->
  ResultManifest ->
  (VName -> Binder Kernels [SubExp]) ->
  Binder Kernels [VName]
segMap1D desc lvl manifest f = do
  ltid <- newVName "ltid"
  ltid_flat <- newVName "ltid_flat"
  let space = SegSpace ltid_flat [(ltid, unCount $ segGroupSize lvl)]

  ((ts, res), stms) <- runBinder $ do
    res <- f ltid
    ts <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $
    Op $
      SegOp $
        SegMap lvl space ts $ KernelBody () stms' $ map (Returns manifest) res'

reconstructGtids1D ::
  Count GroupSize SubExp ->
  VName ->
  VName ->
  VName ->
  Binder Kernels ()
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
  Binder Kernels [InputTile]
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
            letExp "tile_elem" (BasicOp $ Index arr [DimFix j])
      fmap (map Var) $
        case kind of
          TilePartial ->
            letTupExp "pre"
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
  Binder Kernels [VName]
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
      letSubExp "acc" $ BasicOp $ Index acc [DimFix $ Var ltid]
    let sliceTile (InputTiled _ arr) =
          pure arr
        sliceTile (InputUntiled arr) =
          sliceUntiled arr tile_id tile_size tile_size

    tiles' <- mapM sliceTile tiles

    let form' = redomapSOAC [Reduce red_comm red_lam thread_accs] map_lam
    fmap (map Var) $
      letTupExp "acc"
        =<< eIf
          (toExp $ le64 gtid .<. pe64 kdim)
          (eBody [pure $ Op $ OtherOp $ Screma tile_size form' tiles'])
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
  Binder Kernels [VName]
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

    nonemptyTile residual_input = runBodyBinder $ do
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
              <$> letExp "partial_tile" (BasicOp $ Index tile [slice])

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

segMap2D ::
  String ->
  SegLevel ->
  ResultManifest ->
  (SubExp, SubExp) ->
  ((VName, VName) -> Binder Kernels [SubExp]) ->
  Binder Kernels [VName]
segMap2D desc lvl manifest (dim_x, dim_y) f = do
  ltid_x <- newVName "ltid_x"
  ltid_y <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let space = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_x, ltid_y)
    ts <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $
    Op $
      SegOp $
        SegMap lvl space ts $ KernelBody () stms' $ map (Returns manifest) res'

-- Reconstruct the original gtids from group and local IDs.
reconstructGtids2D ::
  SubExp ->
  (VName, VName) ->
  (VName, VName) ->
  (VName, VName) ->
  Binder Kernels ()
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
  Binder Kernels [InputTile]
readTile2D (kdim_x, kdim_y) (gtid_x, gtid_y) (gid_x, gid_y) tile_size num_groups group_size kind privstms tile_id inputs =
  fmap (inputsToTiles inputs)
    . segMap2D
      "full_tile"
      (SegThread num_groups group_size SegNoVirtFull)
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
              ( BasicOp $
                  Index
                    arr
                    [DimFix $ last $ rearrangeShape perm [i, j]]
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
              (eBody [return $ BasicOp $ Index arr [DimFix idx]])
              (eBody [eBlank tile_t])

      fmap (map Var) $
        case kind of
          TilePartial ->
            mapM (letExp "pre" <=< readTileElemIfInBounds) arrs_and_perms
          TileFull ->
            mapM readTileElem arrs_and_perms

findTileSize :: HasScope lore m => [InputTile] -> m SubExp
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
  Binder Kernels [VName]
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
    (SegThread num_groups group_size SegNoVirtFull)
    ResultPrivate
    (tile_size, tile_size)
    $ \(ltid_x, ltid_y) -> do
      reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)

      addPrivStms [DimFix $ Var ltid_x, DimFix $ Var ltid_y] privstms

      -- We replace the neutral elements with the accumulators (this is
      -- OK because the parallel semantics are not used after this
      -- point).
      thread_accs <- forM accs $ \acc ->
        letSubExp "acc" $ BasicOp $ Index acc [DimFix $ Var ltid_x, DimFix $ Var ltid_y]
      let form' = redomapSOAC [Reduce red_comm red_lam thread_accs] map_lam

          sliceTile (InputUntiled arr) =
            sliceUntiled arr tile_id tile_size actual_tile_size
          sliceTile (InputTiled perm tile) = do
            tile_t <- lookupType tile
            let idx = DimFix $ Var $ head $ rearrangeShape perm [ltid_x, ltid_y]
            letExp "tile" $
              BasicOp $ Index tile $ sliceAt tile_t (head perm) [idx]

      tiles' <- mapM sliceTile tiles

      fmap (map Var) $
        letTupExp "acc"
          =<< eIf
            ( toExp $ le64 gtid_x .<. pe64 kdim_x .&&. le64 gtid_y .<. pe64 kdim_y
            )
            (eBody [pure $ Op $ OtherOp $ Screma actual_tile_size form' tiles'])
            (resultBodyM thread_accs)

processResidualTile2D ::
  (VName, VName) ->
  (VName, VName) ->
  (SubExp, SubExp) ->
  SubExp ->
  Count NumGroups SubExp ->
  Count GroupSize SubExp ->
  ResidualTileArgs ->
  Binder Kernels [VName]
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

      nonemptyTile residual_input = renameBody <=< runBodyBinder $ do
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
              <$> letExp "partial_tile" (BasicOp $ Index tile' [slice, slice])
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
  let lvl = SegGroup (Count num_groups) (Count group_size) SegNoVirtFull
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
                  le64 gtid_x .<. pe64 kdim_x
                    .&&. le64 gtid_y .<. pe64 kdim_y
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

-- | The variance table keeps a mapping from a variable name
-- (something produced by a 'Stm') to the kernel thread indices
-- that name depends on.  If a variable is not present in this table,
-- that means it is bound outside the kernel (and so can be considered
-- invariant to all dimensions).
type VarianceTable = M.Map VName Names

varianceInStms :: VarianceTable -> Stms Kernels -> VarianceTable
varianceInStms = foldl varianceInStm

varianceInStm :: VarianceTable -> Stm Kernels -> VarianceTable
varianceInStm variance bnd =
  foldl' add variance $ patternNames $ stmPattern bnd
  where
    add variance' v = M.insert v binding_variance variance'
    look variance' v = oneName v <> M.findWithDefault mempty v variance'
    binding_variance = mconcat $ map (look variance) $ namesToList (freeIn bnd)
