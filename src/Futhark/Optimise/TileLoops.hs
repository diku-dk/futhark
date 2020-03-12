{-egmap LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Perform a restricted form of loop tiling within SegMaps.  We only
-- tile primitive types, to avoid excessive local memory use.
module Futhark.Optimise.TileLoops
       ( tileLoops )
       where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.List

import Prelude hiding (quot)

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Transform.Rename
import Futhark.Pass
import Futhark.Tools
import Futhark.Optimise.BlkRegTiling

tileLoops :: Pass Kernels Kernels
tileLoops = Pass "tile loops" "Tile stream loops inside kernels" $
            fmap Prog . mapM optimiseFunDef . progFunctions

optimiseFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
optimiseFunDef fundec = do
  body' <- modifyNameSource $ runState $
           runReaderT m (scopeOfFParams (funDefParams fundec))
  return fundec { funDefBody = body' }
  where m = optimiseBody $ funDefBody fundec

type TileM = ReaderT (Scope Kernels) (State VNameSource)

optimiseBody :: Body Kernels -> TileM (Body Kernels)
optimiseBody (Body () bnds res) = localScope (scopeOf bnds) $
  Body () <$> (mconcat <$> mapM optimiseStm (stmsToList bnds)) <*> pure res

-- optimiseStm :: Stm Kernels -> TileM (Stms Kernels)
-- optimiseStm stm@(Let pat aux (Op (SegOp (SegMap lvl@SegThread{} space ts kbody)))) = do
--   mmm_tiling <- mmmTiling2D lvl space ts kbody
--   (host_stms, (lvl', space', kbody')) <-
--     case mmm_tiling of
--       Just (host_stms, (lvl', space', kbody')) -> return (host_stms, (lvl', space', kbody'))
--       Nothing -> -- do
--         tileInKernelBody mempty initial_variance lvl space ts kbody
--   return $ host_stms <> oneStm (Let pat aux $ Op $ SegOp $ SegMap lvl' space' ts kbody')
--   where initial_variance = M.map mempty $ scopeOfSegSpace space

optimiseStm :: Stm Kernels -> TileM (Stms Kernels)
optimiseStm stm@(Let pat aux (Op (SegOp (SegMap lvl@SegThread{} space ts kbody)))) = do
  mmm_tiling <- mmmTiling2D stm
  case mmm_tiling of
    Just (extra_bnds, stmt') -> return (extra_bnds <> oneStm stmt')
    Nothing -> do
      (host_stms, (lvl', space', kbody')) <- tileInKernelBody mempty initial_variance lvl space ts kbody
      return $ host_stms <> oneStm (Let pat aux $ Op $ SegOp $ SegMap lvl' space' ts kbody')
  where initial_variance = M.map mempty $ scopeOfSegSpace space

optimiseStm (Let pat aux e) =
  pure <$> (Let pat aux <$> mapExpM optimise e)
  where optimise = identityMapper { mapOnBody = \scope -> localScope scope . optimiseBody }

tileInKernelBody :: Names                -- branch_variant
                 -> VarianceTable        -- initial_variance
                 -> SegLevel             -- lvl
                 -> SegSpace             -- initial_kspace
                 -> [Type]               -- ts
                 -> KernelBody Kernels   -- kbody
                 -> TileM (Stms Kernels, (SegLevel, SegSpace, KernelBody Kernels))
tileInKernelBody branch_variant initial_variance lvl initial_kspace ts kbody
  | Just kbody_res <- mapM isSimpleResult $ kernelBodyResult kbody = do
      maybe_tiled <-
        tileInBody branch_variant initial_variance lvl initial_kspace ts $
        Body () (kernelBodyStms kbody) kbody_res
      case maybe_tiled of
        Just (host_stms, tiling, tiledBody) -> do
          (res', stms') <-
            runBinder $ mapM (tilingTileReturns tiling) =<< tiledBody mempty
          return (host_stms, (tilingLevel tiling,
                              tilingSpace tiling,
                              KernelBody () stms' res'))
        Nothing ->
          return (mempty, (lvl, initial_kspace, kbody))
  | otherwise =
      return (mempty, (lvl, initial_kspace, kbody))
  where isSimpleResult (Returns _ se) = Just se
        isSimpleResult _ = Nothing

tileInBody :: Names         -- branch_variant
           -> VarianceTable -- initial_variance
           -> SegLevel      -- initial_lvl
           -> SegSpace      -- initial_space
           -> [Type]        -- res_ts
           -> Body Kernels  -- (Body () initial_kstms stms_res)
           -> TileM (Maybe (Stms Kernels, Tiling, TiledBody))
tileInBody branch_variant initial_variance initial_lvl initial_space res_ts (Body () initial_kstms stms_res) =
  descend mempty $ stmsToList initial_kstms
  where
    variance = varianceInStms initial_variance initial_kstms

    descend _ [] =
      return Nothing

    descend prestms (stm_to_tile : poststms)

      -- 1D tiling of redomap.
      | (gtid, kdim) : top_space_rev <- reverse $ unSegSpace initial_space,
        Just (w, arrs, form) <- tileable stm_to_tile,
        not $ any (nameIn gtid .
                   flip (M.findWithDefault mempty) variance) arrs,
        not $ gtid `nameIn` branch_variant,
        (prestms', poststms') <-
          preludeToPostlude variance prestms stm_to_tile (stmsFromList poststms),
        used <- freeIn stm_to_tile <> freeIn stms_res =

          Just . injectPrelude initial_space variance prestms' used <$>
          tileGeneric (tiling1d $ reverse top_space_rev)
          initial_lvl res_ts (stmPattern stm_to_tile)
          gtid kdim
          w form (zip arrs $ repeat [0]) poststms' stms_res

      -- 2D tiling of redomap.
      | (gtids, kdims) <- unzip $ unSegSpace initial_space,
        Just (w, arrs, form) <- tileable stm_to_tile,
        Just inner_perm <- mapM (invariantToOneOfTwoInnerDims branch_variant variance gtids) arrs,
        gtid_y : gtid_x : top_gtids_rev <- reverse gtids,
        kdim_y : kdim_x : top_kdims_rev <- reverse kdims,
        (prestms', poststms') <-
          preludeToPostlude variance prestms stm_to_tile (stmsFromList poststms),
        used <- freeIn stm_to_tile <> freeIn stms_res =

          Just . injectPrelude initial_space variance prestms' used <$>
          tileGeneric (tiling2d $ reverse $ zip top_gtids_rev top_kdims_rev)
          initial_lvl res_ts (stmPattern stm_to_tile)
          (gtid_x, gtid_y) (kdim_x, kdim_y)
          w form (zip arrs inner_perm) poststms' stms_res

      -- Tiling inside for-loop.
      | DoLoop [] merge (ForLoop i it bound []) loopbody <- stmExp stm_to_tile,
        (prestms', poststms') <-
          preludeToPostlude variance prestms stm_to_tile (stmsFromList poststms) = do

          let branch_variant' =
                branch_variant <>
                mconcat (map (flip (M.findWithDefault mempty) variance)
                        (namesToList (freeIn bound)))
              merge_params = map fst merge

          maybe_tiled <-
            localScope (M.insert i (IndexInfo it) $ scopeOfFParams merge_params) $
            tileInBody branch_variant' variance initial_lvl initial_space
            (map paramType merge_params) $ mkBody (bodyStms loopbody) (bodyResult loopbody)

          case maybe_tiled of
            Nothing -> next
            Just tiled ->
              Just <$> tileDoLoop initial_space variance prestms'
              (freeIn loopbody <> freeIn merge) tiled
              res_ts (stmPattern stm_to_tile) (stmAux stm_to_tile)
              merge i it bound poststms' stms_res

      | otherwise = next

      where next = localScope (scopeOf stm_to_tile) $
                   descend (prestms <> oneStm stm_to_tile) poststms

-- | Move statements from prelude to postlude if they are not used in
-- the tiled statement anyway.
preludeToPostlude :: VarianceTable -- variance
                  -> Stms Kernels  -- prelude
                  -> Stm Kernels   -- stm_to_tile
                  -> Stms Kernels  -- postlude
                  -> (Stms Kernels, Stms Kernels) -- (used in prelude, moved to postlude)
preludeToPostlude variance prelude stm_to_tile postlude =
  (prelude_used, prelude_not_used <> postlude)
  where used_in_tiled = freeIn stm_to_tile

        used_in_stm_variant =
          (used_in_tiled<>) $ mconcat $
          map (flip (M.findWithDefault mempty) variance) $
          namesToList used_in_tiled

        used stm = any (`nameIn` used_in_stm_variant) $
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
-- results that are views of an array (slicing, rotate, etc), because
-- these cannot be efficiently represented by a scalar segmap (they'll
-- be manifested in memory).
partitionPrelude :: VarianceTable -- variance
                 -> Stms Kernels  -- prestms
                 -> Names         -- tiled_kdims
                 -> (Stms Kernels, Stms Kernels, Stms Kernels)
partitionPrelude variance prestms tiled_kdims =
  (invariant_prestms, precomputed_variant_prestms, recomputed_variant_prestms)
  where
    invariantTo names stm =
      case patternNames (stmPattern stm) of
        [] -> True -- Does not matter.
        v:_ -> not $ any (`nameIn` names) $ namesToList $
               M.findWithDefault mempty v variance
    (invariant_prestms, variant_prestms) =
      Seq.partition (invariantTo tiled_kdims) prestms

    mustBeInlinedExp (BasicOp (Index _ slice)) = not $ null $ sliceDims slice
    mustBeInlinedExp (BasicOp Rotate{}) = True
    mustBeInlinedExp (BasicOp Rearrange{}) = True
    mustBeInlinedExp (BasicOp Reshape{}) = True
    mustBeInlinedExp _ = False
    mustBeInlined = mustBeInlinedExp . stmExp

    must_be_inlined = namesFromList $ concatMap (patternNames . stmPattern) $
                      stmsToList $ Seq.filter mustBeInlined variant_prestms
    recompute stm =
      any (`nameIn` must_be_inlined) (patternNames (stmPattern stm)) ||
      not (invariantTo must_be_inlined stm)
    (recomputed_variant_prestms, precomputed_variant_prestms) =
      Seq.partition recompute variant_prestms

injectPrelude :: SegSpace -> VarianceTable
              -> Stms Kernels -> Names
              -> (Stms Kernels, Tiling, TiledBody)
              -> (Stms Kernels, Tiling, TiledBody)
injectPrelude initial_space variance prestms used (host_stms, tiling, tiledBody) =
  (host_stms, tiling, tiledBody')
  where tiled_kdims = namesFromList $ map fst $
                      filter (`notElem` unSegSpace (tilingSpace tiling)) $
                      unSegSpace initial_space

        tiledBody' privstms = do
          let (invariant_prestms,
               precomputed_variant_prestms,
               recomputed_variant_prestms) =
                partitionPrelude variance prestms tiled_kdims

          addStms invariant_prestms

          let live_set = namesToList $ liveSet precomputed_variant_prestms $
                         used <> freeIn recomputed_variant_prestms
          prelude_arrs <- inScopeOf precomputed_variant_prestms $
                          doPrelude tiling precomputed_variant_prestms live_set

          let prelude_privstms =
                PrivStms recomputed_variant_prestms $
                mkReadPreludeValues prelude_arrs live_set

          tiledBody (prelude_privstms <> privstms)

tileDoLoop :: SegSpace                          -- initial_space
           -> VarianceTable                     -- variance
           -> Stms Kernels                      -- prestms
           -> Names                             -- used_in_body
           -> (Stms Kernels, Tiling, TiledBody) -- host_stms, tiling, tiledBody
           -> [Type]                            -- res_ts
           -> Pattern Kernels                   -- pat
           -> StmAux (ExpAttr Kernels)          -- aux
           -> [(FParam Kernels, SubExp)]        -- merge
           -> VName                             -- i
           -> IntType                           -- it
           -> SubExp                            -- bound
           -> Stms Kernels                      -- poststms
           -> Result                            -- poststms_res
           -> TileM (Stms Kernels, Tiling, TiledBody)
tileDoLoop initial_space variance prestms used_in_body (host_stms, tiling, tiledBody) res_ts pat aux merge i it bound poststms poststms_res = do

  let (invariant_prestms,
       precomputed_variant_prestms,
       recomputed_variant_prestms) =
        partitionPrelude variance prestms tiled_kdims

  let (mergeparams, mergeinits) = unzip merge

      -- Expand the loop merge parameters to be arrays.
      tileDim t = arrayOf t (tilingTileShape tiling) $ uniqueness t

      tiledBody' privstms = inScopeOf host_stms $ do
        addStms invariant_prestms

        let live_set = namesToList $ liveSet precomputed_variant_prestms used_in_body
        prelude_arrs <- inScopeOf precomputed_variant_prestms $
                        doPrelude tiling precomputed_variant_prestms live_set

        mergeparams' <- forM mergeparams $ \(Param pname pt) ->
          Param <$> newVName (baseString pname ++ "_group") <*> pure (tileDim pt)

        let merge_ts = map paramType mergeparams

        let inloop_privstms =
              PrivStms recomputed_variant_prestms $
              mkReadPreludeValues prelude_arrs live_set

        mergeinit' <-
          fmap (map Var) $ certifying (stmAuxCerts aux) $
          tilingSegMap tiling "tiled_loopinit" (scalarLevel tiling) ResultPrivate $
          \in_bounds slice ->
            fmap (map Var) $ protectOutOfBounds "loopinit" in_bounds merge_ts $ do
            addPrivStms slice inloop_privstms
            addPrivStms slice privstms
            return mergeinits

        let merge' = zip mergeparams' mergeinit'

        let indexMergeParams slice =
              localScope (scopeOfFParams mergeparams') $
              forM_ (zip mergeparams mergeparams') $ \(to, from) ->
              letBindNames_ [paramName to] $ BasicOp $ Index (paramName from) $
              fullSlice (paramType from) slice

        loopbody' <- runBodyBinder $ resultBody . map Var <$>
                     tiledBody (privstms <> inloop_privstms <> PrivStms mempty indexMergeParams)
        accs' <- letTupExp "tiled_inside_loop" $
                 DoLoop [] merge' (ForLoop i it bound []) loopbody'

        postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts

  return (host_stms, tiling, tiledBody')

  where tiled_kdims = namesFromList $ map fst $
                      filter (`notElem` unSegSpace (tilingSpace tiling)) $
                      unSegSpace initial_space

doPrelude :: Tiling       -- tiling
          -> Stms Kernels -- prestms
          -> [VName]      -- prestms_live
          -> Binder Kernels [VName]
doPrelude tiling prestms prestms_live =
  -- Create a SegMap that takes care of the prelude for every thread.
  tilingSegMap tiling "prelude" (scalarLevel tiling) ResultPrivate $
  \in_bounds _slice -> do
    ts <- mapM lookupType prestms_live
    fmap (map Var) $ letTupExp "pre" =<<
      eIf (toExp in_bounds)
      (do addStms prestms
          resultBodyM $ map Var prestms_live)
      (eBody $ map eBlank ts)

liveSet :: FreeIn a => Stms Kernels -> a -> Names
liveSet stms after =
  namesFromList (concatMap (patternNames . stmPattern) stms) `namesIntersection`
  freeIn after

tileable :: Stm Kernels
         -> Maybe (SubExp, [VName],
                   (Commutativity, Lambda Kernels, [SubExp], Lambda Kernels))
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
    where stms_z = stms_x <> stms_y
          readPrelude_z slice = readPrelude_x slice >> readPrelude_y slice

instance Monoid PrivStms where
  mempty = privStms mempty

type ReadPrelude = Slice SubExp -> Binder Kernels ()

-- | Information about a loop that has been tiled inside a kernel, as
-- well as the kinds of changes that we would then like to perform on
-- the kernel.
data Tiling =
  Tiling
  { tilingSegMap :: String
                 -> SegLevel
                 -> ResultManifest
                 -> (PrimExp VName
                      -> Slice SubExp
                      -> Binder Kernels [SubExp])
                 -> Binder Kernels [VName]
    -- The boolean PrimExp indicates whether they are in-bounds.

  , tilingReadTile :: TileKind -> PrivStms
                   -> SubExp -> [(VName, [Int])]
                   -> Binder Kernels [VName]

  , tilingProcessTile :: PrivStms
                      -> Commutativity -> Lambda Kernels -> Lambda Kernels
                      -> [(VName, [Int])] -> [VName]
                      -> Binder Kernels [VName]

  , tilingProcessResidualTile :: PrivStms
                              -> Commutativity -> Lambda Kernels -> Lambda Kernels
                              -> SubExp -> [VName] -> SubExp
                              -> [(VName, [Int])]
                              -> Binder Kernels [VName]

  , tilingTileReturns :: VName -> Binder Kernels KernelResult

  , tilingSpace :: SegSpace

  , tilingTileShape :: Shape

  , tilingLevel :: SegLevel

  , tilingNumWholeTiles :: SubExp
  }

type DoTiling gtids kdims =
  SegLevel -> gtids -> kdims -> SubExp -> Binder Kernels Tiling

scalarLevel :: Tiling -> SegLevel
scalarLevel tiling =
  SegThread (segNumGroups lvl) (segGroupSize lvl) SegNoVirt
  where lvl = tilingLevel tiling

protectOutOfBounds :: String -> PrimExp VName -> [Type] -> Binder Kernels [SubExp]
                   -> Binder Kernels [VName]
protectOutOfBounds desc in_bounds ts m =
  letTupExp desc =<< eIf (toExp in_bounds) (resultBody <$> m) (eBody $ map eBlank ts)

postludeGeneric :: Tiling          -- tiling
                -> PrivStms        -- privstms
                -> Pattern Kernels -- pat
                -> [VName]         -- accs'
                -> Stms Kernels    -- poststms
                -> Result          -- poststms_res
                -> [Type]          -- res_ts
                -> Binder Kernels [VName]
postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts =
  tilingSegMap tiling "thread_res" (scalarLevel tiling) ResultPrivate $ \in_bounds slice -> do
    -- Read our per-thread result from the tiled loop.
    forM_ (zip (patternNames pat) accs') $ \(us, everyone) ->
      letBindNames_ [us] $ BasicOp $ Index everyone slice

    if poststms == mempty
      then do -- The privstms may still be necessary for the result.
        addPrivStms slice privstms
        return poststms_res

      else
        fmap (map Var) $ protectOutOfBounds "postlude" in_bounds res_ts $ do
        addPrivStms slice privstms
        addStms poststms
        return poststms_res

type TiledBody = PrivStms -> Binder Kernels [VName]

tileGeneric :: DoTiling gtids kdims -- doTiling
            -> SegLevel             -- initial_lvl
            -> [Type]               -- res_ts
            -> Pattern Kernels      -- pat
            -> gtids                -- gtids
            -> kdims                -- kdims
            -> SubExp               -- w
            -> (Commutativity, Lambda Kernels, [SubExp], Lambda Kernels) -- form
            -> [(VName, [Int])]     -- arrs_and_perms
            -> Stms Kernels         -- poststms
            -> Result               -- poststms_res
            -> TileM (Stms Kernels, Tiling, TiledBody)
tileGeneric doTiling initial_lvl res_ts pat gtids kdims w form arrs_and_perms poststms poststms_res = do

  (tiling, tiling_stms) <- runBinder $ doTiling initial_lvl gtids kdims w

  return (tiling_stms, tiling, tiledBody tiling)

  where
    (red_comm, red_lam, red_nes, map_lam) = form

    tiledBody :: Tiling -> PrivStms -> Binder Kernels [VName]
    tiledBody tiling privstms = do
      let num_whole_tiles = tilingNumWholeTiles tiling
          tile_shape = tilingTileShape tiling

      -- We don't use a Replicate here, because
      -- we want to enforce a scalar memory space.
      mergeinits <- tilingSegMap tiling "tileGeneric.mergeinit" (scalarLevel tiling) ResultPrivate $ \in_bounds slice ->
        -- Constant neutral elements (a common case) do not need protection from OOB.
        if freeIn red_nes == mempty
          then return red_nes
          else fmap (map Var) $ protectOutOfBounds "tileGeneric.neutral" in_bounds (lambdaReturnType red_lam) $ do
          addPrivStms slice privstms
          return red_nes

      merge <- forM (zip (lambdaParams red_lam) mergeinits) $ \(p, mergeinit) ->
        (,) <$>
        newParam ("tileGeneric." ++ baseString (paramName p) ++ "_merge")
        (paramType p `arrayOfShape` tile_shape `toDecl` Unique) <*>
        pure (Var mergeinit)

      tile_id <- newVName "tileGeneric.tile_id"
      let loopform = ForLoop tile_id Int32 num_whole_tiles []
      loopbody <- renameBody <=< runBodyBinder $ inScopeOf loopform $
                  localScope (scopeOfFParams $ map fst merge) $ do

        -- Collectively read a tile.
        tile <- tilingReadTile tiling TileFull privstms (Var tile_id) arrs_and_perms

        -- Now each thread performs a traversal of the tile and
        -- updates its accumulator.
        resultBody . map Var <$>
          tilingProcessTile tiling privstms
          red_comm red_lam map_lam
          (zip tile (map snd arrs_and_perms)) (map (paramName . fst) merge)

      accs <- letTupExp "tileGeneric.accs" $ DoLoop [] merge loopform loopbody

      -- We possibly have to traverse a residual tile.
      red_lam' <- renameLambda red_lam
      map_lam' <- renameLambda map_lam
      accs' <- tilingProcessResidualTile tiling privstms
               red_comm red_lam' map_lam'
               num_whole_tiles accs w arrs_and_perms

      -- Create a SegMap that takes care of the postlude for every thread.
      postludeGeneric tiling privstms pat accs' poststms poststms_res res_ts

data TileKind = TilePartial | TileFull

mkReadPreludeValues :: [VName] -> [VName] -> ReadPrelude
mkReadPreludeValues prestms_live_arrs prestms_live slice =
  fmap mconcat $ forM (zip prestms_live_arrs prestms_live) $ \(arr, v) -> do
  arr_t <- lookupType arr
  letBindNames_ [v] $ BasicOp $ Index arr $ fullSlice arr_t slice

tileReturns :: [(VName, SubExp)] -> [(SubExp, SubExp)] -> VName -> Binder Kernels KernelResult
tileReturns dims_on_top dims arr = do
  let unit_dims = replicate (length dims_on_top) (intConst Int32 1)
  arr' <- if null dims_on_top then return arr
          else do arr_t <- lookupType arr
                  let new_shape = unit_dims ++ arrayDims arr_t
                  letExp (baseString arr) $ BasicOp $ Reshape (map DimNew new_shape) arr
  let tile_dims = zip (map snd dims_on_top) unit_dims ++ dims
  return $ TileReturns tile_dims arr'

segMap1D :: String                             -- desc
         -> SegLevel                           -- lvl
         -> ResultManifest                     -- manifest
         -> (VName -> Binder Kernels [SubExp]) -- f
         -> Binder Kernels [VName]
segMap1D desc lvl manifest f = do
  ltid <- newVName "ltid"
  ltid_flat <- newVName "ltid_flat"
  let space = SegSpace ltid_flat [(ltid, unCount $ segGroupSize lvl)]

  ((ts, res), stms) <- runBinder $ do
    res <- f ltid
    ts <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl space ts $ KernelBody () stms' $ map (Returns manifest) res'

reconstructGtids1D :: Count GroupSize SubExp -> VName -> VName -> VName
                   -> Binder Kernels ()
reconstructGtids1D group_size gtid gid ltid  =
  letBindNames_ [gtid] =<<
    toExp (LeafExp gid int32 *
           primExpFromSubExp int32 (unCount group_size) +
           LeafExp ltid int32)

readTile1D :: SubExp                 -- tile_size
           -> VName                  -- gid
           -> VName                  -- gtid
           -> Count NumGroups SubExp -- num_groups
           -> Count GroupSize SubExp -- group_size
           -> TileKind               -- kind
           -> PrivStms               -- privstms
           -> SubExp                 -- tile_id
           -> [(VName, [Int])]       -- arrs_and_perms
           -> Binder Kernels [VName]
readTile1D
  tile_size gid gtid num_groups group_size
  kind privstms tile_id arrs_and_perms =

  segMap1D "full_tile" (SegThread num_groups group_size SegNoVirt) ResultNoSimplify $ \ltid -> do
    j <- letSubExp "j" =<<
         toExp (primExpFromSubExp int32 tile_id *
                primExpFromSubExp int32 tile_size +
                LeafExp ltid int32)

    reconstructGtids1D group_size gtid gid ltid
    addPrivStms [DimFix $ Var ltid] privstms

    let arrs = map fst arrs_and_perms
    arr_ts <- mapM lookupType arrs
    let tile_ts = map rowType arr_ts
        w = arraysSize 0 arr_ts

    let readTileElem arr =
          -- No need for fullSlice because we are tiling only prims.
          letExp "tile_elem" $ BasicOp $ Index arr [DimFix j]
    fmap (map Var) $
      case kind of
        TilePartial ->
          letTupExp "pre" =<< eIf (toExp $ primExpFromSubExp int32 j .<.
                                   primExpFromSubExp int32 w)
          (resultBody <$> mapM (fmap Var . readTileElem) arrs)
          (eBody $ map eBlank tile_ts)
        TileFull ->
          mapM readTileElem arrs

processTile1D :: VName -> VName -> SubExp -> SubExp
              -> Count NumGroups SubExp -> Count GroupSize SubExp
              -> PrivStms
              -> Commutativity -> Lambda Kernels -> Lambda Kernels
              -> [(VName, [Int])] -> [VName]
              -> Binder Kernels [VName]
processTile1D
  gid gtid kdim tile_size num_groups group_size
  privstms
  red_comm red_lam map_lam tiles_and_perm accs = do

  let tile = map fst tiles_and_perm

  segMap1D "acc1D" (SegThread num_groups group_size SegNoVirt) ResultPrivate $ \ltid -> do

    reconstructGtids1D group_size gtid gid ltid
    addPrivStms [DimFix $ Var ltid] privstms

    -- We replace the neutral elements with the accumulators (this is
    -- OK because the parallel semantics are not used after this
    -- point).
    thread_accs <- forM accs $ \acc ->
      letSubExp "acc" $ BasicOp $ Index acc [DimFix $ Var ltid]
    let form' = redomapSOAC [Reduce red_comm red_lam thread_accs] map_lam

    fmap (map Var) $
      letTupExp "acc" =<< eIf (toExp $ LeafExp gtid int32 .<. primExpFromSubExp int32 kdim)
      (eBody [pure $ Op $ OtherOp $ Screma tile_size form' tile])
      (resultBodyM thread_accs)

processResidualTile1D :: VName                  -- gid
                      -> VName                  -- gtid
                      -> SubExp                 -- kdim
                      -> SubExp                 -- tile_size
                      -> Count NumGroups SubExp -- num_groups
                      -> Count GroupSize SubExp -- group_size
                      -> PrivStms               -- privstms
                      -> Commutativity          -- red_comm
                      -> Lambda Kernels         -- red_lam
                      -> Lambda Kernels         -- map_lam
                      -> SubExp                 -- num_whole_tiles
                      -> [VName]                -- accs
                      -> SubExp                 -- w
                      -> [(VName, [Int])]       -- arrs_and_perms
                      -> Binder Kernels [VName]
processResidualTile1D
  gid gtid kdim tile_size num_groups group_size privstms red_comm red_lam map_lam
  num_whole_tiles accs w arrs_and_perms = do
  -- The number of residual elements that are not covered by
  -- the whole tiles.
  residual_input <- letSubExp "residual_input" $
    BasicOp $ BinOp (SRem Int32) w tile_size

  letTupExp "acc_after_residual" =<<
    eIf (toExp $ primExpFromSubExp int32 residual_input .==. 0)
    (resultBodyM $ map Var accs)
    (nonemptyTile residual_input)

  where
    nonemptyTile residual_input = runBodyBinder $ do
      -- Collectively construct a tile.  Threads that are out-of-bounds
      -- provide a blank dummy value.
      full_tile <- readTile1D tile_size gid gtid  num_groups group_size
                   TilePartial privstms num_whole_tiles arrs_and_perms
      tile <- forM full_tile $ \tile ->
        letExp "partial_tile" $ BasicOp $ Index tile
        [DimSlice (intConst Int32 0) residual_input (intConst Int32 1)]

      -- Now each thread performs a traversal of the tile and
      -- updates its accumulator.
      resultBody . map Var <$> processTile1D
        gid gtid kdim residual_input num_groups group_size privstms
        red_comm red_lam map_lam (zip tile $ repeat [0]) accs

tiling1d :: [(VName, SubExp)] -> DoTiling VName SubExp
tiling1d dims_on_top initial_lvl gtid kdim w = do
  gid <- newVName "gid"
  gid_flat <- newVName "gid_flat"

  (lvl, space) <-
    if null dims_on_top
    then return (SegGroup (segNumGroups initial_lvl) (segGroupSize initial_lvl) $ segVirt initial_lvl,
                 SegSpace gid_flat [(gid, unCount $ segNumGroups initial_lvl)])
    else do
      group_size <- letSubExp "computed_group_size" $
                    BasicOp $ BinOp (SMin Int32) (unCount (segGroupSize initial_lvl)) kdim

      -- How many groups we need to exhaust the innermost dimension.
      ldim <- letSubExp "ldim" =<<
              eDivRoundingUp Int32 (eSubExp kdim) (eSubExp group_size)

      num_groups <- letSubExp "computed_num_groups" =<<
                    foldBinOp (Mul Int32) ldim (map snd dims_on_top)

      return (SegGroup (Count num_groups) (Count group_size) SegNoVirt,
              SegSpace gid_flat $ dims_on_top ++ [(gid, ldim)])
  let tile_size = unCount $ segGroupSize lvl

  -- Number of whole tiles that fit in the input.
  num_whole_tiles <- letSubExp "num_whole_tiles" $ BasicOp $ BinOp (SQuot Int32) w tile_size
  return Tiling
    { tilingSegMap = \desc lvl' manifest f -> segMap1D desc lvl' manifest $ \ltid -> do
        letBindNames_ [gtid] =<<
          toExp (LeafExp gid int32 * primExpFromSubExp int32 tile_size +
                 LeafExp ltid int32)
        f (LeafExp gtid int32 .<. primExpFromSubExp int32 kdim)
          [DimFix $ Var ltid]

    , tilingReadTile =
        readTile1D tile_size gid gtid (segNumGroups lvl) (segGroupSize lvl)

    , tilingProcessTile =
        processTile1D gid gtid kdim tile_size (segNumGroups lvl) (segGroupSize lvl)

    , tilingProcessResidualTile =
        processResidualTile1D gid gtid kdim tile_size (segNumGroups lvl) (segGroupSize lvl)

    , tilingTileReturns = tileReturns dims_on_top [(kdim, tile_size)]

    , tilingTileShape = Shape [tile_size]
    , tilingNumWholeTiles = num_whole_tiles
    , tilingLevel = lvl
    , tilingSpace = space
    }

invariantToOneOfTwoInnerDims :: Names -> M.Map VName Names -> [VName] -> VName
                             -> Maybe [Int]
invariantToOneOfTwoInnerDims branch_variant variance dims arr = do
  j : i : _ <- Just $ reverse dims
  let variant_to = M.findWithDefault mempty arr variance
      branch_invariant = not $ nameIn j branch_variant || nameIn i branch_variant
  if branch_invariant && i `nameIn` variant_to && not (j `nameIn` variant_to) then
    Just [0,1]
  else if branch_invariant && j `nameIn` variant_to && not (i `nameIn` variant_to) then
    Just [1,0]
  else
    Nothing

segMap2D :: String           -- desc
         -> SegLevel         -- lvl
         -> ResultManifest   -- manifest
         -> (SubExp, SubExp) -- (dim_x, dim_y)
         -> ((VName, VName)  -- f
             -> Binder Kernels [SubExp])
         -> Binder Kernels [VName]
segMap2D desc lvl manifest (dim_x, dim_y) f = do
  ltid_x    <- newVName "segMap2D.ltid_x"
  ltid_y    <- newVName "segMap2D.ltid_y"
  ltid_flat <- newVName "segMap2D.ltid_flat"
  let space = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_x, ltid_y)
    ts  <- mapM subExpType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  letTupExp desc $ Op $ SegOp $
    SegMap lvl space ts $ KernelBody () stms' $ map (Returns manifest) res'

-- Reconstruct the original gtids from group and local IDs.
reconstructGtids2D :: SubExp -> (VName, VName) -> (VName, VName) -> (VName, VName)
                   -> Binder Kernels ()
reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y) = do
  -- Reconstruct the original gtids from gid_x/gid_y and ltid_x/ltid_y.
  letBindNames_ [gtid_x] =<<
    toExp (LeafExp gid_x int32 * primExpFromSubExp int32 tile_size +
           LeafExp ltid_x int32)
  letBindNames_ [gtid_y] =<<
    toExp (LeafExp gid_y int32 * primExpFromSubExp int32 tile_size +
            LeafExp ltid_y int32)

readTile2D :: (SubExp, SubExp) -> (VName, VName) -> (VName, VName) -> SubExp
           -> Count NumGroups SubExp -> Count GroupSize SubExp
           -> TileKind -> PrivStms -> SubExp
           -> [(VName, [Int])]
           -> Binder Kernels [VName]
readTile2D (kdim_x, kdim_y) (gtid_x, gtid_y) (gid_x, gid_y) tile_size num_groups group_size kind privstms tile_id arrs_and_perms =
  segMap2D "readTile2D.full_tile" (SegThread num_groups group_size SegNoVirt)
  ResultNoSimplify (tile_size, tile_size) $ \(ltid_x, ltid_y) -> do

    i <- letSubExp "i" =<<
         toExp (primExpFromSubExp int32 tile_id *
                primExpFromSubExp int32 tile_size +
                LeafExp ltid_x int32)
    j <- letSubExp "j" =<<
         toExp (primExpFromSubExp int32 tile_id *
                primExpFromSubExp int32 tile_size +
                LeafExp ltid_y int32)

    reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)
    addPrivStms [DimFix $ Var ltid_x, DimFix $ Var ltid_y] privstms

    let (arrs, perms) = unzip arrs_and_perms
    arr_ts <- mapM lookupType arrs
    let tile_ts = map rowType arr_ts
        w = arraysSize 0 arr_ts

    let readTileElem arr perm =
          -- No need for fullSlice because we are tiling only prims.
          letExp "readTile2D.tile_elem" $ BasicOp $ Index arr
          [DimFix $ last $ rearrangeShape perm [i,j]]
        readTileElemIfInBounds (tile_t, arr, perm) = do
          let idx = last $ rearrangeShape perm [i,j]
              othercheck = last $ rearrangeShape perm
                           [ LeafExp gtid_y int32 .<. primExpFromSubExp int32 kdim_y
                           , LeafExp gtid_x int32 .<. primExpFromSubExp int32 kdim_x
                           ]
          eIf (toExp $
               primExpFromSubExp int32 idx .<. primExpFromSubExp int32 w .&&. othercheck)
            (eBody [return $ BasicOp $ Index arr [DimFix idx]])
            (eBody [eBlank tile_t])

    fmap (map Var) $
      case kind of
        TilePartial ->
          mapM (letExp "readTile2D.pre" <=< readTileElemIfInBounds) (zip3 tile_ts arrs perms)
        TileFull ->
          zipWithM readTileElem arrs perms

processTile2D :: (VName, VName)           -- (gid_x,  gid_y)
              -> (VName, VName)           -- (gtid_x, gtid_y)
              -> (SubExp, SubExp)         -- (kdim_x, kdim_y)
              -> SubExp                   -- tile_size
              -> Count NumGroups SubExp   -- num_groups
              -> Count GroupSize SubExp   -- group_size
              -> PrivStms                 -- privstms
              -> Commutativity            -- red_comm
              -> Lambda Kernels           -- red_lam
              -> Lambda Kernels           -- map_lam
              -> [(VName,[Int])]          -- tiles_and_perms
              -> [VName]                  -- accs
              -> Binder Kernels [VName]
processTile2D
  (gid_x, gid_y) (gtid_x, gtid_y) (kdim_x, kdim_y) tile_size num_groups group_size
  privstms red_comm red_lam map_lam tiles_and_perms accs = do

  -- Might be truncated in case of a partial tile.
  actual_tile_size <- arraysSize 0 <$> mapM (lookupType . fst) tiles_and_perms

  segMap2D "processTile2D.acc" (SegThread num_groups group_size SegNoVirt)
    ResultPrivate (tile_size, tile_size) $ \(ltid_x, ltid_y) -> do
    reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)

    addPrivStms [DimFix $ Var ltid_x, DimFix $ Var ltid_y] privstms

    -- We replace the neutral elements with the accumulators (this is
    -- OK because the parallel semantics are not used after this
    -- point).
    thread_accs <- forM accs $ \acc ->
      letSubExp "processTile2D.acc" $ BasicOp $ Index acc [DimFix $ Var ltid_x, DimFix $ Var ltid_y]
    let form' = redomapSOAC [Reduce red_comm red_lam thread_accs] map_lam

    tiles' <- forM tiles_and_perms $ \(tile, perm) -> do
      tile_t <- lookupType tile
      letExp "processTile2D.tile" $ BasicOp $ Index tile $ sliceAt tile_t (head perm)
        [DimFix $ Var $ head $ rearrangeShape perm [ltid_x, ltid_y]]

    fmap (map Var) $
      letTupExp "processTile2D.acc" =<< eIf (toExp $
                               LeafExp gtid_x int32 .<. primExpFromSubExp int32 kdim_x .&&.
                               LeafExp gtid_y int32 .<. primExpFromSubExp int32 kdim_y)
      (eBody [pure $ Op $ OtherOp $ Screma actual_tile_size form' tiles'])
      (resultBodyM thread_accs)

processResidualTile2D :: (VName, VName)           -- gids
                      -> (VName, VName)           -- gtids
                      -> (SubExp, SubExp)         -- kdims
                      -> SubExp                   -- tile_size
                      -> Count NumGroups SubExp   -- num_groups
                      -> Count GroupSize SubExp   -- group_size
                      -> PrivStms                 -- privstms
                      -> Commutativity            -- red_comm
                      -> Lambda Kernels           -- red_lam
                      -> Lambda Kernels           -- map_lam
                      -> SubExp                   -- num_whole_tiles
                      -> [VName]                  -- accs
                      -> SubExp                   -- w
                      -> [(VName, [Int])]         -- arrs_and_perms
                      -> Binder Kernels [VName]
processResidualTile2D
  gids gtids kdims tile_size num_groups group_size privstms red_comm red_lam map_lam
  num_whole_tiles accs w arrs_and_perms = do
  -- The number of residual elements that are not covered by
  -- the whole tiles.
  residual_input <- letSubExp "procResTile2D.residual_input" $
    BasicOp $ BinOp (SRem Int32) w tile_size

  letTupExp "procResTile2D.acc_after_residual" =<<
    eIf (toExp $ primExpFromSubExp int32 residual_input .==. 0)
    (resultBodyM $ map Var accs)
    (nonemptyTile residual_input)

  where
    nonemptyTile residual_input = renameBody <=< runBodyBinder $ do
      -- Collectively construct a tile.  Threads that are out-of-bounds
      -- provide a blank dummy value.
      full_tile <- readTile2D kdims gtids gids tile_size num_groups group_size
                   TilePartial privstms num_whole_tiles arrs_and_perms

      tile <- forM full_tile $ \tile ->
        letExp "procResTile2D.partial_tile" $ BasicOp $ Index tile
        [DimSlice (intConst Int32 0) residual_input (intConst Int32 1),
         DimSlice (intConst Int32 0) residual_input (intConst Int32 1)]

      -- Now each thread performs a traversal of the tile and
      -- updates its accumulator.
      resultBody . map Var <$>
        processTile2D gids gtids kdims tile_size num_groups group_size
        privstms red_comm red_lam map_lam
        (zip tile (map snd arrs_and_perms)) accs

tiling2d :: [(VName, SubExp)] -> DoTiling (VName, VName) (SubExp, SubExp)
tiling2d dims_on_top _initial_lvl (gtid_x, gtid_y) (kdim_x, kdim_y) w = do
  gid_x <- newVName "tiling2D.gid_x"
  gid_y <- newVName "tiling2D.gid_y"

  tile_size_key <- nameFromString . pretty <$> newVName "tiling2D.tile_size"
  tile_size  <- letSubExp "tiling2D.tile_size" $ Op $ SizeOp $ GetSize tile_size_key SizeTile
  group_size <- letSubExp "tiling2D.group_size" $ BasicOp $ BinOp (Mul Int32) tile_size tile_size

  num_groups_x <- letSubExp "tiling2D.num_groups_x" =<<
                  eDivRoundingUp Int32 (eSubExp kdim_x) (eSubExp tile_size)
  num_groups_y <- letSubExp "tiling2D.num_groups_y" =<<
                  eDivRoundingUp Int32 (eSubExp kdim_y) (eSubExp tile_size)

  num_groups <- letSubExp "tiling2D.num_groups_top" =<<
                foldBinOp (Mul Int32) num_groups_x
                (num_groups_y : map snd dims_on_top)

  gid_flat <- newVName "tiling2D.gid_flat"
  let lvl = SegGroup (Count num_groups) (Count group_size) SegNoVirt
      space = SegSpace gid_flat $
              dims_on_top ++ [(gid_x, num_groups_x), (gid_y, num_groups_y)]

  -- Number of whole tiles that fit in the input.
  num_whole_tiles <- letSubExp "tiling2D.num_whole_tiles" $
    BasicOp $ BinOp (SQuot Int32) w tile_size
  return Tiling
    { tilingSegMap = \desc lvl' manifest f ->
        segMap2D desc lvl' manifest (tile_size, tile_size) $ \(ltid_x, ltid_y) -> do
        reconstructGtids2D tile_size (gtid_x, gtid_y) (gid_x, gid_y) (ltid_x, ltid_y)
        f (LeafExp gtid_x int32 .<. primExpFromSubExp int32 kdim_x .&&.
           LeafExp gtid_y int32 .<. primExpFromSubExp int32 kdim_y)
          [DimFix $ Var ltid_x, DimFix $ Var ltid_y]

    , tilingReadTile = readTile2D (kdim_x, kdim_y) (gtid_x, gtid_y) (gid_x, gid_y) tile_size (segNumGroups lvl) (segGroupSize lvl)
    , tilingProcessTile = processTile2D (gid_x, gid_y) (gtid_x, gtid_y) (kdim_x, kdim_y) tile_size (segNumGroups lvl) (segGroupSize lvl)
    , tilingProcessResidualTile = processResidualTile2D (gid_x, gid_y) (gtid_x, gtid_y) (kdim_x, kdim_y) tile_size (segNumGroups lvl) (segGroupSize lvl)

    , tilingTileReturns = tileReturns dims_on_top [(kdim_x, tile_size), (kdim_y, tile_size)]

    , tilingTileShape = Shape [tile_size, tile_size]
    , tilingNumWholeTiles = num_whole_tiles
    , tilingLevel = lvl
    , tilingSpace = space
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
  where add variance' v = M.insert v binding_variance variance'
        look variance' v = oneName v <> M.findWithDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ namesToList (freeIn bnd)
