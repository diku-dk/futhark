{-# LANGUAGE TypeFamilies #-}

-- | VJP transformation for 'Map' and 'FlatMap'. This is a pretty complicated
-- case due to the possibility of free variables. The two are handled together
-- because the return sweep of a 'FlatMap' is itself a 'Map', and so requires
-- all the same machinery.
module Futhark.AD.Rev.Map (vjpMap, vjpFlatMap) where

import Control.Monad
import Data.Bifunctor (first, second)
import Data.Either (rights)
import Data.Maybe (catMaybes)
import Futhark.AD.Rev.Monad
import Futhark.AD.Shared (asVName, vecPerm)
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.Tools hiding (withAcc)
import Futhark.Transform.Rename
import Futhark.Util (splitAt3)

-- | A classification of a free variable based on its adjoint.  The
-- 'VName' stored is *not* the adjoint, but the primal variable.
data AdjVar
  = -- | Adjoint is already an accumulator.
    FreeAcc VName
  | -- | Currently has no adjoint, but should be given one, and is an
    -- array with this shape and element type.
    FreeArr VName Shape PrimType
  | -- | Does not need an accumulator adjoint (might still be an array).
    FreeNonAcc VName

classifyAdjVars :: [VName] -> ADM [AdjVar]
classifyAdjVars = mapM f
  where
    f v = do
      v_adj <- lookupAdjVal v
      v_adj_t <- lookupType v_adj
      case v_adj_t of
        Array pt shape _ ->
          pure $ FreeArr v shape pt
        Acc {} ->
          pure $ FreeAcc v
        _ ->
          pure $ FreeNonAcc v

partitionAdjVars :: [AdjVar] -> ([(VName, (Shape, PrimType))], [VName], [VName])
partitionAdjVars [] = ([], [], [])
partitionAdjVars (fv : fvs) =
  case fv of
    FreeArr v shape t -> ((v, (shape, t)) : xs, ys, zs)
    FreeAcc v -> (xs, v : ys, zs)
    FreeNonAcc v -> (xs, ys, v : zs)
  where
    (xs, ys, zs) = partitionAdjVars fvs

buildRenamedBody ::
  (MonadBuilder m) =>
  m (Result, a) ->
  m (Body (Rep m), a)
buildRenamedBody m = do
  (body, x) <- buildBody m
  body' <- renameBody body
  pure (body', x)

withAcc ::
  [(Shape, [VName], Maybe (Lambda SOACS, [SubExp]))] ->
  ([VName] -> ADM Result) ->
  ADM [VName]
withAcc [] m =
  mapM (letExp "withacc_res" . BasicOp . SubExp . resSubExp) =<< m []
withAcc inputs m = do
  (cert_params, acc_params) <- fmap unzip $
    forM inputs $ \(shape, arrs, _) -> do
      cert_param <- newParam "acc_cert_p" $ Prim Unit
      ts <- mapM (fmap (stripArray (shapeRank shape)) . lookupType) arrs
      acc_param <- newParam "acc_p" $ Acc (paramName cert_param) shape ts NoUniqueness
      pure (cert_param, acc_param)
  acc_lam <-
    subAD $ mkLambda (cert_params ++ acc_params) $ m $ map paramName acc_params
  letTupExp "withhacc_res" $ WithAcc inputs acc_lam

pushAdjShape :: VName -> ADM VName
pushAdjShape v = do
  adj_shape <- askShape
  v_t <- lookupType v
  if adj_shape == mempty || arrayShape v_t == adj_shape || isAcc v_t
    then pure v
    else do
      let perm = vecPerm adj_shape v_t
      letExp (baseName v <> "_tr") $ BasicOp $ Rearrange v perm

popAdjShape :: VName -> ADM VName
popAdjShape v = do
  adj_shape <- askShape
  v_t <- lookupType v
  if adj_shape == mempty || arrayShape v_t == adj_shape || isAcc v_t
    then pure v
    else do
      let perm = rearrangeInverse $ vecPerm adj_shape v_t
      letExp (baseName v <> "_tr") $ BasicOp $ Rearrange v perm

addIdxParams :: Int -> Lambda SOACS -> ADM (Lambda SOACS)
addIdxParams n lam = do
  idxs <- replicateM n $ newParam "idx" $ Prim int64
  pure $ lam {lambdaParams = idxs ++ lambdaParams lam}

accAddLambda :: Int -> Type -> ADM (Lambda SOACS)
accAddLambda n t = addIdxParams n =<< addLambda t

withAccInput ::
  (VName, (Shape, PrimType)) ->
  ADM (Shape, [VName], Maybe (Lambda SOACS, [SubExp]))
withAccInput (v, (shape, pt)) = do
  v_adj <- lookupAdjVal v
  add_lam <- accAddLambda (shapeRank shape) $ Prim pt
  zero <- letSubExp "zero" $ zeroExp $ Prim pt
  pure (shape, [v_adj], Just (add_lam, [zero]))

-- | Run an action in a context where the array-typed adjoints of the given
-- free variables have been turned into accumulators, so that the contributions
-- from each iteration of the SOAC are summed.  The action is passed the free
-- variables that were given accumulator adjoints, and those that were not.  The
-- 'VName' list is the input arrays of the SOAC.
accAdjoints :: [VName] -> [VName] -> ([VName] -> Names -> ADM ()) -> ADM ()
accAdjoints as free m = do
  (arr_free, acc_free, nonacc_free) <-
    partitionAdjVars <$> classifyAdjVars free
  arr_free' <- mapM withAccInput arr_free
  -- We only consider those input arrays that are also not free in
  -- the lambda.
  let as_nonfree = filter (`notElem` free) as
  (arr_adjs, acc_adjs, rest_adjs) <-
    fmap (splitAt3 (length arr_free) (length acc_free)) . withAcc arr_free' $ \accs -> do
      zipWithM_ insAdj (map fst arr_free) accs
      () <- m (acc_free ++ map fst arr_free) (namesFromList nonacc_free)
      acc_free_adj <- mapM lookupAdjVal acc_free
      arr_free_adj <- mapM (lookupAdjVal . fst) arr_free
      nonacc_free_adj <- mapM lookupAdjVal nonacc_free
      as_nonfree_adj <- mapM lookupAdjVal as_nonfree
      pure $ varsRes $ arr_free_adj <> acc_free_adj <> nonacc_free_adj <> as_nonfree_adj
  zipWithM_ insAdj acc_free acc_adjs
  zipWithM_ insAdj (map fst arr_free) arr_adjs
  let (nonacc_adjs, as_nonfree_adjs) = splitAt (length nonacc_free) rest_adjs
  zipWithM_ insAdj nonacc_free nonacc_adjs
  zipWithM_ insAdj as_nonfree as_nonfree_adjs

-- | Add the per-iteration contributions to the adjoint of a free variable.  If
-- the adjoint is an accumulator, the summation has already taken place.
freeContrib :: SubExp -> VName -> VName -> ADM ()
freeContrib w v contribs = do
  contribs_t <- lookupType contribs
  case rowType contribs_t of
    Acc {} -> void $ insAdj v contribs
    t -> do
      lam <- addLambda t
      zero <- letSubExp "zero" $ zeroExp t
      reduce <- reduceSOAC [Reduce Commutative lam [zero]]
      contrib_sum <-
        letExp (baseName v <> "_contrib_sum") . Op $
          Screma w [contribs] reduce
      void $ updateAdj v contrib_sum

-- | Turn the 'ExtLambda' of a 'FlatMap' into an ordinary 'Lambda', by dropping
-- the size result and coercing each nonuniform result to the given size, which
-- must be dynamically equal to the size the lambda computes.  See Note
-- [Adjoints of FlatMap].
flatMapPlainLambda :: SubExp -> ExtLambda SOACS -> ADM (Lambda SOACS)
flatMapPlainLambda n (Lambda params rettype body) =
  mkLambda params $ do
    res <- bodyBind body
    forM (zip (drop 1 rettype) (drop 1 res)) $ \(t, res_i@(SubExpRes cs se)) ->
      if flatMapNonuniform t
        then do
          v <- asVName se
          v_t <- lookupType v
          fmap varRes . certifying cs . letExp "flatmap_res_coerce" $
            shapeCoerce (arrayDims (v_t `setOuterSize` n)) v
        else pure res_i

-- | Construct the Map that constitutes the return sweep of a Map-like SOAC.
-- Contributions to the free variables of the lambda are handled here; the
-- contributions to the input arrays are passed to the continuation.
mapReturnSweep ::
  (FreeIn t) =>
  VjpOps ->
  StmAux () ->
  -- | Width of the SOAC and its input arrays.
  (SubExp, [VName]) ->
  -- | Lambda of the SOAC, which must already have been renamed.  Only its
  -- parameters and free variables are used.
  GLambda SOACS t ->
  -- | Additional arrays to map across, and the parameters receiving their
  -- elements.
  [(VName, LParam SOACS)] ->
  -- | Produce the adjoints of the results of the lambda to differentiate, along
  -- with that lambda - which need not be the lambda of the SOAC, but must have
  -- the same parameters.  Run inside the return sweep lambda.
  ADM ([Adj], Lambda SOACS) ->
  -- | Given the contribution to each input array.
  ([VName] -> ADM ()) ->
  ADM ()
mapReturnSweep ops aux (w, as) lam extra mkAdjs onContribs = do
  free <- filterM isActive $ namesToList $ freeIn lam
  accAdjoints as free $ \free_with_adjs free_without_adjs -> do
    free_adjs <- mapM lookupAdjVal free_with_adjs
    free_adjs_ts <- mapM lookupType free_adjs
    free_adjs_params <- mapM (newParam "free_adj_p") free_adjs_ts
    let (extra_arrs, extra_params) = unzip extra
        adjs_for = map paramName (lambdaParams lam) ++ free
    lam_rev <-
      mkLambda (lambdaParams lam ++ extra_params ++ free_adjs_params)
        . subAD
        . noAdjsFor free_without_adjs
        $ do
          zipWithM_ insAdj free_with_adjs $ map paramName free_adjs_params
          (res_adjs, lam') <- mkAdjs
          bodyBind . lambdaBody =<< vjpLambda ops res_adjs adjs_for lam'

    (param_contribs, free_contribs) <-
      fmap (splitAt (length (lambdaParams lam))) $
        auxing aux
          . letTupExp "map_adjs"
          . Op
          . Screma w (as ++ extra_arrs ++ free_adjs)
          =<< mapSOAC lam_rev

    -- Crucial that we handle the free contribs first in case 'free'
    -- and 'as' intersect.
    zipWithM_ (freeContrib w) free free_contribs
    onContribs param_contribs

-- | Perform VJP on a Map.  The 'Adj' list is the adjoints of the
-- result of the map.
vjpMap :: VjpOps -> [Adj] -> StmAux () -> SubExp -> Lambda SOACS -> [VName] -> ADM ()
vjpMap ops res_adjs _ w map_lam as
  | Just res_ivs <- mapM isSparse res_adjs = returnSweepCode $ do
      -- Since at most only a constant number of adjoint are nonzero
      -- (length res_ivs), there is no need for the return sweep code to
      -- contain a Map at all.

      free <- filterM isActive $ namesToList $ freeIn map_lam `namesSubtract` namesFromList as
      free_ts <- mapM lookupType free
      let adjs_for = map paramName (lambdaParams map_lam) ++ free
          adjs_ts = map paramType (lambdaParams map_lam) ++ free_ts

      let oneHot res_i adj_v = zipWith f [0 :: Int ..] $ lambdaReturnType map_lam
            where
              f j t
                | res_i == j = adj_v
                | otherwise = AdjZero (arrayShape t) (elemType t)
          -- Values for the out-of-bounds case does not matter, as we will
          -- be writing to an out-of-bounds index anyway, which is ignored.
          ooBounds adj_i = subAD . buildRenamedBody $ do
            forM_ (zip as adjs_ts) $ \(a, t) -> do
              scratch <- letSubExp "oo_scratch" =<< eBlank t
              updateAdjIndex a (OutOfBounds, adj_i) scratch
            -- We must make sure that all free variables have the same
            -- representation in the oo-branch as in the ib-branch.
            -- In practice we do this by manifesting the adjoint.
            -- This is probably efficient, since the adjoint of a free
            -- variable is probably either a scalar or an accumulator.
            forM_ free $ \v -> insAdj v =<< adjVal =<< lookupAdj v
            first subExpsRes . adjsReps <$> mapM lookupAdj (as <> free)
          inBounds res_i adj_i adj_v = subAD . buildRenamedBody $ do
            forM_ (zip (lambdaParams map_lam) as) $ \(p, a) -> do
              a_t <- lookupType a
              letBindNames [paramName p] . BasicOp . Index a $
                fullSlice a_t [DimFix adj_i]
            adj_elems <-
              fmap (map resSubExp) . bodyBind . lambdaBody
                =<< vjpLambda ops (oneHot res_i (AdjVal adj_v)) adjs_for map_lam
            let (as_adj_elems, free_adj_elems) = splitAt (length as) adj_elems
            forM_ (zip as as_adj_elems) $ \(a, a_adj_elem) ->
              updateAdjIndex a (AssumeBounds, adj_i) a_adj_elem
            forM_ (zip free free_adj_elems) $ \(v, adj_se) -> do
              adj_se_v <- letExp "adj_v" (BasicOp $ SubExp adj_se)
              insAdj v adj_se_v
            first subExpsRes . adjsReps <$> mapM lookupAdj (as <> free)

          -- Generate an iteration of the map function for every
          -- position.  This is a bit inefficient - probably we could do
          -- some deduplication.
          forPos res_i (check, adj_i, adj_v) = do
            adjs <-
              case check of
                CheckBounds b -> do
                  (obbranch, mkadjs) <- ooBounds adj_i
                  (ibbranch, _) <- inBounds res_i adj_i adj_v
                  fmap mkadjs . letTupExp' "map_adj_elem"
                    =<< eIf
                      (maybe (eDimInBounds (eSubExp w) (eSubExp adj_i)) eSubExp b)
                      (pure ibbranch)
                      (pure obbranch)
                AssumeBounds -> do
                  (body, mkadjs) <- inBounds res_i adj_i adj_v
                  mkadjs . map resSubExp <$> bodyBind body
                OutOfBounds ->
                  mapM lookupAdj as

            zipWithM setAdj (as <> free) adjs

          -- Generate an iteration of the map function for every result.
          forRes res_i = mapM_ (forPos res_i)

      zipWithM_ forRes [0 ..] res_ivs
  where
    isSparse (AdjSparse (Sparse shape _ vd ivs)) = do
      guard $ drop vd (shapeDims shape) == [w]
      Just ivs
    isSparse _ =
      Nothing
-- See Note [Adjoints of accumulators] for how we deal with
-- accumulators - it's a bit tricky here.
vjpMap ops pat_adj aux w map_lam as = returnSweepCode $ do
  pat_adj_vals <- forM (zip pat_adj (lambdaReturnType map_lam)) $ \(adj, t) ->
    case t of
      Acc {} -> letExp "acc_adj_rep" . BasicOp . Replicate (Shape [w]) . Var =<< adjVal adj
      _ -> pushAdjShape =<< adjVal adj

  pat_adj_params <-
    mapM (newParam "map_adj_p" . rowType <=< lookupType) pat_adj_vals

  map_lam' <- renameLambda map_lam
  let param_ts = map paramType (lambdaParams map_lam')
      extra = zip pat_adj_vals pat_adj_params
      mkAdjs = pure (map adjFromParam pat_adj_params, map_lam')

  mapReturnSweep ops aux (w, as) map_lam' extra mkAdjs $ \param_contribs ->
    forM_ (zip3 param_ts as param_contribs) $ \(param_t, a, param_contrib) ->
      case param_t of
        Acc {} -> freeContrib w a =<< popAdjShape param_contrib -- CHECKME
        _ -> updateAdj a =<< popAdjShape param_contrib

-- | Perform VJP on a FlatMap.  The 'Adj' list is the adjoints of the results of
-- the FlatMap, including the metadata results.  See Note [Adjoints of FlatMap].
vjpFlatMap ::
  VjpOps ->
  Pat Type ->
  [Adj] ->
  StmAux () ->
  SubExp ->
  ExtLambda SOACS ->
  [VName] ->
  ADM ()
vjpFlatMap ops pat pat_adj aux w lam as = returnSweepCode $ do
  let ((_, shape_arr, _, offset_arr), _) = flatMapSplitMeta $ patNames pat
      (_, val_adjs) = flatMapSplitMeta pat_adj

  -- The size of this iteration's segment, and where in the concatenated
  -- results it begins.
  size_p <- newParam "flatmap_size_p" $ Prim int64
  offset_p <- newParam "flatmap_offset_p" $ Prim int64

  -- 'Left' for a nonuniform result, 'Right' for a uniform one; see Note
  -- [Adjoints of FlatMap].
  (res_adjs, uniform_adj_vals) <-
    fmap (second catMaybes) . mapAndUnzipM resAdj $
      zip (drop 1 (lambdaReturnType lam)) val_adjs

  lam' <- renameLambda lam

  let extra =
        [(shape_arr, size_p), (offset_arr, offset_p)]
          ++ zip uniform_adj_vals (rights res_adjs)
      mkAdjs = do
        res_adjs' <- mapM (segmentAdj size_p offset_p) res_adjs
        plain_lam <- flatMapPlainLambda (Var $ paramName size_p) lam'
        pure (res_adjs', plain_lam)

  mapReturnSweep ops aux (w, as) lam' extra mkAdjs $ \param_contribs ->
    forM_ (zip as param_contribs) $ \(a, param_contrib) ->
      updateAdj a =<< popAdjShape param_contrib
  where
    resAdj (t, adj) = do
      adj_v <- adjVal adj
      if flatMapNonuniform t
        then pure (Left adj_v, Nothing)
        else do
          adj_v' <- pushAdjShape adj_v
          adj_p <- newParam "flatmap_res_adj_p" . rowType =<< lookupType adj_v'
          pure (Right adj_p, Just adj_v')

    segmentAdj _ _ (Right adj_p) = pure $ adjFromParam adj_p
    segmentAdj size_p offset_p (Left adj_v) = do
      adj_v_t <- lookupType adj_v
      let segment =
            DimSlice (Var $ paramName offset_p) (Var $ paramName size_p) (intConst Int64 1)
      slice <- vecSlice adj_v_t [segment]
      fmap adjFromVar . letExp (baseName adj_v <> "_slice") . BasicOp $
        Index adj_v slice

-- Note [Adjoints of FlatMap]
--
-- The return sweep of a FlatMap is an ordinary Map, not a FlatMap.  Iteration
-- 'j' of the FlatMap contributed the segment of each nonuniform (concatenated)
-- result that begins at 'offset[j]' and has length 'shape[j]', where the offset
-- and shape arrays are results of the forward sweep.  This is all we need to
-- find the part of an adjoint that a given iteration is responsible for.
--
-- The inputs are necessarily regular arrays of the same length as the Map, and
-- so are handled exactly as for a Map.  Similarly, free variables of the lambda
-- receive contributions from every iteration, and so are handled with the usual
-- accumulator machinery; see Note [Adjoints of accumulators].  The metadata
-- results are integers, so their adjoints are ignored.
--
-- The adjoints of the results are provided to the lambda in one of two ways:
--
--  * A uniform result has exactly one element per iteration, so its adjoint is
--    an ordinary Map input.
--
--  * The adjoint of a nonuniform result is the concatenation of the adjoints of
--    every segment, which cannot be split into one Map input per iteration
--    because the segments have different sizes.  Instead the entire array is
--    passed as a free variable and sliced inside the lambda.
--
-- Accumulators cannot occur among the inputs or results: the lambda would have
-- to return the updated accumulator, and an accumulator can be neither
-- concatenated nor collected into an array. (This is distinct from an array
-- whose *adjoint* is an accumulator, which happens all the time.) FIXME: it is
-- possible that one way we may return adjoints as the uniform result, but this
-- should be easy to add.
--
-- # Coercing the segment size
--
-- One wrinkle remains. Inside the lambda, a nonuniform result has type '[k]t',
-- where 'k' is the size the lambda itself computes and returns as its first
-- result. The adjoint we slice out of the concatenated adjoint has size
-- 'shape[j]' instead. These are dynamically equal, but they are distinct names
-- as far as the type checker is concerned, and the adjoint of a value of type
-- '[k]t' must have type '[k]t'. We handle this by converting the ExtLambda into
-- a Lambda whose nonuniform results are coerced to size 'shape[j]' (see
-- 'flatMapPlainLambda'), and differentiate that. Everything downstream is then
-- the ordinary machinery for differentiating a lambda, and the size mismatch is
-- dealt with by the existing rule for differentiating a coercion.
