{-# LANGUAGE TypeFamilies #-}

-- | VJP transformation for Map SOACs.  This is a pretty complicated
-- case due to the possibility of free variables.
module Futhark.AD.Rev.Map (vjpMap) where

import Control.Monad
import Data.Bifunctor (first)
import Debug.Trace
import Futhark.AD.Rev.Monad
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

vecPerm :: Shape -> Type -> [Int]
vecPerm adj_shape t =
  [shapeRank adj_shape]
    ++ [0 .. shapeRank adj_shape - 1]
    ++ [shapeRank adj_shape + 1 .. arrayRank t - 1]

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
    isSparse (AdjSparse (Sparse shape _ ivs)) = do
      guard $ shapeDims shape == [w]
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
  free <- filterM isActive $ namesToList $ freeIn map_lam'

  accAdjoints free $ \free_with_adjs free_without_adjs -> do
    free_adjs <- mapM lookupAdjVal free_with_adjs
    free_adjs_ts <- mapM lookupType free_adjs
    free_adjs_params <- mapM (newParam "free_adj_p") free_adjs_ts
    let lam_rev_params =
          lambdaParams map_lam' ++ pat_adj_params ++ free_adjs_params
        adjs_for = map paramName (lambdaParams map_lam') ++ free
    lam_rev <-
      mkLambda lam_rev_params . subAD . noAdjsFor free_without_adjs $ do
        zipWithM_ insAdj free_with_adjs $ map paramName free_adjs_params
        bodyBind . lambdaBody
          =<< vjpLambda ops (map adjFromParam pat_adj_params) adjs_for map_lam'

    (param_contribs, free_contribs) <-
      fmap (splitAt (length (lambdaParams map_lam'))) $
        auxing aux . letTupExp "map_adjs" . Op $
          Screma w (as ++ pat_adj_vals ++ free_adjs) (mapSOAC lam_rev)

    -- Crucial that we handle the free contribs first in case 'free'
    -- and 'as' intersect.
    zipWithM_ freeContrib free free_contribs
    let param_ts = map paramType (lambdaParams map_lam')
    forM_ (zip3 param_ts as param_contribs) $ \(param_t, a, param_contrib) ->
      case param_t of
        Acc {} -> freeContrib a =<< popAdjShape param_contrib -- CHECKME
        _ -> updateAdj a =<< popAdjShape param_contrib
  where
    addIdxParams n lam = do
      idxs <- replicateM n $ newParam "idx" $ Prim int64
      pure $ lam {lambdaParams = idxs ++ lambdaParams lam}

    accAddLambda n t = addIdxParams n =<< addLambda t

    withAccInput (v, (shape, pt)) = do
      v_adj <- lookupAdjVal v
      add_lam <- accAddLambda (shapeRank shape) $ Prim pt
      zero <- letSubExp "zero" $ zeroExp $ Prim pt
      pure (shape, [v_adj], Just (add_lam, [zero]))

    accAdjoints free m = do
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

    freeContrib v contribs = do
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
