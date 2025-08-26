{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates #-}

-- | Index simplification mechanics.
module Futhark.Optimise.Simplify.Rules.Index
  ( IndexResult (..),
    simplifyIndexing,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Construct
import Futhark.IR
import Futhark.Optimise.Simplify.Rules.Simple
import Futhark.Util

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False

-- | Some index expressions can be simplified to t'SubExp's, while
-- others produce another index expression (which may be further
-- simplifiable).
data IndexResult
  = IndexResult Certs VName (Slice SubExp)
  | SubExpResult Certs SubExp

-- Fake expressions that we can recognise.
--
-- See Note [Simplifying a Slice].
fakeIndices :: [TPrimExp Int64 VName]
fakeIndices = map f [0 :: Int ..]
  where
    f i = isInt64 $ LeafExp (VName v (negate i)) $ IntType Int64
      where
        v = nameFromText ("fake_" <> showText i)

-- | Try to simplify an index operation.
simplifyIndexing ::
  (MonadBuilder m) =>
  ST.SymbolTable (Rep m) ->
  TypeLookup ->
  VName ->
  Slice SubExp ->
  Bool ->
  (VName -> Bool) ->
  Maybe (m IndexResult)
simplifyIndexing vtable seType idd (Slice inds) consuming consumed =
  case defOf idd of
    -- FIXME: This is a special case to avoid simplifying away a slice of a
    -- rearrange. This is because register tiling cannot otherwise properly
    -- detect what is going on.
    Just (Rearrange src perm, cs)
      | rearrangeReach perm <= length (takeWhile isIndex inds) ->
          let inds' = rearrangeShape (rearrangeInverse perm) inds
           in Just $ pure $ IndexResult cs src $ Slice inds'
      | any isIndex inds ->
          Nothing
      where
        isIndex DimFix {} = True
        isIndex _ = False
    _
      | Just t <- seType (Var idd),
        Slice inds == fullSlice t [] ->
          Just $ pure $ SubExpResult mempty $ Var idd
      | Just inds' <- sliceIndices (Slice inds),
        Just (ST.Indexed cs e) <- ST.index idd inds' vtable,
        worthInlining e,
        all (`ST.elem` vtable) (unCerts cs) ->
          Just $ SubExpResult cs <$> toSubExp "index_primexp" e
      -- For the two cases below, see Note [Simplifying a Slice].
      | Just inds' <- sliceIndices (Slice inds),
        Just (ST.IndexedArray cs arr inds'') <- ST.index idd inds' vtable,
        all (worthInlining . untyped) inds'',
        arr `ST.available` vtable,
        all (`ST.elem` vtable) (unCerts cs) ->
          Just $
            IndexResult cs arr . Slice . map DimFix
              <$> mapM (toSubExp "index_primexp") inds''
      | Just (ST.IndexedArray cs arr inds'') <-
          ST.index' idd (fixSlice (pe64 <$> Slice inds) (map fst matches)) vtable,
        length inds == length inds'',
        all (worthInlining . untyped) inds'',
        arr `ST.available` vtable,
        all (`ST.elem` vtable) (unCerts cs),
        not consuming,
        not $ consumed arr,
        Just (ordering, inds''') <- first concat . unzip <$> mapM okIdx inds'',
        Just perm <- L.sort ordering `isPermutationOf` ordering ->
          if isIdentityPerm perm
            then Just $ IndexResult cs arr . Slice <$> sequence inds'''
            else Just $ do
              arr_sliced <-
                certifying cs $
                  letExp (baseName arr <> "_sliced") . BasicOp . Index arr . Slice
                    =<< sequence inds'''
              arr_sliced_tr <-
                letSubExp (baseName arr_sliced <> "_tr") $
                  BasicOp (Rearrange arr_sliced perm)
              pure $ SubExpResult mempty arr_sliced_tr
      where
        matches = zip fakeIndices $ zip [0 :: Int ..] $ sliceDims $ Slice inds
        okIdx i =
          case lookup i matches of
            Just (j, w) ->
              Just ([j], pure $ DimSlice (constant (0 :: Int64)) w (constant (1 :: Int64)))
            Nothing -> do
              guard $ not $ any ((`namesIntersect` freeIn i) . freeIn . fst) matches
              Just ([], DimFix <$> toSubExp "index_primexp" i)
    Nothing -> Nothing
    Just (SubExp (Var v), cs) ->
      Just $ pure $ IndexResult cs v $ Slice inds
    Just (Iota _ x s to_it, cs)
      | [DimFix ii] <- inds,
        Just (Prim (IntType from_it)) <- seType ii ->
          Just $
            let mul = BinOpExp $ Mul to_it OverflowWrap
                add = BinOpExp $ Add to_it OverflowWrap
             in fmap (SubExpResult cs) $
                  toSubExp "index_iota" $
                    ( sExt to_it (primExpFromSubExp (IntType from_it) ii)
                        `mul` primExpFromSubExp (IntType to_it) s
                    )
                      `add` primExpFromSubExp (IntType to_it) x
      | [DimSlice i_offset i_n i_stride] <- inds ->
          Just $ do
            i_offset' <- asIntS to_it i_offset
            i_stride' <- asIntS to_it i_stride
            let mul = BinOpExp $ Mul to_it OverflowWrap
                add = BinOpExp $ Add to_it OverflowWrap
            i_offset'' <-
              toSubExp "iota_offset" $
                ( primExpFromSubExp (IntType to_it) x
                    `mul` primExpFromSubExp (IntType to_it) s
                )
                  `add` primExpFromSubExp (IntType to_it) i_offset'
            i_stride'' <-
              letSubExp "iota_offset" $
                BasicOp $
                  BinOp (Mul Int64 OverflowWrap) s i_stride'
            fmap (SubExpResult cs) $
              letSubExp "slice_iota" $
                BasicOp $
                  Iota i_n i_offset'' i_stride'' to_it
    Just (Index aa ais, cs) ->
      Just $
        IndexResult cs aa
          <$> subExpSlice (sliceSlice (primExpSlice ais) (primExpSlice (Slice inds)))
    Just (Replicate (Shape [_]) (Var vv), cs)
      | [DimFix {}] <- inds,
        ST.available vv vtable ->
          Just $ pure $ SubExpResult cs $ Var vv
      | DimFix {} : is' <- inds,
        not consuming,
        not $ consumed vv,
        ST.available vv vtable ->
          Just $ pure $ IndexResult cs vv $ Slice is'
    Just (Replicate (Shape [_]) val@(Constant _), cs)
      | [DimFix {}] <- inds, not consuming -> Just $ pure $ SubExpResult cs val
    Just (Replicate (Shape ds) v, cs)
      | (ds_inds, rest_inds) <- splitAt (length ds) inds,
        (ds', ds_inds') <- unzip $ mapMaybe index ds_inds,
        ds' /= ds,
        ST.subExpAvailable v vtable ->
          Just $ do
            arr <- letExp "smaller_replicate" $ BasicOp $ Replicate (Shape ds') v
            pure $ IndexResult cs arr $ Slice $ ds_inds' ++ rest_inds
      where
        index DimFix {} = Nothing
        index (DimSlice _ n s) = Just (n, DimSlice (constant (0 :: Int64)) n s)
    Just (Replicate (Shape []) (Var src), cs)
      | Just dims <- arrayDims <$> seType (Var src),
        length inds == length dims,
        not $ consumed src,
        -- It is generally not safe to simplify a slice of a copy,
        -- because the result may be used in an in-place update of the
        -- original.  But we know this can only happen if the original
        -- is bound the same depth as we are!
        all (isJust . dimFix) inds
          || maybe True ((ST.loopDepth vtable /=) . ST.entryDepth) (ST.lookup src vtable),
        not consuming,
        ST.available src vtable ->
          Just $ pure $ IndexResult cs src $ Slice inds
    Just (Reshape src newshape, cs)
      | ReshapeCoerce <- reshapeKind newshape,
        Just olddims <- arrayDims <$> seType (Var src),
        changed_dims <- zipWith (/=) (shapeDims (newShape newshape)) olddims,
        not $ or $ drop (length inds) changed_dims ->
          Just $ pure $ IndexResult cs src $ Slice inds
      | Just olddims <- arrayDims <$> seType (Var src),
        length newshape == length inds,
        length olddims == length (shapeDims (newShape newshape)) ->
          Just $ pure $ IndexResult cs src $ Slice inds
    Just (Reshape v2 newshape, cs)
      | Shape [_] <- newShape newshape,
        Just [_] <- arrayDims <$> seType (Var v2) ->
          Just $ pure $ IndexResult cs v2 $ Slice inds
    Just (Concat d (x :| xs) _, cs)
      | -- HACK: simplifying the indexing of an N-array concatenation
        -- is going to produce an N-deep if expression, which is bad
        -- when N is large.  To try to avoid that, we use the
        -- heuristic not to simplify as long as any of the operands
        -- are themselves Concats.  The hope it that this will give
        -- simplification some time to cut down the concatenation to
        -- something smaller, before we start inlining.
        not $ any isConcat $ x : xs,
        Just (ibef, DimFix i, iaft) <- focusNth d inds,
        Just (Prim res_t) <-
          (`setArrayDims` sliceDims (Slice inds))
            <$> ST.lookupType x vtable -> Just $ do
          x_len <- arraySize d <$> lookupType x
          xs_lens <- mapM (fmap (arraySize d) . lookupType) xs

          let add n m = do
                added <- letSubExp "index_concat_add" $ BasicOp $ BinOp (Add Int64 OverflowWrap) n m
                pure (added, n)
          (_, starts) <- mapAccumLM add x_len xs_lens
          let xs_and_starts = reverse $ zip xs starts

          let mkBranch [] =
                letSubExp "index_concat" $ BasicOp $ Index x $ Slice $ ibef ++ DimFix i : iaft
              mkBranch ((x', start) : xs_and_starts') = do
                cmp <- letSubExp "index_concat_cmp" $ BasicOp $ CmpOp (CmpSle Int64) start i
                (thisres, thisstms) <- collectStms $ do
                  i' <- letSubExp "index_concat_i" $ BasicOp $ BinOp (Sub Int64 OverflowWrap) i start
                  letSubExp "index_concat" . BasicOp . Index x' $
                    Slice (ibef ++ DimFix i' : iaft)
                thisbody <- mkBodyM thisstms [subExpRes thisres]
                (altres, altstms) <- collectStms $ mkBranch xs_and_starts'
                altbody <- mkBodyM altstms [subExpRes altres]
                certifying cs . letSubExp "index_concat_branch" $
                  Match [cmp] [Case [Just $ BoolValue True] thisbody] altbody $
                    MatchDec [primBodyType res_t] MatchNormal
          SubExpResult mempty <$> mkBranch xs_and_starts
    Just (ArrayLit ses _, cs)
      | DimFix (Constant (IntValue (Int64Value i))) : inds' <- inds,
        Just se <- maybeNth i ses ->
          case inds' of
            [] -> Just $ pure $ SubExpResult cs se
            _ | Var v2 <- se -> Just $ pure $ IndexResult cs v2 $ Slice inds'
            _ -> Nothing
    Just (Update Unsafe _ (Slice update_inds) se, cs)
      | inds == update_inds,
        ST.subExpAvailable se vtable ->
          Just $ pure $ SubExpResult cs se
    -- Indexing single-element arrays.  We know the index must be 0.
    _
      | Just t <- seType $ Var idd,
        isCt1 $ arraySize 0 t,
        DimFix i : inds' <- inds,
        not $ isCt0 i ->
          Just . pure . IndexResult mempty idd . Slice $
            DimFix (constant (0 :: Int64)) : inds'
    _ -> Nothing
  where
    defOf v = do
      (BasicOp op, def_cs) <- ST.lookupExp v vtable
      pure (op, def_cs)
    worthInlining e
      | primExpSizeAtLeast 20 e = False -- totally ad-hoc.
      | otherwise = worthInlining' e
    worthInlining' (BinOpExp Pow {} _ _) = False
    worthInlining' (BinOpExp FPow {} _ _) = False
    worthInlining' (BinOpExp _ x y) = worthInlining' x && worthInlining' y
    worthInlining' (CmpOpExp _ x y) = worthInlining' x && worthInlining' y
    worthInlining' (ConvOpExp _ x) = worthInlining' x
    worthInlining' (UnOpExp _ x) = worthInlining' x
    worthInlining' FunExp {} = False
    worthInlining' _ = True

    isConcat v
      | Just (Concat {}, _) <- defOf v =
          True
      | otherwise =
          False

-- Note [Simplifying a Slice]
--
-- The 'indexOp' simplification machinery permits simplification of
-- full indexing (i.e., where every component of the Slice is a
-- DimFix). We use this in a creative way to also simplify slices.
-- For example, for a slice
--
--   A[i:j:+n]
--
-- we synthesize a uniquely recognizable index expression "ie" (see
-- 'fakeIndices'), we use `indexOp` to simplify the full indexing
--
--   A[ie]
--
-- and if that produces a simplification result
--
--   B[ie]
--
-- then we can replace the "ie" DimFix with our original slice and
-- produce
--
--   B[i:j:+n]
--
-- While the above case is trivial, this is useful for cases that
-- intermix indexing and slicing. We must be careful, however: If we
-- have an original expression
--
--   A[i0:j0:+n0,i1:j1:+n1]
--
-- for which we then synthesize the expression
--
--   A[ie0, ie1]
--
-- then if we receive back the result
--
--   B[ie1, ie0]
--
-- we cannot just replace the indexes with the original slices, as
-- that would change the shape (and semantics) of the result:
--
--   B[i1:j1:+n1,i0:j0:+n0]
--
-- In such cases we must actually insert a Rearrange operation to move
-- the dimensions of the result appropriately.
