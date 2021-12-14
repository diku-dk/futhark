{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates #-}

-- | Index simplification mechanics.
module Futhark.Optimise.Simplify.Rules.Index
  ( IndexResult (..),
    simplifyIndexing,
  )
where

import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.SymbolTable as ST
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

-- | Try to simplify an index operation.
simplifyIndexing ::
  MonadBuilder m =>
  ST.SymbolTable (Rep m) ->
  TypeLookup ->
  VName ->
  Slice SubExp ->
  Bool ->
  Maybe (m IndexResult)
simplifyIndexing vtable seType idd (Slice inds) consuming =
  case defOf idd of
    _
      | Just t <- seType (Var idd),
        Slice inds == fullSlice t [] ->
        Just $ pure $ SubExpResult mempty $ Var idd
      | Just inds' <- sliceIndices (Slice inds),
        Just (ST.Indexed cs e) <- ST.index idd inds' vtable,
        worthInlining e,
        all (`ST.elem` vtable) (unCerts cs) ->
        Just $ SubExpResult cs <$> toSubExp "index_primexp" e
      | Just inds' <- sliceIndices (Slice inds),
        Just (ST.IndexedArray cs arr inds'') <- ST.index idd inds' vtable,
        all (worthInlining . untyped) inds'',
        arr `ST.available` vtable,
        all (`ST.elem` vtable) (unCerts cs) ->
        Just $
          IndexResult cs arr . Slice . map DimFix
            <$> mapM (toSubExp "index_primexp") inds''
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
              BasicOp $ BinOp (Mul Int64 OverflowWrap) s i_stride'
          fmap (SubExpResult cs) $
            letSubExp "slice_iota" $
              BasicOp $ Iota i_n i_offset'' i_stride'' to_it

    -- A rotate cannot be simplified away if we are slicing a rotated dimension.
    Just (Rotate offsets a, cs)
      | not $ or $ zipWith rotateAndSlice offsets inds -> Just $ do
        dims <- arrayDims <$> lookupType a
        let adjustI i o d = do
              i_p_o <- letSubExp "i_p_o" $ BasicOp $ BinOp (Add Int64 OverflowWrap) i o
              letSubExp "rot_i" (BasicOp $ BinOp (SMod Int64 Unsafe) i_p_o d)
            adjust (DimFix i, o, d) =
              DimFix <$> adjustI i o d
            adjust (DimSlice i n s, o, d) =
              DimSlice <$> adjustI i o d <*> pure n <*> pure s
        IndexResult cs a . Slice <$> mapM adjust (zip3 inds offsets dims)
      where
        rotateAndSlice r DimSlice {} = not $ isCt0 r
        rotateAndSlice _ _ = False
    Just (Index aa ais, cs) ->
      Just $
        IndexResult cs aa
          <$> subExpSlice (sliceSlice (primExpSlice ais) (primExpSlice (Slice inds)))
    Just (Replicate (Shape [_]) (Var vv), cs)
      | [DimFix {}] <- inds,
        not consuming,
        ST.available vv vtable ->
        Just $ pure $ SubExpResult cs $ Var vv
      | DimFix {} : is' <- inds,
        not consuming,
        ST.available vv vtable ->
        Just $ pure $ IndexResult cs vv $ Slice is'
    Just (Replicate (Shape [_]) val@(Constant _), cs)
      | [DimFix {}] <- inds, not consuming -> Just $ pure $ SubExpResult cs val
    Just (Replicate (Shape ds) v, cs)
      | (ds_inds, rest_inds) <- splitAt (length ds) inds,
        (ds', ds_inds') <- unzip $ mapMaybe index ds_inds,
        ds' /= ds ->
        Just $ do
          arr <- letExp "smaller_replicate" $ BasicOp $ Replicate (Shape ds') v
          return $ IndexResult cs arr $ Slice $ ds_inds' ++ rest_inds
      where
        index DimFix {} = Nothing
        index (DimSlice _ n s) = Just (n, DimSlice (constant (0 :: Int64)) n s)
    Just (Rearrange perm src, cs)
      | rearrangeReach perm <= length (takeWhile isIndex inds) ->
        let inds' = rearrangeShape (rearrangeInverse perm) inds
         in Just $ pure $ IndexResult cs src $ Slice inds'
      where
        isIndex DimFix {} = True
        isIndex _ = False
    Just (Copy src, cs)
      | Just dims <- arrayDims <$> seType (Var src),
        length inds == length dims,
        -- It is generally not safe to simplify a slice of a copy,
        -- because the result may be used in an in-place update of the
        -- original.  But we know this can only happen if the original
        -- is bound the same depth as we are!
        all (isJust . dimFix) inds
          || maybe True ((ST.loopDepth vtable /=) . ST.entryDepth) (ST.lookup src vtable),
        not consuming,
        ST.available src vtable ->
        Just $ pure $ IndexResult cs src $ Slice inds
    Just (Reshape newshape src, cs)
      | Just newdims <- shapeCoercion newshape,
        Just olddims <- arrayDims <$> seType (Var src),
        changed_dims <- zipWith (/=) newdims olddims,
        not $ or $ drop (length inds) changed_dims ->
        Just $ pure $ IndexResult cs src $ Slice inds
      | Just newdims <- shapeCoercion newshape,
        Just olddims <- arrayDims <$> seType (Var src),
        length newshape == length inds,
        length olddims == length newdims ->
        Just $ pure $ IndexResult cs src $ Slice inds
    Just (Reshape [_] v2, cs)
      | Just [_] <- arrayDims <$> seType (Var v2) ->
        Just $ pure $ IndexResult cs v2 $ Slice inds
    Just (Concat d x xs _, cs)
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
              return (added, n)
        (_, starts) <- mapAccumLM add x_len xs_lens
        let xs_and_starts = reverse $ zip xs starts

        let mkBranch [] =
              letSubExp "index_concat" $ BasicOp $ Index x $ Slice $ ibef ++ DimFix i : iaft
            mkBranch ((x', start) : xs_and_starts') = do
              cmp <- letSubExp "index_concat_cmp" $ BasicOp $ CmpOp (CmpSle Int64) start i
              (thisres, thisstms) <- collectStms $ do
                i' <- letSubExp "index_concat_i" $ BasicOp $ BinOp (Sub Int64 OverflowWrap) i start
                letSubExp "index_concat" . BasicOp . Index x' $
                  Slice $ ibef ++ DimFix i' : iaft
              thisbody <- mkBodyM thisstms [subExpRes thisres]
              (altres, altstms) <- collectStms $ mkBranch xs_and_starts'
              altbody <- mkBodyM altstms [subExpRes altres]
              letSubExp "index_concat_branch" $
                If cmp thisbody altbody $
                  IfDec [primBodyType res_t] IfNormal
        SubExpResult cs <$> mkBranch xs_and_starts
    Just (ArrayLit ses _, cs)
      | DimFix (Constant (IntValue (Int64Value i))) : inds' <- inds,
        Just se <- maybeNth i ses ->
        case inds' of
          [] -> Just $ pure $ SubExpResult cs se
          _ | Var v2 <- se -> Just $ pure $ IndexResult cs v2 $ Slice inds'
          _ -> Nothing
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
      return (op, def_cs)
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
