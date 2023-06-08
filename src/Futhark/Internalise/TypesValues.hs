module Futhark.Internalise.TypesValues
  ( -- * Internalising types
    internaliseReturnType,
    internaliseCoerceType,
    internaliseLambdaReturnType,
    internaliseEntryReturnType,
    internaliseType,
    internaliseParamTypes,
    internaliseLoopParamType,
    internalisePrimType,
    internalisedTypeSize,
    internaliseSumType,
    Tree,

    -- * Internalising values
    internalisePrimValue,

    -- * For internal testing
    inferAliases,
    internaliseConstructors,
  )
where

import Control.Monad.Free (Free (..))
import Control.Monad.State
import Data.Bitraversable (bitraverse)
import Data.Foldable (toList)
import Data.List (delete, find, foldl')
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.SOACS hiding (Free)
import Futhark.IR.SOACS qualified as I
import Futhark.Internalise.Monad
import Futhark.Util (chunkLike, chunks, nubOrd)
import Language.Futhark qualified as E

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

newtype TypeState = TypeState {typeCounter :: Int}

newtype InternaliseTypeM a
  = InternaliseTypeM (State TypeState a)
  deriving (Functor, Applicative, Monad, MonadState TypeState)

runInternaliseTypeM :: InternaliseTypeM a -> a
runInternaliseTypeM = runInternaliseTypeM' mempty

runInternaliseTypeM' :: [VName] -> InternaliseTypeM a -> a
runInternaliseTypeM' exts (InternaliseTypeM m) = evalState m $ TypeState (length exts)

internaliseParamTypes ::
  [E.TypeBase E.Size ()] ->
  InternaliseM [[Tree (I.TypeBase Shape Uniqueness)]]
internaliseParamTypes ts =
  mapM (mapM (mapM mkAccCerts)) . runInternaliseTypeM $
    mapM (fmap (map (fmap onType)) . internaliseTypeM mempty) ts
  where
    onType = fromMaybe bad . hasStaticShape
    bad = error $ "internaliseParamTypes: " ++ prettyString ts

-- We need to fix up the arrays for any Acc return values or loop
-- parameters.  We look at the concrete types for this, since the Acc
-- parameter name in the second list will just be something we made up.
fixupKnownTypes ::
  [TypeBase shape1 u1] ->
  [(TypeBase shape2 u2, b)] ->
  [(TypeBase shape2 u2, b)]
fixupKnownTypes = zipWith fixup
  where
    fixup (Acc acc ispace ts _) (Acc _ _ _ u2, b) = (Acc acc ispace ts u2, b)
    fixup _ t = t

-- Generate proper certificates for the placeholder accumulator
-- certificates produced by internaliseType (identified with tag 0).
-- Only needed when we cannot use 'fixupKnownTypes'.
mkAccCerts :: TypeBase shape u -> InternaliseM (TypeBase shape u)
mkAccCerts (Array pt shape u) =
  pure $ Array pt shape u
mkAccCerts (Acc c shape ts u) =
  Acc <$> c' <*> pure shape <*> pure ts <*> pure u
  where
    c'
      | baseTag c == 0 = newVName "acc_cert"
      | otherwise = pure c
mkAccCerts t = pure t

internaliseLoopParamType ::
  E.TypeBase E.Size () ->
  [TypeBase shape u] ->
  InternaliseM [I.TypeBase Shape Uniqueness]
internaliseLoopParamType et ts =
  map fst . fixupKnownTypes ts . map (,()) . concatMap (concatMap toList)
    <$> internaliseParamTypes [et]

-- Tag every sublist with its offset in corresponding flattened list.
withOffsets :: Foldable a => [a b] -> [(a b, Int)]
withOffsets xs = zip xs (scanl (+) 0 $ map length xs)

ensureMutuals :: [[(a, RetAls)]] -> [[(a, RetAls)]]
ensureMutuals xs = zipWith zip (map (map fst) xs) $ chunks (map length xs) (map check als)
  where
    als = zip (concatMap (map snd) xs) [0 ..]
    check (RetAls pals rals, o) = RetAls pals rals'
      where
        rals' = nubOrd $ rals <> map snd (filter (elem o . otherRetAls . fst) als)

numberFrom :: Int -> Tree a -> Tree (a, Int)
numberFrom o = flip evalState o . f
  where
    f (Pure x) = state $ \i -> (Pure (x, i), i + 1)
    f (Free xs) = Free <$> traverse f xs

numberTrees :: [Tree a] -> [Tree (a, Int)]
numberTrees = map (uncurry $ flip numberFrom) . withOffsets

nonuniqueArray :: TypeBase shape Uniqueness -> Bool
nonuniqueArray t@Array {} = not $ unique t
nonuniqueArray _ = False

matchTrees :: Tree a -> Tree b -> Maybe (Tree (a, b))
matchTrees (Pure a) (Pure b) = Just $ Pure (a, b)
matchTrees (Free as) (Free bs)
  | length as == length bs =
      Free <$> zipWithM matchTrees as bs
matchTrees _ _ = Nothing

subtreesMatching :: Tree a -> Tree b -> [Tree (a, b)]
subtreesMatching as bs =
  case matchTrees as bs of
    Just m -> [m]
    Nothing -> case bs of
      Pure _ -> []
      Free bs' -> foldMap (subtreesMatching as) bs'

inferAliases ::
  [Tree (I.TypeBase Shape Uniqueness)] ->
  [Tree (I.TypeBase ExtShape Uniqueness)] ->
  [[(I.TypeBase ExtShape Uniqueness, RetAls)]]
inferAliases all_param_ts all_res_ts =
  ensureMutuals $ map onRes all_res_ts
  where
    all_res_ts' = numberTrees all_res_ts
    all_param_ts' = numberTrees all_param_ts
    all_res_ts_flat = foldMap toList all_res_ts'
    all_param_ts_flat = foldMap toList all_param_ts'
    aliasable_param_ts = filter (all $ nonuniqueArray . fst) all_param_ts'
    aliasable_res_ts = filter (all $ nonuniqueArray . fst) all_res_ts'
    possible res_t t =
      nonuniqueArray res_t && nonuniqueArray t && elemType res_t == elemType t
    onRes (Pure res_t) =
      -- Necessarily a non-array.
      [(res_t, RetAls mempty mempty)]
    onRes (Free res_ts) =
      [ if nonuniqueArray res_t
          then (res_t, RetAls pals rals)
          else (res_t, mempty)
        | (res_t, pals, rals) <- zip3 (toList (Free res_ts)) palss ralss
      ]
      where
        reorder [] = replicate (length (Free res_ts)) []
        reorder xs = L.transpose xs
        infer ts =
          reorder . map (toList . fmap (snd . snd)) $
            foldMap (subtreesMatching (Free res_ts)) ts
        palss = infer aliasable_param_ts
        ralss = infer aliasable_res_ts

internaliseReturnType ::
  [Tree (I.TypeBase Shape Uniqueness)] ->
  E.StructRetType ->
  [TypeBase shape u] ->
  [(I.TypeBase ExtShape Uniqueness, RetAls)]
internaliseReturnType paramts (E.RetType dims et) ts =
  fixupKnownTypes ts . concat . inferAliases paramts $
    runInternaliseTypeM' dims (internaliseTypeM exts et)
  where
    exts = M.fromList $ zip dims [0 ..]

-- | As 'internaliseReturnType', but returns components of a top-level
-- tuple type piecemeal.
internaliseEntryReturnType ::
  [Tree (I.TypeBase Shape Uniqueness)] ->
  E.StructRetType ->
  [[(I.TypeBase ExtShape Uniqueness, RetAls)]]
internaliseEntryReturnType paramts (E.RetType dims et) =
  let et' = runInternaliseTypeM' dims . mapM (internaliseTypeM exts) $
        case E.isTupleRecord et of
          Just ets | not $ null ets -> ets
          _ -> [et]
   in map concat $ chunkLike et' $ inferAliases paramts $ concat et'
  where
    exts = M.fromList $ zip dims [0 ..]

internaliseCoerceType ::
  E.StructType ->
  [TypeBase shape u] ->
  [I.TypeBase ExtShape Uniqueness]
internaliseCoerceType et ts = map fst $ internaliseReturnType [] (E.RetType [] et) ts

internaliseLambdaReturnType ::
  E.TypeBase E.Size () ->
  [TypeBase shape u] ->
  InternaliseM [I.TypeBase Shape NoUniqueness]
internaliseLambdaReturnType et ts =
  map fromDecl <$> internaliseLoopParamType et ts

internaliseType ::
  E.TypeBase E.Size () ->
  [[I.TypeBase I.ExtShape Uniqueness]]
internaliseType =
  map toList . runInternaliseTypeM . internaliseTypeM mempty

newId :: InternaliseTypeM Int
newId = do
  i <- gets typeCounter
  modify $ \s -> s {typeCounter = i + 1}
  pure i

internaliseDim ::
  M.Map VName Int ->
  E.Size ->
  InternaliseTypeM ExtSize
internaliseDim exts d =
  case d of
    E.AnySize _ -> Ext <$> newId
    E.SizeExpr (E.IntLit n _ _) -> pure $ I.Free $ intConst I.Int64 n
    E.SizeExpr (E.Var name _ _) -> pure $ namedDim name
    E.SizeExpr e -> error $ "Unexpected size expression: " ++ prettyString e
  where
    namedDim (E.QualName _ name)
      | Just x <- name `M.lookup` exts = I.Ext x
      | otherwise = I.Free $ I.Var name

-- | A tree is just an instantiation of the free monad with a list
-- monad.
--
-- The important thing is that we use it to represent the original
-- structure of arrayss, as this matters for aliasing.  Each 'Free'
-- constructor corresponds to an array dimension.  Only non-arrays
-- have a 'Pure' at the top level.
--
-- E.g. @([]i32,[]i32)@ and @[](i32,i32)@ both have the same core
-- representation, but their implications for aliasing are different.
-- We use this when inferring the 'RetAls' during internalisation.
type Tree = Free []

internaliseTypeM ::
  M.Map VName Int ->
  E.StructType ->
  InternaliseTypeM [Tree (I.TypeBase ExtShape Uniqueness)]
internaliseTypeM exts orig_t =
  case orig_t of
    E.Array _ u shape et -> do
      dims <- internaliseShape shape
      ets <- internaliseTypeM exts (E.Scalar et)
      let f et' = I.arrayOf et' (Shape dims) $ internaliseUniqueness u
      pure [Free $ map (fmap f) ets]
    E.Scalar (E.Prim bt) ->
      pure [Pure $ I.Prim $ internalisePrimType bt]
    E.Scalar (E.Record ets)
      -- We map empty records to units, because otherwise arrays of
      -- unit will lose their sizes.
      | null ets -> pure [Pure $ I.Prim I.Unit]
      | otherwise ->
          concat <$> mapM (internaliseTypeM exts . snd) (E.sortFields ets)
    E.Scalar (E.TypeVar _ u tn [E.TypeArgType arr_t])
      | baseTag (E.qualLeaf tn) <= E.maxIntrinsicTag,
        baseString (E.qualLeaf tn) == "acc" -> do
          ts <-
            foldMap (toList . fmap (fromDecl . onAccType))
              <$> internaliseTypeM exts arr_t
          let acc_param = VName "PLACEHOLDER" 0 -- See mkAccCerts.
              acc_shape = Shape [arraysSize 0 ts]
              u' = internaliseUniqueness u
              acc_t = Acc acc_param acc_shape (map rowType ts) u'
          pure [Pure acc_t]
    E.Scalar E.TypeVar {} ->
      error $ "internaliseTypeM: cannot handle type variable: " ++ prettyString orig_t
    E.Scalar E.Arrow {} ->
      error $ "internaliseTypeM: cannot handle function type: " ++ prettyString orig_t
    E.Scalar (E.Sum cs) -> do
      (ts, _) <-
        internaliseConstructors
          <$> traverse (fmap concat . mapM (internaliseTypeM exts)) cs
      pure $ Pure (I.Prim (I.IntType I.Int8)) : ts
  where
    internaliseShape = mapM (internaliseDim exts) . E.shapeDims

    onAccType = fromMaybe bad . hasStaticShape
    bad = error $ "internaliseTypeM Acc: " ++ prettyString orig_t

-- | Only exposed for testing purposes.
internaliseConstructors ::
  M.Map Name [Tree (I.TypeBase ExtShape Uniqueness)] ->
  ( [Tree (I.TypeBase ExtShape Uniqueness)],
    M.Map Name (Int, [Int])
  )
internaliseConstructors cs =
  foldl' onConstructor mempty $ zip (E.sortConstrs cs) [0 ..]
  where
    onConstructor ::
      ( [Tree (I.TypeBase ExtShape Uniqueness)],
        M.Map Name (Int, [Int])
      ) ->
      ((Name, [Tree (I.TypeBase ExtShape Uniqueness)]), Int) ->
      ( [Tree (I.TypeBase ExtShape Uniqueness)],
        M.Map Name (Int, [Int])
      )
    onConstructor (ts, mapping) ((c, c_ts), i) =
      let (_, js, new_ts) =
            foldl' f (withOffsets (map (fmap fromDecl) ts), mempty, mempty) c_ts
       in (ts ++ new_ts, M.insert c (i, js) mapping)
      where
        size = sum . map length
        f (ts', js, new_ts) t
          | Just (_, j) <- find ((== fmap fromDecl t) . fst) ts' =
              ( delete (fmap fromDecl t, j) ts',
                js ++ take (length t) [j ..],
                new_ts
              )
          | otherwise =
              ( ts',
                js ++ take (length t) [size ts + size new_ts ..],
                new_ts ++ [t]
              )

internaliseSumType ::
  M.Map Name [E.StructType] ->
  InternaliseM
    ( [I.TypeBase ExtShape Uniqueness],
      M.Map Name (Int, [Int])
    )
internaliseSumType cs =
  bitraverse (mapM mkAccCerts . foldMap toList) pure . runInternaliseTypeM $
    internaliseConstructors
      <$> traverse (fmap concat . mapM (internaliseTypeM mempty)) cs

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.TypeBase E.Size als -> Int
-- A few special cases for performance.
internalisedTypeSize (E.Scalar (E.Prim _)) = 1
internalisedTypeSize (E.Array _ _ _ (E.Prim _)) = 1
internalisedTypeSize t =
  sum $ map length $ internaliseType (t `E.setAliases` ())

-- | Convert an external primitive to an internal primitive.
internalisePrimType :: E.PrimType -> I.PrimType
internalisePrimType (E.Signed t) = I.IntType t
internalisePrimType (E.Unsigned t) = I.IntType t
internalisePrimType (E.FloatType t) = I.FloatType t
internalisePrimType E.Bool = I.Bool

-- | Convert an external primitive value to an internal primitive value.
internalisePrimValue :: E.PrimValue -> I.PrimValue
internalisePrimValue (E.SignedValue v) = I.IntValue v
internalisePrimValue (E.UnsignedValue v) = I.IntValue v
internalisePrimValue (E.FloatValue v) = I.FloatValue v
internalisePrimValue (E.BoolValue b) = I.BoolValue b
