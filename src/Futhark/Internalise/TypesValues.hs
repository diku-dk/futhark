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

    -- * Internalising values
    internalisePrimValue,

    -- * For internal testing
    inferAliases,
    internaliseConstructors,
  )
where

import Control.Monad.State
import Data.Bitraversable (bitraverse)
import Data.List (delete, find, foldl')
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Trace
import Futhark.IR.SOACS as I
import Futhark.Internalise.Monad
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
  InternaliseM [[[I.TypeBase Shape Uniqueness]]]
internaliseParamTypes ts =
  mapM (mapM (mapM mkAccCerts)) . runInternaliseTypeM $
    mapM (fmap (map (map onType)) . internaliseTypeM mempty) ts
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
  map fst . fixupKnownTypes ts . map (,RetAls mempty mempty) . concatMap concat
    <$> internaliseParamTypes [et]

-- Tag every sublist with its offset in corresponding flattened list.
withOffsets :: [[a]] -> [([a], Int)]
withOffsets xs = zip xs (scanl (+) 0 $ map length xs)

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (== x) xs

inferAliases ::
  [[I.TypeBase Shape Uniqueness]] ->
  [[I.TypeBase ExtShape Uniqueness]] ->
  [[(I.TypeBase ExtShape Uniqueness, RetAls)]]
inferAliases all_param_ts all_res_ts =
  map onRes all_res_ts
  where
    nonuniqueArray t@Array {} = not $ unique t
    nonuniqueArray _ = False
    unAlias t als
      | Array {} <- t, not $ unique t = als
      | otherwise = []
    doAlias res_ts (param_ts, o)
      | length res_ts > 1,
        length res_ts == length param_ts,
        Just True <- allEq <$> zipWithM canComeFrom param_ts res_ts =
          zipWith unAlias res_ts $ zipWith unAlias param_ts $ map pure [o ..]
      | length res_ts == 1 =
          if length param_ts > 1
            then
              [ unAlias res_t $
                  map snd (filter ((`canBeProjectedFrom` res_t) . fst) $ filter (nonuniqueArray . fst) $ zip param_ts [o ..])
                | res_t <- res_ts
              ]
            else
              [ unAlias res_t $
                  map snd (filter (isJust . (`canComeFrom` res_t) . fst) $ filter (nonuniqueArray . fst) $ zip param_ts [o ..])
                | res_t <- res_ts
              ]
      | otherwise = []
      where
        canComeFrom (Array pt1 shape1 _) (Array pt2 shape2 _)
          | pt1 == pt2 =
              Just $ shapeRank shape2 - shapeRank shape1
        canComeFrom _ _ = Nothing
        canBeProjectedFrom (Array pt1 shape1 _) (Array pt2 _ _) =
          pt1 == pt2 && shapeRank shape1 > 1
        canBeProjectedFrom _ _ = False
    infer ts all_ts =
      map concat . L.transpose . (map (const []) ts :) . map (doAlias ts) $
        withOffsets all_ts
    pclasses res_ts = infer res_ts $ map staticShapes all_param_ts
    rclasses res_ts = infer res_ts all_res_ts
    onRes res_ts =
      zip res_ts $ zipWith RetAls (pclasses res_ts) (rclasses res_ts)

internaliseReturnType ::
  [[I.TypeBase Shape Uniqueness]] ->
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
  [[I.TypeBase Shape Uniqueness]] ->
  E.StructRetType ->
  [[(I.TypeBase ExtShape Uniqueness, RetAls)]]
internaliseEntryReturnType paramts (E.RetType dims et) =
  inferAliases paramts $
    runInternaliseTypeM' dims . fmap concat . mapM (internaliseTypeM exts) $
      case E.isTupleRecord et of
        Just ets | not $ null ets -> ets
        _ -> [et]
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
  runInternaliseTypeM . internaliseTypeM mempty

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
    E.SizeExpr (E.IntLit n _ _) -> pure $ Free $ intConst I.Int64 n
    E.SizeExpr (E.Var name _ _) -> pure $ namedDim name
    E.SizeExpr e -> error $ "Unexpected size expression: " ++ prettyString e
  where
    namedDim (E.QualName _ name)
      | Just x <- name `M.lookup` exts = I.Ext x
      | otherwise = I.Free $ I.Var name

internaliseTypeM ::
  M.Map VName Int ->
  E.StructType ->
  InternaliseTypeM [[I.TypeBase ExtShape Uniqueness]]
internaliseTypeM exts orig_t =
  case orig_t of
    E.Array _ u shape et -> do
      dims <- internaliseShape shape
      ets <- concat <$> internaliseTypeM exts (E.Scalar et)
      pure [[I.arrayOf et' (Shape dims) $ internaliseUniqueness u | et' <- ets]]
    E.Scalar (E.Prim bt) ->
      pure [[I.Prim $ internalisePrimType bt]]
    E.Scalar (E.Record ets)
      -- We map empty records to units, because otherwise arrays of
      -- unit will lose their sizes.
      | null ets -> pure [[I.Prim I.Unit]]
      | otherwise ->
          concat <$> mapM (internaliseTypeM exts . snd) (E.sortFields ets)
    E.Scalar (E.TypeVar _ u tn [E.TypeArgType arr_t])
      | baseTag (E.qualLeaf tn) <= E.maxIntrinsicTag,
        baseString (E.qualLeaf tn) == "acc" -> do
          ts <- map (fromDecl . onAccType) . concat <$> internaliseTypeM exts arr_t
          let acc_param = VName "PLACEHOLDER" 0 -- See mkAccCerts.
              acc_t = Acc acc_param (Shape [arraysSize 0 ts]) (map rowType ts) $ internaliseUniqueness u
          pure [[acc_t]]
    E.Scalar E.TypeVar {} ->
      error $ "internaliseTypeM: cannot handle type variable: " ++ prettyString orig_t
    E.Scalar E.Arrow {} ->
      error $ "internaliseTypeM: cannot handle function type: " ++ prettyString orig_t
    E.Scalar (E.Sum cs) -> do
      (ts, _) <-
        internaliseConstructors
          <$> traverse (fmap concat . mapM (internaliseTypeM exts)) cs
      pure $ [I.Prim (I.IntType I.Int8)] : ts
  where
    internaliseShape = mapM (internaliseDim exts) . E.shapeDims

    onAccType = fromMaybe bad . hasStaticShape
    bad = error $ "internaliseTypeM Acc: " ++ prettyString orig_t

-- | Only exposed for testing purposes.
internaliseConstructors ::
  M.Map Name [[I.TypeBase ExtShape Uniqueness]] ->
  ( [[I.TypeBase ExtShape Uniqueness]],
    M.Map Name (Int, [Int])
  )
internaliseConstructors cs =
  foldl' onConstructor mempty $ zip (E.sortConstrs cs) [0 ..]
  where
    onConstructor (ts, mapping) ((c, c_ts), i) =
      let (_, js, new_ts) =
            foldl' f (withOffsets (map (map fromDecl) ts), mempty, mempty) c_ts
       in (ts ++ new_ts, M.insert c (i, js) mapping)
      where
        size = sum . map length
        f (ts', js, new_ts) t
          | Just (_, j) <- find ((== map fromDecl t) . fst) ts' =
              ( delete (map fromDecl t, j) ts',
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
  bitraverse (mapM mkAccCerts . concat) pure . runInternaliseTypeM $
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
