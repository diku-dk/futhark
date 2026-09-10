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
    internaliseSumTypeRep,
    internaliseSumType,
    Tree,

    -- * Internalising values
    internalisePrimValue,

    -- * For internal testing
    inferAliases,
    internaliseConstructors,
  )
where

import Control.Monad
import Control.Monad.Free (Free (..))
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.Foldable (toList)
import Data.List (delete, find)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.SOACS hiding (Free)
import Futhark.IR.SOACS qualified as I
import Futhark.Internalise.Monad
import Futhark.Util (chunkLike)
import Language.Futhark qualified as E

-- | The 'E.Diet' of a source parameter becomes the 'I.Diet' of the
-- internalised parameter; the two types coincide.
internaliseDiet :: E.Diet -> I.Diet
internaliseDiet E.Observe = I.Observe
internaliseDiet E.Consume = I.Consume

-- | The mode of an array type, if it is an array. Used to ask whether aliasing
-- is permitted at a position, which is 'E.Nonfresh' for a result and
-- 'I.Observe' for a parameter.
arrayMode :: TypeBase shape o -> Maybe o
arrayMode (Array _ _ o) = Just o
arrayMode _ = Nothing

aliasableArray :: (o -> Bool) -> TypeBase shape o -> Bool
aliasableArray p = maybe False p . arrayMode

newtype TypeState = TypeState {typeCounter :: Int}

newtype InternaliseTypeM a
  = InternaliseTypeM (State TypeState a)
  deriving (Functor, Applicative, Monad, MonadState TypeState)

runInternaliseTypeM :: InternaliseTypeM a -> a
runInternaliseTypeM = runInternaliseTypeM' mempty

runInternaliseTypeM' :: [VName] -> InternaliseTypeM a -> a
runInternaliseTypeM' exts (InternaliseTypeM m) = evalState m $ TypeState (length exts)

internaliseParamTypes ::
  [E.ParamType] ->
  InternaliseM [[Tree (I.TypeBase Shape I.Diet)]]
internaliseParamTypes ts =
  mapM (mapM (mapM mkAccCerts)) . runInternaliseTypeM $
    mapM (fmap (map (fmap onType)) . internaliseTypeM mempty) ts
  where
    onType = second internaliseDiet . fromMaybe bad . hasStaticShape
    bad = error $ "internaliseParamTypes: " ++ prettyString ts

-- Replace an accumulator's token, index space, and element types with those of
-- a known accumulator type. We must do this because these components cannot be
-- recovered from a source type: 'internaliseTypeM' produces a placeholder token
-- and a guessed index space. The known type is computed elsewhere (from
-- concrete loop values, or from an accumulator parameter).
fixupAcc :: TypeBase shape1 u1 -> (TypeBase shape2 u2, b) -> (TypeBase shape2 u2, b)
fixupAcc (Acc acc ispace ts) (Acc {}, b) = (Acc acc ispace ts, b)
fixupAcc _ t = t

-- Fix up accumulators using a positionally-matching list of concrete
-- types (e.g. the actual types of loop values).
fixupKnownTypes ::
  [TypeBase shape1 o1] ->
  [(TypeBase shape2 o2, b)] ->
  [(TypeBase shape2 o2, b)]
fixupKnownTypes = zipWith fixupAcc

-- Generate proper certificates for the placeholder accumulator
-- certificates produced by internaliseType (identified with tag 0).
-- Only needed when we cannot use 'fixupKnownTypes'.
mkAccCerts :: TypeBase shape o -> InternaliseM (TypeBase shape o)
mkAccCerts (Array pt shape o) =
  pure $ Array pt shape o
mkAccCerts (Acc c shape ts) =
  Acc <$> c' <*> pure shape <*> pure ts
  where
    c'
      | baseTag c == 0 = newVName "acc_cert"
      | otherwise = pure c
mkAccCerts t = pure t

internaliseLoopParamType ::
  E.ParamType ->
  [TypeBase shape o] ->
  InternaliseM [I.TypeBase Shape Diet]
internaliseLoopParamType et ts =
  map fst . fixupKnownTypes ts . map (,()) . concatMap (concatMap toList)
    <$> internaliseParamTypes [et]

-- Tag every sublist with its offset in corresponding flattened list.
withOffsets :: (Foldable a) => [a b] -> [(a b, Int)]
withOffsets xs = zip xs (scanl (+) 0 $ map length xs)

numberFrom :: Int -> Tree a -> Tree (a, Int)
numberFrom o = flip evalState o . f
  where
    f (Pure x) = state $ \i -> (Pure (x, i), i + 1)
    f (Free xs) = Free <$> traverse f xs

numberTrees :: [Tree a] -> [Tree (a, Int)]
numberTrees = map (uncurry $ flip numberFrom) . withOffsets

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

-- See Note [Alias Inference].
inferAliases ::
  [Tree (I.TypeBase Shape I.Diet)] ->
  [Tree (I.TypeBase ExtShape E.Freshness)] ->
  [[(I.TypeBase ExtShape E.Freshness, RetAls)]]
inferAliases all_param_ts all_res_ts =
  map onRes all_res_ts
  where
    all_res_ts' = numberTrees all_res_ts
    all_param_ts' = numberTrees all_param_ts
    observed = aliasableArray (== I.Observe)
    nonfresh = aliasableArray (== E.Nonfresh)
    aliasable_param_ts = filter (all $ observed . fst) all_param_ts'
    aliasable_res_ts = filter (all $ nonfresh . fst) all_res_ts'
    onRes (Pure res_t) =
      -- Necessarily a non-array.
      [(res_t, RetAls mempty mempty)]
    onRes (Free res_ts) =
      [ if nonfresh res_t
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

-- | The mode of the source-level return type is used to compute the
-- 'RetAls', and then discarded: an IR return type says nothing about
-- aliasing on its own.
internaliseReturnType ::
  [Tree (I.TypeBase Shape I.Diet)] ->
  E.ResRetType ->
  [(I.TypeBase ExtShape NoMode, RetAls)]
internaliseReturnType paramts (E.RetType dims et) =
  map (first I.fromDecl) . fixupAccs . concat . inferAliases paramts $
    runInternaliseTypeM' dims (internaliseTypeM exts et)
  where
    exts = M.fromList $ zip dims [0 ..]
    -- Any 'Acc' in the return type must (by the type rules) be the function's
    -- single 'Acc' parameter, so we substitute its known accumulator type.
    fixupAccs = case [t | t@Acc {} <- foldMap toList paramts] of
      acc : _ -> map (fixupAcc acc)
      [] -> id

-- | As 'internaliseReturnType', but returns components of a top-level
-- tuple type piecemeal.
internaliseEntryReturnType ::
  [Tree (I.TypeBase Shape I.Diet)] ->
  E.ResRetType ->
  [[(I.TypeBase ExtShape E.Freshness, RetAls)]]
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
  [I.TypeBase ExtShape NoMode]
internaliseCoerceType et =
  map fst $ internaliseReturnType [] (E.RetType [] $ E.toRes E.Nonfresh et)

internaliseLambdaReturnType ::
  E.ResType ->
  [TypeBase shape o] ->
  InternaliseM [I.TypeBase Shape NoMode]
internaliseLambdaReturnType et ts =
  map fromDecl <$> internaliseLoopParamType (E.resToParam et) ts

internaliseType ::
  E.TypeBase E.Size NoMode ->
  [Tree (I.TypeBase I.ExtShape NoMode)]
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
    e | Just _ <- E.isAnySize e -> Ext <$> newId
    (E.IntLit n _ _) -> pure $ I.Free $ intConst I.Int64 n
    (E.Var name _ _) -> pure $ namedDim name
    e -> error $ "Unexpected size expression: " ++ prettyString e
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
-- have a 'Pure' at the top level.  See Note [Alias Inference].
type Tree = Free []

-- | Internalise a source type, preserving whatever mode it carries: a
-- 'E.Diet' for a parameter type, a 'E.Freshness' for a return type.
-- Which of the two it is matters to 'inferAliases', so it is not
-- collapsed here.
internaliseTypeM ::
  (Pretty o) =>
  M.Map VName Int ->
  E.TypeBase E.Size o ->
  InternaliseTypeM [Tree (I.TypeBase ExtShape o)]
internaliseTypeM exts orig_t =
  case orig_t of
    E.Array o shape et -> do
      dims <- internaliseShape shape
      ets <- internaliseTypeM exts $ E.Scalar et
      let f et' = I.arrayOf et' (Shape dims) o
      pure [array $ map (fmap f) ets]
    E.Scalar (E.Prim bt) ->
      pure [Pure $ I.Prim $ internalisePrimType bt]
    E.Scalar (E.Record ets)
      -- We map empty records to units, because otherwise arrays of
      -- unit will lose their sizes.
      | null ets -> pure [Pure $ I.Prim I.Unit]
      | otherwise ->
          concat <$> mapM (internaliseTypeM exts . snd) (E.sortFields ets)
    E.Scalar (E.TypeVar _ tn [E.TypeArgType arr_t])
      | E.isIntrinsic (E.qualLeaf tn),
        baseName (E.qualLeaf tn) == "acc" -> do
          ts <-
            foldMap (toList . fmap onAccType)
              <$> internaliseTypeM exts arr_t
          let acc_param = VName "PLACEHOLDER" 0 -- See mkAccCerts.
              acc_shape = Shape [arraysSize 0 ts]
              acc_t = Acc acc_param acc_shape (map rowType ts)
          pure [Pure acc_t]
    E.Scalar E.TypeVar {} ->
      error $ "internaliseTypeM: cannot handle type variable: " ++ prettyString orig_t
    E.Scalar E.Arrow {} ->
      error $ "internaliseTypeM: cannot handle function type: " ++ prettyString orig_t
    E.Scalar (E.Sum cs) -> do
      (ts, _) <-
        internaliseConstructors
          <$> traverse (fmap concat . mapM (internaliseTypeM exts)) cs
      pure $
        if length cs == 1
          then ts
          else Pure (I.Prim (I.IntType I.Int8)) : ts
  where
    internaliseShape = mapM (internaliseDim exts) . E.shapeDims
    array [Free ts] = Free ts
    array ts = Free ts

    onAccType = fromMaybe bad . hasStaticShape
    bad = error $ "internaliseTypeM Acc: " ++ prettyString orig_t

-- | Only exposed for testing purposes.
internaliseConstructors ::
  M.Map Name [Tree (I.TypeBase ExtShape o)] ->
  ( [Tree (I.TypeBase ExtShape o)],
    [(Name, [Int])]
  )
internaliseConstructors cs =
  L.mapAccumL onConstructor mempty $ E.sortConstrs cs
  where
    unmoded = fromDecl
    onConstructor ts (c, c_ts) =
      let (_, js, new_ts) =
            foldl' f (withOffsets (map (fmap unmoded) ts), mempty, mempty) c_ts
       in (ts ++ new_ts, (c, js))
      where
        size = sum . map length
        f (ts', js, new_ts) t
          | all primType t,
            Just (_, j) <- find ((== fmap unmoded t) . fst) ts' =
              ( delete (fmap unmoded t, j) ts',
                js ++ take (length t) [j ..],
                new_ts
              )
          | otherwise =
              ( ts',
                js ++ take (length t) [size ts + size new_ts ..],
                new_ts ++ [t]
              )

internaliseSumTypeRep ::
  M.Map Name [E.StructType] ->
  ( [I.TypeBase ExtShape NoMode],
    [(Name, [Int])]
  )
internaliseSumTypeRep cs =
  first (foldMap toList) . runInternaliseTypeM $
    internaliseConstructors
      <$> traverse (fmap concat . mapM (internaliseTypeM mempty)) cs

internaliseSumType ::
  M.Map Name [E.StructType] ->
  InternaliseM
    ( [I.TypeBase ExtShape NoMode],
      [(Name, [Int])]
    )
internaliseSumType =
  bitraverse (mapM mkAccCerts) pure . internaliseSumTypeRep

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.TypeBase E.Size o -> Int
-- A few special cases for performance.
internalisedTypeSize (E.Scalar (E.Prim _)) = 1
internalisedTypeSize (E.Array _ _ (E.Prim _)) = 1
internalisedTypeSize t = sum $ map length $ internaliseType $ E.toStruct t

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

-- Note [Alias Inference]
--
-- The core language requires us to precisely indicate the aliasing of
-- function results (the RetAls type).  This is a problem when coming
-- from the source language, where it is implicit: a nonfresh
-- function return value aliases every function argument.  The problem
-- now occurs because the core language uses a different value
-- representation than the source language - in particular, we do not
-- have arrays of tuples. E.g. @([]i32,[]i32)@ and @[](i32,i32)@ both
-- have the same core representation, but their implications for
-- aliasing are different.
--
--
-- To understand why this is a problem, consider a source program
--
--     def id (x: [](i32,i32)) = x
--
--     def f n =
--       let x = replicate n (0,0)
--       let x' = id x
--       let x'' = x' with [0] = (1,1)
--       in x''
--
-- With the core language value representation, it will be this:
--
--   def id (x1: []i32) (x2: []i32) = (x1,x2)
--
--   def f n =
--     let x1 = replicate n 0
--     let x2 = replicate n 0
--     let (x1', x2') = id x1 x2
--     let x1'' = x1' with [0] = 1
--     let x2'' = x2' with [0] = 1
--     in (x1'', x2'')
--
-- The results of 'id' alias *both* of the arguments, so x1' aliases
-- x1 and x2, and x2' also aliases x1 and x2.  This means that the
-- first with-expression will consume all of x1/x2/x1'/x2', and then
-- the second with-expression is a type error, as it references a
-- consumed variable.
--
-- Our solution is to deduce the possible aliasing such that
-- components that originally constituted the same array-of-tuples are
-- not aliased.  The main complexity is that we have to keep
-- information on the original (source) type structure around for a
-- while.  This is done with the Tree type.
