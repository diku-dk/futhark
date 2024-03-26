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
import Data.List (delete, find, foldl')
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.SOACS hiding (Free)
import Futhark.IR.SOACS qualified as I
import Futhark.Internalise.Monad
import Futhark.Util (chunkLike)
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
  [E.ParamType] ->
  InternaliseM [[Tree (I.TypeBase Shape Uniqueness)]]
internaliseParamTypes ts =
  mapM (mapM (mapM mkAccCerts)) . runInternaliseTypeM $
    mapM (fmap (map (fmap onType)) . internaliseTypeM mempty . E.paramToRes) ts
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
  E.ParamType ->
  [TypeBase shape u] ->
  InternaliseM [I.TypeBase Shape Uniqueness]
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

-- See Note [Alias Inference].
inferAliases ::
  [Tree (I.TypeBase Shape Uniqueness)] ->
  [Tree (I.TypeBase ExtShape Uniqueness)] ->
  [[(I.TypeBase ExtShape Uniqueness, RetAls)]]
inferAliases all_param_ts all_res_ts =
  map onRes all_res_ts
  where
    all_res_ts' = numberTrees all_res_ts
    all_param_ts' = numberTrees all_param_ts
    aliasable_param_ts = filter (all $ nonuniqueArray . fst) all_param_ts'
    aliasable_res_ts = filter (all $ nonuniqueArray . fst) all_res_ts'
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
  E.ResRetType ->
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
  E.ResRetType ->
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
internaliseCoerceType et ts =
  map fst $ internaliseReturnType [] (E.RetType [] $ E.toRes E.Nonunique et) ts

internaliseLambdaReturnType ::
  E.ResType ->
  [TypeBase shape u] ->
  InternaliseM [I.TypeBase Shape NoUniqueness]
internaliseLambdaReturnType et ts =
  map fromDecl <$> internaliseLoopParamType (E.resToParam et) ts

internaliseType ::
  E.TypeBase E.Size NoUniqueness ->
  [Tree (I.TypeBase I.ExtShape Uniqueness)]
internaliseType =
  runInternaliseTypeM . internaliseTypeM mempty . E.toRes E.Nonunique

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
    e | e == E.anySize -> Ext <$> newId
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

internaliseTypeM ::
  M.Map VName Int ->
  E.ResType ->
  InternaliseTypeM [Tree (I.TypeBase ExtShape Uniqueness)]
internaliseTypeM exts orig_t =
  case orig_t of
    E.Array u shape et -> do
      dims <- internaliseShape shape
      ets <- internaliseTypeM exts $ E.toRes E.Nonunique $ E.Scalar et
      let f et' = I.arrayOf et' (Shape dims) $ internaliseUniqueness u
      pure [array $ map (fmap f) ets]
    E.Scalar (E.Prim bt) ->
      pure [Pure $ I.Prim $ internalisePrimType bt]
    E.Scalar (E.Record ets)
      -- We map empty records to units, because otherwise arrays of
      -- unit will lose their sizes.
      | null ets -> pure [Pure $ I.Prim I.Unit]
      | otherwise ->
          concat <$> mapM (internaliseTypeM exts . snd) (E.sortFields ets)
    E.Scalar (E.TypeVar u tn [E.TypeArgType arr_t])
      | baseTag (E.qualLeaf tn) <= E.maxIntrinsicTag,
        baseString (E.qualLeaf tn) == "acc" -> do
          ts <-
            foldMap (toList . fmap (fromDecl . onAccType))
              <$> internaliseTypeM exts (E.toRes Nonunique arr_t)
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
    array [Free ts] = Free ts
    array ts = Free ts

    onAccType = fromMaybe bad . hasStaticShape
    bad = error $ "internaliseTypeM Acc: " ++ prettyString orig_t

-- | Only exposed for testing purposes.
internaliseConstructors ::
  M.Map Name [Tree (I.TypeBase ExtShape Uniqueness)] ->
  ( [Tree (I.TypeBase ExtShape Uniqueness)],
    [(Name, [Int])]
  )
internaliseConstructors cs =
  L.mapAccumL onConstructor mempty $ E.sortConstrs cs
  where
    onConstructor ts (c, c_ts) =
      let (_, js, new_ts) =
            foldl' f (withOffsets (map (fmap fromDecl) ts), mempty, mempty) c_ts
       in (ts ++ new_ts, (c, js))
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

internaliseSumTypeRep ::
  M.Map Name [E.StructType] ->
  ( [I.TypeBase ExtShape Uniqueness],
    [(Name, [Int])]
  )
internaliseSumTypeRep cs =
  first (foldMap toList) . runInternaliseTypeM $
    internaliseConstructors
      <$> traverse (fmap concat . mapM (internaliseTypeM mempty . E.toRes E.Nonunique)) cs

internaliseSumType ::
  M.Map Name [E.StructType] ->
  InternaliseM
    ( [I.TypeBase ExtShape Uniqueness],
      [(Name, [Int])]
    )
internaliseSumType =
  bitraverse (mapM mkAccCerts) pure . internaliseSumTypeRep

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.TypeBase E.Size als -> Int
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
-- from the source language, where it is implicit: a non-unique
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
