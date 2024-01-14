-- | Generating metadata so that programs can run at all.
module Futhark.Internalise.Entry
  ( entryPoint,
    VisibleTypes,
    visibleTypes,
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (find, intersperse)
import Data.Map qualified as M
import Futhark.IR qualified as I
import Futhark.Internalise.TypesValues (internaliseSumTypeRep, internalisedTypeSize)
import Futhark.Util (chunks)
import Futhark.Util.Pretty (prettyTextOneLine)
import Language.Futhark qualified as E hiding (TypeArg)
import Language.Futhark.Core (Name, Uniqueness (..), VName, nameFromText)
import Language.Futhark.Semantic qualified as E

-- | The types that are visible to the outside world.
newtype VisibleTypes = VisibleTypes [E.TypeBind]

-- | Retrieve those type bindings that should be visible to the
-- outside world.  Currently that is everything at top level that does
-- not have type parameters.
visibleTypes :: E.Imports -> VisibleTypes
visibleTypes = VisibleTypes . foldMap (modTypes . snd)
  where
    modTypes = progTypes . E.fileProg
    progTypes = foldMap decTypes . E.progDecs
    decTypes (E.TypeDec tb) = [tb]
    decTypes _ = []

findType :: VName -> VisibleTypes -> Maybe (E.TypeExp E.Info VName)
findType v (VisibleTypes ts) = E.typeExp <$> find ((== v) . E.typeAlias) ts

valueType :: I.TypeBase I.Rank Uniqueness -> I.ValueType
valueType (I.Prim pt) = I.ValueType I.Signed (I.Rank 0) pt
valueType (I.Array pt rank _) = I.ValueType I.Signed rank pt
valueType I.Acc {} = error "valueType Acc"
valueType I.Mem {} = error "valueType Mem"

withoutDims :: E.TypeExp E.Info VName -> (Int, E.TypeExp E.Info VName)
withoutDims (E.TEArray _ te _) =
  let (d, te') = withoutDims te
   in (d + 1, te')
withoutDims te = (0 :: Int, te)

rootType :: E.TypeExp E.Info VName -> E.TypeExp E.Info VName
rootType (E.TEApply te E.TypeArgExpSize {} _) = rootType te
rootType (E.TEUnique te _) = rootType te
rootType (E.TEDim _ te _) = rootType te
rootType te = te

typeExpOpaqueName :: E.TypeExp E.Info VName -> Name
typeExpOpaqueName = nameFromText . f
  where
    f = g . rootType
    g (E.TEArray _ te _) =
      let (d, te') = withoutDims te
       in mconcat (replicate (1 + d) "[]") <> f te'
    g (E.TETuple tes _) =
      "(" <> mconcat (intersperse ", " (map f tes)) <> ")"
    g (E.TERecord tes _) =
      "{" <> mconcat (intersperse ", " (map onField tes)) <> "}"
      where
        onField (k, te) = E.nameToText k <> ":" <> f te
    g (E.TESum cs _) =
      mconcat (intersperse " | " (map onConstr cs))
      where
        onConstr (k, tes) =
          E.nameToText k <> ":" <> mconcat (intersperse " " (map f tes))
    g (E.TEParens te _) = "(" <> f te <> ")"
    g te = prettyTextOneLine te

type GenOpaque = State I.OpaqueTypes

runGenOpaque :: GenOpaque a -> (a, I.OpaqueTypes)
runGenOpaque = flip runState mempty

addType :: Name -> I.OpaqueType -> GenOpaque ()
addType name t = modify $ \(I.OpaqueTypes ts) ->
  case find ((== name) . fst) ts of
    Just (_, t')
      | t /= t' ->
          error $ "Duplicate definition of entry point type " <> E.prettyString name
    _ -> I.OpaqueTypes ts <> I.OpaqueTypes [(name, t)]

isRecord :: VisibleTypes -> E.TypeExp E.Info VName -> Maybe (M.Map Name (E.TypeExp E.Info VName))
isRecord _ (E.TERecord fs _) = Just $ M.fromList fs
isRecord _ (E.TETuple fs _) = Just $ E.tupleFields fs
isRecord types (E.TEVar v _) = isRecord types =<< findType (E.qualLeaf v) types
isRecord _ _ = Nothing

recordFields ::
  VisibleTypes ->
  M.Map Name E.StructType ->
  Maybe (E.TypeExp E.Info VName) ->
  [(Name, E.EntryType)]
recordFields types fs t =
  case isRecord types . rootType =<< t of
    Just e_fs ->
      zipWith f (E.sortFields fs) (E.sortFields e_fs)
      where
        f (k, f_t) (_, e_f_t) = (k, E.EntryType f_t $ Just e_f_t)
    Nothing ->
      map (fmap (`E.EntryType` Nothing)) $ E.sortFields fs

opaqueRecord ::
  VisibleTypes ->
  [(Name, E.EntryType)] ->
  [I.TypeBase I.Rank Uniqueness] ->
  GenOpaque [(Name, I.EntryPointType)]
opaqueRecord _ [] _ = pure []
opaqueRecord types ((f, t) : fs) ts = do
  let (f_ts, ts') = splitAt (internalisedTypeSize $ E.entryType t) ts
  f' <- opaqueField t f_ts
  ((f, f') :) <$> opaqueRecord types fs ts'
  where
    opaqueField e_t i_ts = snd <$> entryPointType types e_t i_ts

isSum :: VisibleTypes -> E.TypeExp E.Info VName -> Maybe (M.Map Name [E.TypeExp E.Info VName])
isSum _ (E.TESum cs _) = Just $ M.fromList cs
isSum types (E.TEVar v _) = isSum types =<< findType (E.qualLeaf v) types
isSum _ _ = Nothing

sumConstrs ::
  VisibleTypes ->
  M.Map Name [E.StructType] ->
  Maybe (E.TypeExp E.Info VName) ->
  [(Name, [E.EntryType])]
sumConstrs types cs t =
  case isSum types . rootType =<< t of
    Just e_cs ->
      zipWith f (E.sortConstrs cs) (E.sortConstrs e_cs)
      where
        f (k, c_ts) (_, e_c_ts) = (k, zipWith E.EntryType c_ts $ map Just e_c_ts)
    Nothing ->
      map (fmap (map (`E.EntryType` Nothing))) $ E.sortConstrs cs

opaqueSum ::
  VisibleTypes ->
  [(Name, ([E.EntryType], [Int]))] ->
  [I.TypeBase I.Rank Uniqueness] ->
  GenOpaque [(Name, [(I.EntryPointType, [Int])])]
opaqueSum types cs ts = mapM (traverse f) cs
  where
    f (ets, is) = do
      let ns = map (internalisedTypeSize . E.entryType) ets
          is' = chunks ns is
      ets' <- map snd <$> zipWithM (entryPointType types) ets (map (map (ts !!)) is')
      pure $ zip ets' $ map (map (+ 1)) is' -- Adjust for tag.

entryPointType ::
  VisibleTypes ->
  E.EntryType ->
  [I.TypeBase I.Rank Uniqueness] ->
  GenOpaque (Uniqueness, I.EntryPointType)
entryPointType types t ts
  | E.Scalar (E.Prim E.Unsigned {}) <- E.entryType t,
    [I.Prim ts0] <- ts =
      pure (u, I.TypeTransparent $ I.ValueType I.Unsigned (I.Rank 0) ts0)
  | E.Array _ _ (E.Prim E.Unsigned {}) <- E.entryType t,
    [I.Array ts0 r _] <- ts =
      pure (u, I.TypeTransparent $ I.ValueType I.Unsigned r ts0)
  | E.Scalar E.Prim {} <- E.entryType t,
    [I.Prim ts0] <- ts =
      pure (u, I.TypeTransparent $ I.ValueType I.Signed (I.Rank 0) ts0)
  | E.Array _ _ E.Prim {} <- E.entryType t,
    [I.Array ts0 r _] <- ts =
      pure (u, I.TypeTransparent $ I.ValueType I.Signed r ts0)
  | otherwise = do
      case E.entryType t of
        E.Scalar (E.Record fs)
          | not $ null fs ->
              let fs' = recordFields types fs $ E.entryAscribed t
               in addType desc . I.OpaqueRecord =<< opaqueRecord types fs' ts
        E.Scalar (E.Sum cs) -> do
          let (_, places) = internaliseSumTypeRep cs
              cs' = sumConstrs types cs $ E.entryAscribed t
              cs'' = zip (map fst cs') (zip (map snd cs') (map snd places))
          addType desc . I.OpaqueSum (map valueType ts)
            =<< opaqueSum types cs'' (drop 1 ts)
        _ -> addType desc $ I.OpaqueType $ map valueType ts
      pure (u, I.TypeOpaque desc)
  where
    u = foldl max Nonunique $ map I.uniqueness ts
    desc =
      maybe (nameFromText $ prettyTextOneLine t') typeExpOpaqueName $
        E.entryAscribed t
    t' = E.noSizes (E.entryType t) `E.setUniqueness` Nonunique

entryPoint ::
  VisibleTypes ->
  Name ->
  [(E.EntryParam, [I.Param I.DeclType])] ->
  ( E.EntryType,
    [[I.TypeBase I.Rank I.Uniqueness]]
  ) ->
  (I.EntryPoint, I.OpaqueTypes)
entryPoint types name params (eret, crets) =
  runGenOpaque $
    (name,,)
      <$> mapM onParam params
      <*> ( map (uncurry I.EntryResult)
              <$> case ( E.isTupleRecord $ E.entryType eret,
                         E.entryAscribed eret
                       ) of
                (Just ts, Just (E.TETuple e_ts _)) ->
                  zipWithM (entryPointType types) (zipWith E.EntryType ts (map Just e_ts)) crets
                (Just ts, Nothing) ->
                  zipWithM (entryPointType types) (map (`E.EntryType` Nothing) ts) crets
                _ ->
                  pure <$> entryPointType types eret (concat crets)
          )
  where
    onParam (E.EntryParam e_p e_t, ps) =
      uncurry (I.EntryParam e_p)
        <$> entryPointType types e_t (map (I.rankShaped . I.paramDeclType) ps)
