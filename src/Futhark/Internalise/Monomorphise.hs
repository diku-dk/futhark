-- | This monomorphization module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent monomorphic program.
--
-- This pass also does a few other simplifications to make the job of
-- subsequent passes easier.  Specifically, it does the following:
--
-- * Turn operator sections into explicit lambdas.
--
-- * Converts identifiers of record type into record patterns.
--
-- * Converts applications of intrinsic SOACs into SOAC AST nodes
--   (Map, Reduce, etc).
--
-- * Elide functions that are not reachable from an entry point (this
--   is a side effect of the monomorphisation algorithm, which uses
--   the entry points as roots).
--
-- * Turns implicit record fields into explicit record fields.
--
-- Note that these changes are unfortunately not visible in the AST
-- representation.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.Monomorphise
  ( transformProg
  , transformDecs
  , runMonoM
  ) where

import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as Seq
import           Data.Foldable

import           Futhark.MonadFreshNames
import           Language.Futhark
import           Language.Futhark.Traversals
import           Language.Futhark.TypeChecker.Monad (TypeBinding(..))
import           Language.Futhark.TypeChecker.Types

-- | The monomorphization monad reads 'PolyBinding's and writes 'ValBinding's.
-- The 'TypeParam's in a 'ValBinding' can only be shape parameters.
newtype PolyBinding = PolyBinding (VName, [TypeParam], [Pattern],
                                   Maybe (TypeExp VName), StructType, Exp, SrcLoc)

-- | Mapping from record names to the variable names that contain the
-- fields.  This is used because the monomorphiser also expands all
-- record patterns.
type RecordReplacements = M.Map VName RecordReplacement

type RecordReplacement = M.Map Name (VName, PatternType)

-- | Monomorphization environment mapping names of polymorphic functions to a
-- representation of their corresponding function bindings.
data Env = Env { envPolyBindings :: M.Map VName PolyBinding
               , envTypeBindings :: M.Map VName TypeBinding
               , envRecordReplacements :: RecordReplacements
               }

instance Sem.Semigroup Env where
  Env tb1 pb1 rr1 <> Env tb2 pb2 rr2 = Env (tb1 <> tb2) (pb1 <> pb2) (rr1 <> rr2)

instance Monoid Env where
  mempty  = Env mempty mempty mempty
  mappend = (Sem.<>)

localEnv :: Env -> MonoM a -> MonoM a
localEnv env = local (env <>)

extendEnv :: VName -> PolyBinding -> MonoM a -> MonoM a
extendEnv vn binding = localEnv
  mempty { envPolyBindings = M.singleton vn binding }

withRecordReplacements :: RecordReplacements -> MonoM a -> MonoM a
withRecordReplacements rr = localEnv mempty { envRecordReplacements = rr}

noRecordReplacements :: MonoM a -> MonoM a
noRecordReplacements = local $ \env -> env { envRecordReplacements = mempty }

-- | The monomorphization monad.
newtype MonoM a = MonoM (RWST Env (Seq.Seq (VName, ValBind)) VNameSource
                         (State Lifts) a)
  deriving (Functor, Applicative, Monad,
            MonadReader Env,
            MonadWriter (Seq.Seq (VName, ValBind)),
            MonadFreshNames)

runMonoM :: VNameSource -> MonoM a -> ((a, Seq.Seq (VName, ValBind)), VNameSource)
runMonoM src (MonoM m) = ((a, defs), src')
  where (a, src', defs) = evalState (runRWST m mempty src) mempty

lookupFun :: VName -> MonoM (Maybe PolyBinding)
lookupFun vn = do
  env <- asks envPolyBindings
  case M.lookup vn env of
    Just valbind -> return $ Just valbind
    Nothing -> return Nothing

lookupRecordReplacement :: VName -> MonoM (Maybe RecordReplacement)
lookupRecordReplacement v = asks $ M.lookup v . envRecordReplacements

-- | Mapping from function name and instance list to a new function name in case
-- the function has already been instantiated with those concrete types.
type Lifts = [((VName, TypeBase () ()), VName)]

getLifts :: MonoM Lifts
getLifts = MonoM $ lift get

modifyLifts :: (Lifts -> Lifts) -> MonoM ()
modifyLifts = MonoM . lift . modify

addLifted :: VName -> TypeBase () () -> VName -> MonoM ()
addLifted fname il lifted_fname =
  modifyLifts (((fname, il), lifted_fname) :)

lookupLifted :: VName -> TypeBase () () -> MonoM (Maybe VName)
lookupLifted fname t = lookup (fname, t) <$> getLifts

transformFName :: VName -> TypeBase () () -> MonoM VName
transformFName fname t
  | baseTag fname <= maxIntrinsicTag = return fname
  | otherwise = do
      maybe_fname <- lookupLifted fname t
      maybe_funbind <- lookupFun fname
      case (maybe_fname, maybe_funbind) of
        -- The function has already been monomorphized.
        (Just fname', _) -> return fname'
        -- An intrinsic function.
        (Nothing, Nothing) -> return fname
        -- A polymorphic function.
        (Nothing, Just funbind) -> do
          (fname', funbind') <- monomorphizeBinding funbind t
          tell $ Seq.singleton (fname, funbind')
          addLifted fname t fname'
          return fname'

-- | Monomorphization of expressions.
transformExp :: Exp -> MonoM Exp
transformExp e@Literal{} = return e
transformExp e@IntLit{} = return e
transformExp e@FloatLit{} = return e

transformExp (Parens e loc) =
  Parens <$> transformExp e <*> pure loc

transformExp (QualParens qn e loc) =
  QualParens qn <$> transformExp e <*> pure loc

transformExp (TupLit es loc) =
  TupLit <$> mapM transformExp es <*> pure loc

transformExp (RecordLit fs loc) =
  RecordLit <$> mapM transformField fs <*> pure loc
  where transformField (RecordFieldExplicit name e loc') =
          RecordFieldExplicit name <$> transformExp e <*> pure loc'
        transformField (RecordFieldImplicit v t _) =
          transformField $ RecordFieldExplicit (baseName v)
          (Var (qualName v) (vacuousShapeAnnotations <$> t) loc) loc

transformExp (ArrayLit es tp loc) =
  ArrayLit <$> mapM transformExp es <*> pure tp <*> pure loc

transformExp (Range e1 me incl tp loc) = do
  e1' <- transformExp e1
  me' <- mapM transformExp me
  incl' <- mapM transformExp incl
  return $ Range e1' me' incl' tp loc

transformExp (Var (QualName qs fname) (Info t) loc) = do
  maybe_fs <- lookupRecordReplacement fname
  case maybe_fs of
    Just fs -> do
      let toField (f, (f_v, f_t)) =
            let f_v' = Var (qualName f_v) (Info $ vacuousShapeAnnotations f_t) loc
            in RecordFieldExplicit f f_v' loc
      return $ RecordLit (map toField $ M.toList fs) loc
    Nothing -> do
      fname' <- transformFName fname (toStructural t)
      return $ Var (QualName qs fname') (Info t) loc

transformExp (Ascript e tp loc) =
  Ascript <$> transformExp e <*> pure tp <*> pure loc

transformExp (LetPat tparams pat e1 e2 loc) = do
  (pat', rr) <- expandRecordPattern pat
  LetPat tparams pat' <$> transformExp e1 <*>
    withRecordReplacements rr (transformExp e2) <*> pure loc

transformExp (LetFun fname (tparams, params, retdecl, Info ret, body) e loc)
  | any isTypeParam tparams = do
      -- Retrieve the lifted monomorphic function bindings that are produced,
      -- filter those that are monomorphic versions of the current let-bound
      -- function and insert them at this point, and propagate the rest.
      let funbind = PolyBinding (fname, tparams, params, retdecl, ret, body, loc)
      pass $ do
        (e', bs) <- listen $ extendEnv fname funbind $ transformExp e
        let (bs_local, bs_prop) = Seq.partition ((== fname) . fst) bs
        return (unfoldLetFuns (map snd $ toList bs_local) e', const bs_prop)

  | otherwise =
      transformExp $ LetPat [] (Id fname (Info ft) loc) lam e loc
        where lam = Lambda tparams params body Nothing (Info (mempty, ret)) loc
              ft = foldFunType (map (vacuousShapeAnnotations . patternType) params) $ fromStruct ret

transformExp (If e1 e2 e3 tp loc) = do
  e1' <- transformExp e1
  e2' <- transformExp e2
  e3' <- transformExp e3
  return $ If e1' e2' e3' tp loc

transformExp (Apply e1 e2 d tp loc) =
  -- We handle on an ad-hoc basis certain polymorphic higher-order
  -- intrinsics here.  They can only be used in very particular ways,
  -- or the compiler will fail.  In practice they will only be used
  -- once, in the basis library, to define normal functions.
  case (e1, e2) of
    (Var v _ _, TupLit [op, ne, arr] _)
      | intrinsic "reduce" v ->
          transformExp $ Reduce Noncommutative op ne arr loc
      | intrinsic "reduce_comm" v ->
          transformExp $ Reduce Commutative op ne arr loc
      | intrinsic "scan" v ->
          transformExp $ Scan op ne arr loc
    (Var v _ _, TupLit [f, arr] _)
      | intrinsic "map" v ->
          transformExp $ Map f arr (removeShapeAnnotations <$> tp) loc
      | intrinsic "filter" v ->
          transformExp $ Filter f arr loc
    (Var v _ _, TupLit [k, f, arr] _)
      | intrinsic "partition" v,
        Just k' <- isInt32 k ->
          transformExp $ Partition (fromIntegral k') f arr loc
    (Var v _ _, TupLit [op, f, arr] _)
      | intrinsic "stream_red" v ->
          transformExp $ Stream (RedLike InOrder Noncommutative op) f arr loc
      | intrinsic "stream_red_per" v ->
          transformExp $ Stream (RedLike Disorder Commutative op) f arr loc
    (Var v _ _, TupLit [f, arr] _)
      | intrinsic "stream_map" v ->
          transformExp $ Stream (MapLike InOrder) f arr loc
      | intrinsic "stream_map_per" v ->
          transformExp $ Stream (MapLike Disorder) f arr loc

    _ -> do
      e1' <- transformExp e1
      e2' <- transformExp e2
      return $ Apply e1' e2' d tp loc
  where intrinsic s (QualName _ v) =
          baseTag v <= maxIntrinsicTag && baseName v == nameFromString s

        isInt32 (Literal (SignedValue (Int32Value k)) _) = Just k
        isInt32 (IntLit k (Info (Prim (Signed Int32))) _) = Just $ fromInteger k
        isInt32 _ = Nothing

transformExp (Negate e loc) =
  Negate <$> transformExp e <*> pure loc

transformExp (Lambda tparams params e0 decl tp loc) = do
  e0' <- transformExp e0
  return $ Lambda tparams params e0' decl tp loc

transformExp (OpSection qn t loc) =
  transformExp $ Var qn t loc

transformExp (OpSectionLeft (QualName qs fname) (Info t) e
               (Info xtype, Info ytype) (Info rettype) loc) = do
  fname' <- transformFName fname (toStructural t)
  e' <- transformExp e
  desugarBinOpSection (QualName qs fname') (Just e') Nothing t xtype ytype rettype loc

transformExp (OpSectionRight (QualName qs fname) (Info t) e
               (Info xtype, Info ytype) (Info rettype) loc) = do
  fname' <- transformFName fname (toStructural t)
  e' <- transformExp e
  desugarBinOpSection (QualName qs fname') Nothing (Just e') t xtype ytype rettype loc

transformExp (ProjectSection fields (Info t) loc) =
  desugarProjectSection fields t loc

transformExp (IndexSection idxs (Info t) loc) =
  desugarIndexSection idxs t loc

transformExp (DoLoop tparams pat e1 form e3 loc) = do
  e1' <- transformExp e1
  form' <- case form of
    For ident e2  -> For ident <$> transformExp e2
    ForIn pat2 e2 -> ForIn pat2 <$> transformExp e2
    While e2      -> While <$> transformExp e2
  e3' <- transformExp e3
  return $ DoLoop tparams pat e1' form' e3' loc

transformExp (BinOp (QualName qs fname) (Info t) (e1, d1) (e2, d2) tp loc) = do
  fname' <- transformFName fname (toStructural t)
  e1' <- transformExp e1
  e2' <- transformExp e2
  return $ BinOp (QualName qs fname') (Info t) (e1', d1) (e2', d2) tp loc

transformExp (Project n e tp loc) = do
  maybe_fs <- case e of
    Var qn _ _ -> lookupRecordReplacement (qualLeaf qn)
    _          -> return Nothing
  case maybe_fs of
    Just m | Just (v, _) <- M.lookup n m ->
               return $ Var (qualName v) (vacuousShapeAnnotations <$> tp) loc
    _ -> do
      e' <- transformExp e
      return $ Project n e' tp loc

transformExp (LetWith id1 id2 idxs e1 body loc) = do
  idxs' <- mapM transformDimIndex idxs
  e1' <- transformExp e1
  body' <- transformExp body
  return $ LetWith id1 id2 idxs' e1' body' loc

transformExp (Index e0 idxs info loc) =
  Index <$> transformExp e0 <*> mapM transformDimIndex idxs <*> pure info <*> pure loc

transformExp (Update e1 idxs e2 loc) =
  Update <$> transformExp e1 <*> mapM transformDimIndex idxs
         <*> transformExp e2 <*> pure loc

transformExp (Map e1 es t loc) =
  Map <$> transformExp e1 <*> transformExp es <*> pure t <*> pure loc

transformExp (Reduce comm e1 e2 e3 loc) =
  Reduce comm <$> transformExp e1 <*> transformExp e2
              <*> transformExp e3 <*> pure loc

transformExp (Scan e1 e2 e3 loc) =
  Scan <$> transformExp e1 <*> transformExp e2 <*> transformExp e3 <*> pure loc

transformExp (Filter e1 e2 loc) =
  Filter <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Partition k f e0 loc) =
  Partition k <$> transformExp f <*> transformExp e0 <*> pure loc

transformExp (Stream form e1 e2 loc) = do
  form' <- case form of
             MapLike _         -> return form
             RedLike so comm e -> RedLike so comm <$> transformExp e
  Stream form' <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Zip i e1 es t loc) = do
  e1' <- transformExp e1
  es' <- mapM transformExp es
  return $ Zip i e1' es' t loc

transformExp (Unzip e0 tps loc) =
  Unzip <$> transformExp e0 <*> pure tps <*> pure loc

transformExp (Unsafe e1 loc) =
  Unsafe <$> transformExp e1 <*> pure loc

transformExp (Assert e1 e2 desc loc) =
  Assert <$> transformExp e1 <*> transformExp e2 <*> pure desc <*> pure loc

transformDimIndex :: DimIndexBase Info VName -> MonoM (DimIndexBase Info VName)
transformDimIndex (DimFix e) = DimFix <$> transformExp e
transformDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> trans me1 <*> trans me2 <*> trans me3
  where trans = mapM transformExp

-- | Transform an operator section into a lambda.
desugarBinOpSection :: QualName VName -> Maybe Exp -> Maybe Exp
                 -> PatternType -> StructType -> StructType -> PatternType -> SrcLoc -> MonoM Exp
desugarBinOpSection qn e_left e_right t xtype ytype rettype loc = do
  (e1, p1) <- makeVarParam e_left $ fromStruct xtype
  (e2, p2) <- makeVarParam e_right $ fromStruct ytype
  let body = BinOp qn (Info t) (e1, Info xtype) (e2, Info ytype) (Info rettype) loc
      rettype' = vacuousShapeAnnotations $ toStruct rettype
  return $ Lambda [] (p1 ++ p2) body Nothing (Info (mempty, rettype')) loc

  where makeVarParam (Just e) _ = return (e, [])
        makeVarParam Nothing argtype = do
          x <- newNameFromString "x"
          return (Var (qualName x) (Info argtype) noLoc,
                  [Id x (Info $ fromStruct argtype) noLoc])

desugarProjectSection :: [Name] -> PatternType -> SrcLoc -> MonoM Exp
desugarProjectSection fields (Arrow _ _ t1 t2) loc = do
  p <- newVName "project_p"
  let body = foldl project (Var (qualName p) (Info t1) noLoc) fields
  return $ Lambda [] [Id p (Info t1) noLoc] body Nothing (Info (mempty, toStruct t2)) loc
  where project e field =
          case typeOf e of
            Record fs | Just t <- M.lookup field fs ->
                          Project field e (Info t) noLoc
            t -> error $ "desugarOpSection: type " ++ pretty t ++
                 " does not have field " ++ pretty field
desugarProjectSection  _ t _ = error $ "desugarOpSection: not a function type: " ++ pretty t

desugarIndexSection :: [DimIndex] -> PatternType -> SrcLoc -> MonoM Exp
desugarIndexSection idxs (Arrow _ _ t1 t2) loc = do
  p <- newVName "index_i"
  let body = Index (Var (qualName p) (Info t1) loc) idxs (Info t2') loc
  return $ Lambda [] [Id p (Info t1) noLoc] body Nothing (Info (mempty, toStruct t2)) loc
  where t2' = removeShapeAnnotations t2
desugarIndexSection  _ t _ = error $ "desugarIndexSection: not a function type: " ++ pretty t

noticeDims :: TypeBase (DimDecl VName) as -> MonoM ()
noticeDims = mapM_ notice . nestedDims
  where notice (NamedDim v) = void $ transformFName (qualLeaf v) $ Prim $ Signed Int32
        notice _            = return ()

-- | Convert a collection of 'ValBind's to a nested sequence of let-bound,
-- monomorphic functions with the given expression at the bottom.
unfoldLetFuns :: [ValBind] -> Exp -> Exp
unfoldLetFuns [] e = e
unfoldLetFuns (ValBind _ fname _ rettype dim_params params body _ loc : rest) e =
  LetFun fname (dim_params, params, Nothing, rettype, body) e' loc
  where e' = unfoldLetFuns rest e

expandRecordPattern :: Pattern -> MonoM (Pattern, RecordReplacements)
expandRecordPattern (Id v (Info (Record fs)) loc) = do
  let fs' = M.toList fs
  (fs_ks, fs_ts) <- fmap unzip $ forM fs' $ \(f, ft) ->
    (,) <$> newVName (nameToString f) <*> pure ft
  return (RecordPattern (zip (map fst fs')
                             (zipWith3 Id fs_ks (map Info fs_ts) $ repeat loc))
                        loc,
          M.singleton v $ M.fromList $ zip (map fst fs') $ zip fs_ks fs_ts)
expandRecordPattern (Id v t loc) = return (Id v t loc, mempty)
expandRecordPattern (TuplePattern pats loc) = do
  (pats', rrs) <- unzip <$> mapM expandRecordPattern pats
  return (TuplePattern pats' loc, mconcat rrs)
expandRecordPattern (RecordPattern fields loc) = do
  let (field_names, field_pats) = unzip fields
  (field_pats', rrs) <- unzip <$> mapM expandRecordPattern field_pats
  return (RecordPattern (zip field_names field_pats') loc, mconcat rrs)
expandRecordPattern (PatternParens pat loc) = do
  (pat', rr) <- expandRecordPattern pat
  return (PatternParens pat' loc, rr)
expandRecordPattern (Wildcard t loc) = return (Wildcard t loc, mempty)
expandRecordPattern (PatternAscription pat td loc) = do
  (pat', rr) <- expandRecordPattern pat
  return (PatternAscription pat' td loc, rr)

-- | Monomorphize a polymorphic function at the types given in the instance
-- list. Monomorphizes the body of the function as well. Returns the fresh name
-- of the generated monomorphic function and its 'ValBind' representation.
monomorphizeBinding :: PolyBinding -> TypeBase () () -> MonoM (VName, ValBind)
monomorphizeBinding (PolyBinding (name, tparams, params, retdecl, rettype, body, loc)) t =
  noRecordReplacements $ do
  t' <- removeTypeVariablesInType t
  let bind_t = foldFunType (map (toStructural . patternType) params) $
               toStructural rettype
      substs = typeSubsts bind_t t'
      rettype' = applySubst (`M.lookup` substs) rettype
      params' = map (substPattern $ applySubst (`M.lookup` substs)) params

  (params'', rrs) <- unzip <$> mapM expandRecordPattern params'

  mapM_ noticeDims $ rettype : map patternStructType params''

  body' <- updateExpTypes (`M.lookup` substs) body
  body'' <- withRecordReplacements (mconcat rrs) $ transformExp body'
  name' <- if null tparams then return name else newName name
  return (name', toValBinding name' params'' rettype' body'')

  where shape_params = filter (not . isTypeParam) tparams

        updateExpTypes substs = astMap $ mapper substs
        mapper substs = ASTMapper { mapOnExp         = astMap $ mapper substs
                                  , mapOnName        = pure
                                  , mapOnQualName    = pure
                                  , mapOnType        = pure . applySubst substs
                                  , mapOnCompType    = pure . applySubst substs
                                  , mapOnStructType  = pure . applySubst substs
                                  , mapOnPatternType = pure . applySubst substs
                                  }

        toValBinding name' params'' rettype' body'' =
          ValBind { valBindEntryPoint = False
                  , valBindName       = name'
                  , valBindRetDecl    = retdecl
                  , valBindRetType    = Info rettype'
                  , valBindTypeParams = shape_params
                  , valBindParams     = params''
                  , valBindBody       = body''
                  , valBindDoc        = Nothing
                  , valBindLocation   = loc
                  }

typeSubsts :: TypeBase () () -> TypeBase () ()
           -> M.Map VName (TypeBase () ())
typeSubsts (Record fields1) (Record fields2) =
  mconcat $ zipWith typeSubsts
  (map snd $ sortFields fields1) (map snd $ sortFields fields2)
typeSubsts (TypeVar _ v _) t =
  M.singleton (typeLeaf v) t
typeSubsts Prim{} Prim{} = mempty
typeSubsts (Arrow _ _ t1a t1b) (Arrow _ _ t2a t2b) =
  typeSubsts t1a t2a <> typeSubsts t1b t2b
typeSubsts t1@Array{} t2@Array{}
  | Just t1' <- peelArray (arrayRank t1) t1,
    Just t2' <- peelArray (arrayRank t1) t2 =
      typeSubsts t1' t2'
typeSubsts t1 t2 = error $ unlines ["typeSubsts: mismatched types:", pretty t1, pretty t2]

-- | Perform a given substitution on the types in a pattern.
substPattern :: (PatternType -> PatternType) -> Pattern -> Pattern
substPattern f pat = case pat of
  TuplePattern pats loc      -> TuplePattern (map (substPattern f) pats) loc
  RecordPattern fs loc       -> RecordPattern (map substField fs) loc
    where substField (n, p) = (n, substPattern f p)
  PatternParens p loc        -> PatternParens (substPattern f p) loc
  Id vn (Info tp) loc        -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc     -> Wildcard (Info $ f tp) loc
  PatternAscription p td loc -> PatternAscription (substPattern f p) td loc

toPolyBinding :: ValBind -> PolyBinding
toPolyBinding (ValBind _ name retdecl (Info rettype) tparams params body _ loc) =
  PolyBinding (name, tparams, params, retdecl, rettype, body, loc)

-- | Remove all type variables and type abbreviations from a value binding.
removeTypeVariables :: ValBind -> MonoM ValBind
removeTypeVariables valbind@(ValBind _ _ _ (Info rettype) _ pats body _ _) = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  let substPatternType = fromStruct . substituteTypes subs . toStruct
      mapper = ASTMapper {
          mapOnExp         = astMap mapper
        , mapOnName        = pure
        , mapOnQualName    = pure
        , mapOnType        = pure . removeShapeAnnotations .
                             substituteTypes subs . vacuousShapeAnnotations
        , mapOnCompType    = pure . fromStruct . removeShapeAnnotations .
                             substituteTypes subs .
                             vacuousShapeAnnotations . toStruct
        , mapOnStructType  = pure . substituteTypes subs
        , mapOnPatternType = pure . substPatternType
        }

  body' <- astMap mapper body

  return valbind { valBindRetType = Info $ substituteTypes subs rettype
                 , valBindParams  = map (substPattern substPatternType) pats
                 , valBindBody    = body'
                 }

removeTypeVariablesInType :: TypeBase dim () -> MonoM (TypeBase () ())
removeTypeVariablesInType t = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  return $ removeShapeAnnotations $ substituteTypes subs $ vacuousShapeAnnotations t

transformValBind :: ValBind -> MonoM Env
transformValBind valbind = do
  valbind' <- toPolyBinding <$> removeTypeVariables valbind
  when (valBindEntryPoint valbind) $ do
    t <- removeTypeVariablesInType $ removeShapeAnnotations $ foldFunType
         (map patternStructType (valBindParams valbind)) $
         unInfo $ valBindRetType valbind
    (name, valbind'') <- monomorphizeBinding valbind' t
    tell $ Seq.singleton (name, valbind'' { valBindEntryPoint = True})
    addLifted (valBindName valbind) t name
  return mempty { envPolyBindings = M.singleton (valBindName valbind) valbind' }

transformTypeBind :: TypeBind -> MonoM Env
transformTypeBind (TypeBind name tparams tydecl _ _) = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  noticeDims $ unInfo $ expandedType tydecl
  let tp = substituteTypes subs . unInfo $ expandedType tydecl
      tbinding = TypeAbbr Lifted tparams tp -- The Lifted is arbitrary.
  return mempty { envTypeBindings = M.singleton name tbinding }

-- | Monomorphize a list of top-level declarations. A module-free input program
-- is expected, so only value declarations and type declaration are accepted.
transformDecs :: [Dec] -> MonoM ()
transformDecs [] = return ()
transformDecs (ValDec valbind : ds) = do
  env <- transformValBind valbind
  localEnv env $ transformDecs ds

transformDecs (TypeDec typebind : ds) = do
  env <- transformTypeBind typebind
  localEnv env $ transformDecs ds

transformDecs (dec : _) =
  error $ "The monomorphization module expects a module-free " ++
  "input program, but received: " ++ pretty dec

transformProg :: MonadFreshNames m => [Dec] -> m [ValBind]
transformProg decs =
  fmap (toList . fmap snd . snd) $ modifyNameSource $ \namesrc ->
  runMonoM namesrc $ transformDecs decs
