{-# LANGUAGE FlexibleContexts #-}
-- | This monomorphization module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent monomorphic program.
--
-- This pass also does a few other simplifications to make the job of
-- subsequent passes easier.  Specifically, it does the following:
--
-- * Turn operator sections into explicit lambdas.
--
-- * Converts identifiers of record type into record patterns (and
--   similarly for tuples).
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
  ) where

import           Control.Monad.RWS hiding (Sum)
import           Control.Monad.State
import           Control.Monad.Writer hiding (Sum)
import           Data.Bitraversable
import           Data.Bifunctor
import           Data.List
import           Data.Loc
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Foldable

import           Futhark.MonadFreshNames
import           Language.Futhark
import           Language.Futhark.Traversals
import           Language.Futhark.Semantic (TypeBinding(..))
import           Language.Futhark.TypeChecker.Types

i32 :: TypeBase dim als
i32 = Scalar $ Prim $ Signed Int32

-- | The monomorphization monad reads 'PolyBinding's and writes 'ValBinding's.
-- The 'TypeParam's in a 'ValBinding' can only be shape parameters.
--
-- Each 'Polybinding' is also connected with the 'RecordReplacements'
-- that were active when the binding was defined.  This is used only
-- in local functions.
data PolyBinding = PolyBinding RecordReplacements
                   (VName, [TypeParam], [Pattern],
                     Maybe (TypeExp VName), StructType, [VName], Exp, SrcLoc)

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

instance Semigroup Env where
  Env tb1 pb1 rr1 <> Env tb2 pb2 rr2 = Env (tb1 <> tb2) (pb1 <> pb2) (rr1 <> rr2)

instance Monoid Env where
  mempty  = Env mempty mempty mempty

localEnv :: Env -> MonoM a -> MonoM a
localEnv env = local (env <>)

extendEnv :: VName -> PolyBinding -> MonoM a -> MonoM a
extendEnv vn binding = localEnv
  mempty { envPolyBindings = M.singleton vn binding }

withRecordReplacements :: RecordReplacements -> MonoM a -> MonoM a
withRecordReplacements rr = localEnv mempty { envRecordReplacements = rr }

replaceRecordReplacements :: RecordReplacements -> MonoM a -> MonoM a
replaceRecordReplacements rr = local $ \env -> env { envRecordReplacements = rr }

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

-- | Given instantiated type of function, produce size arguments.
type InferSizeArgs = StructType -> [Exp]

-- | The kind of type relative to which we monomorphise.  What is
-- important to us is not the specific dimensions, but merely whether
-- they are known or anonymous (the latter False).
type MonoType = TypeBase Bool ()

monoType :: TypeBase (DimDecl VName) als -> MonoType
monoType = bimap onDim (const ())
  where onDim AnyDim = False
        onDim _      = True

-- | Mapping from function name and instance list to a new function name in case
-- the function has already been instantiated with those concrete types.
type Lifts = [((VName, MonoType), (VName, InferSizeArgs))]

getLifts :: MonoM Lifts
getLifts = MonoM $ lift get

modifyLifts :: (Lifts -> Lifts) -> MonoM ()
modifyLifts = MonoM . lift . modify

addLifted :: VName -> MonoType -> (VName, InferSizeArgs) -> MonoM ()
addLifted fname il liftf =
  modifyLifts (((fname, il), liftf) :)

lookupLifted :: VName -> MonoType -> MonoM (Maybe (VName, InferSizeArgs))
lookupLifted fname t = lookup (fname, t) <$> getLifts

transformFName :: SrcLoc -> QualName VName -> StructType -> MonoM Exp
transformFName loc fname t
  | baseTag (qualLeaf fname) <= maxIntrinsicTag = return $ var fname
  | otherwise = do
      maybe_fname <- lookupLifted (qualLeaf fname) (monoType t)
      maybe_funbind <- lookupFun $ qualLeaf fname
      t' <- removeTypeVariablesInType t
      case (maybe_fname, maybe_funbind) of
        -- The function has already been monomorphised.
        (Just (fname', infer), _) ->
          return $ applySizeArgs fname' t' $ infer t'
        -- An intrinsic function.
        (Nothing, Nothing) -> return $ var fname
        -- A polymorphic function.
        (Nothing, Just funbind) -> do
          (fname', infer, funbind') <- monomorphiseBinding False funbind (monoType t')
          tell $ Seq.singleton (qualLeaf fname, funbind')
          addLifted (qualLeaf fname) (monoType t) (fname', infer)
          return $ applySizeArgs fname' t' $ infer t'

  where var fname' = Var fname' (Info (fromStruct t)) loc

        applySizeArg (i, f) size_arg =
          (i-1,
           Apply f size_arg (Info (Observe, Nothing))
           (Info (foldFunType (replicate i i32) (fromStruct t)), Info [])
           loc)

        applySizeArgs fname' t' size_args =
          snd $ foldl' applySizeArg (length size_args - 1,
                                     Var (qualName fname')
                                     (Info (foldFunType (map (const i32) size_args)
                                            (fromStruct t')))
                                     loc)
          size_args

-- | This carries out record replacements in the alias information of a type.
transformType :: TypeBase dim Aliasing -> MonoM (TypeBase dim Aliasing)
transformType t = do
  rrs <- asks envRecordReplacements
  let replace (AliasBound v) | Just d <- M.lookup v rrs =
                                 S.fromList $ map (AliasBound . fst) $ M.elems d
      replace x = S.singleton x
  -- As an attempt at an optimisation, only transform the aliases if
  -- they refer to a variable we have record-replaced.
  return $ if any ((`M.member` rrs) . aliasVar) $ aliases t
           then second (mconcat . map replace . S.toList) t
           else t

-- | Monomorphization of expressions.
transformExp :: Exp -> MonoM Exp
transformExp e@Literal{} = return e
transformExp e@IntLit{} = return e
transformExp e@FloatLit{} = return e
transformExp e@StringLit{} = return e

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
        transformField (RecordFieldImplicit v t _) = do
          t' <- traverse transformType t
          transformField $ RecordFieldExplicit (baseName v)
            (Var (qualName v) t' loc) loc

transformExp (ArrayLit es t loc) =
  ArrayLit <$> mapM transformExp es <*> traverse transformType t <*> pure loc

transformExp (Range e1 me incl tp loc) = do
  e1' <- transformExp e1
  me' <- mapM transformExp me
  incl' <- mapM transformExp incl
  return $ Range e1' me' incl' tp loc

transformExp (Var fname (Info t) loc) = do
  maybe_fs <- lookupRecordReplacement $ qualLeaf fname
  case maybe_fs of
    Just fs -> do
      let toField (f, (f_v, f_t)) = do
            f_t' <- transformType f_t
            let f_v' = Var (qualName f_v) (Info f_t') loc
            return $ RecordFieldExplicit f f_v' loc
      RecordLit <$> mapM toField (M.toList fs) <*> pure loc
    Nothing -> do
      t' <- transformType t
      transformFName loc fname (toStruct t')

transformExp (Ascript e tp loc) =
  Ascript <$> transformExp e <*> pure tp <*> pure loc

transformExp (Coerce e tp (Info t, ext) loc) = do
  noticeDims t
  Coerce <$> transformExp e <*> pure tp <*>
    ((,) <$> (Info <$> transformType t) <*> pure ext) <*> pure loc

transformExp (LetPat pat e1 e2 (Info t, retext) loc) = do
  (pat', rr) <- transformPattern pat
  t' <- transformType t
  LetPat pat' <$> transformExp e1 <*>
    withRecordReplacements rr (transformExp e2) <*>
    pure (Info t', retext) <*> pure loc

transformExp (LetFun fname (tparams, params, retdecl, Info ret, body) e e_t loc)
  | any isTypeParam tparams = do
      -- Retrieve the lifted monomorphic function bindings that are produced,
      -- filter those that are monomorphic versions of the current let-bound
      -- function and insert them at this point, and propagate the rest.
      rr <- asks envRecordReplacements
      let funbind = PolyBinding rr (fname, tparams, params, retdecl, ret, [], body, loc)
      pass $ do
        (e', bs) <- listen $ extendEnv fname funbind $ transformExp e
        let (bs_local, bs_prop) = Seq.partition ((== fname) . fst) bs
        return (unfoldLetFuns (map snd $ toList bs_local) e', const bs_prop)

  | otherwise = do
      body' <- transformExp body
      LetFun fname (tparams, params, retdecl, Info ret, body') <$>
        transformExp e <*> traverse transformType e_t <*> pure loc

transformExp (If e1 e2 e3 (tp, retext) loc) = do
  e1' <- transformExp e1
  e2' <- transformExp e2
  e3' <- transformExp e3
  tp' <- traverse transformType tp
  return $ If e1' e2' e3' (tp', retext) loc

transformExp (Apply e1 e2 d (ret, ext) loc) = do
  e1' <- transformExp e1
  e2' <- transformExp e2
  ret' <- traverse transformType ret
  return $ Apply e1' e2' d (ret', ext) loc

transformExp (Negate e loc) =
  Negate <$> transformExp e <*> pure loc

transformExp (Lambda params e0 decl tp loc) = do
  e0' <- transformExp e0
  return $ Lambda params e0' decl tp loc

transformExp (OpSection qn t loc) =
  transformExp $ Var qn t loc

transformExp (OpSectionLeft fname (Info t) e
               (Info (xtype, xargext), Info ytype) (Info rettype, Info retext) loc) = do
  fname' <- transformFName loc fname $ toStruct t
  e' <- transformExp e
  desugarBinOpSection fname' (Just e') Nothing
    t (xtype, xargext) (ytype, Nothing) (rettype, retext) loc

transformExp (OpSectionRight fname (Info t) e
              (Info xtype, Info (ytype, yargext)) (Info rettype) loc) = do
  fname' <- transformFName loc fname $ toStruct t
  e' <- transformExp e
  desugarBinOpSection fname' Nothing (Just e')
    t (xtype, Nothing) (ytype, yargext) (rettype, []) loc

transformExp (ProjectSection fields (Info t) loc) =
  desugarProjectSection fields t loc

transformExp (IndexSection idxs (Info t) loc) =
  desugarIndexSection idxs t loc

transformExp (DoLoop sparams pat e1 form e3 ret loc) = do
  e1' <- transformExp e1
  form' <- case form of
    For ident e2  -> For ident <$> transformExp e2
    ForIn pat2 e2 -> ForIn pat2 <$> transformExp e2
    While e2      -> While <$> transformExp e2
  e3' <- transformExp e3
  return $ DoLoop sparams pat e1' form' e3' ret loc

transformExp (BinOp (fname, oploc) (Info t) (e1, d1) (e2, d2) tp ext loc) = do
  fname' <- transformFName loc fname $ toStruct t
  e1' <- transformExp e1
  e2' <- transformExp e2
  return $
    case fname' of
      Var fname'' _ _ ->
        BinOp (fname'', oploc) (Info t) (e1', d1) (e2', d2) tp ext loc
      _ ->
        Apply (Apply fname' e1' (Info (Observe, snd (unInfo d1)))
               (Info (foldFunType [fromStruct $ fst (unInfo d2)] (unInfo tp)),
                Info mempty) loc)
        e2' (Info (Observe, snd (unInfo d2))) (tp, ext) loc

transformExp (Project n e tp loc) = do
  maybe_fs <- case e of
    Var qn _ _ -> lookupRecordReplacement (qualLeaf qn)
    _          -> return Nothing
  case maybe_fs of
    Just m | Just (v, _) <- M.lookup n m ->
               return $ Var (qualName v) tp loc
    _ -> do
      e' <- transformExp e
      return $ Project n e' tp loc

transformExp (LetWith id1 id2 idxs e1 body (Info t) loc) = do
  idxs' <- mapM transformDimIndex idxs
  e1' <- transformExp e1
  body' <- transformExp body
  t' <- transformType t
  return $ LetWith id1 id2 idxs' e1' body' (Info t') loc

transformExp (Index e0 idxs info loc) =
  Index <$> transformExp e0 <*> mapM transformDimIndex idxs <*> pure info <*> pure loc

transformExp (Update e1 idxs e2 loc) =
  Update <$> transformExp e1 <*> mapM transformDimIndex idxs
         <*> transformExp e2 <*> pure loc

transformExp (RecordUpdate e1 fs e2 t loc) =
  RecordUpdate <$> transformExp e1 <*> pure fs
               <*> transformExp e2 <*> pure t <*> pure loc

transformExp (Unsafe e1 loc) =
  Unsafe <$> transformExp e1 <*> pure loc

transformExp (Assert e1 e2 desc loc) =
  Assert <$> transformExp e1 <*> transformExp e2 <*> pure desc <*> pure loc

transformExp (Constr name all_es t loc) =
  Constr name <$> mapM transformExp all_es <*> pure t <*> pure loc

transformExp (Match e cs (t, retext) loc) =
  Match <$> transformExp e <*> mapM transformCase cs <*>
  ((,) <$> traverse transformType t <*> pure retext) <*> pure loc

transformCase :: Case -> MonoM Case
transformCase (CasePat p e loc) = do
  (p', rr) <- transformPattern p
  CasePat <$> pure p' <*> withRecordReplacements rr (transformExp e) <*> pure loc

transformDimIndex :: DimIndexBase Info VName -> MonoM (DimIndexBase Info VName)
transformDimIndex (DimFix e) = DimFix <$> transformExp e
transformDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> trans me1 <*> trans me2 <*> trans me3
  where trans = mapM transformExp

-- | Transform an operator section into a lambda.
desugarBinOpSection :: Exp -> Maybe Exp -> Maybe Exp
                    -> PatternType
                    -> (StructType, Maybe VName) -> (StructType, Maybe VName)
                    -> (PatternType, [VName]) -> SrcLoc -> MonoM Exp
desugarBinOpSection op e_left e_right t (xtype, xext) (ytype, yext) (rettype, retext) loc = do
  (e1, p1) <- makeVarParam e_left $ fromStruct xtype
  (e2, p2) <- makeVarParam e_right $ fromStruct ytype
  let apply_left = Apply op e1 (Info (Observe, xext))
                   (Info $ foldFunType [fromStruct ytype] t, Info []) loc
      body = Apply apply_left e2 (Info (Observe, yext))
             (Info rettype, Info retext) loc
      rettype' = toStruct rettype
  return $ Lambda (p1 ++ p2) body Nothing (Info (mempty, rettype')) loc

  where makeVarParam (Just e) _ = return (e, [])
        makeVarParam Nothing argtype = do
          x <- newNameFromString "x"
          return (Var (qualName x) (Info argtype) noLoc,
                  [Id x (Info $ fromStruct argtype) noLoc])

desugarProjectSection :: [Name] -> PatternType -> SrcLoc -> MonoM Exp
desugarProjectSection fields (Scalar (Arrow _ _ t1 t2)) loc = do
  p <- newVName "project_p"
  let body = foldl project (Var (qualName p) (Info t1) noLoc) fields
  return $ Lambda [Id p (Info t1) noLoc] body Nothing (Info (mempty, toStruct t2)) loc
  where project e field =
          case typeOf e of
            Scalar (Record fs)
              | Just t <- M.lookup field fs ->
                  Project field e (Info t) noLoc
            t -> error $ "desugarOpSection: type " ++ pretty t ++
                 " does not have field " ++ pretty field
desugarProjectSection  _ t _ = error $ "desugarOpSection: not a function type: " ++ pretty t

desugarIndexSection :: [DimIndex] -> PatternType -> SrcLoc -> MonoM Exp
desugarIndexSection idxs (Scalar (Arrow _ _ t1 t2)) loc = do
  p <- newVName "index_i"
  let body = Index (Var (qualName p) (Info t1) loc) idxs (Info t2, Info []) loc
  return $ Lambda [Id p (Info t1) noLoc] body Nothing (Info (mempty, toStruct t2)) loc
desugarIndexSection  _ t _ = error $ "desugarIndexSection: not a function type: " ++ pretty t

noticeDims :: TypeBase (DimDecl VName) as -> MonoM ()
noticeDims = mapM_ notice . nestedDims
  where notice (NamedDim v) = void $ transformFName noLoc v i32
        notice _            = return ()

-- | Convert a collection of 'ValBind's to a nested sequence of let-bound,
-- monomorphic functions with the given expression at the bottom.
unfoldLetFuns :: [ValBind] -> Exp -> Exp
unfoldLetFuns [] e = e
unfoldLetFuns (ValBind _ fname _ (Info (rettype, _)) dim_params params body _ loc : rest) e =
  LetFun fname (dim_params, params, Nothing, Info rettype, body) e' (Info e_t) loc
  where e' = unfoldLetFuns rest e
        e_t = typeOf e'

transformPattern :: Pattern -> MonoM (Pattern, RecordReplacements)
transformPattern (Id v (Info (Scalar (Record fs))) loc) = do
  let fs' = M.toList fs
  (fs_ks, fs_ts) <- fmap unzip $ forM fs' $ \(f, ft) ->
    (,) <$> newVName (nameToString f) <*> transformType ft
  return (RecordPattern (zip (map fst fs')
                             (zipWith3 Id fs_ks (map Info fs_ts) $ repeat loc))
                        loc,
          M.singleton v $ M.fromList $ zip (map fst fs') $ zip fs_ks fs_ts)
transformPattern (Id v t loc) = return (Id v t loc, mempty)
transformPattern (TuplePattern pats loc) = do
  (pats', rrs) <- unzip <$> mapM transformPattern pats
  return (TuplePattern pats' loc, mconcat rrs)
transformPattern (RecordPattern fields loc) = do
  let (field_names, field_pats) = unzip fields
  (field_pats', rrs) <- unzip <$> mapM transformPattern field_pats
  return (RecordPattern (zip field_names field_pats') loc, mconcat rrs)
transformPattern (PatternParens pat loc) = do
  (pat', rr) <- transformPattern pat
  return (PatternParens pat' loc, rr)
transformPattern (Wildcard (Info t) loc) = do
  t' <- transformType t
  return (wildcard t' loc, mempty)
transformPattern (PatternAscription pat td loc) = do
  (pat', rr) <- transformPattern pat
  return (PatternAscription pat' td loc, rr)
transformPattern (PatternLit e t loc) = return (PatternLit e t loc, mempty)
transformPattern (PatternConstr name t all_ps loc) = do
  (all_ps', rrs) <- unzip <$> mapM transformPattern all_ps
  return (PatternConstr name t all_ps' loc, mconcat rrs)

wildcard :: PatternType -> SrcLoc -> Pattern
wildcard (Scalar (Record fs)) loc =
  RecordPattern (zip (M.keys fs) $ map ((`Wildcard` loc) . Info) $ M.elems fs) loc
wildcard t loc =
  Wildcard (Info t) loc

type DimInst = M.Map VName (DimDecl VName)

dimMapping :: Monoid a =>
              TypeBase (DimDecl VName) a
           -> TypeBase (DimDecl VName) a
           -> DimInst
dimMapping t1 t2 = execState (matchDims f t1 t2) mempty
  where f (NamedDim d1) d2 = do
          modify $ M.insert (qualLeaf d1) d2
          return $ NamedDim d1
        f d _ = return d

inferSizeArgs :: [TypeParam] -> StructType -> StructType -> [Exp]
inferSizeArgs tparams bind_t t =
  mapMaybe (tparamArg (dimMapping bind_t t)) tparams
  where tparamArg dinst tp =
          case M.lookup (typeParamName tp) dinst of
            Just (NamedDim d) ->
              Just $ Var d (Info i32) noLoc
            Just (ConstDim x) ->
              Just $ Literal (SignedValue $ Int32Value $ fromIntegral x) noLoc
            _ ->
              Nothing

explicitSizes :: StructType -> MonoType -> S.Set VName
explicitSizes t1 t2 =
  execState (matchDims onDims t1 t2) mempty `S.intersection` mustBeExplicit t1
  where onDims d1 d2 = do
          case (d1, d2) of
            (NamedDim v, True) -> modify $ S.insert $ qualLeaf v
            _                  -> return ()
          return d1

-- Monomorphising higher-order functions can result in function types
-- where the same named parameter occurs in multiple spots.  When
-- monomorphising we don't really need those parameter names anymore,
-- and the defunctionaliser can be confused if there are duplicates
-- (it doesn't handle shadowing), so let's just remove all parameter
-- names here.  This is safe because a MonoType does not contain sizes
-- anyway.
noNamedParams :: MonoType -> MonoType
noNamedParams = f
  where f (Array () u t shape) = Array () u (f' t) shape
        f (Scalar t) = Scalar $ f' t
        f' (Arrow () _ t1 t2) =
          Arrow () Unnamed (f t1) (f t2)
        f' (Record fs) =
          Record $ fmap f fs
        f' (Sum cs) =
          Sum $ fmap (map f) cs
        f' t = t

-- | Monomorphise a polymorphic function at the types given in the instance
-- list. Monomorphises the body of the function as well. Returns the fresh name
-- of the generated monomorphic function and its 'ValBind' representation.
monomorphiseBinding :: Bool -> PolyBinding -> MonoType
                    -> MonoM (VName, InferSizeArgs, ValBind)
monomorphiseBinding entry (PolyBinding rr (name, tparams, params, retdecl, rettype, retext, body, loc)) t =
  replaceRecordReplacements rr $ do
  let bind_t = foldFunType (map patternStructType params) rettype
  (substs, t_shape_params) <- typeSubstsM loc (noSizes bind_t) $ noNamedParams t
  let substs' = M.map Subst substs
      rettype' = substTypesAny (`M.lookup` substs') rettype
      substPatternType =
        substTypesAny (fmap (fmap fromStruct) . (`M.lookup` substs'))
      params' = map (substPattern entry substPatternType) params
      bind_t' = substTypesAny (`M.lookup` substs') bind_t
      (shape_params_explicit, shape_params_implicit) =
        partition ((`S.member` explicitSizes bind_t' t) . typeParamName) $
        shape_params ++ t_shape_params

  (params'', rrs) <- unzip <$> mapM transformPattern params'

  mapM_ noticeDims $ rettype : map patternStructType params''

  body' <- updateExpTypes (`M.lookup` substs') body
  body'' <- withRecordReplacements (mconcat rrs) $ transformExp body'
  name' <- if null tparams && not entry then return name else newName name

  return (name',
          inferSizeArgs shape_params_explicit bind_t',
          if entry
          then toValBinding name'
               (shape_params_explicit++shape_params_implicit) params''
               (rettype', retext) body''
          else toValBinding name' shape_params_implicit
               (map shapeParam shape_params_explicit ++ params'')
               (rettype', retext) body'')

  where shape_params = filter (not . isTypeParam) tparams

        updateExpTypes substs = astMap $ mapper substs
        mapper substs = ASTMapper { mapOnExp         = astMap $ mapper substs
                                  , mapOnName        = pure
                                  , mapOnQualName    = pure
                                  , mapOnStructType  = pure . applySubst substs
                                  , mapOnPatternType = pure . applySubst substs
                                  }

        shapeParam tp = Id (typeParamName tp) (Info i32) $ srclocOf tp

        toValBinding name' tparams' params'' rettype' body'' =
          ValBind { valBindEntryPoint = Nothing
                  , valBindName       = name'
                  , valBindRetDecl    = retdecl
                  , valBindRetType    = Info rettype'
                  , valBindTypeParams = tparams'
                  , valBindParams     = params''
                  , valBindBody       = body''
                  , valBindDoc        = Nothing
                  , valBindLocation   = loc
                  }

typeSubstsM :: MonadFreshNames m =>
               SrcLoc -> TypeBase () () -> MonoType
            -> m (M.Map VName StructType, [TypeParam])
typeSubstsM loc orig_t1 orig_t2 =
  let m = sub orig_t1 orig_t2
  in runWriterT $ execStateT m mempty

  where sub t1@Array{} t2@Array{}
          | Just t1' <- peelArray (arrayRank t1) t1,
            Just t2' <- peelArray (arrayRank t1) t2 =
              sub t1' t2'
        sub (Scalar (TypeVar _ _ v _)) t = addSubst v t
        sub (Scalar (Record fields1)) (Scalar (Record fields2)) =
          zipWithM_ sub
          (map snd $ sortFields fields1) (map snd $ sortFields fields2)
        sub (Scalar Prim{}) (Scalar Prim{}) = return ()
        sub (Scalar (Arrow _ _ t1a t1b)) (Scalar (Arrow _ _ t2a t2b)) = do
          sub t1a t2a
          sub t1b t2b
        sub (Scalar (Sum cs1)) (Scalar (Sum cs2)) =
          zipWithM_ typeSubstClause (sortConstrs cs1) (sortConstrs cs2)
          where typeSubstClause (_, ts1) (_, ts2) = zipWithM sub ts1 ts2
        sub t1@(Scalar Sum{}) t2 = sub t1 t2
        sub t1 t2@(Scalar Sum{}) = sub t1 t2

        sub t1 t2 = error $ unlines ["typeSubstsM: mismatched types:", pretty t1, pretty t2]

        addSubst (TypeName _ v) t = do
          exists <- gets $ M.member v
          unless exists $ do
            t' <- bitraverse onDim pure t
            modify $ M.insert v t'

        onDim True = do d <- lift $ lift $ newVName "d"
                        tell [TypeParamDim d loc]
                        return $ NamedDim $ qualName d
        onDim False = return AnyDim

-- | Perform a given substitution on the types in a pattern.
substPattern :: Bool -> (PatternType -> PatternType) -> Pattern -> Pattern
substPattern entry f pat = case pat of
  TuplePattern pats loc       -> TuplePattern (map (substPattern entry f) pats) loc
  RecordPattern fs loc        -> RecordPattern (map substField fs) loc
    where substField (n, p) = (n, substPattern entry f p)
  PatternParens p loc         -> PatternParens (substPattern entry f p) loc
  Id vn (Info tp) loc         -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc      -> Wildcard (Info $ f tp) loc
  PatternAscription p td loc | entry     -> PatternAscription (substPattern False f p) td loc
                             | otherwise -> substPattern False f p
  PatternLit e (Info tp) loc  -> PatternLit e (Info $ f tp) loc
  PatternConstr n (Info tp) ps loc -> PatternConstr n (Info $ f tp) ps loc

toPolyBinding :: ValBind -> PolyBinding
toPolyBinding (ValBind _ name retdecl (Info (rettype, retext)) tparams params body _ loc) =
  PolyBinding mempty (name, tparams, params, retdecl, rettype, retext, body, loc)

-- | Remove all type variables and type abbreviations from a value binding.
removeTypeVariables :: Bool -> ValBind -> MonoM ValBind
removeTypeVariables entry valbind@(ValBind _ _ _ (Info (rettype, retext)) _ pats body _ _) = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  let mapper = ASTMapper {
          mapOnExp         = astMap mapper
        , mapOnName        = pure
        , mapOnQualName    = pure
        , mapOnStructType  = pure . substituteTypes subs
        , mapOnPatternType = pure . substituteTypes subs
        }

  body' <- astMap mapper body

  return valbind { valBindRetType = Info (substituteTypes subs rettype, retext)
                 , valBindParams  = map (substPattern entry $ substituteTypes subs) pats
                 , valBindBody    = body'
                 }

removeTypeVariablesInType :: StructType -> MonoM StructType
removeTypeVariablesInType t = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  return $ substituteTypes subs t

transformValBind :: ValBind -> MonoM Env
transformValBind valbind = do
  valbind' <- toPolyBinding <$>
              removeTypeVariables (isJust (valBindEntryPoint valbind)) valbind

  when (isJust $ valBindEntryPoint valbind) $ do
    t <- removeTypeVariablesInType $ foldFunType
         (map patternStructType (valBindParams valbind)) $
         fst $ unInfo $ valBindRetType valbind
    -- We are monomorphising in a special way here, so we don't want
    -- the monomorphisations of local functions to be remembered.
    old_lifts <- getLifts
    (name, _, valbind'') <- monomorphiseBinding True valbind' $ monoType t
    modifyLifts $ const old_lifts
    tell $ Seq.singleton (name, valbind'' { valBindEntryPoint = valBindEntryPoint valbind})

  return mempty { envPolyBindings = M.singleton (valBindName valbind) valbind' }

transformTypeBind :: TypeBind -> MonoM Env
transformTypeBind (TypeBind name l tparams tydecl _ _) = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  noticeDims $ unInfo $ expandedType tydecl
  let tp = substituteTypes subs . unInfo $ expandedType tydecl
      tbinding = TypeAbbr l tparams tp
  return mempty { envTypeBindings = M.singleton name tbinding }

-- | Monomorphise a list of top-level declarations. A module-free input program
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
