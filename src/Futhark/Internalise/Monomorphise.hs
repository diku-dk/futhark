-- | This monomorphization module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent monomorphic program.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.Monomorphise
  ( transformProg
  , transformDecs
  , runMonoM
  ) where

import           Control.Monad.RWS
import           Control.Monad.State
import           Data.List
import           Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Sem

import           Futhark.MonadFreshNames
import           Language.Futhark
import           Language.Futhark.Traversals
import           Language.Futhark.TypeChecker.Monad (TypeBinding(..))
import           Language.Futhark.TypeChecker.Types

-- | The monomorphization monad reads 'PolyBinding's and writes 'MonoBinding's.
-- The 'TypeParam's in a 'MonoBinding' can only be shape parameters.
newtype PolyBinding = PolyBinding (VName, [TypeParam], [Pattern], StructType, Exp)
newtype MonoBinding = MonoBinding (VName, [TypeParam], [Pattern], StructType, Exp)

type InstanceList = [TypeBase () ()]

-- | Monomorphization environment mapping names of polymorphic functions to a
-- representation of their corresponding function bindings.
data Env = Env { envPolyBindings :: M.Map VName PolyBinding
               , envTypeBindings :: M.Map VName TypeBinding
               }

instance Sem.Semigroup Env where
  Env tb1 pb1 <> Env tb2 pb2 = Env (tb1 <> tb2) (pb1 <> pb2)

instance Monoid Env where
  mempty  = Env mempty mempty
  mappend = (Sem.<>)

localEnv :: Env -> MonoM a -> MonoM a
localEnv env = local (env <>)

extendEnv :: VName -> PolyBinding -> MonoM a -> MonoM a
extendEnv vn binding = localEnv
  mempty { envPolyBindings = M.singleton vn binding }

-- | The monomorphization monad.
newtype MonoM a = MonoM (RWST Env [(VName, MonoBinding)] VNameSource
                          (State Lifts) a)
  deriving (Functor, Applicative, Monad,
            MonadReader Env,
            MonadWriter [(VName, MonoBinding)],
            MonadFreshNames)

runMonoM :: VNameSource -> MonoM a -> (a, VNameSource)
runMonoM src (MonoM m) = (a, src')
  where (a, src', _) = evalState (runRWST m mempty src) mempty

lookupVar :: SrcLoc -> VName -> MonoM PolyBinding
lookupVar loc vn = do
  env <- asks envPolyBindings
  case M.lookup vn env of
    Just valbind -> return valbind
    Nothing -> error $ "Unknown variable " ++ pretty vn ++ " at " ++ locStr loc

-- | Mapping from function name and instance list to a new function name in case
-- the function has already been instantiated with those concrete types.
type Lifts = [((VName, InstanceList), VName)]

getLifts :: MonoM Lifts
getLifts = MonoM $ lift get

modifyLifts :: (Lifts -> Lifts) -> MonoM ()
modifyLifts = MonoM . lift . modify

addLifted :: VName -> InstanceList -> VName -> MonoM ()
addLifted fname il lifted_fname =
  modifyLifts (((fname, il), lifted_fname) :)

lookupLifted :: VName -> InstanceList -> MonoM (Maybe VName)
lookupLifted fname il = do
  lifts <- getLifts
  return $ lookup (fname, il) lifts

transformFName :: VName -> [TypeBase () ()] -> SrcLoc -> MonoM VName
transformFName fname il loc
  | null il = return fname -- If the instance list is empty, the variable
                           -- does not refer to a polymorphic function.
  | baseTag fname <= maxIntrinsicTag = return fname
  | otherwise = do
      funbind <- lookupVar loc fname
      maybe_fname <- lookupLifted fname il
      case maybe_fname of
        Just x -> return x  -- The function has alread been
                            -- monomorphized for the given il.
        Nothing -> do
          (fname', funbind') <- monomorphizeBinding funbind il
          tell [(fname, funbind')]
          addLifted fname il fname'
          return fname'

-- | Monomorphization of expressions.
transformExp :: Exp -> MonoM Exp
transformExp e@Literal{} = return e

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
        transformField f@RecordFieldImplicit{} =
          return f  -- TODO: What if this is a polymorphic function?

transformExp (ArrayLit es tp loc) =
  ArrayLit <$> mapM transformExp es <*> pure tp <*> pure loc

transformExp (Range e1 me incl tp loc) = do
  e1' <- transformExp e1
  me' <- mapM transformExp me
  incl' <- mapM transformExp incl
  return $ Range e1' me' incl' tp loc

transformExp e@Empty{} = return e

transformExp (Var (QualName qs fname) (Info (il, ps, ret)) loc) = do
  fname' <- transformFName fname il loc
  return $ Var (QualName qs fname') (Info (il, ps, ret)) loc

transformExp (Ascript e tp loc) =
  Ascript <$> transformExp e <*> pure tp <*> pure loc

transformExp (LetPat tparams pat e1 e2 loc) =
  LetPat tparams pat <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (LetFun fname (tparams, params, _, Info ret, body) e loc)
  | any isTypeParam tparams = do
      -- Retrieve the lifted monomorphic function bindings that are produced,
      -- filter those that are monomorphic versions of the current let-bound
      -- function and insert them at this point, and propagate the rest.
      let funbind = PolyBinding (fname, tparams, params, ret, body)
      pass $ do
        (e', bs) <- listen $ extendEnv fname funbind $ transformExp e
        let (bs_local, bs_prop) = partition ((== fname) . fst) bs
        return (unfoldLetFuns (map snd bs_local) e', const bs_prop)

  | otherwise = do
      body' <- transformExp body
      e' <- transformExp e
      return $ LetFun fname (tparams, params, Nothing, Info ret, body') e' loc

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
      | intrinsic "filter" v ->
          transformExp $ Filter f arr loc

    _ -> do
      e1' <- transformExp e1
      e2' <- transformExp e2
      return $ Apply e1' e2' d tp loc
  where intrinsic s (QualName _ v) =
          baseTag v <= maxIntrinsicTag && baseName v == nameFromString s

transformExp (Negate e loc) =
  Negate <$> transformExp e <*> pure loc

transformExp (Lambda tparams params e0 decl tp loc) = do
  e0' <- transformExp e0
  return $ Lambda tparams params e0' decl tp loc

transformExp (OpSection (QualName qs fname) (Info il)
               (Info xtype) (Info ytype) (Info rettype) loc) = do
  fname' <- transformFName fname il loc
  desugarOpSection (QualName qs fname') Nothing Nothing il xtype ytype rettype loc

transformExp (OpSectionLeft (QualName qs fname) (Info il) e
               (Info xtype, Info ytype) (Info rettype) loc) = do
  fname' <- transformFName fname il loc
  e' <- transformExp e
  desugarOpSection (QualName qs fname') (Just e') Nothing il xtype ytype rettype loc

transformExp (OpSectionRight (QualName qs fname) (Info il) e
               (Info xtype, Info ytype) (Info rettype) loc) = do
  fname' <- transformFName fname il loc
  e' <- transformExp e
  desugarOpSection (QualName qs fname') Nothing (Just e') il xtype ytype rettype loc

transformExp (DoLoop tparams pat e1 form e3 loc) = do
  e1' <- transformExp e1
  form' <- case form of
    For ident e2  -> For ident <$> transformExp e2
    ForIn pat2 e2 -> ForIn pat2 <$> transformExp e2
    While e2      -> While <$> transformExp e2
  e3' <- transformExp e3
  return $ DoLoop tparams pat e1' form' e3' loc

transformExp (BinOp (QualName qs fname) (Info il) (e1, d1) (e2, d2) tp loc) = do
  fname' <- transformFName fname il loc
  e1' <- transformExp e1
  e2' <- transformExp e2
  return $ BinOp (QualName qs fname') (Info il) (e1', d1) (e2', d2) tp loc

transformExp (Project n e tp loc) = do
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

transformExp (Concat i e1 es loc) =
  Concat i <$> transformExp e1 <*> mapM transformExp es <*> pure loc

transformExp (Reshape e1 e2 t loc) =
  Reshape <$> transformExp e1 <*> transformExp e2 <*> pure t <*> pure loc

transformExp (Rearrange is e0 loc) =
  Rearrange is <$> transformExp e0 <*> pure loc

transformExp (Rotate i e1 e2 loc) =
  Rotate i <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Map e1 es tp loc) =
  Map <$> transformExp e1 <*> mapM transformExp es <*> pure tp <*> pure loc

transformExp (Reduce comm e1 e2 e3 loc) =
  Reduce comm <$> transformExp e1 <*> transformExp e2
              <*> transformExp e3 <*> pure loc

transformExp (Scan e1 e2 e3 loc) =
  Scan <$> transformExp e1 <*> transformExp e2 <*> transformExp e3 <*> pure loc

transformExp (Filter e1 e2 loc) =
  Filter <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Partition es e0 loc) =
  Partition <$> mapM transformExp es <*> transformExp e0 <*> pure loc

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

transformDimIndex :: DimIndexBase Info VName -> MonoM (DimIndexBase Info VName)
transformDimIndex (DimFix e) = DimFix <$> transformExp e
transformDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> trans me1 <*> trans me2 <*> trans me3
  where trans = mapM transformExp

-- | Transform an operator section into a lambda.
desugarOpSection :: QualName VName -> Maybe Exp -> Maybe Exp -> [TypeBase () ()]
                 -> StructType -> StructType -> CompType -> SrcLoc -> MonoM Exp
desugarOpSection qn e_left e_right il xtype ytype rettype loc = do
  (e1, p1) <- makeVarParam e_left xtype
  (e2, p2) <- makeVarParam e_right ytype
  let (paramtypes, ret) = unfoldFunType rettype
      paramtypes' = map (vacuousShapeAnnotations. toStruct) paramtypes
      body = BinOp qn (Info il) (e1, Info xtype) (e2, Info ytype)
                      (Info (paramtypes', ret)) loc
      rettype' = vacuousShapeAnnotations $ toStruct rettype
  return $ Lambda [] (p1 ++ p2) body Nothing (Info rettype') loc

  where makeVarParam (Just e) _ = return (e, [])
        makeVarParam Nothing argtype = do
          x <- newNameFromString "x"
          let (pts, ret) = unfoldFunType argtype
              ret' = fromStruct $ removeShapeAnnotations ret
          return (Var (qualName x) (Info ([], pts, ret')) noLoc,
                  [Id x (Info $ fromStruct argtype) noLoc])

-- | Convert a collection of 'MonoBinding's to a nested sequence of let-bound,
-- monomorphic functions with the given expression at the bottom.
unfoldLetFuns :: [MonoBinding] -> Exp -> Exp
unfoldLetFuns [] e = e
unfoldLetFuns (MonoBinding (fname, dim_params, params, rettype, body) : rest) e =
  LetFun fname (dim_params, params, Nothing, Info rettype, body) e' noLoc
  where e' = unfoldLetFuns rest e

-- | Monomorphize a polymorphic function at the types given in the instance
-- list. Monomorphizes the body of the function as well. Returns the fresh name
-- of the generated monomorphic function and its 'MonoBinding' representation.
monomorphizeBinding :: PolyBinding -> InstanceList -> MonoM (VName, MonoBinding)
monomorphizeBinding (PolyBinding (name, tparams, params, rettype, body)) il = do
  let rettype' = applySubst substs rettype
      params' = map (substPattern $ applySubst substs) params
  body' <- updateExpTypes body
  body'' <- transformExp body'
  name' <- newName name
  let monobind = MonoBinding (name', shape_params, params', rettype', body'')
  return (name', monobind)

  where (type_params, shape_params) = partition isTypeParam tparams
        tnames = map typeParamName type_params
        substs = M.fromList $ zip tnames il

        updateExpTypes = astMap mapper
        mapper = ASTMapper { mapOnExp         = astMap mapper
                           , mapOnName        = pure
                           , mapOnQualName    = pure
                           , mapOnType        = pure . applySubst substs
                           , mapOnCompType    = pure . applySubst substs
                           , mapOnStructType  = pure . applySubst substs
                           , mapOnPatternType = pure . applySubst substs
                           }

-- | Perform a given substitution on the types in a pattern.
substPattern :: (PatternType -> PatternType) -> Pattern -> Pattern
substPattern f pat = case pat of
  TuplePattern pats loc  -> TuplePattern (map (substPattern f) pats) loc
  RecordPattern fs loc   -> RecordPattern (map substField fs) loc
    where substField (n, p) = (n, substPattern f p)
  PatternParens p loc    -> PatternParens (substPattern f p) loc
  Id vn (Info tp) loc    -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc -> Wildcard (Info $ f tp) loc
  PatternAscription p _  -> substPattern f p

toPolyBinding :: ValBind -> PolyBinding
toPolyBinding (ValBind _ name _ (Info rettype) tparams params body _ _) =
  PolyBinding (name, tparams, params, rettype, body)

toValBinding :: MonoBinding -> ValBind
toValBinding (MonoBinding (name, shape_params, params, rettype, body)) =
  ValBind { valBindEntryPoint = False
          , valBindName       = name
          , valBindRetDecl    = Nothing
          , valBindRetType    = Info rettype
          , valBindTypeParams = shape_params
          , valBindParams     = params
          , valBindBody       = body
          , valBindDoc        = Nothing
          , valBindLocation   = noLoc
          }

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

transformValBind :: ValBind -> MonoM ([ValBind], Env)
transformValBind valbind@(ValBind _ name _ _ tparams _ body _ _)
  | any isTypeParam tparams =
      return ([], mempty { envPolyBindings =
                             M.singleton name $ toPolyBinding valbind })
  | otherwise = do
      (body', binds) <- censor (const mempty) $ listen $ transformExp body
      valbind' <- removeTypeVariables valbind { valBindBody = body' }
      valbinds <- mapM (removeTypeVariables . toValBinding . snd) binds
      return (valbinds ++ [valbind'], mempty)

transformTypeBind :: TypeBind -> MonoM Env
transformTypeBind (TypeBind name tparams tydecl _ _) = do
  subs <- asks $ M.map TypeSub . envTypeBindings
  let tp = substituteTypes subs . unInfo $ expandedType tydecl
      tbinding = TypeAbbr tparams tp
  return mempty { envTypeBindings = M.singleton name tbinding }

-- | Monomorphize a list of top-level declarations. A module-free input program
-- is expected, so only value declarations and type declaration are accepted.
transformDecs :: [Dec] -> MonoM [Dec]
transformDecs [] = return []
transformDecs (ValDec valbind : ds) = do
  (valbinds, env) <- transformValBind valbind
  ds' <- localEnv env $ transformDecs ds
  return $ map ValDec valbinds ++ ds'

transformDecs (TypeDec typebind : ds) = do
  env <- transformTypeBind typebind
  localEnv env $ transformDecs ds

transformDecs (dec : _) =
  error $ "The monomorphization module expects a module-free " ++
  "input program, but received: " ++ pretty dec

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs = modifyNameSource $ \namesrc ->
  runMonoM namesrc $ transformDecs decs
