{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

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
-- * Rewrite BinOp nodes to Apply nodes.
--
-- Note that these changes are unfortunately not visible in the AST
-- representation.
module Futhark.Internalise.Monomorphise (transformProg) where

import Control.Monad.Identity
import Control.Monad.RWS hiding (Sum)
import Control.Monad.State
import Control.Monad.Writer hiding (Sum)
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.List (partition)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Semantic (TypeBinding (..))
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

i64 :: TypeBase dim als
i64 = Scalar $ Prim $ Signed Int64

-- The monomorphization monad reads 'PolyBinding's and writes
-- 'ValBind's.  The 'TypeParam's in the 'ValBind's can only be size
-- parameters.
--
-- Each 'Polybinding' is also connected with the 'RecordReplacements'
-- that were active when the binding was defined.  This is used only
-- in local functions.
data PolyBinding
  = PolyBinding
      RecordReplacements
      ( VName,
        [TypeParam],
        [Pat],
        StructRetType,
        Exp,
        [AttrInfo VName],
        SrcLoc
      )

-- Mapping from record names to the variable names that contain the
-- fields.  This is used because the monomorphiser also expands all
-- record patterns.
type RecordReplacements = M.Map VName RecordReplacement

type RecordReplacement = M.Map Name (VName, PatType)

-- Monomorphization environment mapping names of polymorphic functions
-- to a representation of their corresponding function bindings.
data Env = Env
  { envPolyBindings :: M.Map VName PolyBinding,
    envTypeBindings :: M.Map VName TypeBinding,
    envRecordReplacements :: RecordReplacements
  }

instance Semigroup Env where
  Env tb1 pb1 rr1 <> Env tb2 pb2 rr2 = Env (tb1 <> tb2) (pb1 <> pb2) (rr1 <> rr2)

instance Monoid Env where
  mempty = Env mempty mempty mempty

localEnv :: Env -> MonoM a -> MonoM a
localEnv env = local (env <>)

extendEnv :: VName -> PolyBinding -> MonoM a -> MonoM a
extendEnv vn binding =
  localEnv
    mempty {envPolyBindings = M.singleton vn binding}

withRecordReplacements :: RecordReplacements -> MonoM a -> MonoM a
withRecordReplacements rr = localEnv mempty {envRecordReplacements = rr}

replaceRecordReplacements :: RecordReplacements -> MonoM a -> MonoM a
replaceRecordReplacements rr = local $ \env -> env {envRecordReplacements = rr}

-- The monomorphization monad.
newtype MonoM a
  = MonoM
      ( RWST
          Env
          (Seq.Seq (VName, ValBind))
          VNameSource
          (State Lifts)
          a
      )
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter (Seq.Seq (VName, ValBind)),
      MonadFreshNames
    )

runMonoM :: VNameSource -> MonoM a -> ((a, Seq.Seq (VName, ValBind)), VNameSource)
runMonoM src (MonoM m) = ((a, defs), src')
  where
    (a, src', defs) = evalState (runRWST m mempty src) mempty

lookupFun :: VName -> MonoM (Maybe PolyBinding)
lookupFun vn = do
  env <- asks envPolyBindings
  case M.lookup vn env of
    Just valbind -> pure $ Just valbind
    Nothing -> pure Nothing

lookupRecordReplacement :: VName -> MonoM (Maybe RecordReplacement)
lookupRecordReplacement v = asks $ M.lookup v . envRecordReplacements

-- Given instantiated type of function, produce size arguments.
type InferSizeArgs = StructType -> [Exp]

data MonoSize
  = -- | The integer encodes an equivalence class, so we can keep
    -- track of sizes that are statically identical.
    MonoKnown Int
  | MonoAnon VName
  deriving (Show)

-- We treat all MonoAnon as identical.
instance Eq MonoSize where
  MonoKnown x == MonoKnown y = x == y
  MonoAnon _ == MonoAnon _ = True
  _ == _ = False

instance Pretty MonoSize where
  ppr (MonoKnown i) = text "?" <> ppr i
  ppr (MonoAnon v) = text "?" <> pprName v

instance Pretty (ShapeDecl MonoSize) where
  ppr (ShapeDecl ds) = mconcat (map (brackets . ppr) ds)

-- The kind of type relative to which we monomorphise.  What is most
-- important to us is not the specific dimensions, but merely whether
-- they are known or anonymous/local.
type MonoType = TypeBase MonoSize ()

monoType :: TypeBase (DimDecl VName) als -> MonoType
monoType = (`evalState` (0, mempty)) . traverseDims onDim . toStruct
  where
    onDim bound _ (NamedDim d)
      -- A locally bound size.
      | qualLeaf d `S.member` bound = pure $ MonoAnon $ qualLeaf d
    onDim _ _ d = do
      (i, m) <- get
      case M.lookup d m of
        Just prev ->
          pure $ MonoKnown prev
        Nothing -> do
          put (i + 1, M.insert d i m)
          pure $ MonoKnown i

-- Mapping from function name and instance list to a new function name in case
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
  | baseTag (qualLeaf fname) <= maxIntrinsicTag = pure $ var fname
  | otherwise = do
      t' <- removeTypeVariablesInType t
      let mono_t = monoType t'
      maybe_fname <- lookupLifted (qualLeaf fname) mono_t
      maybe_funbind <- lookupFun $ qualLeaf fname
      case (maybe_fname, maybe_funbind) of
        -- The function has already been monomorphised.
        (Just (fname', infer), _) ->
          pure $ applySizeArgs fname' t' $ infer t'
        -- An intrinsic function.
        (Nothing, Nothing) -> pure $ var fname
        -- A polymorphic function.
        (Nothing, Just funbind) -> do
          (fname', infer, funbind') <- monomorphiseBinding False funbind mono_t
          tell $ Seq.singleton (qualLeaf fname, funbind')
          addLifted (qualLeaf fname) mono_t (fname', infer)
          pure $ applySizeArgs fname' t' $ infer t'
  where
    var fname' = Var fname' (Info (fromStruct t)) loc

    applySizeArg (i, f) size_arg =
      ( i - 1,
        AppExp
          (Apply f size_arg (Info (Observe, Nothing)) loc)
          (Info $ AppRes (foldFunType (replicate i i64) (RetType [] (fromStruct t))) [])
      )

    applySizeArgs fname' t' size_args =
      snd $
        foldl'
          applySizeArg
          ( length size_args - 1,
            Var
              (qualName fname')
              ( Info
                  ( foldFunType
                      (map (const i64) size_args)
                      (RetType [] $ fromStruct t')
                  )
              )
              loc
          )
          size_args

-- This carries out record replacements in the alias information of a type.
transformType :: TypeBase dim Aliasing -> MonoM (TypeBase dim Aliasing)
transformType t = do
  rrs <- asks envRecordReplacements
  let replace (AliasBound v)
        | Just d <- M.lookup v rrs =
            S.fromList $ map (AliasBound . fst) $ M.elems d
      replace x = S.singleton x
  -- As an attempt at an optimisation, only transform the aliases if
  -- they refer to a variable we have record-replaced.
  pure $
    if any ((`M.member` rrs) . aliasVar) $ aliases t
      then second (mconcat . map replace . S.toList) t
      else t

sizesForPat :: MonadFreshNames m => Pat -> m ([VName], Pat)
sizesForPat pat = do
  (params', sizes) <- runStateT (astMap tv pat) []
  pure (sizes, params')
  where
    tv = identityMapper {mapOnPatType = bitraverse onDim pure}
    onDim (AnyDim _) = do
      v <- lift $ newVName "size"
      modify (v :)
      pure $ NamedDim $ qualName v
    onDim d = pure d

transformAppRes :: AppRes -> MonoM AppRes
transformAppRes (AppRes t ext) =
  AppRes <$> transformType t <*> pure ext

transformAppExp :: AppExp -> AppRes -> MonoM Exp
transformAppExp (Range e1 me incl loc) res = do
  e1' <- transformExp e1
  me' <- mapM transformExp me
  incl' <- mapM transformExp incl
  pure $ AppExp (Range e1' me' incl' loc) (Info res)
transformAppExp (Coerce e tp loc) res =
  AppExp <$> (Coerce <$> transformExp e <*> pure tp <*> pure loc) <*> pure (Info res)
transformAppExp (LetPat sizes pat e1 e2 loc) res = do
  (pat', rr) <- transformPat pat
  AppExp
    <$> ( LetPat sizes pat' <$> transformExp e1
            <*> withRecordReplacements rr (transformExp e2)
            <*> pure loc
        )
    <*> pure (Info res)
transformAppExp (LetFun fname (tparams, params, retdecl, Info ret, body) e loc) res
  | not $ null tparams = do
      -- Retrieve the lifted monomorphic function bindings that are produced,
      -- filter those that are monomorphic versions of the current let-bound
      -- function and insert them at this point, and propagate the rest.
      rr <- asks envRecordReplacements
      let funbind = PolyBinding rr (fname, tparams, params, ret, body, mempty, loc)
      pass $ do
        (e', bs) <- listen $ extendEnv fname funbind $ transformExp e
        -- Do not remember this one for next time we monomorphise this
        -- function.
        modifyLifts $ filter ((/= fname) . fst . fst)
        let (bs_local, bs_prop) = Seq.partition ((== fname) . fst) bs
        pure (unfoldLetFuns (map snd $ toList bs_local) e', const bs_prop)
  | otherwise = do
      body' <- transformExp body
      AppExp
        <$> (LetFun fname (tparams, params, retdecl, Info ret, body') <$> transformExp e <*> pure loc)
        <*> pure (Info res)
transformAppExp (If e1 e2 e3 loc) res =
  AppExp <$> (If <$> transformExp e1 <*> transformExp e2 <*> transformExp e3 <*> pure loc) <*> pure (Info res)
transformAppExp (Apply e1 e2 d loc) res =
  AppExp <$> (Apply <$> transformExp e1 <*> transformExp e2 <*> pure d <*> pure loc) <*> pure (Info res)
transformAppExp (DoLoop sparams pat e1 form e3 loc) res = do
  e1' <- transformExp e1
  form' <- case form of
    For ident e2 -> For ident <$> transformExp e2
    ForIn pat2 e2 -> ForIn pat2 <$> transformExp e2
    While e2 -> While <$> transformExp e2
  e3' <- transformExp e3
  -- Maybe monomorphisation introduced new arrays to the loop, and
  -- maybe they have AnyDim sizes.  This is not allowed.  Invent some
  -- sizes for them.
  (pat_sizes, pat') <- sizesForPat pat
  pure $ AppExp (DoLoop (sparams ++ pat_sizes) pat' e1' form' e3' loc) (Info res)
transformAppExp (BinOp (fname, _) (Info t) (e1, d1) (e2, d2) loc) (AppRes ret ext) = do
  fname' <- transformFName loc fname $ toStruct t
  e1' <- transformExp e1
  e2' <- transformExp e2
  if orderZero (typeOf e1') && orderZero (typeOf e2')
    then pure $ applyOp fname' e1' e2'
    else do
      -- We have to flip the arguments to the function, because
      -- operator application is left-to-right, while function
      -- application is outside-in.  This matters when the arguments
      -- produce existential sizes.  There are later places in the
      -- compiler where we transform BinOp to Apply, but anything that
      -- involves existential sizes will necessarily go through here.
      (x_param_e, x_param) <- makeVarParam e1'
      (y_param_e, y_param) <- makeVarParam e2'
      -- XXX: the type annotations here are wrong, but hopefully it
      -- doesn't matter as there will be an outer AppExp to handle
      -- them.
      pure $
        AppExp
          ( LetPat
              []
              x_param
              e1'
              ( AppExp
                  (LetPat [] y_param e2' (applyOp fname' x_param_e y_param_e) loc)
                  (Info $ AppRes ret mempty)
              )
              mempty
          )
          (Info (AppRes ret mempty))
  where
    applyOp fname' x y =
      AppExp
        ( Apply
            ( AppExp
                (Apply fname' x (Info (Observe, snd (unInfo d1))) loc)
                (Info $ AppRes ret mempty)
            )
            y
            (Info (Observe, snd (unInfo d2)))
            loc
        )
        (Info (AppRes ret ext))

    makeVarParam arg = do
      let argtype = typeOf arg
      x <- newNameFromString "binop_p"
      pure
        ( Var (qualName x) (Info argtype) mempty,
          Id x (Info $ fromStruct argtype) mempty
        )
transformAppExp (LetWith id1 id2 idxs e1 body loc) res = do
  idxs' <- mapM transformDimIndex idxs
  e1' <- transformExp e1
  body' <- transformExp body
  pure $ AppExp (LetWith id1 id2 idxs' e1' body' loc) (Info res)
transformAppExp (Index e0 idxs loc) res =
  AppExp
    <$> (Index <$> transformExp e0 <*> mapM transformDimIndex idxs <*> pure loc)
    <*> pure (Info res)
transformAppExp (Match e cs loc) res =
  AppExp
    <$> (Match <$> transformExp e <*> mapM transformCase cs <*> pure loc)
    <*> pure (Info res)

-- Monomorphization of expressions.
transformExp :: Exp -> MonoM Exp
transformExp e@Literal {} = pure e
transformExp e@IntLit {} = pure e
transformExp e@FloatLit {} = pure e
transformExp e@StringLit {} = pure e
transformExp (Parens e loc) =
  Parens <$> transformExp e <*> pure loc
transformExp (QualParens qn e loc) =
  QualParens qn <$> transformExp e <*> pure loc
transformExp (TupLit es loc) =
  TupLit <$> mapM transformExp es <*> pure loc
transformExp (RecordLit fs loc) =
  RecordLit <$> mapM transformField fs <*> pure loc
  where
    transformField (RecordFieldExplicit name e loc') =
      RecordFieldExplicit name <$> transformExp e <*> pure loc'
    transformField (RecordFieldImplicit v t _) = do
      t' <- traverse transformType t
      transformField $
        RecordFieldExplicit
          (baseName v)
          (Var (qualName v) t' loc)
          loc
transformExp (ArrayLit es t loc) =
  ArrayLit <$> mapM transformExp es <*> traverse transformType t <*> pure loc
transformExp (AppExp e res) = do
  noticeDims $ appResType $ unInfo res
  transformAppExp e =<< transformAppRes (unInfo res)
transformExp (Var fname (Info t) loc) = do
  maybe_fs <- lookupRecordReplacement $ qualLeaf fname
  case maybe_fs of
    Just fs -> do
      let toField (f, (f_v, f_t)) = do
            f_t' <- transformType f_t
            let f_v' = Var (qualName f_v) (Info f_t') loc
            pure $ RecordFieldExplicit f f_v' loc
      RecordLit <$> mapM toField (M.toList fs) <*> pure loc
    Nothing -> do
      t' <- transformType t
      transformFName loc fname (toStruct t')
transformExp (Hole t loc) =
  Hole <$> traverse transformType t <*> pure loc
transformExp (Ascript e tp loc) =
  Ascript <$> transformExp e <*> pure tp <*> pure loc
transformExp (Negate e loc) =
  Negate <$> transformExp e <*> pure loc
transformExp (Not e loc) =
  Not <$> transformExp e <*> pure loc
transformExp (Lambda params e0 decl tp loc) = do
  e0' <- transformExp e0
  pure $ Lambda params e0' decl tp loc
transformExp (OpSection qn t loc) =
  transformExp $ Var qn t loc
transformExp (OpSectionLeft fname (Info t) e arg (Info rettype, Info retext) loc) = do
  let (Info (xp, xtype, xargext), Info (yp, ytype)) = arg
  fname' <- transformFName loc fname $ toStruct t
  e' <- transformExp e
  desugarBinOpSection
    fname'
    (Just e')
    Nothing
    t
    (xp, xtype, xargext)
    (yp, ytype, Nothing)
    (rettype, retext)
    loc
transformExp (OpSectionRight fname (Info t) e arg (Info rettype) loc) = do
  let (Info (xp, xtype), Info (yp, ytype, yargext)) = arg
  fname' <- transformFName loc fname $ toStruct t
  e' <- transformExp e
  desugarBinOpSection
    fname'
    Nothing
    (Just e')
    t
    (xp, xtype, Nothing)
    (yp, ytype, yargext)
    (rettype, [])
    loc
transformExp (ProjectSection fields (Info t) loc) =
  desugarProjectSection fields t loc
transformExp (IndexSection idxs (Info t) loc) = do
  idxs' <- mapM transformDimIndex idxs
  desugarIndexSection idxs' t loc
transformExp (Project n e tp loc) = do
  maybe_fs <- case e of
    Var qn _ _ -> lookupRecordReplacement (qualLeaf qn)
    _ -> pure Nothing
  case maybe_fs of
    Just m
      | Just (v, _) <- M.lookup n m ->
          pure $ Var (qualName v) tp loc
    _ -> do
      e' <- transformExp e
      pure $ Project n e' tp loc
transformExp (Update e1 idxs e2 loc) =
  Update <$> transformExp e1 <*> mapM transformDimIndex idxs
    <*> transformExp e2
    <*> pure loc
transformExp (RecordUpdate e1 fs e2 t loc) =
  RecordUpdate <$> transformExp e1 <*> pure fs
    <*> transformExp e2
    <*> pure t
    <*> pure loc
transformExp (Assert e1 e2 desc loc) =
  Assert <$> transformExp e1 <*> transformExp e2 <*> pure desc <*> pure loc
transformExp (Constr name all_es t loc) =
  Constr name <$> mapM transformExp all_es <*> pure t <*> pure loc
transformExp (Attr info e loc) =
  Attr info <$> transformExp e <*> pure loc

transformCase :: Case -> MonoM Case
transformCase (CasePat p e loc) = do
  (p', rr) <- transformPat p
  CasePat p' <$> withRecordReplacements rr (transformExp e) <*> pure loc

transformDimIndex :: DimIndexBase Info VName -> MonoM (DimIndexBase Info VName)
transformDimIndex (DimFix e) = DimFix <$> transformExp e
transformDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> trans me1 <*> trans me2 <*> trans me3
  where
    trans = mapM transformExp

-- Transform an operator section into a lambda.
desugarBinOpSection ::
  Exp ->
  Maybe Exp ->
  Maybe Exp ->
  PatType ->
  (PName, StructType, Maybe VName) ->
  (PName, StructType, Maybe VName) ->
  (PatRetType, [VName]) ->
  SrcLoc ->
  MonoM Exp
desugarBinOpSection op e_left e_right t (xp, xtype, xext) (yp, ytype, yext) (RetType dims rettype, retext) loc = do
  (v1, wrap_left, e1, p1) <- makeVarParam e_left $ fromStruct xtype
  (v2, wrap_right, e2, p2) <- makeVarParam e_right $ fromStruct ytype
  let apply_left =
        AppExp
          ( Apply
              op
              e1
              (Info (Observe, xext))
              loc
          )
          (Info $ AppRes (Scalar $ Arrow mempty yp ytype (RetType [] t)) [])
      rettype' =
        let onDim (NamedDim d)
              | Named p <- xp, qualLeaf d == p = NamedDim $ qualName v1
              | Named p <- yp, qualLeaf d == p = NamedDim $ qualName v2
            onDim d = d
         in first onDim rettype
      body =
        AppExp
          ( Apply
              apply_left
              e2
              (Info (Observe, yext))
              loc
          )
          (Info $ AppRes rettype' retext)
      rettype'' = toStruct rettype'
  pure $
    wrap_left $
      wrap_right $
        Lambda (p1 ++ p2) body Nothing (Info (mempty, RetType dims rettype'')) loc
  where
    patAndVar argtype = do
      x <- newNameFromString "x"
      pure
        ( x,
          Id x (Info argtype) mempty,
          Var (qualName x) (Info argtype) mempty
        )

    makeVarParam (Just e) argtype = do
      (v, pat, var_e) <- patAndVar argtype
      let wrap body =
            AppExp (LetPat [] pat e body mempty) (Info $ AppRes (typeOf body) mempty)
      pure (v, wrap, var_e, [])
    makeVarParam Nothing argtype = do
      (v, pat, var_e) <- patAndVar argtype
      pure (v, id, var_e, [pat])

desugarProjectSection :: [Name] -> PatType -> SrcLoc -> MonoM Exp
desugarProjectSection fields (Scalar (Arrow _ _ t1 (RetType dims t2))) loc = do
  p <- newVName "project_p"
  let body = foldl project (Var (qualName p) (Info t1') mempty) fields
  pure $
    Lambda
      [Id p (Info t1') mempty]
      body
      Nothing
      (Info (mempty, RetType dims $ toStruct t2))
      loc
  where
    t1' = fromStruct t1
    project e field =
      case typeOf e of
        Scalar (Record fs)
          | Just t <- M.lookup field fs ->
              Project field e (Info t) mempty
        t ->
          error $
            "desugarOpSection: type " ++ pretty t
              ++ " does not have field "
              ++ pretty field
desugarProjectSection _ t _ = error $ "desugarOpSection: not a function type: " ++ pretty t

desugarIndexSection :: [DimIndex] -> PatType -> SrcLoc -> MonoM Exp
desugarIndexSection idxs (Scalar (Arrow _ _ t1 (RetType dims t2))) loc = do
  p <- newVName "index_i"
  let body = AppExp (Index (Var (qualName p) (Info t1') loc) idxs loc) (Info (AppRes t2 []))
  pure $
    Lambda
      [Id p (Info (fromStruct t1')) mempty]
      body
      Nothing
      (Info (mempty, RetType dims $ toStruct t2))
      loc
  where
    t1' = fromStruct t1
desugarIndexSection _ t _ = error $ "desugarIndexSection: not a function type: " ++ pretty t

noticeDims :: TypeBase (DimDecl VName) as -> MonoM ()
noticeDims = mapM_ notice . nestedDims
  where
    notice (NamedDim v) = void $ transformFName mempty v i64
    notice _ = pure ()

-- Convert a collection of 'ValBind's to a nested sequence of let-bound,
-- monomorphic functions with the given expression at the bottom.
unfoldLetFuns :: [ValBind] -> Exp -> Exp
unfoldLetFuns [] e = e
unfoldLetFuns (ValBind _ fname _ (Info rettype) dim_params params body _ _ loc : rest) e =
  AppExp (LetFun fname (dim_params, params, Nothing, Info rettype, body) e' loc) (Info $ AppRes e_t mempty)
  where
    e' = unfoldLetFuns rest e
    e_t = typeOf e'

transformPat :: Pat -> MonoM (Pat, RecordReplacements)
transformPat (Id v (Info (Scalar (Record fs))) loc) = do
  let fs' = M.toList fs
  (fs_ks, fs_ts) <- fmap unzip $
    forM fs' $ \(f, ft) ->
      (,) <$> newVName (nameToString f) <*> transformType ft
  pure
    ( RecordPat
        (zip (map fst fs') (zipWith3 Id fs_ks (map Info fs_ts) $ repeat loc))
        loc,
      M.singleton v $ M.fromList $ zip (map fst fs') $ zip fs_ks fs_ts
    )
transformPat (Id v t loc) = pure (Id v t loc, mempty)
transformPat (TuplePat pats loc) = do
  (pats', rrs) <- unzip <$> mapM transformPat pats
  pure (TuplePat pats' loc, mconcat rrs)
transformPat (RecordPat fields loc) = do
  let (field_names, field_pats) = unzip fields
  (field_pats', rrs) <- unzip <$> mapM transformPat field_pats
  pure (RecordPat (zip field_names field_pats') loc, mconcat rrs)
transformPat (PatParens pat loc) = do
  (pat', rr) <- transformPat pat
  pure (PatParens pat' loc, rr)
transformPat (PatAttr attr pat loc) = do
  (pat', rr) <- transformPat pat
  pure (PatAttr attr pat' loc, rr)
transformPat (Wildcard (Info t) loc) = do
  t' <- transformType t
  pure (wildcard t' loc, mempty)
transformPat (PatAscription pat td loc) = do
  (pat', rr) <- transformPat pat
  pure (PatAscription pat' td loc, rr)
transformPat (PatLit e t loc) = pure (PatLit e t loc, mempty)
transformPat (PatConstr name t all_ps loc) = do
  (all_ps', rrs) <- unzip <$> mapM transformPat all_ps
  pure (PatConstr name t all_ps' loc, mconcat rrs)

wildcard :: PatType -> SrcLoc -> Pat
wildcard (Scalar (Record fs)) loc =
  RecordPat (zip (M.keys fs) $ map ((`Wildcard` loc) . Info) $ M.elems fs) loc
wildcard t loc =
  Wildcard (Info t) loc

type DimInst = M.Map VName (DimDecl VName)

dimMapping ::
  Monoid a =>
  TypeBase (DimDecl VName) a ->
  TypeBase (DimDecl VName) a ->
  DimInst
dimMapping t1 t2 = execState (matchDims f t1 t2) mempty
  where
    f bound d1 (NamedDim d2)
      | qualLeaf d2 `elem` bound = pure d1
    f _ (NamedDim d1) d2 = do
      modify $ M.insert (qualLeaf d1) d2
      pure $ NamedDim d1
    f _ d _ = pure d

inferSizeArgs :: [TypeParam] -> StructType -> StructType -> [Exp]
inferSizeArgs tparams bind_t t =
  mapMaybe (tparamArg (dimMapping bind_t t)) tparams
  where
    tparamArg dinst tp =
      case M.lookup (typeParamName tp) dinst of
        Just (NamedDim d) ->
          Just $ Var d (Info i64) mempty
        Just (ConstDim x) ->
          Just $ Literal (SignedValue $ Int64Value $ fromIntegral x) mempty
        _ ->
          Just $ Literal (SignedValue $ Int64Value 0) mempty

-- Monomorphising higher-order functions can result in function types
-- where the same named parameter occurs in multiple spots.  When
-- monomorphising we don't really need those parameter names anymore,
-- and the defunctionaliser can be confused if there are duplicates
-- (it doesn't handle shadowing), so let's just remove all parameter
-- names here.  This is safe because a MonoType does not contain sizes
-- anyway.
noNamedParams :: MonoType -> MonoType
noNamedParams = f
  where
    f (Array () u shape t) = Array () u shape (f' t)
    f (Scalar t) = Scalar $ f' t
    f' (Arrow () _ t1 (RetType dims t2)) =
      Arrow () Unnamed (f t1) (RetType dims (f t2))
    f' (Record fs) =
      Record $ fmap f fs
    f' (Sum cs) =
      Sum $ fmap (map f) cs
    f' t = t

-- Monomorphise a polymorphic function at the types given in the instance
-- list. Monomorphises the body of the function as well. Returns the fresh name
-- of the generated monomorphic function and its 'ValBind' representation.
monomorphiseBinding ::
  Bool ->
  PolyBinding ->
  MonoType ->
  MonoM (VName, InferSizeArgs, ValBind)
monomorphiseBinding entry (PolyBinding rr (name, tparams, params, rettype, body, attrs, loc)) inst_t =
  replaceRecordReplacements rr $ do
    let bind_t = foldFunType (map patternStructType params) rettype
    (substs, t_shape_params) <- typeSubstsM loc (noSizes bind_t) $ noNamedParams inst_t
    let substs' = M.map (Subst []) substs
        rettype' = applySubst (`M.lookup` substs') rettype
        substPatType =
          substTypesAny (fmap (fmap (second (const mempty))) . (`M.lookup` substs'))
        params' = map (substPat entry substPatType) params
        bind_t' = substTypesAny (`M.lookup` substs') bind_t
        (shape_params_explicit, shape_params_implicit) =
          partition ((`S.member` mustBeExplicit bind_t') . typeParamName) $
            shape_params ++ t_shape_params

    (params'', rrs) <- unzip <$> mapM transformPat params'

    mapM_ noticeDims $ retType rettype : map patternStructType params''

    body' <- updateExpTypes (`M.lookup` substs') body
    body'' <- withRecordReplacements (mconcat rrs) $ transformExp body'
    seen_before <- elem name . map (fst . fst) <$> getLifts
    name' <-
      if null tparams && not entry && not seen_before
        then pure name
        else newName name

    pure
      ( name',
        inferSizeArgs shape_params_explicit bind_t',
        if entry
          then
            toValBinding
              name'
              (shape_params_explicit ++ shape_params_implicit)
              params''
              rettype'
              body''
          else
            toValBinding
              name'
              shape_params_implicit
              (map shapeParam shape_params_explicit ++ params'')
              rettype'
              body''
      )
  where
    shape_params = filter (not . isTypeParam) tparams

    updateExpTypes substs = astMap (mapper substs)

    mapper substs =
      ASTMapper
        { mapOnExp = updateExpTypes substs,
          mapOnName = pure,
          mapOnQualName = pure,
          mapOnStructType = pure . applySubst substs,
          mapOnPatType = pure . applySubst substs,
          mapOnStructRetType = pure . applySubst substs,
          mapOnPatRetType = pure . applySubst substs
        }

    shapeParam tp = Id (typeParamName tp) (Info i64) $ srclocOf tp

    toValBinding name' tparams' params'' rettype' body'' =
      ValBind
        { valBindEntryPoint = Nothing,
          valBindName = name',
          valBindRetType = Info rettype',
          valBindRetDecl = Nothing,
          valBindTypeParams = tparams',
          valBindParams = params'',
          valBindBody = body'',
          valBindDoc = Nothing,
          valBindAttrs = attrs,
          valBindLocation = loc
        }

typeSubstsM ::
  MonadFreshNames m =>
  SrcLoc ->
  TypeBase () () ->
  MonoType ->
  m (M.Map VName StructRetType, [TypeParam])
typeSubstsM loc orig_t1 orig_t2 =
  runWriterT $ fst <$> execStateT (sub orig_t1 orig_t2) (mempty, mempty)
  where
    subRet (Scalar (TypeVar _ _ v _)) rt =
      unless (baseTag (typeLeaf v) <= maxIntrinsicTag) $
        addSubst v rt
    subRet t1 (RetType _ t2) =
      sub t1 t2

    sub t1@Array {} t2@Array {}
      | Just t1' <- peelArray (arrayRank t1) t1,
        Just t2' <- peelArray (arrayRank t1) t2 =
          sub t1' t2'
    sub (Scalar (TypeVar _ _ v _)) t =
      unless (baseTag (typeLeaf v) <= maxIntrinsicTag) $
        addSubst v $ RetType [] t
    sub (Scalar (Record fields1)) (Scalar (Record fields2)) =
      zipWithM_
        sub
        (map snd $ sortFields fields1)
        (map snd $ sortFields fields2)
    sub (Scalar Prim {}) (Scalar Prim {}) = pure ()
    sub (Scalar (Arrow _ _ t1a (RetType _ t1b))) (Scalar (Arrow _ _ t2a t2b)) = do
      sub t1a t2a
      subRet t1b t2b
    sub (Scalar (Sum cs1)) (Scalar (Sum cs2)) =
      zipWithM_ typeSubstClause (sortConstrs cs1) (sortConstrs cs2)
      where
        typeSubstClause (_, ts1) (_, ts2) = zipWithM sub ts1 ts2
    sub t1@(Scalar Sum {}) t2 = sub t1 t2
    sub t1 t2@(Scalar Sum {}) = sub t1 t2
    sub t1 t2 = error $ unlines ["typeSubstsM: mismatched types:", pretty t1, pretty t2]

    addSubst (TypeName _ v) (RetType ext t) = do
      (ts, sizes) <- get
      unless (v `M.member` ts) $ do
        t' <- bitraverse onDim pure t
        put (M.insert v (RetType ext t') ts, sizes)

    onDim (MonoKnown i) = do
      (ts, sizes) <- get
      case M.lookup i sizes of
        Nothing -> do
          d <- lift $ lift $ newVName "d"
          tell [TypeParamDim d loc]
          put (ts, M.insert i d sizes)
          pure $ NamedDim $ qualName d
        Just d ->
          pure $ NamedDim $ qualName d
    onDim (MonoAnon v) = pure $ AnyDim $ Just v

-- Perform a given substitution on the types in a pattern.
substPat :: Bool -> (PatType -> PatType) -> Pat -> Pat
substPat entry f pat = case pat of
  TuplePat pats loc -> TuplePat (map (substPat entry f) pats) loc
  RecordPat fs loc -> RecordPat (map substField fs) loc
    where
      substField (n, p) = (n, substPat entry f p)
  PatParens p loc -> PatParens (substPat entry f p) loc
  PatAttr attr p loc -> PatAttr attr (substPat entry f p) loc
  Id vn (Info tp) loc -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc -> Wildcard (Info $ f tp) loc
  PatAscription p td loc
    | entry -> PatAscription (substPat False f p) td loc
    | otherwise -> substPat False f p
  PatLit e (Info tp) loc -> PatLit e (Info $ f tp) loc
  PatConstr n (Info tp) ps loc -> PatConstr n (Info $ f tp) ps loc

toPolyBinding :: ValBind -> PolyBinding
toPolyBinding (ValBind _ name _ (Info rettype) tparams params body _ attrs loc) =
  PolyBinding mempty (name, tparams, params, rettype, body, attrs, loc)

-- Remove all type variables and type abbreviations from a value binding.
removeTypeVariables :: Bool -> ValBind -> MonoM ValBind
removeTypeVariables entry valbind = do
  let (ValBind _ _ _ (Info (RetType dims rettype)) _ pats body _ _ _) = valbind
  subs <- asks $ M.map substFromAbbr . envTypeBindings
  let mapper =
        ASTMapper
          { mapOnExp = onExp,
            mapOnName = pure,
            mapOnQualName = pure,
            mapOnStructType = pure . applySubst (`M.lookup` subs),
            mapOnPatType = pure . applySubst (`M.lookup` subs),
            mapOnStructRetType = pure . applySubst (`M.lookup` subs),
            mapOnPatRetType = pure . applySubst (`M.lookup` subs)
          }

      onExp = astMap mapper

  body' <- onExp body

  pure
    valbind
      { valBindRetType = Info (applySubst (`M.lookup` subs) $ RetType dims rettype),
        valBindParams = map (substPat entry $ applySubst (`M.lookup` subs)) pats,
        valBindBody = body'
      }

removeTypeVariablesInType :: StructType -> MonoM StructType
removeTypeVariablesInType t = do
  subs <- asks $ M.map substFromAbbr . envTypeBindings
  pure $ applySubst (`M.lookup` subs) t

transformValBind :: ValBind -> MonoM Env
transformValBind valbind = do
  valbind' <-
    toPolyBinding
      <$> removeTypeVariables (isJust (valBindEntryPoint valbind)) valbind

  when (isJust $ valBindEntryPoint valbind) $ do
    t <-
      removeTypeVariablesInType $
        foldFunType
          (map patternStructType (valBindParams valbind))
          $ unInfo $ valBindRetType valbind
    (name, infer, valbind'') <- monomorphiseBinding True valbind' $ monoType t
    tell $ Seq.singleton (name, valbind'' {valBindEntryPoint = valBindEntryPoint valbind})
    addLifted (valBindName valbind) (monoType t) (name, infer)

  pure mempty {envPolyBindings = M.singleton (valBindName valbind) valbind'}

transformTypeBind :: TypeBind -> MonoM Env
transformTypeBind (TypeBind name l tparams _ (Info (RetType dims t)) _ _) = do
  subs <- asks $ M.map substFromAbbr . envTypeBindings
  noticeDims t
  let tbinding = TypeAbbr l tparams $ RetType dims $ applySubst (`M.lookup` subs) t
  pure mempty {envTypeBindings = M.singleton name tbinding}

transformDecs :: [Dec] -> MonoM ()
transformDecs [] = pure ()
transformDecs (ValDec valbind : ds) = do
  env <- transformValBind valbind
  localEnv env $ transformDecs ds
transformDecs (TypeDec typebind : ds) = do
  env <- transformTypeBind typebind
  localEnv env $ transformDecs ds
transformDecs (dec : _) =
  error $
    "The monomorphization module expects a module-free "
      ++ "input program, but received: "
      ++ pretty dec

-- | Monomorphise a list of top-level declarations. A module-free input program
-- is expected, so only value declarations and type declaration are accepted.
transformProg :: MonadFreshNames m => [Dec] -> m [ValBind]
transformProg decs =
  fmap (toList . fmap snd . snd) $
    modifyNameSource $ \namesrc ->
      runMonoM namesrc $ transformDecs decs
