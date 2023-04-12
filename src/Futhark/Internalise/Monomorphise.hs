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

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS (MonadReader (..), MonadWriter (..), RWST, asks, runRWST)
import Control.Monad.State
import Control.Monad.Writer (runWriterT)
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
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

-- | Box for Exp that allows better comparison than straight forward structural equality
newtype ReplacedExp = ReplacedExp {unReplaced :: Exp}
  deriving (Show)

instance Pretty ReplacedExp where
  pretty (ReplacedExp e) = pretty e

instance Eq ReplacedExp where
  ReplacedExp e1 == ReplacedExp e2
    | Just es <- similarExps e1 e2 =
        all (uncurry (==) . bimap ReplacedExp ReplacedExp) es
  _ == _ = False

type ExpReplacements = [(ReplacedExp, VName)]

canCalculate :: S.Set VName -> ExpReplacements -> ExpReplacements
canCalculate scope =
  filter ((`S.isSubsetOf` scope) . S.filter notIntrisic . M.keysSet . unFV . freeInExp . unReplaced . fst)
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

-- Replace some expressions by a parameter.
expReplace :: ExpReplacements -> Exp -> Exp
expReplace mapping e
  | Just vn <- lookup (ReplacedExp e) mapping =
      Var (qualName vn) (Info $ typeOf e) (srclocOf e)
expReplace mapping e = runIdentity $ astMap mapper e
  where
    mapper = identityMapper {mapOnExp = pure . expReplace mapping}

-- Construct an Assert expression that checks that the names (values)
-- in the mapping have the same value as the expression they
-- represent.  This is injected into entry points, where we cannot
-- otherwise trust the input.  XXX: the error message generated from
-- this is not great; we should rework it eventually.
entryAssert :: ExpReplacements -> Exp -> Exp
entryAssert [] body = body
entryAssert (x : xs) body =
  Assert (foldl logAnd (cmpExp x) $ map cmpExp xs) body errmsg (srclocOf body)
  where
    errmsg = Info "entry point arguments have invalid sizes."
    bool = Scalar $ Prim Bool
    opt = foldFunType [(Observe, bool), (Observe, bool)] $ RetType [] bool
    andop = Var (qualName (intrinsicVar "&&")) (Info opt) mempty
    eqop = Var (qualName (intrinsicVar "==")) (Info opt) mempty
    logAnd x' y =
      mkApply andop [(Observe, Nothing, x'), (Observe, Nothing, y)] $
        AppRes bool []
    cmpExp (ReplacedExp x', y) =
      mkApply eqop [(Observe, Nothing, x'), (Observe, Nothing, y')] $
        AppRes bool []
      where
        y' = Var (qualName y) (Info i64) mempty

-- Monomorphization environment mapping names of polymorphic functions
-- to a representation of their corresponding function bindings.
data Env = Env
  { envPolyBindings :: M.Map VName PolyBinding,
    envTypeBindings :: M.Map VName TypeBinding,
    envRecordReplacements :: RecordReplacements,
    envScope :: S.Set VName,
    envParametrized :: ExpReplacements
  }

instance Semigroup Env where
  Env tb1 pb1 rr1 sc1 pr1 <> Env tb2 pb2 rr2 sc2 pr2 = Env (tb1 <> tb2) (pb1 <> pb2) (rr1 <> rr2) (sc1 <> sc2) (pr1 <> pr2)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty

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

withArgs :: S.Set VName -> MonoM a -> MonoM a
withArgs args = localEnv $ mempty {envScope = args}

withParams :: ExpReplacements -> MonoM a -> MonoM a
withParams params = localEnv $ mempty {envParametrized = params}

-- The monomorphization monad.
newtype MonoM a
  = MonoM
      ( RWST
          Env
          (Seq.Seq (VName, ValBind))
          (ExpReplacements, VNameSource)
          (State Lifts)
          a
      )
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter (Seq.Seq (VName, ValBind))
    )

instance MonadFreshNames MonoM where
  getNameSource = MonoM $ gets snd
  putNameSource = MonoM . modify . second . const

instance MonadState ExpReplacements MonoM where
  get = MonoM $ gets fst
  put = MonoM . modify . first . const

runMonoM :: VNameSource -> MonoM a -> ((a, Seq.Seq (VName, ValBind)), VNameSource)
runMonoM src (MonoM m) = ((a, defs), src')
  where
    (a, (_, src'), defs) = evalState (runRWST m mempty (mempty, src)) mempty

lookupFun :: VName -> MonoM (Maybe PolyBinding)
lookupFun vn = do
  env <- asks envPolyBindings
  case M.lookup vn env of
    Just valbind -> pure $ Just valbind
    Nothing -> pure Nothing

lookupRecordReplacement :: VName -> MonoM (Maybe RecordReplacement)
lookupRecordReplacement v = asks $ M.lookup v . envRecordReplacements

-- | Asks the introduced variables in a set of argument,
-- that is arguments not currently in scope.
askIntros :: S.Set VName -> MonoM (S.Set VName)
askIntros argset =
  asks $ (S.filter notIntrisic argset `S.difference`) . envScope
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

-- | Gets and removes expressions that could not be calculated when
-- the arguments set will be unscoped.
-- This should be called without argset in scope, for good detection of intros.
parametrizing :: S.Set VName -> MonoM ExpReplacements
parametrizing argset = do
  intros <- askIntros argset
  (params, nxtBind) <- gets $ partition (not . S.disjoint intros . M.keysSet . unFV . freeInExp . unReplaced . fst)
  put nxtBind
  pure params

insertDimCalculus :: MonadFreshNames m => (ReplacedExp, VName) -> Exp -> m Exp
insertDimCalculus (dim, name) body = do
  reName <- newName name
  let expr = unReplaced dim
  pure $
    AppExp
      ( LetPat
          []
          (Id name (Info i64) (srclocOf expr))
          expr
          body
          mempty
      )
      (appRes reName body)
  where
    appRes reName (AppExp _ (Info (AppRes ty ext))) =
      Info $ AppRes (applySubst (subst reName) ty) (reName : ext)
    appRes reName e =
      Info $ AppRes (applySubst (subst reName) $ typeOf e) [reName]

    subst reName vn
      | vn == name = Just $ ExpSubst $ sizeVar (qualName reName) mempty
      | otherwise = Nothing

unscoping :: S.Set VName -> Exp -> MonoM Exp
unscoping argset body = do
  localDims <- parametrizing argset
  scope <- asks $ S.union argset . envScope
  foldrM insertDimCalculus body $ canCalculate scope localDims

scoping :: S.Set VName -> MonoM Exp -> MonoM Exp
scoping argset m =
  withArgs argset m >>= unscoping argset

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
  pretty (MonoKnown i) = "?" <> pretty i
  pretty (MonoAnon v) = "?" <> prettyName v

instance Pretty (Shape MonoSize) where
  pretty (Shape ds) = mconcat (map (brackets . pretty) ds)

-- The kind of type relative to which we monomorphise.  What is most
-- important to us is not the specific dimensions, but merely whether
-- they are known or anonymous/local.
type MonoType = TypeBase MonoSize ()

monoType :: TypeBase Size als -> MonoType
monoType = (`evalState` (0, mempty)) . traverseDims onDim . toStruct
  where
    onDim bound _ (SizeExpr (Var d _ _))
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

-- | Creates a new expression replacement if needed, this always produces normalised sizes.
-- (e.g. single variable or constant)
replaceExp :: Exp -> MonoM Exp
replaceExp e =
  case maybeNormalisedSize e of
    Just e' -> pure e'
    Nothing -> do
      let e' = ReplacedExp e
      prev <- gets $ lookup e'
      prev_param <- asks $ lookup e' . envParametrized
      case (prev_param, prev) of
        (Just vn, _) -> pure $ sizeVar (qualName vn) (srclocOf e)
        (Nothing, Just vn) -> pure $ sizeVar (qualName vn) (srclocOf e)
        (Nothing, Nothing) -> do
          vn <- newNameFromString $ "d<{" ++ prettyString e ++ "}>"
          modify ((e', vn) :)
          pure $ sizeVar (qualName vn) (srclocOf e)
  where
    -- Avoid replacing of some 'already normalised' sizes that are just surounded by some parentheses.
    maybeNormalisedSize e'
      | Just e'' <- strip e' = maybeNormalisedSize e''
    maybeNormalisedSize (Var qn _ loc) = Just $ sizeVar qn loc
    maybeNormalisedSize (IntLit v _ loc) = Just $ IntLit v (Info i64) loc
    maybeNormalisedSize _ = Nothing

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
        mkApply
          f
          [(Observe, Nothing, size_arg)]
          (AppRes (foldFunType (replicate i (Observe, i64)) (RetType [] (fromStruct t))) [])
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
                      (map (const (Observe, i64)) size_args)
                      (RetType [] $ fromStruct t')
                  )
              )
              loc
          )
          size_args

transformTypeSizes :: TypeBase Size as -> MonoM (TypeBase Size as)
transformTypeSizes typ =
  case typ of
    Scalar scalar -> Scalar <$> transformScalarSizes scalar
    Array as u shape scalar -> Array as u <$> mapM onDim shape <*> transformScalarSizes scalar
  where
    transformScalarSizes (Record fs) =
      Record <$> traverse transformTypeSizes fs
    transformScalarSizes (Sum cs) =
      Sum <$> (traverse . traverse) transformTypeSizes cs
    transformScalarSizes (Arrow as argName d argT retT) =
      Arrow as argName d <$> transformTypeSizes argT <*> transformRetTypeSizes argset retT
      where
        argset =
          M.keysSet (unFV $ freeInType argT)
            <> case argName of
              Unnamed -> mempty
              Named vn -> S.singleton vn
    transformScalarSizes (TypeVar as uniq qn args) =
      TypeVar as uniq qn <$> mapM onArg args
      where
        onArg (TypeArgDim dim loc) = TypeArgDim <$> onDim dim <*> pure loc
        onArg (TypeArgType ty loc) = TypeArgType <$> transformTypeSizes ty <*> pure loc
    transformScalarSizes ty = pure ty

    transformRetTypeSizes argset (RetType dims ty) = do
      ty' <- withArgs argset $ transformTypeSizes ty
      rl <- parametrizing argset
      let dims' = dims <> map snd rl
      pure $ RetType dims' ty'

    onDim (SizeExpr e) = SizeExpr <$> (replaceExp =<< transformExp e)
    onDim (AnySize v) = pure $ AnySize v

transformTypeExp :: TypeExp Info VName -> MonoM (TypeExp Info VName)
transformTypeExp te@TEVar {} = pure te
transformTypeExp (TETuple tes loc) =
  TETuple <$> mapM transformTypeExp tes <*> pure loc
transformTypeExp (TERecord fs loc) =
  TERecord <$> mapM (traverse transformTypeExp) fs <*> pure loc
transformTypeExp (TEArray size te loc) =
  TEArray <$> transformSizeExp size <*> transformTypeExp te <*> pure loc
  where
    transformSizeExp (SizeExp e loc') =
      SizeExp <$> (replaceExp =<< transformExp e) <*> pure loc'
    transformSizeExp (SizeExpAny loc') =
      pure $ SizeExpAny loc'
transformTypeExp (TEUnique te loc) =
  TEUnique <$> transformTypeExp te <*> pure loc
transformTypeExp (TEApply te args loc) =
  TEApply <$> transformTypeExp te <*> transformTypeArg args <*> pure loc
  where
    transformTypeArg (TypeArgExpSize size) =
      TypeArgExpSize <$> transformSizeExp size
    transformTypeArg (TypeArgExpType arg) =
      TypeArgExpType <$> transformTypeExp arg
    transformSizeExp (SizeExp e loc') =
      SizeExp <$> (replaceExp =<< transformExp e) <*> pure loc'
    transformSizeExp (SizeExpAny loc') =
      pure $ SizeExpAny loc'
transformTypeExp (TEArrow aname ta tr loc) = do
  tr' <- case aname of
    Just vn -> do
      let argset = S.singleton vn
      ret <- withArgs argset $ transformTypeExp tr
      dims <- parametrizing argset
      if null dims
        then pure ret
        else pure $ TEDim (map snd dims) ret mempty
    Nothing -> transformTypeExp tr
  TEArrow aname <$> transformTypeExp ta <*> pure tr' <*> pure loc
transformTypeExp (TESum cs loc) =
  TESum <$> traverse (traverse (traverse transformTypeExp)) cs <*> pure loc
transformTypeExp (TEDim dims te loc) =
  TEDim dims <$> transformTypeExp te <*> pure loc

-- This carries out record replacements in the alias information of a type.
--
-- It also transforms any size expressions.
transformType :: PatType -> MonoM PatType
transformType t = do
  rrs <- asks envRecordReplacements
  let replace (AliasBound v)
        | Just d <- M.lookup v rrs =
            S.fromList $ map (AliasBound . fst) $ M.elems d
      replace x = S.singleton x
  t' <- transformTypeSizes t
  -- As an attempt at an optimisation, only transform the aliases if
  -- they refer to a variable we have record-replaced.
  pure $
    if any ((`M.member` rrs) . aliasVar) $ aliases t
      then second (mconcat . map replace . S.toList) t'
      else t'

sizesForPat :: MonadFreshNames m => Pat -> m ([VName], Pat)
sizesForPat pat = do
  (params', sizes) <- runStateT (astMap tv pat) []
  pure (sizes, params')
  where
    tv = identityMapper {mapOnPatType = bitraverse onDim pure}
    onDim (AnySize _) = do
      v <- lift $ newVName "size"
      modify (v :)
      pure $ sizeFromName (qualName v) mempty
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
  AppExp <$> (Coerce <$> transformExp e <*> transformTypeExp tp <*> pure loc) <*> pure (Info res)
transformAppExp (LetPat sizes pat e body loc) res = do
  let dimArgs = S.fromList (map sizeName sizes)
  implicitDims <- withArgs dimArgs $ askIntros (M.keysSet $ unFV $ freeInPat pat)
  let dimArgs' = dimArgs <> implicitDims
      letArgs = patNames pat
      argset = dimArgs' `S.union` letArgs
  (pat', rr) <- withArgs dimArgs' $ transformPat pat
  params <- parametrizing dimArgs'
  let sizes' = sizes <> map (`SizeBinder` mempty) (map snd params <> S.toList implicitDims)
  body' <- withRecordReplacements rr $ withParams params $ scoping argset $ transformExp body
  AppExp
    <$> ( LetPat sizes' pat'
            <$> transformExp e
            <*> pure body'
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
        (e', bs) <- listen $ extendEnv fname funbind $ scoping (S.singleton fname) $ transformExp e
        -- Do not remember this one for next time we monomorphise this
        -- function.
        modifyLifts $ filter ((/= fname) . fst . fst)
        let (bs_local, bs_prop) = Seq.partition ((== fname) . fst) bs
        pure (unfoldLetFuns (map snd $ toList bs_local) e', const bs_prop)
  | otherwise = do
      body' <- transformExp body
      AppExp
        <$> ( LetFun fname (tparams, params, retdecl, Info ret, body')
                <$> scoping (S.singleton fname) (transformExp e)
                <*> pure loc
            )
        <*> pure (Info res)
transformAppExp (If e1 e2 e3 loc) res =
  AppExp <$> (If <$> transformExp e1 <*> transformExp e2 <*> transformExp e3 <*> pure loc) <*> pure (Info res)
transformAppExp (Apply fe args _) res =
  mkApply
    <$> transformExp fe
    <*> mapM onArg (NE.toList args)
    <*> pure res
  where
    onArg (Info (d, ext), e) = (d,ext,) <$> transformExp e
transformAppExp (DoLoop sparams pat e1 form body loc) res = do
  e1' <- transformExp e1

  let dimArgs = S.fromList sparams
  (pat', rr) <- withArgs dimArgs $ transformPat pat
  params <- parametrizing dimArgs
  let sparams' = sparams <> map snd params
      mergeArgs = dimArgs `S.union` patNames pat

  (form', rr', formArgs) <- case form of
    For ident e2 -> (,mempty,S.singleton $ identName ident) . For ident <$> transformExp e2
    ForIn pat2 e2 -> do
      (pat2', rr') <- transformPat pat2
      (,rr',patNames pat2) . ForIn pat2' <$> transformExp e2
    While e2 ->
      fmap ((,mempty,mempty) . While) $
        withRecordReplacements rr $
          withParams params $
            scoping mergeArgs $
              transformExp e2
  let argset = mergeArgs `S.union` formArgs

  body' <- withRecordReplacements (rr <> rr') $ withParams params $ scoping argset $ transformExp body
  -- Maybe monomorphisation introduced new arrays to the loop, and
  -- maybe they have AnySize sizes.  This is not allowed.  Invent some
  -- sizes for them.
  (pat_sizes, pat'') <- sizesForPat pat'
  pure $ AppExp (DoLoop (sparams' ++ pat_sizes) pat'' e1' form' body' loc) (Info res)
transformAppExp (BinOp (fname, _) (Info t) (e1, d1) (e2, d2) loc) (AppRes ret ext) = do
  fname' <- transformFName loc fname =<< transformTypeSizes (toStruct t)
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
      mkApply
        (mkApply fname' [(Observe, snd (unInfo d1), x)] (AppRes ret mempty))
        [(Observe, snd (unInfo d2), y)]
        (AppRes ret ext)

    makeVarParam arg = do
      let argtype = typeOf arg
      x <- newNameFromString "binop_p"
      pure
        ( Var (qualName x) (Info argtype) mempty,
          Id x (Info $ fromStruct argtype) mempty
        )
transformAppExp (LetWith id1 id2 idxs e1 body loc) res = do
  id1' <- transformIdent id1
  id2' <- transformIdent id2
  idxs' <- mapM transformDimIndex idxs
  e1' <- transformExp e1
  body' <- scoping (S.singleton $ identName id1') $ transformExp body
  pure $ AppExp (LetWith id1' id2' idxs' e1' body' loc) (Info res)
  where
    transformIdent (Ident v t vloc) =
      Ident v <$> traverse transformType t <*> pure vloc
transformAppExp (Index e0 idxs loc) res =
  AppExp
    <$> (Index <$> transformExp e0 <*> mapM transformDimIndex idxs <*> pure loc)
    <*> pure (Info res)
transformAppExp (Match e cs loc) res = do
  implicitDims <- askIntros (M.keysSet $ unFV $ freeInType $ typeOf e)
  e' <- transformExp e
  cs' <- mapM (transformCase implicitDims) cs
  if S.null implicitDims
    then pure $ AppExp (Match e' cs' loc) (Info res)
    else do
      tmpVar <- newNameFromString "matched_variable"
      pure $
        AppExp
          ( LetPat
              (map (`SizeBinder` mempty) $ S.toList implicitDims)
              (Id tmpVar (Info $ typeOf e') mempty)
              e'
              ( AppExp
                  (Match (Var (qualName tmpVar) (Info $ typeOf e') mempty) cs' loc)
                  (Info res)
              )
              mempty
          )
          (Info res)

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
transformExp (AppExp e res) =
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
  (params', rrs) <- mapAndUnzipM transformPat params
  withRecordReplacements (mconcat rrs) $
    Lambda params'
      <$> transformExp e0
      <*> pure decl
      <*> traverse (traverse transformRetType) tp
      <*> pure loc
transformExp (OpSection qn t loc) =
  transformExp $ Var qn t loc
transformExp (OpSectionLeft fname (Info t) e arg (Info rettype, Info retext) loc) = do
  let (Info (xp, xtype, xargext), Info (yp, ytype)) = arg
  e' <- transformExp e
  desugarBinOpSection
    fname
    (Just e')
    Nothing
    t
    (xp, xtype, xargext)
    (yp, ytype, Nothing)
    (rettype, retext)
    loc
transformExp (OpSectionRight fname (Info t) e arg (Info rettype) loc) = do
  let (Info (xp, xtype), Info (yp, ytype, yargext)) = arg
  e' <- transformExp e
  desugarBinOpSection
    fname
    Nothing
    (Just e')
    t
    (xp, xtype, Nothing)
    (yp, ytype, yargext)
    (rettype, [])
    loc
transformExp (ProjectSection fields (Info t) loc) = do
  t' <- transformType t
  desugarProjectSection fields t' loc
transformExp (IndexSection idxs (Info t) loc) = do
  idxs' <- mapM transformDimIndex idxs
  desugarIndexSection idxs' t loc
transformExp (Project n e tp loc) = do
  tp' <- traverse transformType tp
  maybe_fs <- case e of
    Var qn _ _ -> lookupRecordReplacement (qualLeaf qn)
    _ -> pure Nothing
  case maybe_fs of
    Just m
      | Just (v, _) <- M.lookup n m ->
          pure $ Var (qualName v) tp' loc
    _ -> do
      e' <- transformExp e
      pure $ Project n e' tp' loc
transformExp (Update e1 idxs e2 loc) =
  Update
    <$> transformExp e1
    <*> mapM transformDimIndex idxs
    <*> transformExp e2
    <*> pure loc
transformExp (RecordUpdate e1 fs e2 t loc) =
  RecordUpdate
    <$> transformExp e1
    <*> pure fs
    <*> transformExp e2
    <*> traverse transformType t
    <*> pure loc
transformExp (Assert e1 e2 desc loc) =
  Assert <$> transformExp e1 <*> transformExp e2 <*> pure desc <*> pure loc
transformExp (Constr name all_es t loc) =
  Constr name <$> mapM transformExp all_es <*> traverse transformType t <*> pure loc
transformExp (Attr info e loc) =
  Attr info <$> transformExp e <*> pure loc

transformCase :: S.Set VName -> Case -> MonoM Case
transformCase implicitDims (CasePat p e loc) = do
  (p', rr) <- transformPat p
  CasePat p' <$> withRecordReplacements rr (scoping (patNames p `S.union` implicitDims) $ transformExp e) <*> pure loc

transformDimIndex :: DimIndexBase Info VName -> MonoM (DimIndexBase Info VName)
transformDimIndex (DimFix e) = DimFix <$> transformExp e
transformDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> trans me1 <*> trans me2 <*> trans me3
  where
    trans = mapM transformExp

-- Transform an operator section into a lambda.
desugarBinOpSection ::
  QualName VName ->
  Maybe Exp ->
  Maybe Exp ->
  PatType ->
  (PName, StructType, Maybe VName) ->
  (PName, StructType, Maybe VName) ->
  (PatRetType, [VName]) ->
  SrcLoc ->
  MonoM Exp
desugarBinOpSection fname e_left e_right t (xp, xtype, xext) (yp, ytype, yext) (RetType dims rettype, retext) loc = do
  op <- transformFName loc fname <=< transformTypeSizes $ toStruct t
  (v1, wrap_left, e1, p1) <- makeVarParam e_left . fromStruct =<< transformTypeSizes xtype
  (v2, wrap_right, e2, p2) <- makeVarParam e_right . fromStruct =<< transformTypeSizes ytype
  let apply_left =
        mkApply
          op
          [(Observe, xext, e1)]
          (AppRes (Scalar $ Arrow mempty yp Observe ytype (RetType [] t)) [])
      onDim (SizeExpr (Var d typ _))
        | Named p <- xp, qualLeaf d == p = SizeExpr $ Var (qualName v1) typ loc
        | Named p <- yp, qualLeaf d == p = SizeExpr $ Var (qualName v2) typ loc
      onDim d = d
  rettype' <- first onDim <$> transformTypeSizes rettype
  let body =
        mkApply apply_left [(Observe, yext, e2)] (AppRes rettype' retext)
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
desugarProjectSection fields (Scalar (Arrow _ _ _ t1 (RetType dims t2))) loc = do
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
            "desugarOpSection: type "
              ++ prettyString t
              ++ " does not have field "
              ++ prettyString field
desugarProjectSection _ t _ = error $ "desugarOpSection: not a function type: " ++ prettyString t

desugarIndexSection :: [DimIndex] -> PatType -> SrcLoc -> MonoM Exp
desugarIndexSection idxs (Scalar (Arrow _ _ _ t1 (RetType dims t2))) loc = do
  p <- newVName "index_i"
  t1' <- fromStruct <$> transformTypeSizes t1
  t2' <- transformType t2
  let body = AppExp (Index (Var (qualName p) (Info t1') loc) idxs loc) (Info (AppRes t2' []))
  pure $
    Lambda
      [Id p (Info (fromStruct t1')) mempty]
      body
      Nothing
      (Info (mempty, RetType dims $ toStruct t2'))
      loc
desugarIndexSection _ t _ = error $ "desugarIndexSection: not a function type: " ++ prettyString t

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
transformPat (Id v t loc) = do
  t' <- traverse transformType t
  pure (Id v t' loc, mempty)
transformPat (TuplePat pats loc) = do
  (pats', rrs) <- mapAndUnzipM transformPat pats
  pure (TuplePat pats' loc, mconcat rrs)
transformPat (RecordPat fields loc) = do
  let (field_names, field_pats) = unzip fields
  (field_pats', rrs) <- mapAndUnzipM transformPat field_pats
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
transformPat (PatAscription pat _ _) =
  transformPat pat
transformPat (PatLit e t loc) = pure (PatLit e t loc, mempty)
transformPat (PatConstr name t all_ps loc) = do
  (all_ps', rrs) <- mapAndUnzipM transformPat all_ps
  pure (PatConstr name t all_ps' loc, mconcat rrs)

wildcard :: PatType -> SrcLoc -> Pat
wildcard (Scalar (Record fs)) loc =
  RecordPat (zip (M.keys fs) $ map ((`Wildcard` loc) . Info) $ M.elems fs) loc
wildcard t loc =
  Wildcard (Info t) loc

type DimInst = M.Map VName Size

dimMapping ::
  Monoid a =>
  TypeBase Size a ->
  TypeBase Size a ->
  DimInst
dimMapping t1 t2 = execState (matchDims onDims t1 t2) mempty
  where
    onDims bound (SizeExpr e1) (SizeExpr e2) = do
      onExps bound e1 e2
      pure $ SizeExpr e1
    onDims _ d _ = pure d

    onExps bound (Var v _ _) e
      | not $ any (`elem` bound) $ freeVarsInExp e =
          modify $ M.insert (qualLeaf v) $ SizeExpr e
    onExps bound e1 e2
      | Just es <- similarExps e1 e2 =
          mapM_ (uncurry $ onExps bound) es
    onExps _ _ _ = pure mempty

    freeVarsInExp = M.keys . unFV . freeInExp

inferSizeArgs :: [TypeParam] -> StructType -> StructType -> [Exp]
inferSizeArgs tparams bind_t t =
  mapMaybe (tparamArg (dimMapping bind_t t)) tparams
  where
    tparamArg dinst tp =
      case M.lookup (typeParamName tp) dinst of
        -- do we really need to make a Literal from the IntLit ?
        Just (SizeExpr (IntLit x _ _)) ->
          Just $ Literal (SignedValue $ Int64Value $ fromIntegral x) mempty
        Just (SizeExpr e) ->
          Just e
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
    f' (Arrow () _ d1 t1 (RetType dims t2)) =
      Arrow () Unnamed d1 (f t1) (RetType dims (f t2))
    f' (Record fs) =
      Record $ fmap f fs
    f' (Sum cs) =
      Sum $ fmap (map f) cs
    f' t = t

transformRetType :: StructRetType -> MonoM StructRetType
transformRetType (RetType ext t) = RetType ext <$> transformTypeSizes t

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
    let bind_t = funType params rettype
    (substs, t_shape_params) <-
      typeSubstsM loc (noSizes bind_t) $ noNamedParams inst_t
    let substs' = M.map (Subst []) substs
    rettype' <- applySubst (`M.lookup` substs') <$> transformRetType rettype
    let substPatType =
          substTypesAny (fmap (fmap (second (const mempty))) . (`M.lookup` substs'))
        params' = map (substPat entry substPatType) params
    bind_t' <- substTypesAny (`M.lookup` substs') <$> transformTypeSizes bind_t
    let (shape_params_explicit, shape_params_implicit) =
          partition ((`S.member` mustBeExplicitInBinding bind_t') . typeParamName) $
            shape_params ++ t_shape_params

    (params'', rrs) <- mapAndUnzipM transformPat params'

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
      unless (baseTag (qualLeaf v) <= maxIntrinsicTag) $
        addSubst v rt
    subRet t1 (RetType _ t2) =
      sub t1 t2

    sub t1@Array {} t2@Array {}
      | Just t1' <- peelArray (arrayRank t1) t1,
        Just t2' <- peelArray (arrayRank t1) t2 =
          sub t1' t2'
    sub (Scalar (TypeVar _ _ v _)) t =
      unless (baseTag (qualLeaf v) <= maxIntrinsicTag) $
        addSubst v $
          RetType [] t
    sub (Scalar (Record fields1)) (Scalar (Record fields2)) =
      zipWithM_
        sub
        (map snd $ sortFields fields1)
        (map snd $ sortFields fields2)
    sub (Scalar Prim {}) (Scalar Prim {}) = pure ()
    sub (Scalar (Arrow _ _ _ t1a (RetType _ t1b))) (Scalar (Arrow _ _ _ t2a t2b)) = do
      sub t1a t2a
      subRet t1b t2b
    sub (Scalar (Sum cs1)) (Scalar (Sum cs2)) =
      zipWithM_ typeSubstClause (sortConstrs cs1) (sortConstrs cs2)
      where
        typeSubstClause (_, ts1) (_, ts2) = zipWithM sub ts1 ts2
    sub t1@(Scalar Sum {}) t2 = sub t1 t2
    sub t1 t2@(Scalar Sum {}) = sub t1 t2
    sub t1 t2 = error $ unlines ["typeSubstsM: mismatched types:", prettyString t1, prettyString t2]

    addSubst (QualName _ v) (RetType ext t) = do
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
          pure $ sizeFromName (qualName d) mempty
        Just d ->
          pure $ sizeFromName (qualName d) mempty
    onDim (MonoAnon v) = pure $ AnySize $ Just v

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

transformEntryPoint :: EntryPoint -> MonoM EntryPoint
transformEntryPoint (EntryPoint params ret) =
  EntryPoint <$> mapM onEntryParam params <*> onEntryType ret
  where
    onEntryParam (EntryParam v t) =
      EntryParam v <$> onEntryType t
    onEntryType (EntryType t te) =
      EntryType <$> removeTypeVariablesInType t <*> pure te

transformValBind :: ValBind -> MonoM Env
transformValBind valbind = do
  valbind' <-
    toPolyBinding
      <$> removeTypeVariables (isJust (valBindEntryPoint valbind)) valbind

  case valBindEntryPoint valbind of
    Nothing -> pure ()
    Just (Info entry) -> do
      t <-
        removeTypeVariablesInType $
          funType (valBindParams valbind) $
            unInfo $
              valBindRetType valbind
      (name, infer, valbind'') <- monomorphiseBinding True valbind' $ monoType t
      entry' <- transformEntryPoint entry
      tell $ Seq.singleton (name, valbind'' {valBindEntryPoint = Just $ Info entry'})
      addLifted (valBindName valbind) (monoType t) (name, infer)

  pure mempty {envPolyBindings = M.singleton (valBindName valbind) valbind'}

transformTypeBind :: TypeBind -> MonoM Env
transformTypeBind (TypeBind name l tparams _ (Info (RetType dims t)) _ _) = do
  subs <- asks $ M.map substFromAbbr . envTypeBindings
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
      ++ prettyString dec

-- | Monomorphise a list of top-level declarations. A module-free input program
-- is expected, so only value declarations and type declaration are accepted.
transformProg :: MonadFreshNames m => [Dec] -> m [ValBind]
transformProg decs =
  fmap (toList . fmap snd . snd) $
    modifyNameSource $ \namesrc ->
      runMonoM namesrc $ transformDecs decs
