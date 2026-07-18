-- | This monomorphization module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent monomorphic program.
--
-- This pass also does a few other simplifications to make the job of
-- subsequent passes easier.  Specifically, it does the following:
--
-- * Turn operator sections into explicit lambdas.
--
-- * Converts applications of intrinsic SOACs into SOAC AST nodes
--   (Map, Reduce, etc).
--
-- * Elide functions that are not reachable from an entry point (this
--   is a side effect of the monomorphisation algorithm, which uses
--   the entry points as roots).
--
-- * Rewrite BinOp nodes to Apply nodes.
--
-- * Replace all size expressions by constants or variables,
--   complex expressions replaced by variables are calculated in
--   let binding or replaced by size parameters if in argument.
--
-- Note that these changes are unfortunately not visible in the AST
-- representation.
module Futhark.Internalise.Monomorphise
  ( transformProg,
    MonoType,
    MonoStats,
  )
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Function
import Data.List (intersperse, partition, sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S
import Futhark.MonadFreshNames
import Futhark.Util (nubOrd, topologicalSort)
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

i64 :: TypeBase dim als
i64 = Scalar $ Prim $ Signed Int64

-- The monomorphization monad reads 'PolyBinding's and writes
-- 'ValBind's.  The 'TypeParam's in the 'ValBind's can only be size
-- parameters.
newtype PolyBinding
  = PolyBinding
      ( Maybe EntryPoint,
        VName,
        [TypeParam],
        [Pat ParamType],
        ResRetType,
        Exp,
        [AttrInfo VName],
        SrcLoc
      )

-- | To deduplicate size expressions, we want a looser notion of
-- equality than the strict syntactical equality provided by the Eq
-- instance on Exp. This newtype wrapper provides such a looser notion
-- of equality.
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
canCalculate scope mapping = do
  filter
    ( (`S.isSubsetOf` scope)
        . S.filter (not . isIntrinsic)
        . fvVars
        . freeInExp
        . unReplaced
        . fst
    )
    mapping

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
    opt = foldFunType [bool, bool] $ RetType [] bool
    andop = Var (qualName (intrinsicVar "&&")) (Info opt) mempty
    eqop = Var (qualName (intrinsicVar "==")) (Info opt) mempty
    logAnd x' y =
      mkApply andop [(Nothing, x'), (Nothing, y)] $
        AppRes bool []
    cmpExp (ReplacedExp x', y) =
      mkApply eqop [(Nothing, x'), (Nothing, y')] $
        AppRes bool []
      where
        y' = Var (qualName y) (Info i64) mempty

-- Monomorphization environment mapping names of polymorphic functions
-- to a representation of their corresponding function bindings.
data Env = Env
  { envPolyBindings :: M.Map VName PolyBinding,
    envScope :: S.Set VName,
    envGlobalScope :: S.Set VName,
    envParametrized :: ExpReplacements
  }

isolateNormalisation :: MonoM a -> MonoM a
isolateNormalisation m = do
  prevRepl <- getExpReplacements
  putExpReplacements mempty
  ret <-
    local
      ( \env ->
          env
            { envScope = envGlobalScope env <> M.keysSet (envPolyBindings env),
              envParametrized = mempty
            }
      )
      m
  putExpReplacements prevRepl
  pure ret

-- | These now have monomorphic types in the given action. This is
-- used to handle shadowing.
withMono :: [VName] -> MonoM a -> MonoM a
withMono [] = id
withMono vs = local $ \env ->
  env {envPolyBindings = M.filterWithKey keep (envPolyBindings env)}
  where
    keep v _ = v `notElem` vs

withArgs :: S.Set VName -> MonoM a -> MonoM a
withArgs args = local $ \env -> env {envScope = args <> envScope env}

withParams :: ExpReplacements -> MonoM a -> MonoM a
withParams params = local $ \env -> env {envParametrized = params <> envParametrized env}

-- Mapping from function name and instance list to a new function name in case
-- the function has already been instantiated with those concrete types.
type Lifts = M.Map (VName, MonoType) (VName, InferSizeArgs)

data MonoState = MonoState
  { sVNameSource :: !VNameSource,
    sExpReplacements :: ExpReplacements,
    sAnySizes :: [(ReplacedExp, Int)],
    sLifts :: Lifts,
    sLiftedNames :: S.Set VName,
    sValBinds :: [ValBind]
  }

-- The monomorphization monad.
newtype MonoM a
  = MonoM (ReaderT Env (State MonoState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState MonoState
    )

instance MonadFreshNames MonoM where
  getNameSource = gets sVNameSource
  putNameSource src = modify $ \s -> s {sVNameSource = src}

runMonoM :: VNameSource -> MonoM () -> (([ValBind], Lifts), VNameSource)
runMonoM src (MonoM m) =
  ( (reverse (sValBinds final_state), sLifts final_state),
    sVNameSource final_state
  )
  where
    ((), final_state) = runState (runReaderT m initial_env) initial_state
    initial_state = MonoState src mempty mempty mempty mempty mempty
    initial_env = Env mempty mempty mempty mempty

lookupFun :: VName -> MonoM (Maybe PolyBinding)
lookupFun vn = do
  env <- asks envPolyBindings
  case M.lookup vn env of
    Just valbind -> pure $ Just valbind
    Nothing -> pure Nothing

addValBind :: ValBind -> MonoM ()
addValBind funbind =
  modify $ \s -> s {sValBinds = funbind : sValBinds s}

askScope :: MonoM (S.Set VName)
askScope = do
  scope <- asks envScope
  gets $ S.union scope . sLiftedNames

getExpReplacements :: MonoM ExpReplacements
getExpReplacements = gets sExpReplacements

putExpReplacements :: ExpReplacements -> MonoM ()
putExpReplacements x = modify $ \s -> s {sExpReplacements = x}

getLifts :: MonoM Lifts
getLifts = gets sLifts

addLifted :: VName -> MonoType -> (VName, InferSizeArgs) -> MonoM ()
addLifted fname il liftf =
  modify $ \s ->
    s
      { sLifts = M.insert (fname, il) liftf (sLifts s),
        sLiftedNames = S.insert (fst liftf) $ sLiftedNames s
      }

lookupLifted :: VName -> MonoType -> MonoM (Maybe (VName, InferSizeArgs))
lookupLifted fname t = M.lookup (fname, t) <$> getLifts

-- | Asks the introduced variables in a set of argument,
-- that is arguments not currently in scope.
askIntros :: S.Set VName -> MonoM (S.Set VName)
askIntros argset =
  (S.filter (not . isIntrinsic) argset `S.difference`) <$> askScope

-- | Gets and removes expressions that could not be calculated when
-- the arguments set will be unscoped.
-- This should be called without argset in scope, for good detection of intros.
parametrizing :: S.Set VName -> MonoM ExpReplacements
parametrizing argset = do
  intros <- askIntros argset
  let usesIntros = not . S.disjoint intros . fvVars . freeInExp
  (params, nxtBind) <-
    partition (usesIntros . unReplaced . fst) <$> getExpReplacements
  putExpReplacements nxtBind
  pure params

calculateDims :: Exp -> ExpReplacements -> MonoM Exp
calculateDims body repl =
  foldCalc top_repl $ expReplace top_repl body
  where
    depends (a, _) (b, _) = unReplaced b `elem` subExps (unReplaced a)
    top_repl = topologicalSort depends repl

    ---- Calculus insertion
    foldCalc [] body' = pure body'
    foldCalc ((dim, vn) : repls) body' = do
      reName <- newName vn
      let expr = expReplace repls $ unReplaced dim
          subst vn' =
            if vn' == vn
              then Just $ ExpSubst $ sizeFromName (qualName reName) mempty
              else Nothing
          appRes = case body' of
            (AppExp _ (Info (AppRes ty ext))) -> Info $ AppRes (applySubst subst ty) (reName : ext)
            e -> Info $ AppRes (applySubst subst $ typeOf e) [reName]
      foldCalc repls $
        AppExp
          ( LetPat
              []
              (Id vn (Info i64) (srclocOf expr))
              expr
              body'
              mempty
          )
          appRes

unscoping :: S.Set VName -> Exp -> MonoM Exp
unscoping argset body = do
  localDims <- parametrizing argset
  scope <- S.union argset <$> askScope
  calculateDims body $ canCalculate scope localDims

scoping :: S.Set VName -> MonoM Exp -> MonoM Exp
scoping argset m =
  withArgs argset m >>= unscoping argset

-- Given instantiated type of function, produce size arguments.
type InferSizeArgs = StructType -> MonoM [Exp]

-- | The integer encodes an equivalence class, so we can keep
-- track of sizes that are statically identical.
data MonoSize
  = MonoKnown Int
  | MonoAnon Int
  deriving (Eq, Ord, Show)

instance Pretty MonoSize where
  pretty (MonoKnown i) = "?" <> pretty i
  pretty (MonoAnon i) = "??" <> pretty i

instance Pretty (Shape MonoSize) where
  pretty (Shape ds) = mconcat (map (brackets . pretty) ds)

-- | The kind of type relative to which we monomorphise. What is most important
-- to us is not the specific dimensions, but merely whether they are known or
-- anonymous/local.
type MonoType = TypeBase MonoSize NoUniqueness

monoType :: TypeBase Size als -> MonoType
monoType = noExts . (`evalState` (0, mempty)) . traverseDims onDim . toStruct
  where
    -- Remove exts from return types because we don't use them anymore.
    noExts :: TypeBase MonoSize u -> TypeBase MonoSize u
    noExts (Array u shape t) = Array u shape $ noExtsScalar t
    noExts (Scalar t) = Scalar $ noExtsScalar t
    noExtsScalar (Record fs) = Record $ M.map noExts fs
    noExtsScalar (Sum fs) = Sum $ M.map (map noExts) fs
    noExtsScalar (Arrow as p d t1 (RetType _ t2)) =
      Arrow as p d (noExts t1) (RetType [] (noExts t2))
    noExtsScalar t = t
    onDim bound _ d
      -- A locally bound size.
      | any (`S.member` bound) $ fvVars $ freeInExp d = do
          (i, m) <- get
          case M.lookup d m of
            Just prev ->
              pure $ MonoAnon prev
            Nothing -> do
              put (i + 1, M.insert d i m)
              pure $ MonoAnon i
    onDim _ _ d = do
      (i, m) <- get
      case M.lookup d m of
        Just prev ->
          pure $ MonoKnown prev
        Nothing -> do
          put
            ( i + 1,
              M.insert d i m
            )
          pure $ MonoKnown i

sizeVarName :: Exp -> Name
sizeVarName e = "d<{" <> nameFromText (prettyText (bareExp e)) <> "}>"

-- | Is this size expression already normalised (a single variable or
-- constant), modulo being surrounded by some parentheses? If so, return the
-- normalised form.
maybeNormalisedSize :: Exp -> Maybe Exp
maybeNormalisedSize e
  | Just e' <- stripExp e = maybeNormalisedSize e'
maybeNormalisedSize (Var qn _ loc) = Just $ sizeFromName qn loc
maybeNormalisedSize (IntLit v _ loc) = Just $ IntLit v (Info i64) loc
maybeNormalisedSize _ = Nothing

-- | Creates a new expression replacement if needed, this always produces normalised sizes.
-- (e.g. single variable or constant)
replaceExp :: Exp -> MonoM Exp
replaceExp e =
  case maybeNormalisedSize e of
    Just e' -> pure e'
    Nothing -> do
      let e' = ReplacedExp e
      prev <- lookup e' <$> getExpReplacements
      prev_param <- asks $ lookup e' . envParametrized
      case (prev_param, prev) of
        (Just vn, _) -> pure $ sizeFromName (qualName vn) (srclocOf e)
        (Nothing, Just vn) -> pure $ sizeFromName (qualName vn) (srclocOf e)
        (Nothing, Nothing) -> do
          vn <- newVName $ sizeVarName e
          putExpReplacements . ((e', vn) :) =<< getExpReplacements
          pure $ sizeFromName (qualName vn) (srclocOf e)

-- | Turn a non-trivial size expression occurring in a *type* into an
-- 'anySize', rather than trying to hoist it into a named size parameter that
-- 'inferSizeArgs' would later have to (fallibly) reconstruct a concrete
-- argument for at every reference to the function. Some such expressions
-- (e.g. a product of several of the function's own parameters, as in #2230)
-- cannot in general be recovered at the point of reference, so instead of
-- pretending we can, we erase the information here and let internalisation
-- resolve it from the actual sizes of the values involved, exactly as is
-- already done for other 'anySize's (see Note [AnySize]).
--
-- Repeated occurrences of the same expression are mapped to the same
-- equivalence class, so that size equalities implied by the original
-- expression are not lost.
anySizeForType :: Exp -> MonoM Exp
anySizeForType e = do
  let e' = ReplacedExp e
  substs <- gets sAnySizes
  case lookup e' substs of
    Just i -> pure $ anySize i
    Nothing -> do
      i <- baseTag <$> newVName "any_size"
      modify $ \s -> s {sAnySizes = (e', i) : substs}
      pure $ anySize i

transformFName :: SrcLoc -> QualName VName -> StructType -> MonoM Exp
transformFName loc fname ft = do
  -- The type 't' is later matched structurally, syntactically, against the
  -- callee's own declared signature (in 'inferSizeArgs', via 'dimMapping'),
  -- so it must stay fully precise here, even if we are currently inside the
  -- return type of some outer higher-order parameter.
  t' <- transformFNameType ft
  let mono_t = monoType ft
  if isIntrinsic (qualLeaf fname)
    then pure $ var fname t'
    else do
      maybe_fname <- lookupLifted (qualLeaf fname) mono_t
      maybe_funbind <- lookupFun $ qualLeaf fname
      case (maybe_fname, maybe_funbind) of
        -- The function has already been monomorphised.
        (Just (fname', infer), _) ->
          applySizeArgs fname' (toRes Nonunique t') <$> infer t'
        -- An intrinsic function.
        (Nothing, Nothing) -> pure $ var fname t'
        -- A polymorphic function.
        (Nothing, Just funbind) -> do
          (fname', infer) <- monomorphiseBinding funbind mono_t
          applySizeArgs fname' (toRes Nonunique t') <$> infer t'
  where
    var fname' t' = Var fname' (Info t') loc

    applySizeArg t (i, f) size_arg =
      ( i - 1,
        mkApply
          f
          [(Nothing, size_arg)]
          (AppRes (foldFunType (replicate i i64) (RetType [] t)) [])
      )

    applySizeArgs fname' t size_args =
      setApplyLoc loc . snd $
        foldl'
          (applySizeArg t)
          ( length size_args - 1,
            Var
              (qualName fname')
              (Info (foldFunType (map (const i64) size_args) (RetType [] t)))
              loc
          )
          size_args

-- | General-purpose size transformation for a type, used everywhere except when
-- reconstructing the type of a function reference (see 'transformFNameType').
-- Sizes immediately in the type (before any 'Arrow' is crossed) are hoisted
-- into a reference to a fresh named size variable (see 'replaceExp'), while
-- sizes found within a higher-order (function-valued) parameter or return
-- type - i.e. anywhere 'traverseDims' reports a 'DimPos' other than
-- 'PosImmediate' - are instead erased into an 'anySize'. Whether such a
-- hoisted name ends up as an actual explicit parameter of the enclosing
-- function, an ordinary local @let@-binding, or something else again,
-- depends on what the caller of 'transformType' does with the resulting
-- 'ExpReplacements' afterwards; 'transformType' itself does not add any
-- parameters. See Note [Higher-Order Parameter Sizes].
transformType :: TypeBase Size u -> MonoM (TypeBase Size u)
transformType = traverseDims onDim
  where
    onDim _ pos e
      | Just _ <- isAnySize e = pure e
      | otherwise = do
          e' <- transformExp e
          case maybeNormalisedSize e' of
            Just e'' -> pure e''
            Nothing
              | PosImmediate <- pos -> replaceExp e'
              | otherwise -> anySizeForType e'

-- | Like 'transformType', but replaces every non-trivial size, regardless of
-- where it occurs, with a reference to a fresh named size variable (see
-- 'replaceExp'), rather than erasing sizes found within higher-order
-- parameter/return types into an 'anySize'. Note that this does not turn any
-- of these sizes into an actual parameter of anything: the resulting type has
-- exactly the same 'Arrow' structure (and hence the same arity) as the input.
-- The named variables are merely placeholders, used only so that
-- 'dimMapping' can structurally line this reconstructed type up against the
-- callee's own declared signature - whose *own* analogous sizes did, when
-- the callee itself was monomorphised, become real explicit parameters - so
-- that 'inferSizeArgs' can read off a concrete argument expression for each
-- of those parameters. Used only to reconstruct the type of a plain function
-- *reference* (in 'transformFName'); that is why it must stay fully precise,
-- even through further nested 'Arrow's - which, for this particular type,
-- are simply the callee's own remaining curried parameters, not necessarily
-- higher-order values. This also means we cannot use 'traverseDims' here
-- (unlike in 'transformType'): a size hoisted from deep within this type may
-- refer to a curried parameter bound by some enclosing 'Arrow' in the very
-- same type, so each 'Arrow' we cross must, before returning, re-scope any
-- such size locally to its own return type (via 'transformRetTypeSizesWith').
-- See Note [Higher-Order Parameter Sizes].
transformFNameType :: TypeBase Size u -> MonoM (TypeBase Size u)
transformFNameType typ =
  case typ of
    Scalar scalar ->
      Scalar <$> transformScalarSizes scalar
    Array u shape scalar ->
      Array u <$> mapM onDim shape <*> transformScalarSizes scalar
  where
    transformScalarSizes :: ScalarTypeBase Size u -> MonoM (ScalarTypeBase Size u)
    transformScalarSizes (Record fs) =
      Record <$> traverse transformFNameType fs
    transformScalarSizes (Sum cs) =
      Sum <$> (traverse . traverse) transformFNameType cs
    transformScalarSizes (Arrow as argName d argT retT) =
      Arrow as argName d
        <$> transformFNameType argT
        <*> transformRetTypeSizesWith transformFNameType argset retT
      where
        argset =
          case argName of
            Unnamed -> mempty
            Named vn -> S.singleton vn
    transformScalarSizes (TypeVar u qn args) =
      TypeVar u qn <$> mapM onArg args
      where
        onArg (TypeArgDim dim) = TypeArgDim <$> onDim dim
        onArg (TypeArgType ty) = TypeArgType <$> transformFNameType ty
    transformScalarSizes ty@Prim {} = pure ty

    onDim e
      | Just _ <- isAnySize e = pure e
      | otherwise = replaceExp =<< transformExp e

-- | Transform the sizes of a return type using the given size transformer,
-- given the set of parameter names newly in scope for this return type (usually
-- the name of the 'Arrow' parameter it is the return type of). Any size hoisted
-- while transforming that turns out to depend on one of these parameters cannot
-- be a top-level named parameter (those names are not in scope outside), so it
-- is instead added to the return type's own existentially-bound sizes.
transformRetTypeSizesWith ::
  (TypeBase Size as -> MonoM (TypeBase Size as)) ->
  S.Set VName ->
  RetTypeBase Size as ->
  MonoM (RetTypeBase Size as)
transformRetTypeSizesWith f argset (RetType dims ty) = do
  ty' <- withArgs argset $ withMono dims $ f ty
  rl <- parametrizing argset
  let dims' = dims <> map snd rl
  pure $ RetType dims' ty'

transformRetTypeSizes :: S.Set VName -> RetTypeBase Size as -> MonoM (RetTypeBase Size as)
transformRetTypeSizes = transformRetTypeSizesWith transformType

sizesForPat :: (MonadFreshNames m) => Pat ParamType -> m ([VName], Pat ParamType)
sizesForPat pat = do
  (params', sizes) <- runStateT (traverse (bitraverse onDim pure) pat) []
  pure (map snd sizes, params')
  where
    onDim d
      | Just k <- isAnySize d = do
          prev <- gets $ lookup k
          case prev of
            Nothing -> do
              v <- lift $ newVName "size"
              modify ((k, v) :)
              pure $ sizeFromName (qualName v) mempty
            Just v ->
              pure $ sizeFromName (qualName v) mempty
      | otherwise = pure d

transformAppRes :: AppRes -> MonoM AppRes
transformAppRes (AppRes t ext) =
  AppRes <$> transformType t <*> pure ext

transformAppExp :: AppExp -> AppRes -> MonoM Exp
transformAppExp (Range e1 me incl loc) res = do
  e1' <- transformExp e1
  me' <- mapM transformExp me
  incl' <- mapM transformExp incl
  res' <- transformAppRes res
  pure $ AppExp (Range e1' me' incl' loc) (Info res')
transformAppExp (LetPat sizes pat e body loc) res = do
  e' <- transformExp e
  let dimArgs = S.fromList (map sizeName sizes)
  implicitDims <- withArgs dimArgs $ askIntros $ fvVars $ freeInPat pat
  let dimArgs' = dimArgs <> implicitDims
      letArgs = S.fromList $ patNames pat
      argset = dimArgs' `S.union` letArgs
  pat' <- withArgs dimArgs' $ transformPat pat
  params <- parametrizing dimArgs'
  let sizes' = sizes <> map (`SizeBinder` mempty) (map snd params <> S.toList implicitDims)
  body' <- withParams params $ scoping argset $ transformExp body
  res' <- transformAppRes res
  pure $ AppExp (LetPat sizes' pat' e' body' loc) (Info res')
transformAppExp LetFun {} _ =
  error "transformAppExp: LetFun is not supposed to occur"
transformAppExp (If e1 e2 e3 loc) res =
  AppExp <$> (If <$> transformExp e1 <*> transformExp e2 <*> transformExp e3 <*> pure loc) <*> (Info <$> transformAppRes res)
transformAppExp (Apply fe args loc) res =
  setApplyLoc loc
    <$> (mkApply <$> transformExp fe <*> mapM onArg (NE.toList args) <*> transformAppRes res)
  where
    onArg (Info ext, e) = (ext,) <$> transformExp e
transformAppExp (Loop sparams pat loopinit form body loc) res = do
  e1' <- transformExp $ loopInitExp loopinit

  let dimArgs = S.fromList sparams
  pat' <- withArgs dimArgs $ transformPat pat
  params <- parametrizing dimArgs
  let sparams' = sparams <> map snd params
      mergeArgs = dimArgs `S.union` S.fromList (patNames pat)

  (form', formArgs) <- case form of
    For ident e2 -> (,S.singleton $ identName ident) . For ident <$> transformExp e2
    ForIn pat2 e2 -> do
      pat2' <- transformPat pat2
      (,S.fromList (patNames pat2)) . ForIn pat2' <$> transformExp e2
    While e2 ->
      fmap ((,mempty) . While) $
        withParams params $
          scoping mergeArgs $
            transformExp e2
  let argset = mergeArgs `S.union` formArgs

  body' <- withParams params $ scoping argset $ transformExp body
  -- Maybe monomorphisation introduced new arrays to the loop, and
  -- maybe they have AnySize sizes.  This is not allowed.  Invent some
  -- sizes for them.
  (pat_sizes, pat'') <- sizesForPat pat'
  res' <- transformAppRes res
  pure $ AppExp (Loop (sparams' ++ pat_sizes) pat'' (LoopInitExplicit e1') form' body' loc) (Info res')
transformAppExp (BinOp (fname, _) (Info t) (e1, d1) (e2, d2) loc) res = do
  (AppRes ret ext) <- transformAppRes res
  fname' <- transformFName loc fname (toStruct t)
  e1' <- transformExp e1
  e2' <- transformExp e2
  if orderZero (typeOf e1') && orderZero (typeOf e2')
    then pure $ applyOp ret ext fname' e1' e2'
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
                  (LetPat [] y_param e2' (applyOp ret ext fname' x_param_e y_param_e) loc)
                  (Info $ AppRes ret mempty)
              )
              mempty
          )
          (Info (AppRes ret mempty))
  where
    applyOp ret ext fname' x y =
      mkApply
        (mkApply fname' [(unInfo d1, x)] (AppRes ret mempty))
        [(unInfo d2, y)]
        (AppRes ret ext)

    makeVarParam arg = do
      let argtype = typeOf arg
      x <- newVName "binop_p"
      pure
        ( Var (qualName x) (Info argtype) mempty,
          Id x (Info argtype) mempty
        )
transformAppExp LetWith {} _ =
  error "transformAppExp: LetWith is not supposed to occur"
transformAppExp (Index e0 idxs loc) res =
  AppExp
    <$> (Index <$> transformExp e0 <*> mapM transformDimIndex idxs <*> pure loc)
    <*> (Info <$> transformAppRes res)
transformAppExp (Match e cs loc) res = do
  implicitDims <- askIntros $ fvVars $ freeInType $ typeOf e
  e' <- transformExp e
  cs' <- mapM (transformCase implicitDims) cs
  res' <- transformAppRes res
  if S.null implicitDims
    then pure $ AppExp (Match e' cs' loc) (Info res')
    else do
      tmpVar <- newVName "matched_variable"
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
          (Info res')

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
    transformField (RecordFieldImplicit (L vloc v) t _) = do
      t' <- traverse transformType t
      transformField $
        RecordFieldExplicit
          (L vloc (baseName v))
          (Var (qualName v) t' loc)
          loc
transformExp (ArrayVal vs t loc) =
  pure $ ArrayVal vs t loc
transformExp (ArrayLit es t loc) =
  ArrayLit <$> mapM transformExp es <*> traverse transformType t <*> pure loc
transformExp (AppExp e res) =
  transformAppExp e (unInfo res)
transformExp (Var fname (Info t) loc) =
  transformFName loc fname (toStruct t)
transformExp (Hole t loc) =
  Hole <$> traverse transformType t <*> pure loc
transformExp (Ascript e tp loc) =
  Ascript <$> transformExp e <*> pure tp <*> pure loc
transformExp (Coerce e te t loc) =
  Coerce <$> transformExp e <*> pure te <*> traverse transformType t <*> pure loc
transformExp (Negate e loc) =
  Negate <$> transformExp e <*> pure loc
transformExp (Not e loc) =
  Not <$> transformExp e <*> pure loc
transformExp (Lambda {}) =
  error "transformExp: Lambda is not supposed to occur"
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
transformExp (UpdateSection steps (Info t) loc) = do
  t' <- transformType t
  steps' <- mapM transformStep steps
  desugarUpdateSection steps' t' loc
  where
    transformStep (UpdateStepSlice idxs) =
      UpdateStepSlice <$> mapM transformDimIndex idxs
    transformStep (UpdateStepField f) =
      pure $ UpdateStepField f
transformExp (Project n e tp loc) = do
  tp' <- traverse transformType tp
  e' <- transformExp e
  pure $ Project n e' tp' loc
transformExp (Update e1 steps e2 t loc) =
  Update
    <$> transformExp e1
    <*> mapM transformStep steps
    <*> transformExp e2
    <*> traverse transformType t
    <*> pure loc
  where
    transformStep (UpdateStepSlice idxs) =
      UpdateStepSlice <$> mapM transformDimIndex idxs
    transformStep (UpdateStepField f) =
      pure $ UpdateStepField f
transformExp (Assert e1 e2 desc loc) =
  Assert <$> transformExp e1 <*> transformExp e2 <*> pure desc <*> pure loc
transformExp (Constr name all_es t loc) =
  Constr name <$> mapM transformExp all_es <*> traverse transformType t <*> pure loc
transformExp (Attr info e loc) =
  Attr info <$> transformExp e <*> pure loc

transformCase :: S.Set VName -> Case -> MonoM Case
transformCase implicitDims (CasePat p e loc) = do
  p' <- transformPat p
  CasePat p' <$> scoping (S.fromList (patNames p) `S.union` implicitDims) (transformExp e) <*> pure loc

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
  StructType ->
  (PName, ParamType, Maybe VName) ->
  (PName, ParamType, Maybe VName) ->
  (ResRetType, [VName]) ->
  SrcLoc ->
  MonoM Exp
desugarBinOpSection fname e_left e_right t (xp, xtype, xext) (yp, ytype, yext) (RetType dims rettype, retext) loc = do
  t' <- transformType t
  op <- transformFName loc fname $ toStruct t
  (v1, wrap_left, e1, p1) <- makeVarParam e_left =<< transformType xtype
  (v2, wrap_right, e2, p2) <- makeVarParam e_right =<< transformType ytype
  let apply_left =
        mkApply
          op
          [(xext, e1)]
          (AppRes (Scalar $ Arrow mempty yp (diet ytype) (toStruct ytype) (RetType [] $ toRes Nonunique t')) [])
      onDim (Var d typ _)
        | Named p <- xp, qualLeaf d == p = Var (qualName v1) typ loc
        | Named p <- yp, qualLeaf d == p = Var (qualName v2) typ loc
      onDim d = d
      rettype' = first onDim rettype
  body <-
    scoping (S.fromList [v1, v2]) $
      mkApply apply_left [(yext, e2)]
        <$> transformAppRes (AppRes (toStruct rettype') retext)
  rettype'' <- transformRetTypeSizes (S.fromList [v1, v2]) $ RetType dims rettype'
  pure . wrap_left . wrap_right $
    Lambda (p1 ++ p2) body Nothing (Info rettype'') loc
  where
    patAndVar argtype = do
      x <- newVName "x"
      pure
        ( x,
          Id x (Info argtype) mempty,
          Var (qualName x) (Info (toStruct argtype)) mempty
        )

    makeVarParam (Just e) argtype = do
      (v, pat, var_e) <- patAndVar argtype
      let wrap body =
            AppExp (LetPat [] (fmap toStruct pat) e body mempty) (Info $ AppRes (typeOf body) mempty)
      pure (v, wrap, var_e, [])
    makeVarParam Nothing argtype = do
      (v, pat, var_e) <- patAndVar argtype
      pure (v, id, var_e, [pat])

desugarUpdateSection :: [UpdateStep Info VName] -> StructType -> SrcLoc -> MonoM Exp
desugarUpdateSection steps (Scalar (Arrow _ _ _ t1 (RetType dims t2))) loc = do
  p <- newVName "section_p"
  let body = fst $ foldl applyStep (Var (qualName p) (Info t1) mempty, t1) steps
  pure $
    Lambda
      [Id p (Info $ toParam Observe t1) mempty]
      body
      Nothing
      (Info (RetType dims t2))
      loc
  where
    applyStep (e, t) (UpdateStepField field) =
      case t of
        Scalar (Record fs)
          | Just t' <- M.lookup field fs ->
              (Project field e (Info t') mempty, t')
        _ ->
          error $
            "desugarUpdateSection: type "
              ++ prettyString t
              ++ " does not have field "
              ++ prettyString field
    applyStep (e, t) (UpdateStepSlice idxs) =
      let t' = stripArray (fixedDims idxs) t
          e' = AppExp (Index e idxs loc) (Info (AppRes t' []))
       in (e', t')

    fixedDims = length . filter isFix
    isFix DimFix {} = True
    isFix _ = False
desugarUpdateSection _ t _ = error $ "desugarUpdateSection: not a function type: " ++ prettyString t

transformPat :: Pat (TypeBase Size u) -> MonoM (Pat (TypeBase Size u))
transformPat = traverse transformType

type DimInst = M.Map VName Size

dimMapping ::
  (Monoid a) =>
  TypeBase Size a ->
  TypeBase Size a ->
  ExpReplacements ->
  ExpReplacements ->
  DimInst
dimMapping t1 t2 r1 r2 = execState (matchDims onDims t1 t2) mempty
  where
    revMap = map (\(k, v) -> (v, k))
    named1 = revMap r1
    named2 = revMap r2

    onDims bound e1 e2 = do
      onExps bound e1 e2
      pure e1

    -- XXX: It is intentional that we throw away the 'bound'
    -- information after looking up ExpReplacements, as there are
    -- cases (particularly including the function types that occur
    -- after lambda lifting) where some troublesome shadowing occurs,
    -- and the names are not actually locally bound. We cannot just
    -- ignore the bound information entirely, and expect that any
    -- instantiation uses only names in scope at the outer level, due
    -- to truly exotic cases like entry-lifted.fut, where the real
    -- sizes were not visible to the type checker. Arguably that is
    -- the thing that should be fixed, but it requires fiddling with
    -- the defunctorisation of size-lifted types.

    onExps bound (Var v _ _) e = do
      unless (any (`elem` bound) $ freeVarsInExp e) $
        modify (M.insert (qualLeaf v) e)
      case lookup (qualLeaf v) named1 of
        Just rexp -> onExps mempty (unReplaced rexp) e
        Nothing -> pure ()
    onExps _bound e (Var v _ _)
      | Just rexp <- lookup (qualLeaf v) named2 =
          onExps mempty e (unReplaced rexp)
    onExps bound e1 e2
      | Just es <- similarExps e1 e2 =
          mapM_ (uncurry $ onExps bound) es
    onExps _ _ _ = pure mempty

    freeVarsInExp = fvVars . freeInExp

inferSizeArgs :: [TypeParam] -> StructType -> ExpReplacements -> StructType -> MonoM [Exp]
inferSizeArgs tparams bind_t bind_r t = do
  r <- (<>) <$> getExpReplacements <*> asks envParametrized
  let dinst = dimMapping bind_t t bind_r r
  mapM (tparamArg dinst) tparams
  where
    tparamArg dinst tp =
      case M.lookup (typeParamName tp) dinst of
        Just e
          -- In some cases we infer anySizes for size arguments. This
          -- only occurs when those sizes don't actually matter (knock
          -- on wood...), but we should never actually insert anySize
          -- as a concrete argument.
          | Nothing <- isAnySize e ->
              replaceExp e
        _ ->
          pure $ sizeFromInteger 0 mempty

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
    f :: TypeBase MonoSize u -> TypeBase MonoSize u
    f (Array u shape t) = Array u shape (f' t)
    f (Scalar t) = Scalar $ f' t
    f' :: ScalarTypeBase MonoSize u -> ScalarTypeBase MonoSize u
    f' (Record fs) = Record $ fmap f fs
    f' (Sum cs) = Sum $ fmap (map f) cs
    f' (Arrow u _ d1 t1 (RetType dims t2)) =
      Arrow u Unnamed d1 (f t1) (RetType dims (f t2))
    f' t = t

-- | arrowArg takes a return type and returns it
-- with the existentials bound moved at the right of arrows.
-- It also gives the new set of parameters to consider.
arrowArg ::
  S.Set VName -> -- scope
  S.Set VName -> -- set of argument
  [VName] -> -- size parameters
  RetTypeBase Size as ->
  (RetTypeBase Size as, S.Set VName)
arrowArg scope argset args_params rety =
  let (rety', (funArgs, _)) = runWriter (arrowArgRetType (scope, mempty) argset rety)
      new_params = funArgs `S.union` S.fromList args_params
   in (arrowCleanRetType new_params rety', new_params)
  where
    -- \| takes a type (or return type) and returns it
    -- with the existentials bound moved at the right of arrows.
    -- It also gives (through writer monad) size variables used in arrow arguments
    -- and variables that are constructively used.
    -- The returned type should be cleanned, as too many existentials are introduced.
    arrowArgRetType ::
      (S.Set VName, [VName]) ->
      S.Set VName ->
      RetTypeBase Size as' ->
      Writer (S.Set VName, S.Set VName) (RetTypeBase Size as')
    arrowArgRetType (scope', dimsToPush) argset' (RetType dims ty) = pass $ do
      let dims' = dims <> dimsToPush
      (ty', (_, canExt)) <- listen $ arrowArgType (argset' `S.union` scope', dims') ty
      pure (RetType (filter (`S.member` canExt) dims') ty', first (`S.difference` canExt))

    arrowArgScalar env (Record fs) =
      Record <$> traverse (arrowArgType env) fs
    arrowArgScalar env (Sum cs) =
      Sum <$> (traverse . traverse) (arrowArgType env) cs
    arrowArgScalar (scope', dimsToPush) (Arrow as argName d argT retT) =
      pass $ do
        let intros = S.filter (not . isIntrinsic) argset' `S.difference` scope'
        retT' <- arrowArgRetType (scope', filter (`S.notMember` intros) dimsToPush) fullArgset retT
        pure (Arrow as argName d argT retT', bimap (intros `S.union`) (const mempty))
      where
        argset' = fvVars $ freeInType argT
        fullArgset =
          case argName of
            Unnamed -> argset'
            Named vn -> S.insert vn argset'
    arrowArgScalar env (TypeVar u qn args) =
      TypeVar u qn <$> mapM arrowArgArg args
      where
        arrowArgArg (TypeArgDim dim) = TypeArgDim <$> arrowArgSize dim
        arrowArgArg (TypeArgType ty) = TypeArgType <$> arrowArgType env ty
    arrowArgScalar _ ty = pure ty

    arrowArgType ::
      (S.Set VName, [VName]) ->
      TypeBase Size as' ->
      Writer (S.Set VName, S.Set VName) (TypeBase Size as')
    arrowArgType env (Array u shape scalar) =
      Array u <$> traverse arrowArgSize shape <*> arrowArgScalar env scalar
    arrowArgType env (Scalar ty) =
      Scalar <$> arrowArgScalar env ty

    arrowArgSize s@(Var qn _ _) = writer (s, (mempty, S.singleton $ qualLeaf qn))
    arrowArgSize s = pure s

    -- \| arrowClean cleans the mess in the type
    arrowCleanRetType :: S.Set VName -> RetTypeBase Size as -> RetTypeBase Size as
    arrowCleanRetType paramed (RetType dims ty) =
      RetType (nubOrd $ filter (`S.notMember` paramed) dims) (arrowCleanType (paramed `S.union` S.fromList dims) ty)

    arrowCleanScalar :: S.Set VName -> ScalarTypeBase Size as -> ScalarTypeBase Size as
    arrowCleanScalar paramed (Record fs) =
      Record $ M.map (arrowCleanType paramed) fs
    arrowCleanScalar paramed (Sum cs) =
      Sum $ (M.map . map) (arrowCleanType paramed) cs
    arrowCleanScalar paramed (Arrow as argName d argT retT) =
      Arrow as argName d argT (arrowCleanRetType paramed retT)
    arrowCleanScalar paramed (TypeVar u qn args) =
      TypeVar u qn $ map arrowCleanArg args
      where
        arrowCleanArg (TypeArgDim dim) = TypeArgDim dim
        arrowCleanArg (TypeArgType ty) = TypeArgType $ arrowCleanType paramed ty
    arrowCleanScalar _ ty = ty

    arrowCleanType :: S.Set VName -> TypeBase Size as -> TypeBase Size as
    arrowCleanType paramed (Array u shape scalar) =
      Array u shape $ arrowCleanScalar paramed scalar
    arrowCleanType paramed (Scalar ty) =
      Scalar $ arrowCleanScalar paramed ty

removeEntryPoint :: PolyBinding -> PolyBinding
removeEntryPoint (PolyBinding (_, name, tparams, params, rettype, body, attrs, loc)) =
  PolyBinding (Nothing, name, tparams, params, rettype, body, attrs, loc)

-- Monomorphise a polymorphic function at the types given in the instance list.
-- Monomorphises the body of the function as well. Returns the fresh name of the
-- generated monomorphic function as well a function for constructing additional
-- size arguments.
monomorphiseBinding ::
  PolyBinding ->
  MonoType ->
  MonoM (VName, InferSizeArgs)
monomorphiseBinding (PolyBinding (entry, name, tparams, params, rettype, body, attrs, loc)) inst_t = isolateNormalisation $ do
  let bind_t = funType params rettype
  (substs, t_shape_params) <-
    typeSubstsM loc bind_t $ noNamedParams inst_t
  let shape_names = S.fromList $ map typeParamName $ shape_params ++ t_shape_params
      substs' = M.map (Subst []) substs
      substStructType =
        substTypesAny (fmap (fmap (second (const mempty))) . (`M.lookup` substs'))
      params' = map (substPat substStructType) params
  params'' <- withArgs shape_names $ mapM transformPat params'
  exp_naming <- getExpReplacements <* putExpReplacements mempty

  let args = S.fromList $ foldMap patNames params
      arg_params = map snd exp_naming

  rettype' <-
    withParams exp_naming $
      withArgs (args <> shape_names) $
        hardTransformRetType (applySubst (`M.lookup` substs') rettype)
  extNaming <- getExpReplacements <* putExpReplacements mempty
  scope <- S.union shape_names <$> askScope'
  let (rettype'', new_params) = arrowArg scope args arg_params rettype'
      bind_t' = substTypesAny (`M.lookup` substs') bind_t
      mkExplicit =
        flip
          S.member
          (mustBeExplicitInBinding bind_t'' <> mustBeExplicitInBinding bind_t')
      (shape_params_explicit, shape_params_implicit) =
        partition (mkExplicit . typeParamName) $
          shape_params ++ t_shape_params ++ map (`TypeParamDim` mempty) (S.toList new_params)
      exp_naming' = filter ((`S.member` new_params) . snd) (extNaming <> exp_naming)

      bind_t'' = funType params'' rettype''
      bind_r = exp_naming <> extNaming

  -- It is important that we record the lifted function before transforming the
  -- body, in order to handle recursion. Fortunately the "calling convention"
  -- does not depend on the body.
  seen_before <- elem name . map fst . M.keys <$> getLifts
  name' <-
    if null tparams && isNothing entry && not seen_before
      then pure name
      else newName name

  let infer =
        -- If the function is an entry point, then it cannot possibly
        -- need any explicit size arguments (checked by type checker).
        if isJust entry
          then const $ pure []
          else inferSizeArgs shape_params_explicit bind_t'' bind_r

  addLifted name inst_t (name', infer)

  body' <- updateExpTypes (`M.lookup` substs') body
  body'' <- withParams exp_naming' $ withArgs (shape_names <> args) $ transformExp body'
  scope' <- S.union (shape_names <> args) <$> askScope'
  body''' <-
    expReplace exp_naming' <$> (calculateDims body'' . canCalculate scope' =<< getExpReplacements)

  addValBind $
    if isJust entry
      then
        toValBinding
          name'
          (shape_params_explicit ++ shape_params_implicit)
          params''
          rettype''
          (entryAssert exp_naming body''')
      else
        toValBinding
          name'
          shape_params_implicit
          (map shapeParam shape_params_explicit ++ params'')
          rettype''
          body'''

  pure (name', infer)
  where
    askScope' = S.filter (`notElem` retDims rettype) <$> askScope

    shape_params = filter (not . isTypeParam) tparams

    updateExpTypes substs = astMap (mapper substs)

    hardTransformRetType (RetType dims ty) = do
      ty' <- transformType ty
      unbounded <- askIntros $ fvVars $ freeInType ty'
      let dims' = S.toList unbounded
      pure $ RetType (dims' <> dims) ty'

    mapper substs =
      ASTMapper
        { mapOnExp = updateExpTypes substs,
          mapOnName = pure,
          mapOnStructType = pure . applySubst substs,
          mapOnParamType = pure . applySubst substs,
          mapOnResRetType = pure . applySubst substs
        }

    shapeParam tp = Id (typeParamName tp) (Info i64) $ srclocOf tp

    toValBinding name' tparams' params'' rettype' body'' =
      ValBind
        { valBindEntryPoint = Info <$> entry,
          valBindName = name',
          valBindNameLoc = mempty,
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
  (MonadFreshNames m) =>
  SrcLoc ->
  StructType ->
  MonoType ->
  m (M.Map VName StructRetType, [TypeParam])
typeSubstsM loc orig_t1 orig_t2 =
  runWriterT $ fst <$> execStateT (sub orig_t1 orig_t2) (mempty, mempty)
  where
    subRet (Scalar (TypeVar _ v _)) rt =
      unless (isIntrinsic (qualLeaf v)) $
        addSubst v rt
    subRet t1 (RetType _ t2) =
      sub t1 t2

    sub t1@(Array _ (Shape (d1 : _)) _) t2@(Array _ (Shape (d2 : _)) _) = do
      case d2 of
        MonoAnon i -> do
          (ts, sizes) <- get
          put (ts, M.insert i d1 sizes)
        _ -> pure ()
      sub (stripArray 1 t1) (stripArray 1 t2)
    sub (Scalar (TypeVar _ v _)) t =
      unless (isIntrinsic (qualLeaf v)) $
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
      subRet (toStruct t1b) (second (const NoUniqueness) t2b)
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
          put (ts, M.insert i (sizeFromName (qualName d) mempty) sizes)
          pure $ sizeFromName (qualName d) mempty
        Just d ->
          pure d
    onDim (MonoAnon i) = do
      (_, sizes) <- get
      case M.lookup i sizes of
        Nothing -> pure $ anySize i
        Just d -> pure d

-- Perform a given substitution on the types in a pattern.
substPat :: (t -> t) -> Pat t -> Pat t
substPat f pat = case pat of
  TuplePat pats loc -> TuplePat (map (substPat f) pats) loc
  RecordPat fs loc -> RecordPat (map substField fs) loc
    where
      substField (n, p) = (n, substPat f p)
  PatParens p loc -> PatParens (substPat f p) loc
  PatAttr attr p loc -> PatAttr attr (substPat f p) loc
  Id vn (Info tp) loc -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc -> Wildcard (Info $ f tp) loc
  PatAscription p _ _ -> substPat f p
  PatLit e (Info tp) loc -> PatLit e (Info $ f tp) loc
  PatConstr n (Info tp) ps loc -> PatConstr n (Info $ f tp) ps loc

toPolyBinding :: ValBind -> PolyBinding
toPolyBinding (ValBind entry name _ _ (Info rettype) tparams params body _ attrs loc) =
  PolyBinding (unInfo <$> entry, name, tparams, params, rettype, body, attrs, loc)

transformValBind :: ValBind -> MonoM Env
transformValBind valbind = do
  let valbind' = toPolyBinding valbind

  when (isJust $ valBindEntryPoint valbind) $ do
    let t =
          funType (valBindParams valbind) $
            unInfo $
              valBindRetType valbind
    void $ monomorphiseBinding valbind' $ monoType t

  let global =
        if null (valBindParams valbind)
          then S.fromList $ retDims $ unInfo $ valBindRetType valbind
          else mempty

  env <- ask

  pure
    env
      { envPolyBindings =
          M.insert (valBindName valbind) (removeEntryPoint valbind') $
            envPolyBindings env,
        envGlobalScope = global <> envGlobalScope env,
        envScope = S.insert (valBindName valbind) global <> envScope env
      }

transformValBinds :: [ValBind] -> MonoM ()
transformValBinds [] = pure ()
transformValBinds (valbind : ds) = do
  env <- transformValBind valbind
  local (const env) $ transformValBinds ds

-- | Statistics about which functions were monomorphised.
newtype MonoStats = MonoStats [(VName, [MonoType])]

instance Pretty MonoStats where
  pretty (MonoStats l) =
    stack $ intersperse "" $ map pf l
    where
      comment = ("-- " <>)
      pf (name, ts) =
        comment (pretty (baseName name) <> "_" <> pretty (baseTag name))
          <+> parens (pretty (length ts) <+> "instantiations")
          <> ":"
            </> stack (map (comment . indent 2 . pretty) ts)

-- | Monomorphise a list of top-level value bindings.
transformProg :: (MonadFreshNames m) => [ValBind] -> m ([ValBind], MonoStats)
transformProg decs = do
  (a, b) <-
    modifyNameSource $ \namesrc ->
      runMonoM namesrc $ transformValBinds decs
  pure
    ( toList a,
      MonoStats . sortBy (comparing (Down . length . snd)) $
        map (\l -> (fst (NE.head l), NE.toList $ fmap snd l)) $
          NE.groupBy ((==) `on` fst) $
            sortBy (comparing fst) $
              map fst $
                M.toList b
    )

-- Note [Higher-Order Parameter Sizes]
--
-- Normally, when 'transformType' (via 'onDim') encounters a size that is some
-- non-trivial expression rather than a single variable or a constant (say, 'm *
-- n'), it hoists that expression into a fresh named size parameter, tracked via
-- 'ExpReplacements' ('replaceExp'). Any function that ends up with such a
-- parameter in its (monomorphised) signature has it turned into an explicit
-- leading argument, and every reference to that function must then be augmented
-- with a concrete argument for it. This augmentation happens in
-- 'inferSizeArgs', which tries to recover a concrete expression for the
-- parameter by structurally comparing the callee's own declared type against
-- the type at the point of reference ('dimMapping').
--
-- This works fine as long as the expression's free variables are all reliably
-- available, spelled out the same way, at every place the function is
-- referenced. But consider (#2230):
--
--   def f (n: i64) (m: i64) (g: f64 -> [m ** n]f64) = g 0
--   entry main n m = f n m (\x -> replicate (m ** n) x)
--
-- Here 'm ** n' only occurs as the return size of a *parameter* of 'f' (the
-- higher-order function 'g'), not as a size of 'f' itself. Because
-- 'transformFName' processes the bare reference to 'f' before its arguments
-- have been applied, 'dimMapping' has no way to know, at that point, that the
-- surrounding application happens to supply concrete values for 'n' and 'm'.
-- (In general it may not even be a full application - 'f' could be passed
-- onward as a value.) 'inferSizeArgs' then has no honest answer and used to
-- fall back to inserting a literal '0' as the argument, which will then be
-- treated as ground truth by the internaliser.
--
-- The same thing happens if the problematic size instead occurs in the *
-- argument* type of a returned or parameter-bound function, e.g.
--
--   def f (n: i64) (m: i64) =
--     \(g: [m ** n]f64 -> f64) -> g (replicate (m ** n) 0.0)
--   entry main n m = f n m (\x -> head x)
--
-- (see @test.fut@ in the repository root at the time of writing). 'd = m ** n'
-- is here unwitnessed by anything else in 'f's own signature, so it again
-- becomes an explicit parameter of 'f' that 'inferSizeArgs' cannot recover at
-- the unsaturated reference to 'f'.
--
-- The fix implemented here recognises that a size occurring anywhere in the
-- type of a higher-order (function-valued) parameter or return value - whether
-- describing what it consumes or what it produces - is different in kind from
-- an ordinary size: the function that declares it (here 'f') always has the
-- free variables it depends on in scope, since such a size can only be built
-- from parameters of the enclosing function itself. So nothing ever needs its
-- value threaded in externally as an explicit argument; the enclosing function
-- can always reconstruct it itself, wherever it actually needs it (to type the
-- result of calling 'g', to build an array to pass to 'g', or just to pass it
-- on to a further nested 'Arrow'). We erase the expression into a fresh
-- 'anySize' right where it occurs, instead of pretending we can reconstruct it
-- as an explicit argument (see Note [AnySize]). Since the size is never turned
-- into an explicit parameter of 'f' in the first place, there is also nothing
-- for 'inferSizeArgs' to fail to supply.
--
-- Deciding when we are in "the type of a higher-order parameter or return
-- value" is the job of the 'DimPos' that 'traverseDims' reports for each
-- size it visits: 'PosImmediate' means the size is not (yet) inside any
-- 'Arrow', while 'PosParam'/'PosReturn' mean it is on the argument or return
-- side of one (we treat the two identically). 'transformType' is thus
-- simply 'traverseDims' with a callback that hoists at 'PosImmediate' and
-- erases to an 'anySize' otherwise; unlike the hand-rolled traversal this
-- replaced, it gets the structural recursion through 'Record'/'Sum'/'Arrow'
-- for free. The one place that cannot use 'traverseDims' directly is
-- 'transformFNameType', which must stay precise through every nested
-- 'Arrow' regardless of position (see its docstring for why).
--
-- Repeated occurrences of the same original expression within one binding group
-- (e.g. the same higher-order parameter's size mentioned twice, or two
-- different higher-order parameters sharing a size) are mapped to the same
-- 'anySize' equivalence class via the 'sAnySizes' cache in 'MonoState', rather
-- than each minting a fresh one, mirroring 'sizesForPat' below. Two erasures
-- that end up in *different* equivalence classes (e.g. one introduced while
-- processing 'f's own parameter list, another introduced independently at some
-- call site's reconstructed argument type) are not a problem: 'anySize'
-- equivalence classes are only ever compared within a single binding group,
-- never across independent functions or call sites, so there is nothing to keep
-- synchronised between the two.
