-- | Defunctionalization of typed, monomorphic Futhark programs without modules.
module Futhark.Internalise.Defunctionalise (transformProg) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.List (partition, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.IR.Pretty ()
import Futhark.MonadFreshNames
import Futhark.Util (mapAccumLM, nubOrd)
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types (Subst (..), applySubst)

-- | A static value stores additional information about the result of
-- defunctionalization of an expression, aside from the residual expression.
data StaticVal
  = Dynamic ParamType
  | -- | The Env is the lexical closure of the lambda.
    LambdaSV (Pat ParamType) ResRetType Exp Env
  | RecordSV [(Name, StaticVal)]
  | -- | The constructor that is actually present, plus
    -- the others that are not.
    SumSV Name [StaticVal] [(Name, [ParamType])]
  | -- | The pair is the StaticVal and residual expression of this
    -- function as a whole, while the second StaticVal is its
    -- body. (Don't trust this too much, my understanding may have
    -- holes.)
    DynamicFun (Exp, StaticVal) StaticVal
  | IntrinsicSV
  | HoleSV StructType SrcLoc
  deriving (Show)

data Binding = Binding
  { -- | Just if this is a polymorphic binding that must be
    -- instantiated.
    bindingType :: Maybe ([VName], StructType),
    bindingSV :: StaticVal
  }
  deriving (Show)

-- | Environment mapping variable names to their associated static
-- value.
type Env = M.Map VName Binding

localEnv :: Env -> DefM a -> DefM a
localEnv env = local $ second (env <>)

-- Even when using a "new" environment (for evaluating closures) we
-- still ram the global environment of DynamicFuns in there.
localNewEnv :: Env -> DefM a -> DefM a
localNewEnv env = local $ \(globals, old_env) ->
  (globals, M.filterWithKey (\k _ -> k `S.member` globals) old_env <> env)

askEnv :: DefM Env
askEnv = asks snd

areGlobal :: [VName] -> DefM a -> DefM a
areGlobal vs = local $ first (S.fromList vs <>)

replaceTypeSizes ::
  M.Map VName SizeSubst ->
  TypeBase Size als ->
  TypeBase Size als
replaceTypeSizes substs = first onDim
  where
    onDim (Var v typ loc) =
      case M.lookup (qualLeaf v) substs of
        Just (SubstNamed v') -> Var v' typ loc
        Just (SubstConst d) -> sizeFromInteger (toInteger d) loc
        Nothing -> Var v typ loc
    onDim d = d

replaceStaticValSizes ::
  S.Set VName ->
  M.Map VName SizeSubst ->
  StaticVal ->
  StaticVal
replaceStaticValSizes globals orig_substs sv =
  case sv of
    _ | M.null orig_substs -> sv
    LambdaSV param (RetType t_dims t) e closure_env ->
      let substs =
            foldl' (flip M.delete) orig_substs $
              S.fromList (M.keys closure_env)
       in LambdaSV
            (fmap (replaceTypeSizes substs) param)
            (RetType t_dims (replaceTypeSizes substs t))
            (onExp substs e)
            (onEnv orig_substs closure_env) -- intentional
    Dynamic t ->
      Dynamic $ replaceTypeSizes orig_substs t
    RecordSV fs ->
      RecordSV $ map (fmap (replaceStaticValSizes globals orig_substs)) fs
    SumSV c svs ts ->
      SumSV c (map (replaceStaticValSizes globals orig_substs) svs) $
        map (fmap $ map $ replaceTypeSizes orig_substs) ts
    DynamicFun (e, sv1) sv2 ->
      DynamicFun (onExp orig_substs e, replaceStaticValSizes globals orig_substs sv1) $
        replaceStaticValSizes globals orig_substs sv2
    IntrinsicSV ->
      IntrinsicSV
    HoleSV t loc ->
      HoleSV t loc
  where
    tv substs =
      ASTMapper
        { mapOnStructType = pure . replaceTypeSizes substs,
          mapOnParamType = pure . replaceTypeSizes substs,
          mapOnResRetType = pure,
          mapOnExp = pure . onExp substs,
          mapOnName = pure . fmap (onName substs)
        }

    onName substs v =
      case M.lookup v substs of
        Just (SubstNamed v') -> qualLeaf v'
        _ -> v

    onExp substs (Var v t loc) =
      case M.lookup (qualLeaf v) substs of
        Just (SubstNamed v') ->
          Var v' t loc
        Just (SubstConst d) ->
          Literal (SignedValue (Int64Value (fromIntegral d))) loc
        Nothing ->
          Var v (replaceTypeSizes substs <$> t) loc
    onExp substs (Coerce e te t loc) =
      Coerce (onExp substs e) te (replaceTypeSizes substs <$> t) loc
    onExp substs (Lambda params e ret (Info (RetType t_dims t)) loc) =
      Lambda
        (map (fmap $ replaceTypeSizes substs) params)
        (onExp substs e)
        ret
        (Info (RetType t_dims (replaceTypeSizes substs t)))
        loc
    onExp substs e = runIdentity $ astMap (tv substs) e

    onEnv substs =
      M.fromList
        . map (second (onBinding substs))
        . M.toList

    onBinding substs (Binding t bsv) =
      Binding
        (second (replaceTypeSizes substs) <$> t)
        (replaceStaticValSizes globals substs bsv)

-- | Returns the defunctionalization environment restricted
-- to the given set of variable names.
restrictEnvTo :: FV -> DefM Env
restrictEnvTo fv = asks restrict
  where
    restrict (globals, env) = M.mapMaybeWithKey keep env
      where
        keep k (Binding t sv) = do
          guard $ not (k `S.member` globals) && S.member k (fvVars fv)
          Just $ Binding t $ restrict' sv
    restrict' (Dynamic t) =
      Dynamic t
    restrict' (LambdaSV pat t e env) =
      LambdaSV pat t e $ M.map restrict'' env
    restrict' (RecordSV fields) =
      RecordSV $ map (fmap restrict') fields
    restrict' (SumSV c svs fields) =
      SumSV c (map restrict' svs) fields
    restrict' (DynamicFun (e, sv1) sv2) =
      DynamicFun (e, restrict' sv1) $ restrict' sv2
    restrict' IntrinsicSV = IntrinsicSV
    restrict' (HoleSV t loc) = HoleSV t loc
    restrict'' (Binding t sv) = Binding t $ restrict' sv

-- | Defunctionalization monad.  The Reader environment tracks both
-- the current Env as well as the set of globally defined dynamic
-- functions.  This is used to avoid unnecessarily large closure
-- environments.
newtype DefM a
  = DefM (ReaderT (S.Set VName, Env) (State ([ValBind], VNameSource)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (S.Set VName, Env),
      MonadState ([ValBind], VNameSource)
    )

instance MonadFreshNames DefM where
  putNameSource src = modify $ \(x, _) -> (x, src)
  getNameSource = gets snd

-- | Run a computation in the defunctionalization monad. Returns the result of
-- the computation, a new name source, and a list of lifted function declations.
runDefM :: VNameSource -> DefM a -> (a, VNameSource, [ValBind])
runDefM src (DefM m) =
  let (x, (vbs, src')) = runState (runReaderT m mempty) (mempty, src)
   in (x, src', reverse vbs)

addValBind :: ValBind -> DefM ()
addValBind vb = modify $ first (vb :)

-- | Create a new top-level value declaration with the given function name,
-- return type, list of parameters, and body expression.
liftValDec :: VName -> ResRetType -> [VName] -> [Pat ParamType] -> Exp -> DefM ()
liftValDec fname (RetType ret_dims ret) dims pats body = addValBind dec
  where
    dims' = map (`TypeParamDim` mempty) dims
    -- FIXME: this pass is still not correctly size-preserving, so
    -- forget those return sizes that we forgot to propagate along
    -- the way.  Hopefully the internaliser is conservative and
    -- will insert reshapes...
    bound_here = S.fromList $ dims <> foldMap patNames pats
    mkExt v
      | not $ v `S.member` bound_here = Just v
    mkExt _ = Nothing
    rettype_st = RetType (mapMaybe mkExt (S.toList $ fvVars $ freeInType ret) ++ ret_dims) ret

    dec =
      ValBind
        { valBindEntryPoint = Nothing,
          valBindName = fname,
          valBindRetDecl = Nothing,
          valBindRetType = Info rettype_st,
          valBindTypeParams = dims',
          valBindParams = pats,
          valBindBody = body,
          valBindDoc = Nothing,
          valBindAttrs = mempty,
          valBindLocation = mempty
        }

-- | Looks up the associated static value for a given name in the environment.
lookupVar :: StructType -> VName -> DefM StaticVal
lookupVar t x = do
  env <- askEnv
  case M.lookup x env of
    Just (Binding (Just (dims, sv_t)) sv) -> do
      globals <- asks fst
      instStaticVal globals dims t sv_t sv
    Just (Binding Nothing sv) ->
      pure sv
    Nothing -- If the variable is unknown, it may refer to the 'intrinsics'
    -- module, which we will have to treat specially.
      | baseTag x <= maxIntrinsicTag -> pure IntrinsicSV
      | otherwise ->
          -- Anything not in scope is going to be an existential size.
          pure $ Dynamic $ Scalar $ Prim $ Signed Int64

-- Like freeInPat, but ignores sizes that are only found in
-- funtion types.
arraySizes :: StructType -> S.Set VName
arraySizes (Scalar Arrow {}) = mempty
arraySizes (Scalar (Record fields)) = foldMap arraySizes fields
arraySizes (Scalar (Sum cs)) = foldMap (foldMap arraySizes) cs
arraySizes (Scalar (TypeVar _ _ targs)) =
  mconcat $ map f targs
  where
    f (TypeArgDim (Var d _ _)) = S.singleton $ qualLeaf d
    f TypeArgDim {} = mempty
    f (TypeArgType t) = arraySizes t
arraySizes (Scalar Prim {}) = mempty
arraySizes (Array _ shape t) =
  arraySizes (Scalar t) <> foldMap dimName (shapeDims shape)
  where
    dimName :: Size -> S.Set VName
    dimName (Var qn _ _) = S.singleton $ qualLeaf qn
    dimName _ = mempty

patternArraySizes :: Pat ParamType -> S.Set VName
patternArraySizes = arraySizes . patternStructType

data SizeSubst
  = SubstNamed (QualName VName)
  | SubstConst Int64
  deriving (Eq, Ord, Show)

dimMapping ::
  (Monoid a) =>
  TypeBase Size a ->
  TypeBase Size a ->
  M.Map VName SizeSubst
dimMapping t1 t2 = execState (matchDims f t1 t2) mempty
  where
    f bound d1 (Var d2 _ _)
      | qualLeaf d2 `elem` bound = pure d1
    f _ (Var d1 typ loc) (Var d2 _ _) = do
      modify $ M.insert (qualLeaf d1) $ SubstNamed d2
      pure $ Var d1 typ loc
    f _ (Var d1 typ loc) (IntLit d2 _ _) = do
      modify $ M.insert (qualLeaf d1) $ SubstConst $ fromInteger d2
      pure $ Var d1 typ loc
    f _ d _ = pure d

dimMapping' ::
  (Monoid a) =>
  TypeBase Size a ->
  TypeBase Size a ->
  M.Map VName VName
dimMapping' t1 t2 = M.mapMaybe f $ dimMapping t1 t2
  where
    f (SubstNamed d) = Just $ qualLeaf d
    f _ = Nothing

sizesToRename :: StaticVal -> S.Set VName
sizesToRename (DynamicFun (_, sv1) _sv2) =
  -- It is intentional that we do not look at sv2 here, as some names
  -- that are free in sv2 are actually bound by the parameters in sv1.
  -- See #2234.
  sizesToRename sv1
sizesToRename IntrinsicSV =
  mempty
sizesToRename HoleSV {} =
  mempty
sizesToRename Dynamic {} =
  mempty
sizesToRename (RecordSV fs) =
  foldMap (sizesToRename . snd) fs
sizesToRename (SumSV _ svs _) =
  foldMap sizesToRename svs
sizesToRename (LambdaSV param _ _ _) =
  -- We used to rename parameters here, but I don't understand why
  -- that was necessary and it caused some problems.
  fvVars (freeInPat param)

-- | Combine the shape information of types as much as possible. The first
-- argument is the orignal type and the second is the type of the transformed
-- expression. This is necessary since the original type may contain additional
-- information (e.g., shape restrictions) from the user given annotation.
combineTypeShapes ::
  (Monoid as) =>
  TypeBase Size as ->
  TypeBase Size as ->
  TypeBase Size as
combineTypeShapes (Scalar (Record ts1)) (Scalar (Record ts2))
  | M.keys ts1 == M.keys ts2 =
      Scalar $
        Record $
          M.map
            (uncurry combineTypeShapes)
            (M.intersectionWith (,) ts1 ts2)
combineTypeShapes (Scalar (Sum cs1)) (Scalar (Sum cs2))
  | M.keys cs1 == M.keys cs2 =
      Scalar $
        Sum $
          M.map
            (uncurry $ zipWith combineTypeShapes)
            (M.intersectionWith (,) cs1 cs2)
combineTypeShapes (Scalar (Arrow als1 p1 d1 a1 (RetType dims1 b1))) (Scalar (Arrow als2 _p2 _d2 a2 (RetType _ b2))) =
  Scalar $
    Arrow
      (als1 <> als2)
      p1
      d1
      (combineTypeShapes a1 a2)
      (RetType dims1 (combineTypeShapes b1 b2))
combineTypeShapes (Scalar (TypeVar u v targs1)) (Scalar (TypeVar _ _ targs2)) =
  Scalar $ TypeVar u v $ zipWith f targs1 targs2
  where
    f (TypeArgType t1) (TypeArgType t2) = TypeArgType (combineTypeShapes t1 t2)
    f targ _ = targ
combineTypeShapes (Array u shape1 et1) (Array _ _shape2 et2) =
  arrayOfWithAliases
    u
    shape1
    (combineTypeShapes (setUniqueness (Scalar et1) u) (setUniqueness (Scalar et2) u))
combineTypeShapes _ t = t

-- When we instantiate a polymorphic StaticVal, we rename all the
-- sizes to avoid name conflicts later on.  This is a bit of a hack...
instStaticVal ::
  (MonadFreshNames m) =>
  S.Set VName ->
  [VName] ->
  StructType ->
  StructType ->
  StaticVal ->
  m StaticVal
instStaticVal globals dims t sv_t sv = do
  fresh_substs <-
    mkSubsts . filter (`S.notMember` globals) . S.toList $
      S.fromList dims <> sizesToRename sv
  let dims' = map (onName fresh_substs) dims
      isDim k _ = k `elem` dims'
      dim_substs =
        M.filterWithKey isDim $ dimMapping (replaceTypeSizes fresh_substs sv_t) t
      replace (SubstNamed k) = fromMaybe (SubstNamed k) $ M.lookup (qualLeaf k) dim_substs
      replace k = k
      substs = M.map replace fresh_substs <> dim_substs

  pure $ replaceStaticValSizes globals substs sv
  where
    mkSubsts names =
      M.fromList . zip names . map (SubstNamed . qualName)
        <$> mapM newName names

    onName substs v =
      case M.lookup v substs of
        Just (SubstNamed v') -> qualLeaf v'
        _ -> v

defuncFun ::
  [VName] ->
  [Pat ParamType] ->
  Exp ->
  ResRetType ->
  SrcLoc ->
  DefM (Exp, StaticVal)
defuncFun tparams pats e0 ret loc = do
  -- Extract the first parameter of the lambda and "push" the
  -- remaining ones (if there are any) into the body of the lambda.
  let (pat, ret', e0') = case pats of
        [] -> error "Received a lambda with no parameters."
        [pat'] -> (pat', ret, e0)
        (pat' : pats') ->
          ( pat',
            RetType [] $ second (const Nonunique) $ funType pats' ret,
            Lambda pats' e0 Nothing (Info ret) loc
          )

  -- Construct a record literal that closes over the environment of
  -- the lambda.  Closed-over 'DynamicFun's are converted to their
  -- closure representation.
  let used =
        freeInExp (Lambda pats e0 Nothing (Info ret) loc)
          `freeWithout` S.fromList tparams
  used_env <- restrictEnvTo used

  -- The closure parts that are sizes are proactively turned into size
  -- parameters.
  let sizes_of_arrays =
        foldMap (arraySizes . structTypeFromSV . bindingSV) used_env
          <> patternArraySizes pat
      notSize = not . (`S.member` sizes_of_arrays)
      (fields, env) =
        second M.fromList
          . unzip
          . map closureFromDynamicFun
          . filter (notSize . fst)
          $ M.toList used_env

  pure
    ( RecordLit fields loc,
      LambdaSV pat ret' e0' env
    )
  where
    closureFromDynamicFun (vn, Binding _ (DynamicFun (clsr_env, sv) _)) =
      let name = nameFromString $ prettyString vn
       in ( RecordFieldExplicit (L noLoc name) clsr_env mempty,
            (vn, Binding Nothing sv)
          )
    closureFromDynamicFun (vn, Binding _ sv) =
      let name = nameFromString $ prettyString vn
          tp' = structTypeFromSV sv
       in ( RecordFieldExplicit
              (L noLoc name)
              (Var (qualName vn) (Info tp') mempty)
              mempty,
            (vn, Binding Nothing sv)
          )

-- | Defunctionalization of an expression. Returns the residual expression and
-- the associated static value in the defunctionalization monad.
defuncExp :: Exp -> DefM (Exp, StaticVal)
defuncExp e@Literal {} =
  pure (e, Dynamic $ toParam Observe $ typeOf e)
defuncExp e@IntLit {} =
  pure (e, Dynamic $ toParam Observe $ typeOf e)
defuncExp e@FloatLit {} =
  pure (e, Dynamic $ toParam Observe $ typeOf e)
defuncExp e@StringLit {} =
  pure (e, Dynamic $ toParam Observe $ typeOf e)
defuncExp (Parens e loc) = do
  (e', sv) <- defuncExp e
  pure (Parens e' loc, sv)
defuncExp (QualParens qn e loc) = do
  (e', sv) <- defuncExp e
  pure (QualParens qn e' loc, sv)
defuncExp (TupLit es loc) = do
  (es', svs) <- mapAndUnzipM defuncExp es
  pure (TupLit es' loc, RecordSV $ zip tupleFieldNames svs)
defuncExp (RecordLit fs loc) = do
  (fs', names_svs) <- mapAndUnzipM defuncField fs
  pure (RecordLit fs' loc, RecordSV names_svs)
  where
    defuncField (RecordFieldExplicit vn e loc') = do
      (e', sv) <- defuncExp e
      pure (RecordFieldExplicit vn e' loc', (unLoc vn, sv))
    defuncField (RecordFieldImplicit (L _ vn) (Info t) loc') = do
      sv <- lookupVar (toStruct t) vn
      case sv of
        -- If the implicit field refers to a dynamic function, we
        -- convert it to an explicit field with a record closing over
        -- the environment and bind the corresponding static value.
        DynamicFun (e, sv') _ ->
          let vn' = baseName vn
           in pure
                ( RecordFieldExplicit (L noLoc vn') e loc',
                  (vn', sv')
                )
        -- The field may refer to a functional expression, so we get the
        -- type from the static value and not the one from the AST.
        _ ->
          let tp = Info $ structTypeFromSV sv
           in pure
                ( RecordFieldImplicit (L noLoc vn) tp loc',
                  (baseName vn, sv)
                )
defuncExp e@(ArrayVal vs t loc) =
  pure (ArrayVal vs t loc, Dynamic $ toParam Observe $ typeOf e)
defuncExp (ArrayLit es t@(Info t') loc) = do
  es' <- mapM defuncExp' es
  pure (ArrayLit es' t loc, Dynamic $ toParam Observe t')
defuncExp (AppExp (Range e1 me incl loc) res) = do
  e1' <- defuncExp' e1
  me' <- mapM defuncExp' me
  incl' <- mapM defuncExp' incl
  pure
    ( AppExp (Range e1' me' incl' loc) res,
      Dynamic $ toParam Observe $ appResType $ unInfo res
    )
defuncExp e@(Var qn (Info t) loc) = do
  sv <- lookupVar (toStruct t) (qualLeaf qn)
  case sv of
    -- If the variable refers to a dynamic function, we eta-expand it
    -- so that we do not have to duplicate its definition.
    DynamicFun {} -> do
      (params, body, ret) <- etaExpand (RetType [] $ toRes Nonunique t) e
      defuncFun [] params body ret mempty
    -- Intrinsic functions used as variables are eta-expanded, so we
    -- can get rid of them.
    IntrinsicSV -> do
      (pats, body, tp) <- etaExpand (RetType [] $ toRes Nonunique t) e
      defuncExp $ Lambda pats body Nothing (Info tp) mempty
    HoleSV _ hole_loc ->
      pure (Hole (Info t) hole_loc, sv)
    _ ->
      pure (Var qn (Info (structTypeFromSV sv)) loc, sv)
defuncExp (Hole (Info t) loc) =
  pure (Hole (Info t) loc, HoleSV t loc)
defuncExp (Ascript e0 tydecl loc)
  | orderZero (typeOf e0) = do
      (e0', sv) <- defuncExp e0
      pure (Ascript e0' tydecl loc, sv)
  | otherwise = defuncExp e0
defuncExp (Coerce e0 tydecl t loc)
  | orderZero (typeOf e0) = do
      (e0', sv) <- defuncExp e0
      pure (Coerce e0' tydecl t loc, sv)
  | otherwise = defuncExp e0
defuncExp (AppExp (LetPat sizes pat e1 e2 loc) (Info (AppRes t retext))) = do
  (e1', sv1) <- defuncExp e1
  let env = alwaysMatchPatSV (fmap (toParam Observe) pat) sv1
      pat' = updatePat (fmap (toParam Observe) pat) sv1
  (e2', sv2) <- localEnv env $ defuncExp e2
  -- To maintain any sizes going out of scope, we need to compute the
  -- old size substitution induced by retext and also apply it to the
  -- newly computed body type.
  let mapping = dimMapping' (typeOf e2) t
      subst v = ExpSubst . flip sizeFromName mempty . qualName <$> M.lookup v mapping
      t' = applySubst subst $ typeOf e2'
  pure (AppExp (LetPat sizes (fmap toStruct pat') e1' e2' loc) (Info (AppRes t' retext)), sv2)
defuncExp (AppExp (LetFun vn _ _ _) _) =
  error $ "defuncExp: Unexpected LetFun: " ++ show vn
defuncExp (AppExp (If e1 e2 e3 loc) res) = do
  (e1', _) <- defuncExp e1
  (e2', sv) <- defuncExp e2
  (e3', _) <- defuncExp e3
  pure (AppExp (If e1' e2' e3' loc) res, sv)
defuncExp (AppExp (Apply f args loc) (Info appres)) =
  defuncApply f (fmap (first unInfo) args) appres loc
defuncExp (Negate e0 loc) = do
  (e0', sv) <- defuncExp e0
  pure (Negate e0' loc, sv)
defuncExp (Not e0 loc) = do
  (e0', sv) <- defuncExp e0
  pure (Not e0' loc, sv)
defuncExp (Lambda pats e0 _ (Info ret) loc) =
  defuncFun [] pats e0 ret loc
-- Operator sections are expected to be converted to lambda-expressions
-- by the monomorphizer, so they should no longer occur at this point.
defuncExp OpSection {} = error "defuncExp: unexpected operator section."
defuncExp OpSectionLeft {} = error "defuncExp: unexpected operator section."
defuncExp OpSectionRight {} = error "defuncExp: unexpected operator section."
defuncExp ProjectSection {} = error "defuncExp: unexpected projection section."
defuncExp IndexSection {} = error "defuncExp: unexpected projection section."
defuncExp (AppExp (Loop sparams pat loopinit form e3 loc) res) = do
  (e1', sv1) <- defuncExp $ loopInitExp loopinit
  let env1 = alwaysMatchPatSV pat sv1
  (form', env2) <- case form of
    For v e2 -> do
      e2' <- defuncExp' e2
      pure (For v e2', envFromIdent v)
    ForIn pat2 e2 -> do
      e2' <- defuncExp' e2
      pure (ForIn pat2 e2', envFromPat $ fmap (toParam Observe) pat2)
    While e2 -> do
      e2' <- localEnv env1 $ defuncExp' e2
      pure (While e2', mempty)
  (e3', sv) <- localEnv (env1 <> env2) $ defuncExp e3
  pure (AppExp (Loop sparams pat (LoopInitExplicit e1') form' e3' loc) res, sv)
  where
    envFromIdent (Ident vn (Info tp) _) =
      M.singleton vn $ Binding Nothing $ Dynamic $ toParam Observe tp
defuncExp e@(AppExp BinOp {} _) =
  error $ "defuncExp: unexpected binary operator: " ++ prettyString e
defuncExp (Project vn e0 tp@(Info tp') loc) = do
  (e0', sv0) <- defuncExp e0
  case sv0 of
    RecordSV svs -> case lookup vn svs of
      Just sv -> pure (Project vn e0' (Info $ structTypeFromSV sv) loc, sv)
      Nothing -> error "Invalid record projection."
    Dynamic _ -> pure (Project vn e0' tp loc, Dynamic $ toParam Observe tp')
    HoleSV _ hloc -> pure (Project vn e0' tp loc, HoleSV tp' hloc)
    _ -> error $ "Projection of an expression with static value " ++ show sv0
defuncExp (AppExp LetWith {} _) =
  error "defuncExp: unexpected LetWith"
defuncExp expr@(AppExp (Index e0 idxs loc) res) = do
  e0' <- defuncExp' e0
  idxs' <- mapM defuncDimIndex idxs
  pure
    ( AppExp (Index e0' idxs' loc) res,
      Dynamic $ toParam Observe $ typeOf expr
    )
defuncExp (Update e1 idxs e2 loc) = do
  (e1', sv) <- defuncExp e1
  idxs' <- mapM defuncDimIndex idxs
  e2' <- defuncExp' e2
  pure (Update e1' idxs' e2' loc, sv)

-- Note that we might change the type of the record field here.  This
-- is not permitted in the type checker due to problems with type
-- inference, but it actually works fine.
defuncExp (RecordUpdate e1 fs e2 _ loc) = do
  (e1', sv1) <- defuncExp e1
  (e2', sv2) <- defuncExp e2
  let sv = staticField sv1 sv2 fs
  pure
    ( RecordUpdate e1' fs e2' (Info $ structTypeFromSV sv1) loc,
      sv
    )
  where
    staticField (RecordSV svs) sv2 (f : fs') =
      case lookup f svs of
        Just sv ->
          RecordSV $
            (f, staticField sv sv2 fs') : filter ((/= f) . fst) svs
        Nothing -> error "Invalid record projection."
    staticField (Dynamic t@(Scalar Record {})) sv2 fs'@(_ : _) =
      staticField (svFromType t) sv2 fs'
    staticField _ sv2 _ = sv2
defuncExp (Assert e1 e2 desc loc) = do
  (e1', _) <- defuncExp e1
  (e2', sv) <- defuncExp e2
  pure (Assert e1' e2' desc loc, sv)
defuncExp (Constr name es (Info sum_t@(Scalar (Sum all_fs))) loc) = do
  (es', svs) <- mapAndUnzipM defuncExp es
  let sv =
        SumSV name svs $
          M.toList $
            name `M.delete` M.map (map (toParam Observe . defuncType)) all_fs
      sum_t' = combineTypeShapes sum_t (structTypeFromSV sv)
  pure (Constr name es' (Info sum_t') loc, sv)
  where
    defuncType ::
      (Monoid als) =>
      TypeBase Size als ->
      TypeBase Size als
    defuncType (Array u shape t) = Array u shape (defuncScalar t)
    defuncType (Scalar t) = Scalar $ defuncScalar t

    defuncScalar ::
      (Monoid als) =>
      ScalarTypeBase Size als ->
      ScalarTypeBase Size als
    defuncScalar (Record fs) = Record $ M.map defuncType fs
    defuncScalar Arrow {} = Record mempty
    defuncScalar (Sum fs) = Sum $ M.map (map defuncType) fs
    defuncScalar (Prim t) = Prim t
    defuncScalar (TypeVar u tn targs) = TypeVar u tn targs
defuncExp (Constr name _ (Info t) loc) =
  error $
    "Constructor "
      ++ prettyString name
      ++ " given type "
      ++ prettyString t
      ++ " at "
      ++ locStr loc
defuncExp (AppExp (Match e cs loc) res) = do
  (e', sv) <- defuncExp e
  let bad = error $ "No case matches StaticVal\n" <> show sv
  csPairs <-
    fromMaybe bad . NE.nonEmpty . catMaybes
      <$> mapM (defuncCase sv) (NE.toList cs)
  let cs' = fmap fst csPairs
      sv' = snd $ NE.head csPairs
  pure (AppExp (Match e' cs' loc) res, sv')
defuncExp (Attr info e loc) = do
  (e', sv) <- defuncExp e
  pure (Attr info e' loc, sv)

-- | Same as 'defuncExp', except it ignores the static value.
defuncExp' :: Exp -> DefM Exp
defuncExp' = fmap fst . defuncExp

defuncCase :: StaticVal -> Case -> DefM (Maybe (Case, StaticVal))
defuncCase sv (CasePat p e loc) = do
  let p' = updatePat (fmap (toParam Observe) p) sv
  case matchPatSV (fmap (toParam Observe) p) sv of
    Just env -> do
      (e', sv') <- localEnv env $ defuncExp e
      pure $ Just (CasePat (fmap toStruct p') e' loc, sv')
    Nothing ->
      pure Nothing

-- | Defunctionalize the function argument to a SOAC by eta-expanding if
-- necessary and then defunctionalizing the body of the introduced lambda.
defuncSoacExp :: Exp -> DefM Exp
defuncSoacExp e@OpSection {} = pure e
defuncSoacExp e@OpSectionLeft {} = pure e
defuncSoacExp e@OpSectionRight {} = pure e
defuncSoacExp e@ProjectSection {} = pure e
defuncSoacExp (Parens e loc) =
  Parens <$> defuncSoacExp e <*> pure loc
defuncSoacExp (Lambda params e0 decl tp loc) = do
  let env = foldMap envFromPat params
  e0' <- localEnv env $ defuncSoacExp e0
  pure $ Lambda params e0' decl tp loc
defuncSoacExp e
  | Scalar Arrow {} <- typeOf e = do
      (pats, body, tp) <- etaExpand (RetType [] $ toRes Nonunique $ typeOf e) e
      let env = foldMap envFromPat pats
      body' <- localEnv env $ defuncExp' body
      pure $ Lambda pats body' Nothing (Info tp) (srclocOf e)
  | otherwise = defuncExp' e

etaExpand :: ResRetType -> Exp -> DefM ([Pat ParamType], Exp, ResRetType)
etaExpand e_t e = do
  let (ps, ret) = getType e_t
  -- Some careful hackery to avoid duplicate names.
  (_, (params, vars)) <- second unzip <$> mapAccumLM f [] ps
  -- Important that we synthesize new existential names and substitute
  -- them into the (body) return type.
  ext' <- mapM newName $ retDims ret
  let extsubst =
        M.fromList . zip (retDims ret) $
          map (ExpSubst . flip sizeFromName mempty . qualName) ext'
      ret' = applySubst (`M.lookup` extsubst) ret
      e' = mkApply e (map (\v -> (Nothing, mempty, v)) vars) $ AppRes (toStruct $ retType ret') ext'
  pure (params, e', ret)
  where
    getType (RetType _ (Scalar (Arrow _ p d t1 t2))) =
      let (ps, r) = getType t2
       in ((p, (d, t1)) : ps, r)
    getType t = ([], t)

    f prev (p, (d, t)) = do
      let t' = second (const d) t
      x <- case p of
        Named x | x `notElem` prev -> pure x
        _ -> newNameFromString "eta_p"
      pure
        ( x : prev,
          ( Id x (Info t') mempty,
            Var (qualName x) (Info $ toStruct t') mempty
          )
        )

-- | Defunctionalize an indexing of a single array dimension.
defuncDimIndex :: DimIndexBase Info VName -> DefM (DimIndexBase Info VName)
defuncDimIndex (DimFix e1) = DimFix . fst <$> defuncExp e1
defuncDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> defunc' me1 <*> defunc' me2 <*> defunc' me3
  where
    defunc' = mapM defuncExp'

envFromDimNames :: [VName] -> Env
envFromDimNames = M.fromList . flip zip (repeat d)
  where
    d = Binding Nothing $ Dynamic $ Scalar $ Prim $ Signed Int64

-- | Defunctionalize a let-bound function, while preserving parameters
-- that have order 0 types (i.e., non-functional).
defuncLet ::
  [VName] ->
  [Pat ParamType] ->
  Exp ->
  ResRetType ->
  DefM ([VName], [Pat ParamType], Exp, StaticVal, ResType)
defuncLet dims ps@(pat : pats) body (RetType ret_dims rettype)
  | patternOrderZero pat = do
      let bound_by_pat = (`S.member` fvVars (freeInPat pat))
          -- Take care to not include more size parameters than necessary.
          (pat_dims, rest_dims) = partition bound_by_pat dims
          env = envFromPat pat <> envFromDimNames pat_dims
      (rest_dims', pats', body', sv, sv_t) <-
        localEnv env $ defuncLet rest_dims pats body $ RetType ret_dims rettype
      closure <- defuncFun dims ps body (RetType ret_dims rettype) mempty
      pure
        ( pat_dims ++ rest_dims',
          pat : pats',
          body',
          DynamicFun closure sv,
          sv_t
        )
  | otherwise = do
      (e, sv) <- defuncFun dims ps body (RetType ret_dims rettype) mempty
      pure ([], [], e, sv, resTypeFromSV sv)
defuncLet _ [] body (RetType _ rettype) = do
  (body', sv) <- defuncExp body
  pure
    ( [],
      [],
      body',
      imposeType sv $ resToParam rettype,
      resTypeFromSV sv
    )
  where
    imposeType Dynamic {} t =
      Dynamic t
    imposeType (RecordSV fs1) (Scalar (Record fs2)) =
      RecordSV $ M.toList $ M.intersectionWith imposeType (M.fromList fs1) fs2
    imposeType sv _ = sv

instAnySizes :: (MonadFreshNames m) => [Pat ParamType] -> m [Pat ParamType]
instAnySizes = traverse $ traverse $ bitraverse onDim pure
  where
    onDim d
      | d == anySize = do
          v <- newVName "size"
          pure $ sizeFromName (qualName v) mempty
    onDim d = pure d

unboundSizes :: S.Set VName -> [Pat ParamType] -> [VName]
unboundSizes bound_sizes params = nubOrd $ execState (f params) []
  where
    f = traverse $ traverse $ bitraverse onDim pure
    bound = bound_sizes <> S.fromList (foldMap patNames params)
    onDim (Var d typ loc) = do
      unless (qualLeaf d `S.member` bound) $ modify (qualLeaf d :)
      pure $ Var d typ loc
    onDim d = pure d

unRetType :: ResRetType -> DefM AppRes
unRetType (RetType [] t) = pure $ AppRes (toStruct t) []
unRetType (RetType ext t) = do
  ext' <- mapM newName ext
  let extsubst =
        M.fromList . zip ext $
          map (ExpSubst . flip sizeFromName mempty . qualName) ext'
  pure $ AppRes (applySubst (`M.lookup` extsubst) $ toStruct t) ext'

defuncApplyFunction :: Exp -> Int -> DefM (Exp, StaticVal)
defuncApplyFunction e@(Var qn (Info t) loc) num_args = do
  let (argtypes, rettype) = first (map snd) $ unfoldFunType t
  sv <- lookupVar (toStruct t) (qualLeaf qn)

  case sv of
    DynamicFun _ _
      | fullyApplied sv num_args -> do
          -- We still need to update the types in case the dynamic
          -- function returns a higher-order term.
          let (argtypes', rettype') = dynamicFunType sv argtypes
          pure (Var qn (Info (foldFunType argtypes' $ RetType [] rettype')) loc, sv)
      | all orderZero argtypes,
        orderZero rettype -> do
          (params, body, ret) <- etaExpand (RetType [] $ toRes Nonunique t) e
          defuncFun [] params body ret mempty
      | otherwise -> do
          fname <- newVName $ "dyn_" <> baseString (qualLeaf qn)
          let (pats, e0, sv') = liftDynFun (prettyString qn) sv num_args
              (argtypes', rettype') = dynamicFunType sv' argtypes
              dims' = mempty

          -- Ensure that no parameter sizes are AnySize.  The internaliser
          -- expects this.  This is easy, because they are all
          -- first-order.
          globals <- asks fst
          let bound_sizes = S.fromList dims' <> globals
          pats' <- instAnySizes pats
          let dims'' = dims' ++ unboundSizes bound_sizes pats'

          liftValDec fname (RetType [] rettype') dims'' pats' e0
          pure
            ( Var
                (qualName fname)
                (Info (foldFunType argtypes' $ RetType [] rettype'))
                loc,
              sv'
            )
    IntrinsicSV -> pure (e, IntrinsicSV)
    _ -> pure (Var qn (Info (structTypeFromSV sv)) loc, sv)
defuncApplyFunction e _ = defuncExp e

-- Embed some information about the original function
-- into the name of the lifted function, to make the
-- result slightly more human-readable.
liftedName :: Int -> Exp -> String
liftedName i (Var f _ _) =
  "defunc_" ++ show i ++ "_" ++ baseString (qualLeaf f)
liftedName i (AppExp (Apply f _ _) _) =
  liftedName (i + 1) f
liftedName _ _ = "defunc"

defuncApplyArg ::
  (String, SrcLoc) ->
  (Exp, StaticVal) ->
  (((Maybe VName, AutoMap), Exp), [ParamType]) ->
  DefM (Exp, StaticVal)
defuncApplyArg (fname_s, floc) (f', LambdaSV pat lam_e_t lam_e closure_env) (((argext, _), arg), _) = do
  (arg', arg_sv) <- defuncExp arg
  let env' = alwaysMatchPatSV pat arg_sv
      dims = mempty
  (lam_e', sv) <-
    localNewEnv (env' <> closure_env) $
      defuncExp lam_e

  let closure_pat = buildEnvPat dims closure_env
      pat' = updatePat pat arg_sv

  globals <- asks fst

  -- Lift lambda to top-level function definition.  We put in
  -- a lot of effort to try to infer the uniqueness attributes
  -- of the lifted function, but this is ultimately all a sham
  -- and a hack.  There is some piece we're missing.
  let params = [closure_pat, pat']
      lifted_rettype =
        RetType (retDims lam_e_t) $
          combineTypeShapes (retType lam_e_t) (resTypeFromSV sv)

      already_bound =
        globals <> S.fromList (dims <> foldMap patNames params)

      more_dims =
        S.toList $
          S.filter (`S.notMember` already_bound) $
            foldMap patternArraySizes params

  -- Ensure that no parameter sizes are AnySize.  The internaliser
  -- expects this.  This is easy, because they are all
  -- first-order.
  let bound_sizes = S.fromList (dims <> more_dims) <> globals
  params' <- instAnySizes params

  fname <- newNameFromString fname_s
  liftValDec
    fname
    lifted_rettype
    (dims ++ more_dims ++ unboundSizes bound_sizes params')
    params'
    lam_e'

  let f_t = toStruct $ typeOf f'
      arg_t = toStruct $ typeOf arg'
      fname_t = foldFunType [toParam Observe f_t, toParam (diet (patternType pat)) arg_t] lifted_rettype
      fname' = Var (qualName fname) (Info fname_t) floc
  callret <- unRetType lifted_rettype

  pure
    ( mkApply fname' [(Nothing, mempty, f'), (argext, mempty, arg')] callret,
      sv
    )
-- If 'f' is a dynamic function, we just leave the application in
-- place, but we update the types since it may be partially
-- applied or return a higher-order value.
defuncApplyArg _ (f', DynamicFun _ sv) (((argext, _), arg), argtypes) = do
  (arg', _) <- defuncExp arg
  let (argtypes', rettype) = dynamicFunType sv argtypes
      restype = foldFunType argtypes' (RetType [] rettype)
      callret = AppRes restype []
      apply_e = mkApply f' [(argext, mempty, arg')] callret
  pure (apply_e, sv)
--
defuncApplyArg (fname_s, _) (_, sv) ((_, arg), _) =
  error $
    "defuncApplyArg: cannot apply StaticVal\n"
      <> show sv
      <> "\nFunction name: "
      <> prettyString fname_s
      <> "\nArgument: "
      <> prettyString arg

updateReturn :: AppRes -> Exp -> Exp
updateReturn (AppRes ret1 ext1) (AppExp apply (Info (AppRes ret2 ext2))) =
  AppExp apply $ Info $ AppRes (combineTypeShapes ret1 ret2) (ext1 <> ext2)
updateReturn _ e = e

defuncApply :: Exp -> NE.NonEmpty ((Maybe VName, AutoMap), Exp) -> AppRes -> SrcLoc -> DefM (Exp, StaticVal)
defuncApply f args appres loc = do
  (f', f_sv) <- defuncApplyFunction f (length args)
  case f_sv of
    IntrinsicSV -> do
      args' <- fmap (first Info) <$> traverse (traverse defuncSoacExp) args
      let e' = AppExp (Apply f' args' loc) (Info appres)
      intrinsicOrHole e'
    HoleSV {} -> do
      args' <- fmap (first Info) <$> traverse (traverse $ fmap fst . defuncExp) args
      let e' = AppExp (Apply f' args' loc) (Info appres)
      intrinsicOrHole e'
    _ -> do
      let fname = liftedName 0 f
          (argtypes, _) = unfoldFunType $ typeOf f
      fmap (first $ updateReturn appres) $
        foldM (defuncApplyArg (fname, loc)) (f', f_sv) $
          NE.zip args . NE.tails . map snd $
            argtypes
  where
    intrinsicOrHole e' = do
      -- If the intrinsic is fully applied, then we are done.
      -- Otherwise we need to eta-expand it and recursively
      -- defunctionalise. XXX: might it be better to simply eta-expand
      -- immediately any time we encounter a non-fully-applied
      -- intrinsic?
      if null $ fst $ unfoldFunType $ appResType appres
        then pure (e', Dynamic $ toParam Observe $ appResType appres)
        else do
          (pats, body, tp) <- etaExpand (RetType [] $ toRes Nonunique $ typeOf e') e'
          defuncExp $ Lambda pats body Nothing (Info tp) mempty

-- | Check if a 'StaticVal' and a given application depth corresponds
-- to a fully applied dynamic function.
fullyApplied :: StaticVal -> Int -> Bool
fullyApplied (DynamicFun _ sv) depth
  | depth == 0 = False
  | depth > 0 = fullyApplied sv (depth - 1)
fullyApplied _ _ = True

-- | Converts a dynamic function 'StaticVal' into a list of
-- dimensions, a list of parameters, a function body, and the
-- appropriate static value for applying the function at the given
-- depth of partial application.
liftDynFun :: String -> StaticVal -> Int -> ([Pat ParamType], Exp, StaticVal)
liftDynFun _ (DynamicFun (e, sv) _) 0 = ([], e, sv)
liftDynFun s (DynamicFun clsr@(_, LambdaSV pat _ _ _) sv) d
  | d > 0 =
      let (pats, e', sv') = liftDynFun s sv (d - 1)
       in (pat : pats, e', DynamicFun clsr sv')
liftDynFun s sv d =
  error $
    s
      ++ " Tried to lift a StaticVal "
      ++ take 100 (show sv)
      ++ ", but expected a dynamic function.\n"
      ++ prettyString d

-- | Converts a pattern to an environment that binds the individual names of the
-- pattern to their corresponding types wrapped in a 'Dynamic' static value.
envFromPat :: Pat ParamType -> Env
envFromPat pat = case pat of
  TuplePat ps _ -> foldMap envFromPat ps
  RecordPat fs _ -> foldMap (envFromPat . snd) fs
  PatParens p _ -> envFromPat p
  PatAttr _ p _ -> envFromPat p
  Id vn (Info t) _ -> M.singleton vn $ Binding Nothing $ Dynamic t
  Wildcard _ _ -> mempty
  PatAscription p _ _ -> envFromPat p
  PatLit {} -> mempty
  PatConstr _ _ ps _ -> foldMap envFromPat ps

-- | Given a closure environment, construct a record pattern that
-- binds the closed over variables.  Insert wildcard for any patterns
-- that would otherwise clash with size parameters.
buildEnvPat :: [VName] -> Env -> Pat ParamType
buildEnvPat sizes env = RecordPat (map buildField $ M.toList env) mempty
  where
    buildField (vn, Binding _ sv) =
      ( L noLoc $ nameFromText (prettyText vn),
        if vn `elem` sizes
          then Wildcard (Info $ paramTypeFromSV sv) mempty
          else Id vn (Info $ paramTypeFromSV sv) mempty
      )

-- | Compute the corresponding type for the *representation* of a
-- given static value (not the original possibly higher-order value).
typeFromSV :: StaticVal -> ParamType
typeFromSV (Dynamic tp) =
  tp
typeFromSV (LambdaSV _ _ _ env) =
  Scalar . Record . M.fromList $
    map (bimap (nameFromString . prettyString) (typeFromSV . bindingSV)) $
      M.toList env
typeFromSV (RecordSV ls) =
  let ts = map (fmap typeFromSV) ls
   in Scalar $ Record $ M.fromList ts
typeFromSV (DynamicFun (_, sv) _) =
  typeFromSV sv
typeFromSV (SumSV name svs fields) =
  let svs' = map typeFromSV svs
   in Scalar $ Sum $ M.insert name svs' $ M.fromList fields
typeFromSV (HoleSV t _) =
  toParam Observe t
typeFromSV IntrinsicSV =
  error "Tried to get the type from the static value of an intrinsic."

resTypeFromSV :: StaticVal -> ResType
resTypeFromSV = paramToRes . typeFromSV

structTypeFromSV :: StaticVal -> StructType
structTypeFromSV = toStruct . typeFromSV

paramTypeFromSV :: StaticVal -> ParamType
paramTypeFromSV = typeFromSV

-- | Construct the type for a fully-applied dynamic function from its
-- static value and the original types of its arguments.
dynamicFunType :: StaticVal -> [ParamType] -> ([ParamType], ResType)
dynamicFunType (DynamicFun _ sv) (p : ps) =
  let (ps', ret) = dynamicFunType sv ps
   in (p : ps', ret)
dynamicFunType sv _ = ([], resTypeFromSV sv)

-- | Match a pattern with its static value. Returns an environment
-- with the identifier components of the pattern mapped to the
-- corresponding subcomponents of the static value.  If this function
-- returns 'Nothing', then it corresponds to an unmatchable case.
-- These should only occur for 'Match' expressions.
matchPatSV :: Pat ParamType -> StaticVal -> Maybe Env
matchPatSV (TuplePat ps _) (RecordSV ls) =
  mconcat <$> zipWithM (\p (_, sv) -> matchPatSV p sv) ps ls
matchPatSV (RecordPat ps _) (RecordSV ls)
  | ps' <- sortOn fst $ map (first unLoc) ps,
    ls' <- sortOn fst ls,
    map fst ps' == map fst ls' =
      mconcat <$> zipWithM (\(_, p) (_, sv) -> matchPatSV p sv) ps' ls'
matchPatSV (PatParens pat _) sv = matchPatSV pat sv
matchPatSV (PatAttr _ pat _) sv = matchPatSV pat sv
matchPatSV (Id vn (Info t) _) sv =
  -- When matching a zero-order pattern with a StaticVal, the type of
  -- the pattern wins out.  This is important for propagating sizes
  -- (but probably reveals a flaw in our bookkeeping).
  pure $
    if orderZero t
      then dim_env <> M.singleton vn (Binding Nothing $ Dynamic t)
      else dim_env <> M.singleton vn (Binding Nothing sv)
  where
    -- Extract all sizes that are potentially bound here. This is
    -- different from all free variables (see #2040).
    dim_env = bifoldMap onDim (const mempty) t
    onDim (Var v _ _) = M.singleton (qualLeaf v) i64
    onDim _ = mempty
    i64 = Binding Nothing $ Dynamic $ Scalar $ Prim $ Signed Int64
matchPatSV (Wildcard _ _) _ = pure mempty
matchPatSV (PatAscription pat _ _) sv = matchPatSV pat sv
matchPatSV PatLit {} _ = pure mempty
matchPatSV (PatConstr c1 _ ps _) (SumSV c2 ls fs)
  | c1 == c2 =
      mconcat <$> zipWithM matchPatSV ps ls
  | Just _ <- lookup c1 fs =
      Nothing
  | otherwise =
      error $ "matchPatSV: missing constructor in type: " ++ prettyString c1
matchPatSV (PatConstr c1 _ ps _) (Dynamic (Scalar (Sum fs)))
  | Just ts <- M.lookup c1 fs =
      -- A higher-order pattern can only match an appropriate SumSV.
      if all orderZero ts
        then mconcat <$> zipWithM matchPatSV ps (map svFromType ts)
        else Nothing
  | otherwise =
      error $ "matchPatSV: missing constructor in type: " ++ prettyString c1
matchPatSV pat (Dynamic t) = matchPatSV pat $ svFromType t
matchPatSV pat (HoleSV t _) = matchPatSV pat $ svFromType $ toParam Observe t
matchPatSV pat sv =
  error $
    "Tried to match pattern\n"
      ++ prettyString pat
      ++ "\n with static value\n"
      ++ show sv

alwaysMatchPatSV :: Pat ParamType -> StaticVal -> Env
alwaysMatchPatSV pat sv = fromMaybe bad $ matchPatSV pat sv
  where
    bad = error $ unlines [prettyString pat, "cannot match StaticVal", show sv]

-- | Given a pattern and the static value for the defunctionalized argument,
-- update the pattern to reflect the changes in the types.
updatePat :: Pat ParamType -> StaticVal -> Pat ParamType
updatePat (TuplePat ps loc) (RecordSV svs) =
  TuplePat (zipWith updatePat ps $ map snd svs) loc
updatePat (RecordPat ps loc) (RecordSV svs)
  | ps' <- sortOn fst ps,
    svs' <- sortOn fst svs =
      RecordPat
        (zipWith (\(n, p) (_, sv) -> (n, updatePat p sv)) ps' svs')
        loc
updatePat (PatParens pat loc) sv =
  PatParens (updatePat pat sv) loc
updatePat (PatAttr attr pat loc) sv =
  PatAttr attr (updatePat pat sv) loc
updatePat (Id vn (Info tp) loc) sv =
  Id vn (Info $ comb tp $ paramTypeFromSV sv) loc
  where
    -- Preserve any original zeroth-order types.
    comb (Scalar Arrow {}) t2 = t2
    comb (Scalar (Record m1)) (Scalar (Record m2)) =
      Scalar $ Record $ M.intersectionWith comb m1 m2
    comb (Scalar (Sum m1)) (Scalar (Sum m2)) =
      Scalar $ Sum $ M.intersectionWith (zipWith comb) m1 m2
    comb t1 _ = t1 -- t1 must be array or prim.
updatePat pat@(Wildcard (Info tp) loc) sv
  | orderZero tp = pat
  | otherwise = Wildcard (Info $ paramTypeFromSV sv) loc
updatePat (PatAscription pat _ _) sv =
  updatePat pat sv
updatePat p@PatLit {} _ = p
updatePat pat@(PatConstr c1 (Info t) ps loc) sv@(SumSV _ svs _)
  | orderZero t = pat
  | otherwise = PatConstr c1 (Info $ toParam Observe t') ps' loc
  where
    t' = resTypeFromSV sv
    ps' = zipWith updatePat ps svs
updatePat (PatConstr c1 _ ps loc) (Dynamic t) =
  PatConstr c1 (Info $ toParam Observe t) ps loc
updatePat pat (Dynamic t) = updatePat pat (svFromType t)
updatePat pat (HoleSV t _) = updatePat pat (svFromType $ toParam Observe t)
updatePat pat sv =
  error $
    "Tried to update pattern\n"
      ++ prettyString pat
      ++ "\nto reflect the static value\n"
      ++ show sv

-- | Convert a record (or tuple) type to a record static value. This
-- is used for "unwrapping" tuples and records that are nested in
-- 'Dynamic' static values.
svFromType :: ParamType -> StaticVal
svFromType (Scalar (Record fs)) = RecordSV . M.toList $ M.map svFromType fs
svFromType t = Dynamic t

-- | Defunctionalize a top-level value binding. Returns the
-- transformed result as well as an environment that binds the name of
-- the value binding to the static value of the transformed body.  The
-- boolean is true if the function is a 'DynamicFun'.
defuncValBind :: ValBind -> DefM (ValBind, Env)
-- Eta-expand entry points with a functional return type.
defuncValBind (ValBind entry name _ (Info rettype) tparams params body _ attrs loc)
  | Scalar Arrow {} <- retType rettype = do
      (body_pats, body', rettype') <- etaExpand (second (const mempty) rettype) body
      defuncValBind $
        ValBind
          entry
          name
          Nothing
          (Info rettype')
          tparams
          (params <> body_pats)
          body'
          Nothing
          attrs
          loc
defuncValBind valbind@(ValBind _ name retdecl (Info (RetType ret_dims rettype)) tparams params body _ _ _) = do
  when (any isTypeParam tparams) $
    error $
      show name
        ++ " has type parameters, "
        ++ "but the defunctionaliser expects a monomorphic input program."
  (tparams', params', body', sv, sv_t) <-
    defuncLet (map typeParamName tparams) params body $ RetType ret_dims rettype
  globals <- asks fst
  let bound_sizes = S.fromList (foldMap patNames params') <> S.fromList tparams' <> globals
  params'' <- instAnySizes params'
  let rettype' = combineTypeShapes rettype sv_t
      tparams'' = tparams' ++ unboundSizes bound_sizes params''
      ret_dims' = filter (`notElem` bound_sizes) $ S.toList $ fvVars $ freeInType rettype'

  pure
    ( valbind
        { valBindRetDecl = retdecl,
          valBindRetType =
            Info $
              if null params'
                then RetType ret_dims' $ rettype' `setUniqueness` Nonunique
                else RetType ret_dims' rettype',
          valBindTypeParams = map (`TypeParamDim` mempty) tparams'',
          valBindParams = params'',
          valBindBody = body'
        },
      M.singleton name $
        Binding
          (Just (first (map typeParamName) (valBindTypeScheme valbind)))
          sv
    )

-- | Defunctionalize a list of top-level declarations.
defuncVals :: [ValBind] -> DefM ()
defuncVals [] = pure ()
defuncVals (valbind : ds) = do
  (valbind', env) <- defuncValBind valbind
  addValBind valbind'
  let globals = valBindBound valbind'
  localEnv env $ areGlobal globals $ defuncVals ds

{-# NOINLINE transformProg #-}

-- | Transform a list of top-level value bindings. May produce new
-- lifted function definitions, which are placed in front of the
-- resulting list of declarations.
transformProg :: (MonadFreshNames m) => [ValBind] -> m [ValBind]
transformProg decs = modifyNameSource $ \namesrc ->
  let ((), namesrc', decs') = runDefM namesrc $ defuncVals decs
   in (decs', namesrc')
