-- | An interpreter operating on type-checked source Futhark terms.
-- Relatively slow.
module Language.Futhark.Interpreter
  ( Ctx (..),
    Env,
    InterpreterError,
    prettyInterpreterError,
    initialCtx,
    interpretExp,
    interpretDec,
    interpretImport,
    interpretFunction,
    ctxWithImports,
    ExtOp (..),
    BreakReason (..),
    StackFrame (..),
    typeCheckerEnv,

    -- * Values
    Value,
    fromTuple,
    isEmptyArray,
    prettyEmptyArray,
    prettyValue,
    valueText,
  )
where

import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Bifunctor (first, second)
import Data.List
  ( find,
    foldl',
    genericLength,
    genericTake,
    isPrefixOf,
    transpose,
  )
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid hiding (Sum)
import Data.Text qualified as T
import Futhark.Data qualified as V
import Futhark.Util (chunk, maybeHead, splitFromEnd)
import Futhark.Util.Loc
import Futhark.Util.Pretty hiding (apply)
import Language.Futhark hiding (Shape, matchDims)
import Language.Futhark qualified as F
import Language.Futhark.Interpreter.Values hiding (Value)
import Language.Futhark.Interpreter.Values qualified
import Language.Futhark.Primitive (floatValue, intValue)
import Language.Futhark.Primitive qualified as P
import Language.Futhark.Semantic qualified as T
import Prelude hiding (break, mod)

data StackFrame = StackFrame
  { stackFrameLoc :: Loc,
    stackFrameCtx :: Ctx
  }

instance Located StackFrame where
  locOf = stackFrameLoc

-- | What is the reason for this break point?
data BreakReason
  = -- | An explicit breakpoint in the program.
    BreakPoint
  | -- | A
    BreakNaN

data ExtOp a
  = ExtOpTrace T.Text (Doc ()) a
  | ExtOpBreak Loc BreakReason (NE.NonEmpty StackFrame) a
  | ExtOpError InterpreterError

instance Functor ExtOp where
  fmap f (ExtOpTrace w s x) = ExtOpTrace w s $ f x
  fmap f (ExtOpBreak w why backtrace x) = ExtOpBreak w why backtrace $ f x
  fmap _ (ExtOpError err) = ExtOpError err

type Stack = [StackFrame]

type Sizes = M.Map VName Int64

-- | The monad in which evaluation takes place.
newtype EvalM a
  = EvalM
      ( ReaderT
          (Stack, M.Map ImportName Env)
          (StateT Sizes (F ExtOp))
          a
      )
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadFree ExtOp,
      MonadReader (Stack, M.Map ImportName Env),
      MonadState Sizes
    )

runEvalM :: M.Map ImportName Env -> EvalM a -> F ExtOp a
runEvalM imports (EvalM m) = evalStateT (runReaderT m (mempty, imports)) mempty

stacking :: SrcLoc -> Env -> EvalM a -> EvalM a
stacking loc env = local $ \(ss, imports) ->
  if isNoLoc loc
    then (ss, imports)
    else
      let s = StackFrame (locOf loc) (Ctx env imports)
       in (s : ss, imports)
  where
    isNoLoc :: SrcLoc -> Bool
    isNoLoc = (== NoLoc) . locOf

stacktrace :: EvalM [Loc]
stacktrace = asks $ map stackFrameLoc . fst

lookupImport :: ImportName -> EvalM (Maybe Env)
lookupImport f = asks $ M.lookup f . snd

putExtSize :: VName -> Int64 -> EvalM ()
putExtSize v x = modify $ M.insert v x

getSizes :: EvalM Sizes
getSizes = get

-- | Disregard any existential sizes computed during this action.
-- This is used so that existentials computed during one iteration of
-- a loop or a function call are not remembered the next time around.
localExts :: EvalM a -> EvalM a
localExts m = do
  s <- get
  x <- m
  put s
  pure x

extSizeEnv :: EvalM Env
extSizeEnv = i64Env <$> getSizes

valueStructType :: ValueType -> StructType
valueStructType = first (ConstSize . fromIntegral)

resolveTypeParams :: [VName] -> StructType -> StructType -> Env
resolveTypeParams names = match
  where
    match (Scalar (TypeVar _ _ tn _)) t
      | qualLeaf tn `elem` names =
          typeEnv $ M.singleton (qualLeaf tn) t
    match (Scalar (Record poly_fields)) (Scalar (Record fields)) =
      mconcat $
        M.elems $
          M.intersectionWith match poly_fields fields
    match (Scalar (Sum poly_fields)) (Scalar (Sum fields)) =
      mconcat $
        map mconcat $
          M.elems $
            M.intersectionWith (zipWith match) poly_fields fields
    match
      (Scalar (Arrow _ _ _ poly_t1 (RetType _ poly_t2)))
      (Scalar (Arrow _ _ _ t1 (RetType _ t2))) =
        match poly_t1 t1 <> match poly_t2 t2
    match poly_t t
      | d1 : _ <- shapeDims (arrayShape poly_t),
        d2 : _ <- shapeDims (arrayShape t) =
          matchDims d1 d2 <> match (stripArray 1 poly_t) (stripArray 1 t)
    match _ _ = mempty

    matchDims (NamedSize (QualName _ d1)) (ConstSize d2)
      | d1 `elem` names =
          i64Env $ M.singleton d1 $ fromIntegral d2
    matchDims _ _ = mempty

resolveExistentials :: [VName] -> StructType -> ValueShape -> M.Map VName Int64
resolveExistentials names = match
  where
    match (Scalar (Record poly_fields)) (ShapeRecord fields) =
      mconcat $
        M.elems $
          M.intersectionWith match poly_fields fields
    match (Scalar (Sum poly_fields)) (ShapeSum fields) =
      mconcat $
        map mconcat $
          M.elems $
            M.intersectionWith (zipWith match) poly_fields fields
    match poly_t (ShapeDim d2 rowshape)
      | d1 : _ <- shapeDims (arrayShape poly_t) =
          matchDims d1 d2 <> match (stripArray 1 poly_t) rowshape
    match _ _ = mempty

    matchDims (NamedSize (QualName _ d1)) d2
      | d1 `elem` names = M.singleton d1 d2
    matchDims _ _ = mempty

checkShape :: Shape (Maybe Int64) -> ValueShape -> Maybe ValueShape
checkShape (ShapeDim Nothing shape1) (ShapeDim d2 shape2) =
  ShapeDim d2 <$> checkShape shape1 shape2
checkShape (ShapeDim (Just d1) shape1) (ShapeDim d2 shape2) = do
  guard $ d1 == d2
  ShapeDim d2 <$> checkShape shape1 shape2
checkShape (ShapeDim d1 shape1) ShapeLeaf =
  -- This case is for handling polymorphism, when a function doesn't
  -- know that the array it produced actually has more dimensions.
  ShapeDim (fromMaybe 0 d1) <$> checkShape shape1 ShapeLeaf
checkShape (ShapeRecord shapes1) (ShapeRecord shapes2) =
  ShapeRecord <$> sequence (M.intersectionWith checkShape shapes1 shapes2)
checkShape (ShapeRecord shapes1) ShapeLeaf =
  Just $ fromMaybe 0 <$> ShapeRecord shapes1
checkShape (ShapeSum shapes1) (ShapeSum shapes2) =
  ShapeSum <$> sequence (M.intersectionWith (zipWithM checkShape) shapes1 shapes2)
checkShape (ShapeSum shapes1) ShapeLeaf =
  Just $ fromMaybe 0 <$> ShapeSum shapes1
checkShape _ shape2 =
  Just shape2

type Value = Language.Futhark.Interpreter.Values.Value EvalM

asInteger :: Value -> Integer
asInteger (ValuePrim (SignedValue v)) = P.valueIntegral v
asInteger (ValuePrim (UnsignedValue v)) =
  toInteger (P.valueIntegral (P.doZExt v Int64) :: Word64)
asInteger v = error $ "Unexpectedly not an integer: " <> show v

asInt :: Value -> Int
asInt = fromIntegral . asInteger

asSigned :: Value -> IntValue
asSigned (ValuePrim (SignedValue v)) = v
asSigned v = error $ "Unexpected not a signed integer: " <> show v

asInt64 :: Value -> Int64
asInt64 = fromIntegral . asInteger

asBool :: Value -> Bool
asBool (ValuePrim (BoolValue x)) = x
asBool v = error $ "Unexpectedly not a boolean: " <> show v

lookupInEnv ::
  (Env -> M.Map VName x) ->
  QualName VName ->
  Env ->
  Maybe x
lookupInEnv onEnv qv env = f env $ qualQuals qv
  where
    f m (q : qs) =
      case M.lookup q $ envTerm m of
        Just (TermModule (Module mod)) -> f mod qs
        _ -> Nothing
    f m [] = M.lookup (qualLeaf qv) $ onEnv m

lookupVar :: QualName VName -> Env -> Maybe TermBinding
lookupVar = lookupInEnv envTerm

lookupType :: QualName VName -> Env -> Maybe T.TypeBinding
lookupType = lookupInEnv envType

-- | A TermValue with a 'Nothing' type annotation is an intrinsic.
data TermBinding
  = TermValue (Maybe T.BoundV) Value
  | -- | A polymorphic value that must be instantiated.
    TermPoly (Maybe T.BoundV) (StructType -> EvalM Value)
  | TermModule Module

data Module
  = Module Env
  | ModuleFun (Module -> EvalM Module)

-- | The actual type- and value environment.
data Env = Env
  { envTerm :: M.Map VName TermBinding,
    envType :: M.Map VName T.TypeBinding
  }

instance Monoid Env where
  mempty = Env mempty mempty

instance Semigroup Env where
  Env vm1 tm1 <> Env vm2 tm2 = Env (vm1 <> vm2) (tm1 <> tm2)

-- | An error occurred during interpretation due to an error in the
-- user program.  Actual interpreter errors will be signaled with an
-- IO exception ('error').
newtype InterpreterError = InterpreterError T.Text

-- | Prettyprint the error for human consumption.
prettyInterpreterError :: InterpreterError -> Doc AnsiStyle
prettyInterpreterError (InterpreterError e) = pretty e

valEnv :: M.Map VName (Maybe T.BoundV, Value) -> Env
valEnv m =
  Env
    { envTerm = M.map (uncurry TermValue) m,
      envType = mempty
    }

modEnv :: M.Map VName Module -> Env
modEnv m =
  Env
    { envTerm = M.map TermModule m,
      envType = mempty
    }

typeEnv :: M.Map VName StructType -> Env
typeEnv m =
  Env
    { envTerm = mempty,
      envType = M.map tbind m
    }
  where
    tbind = T.TypeAbbr Unlifted [] . RetType []

i64Env :: M.Map VName Int64 -> Env
i64Env = valEnv . M.map f
  where
    f x =
      ( Just $ T.BoundV [] $ Scalar $ Prim $ Signed Int64,
        ValuePrim $ SignedValue $ Int64Value x
      )

instance Show InterpreterError where
  show (InterpreterError s) = T.unpack s

bad :: SrcLoc -> Env -> T.Text -> EvalM a
bad loc env s = stacking loc env $ do
  ss <- map (locText . srclocOf) <$> stacktrace
  liftF . ExtOpError . InterpreterError $
    "Error at\n" <> prettyStacktrace 0 ss <> s

trace :: T.Text -> Value -> EvalM ()
trace w v = do
  liftF $ ExtOpTrace w (prettyValue v) ()

typeCheckerEnv :: Env -> T.Env
typeCheckerEnv env =
  -- FIXME: some shadowing issues are probably not right here.
  let valMap (TermValue (Just t) _) = Just t
      valMap _ = Nothing
      vtable = M.mapMaybe valMap $ envTerm env
      nameMap k
        | k `M.member` vtable = Just ((T.Term, baseName k), qualName k)
        | otherwise = Nothing
   in mempty
        { T.envNameMap = M.fromList $ mapMaybe nameMap $ M.keys $ envTerm env,
          T.envVtable = vtable
        }

break :: Env -> Loc -> EvalM ()
break env loc = do
  imports <- asks snd
  backtrace <- asks ((StackFrame loc (Ctx env imports) NE.:|) . fst)
  liftF $ ExtOpBreak loc BreakPoint backtrace ()

fromArray :: Value -> (ValueShape, [Value])
fromArray (ValueArray shape as) = (shape, elems as)
fromArray v = error $ "Expected array value, but found: " <> show v

apply :: SrcLoc -> Env -> Value -> Value -> EvalM Value
apply loc env (ValueFun f) v = stacking loc env (f v)
apply _ _ f _ = error $ "Cannot apply non-function: " <> show f

apply2 :: SrcLoc -> Env -> Value -> Value -> Value -> EvalM Value
apply2 loc env f x y = stacking loc env $ do
  f' <- apply noLoc mempty f x
  apply noLoc mempty f' y

matchPat :: Env -> Pat -> Value -> EvalM Env
matchPat env p v = do
  m <- runMaybeT $ patternMatch env p v
  case m of
    Nothing -> error $ "matchPat: missing case for " <> prettyString p ++ " and " <> show v
    Just env' -> pure env'

patternMatch :: Env -> Pat -> Value -> MaybeT EvalM Env
patternMatch env (Id v (Info t) _) val =
  lift $
    pure $
      valEnv (M.singleton v (Just $ T.BoundV [] $ toStruct t, val)) <> env
patternMatch env Wildcard {} _ =
  lift $ pure env
patternMatch env (TuplePat ps _) (ValueRecord vs) =
  foldM (\env' (p, v) -> patternMatch env' p v) env $
    zip ps (map snd $ sortFields vs)
patternMatch env (RecordPat ps _) (ValueRecord vs) =
  foldM (\env' (p, v) -> patternMatch env' p v) env $
    M.intersectionWith (,) (M.fromList ps) vs
patternMatch env (PatParens p _) v = patternMatch env p v
patternMatch env (PatAscription p _ _) v =
  patternMatch env p v
patternMatch env (PatLit l t _) v = do
  l' <- case l of
    PatLitInt x -> lift $ eval env $ IntLit x t mempty
    PatLitFloat x -> lift $ eval env $ FloatLit x t mempty
    PatLitPrim lv -> pure $ ValuePrim lv
  if v == l'
    then pure env
    else mzero
patternMatch env (PatConstr n _ ps _) (ValueSum _ n' vs)
  | n == n' =
      foldM (\env' (p, v) -> patternMatch env' p v) env $ zip ps vs
patternMatch _ _ _ = mzero

data Indexing
  = IndexingFix Int64
  | IndexingSlice (Maybe Int64) (Maybe Int64) (Maybe Int64)

instance Pretty Indexing where
  pretty (IndexingFix i) = pretty i
  pretty (IndexingSlice i j (Just s)) =
    maybe mempty pretty i
      <> ":"
      <> maybe mempty pretty j
      <> ":"
      <> pretty s
  pretty (IndexingSlice i (Just j) s) =
    maybe mempty pretty i
      <> ":"
      <> pretty j
      <> maybe mempty ((":" <>) . pretty) s
  pretty (IndexingSlice i Nothing Nothing) =
    maybe mempty pretty i <> ":"

indexesFor ::
  Maybe Int64 ->
  Maybe Int64 ->
  Maybe Int64 ->
  Int64 ->
  Maybe [Int]
indexesFor start end stride n
  | (start', end', stride') <- slice,
    end' == start' || signum' (end' - start') == signum' stride',
    stride' /= 0,
    is <- [start', start' + stride' .. end' - signum stride'],
    all inBounds is =
      Just $ map fromIntegral is
  | otherwise =
      Nothing
  where
    inBounds i = i >= 0 && i < n

    slice =
      case (start, end, stride) of
        (Just start', _, _) ->
          let end' = fromMaybe n end
           in (start', end', fromMaybe 1 stride)
        (Nothing, Just end', _) ->
          let start' = 0
           in (start', end', fromMaybe 1 stride)
        (Nothing, Nothing, Just stride') ->
          ( if stride' > 0 then 0 else n - 1,
            if stride' > 0 then n else -1,
            stride'
          )
        (Nothing, Nothing, Nothing) ->
          (0, n, 1)

-- | 'signum', but with 0 as 1.
signum' :: (Eq p, Num p) => p -> p
signum' 0 = 1
signum' x = signum x

indexShape :: [Indexing] -> ValueShape -> ValueShape
indexShape (IndexingFix {} : is) (ShapeDim _ shape) =
  indexShape is shape
indexShape (IndexingSlice start end stride : is) (ShapeDim d shape) =
  ShapeDim n $ indexShape is shape
  where
    n = maybe 0 genericLength $ indexesFor start end stride d
indexShape _ shape =
  shape

indexArray :: [Indexing] -> Value -> Maybe Value
indexArray (IndexingFix i : is) (ValueArray _ arr)
  | i >= 0,
    i < n =
      indexArray is $ arr ! fromIntegral i
  | otherwise =
      Nothing
  where
    n = arrayLength arr
indexArray (IndexingSlice start end stride : is) (ValueArray (ShapeDim _ rowshape) arr) = do
  js <- indexesFor start end stride $ arrayLength arr
  toArray' (indexShape is rowshape) <$> mapM (indexArray is . (arr !)) js
indexArray _ v = Just v

writeArray :: [Indexing] -> Value -> Value -> Maybe Value
writeArray slice x y = runIdentity $ updateArray (\_ y' -> pure y') slice x y

updateArray ::
  Monad m =>
  (Value -> Value -> m Value) ->
  [Indexing] ->
  Value ->
  Value ->
  m (Maybe Value)
updateArray f (IndexingFix i : is) (ValueArray shape arr) v
  | i >= 0,
    i < n = do
      v' <- updateArray f is (arr ! i') v
      pure $ do
        v'' <- v'
        Just $ ValueArray shape $ arr // [(i', v'')]
  | otherwise =
      pure Nothing
  where
    n = arrayLength arr
    i' = fromIntegral i
updateArray f (IndexingSlice start end stride : is) (ValueArray shape arr) (ValueArray _ v)
  | Just arr_is <- indexesFor start end stride $ arrayLength arr,
    length arr_is == arrayLength v = do
      let update (Just arr') (i, v') = do
            x <- updateArray f is (arr ! i) v'
            pure $ do
              x' <- x
              Just $ arr' // [(i, x')]
          update Nothing _ = pure Nothing
      fmap (fmap (ValueArray shape)) $ foldM update (Just arr) $ zip arr_is $ elems v
  | otherwise =
      pure Nothing
updateArray f _ x y = Just <$> f x y

evalDimIndex :: Env -> DimIndex -> EvalM Indexing
evalDimIndex env (DimFix x) =
  IndexingFix . asInt64 <$> eval env x
evalDimIndex env (DimSlice start end stride) =
  IndexingSlice
    <$> traverse (fmap asInt64 . eval env) start
    <*> traverse (fmap asInt64 . eval env) end
    <*> traverse (fmap asInt64 . eval env) stride

evalIndex :: SrcLoc -> Env -> [Indexing] -> Value -> EvalM Value
evalIndex loc env is arr = do
  let oob =
        bad loc env $
          "Index ["
            <> T.intercalate ", " (map prettyText is)
            <> "] out of bounds for array of shape "
            <> prettyText (valueShape arr)
            <> "."
  maybe oob pure $ indexArray is arr

-- | Expand type based on information that was not available at
-- type-checking time (the structure of abstract types).
expandType :: Env -> StructType -> StructType
expandType _ (Scalar (Prim pt)) = Scalar $ Prim pt
expandType env (Scalar (Record fs)) = Scalar $ Record $ fmap (expandType env) fs
expandType env (Scalar (Arrow () p d t1 (RetType dims t2))) =
  Scalar $ Arrow () p d (expandType env t1) (RetType dims (expandType env t2))
expandType env t@(Array _ u shape _) =
  let et = stripArray (shapeRank shape) t
      et' = expandType env et
   in arrayOf u shape et'
expandType env t@(Scalar (TypeVar () _ tn args)) =
  case lookupType tn env of
    Just (T.TypeAbbr _ ps (RetType _ t')) ->
      let (substs, types) = mconcat $ zipWith matchPtoA ps args
          onDim (NamedSize v) = fromMaybe (NamedSize v) $ M.lookup (qualLeaf v) substs
          onDim d = d
       in if null ps
            then first onDim t'
            else expandType (Env mempty types <> env) $ first onDim t'
    Nothing -> t
  where
    matchPtoA (TypeParamDim p _) (TypeArgDim (NamedSize qv) _) =
      (M.singleton p $ NamedSize qv, mempty)
    matchPtoA (TypeParamDim p _) (TypeArgDim (ConstSize k) _) =
      (M.singleton p $ ConstSize k, mempty)
    matchPtoA (TypeParamType l p _) (TypeArgType t' _) =
      let t'' = expandType env t'
       in (mempty, M.singleton p $ T.TypeAbbr l [] $ RetType [] t'')
    matchPtoA _ _ = mempty
expandType env (Scalar (Sum cs)) = Scalar $ Sum $ (fmap . fmap) (expandType env) cs

-- | First expand type abbreviations, then evaluate all possible
-- sizes.
evalType :: Env -> StructType -> EvalM StructType
evalType outer_env t = do
  size_env <- extSizeEnv
  let env = size_env <> outer_env
      evalDim (NamedSize qn)
        | Just (TermValue _ (ValuePrim (SignedValue (Int64Value x)))) <-
            lookupVar qn env =
            ConstSize $ fromIntegral x
      evalDim d = d
  pure $ first evalDim $ expandType env t

evalTermVar :: Env -> QualName VName -> StructType -> EvalM Value
evalTermVar env qv t =
  case lookupVar qv env of
    Just (TermPoly _ v) -> v =<< evalType env t
    Just (TermValue _ v) -> pure v
    _ -> error $ "\"" <> prettyString qv <> "\" is not bound to a value."

typeValueShape :: Env -> StructType -> EvalM ValueShape
typeValueShape env t = do
  t' <- evalType env t
  case traverse dim $ typeShape t' of
    Nothing -> error $ "typeValueShape: failed to fully evaluate type " <> prettyString t'
    Just shape -> pure shape
  where
    dim (ConstSize x) = Just $ fromIntegral x
    dim _ = Nothing

evalFunction :: Env -> [VName] -> [Pat] -> Exp -> StructType -> EvalM Value
-- We treat zero-parameter lambdas as simply an expression to
-- evaluate immediately.  Note that this is *not* the same as a lambda
-- that takes an empty tuple '()' as argument!  Zero-parameter lambdas
-- can never occur in a well-formed Futhark program, but they are
-- convenient in the interpreter.
evalFunction env _ [] body rettype =
  -- Eta-expand the rest to make any sizes visible.
  etaExpand [] env rettype
  where
    etaExpand vs env' (Scalar (Arrow _ _ _ pt (RetType _ rt))) =
      pure . ValueFun $ \v -> do
        env'' <- matchPat env' (Wildcard (Info $ fromStruct pt) noLoc) v
        etaExpand (v : vs) env'' rt
    etaExpand vs env' _ = do
      f <- localExts $ eval env' body
      foldM (apply noLoc mempty) f $ reverse vs
evalFunction env missing_sizes (p : ps) body rettype =
  pure . ValueFun $ \v -> do
    env' <- matchPat env p v
    -- Fix up the last sizes, if any.
    let p_t = expandType env $ patternStructType p
        env''
          | null missing_sizes =
              env'
          | otherwise =
              env' <> i64Env (resolveExistentials missing_sizes p_t (valueShape v))
    evalFunction env'' missing_sizes ps body rettype

evalFunctionBinding ::
  Env ->
  [TypeParam] ->
  [Pat] ->
  StructRetType ->
  Exp ->
  EvalM TermBinding
evalFunctionBinding env tparams ps ret fbody = do
  let arrow (xp, d, xt) yt = Scalar $ Arrow () xp d xt $ RetType [] yt
      ftype = foldr (arrow . patternParam) (retType ret) ps
      retext = case ps of
        [] -> retDims ret
        _ -> []

  -- Distinguish polymorphic and non-polymorphic bindings here.
  if null tparams
    then
      fmap (TermValue (Just $ T.BoundV [] ftype))
        . returned env (retType ret) retext
        =<< evalFunction env [] ps fbody (retType ret)
    else pure . TermPoly (Just $ T.BoundV [] ftype) $ \ftype' -> do
      let tparam_names = map typeParamName tparams
          env' = resolveTypeParams tparam_names ftype ftype' <> env

          -- In some cases (abstract lifted types) there may be
          -- missing sizes that were not fixed by the type
          -- instantiation.  These will have to be set by looking
          -- at the actual function arguments.
          missing_sizes =
            filter (`M.notMember` envTerm env') $
              map typeParamName (filter isSizeParam tparams)
      returned env (retType ret) retext
        =<< evalFunction env' missing_sizes ps fbody (retType ret)

evalArg :: Env -> Exp -> Maybe VName -> EvalM Value
evalArg env e ext = do
  v <- eval env e
  case ext of
    Just ext' -> putExtSize ext' $ asInt64 v
    Nothing -> pure ()
  pure v

returned :: Env -> TypeBase Size als -> [VName] -> Value -> EvalM Value
returned _ _ [] v = pure v
returned env ret retext v = do
  mapM_ (uncurry putExtSize) . M.toList $
    resolveExistentials retext (expandType env $ toStruct ret) $
      valueShape v
  pure v

evalAppExp :: Env -> StructType -> AppExp -> EvalM Value
evalAppExp env _ (Range start maybe_second end loc) = do
  start' <- asInteger <$> eval env start
  maybe_second' <- traverse (fmap asInteger . eval env) maybe_second
  end' <- traverse (fmap asInteger . eval env) end

  let (end_adj, step, ok) =
        case (end', maybe_second') of
          (DownToExclusive end'', Nothing) ->
            (end'' + 1, -1, start' >= end'')
          (DownToExclusive end'', Just second') ->
            (end'' + 1, second' - start', start' >= end'' && second' < start')
          (ToInclusive end'', Nothing) ->
            (end'', 1, start' <= end'')
          (ToInclusive end'', Just second')
            | second' > start' ->
                (end'', second' - start', start' <= end'')
            | otherwise ->
                (end'', second' - start', start' >= end'' && second' /= start')
          (UpToExclusive x, Nothing) ->
            (x - 1, 1, start' <= x)
          (UpToExclusive x, Just second') ->
            (x - 1, second' - start', start' <= x && second' > start')

  if ok
    then pure $ toArray' ShapeLeaf $ map toInt [start', start' + step .. end_adj]
    else bad loc env $ badRange start' maybe_second' end'
  where
    toInt =
      case typeOf start of
        Scalar (Prim (Signed t')) ->
          ValuePrim . SignedValue . intValue t'
        Scalar (Prim (Unsigned t')) ->
          ValuePrim . UnsignedValue . intValue t'
        t -> error $ "Nonsensical range type: " ++ show t

    badRange start' maybe_second' end' =
      "Range "
        <> prettyText start'
        <> ( case maybe_second' of
               Nothing -> ""
               Just second' -> ".." <> prettyText second'
           )
        <> ( case end' of
               DownToExclusive x -> "..>" <> prettyText x
               ToInclusive x -> "..." <> prettyText x
               UpToExclusive x -> "..<" <> prettyText x
           )
        <> " is invalid."
evalAppExp env t (Coerce e te loc) = do
  v <- eval env e
  case checkShape (structTypeShape t) (valueShape v) of
    Just _ -> pure v
    Nothing ->
      bad loc env . docText $
        "Value `"
          <> prettyValue v
          <> "` of shape `"
          <> pretty (valueShape v)
          <> "` cannot match shape of type `"
          <> pretty te
          <> "` (`"
          <> pretty t
          <> "`)"
evalAppExp env _ (LetPat sizes p e body _) = do
  v <- eval env e
  env' <- matchPat env p v
  let p_t = expandType env $ patternStructType p
      v_s = valueShape v
      env'' = env' <> i64Env (resolveExistentials (map sizeName sizes) p_t v_s)
  eval env'' body
evalAppExp env _ (LetFun f (tparams, ps, _, Info ret, fbody) body _) = do
  binding <- evalFunctionBinding env tparams ps ret fbody
  eval (env {envTerm = M.insert f binding $ envTerm env}) body
evalAppExp
  env
  _
  (BinOp (op, _) op_t (x, Info (_, xext)) (y, Info (_, yext)) loc)
    | baseString (qualLeaf op) == "&&" = do
        x' <- asBool <$> eval env x
        if x'
          then eval env y
          else pure $ ValuePrim $ BoolValue False
    | baseString (qualLeaf op) == "||" = do
        x' <- asBool <$> eval env x
        if x'
          then pure $ ValuePrim $ BoolValue True
          else eval env y
    | otherwise = do
        op' <- eval env $ Var op op_t loc
        x' <- evalArg env x xext
        y' <- evalArg env y yext
        apply2 loc env op' x' y'
evalAppExp env _ (If cond e1 e2 _) = do
  cond' <- asBool <$> eval env cond
  if cond' then eval env e1 else eval env e2
evalAppExp env _ (Apply f args loc) = do
  -- It is important that 'arguments' are evaluated in reverse order
  -- in order to bring any sizes into scope that may be used in the
  -- type of the functions.
  args' <- reverse <$> mapM evalArg' (reverse $ NE.toList args)
  f' <- eval env f
  foldM (apply loc env) f' args'
  where
    evalArg' (Info (_, ext), x) = evalArg env x ext
evalAppExp env _ (Index e is loc) = do
  is' <- mapM (evalDimIndex env) is
  arr <- eval env e
  evalIndex loc env is' arr
evalAppExp env _ (LetWith dest src is v body loc) = do
  let Ident src_vn (Info src_t) _ = src
  dest' <-
    maybe oob pure
      =<< writeArray
        <$> mapM (evalDimIndex env) is
        <*> evalTermVar env (qualName src_vn) (toStruct src_t)
        <*> eval env v
  let t = T.BoundV [] $ toStruct $ unInfo $ identType dest
  eval (valEnv (M.singleton (identName dest) (Just t, dest')) <> env) body
  where
    oob = bad loc env "Update out of bounds"
evalAppExp env _ (DoLoop sparams pat init_e form body _) = do
  init_v <- eval env init_e
  case form of
    For iv bound -> do
      bound' <- asSigned <$> eval env bound
      forLoop (identName iv) bound' (zero bound') init_v
    ForIn in_pat in_e -> do
      (_, in_vs) <- fromArray <$> eval env in_e
      foldM (forInLoop in_pat) init_v in_vs
    While cond ->
      whileLoop cond init_v
  where
    withLoopParams v =
      let sparams' =
            resolveExistentials
              sparams
              (patternStructType pat)
              (valueShape v)
       in matchPat (i64Env sparams' <> env) pat v

    inc = (`P.doAdd` Int64Value 1)
    zero = (`P.doMul` Int64Value 0)

    evalBody env' = localExts $ eval env' body

    forLoopEnv iv i =
      valEnv
        ( M.singleton
            iv
            ( Just $ T.BoundV [] $ Scalar $ Prim $ Signed Int64,
              ValuePrim (SignedValue i)
            )
        )

    forLoop iv bound i v
      | i >= bound = pure v
      | otherwise = do
          env' <- withLoopParams v
          forLoop iv bound (inc i) =<< evalBody (forLoopEnv iv i <> env')

    whileLoop cond v = do
      env' <- withLoopParams v
      continue <- asBool <$> eval env' cond
      if continue
        then whileLoop cond =<< evalBody env'
        else pure v

    forInLoop in_pat v in_v = do
      env' <- withLoopParams v
      env'' <- matchPat env' in_pat in_v
      evalBody env''
evalAppExp env _ (Match e cs _) = do
  v <- eval env e
  match v (NE.toList cs)
  where
    match _ [] =
      error "Pattern match failure."
    match v (c : cs') = do
      c' <- evalCase v env c
      case c' of
        Just v' -> pure v'
        Nothing -> match v cs'

eval :: Env -> Exp -> EvalM Value
eval _ (Literal v _) = pure $ ValuePrim v
eval env (Hole (Info t) loc) =
  bad loc env $ "Hole of type: " <> prettyTextOneLine t
eval env (Parens e _) = eval env e
eval env (QualParens (qv, _) e loc) = do
  m <- evalModuleVar env qv
  case m of
    ModuleFun {} -> error $ "Local open of module function at " ++ locStr loc
    Module m' -> eval (m' <> env) e
eval env (TupLit vs _) = toTuple <$> mapM (eval env) vs
eval env (RecordLit fields _) =
  ValueRecord . M.fromList <$> mapM evalField fields
  where
    evalField (RecordFieldExplicit k e _) = do
      v <- eval env e
      pure (k, v)
    evalField (RecordFieldImplicit k t loc) = do
      v <- eval env $ Var (qualName k) t loc
      pure (baseName k, v)
eval _ (StringLit vs _) =
  pure $
    toArray' ShapeLeaf $
      map (ValuePrim . UnsignedValue . Int8Value . fromIntegral) vs
eval env (ArrayLit [] (Info t) _) = do
  t' <- typeValueShape env $ toStruct t
  pure $ toArray t' []
eval env (ArrayLit (v : vs) _ _) = do
  v' <- eval env v
  vs' <- mapM (eval env) vs
  pure $ toArray' (valueShape v') (v' : vs')
eval env (AppExp e (Info (AppRes t retext))) = do
  t' <- evalType env $ toStruct t
  returned env t' retext =<< evalAppExp env t' e
eval env (Var qv (Info t) _) = evalTermVar env qv (toStruct t)
eval env (Ascript e _ _) = eval env e
eval _ (IntLit v (Info t) _) =
  case t of
    Scalar (Prim (Signed it)) ->
      pure $ ValuePrim $ SignedValue $ intValue it v
    Scalar (Prim (Unsigned it)) ->
      pure $ ValuePrim $ UnsignedValue $ intValue it v
    Scalar (Prim (FloatType ft)) ->
      pure $ ValuePrim $ FloatValue $ floatValue ft v
    _ -> error $ "eval: nonsensical type for integer literal: " <> prettyString t
eval _ (FloatLit v (Info t) _) =
  case t of
    Scalar (Prim (FloatType ft)) ->
      pure $ ValuePrim $ FloatValue $ floatValue ft v
    _ -> error $ "eval: nonsensical type for float literal: " <> prettyString t
eval env (Negate e _) = do
  ev <- eval env e
  ValuePrim <$> case ev of
    ValuePrim (SignedValue (Int8Value v)) -> pure $ SignedValue $ Int8Value (-v)
    ValuePrim (SignedValue (Int16Value v)) -> pure $ SignedValue $ Int16Value (-v)
    ValuePrim (SignedValue (Int32Value v)) -> pure $ SignedValue $ Int32Value (-v)
    ValuePrim (SignedValue (Int64Value v)) -> pure $ SignedValue $ Int64Value (-v)
    ValuePrim (UnsignedValue (Int8Value v)) -> pure $ UnsignedValue $ Int8Value (-v)
    ValuePrim (UnsignedValue (Int16Value v)) -> pure $ UnsignedValue $ Int16Value (-v)
    ValuePrim (UnsignedValue (Int32Value v)) -> pure $ UnsignedValue $ Int32Value (-v)
    ValuePrim (UnsignedValue (Int64Value v)) -> pure $ UnsignedValue $ Int64Value (-v)
    ValuePrim (FloatValue (Float16Value v)) -> pure $ FloatValue $ Float16Value (-v)
    ValuePrim (FloatValue (Float32Value v)) -> pure $ FloatValue $ Float32Value (-v)
    ValuePrim (FloatValue (Float64Value v)) -> pure $ FloatValue $ Float64Value (-v)
    _ -> error $ "Cannot negate " <> show ev
eval env (Not e _) = do
  ev <- eval env e
  ValuePrim <$> case ev of
    ValuePrim (BoolValue b) -> pure $ BoolValue $ not b
    ValuePrim (SignedValue iv) -> pure $ SignedValue $ P.doComplement iv
    ValuePrim (UnsignedValue iv) -> pure $ UnsignedValue $ P.doComplement iv
    _ -> error $ "Cannot logically negate " <> show ev
eval env (Update src is v loc) =
  maybe oob pure
    =<< writeArray <$> mapM (evalDimIndex env) is <*> eval env src <*> eval env v
  where
    oob = bad loc env "Bad update"
eval env (RecordUpdate src all_fs v _ _) =
  update <$> eval env src <*> pure all_fs <*> eval env v
  where
    update _ [] v' = v'
    update (ValueRecord src') (f : fs) v'
      | Just f_v <- M.lookup f src' =
          ValueRecord $ M.insert f (update f_v fs v') src'
    update _ _ _ = error "eval RecordUpdate: invalid value."
-- We treat zero-parameter lambdas as simply an expression to
-- evaluate immediately.  Note that this is *not* the same as a lambda
-- that takes an empty tuple '()' as argument!  Zero-parameter lambdas
-- can never occur in a well-formed Futhark program, but they are
-- convenient in the interpreter.
eval env (Lambda ps body _ (Info (_, RetType _ rt)) _) =
  evalFunction env [] ps body rt
eval env (OpSection qv (Info t) _) =
  evalTermVar env qv $ toStruct t
eval env (OpSectionLeft qv _ e (Info (_, _, argext), _) (Info (RetType _ t), _) loc) = do
  v <- evalArg env e argext
  f <- evalTermVar env qv (toStruct t)
  apply loc env f v
eval env (OpSectionRight qv _ e (Info _, Info (_, _, argext)) (Info (RetType _ t)) loc) = do
  y <- evalArg env e argext
  pure $
    ValueFun $ \x -> do
      f <- evalTermVar env qv $ toStruct t
      apply2 loc env f x y
eval env (IndexSection is _ loc) = do
  is' <- mapM (evalDimIndex env) is
  pure $ ValueFun $ evalIndex loc env is'
eval _ (ProjectSection ks _ _) =
  pure $ ValueFun $ flip (foldM walk) ks
  where
    walk (ValueRecord fs) f
      | Just v' <- M.lookup f fs = pure v'
    walk _ _ = error "Value does not have expected field."
eval env (Project f e _ _) = do
  v <- eval env e
  case v of
    ValueRecord fs | Just v' <- M.lookup f fs -> pure v'
    _ -> error "Value does not have expected field."
eval env (Assert what e (Info s) loc) = do
  cond <- asBool <$> eval env what
  unless cond $ bad loc env s
  eval env e
eval env (Constr c es (Info t) _) = do
  vs <- mapM (eval env) es
  shape <- typeValueShape env $ toStruct t
  pure $ ValueSum shape c vs
eval env (Attr (AttrAtom (AtomName "break") _) e loc) = do
  break env (locOf loc)
  eval env e
eval env (Attr (AttrAtom (AtomName "trace") _) e loc) = do
  v <- eval env e
  trace (locText (locOf loc)) v
  pure v
eval env (Attr (AttrComp "trace" [AttrAtom (AtomName tag) _] _) e _) = do
  v <- eval env e
  trace (nameToText tag) v
  pure v
eval env (Attr _ e _) =
  eval env e

evalCase ::
  Value ->
  Env ->
  CaseBase Info VName ->
  EvalM (Maybe Value)
evalCase v env (CasePat p cExp _) = runMaybeT $ do
  env' <- patternMatch env p v
  lift $ eval env' cExp

-- We hackily do multiple substitutions in modules, because otherwise
-- we would lose in cases where the parameter substitutions are [a->x,
-- b->x] when we reverse. (See issue #1250.)
reverseSubstitutions :: M.Map VName VName -> M.Map VName [VName]
reverseSubstitutions =
  M.fromListWith (<>) . map (second pure . uncurry (flip (,))) . M.toList

substituteInModule :: M.Map VName VName -> Module -> Module
substituteInModule substs = onModule
  where
    rev_substs = reverseSubstitutions substs
    replace v = fromMaybe [v] $ M.lookup v rev_substs
    replaceQ v = maybe v qualName $ maybeHead =<< M.lookup (qualLeaf v) rev_substs
    replaceM f m = M.fromList $ do
      (k, v) <- M.toList m
      k' <- replace k
      pure (k', f v)
    onModule (Module (Env terms types)) =
      Module $ Env (replaceM onTerm terms) (replaceM onType types)
    onModule (ModuleFun f) =
      ModuleFun $ \m -> onModule <$> f (substituteInModule substs m)
    onTerm (TermValue t v) = TermValue t v
    onTerm (TermPoly t v) = TermPoly t v
    onTerm (TermModule m) = TermModule $ onModule m
    onType (T.TypeAbbr l ps t) = T.TypeAbbr l ps $ first onDim t
    onDim (NamedSize v) = NamedSize $ replaceQ v
    onDim (ConstSize x) = ConstSize x
    onDim (AnySize v) = AnySize v

evalModuleVar :: Env -> QualName VName -> EvalM Module
evalModuleVar env qv =
  case lookupVar qv env of
    Just (TermModule m) -> pure m
    _ -> error $ prettyString qv <> " is not bound to a module."

evalModExp :: Env -> ModExp -> EvalM Module
evalModExp _ (ModImport _ (Info f) _) = do
  f' <- lookupImport f
  known <- asks snd
  case f' of
    Nothing ->
      error $
        unlines
          [ "Unknown interpreter import: " ++ show f,
            "Known: " ++ show (M.keys known)
          ]
    Just m -> pure $ Module m
evalModExp env (ModDecs ds _) = do
  Env terms types <- foldM evalDec env ds
  -- Remove everything that was present in the original Env.
  pure $
    Module $
      Env
        (terms `M.difference` envTerm env)
        (types `M.difference` envType env)
evalModExp env (ModVar qv _) =
  evalModuleVar env qv
evalModExp env (ModAscript me _ (Info substs) _) =
  substituteInModule substs <$> evalModExp env me
evalModExp env (ModParens me _) = evalModExp env me
evalModExp env (ModLambda p ret e loc) =
  pure $
    ModuleFun $ \am -> do
      let env' = env {envTerm = M.insert (modParamName p) (TermModule am) $ envTerm env}
      evalModExp env' $ case ret of
        Nothing -> e
        Just (se, rsubsts) -> ModAscript e se rsubsts loc
evalModExp env (ModApply f e (Info psubst) (Info rsubst) _) = do
  f' <- evalModExp env f
  case f' of
    ModuleFun f'' -> do
      e' <- evalModExp env e
      substituteInModule rsubst <$> f'' (substituteInModule psubst e')
    _ -> error "Expected ModuleFun."

evalDec :: Env -> Dec -> EvalM Env
evalDec env (ValDec (ValBind _ v _ (Info ret) tparams ps fbody _ _ _)) = do
  binding <- evalFunctionBinding env tparams ps ret fbody
  pure $ env {envTerm = M.insert v binding $ envTerm env}
evalDec env (OpenDec me _) = do
  me' <- evalModExp env me
  case me' of
    Module me'' -> pure $ me'' <> env
    _ -> error "Expected Module"
evalDec env (ImportDec name name' loc) =
  evalDec env $ LocalDec (OpenDec (ModImport name name' loc) loc) loc
evalDec env (LocalDec d _) = evalDec env d
evalDec env SigDec {} = pure env
evalDec env (TypeDec (TypeBind v l ps _ (Info (RetType dims t)) _ _)) = do
  let abbr = T.TypeAbbr l ps . RetType dims $ expandType env t
  pure env {envType = M.insert v abbr $ envType env}
evalDec env (ModDec (ModBind v ps ret body _ loc)) = do
  mod <- evalModExp env $ wrapInLambda ps
  pure $ modEnv (M.singleton v mod) <> env
  where
    wrapInLambda [] = case ret of
      Just (se, substs) -> ModAscript body se substs loc
      Nothing -> body
    wrapInLambda [p] = ModLambda p ret body loc
    wrapInLambda (p : ps') = ModLambda p Nothing (wrapInLambda ps') loc

-- | The interpreter context.  All evaluation takes place with respect
-- to a context, and it can be extended with more definitions, which
-- is how the REPL works.
data Ctx = Ctx
  { ctxEnv :: Env,
    ctxImports :: M.Map ImportName Env
  }

nanValue :: PrimValue -> Bool
nanValue (FloatValue v) =
  case v of
    Float16Value x -> isNaN x
    Float32Value x -> isNaN x
    Float64Value x -> isNaN x
nanValue _ = False

breakOnNaN :: [PrimValue] -> PrimValue -> EvalM ()
breakOnNaN inputs result
  | not (any nanValue inputs) && nanValue result = do
      backtrace <- asks fst
      case NE.nonEmpty backtrace of
        Nothing -> pure ()
        Just backtrace' ->
          let loc = stackFrameLoc $ NE.head backtrace'
           in liftF $ ExtOpBreak loc BreakNaN backtrace' ()
breakOnNaN _ _ =
  pure ()

-- | The initial environment contains definitions of the various intrinsic functions.
initialCtx :: Ctx
initialCtx =
  Ctx
    ( Env
        ( M.insert
            (VName (nameFromString "intrinsics") 0)
            (TermModule (Module $ Env terms types))
            terms
        )
        types
    )
    mempty
  where
    terms = M.mapMaybeWithKey (const . def . baseString) intrinsics
    types = M.mapMaybeWithKey (const . tdef . baseString) intrinsics

    sintOp f =
      [ (getS, putS, P.doBinOp (f Int8)),
        (getS, putS, P.doBinOp (f Int16)),
        (getS, putS, P.doBinOp (f Int32)),
        (getS, putS, P.doBinOp (f Int64))
      ]
    uintOp f =
      [ (getU, putU, P.doBinOp (f Int8)),
        (getU, putU, P.doBinOp (f Int16)),
        (getU, putU, P.doBinOp (f Int32)),
        (getU, putU, P.doBinOp (f Int64))
      ]
    intOp f = sintOp f ++ uintOp f
    floatOp f =
      [ (getF, putF, P.doBinOp (f Float16)),
        (getF, putF, P.doBinOp (f Float32)),
        (getF, putF, P.doBinOp (f Float64))
      ]
    arithOp f g = Just $ bopDef $ intOp f ++ floatOp g

    flipCmps = map (\(f, g, h) -> (f, g, flip h))
    sintCmp f =
      [ (getS, Just . BoolValue, P.doCmpOp (f Int8)),
        (getS, Just . BoolValue, P.doCmpOp (f Int16)),
        (getS, Just . BoolValue, P.doCmpOp (f Int32)),
        (getS, Just . BoolValue, P.doCmpOp (f Int64))
      ]
    uintCmp f =
      [ (getU, Just . BoolValue, P.doCmpOp (f Int8)),
        (getU, Just . BoolValue, P.doCmpOp (f Int16)),
        (getU, Just . BoolValue, P.doCmpOp (f Int32)),
        (getU, Just . BoolValue, P.doCmpOp (f Int64))
      ]
    floatCmp f =
      [ (getF, Just . BoolValue, P.doCmpOp (f Float16)),
        (getF, Just . BoolValue, P.doCmpOp (f Float32)),
        (getF, Just . BoolValue, P.doCmpOp (f Float64))
      ]
    boolCmp f = [(getB, Just . BoolValue, P.doCmpOp f)]

    getV (SignedValue x) = Just $ P.IntValue x
    getV (UnsignedValue x) = Just $ P.IntValue x
    getV (FloatValue x) = Just $ P.FloatValue x
    getV (BoolValue x) = Just $ P.BoolValue x
    putV (P.IntValue x) = SignedValue x
    putV (P.FloatValue x) = FloatValue x
    putV (P.BoolValue x) = BoolValue x
    putV P.UnitValue = BoolValue True

    getS (SignedValue x) = Just $ P.IntValue x
    getS _ = Nothing
    putS (P.IntValue x) = Just $ SignedValue x
    putS _ = Nothing

    getU (UnsignedValue x) = Just $ P.IntValue x
    getU _ = Nothing
    putU (P.IntValue x) = Just $ UnsignedValue x
    putU _ = Nothing

    getF (FloatValue x) = Just $ P.FloatValue x
    getF _ = Nothing
    putF (P.FloatValue x) = Just $ FloatValue x
    putF _ = Nothing

    getB (BoolValue x) = Just $ P.BoolValue x
    getB _ = Nothing
    putB (P.BoolValue x) = Just $ BoolValue x
    putB _ = Nothing

    fun1 f =
      TermValue Nothing $ ValueFun $ \x -> f x

    fun2 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y -> f x y

    fun3 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y ->
          pure . ValueFun $ \z -> f x y z

    fun5 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y ->
          pure . ValueFun $ \z ->
            pure . ValueFun $ \a ->
              pure . ValueFun $ \b -> f x y z a b

    fun6 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y ->
          pure . ValueFun $ \z ->
            pure . ValueFun $ \a ->
              pure . ValueFun $ \b ->
                pure . ValueFun $ \c -> f x y z a b c

    fun7 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y ->
          pure . ValueFun $ \z ->
            pure . ValueFun $ \a ->
              pure . ValueFun $ \b ->
                pure . ValueFun $ \c ->
                  pure . ValueFun $ \d -> f x y z a b c d

    fun8 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y ->
          pure . ValueFun $ \z ->
            pure . ValueFun $ \a ->
              pure . ValueFun $ \b ->
                pure . ValueFun $ \c ->
                  pure . ValueFun $ \d ->
                    pure . ValueFun $ \e -> f x y z a b c d e

    fun10 f =
      TermValue Nothing . ValueFun $ \x ->
        pure . ValueFun $ \y ->
          pure . ValueFun $ \z ->
            pure . ValueFun $ \a ->
              pure . ValueFun $ \b ->
                pure . ValueFun $ \c ->
                  pure . ValueFun $ \d ->
                    pure . ValueFun $ \e ->
                      pure . ValueFun $ \g ->
                        pure . ValueFun $ \h -> f x y z a b c d e g h

    bopDef fs = fun2 $ \x y ->
      case (x, y) of
        (ValuePrim x', ValuePrim y')
          | Just z <- msum $ map (`bopDef'` (x', y')) fs -> do
              breakOnNaN [x', y'] z
              pure $ ValuePrim z
        _ ->
          bad noLoc mempty . docText $
            "Cannot apply operator to arguments"
              <+> dquotes (prettyValue x)
              <+> "and"
              <+> dquotes (prettyValue y)
                <> "."
      where
        bopDef' (valf, retf, op) (x, y) = do
          x' <- valf x
          y' <- valf y
          retf =<< op x' y'

    unopDef fs = fun1 $ \x ->
      case x of
        (ValuePrim x')
          | Just r <- msum $ map (`unopDef'` x') fs -> do
              breakOnNaN [x'] r
              pure $ ValuePrim r
        _ ->
          bad noLoc mempty . docText $
            "Cannot apply function to argument"
              <+> dquotes (prettyValue x)
                <> "."
      where
        unopDef' (valf, retf, op) x = do
          x' <- valf x
          retf =<< op x'

    tbopDef f = fun1 $ \v ->
      case fromTuple v of
        Just [ValuePrim x, ValuePrim y]
          | Just x' <- getV x,
            Just y' <- getV y,
            Just z <- putV <$> f x' y' -> do
              breakOnNaN [x, y] z
              pure $ ValuePrim z
        _ ->
          bad noLoc mempty . docText $
            "Cannot apply operator to argument"
              <+> dquotes (prettyValue v) <> "."

    def "!" =
      Just $
        unopDef
          [ (getS, putS, P.doUnOp $ P.Complement Int8),
            (getS, putS, P.doUnOp $ P.Complement Int16),
            (getS, putS, P.doUnOp $ P.Complement Int32),
            (getS, putS, P.doUnOp $ P.Complement Int64),
            (getU, putU, P.doUnOp $ P.Complement Int8),
            (getU, putU, P.doUnOp $ P.Complement Int16),
            (getU, putU, P.doUnOp $ P.Complement Int32),
            (getU, putU, P.doUnOp $ P.Complement Int64),
            (getB, putB, P.doUnOp P.Not)
          ]
    def "+" = arithOp (`P.Add` P.OverflowWrap) P.FAdd
    def "-" = arithOp (`P.Sub` P.OverflowWrap) P.FSub
    def "*" = arithOp (`P.Mul` P.OverflowWrap) P.FMul
    def "**" = arithOp P.Pow P.FPow
    def "/" =
      Just $
        bopDef $
          sintOp (`P.SDiv` P.Unsafe)
            ++ uintOp (`P.UDiv` P.Unsafe)
            ++ floatOp P.FDiv
    def "%" =
      Just $
        bopDef $
          sintOp (`P.SMod` P.Unsafe)
            ++ uintOp (`P.UMod` P.Unsafe)
            ++ floatOp P.FMod
    def "//" =
      Just $
        bopDef $
          sintOp (`P.SQuot` P.Unsafe)
            ++ uintOp (`P.UDiv` P.Unsafe)
    def "%%" =
      Just $
        bopDef $
          sintOp (`P.SRem` P.Unsafe)
            ++ uintOp (`P.UMod` P.Unsafe)
    def "^" = Just $ bopDef $ intOp P.Xor
    def "&" = Just $ bopDef $ intOp P.And
    def "|" = Just $ bopDef $ intOp P.Or
    def ">>" = Just $ bopDef $ sintOp P.AShr ++ uintOp P.LShr
    def "<<" = Just $ bopDef $ intOp P.Shl
    def ">>>" = Just $ bopDef $ sintOp P.LShr ++ uintOp P.LShr
    def "==" = Just $
      fun2 $
        \xs ys -> pure $ ValuePrim $ BoolValue $ xs == ys
    def "!=" = Just $
      fun2 $
        \xs ys -> pure $ ValuePrim $ BoolValue $ xs /= ys
    -- The short-circuiting is handled directly in 'eval'; these cases
    -- are only used when partially applying and such.
    def "&&" = Just $
      fun2 $ \x y ->
        pure $ ValuePrim $ BoolValue $ asBool x && asBool y
    def "||" = Just $
      fun2 $ \x y ->
        pure $ ValuePrim $ BoolValue $ asBool x || asBool y
    def "<" =
      Just $
        bopDef $
          sintCmp P.CmpSlt
            ++ uintCmp P.CmpUlt
            ++ floatCmp P.FCmpLt
            ++ boolCmp P.CmpLlt
    def ">" =
      Just $
        bopDef $
          flipCmps $
            sintCmp P.CmpSlt
              ++ uintCmp P.CmpUlt
              ++ floatCmp P.FCmpLt
              ++ boolCmp P.CmpLlt
    def "<=" =
      Just $
        bopDef $
          sintCmp P.CmpSle
            ++ uintCmp P.CmpUle
            ++ floatCmp P.FCmpLe
            ++ boolCmp P.CmpLle
    def ">=" =
      Just $
        bopDef $
          flipCmps $
            sintCmp P.CmpSle
              ++ uintCmp P.CmpUle
              ++ floatCmp P.FCmpLe
              ++ boolCmp P.CmpLle
    def s
      | Just bop <- find ((s ==) . prettyString) P.allBinOps =
          Just $ tbopDef $ P.doBinOp bop
      | Just unop <- find ((s ==) . prettyString) P.allCmpOps =
          Just $ tbopDef $ \x y -> P.BoolValue <$> P.doCmpOp unop x y
      | Just cop <- find ((s ==) . prettyString) P.allConvOps =
          Just $ unopDef [(getV, Just . putV, P.doConvOp cop)]
      | Just unop <- find ((s ==) . prettyString) P.allUnOps =
          Just $ unopDef [(getV, Just . putV, P.doUnOp unop)]
      | Just (pts, _, f) <- M.lookup s P.primFuns =
          case length pts of
            1 -> Just $ unopDef [(getV, Just . putV, f . pure)]
            _ -> Just $
              fun1 $ \x -> do
                let getV' (ValuePrim v) = Just v
                    getV' _ = Nothing
                case mapM getV' =<< fromTuple x of
                  Just vs
                    | Just res <- fmap putV . f =<< mapM getV vs -> do
                        breakOnNaN vs res
                        pure $ ValuePrim res
                  _ ->
                    error $ "Cannot apply " <> prettyString s ++ " to " <> show x
      | "sign_" `isPrefixOf` s =
          Just $
            fun1 $ \x ->
              case x of
                (ValuePrim (UnsignedValue x')) ->
                  pure $ ValuePrim $ SignedValue x'
                _ -> error $ "Cannot sign: " <> show x
      | "unsign_" `isPrefixOf` s =
          Just $
            fun1 $ \x ->
              case x of
                (ValuePrim (SignedValue x')) ->
                  pure $ ValuePrim $ UnsignedValue x'
                _ -> error $ "Cannot unsign: " <> show x
    def s
      | "map_stream" `isPrefixOf` s =
          Just $ fun2 stream
    def s | "reduce_stream" `isPrefixOf` s =
      Just $ fun3 $ \_ f arg -> stream f arg
    def "map" = Just $
      TermPoly Nothing $ \t -> pure $
        ValueFun $ \f -> pure . ValueFun $ \xs ->
          case unfoldFunType t of
            ([_, _], ret_t)
              | Just rowshape <- typeRowShape ret_t ->
                  toArray' rowshape <$> mapM (apply noLoc mempty f) (snd $ fromArray xs)
              | otherwise ->
                  error $ "Bad return type: " <> prettyString ret_t
            _ ->
              error $
                "Invalid arguments to map intrinsic:\n"
                  ++ unlines [prettyString t, show f, show xs]
      where
        typeRowShape = sequenceA . structTypeShape . stripArray 1
    def s | "reduce" `isPrefixOf` s = Just $
      fun3 $ \f ne xs ->
        foldM (apply2 noLoc mempty f) ne $ snd $ fromArray xs
    def "scan" = Just $
      fun3 $ \f ne xs -> do
        let next (out, acc) x = do
              x' <- apply2 noLoc mempty f acc x
              pure (x' : out, x')
        toArray' (valueShape ne) . reverse . fst
          <$> foldM next ([], ne) (snd $ fromArray xs)
    def "scatter" = Just $
      fun3 $ \arr is vs ->
        case arr of
          ValueArray shape arr' ->
            pure $
              ValueArray shape $
                foldl' update arr' $
                  zip (map asInt $ snd $ fromArray is) (snd $ fromArray vs)
          _ ->
            error $ "scatter expects array, but got: " <> show arr
      where
        update arr' (i, v) =
          if i >= 0 && i < arrayLength arr'
            then arr' // [(i, v)]
            else arr'
    def "scatter_2d" = Just $
      fun3 $ \arr is vs ->
        case arr of
          ValueArray _ _ ->
            pure $
              foldl' update arr $
                zip (map fromTuple $ snd $ fromArray is) (snd $ fromArray vs)
          _ ->
            error $ "scatter_2d expects array, but got: " <> show arr
      where
        update :: Value -> (Maybe [Value], Value) -> Value
        update arr (Just idxs@[_, _], v) =
          fromMaybe arr $ writeArray (map (IndexingFix . asInt64) idxs) arr v
        update _ _ =
          error "scatter_2d expects 2-dimensional indices"
    def "scatter_3d" = Just $
      fun3 $ \arr is vs ->
        case arr of
          ValueArray _ _ ->
            pure $
              foldl' update arr $
                zip (map fromTuple $ snd $ fromArray is) (snd $ fromArray vs)
          _ ->
            error $ "scatter_3d expects array, but got: " <> show arr
      where
        update :: Value -> (Maybe [Value], Value) -> Value
        update arr (Just idxs@[_, _, _], v) =
          fromMaybe arr $ writeArray (map (IndexingFix . asInt64) idxs) arr v
        update _ _ =
          error "scatter_3d expects 3-dimensional indices"
    def "hist_1d" = Just . fun6 $ \_ arr fun _ is vs ->
      foldM
        (update fun)
        arr
        (zip (map asInt64 $ snd $ fromArray is) (snd $ fromArray vs))
      where
        op = apply2 mempty mempty
        update fun arr (i, v) =
          fromMaybe arr <$> updateArray (op fun) [IndexingFix i] arr v
    def "hist_2d" = Just . fun6 $ \_ arr fun _ is vs ->
      foldM
        (update fun)
        arr
        (zip (map fromTuple $ snd $ fromArray is) (snd $ fromArray vs))
      where
        op = apply2 mempty mempty
        update fun arr (Just idxs@[_, _], v) =
          fromMaybe arr
            <$> updateArray (op fun) (map (IndexingFix . asInt64) idxs) arr v
        update _ _ _ =
          error "hist_2d: bad index value"
    def "hist_3d" = Just . fun6 $ \_ arr fun _ is vs ->
      foldM
        (update fun)
        arr
        (zip (map fromTuple $ snd $ fromArray is) (snd $ fromArray vs))
      where
        op = apply2 mempty mempty
        update fun arr (Just idxs@[_, _, _], v) =
          fromMaybe arr
            <$> updateArray (op fun) (map (IndexingFix . asInt64) idxs) arr v
        update _ _ _ =
          error "hist_2d: bad index value"
    def "partition" = Just $
      fun3 $ \k f xs -> do
        let (ShapeDim _ rowshape, xs') = fromArray xs

            next outs x = do
              i <- asInt <$> apply noLoc mempty f x
              pure $ insertAt i x outs
            pack parts =
              toTuple
                [ toArray' rowshape $ concat parts,
                  toArray' rowshape $
                    map (ValuePrim . SignedValue . Int64Value . genericLength) parts
                ]

        pack . map reverse
          <$> foldM next (replicate (asInt k) []) xs'
      where
        insertAt 0 x (l : ls) = (x : l) : ls
        insertAt i x (l : ls) = l : insertAt (i - 1) x ls
        insertAt _ _ ls = ls
    def "scatter_stream" = Just $
      fun3 $ \dest f vs ->
        case (dest, vs) of
          ( ValueArray dest_shape dest_arr,
            ValueArray _ vs_arr
            ) -> do
              let acc = ValueAcc (\_ x -> pure x) dest_arr
              acc' <- foldM (apply2 noLoc mempty f) acc vs_arr
              case acc' of
                ValueAcc _ dest_arr' ->
                  pure $ ValueArray dest_shape dest_arr'
                _ ->
                  error $ "scatter_stream produced: " <> show acc'
          _ ->
            error $ "scatter_stream expects array, but got: " <> prettyString (show vs, show vs)
    def "hist_stream" = Just $
      fun5 $ \dest op _ne f vs ->
        case (dest, vs) of
          ( ValueArray dest_shape dest_arr,
            ValueArray _ vs_arr
            ) -> do
              let acc = ValueAcc (apply2 noLoc mempty op) dest_arr
              acc' <- foldM (apply2 noLoc mempty f) acc vs_arr
              case acc' of
                ValueAcc _ dest_arr' ->
                  pure $ ValueArray dest_shape dest_arr'
                _ ->
                  error $ "hist_stream produced: " <> show acc'
          _ ->
            error $ "hist_stream expects array, but got: " <> prettyString (show dest, show vs)
    def "acc_write" = Just $
      fun3 $ \acc i v ->
        case (acc, i) of
          ( ValueAcc op acc_arr,
            ValuePrim (SignedValue (Int64Value i'))
            ) ->
              if i' >= 0 && i' < arrayLength acc_arr
                then do
                  let x = acc_arr ! fromIntegral i'
                  res <- op x v
                  pure $ ValueAcc op $ acc_arr // [(fromIntegral i', res)]
                else pure acc
          _ ->
            error $ "acc_write invalid arguments: " <> prettyString (show acc, show i, show v)
    --
    def "flat_index_2d" = Just . fun6 $ \arr offset n1 s1 n2 s2 -> do
      let offset' = asInt64 offset
          n1' = asInt64 n1
          n2' = asInt64 n2
          s1' = asInt64 s1
          s2' = asInt64 s2
          shapeFromDims = foldr ShapeDim ShapeLeaf
          mk1 = fmap (toArray (shapeFromDims [n1', n2'])) . sequence
          mk2 = fmap (toArray $ shapeFromDims [n2']) . sequence
          iota x = [0 .. x - 1]
          f i j =
            indexArray [IndexingFix $ offset' + i * s1' + j * s2'] arr

      case mk1 [mk2 [f i j | j <- iota n2'] | i <- iota n1'] of
        Just arr' -> pure arr'
        Nothing ->
          bad mempty mempty $
            "Index out of bounds: " <> prettyText [((n1', s1'), (n2', s2'))]
    --
    def "flat_update_2d" = Just . fun5 $ \arr offset s1 s2 v -> do
      let offset' = asInt64 offset
          s1' = asInt64 s1
          s2' = asInt64 s2
      case valueShape v of
        ShapeDim n1 (ShapeDim n2 _) -> do
          let iota x = [0 .. x - 1]
              f arr' (i, j) =
                writeArray [IndexingFix $ offset' + i * s1' + j * s2'] arr'
                  =<< indexArray [IndexingFix i, IndexingFix j] v
          case foldM f arr [(i, j) | i <- iota n1, j <- iota n2] of
            Just arr' -> pure arr'
            Nothing ->
              bad mempty mempty $
                "Index out of bounds: " <> prettyText [((n1, s1'), (n2, s2'))]
        s -> error $ "flat_update_2d: invalid arg shape: " ++ show s
    --
    def "flat_index_3d" = Just . fun8 $ \arr offset n1 s1 n2 s2 n3 s3 -> do
      let offset' = asInt64 offset
          n1' = asInt64 n1
          n2' = asInt64 n2
          n3' = asInt64 n3
          s1' = asInt64 s1
          s2' = asInt64 s2
          s3' = asInt64 s3
          shapeFromDims = foldr ShapeDim ShapeLeaf
          mk1 = fmap (toArray (shapeFromDims [n1', n2', n3'])) . sequence
          mk2 = fmap (toArray $ shapeFromDims [n2', n3']) . sequence
          mk3 = fmap (toArray $ shapeFromDims [n3']) . sequence
          iota x = [0 .. x - 1]
          f i j l =
            indexArray [IndexingFix $ offset' + i * s1' + j * s2' + l * s3'] arr

      case mk1 [mk2 [mk3 [f i j l | l <- iota n3'] | j <- iota n2'] | i <- iota n1'] of
        Just arr' -> pure arr'
        Nothing ->
          bad mempty mempty $
            "Index out of bounds: " <> prettyText [((n1', s1'), (n2', s2'), (n3', s3'))]
    --
    def "flat_update_3d" = Just . fun6 $ \arr offset s1 s2 s3 v -> do
      let offset' = asInt64 offset
          s1' = asInt64 s1
          s2' = asInt64 s2
          s3' = asInt64 s3
      case valueShape v of
        ShapeDim n1 (ShapeDim n2 (ShapeDim n3 _)) -> do
          let iota x = [0 .. x - 1]
              f arr' (i, j, l) =
                writeArray [IndexingFix $ offset' + i * s1' + j * s2' + l * s3'] arr'
                  =<< indexArray [IndexingFix i, IndexingFix j, IndexingFix l] v
          case foldM f arr [(i, j, l) | i <- iota n1, j <- iota n2, l <- iota n3] of
            Just arr' -> pure arr'
            Nothing ->
              bad mempty mempty $
                "Index out of bounds: " <> prettyText [((n1, s1'), (n2, s2'), (n3, s3'))]
        s -> error $ "flat_update_3d: invalid arg shape: " ++ show s
    --
    def "flat_index_4d" = Just . fun10 $ \arr offset n1 s1 n2 s2 n3 s3 n4 s4 -> do
      let offset' = asInt64 offset
          n1' = asInt64 n1
          n2' = asInt64 n2
          n3' = asInt64 n3
          n4' = asInt64 n4
          s1' = asInt64 s1
          s2' = asInt64 s2
          s3' = asInt64 s3
          s4' = asInt64 s4
          shapeFromDims = foldr ShapeDim ShapeLeaf
          mk1 = fmap (toArray (shapeFromDims [n1', n2', n3', n4'])) . sequence
          mk2 = fmap (toArray $ shapeFromDims [n2', n3', n4']) . sequence
          mk3 = fmap (toArray $ shapeFromDims [n3', n4']) . sequence
          mk4 = fmap (toArray $ shapeFromDims [n4']) . sequence
          iota x = [0 .. x - 1]
          f i j l m =
            indexArray [IndexingFix $ offset' + i * s1' + j * s2' + l * s3' + m * s4'] arr

      case mk1 [mk2 [mk3 [mk4 [f i j l m | m <- iota n4'] | l <- iota n3'] | j <- iota n2'] | i <- iota n1'] of
        Just arr' -> pure arr'
        Nothing ->
          bad mempty mempty $
            "Index out of bounds: " <> prettyText [(((n1', s1'), (n2', s2')), ((n3', s3'), (n4', s4')))]
    --
    def "flat_update_4d" = Just . fun7 $ \arr offset s1 s2 s3 s4 v -> do
      let offset' = asInt64 offset
          s1' = asInt64 s1
          s2' = asInt64 s2
          s3' = asInt64 s3
          s4' = asInt64 s4
      case valueShape v of
        ShapeDim n1 (ShapeDim n2 (ShapeDim n3 (ShapeDim n4 _))) -> do
          let iota x = [0 .. x - 1]
              f arr' (i, j, l, m) =
                writeArray [IndexingFix $ offset' + i * s1' + j * s2' + l * s3' + m * s4'] arr'
                  =<< indexArray [IndexingFix i, IndexingFix j, IndexingFix l, IndexingFix m] v
          case foldM f arr [(i, j, l, m) | i <- iota n1, j <- iota n2, l <- iota n3, m <- iota n4] of
            Just arr' -> pure arr'
            Nothing ->
              bad mempty mempty $
                "Index out of bounds: " <> prettyText [(((n1, s1'), (n2, s2')), ((n3, s3'), (n4, s4')))]
        s -> error $ "flat_update_4d: invalid arg shape: " ++ show s
    --
    def "unzip" = Just $
      fun1 $ \x -> do
        let ShapeDim _ (ShapeRecord fs) = valueShape x
            Just [xs_shape, ys_shape] = areTupleFields fs
            listPair (xs, ys) =
              [toArray' xs_shape xs, toArray' ys_shape ys]

        pure $ toTuple $ listPair $ unzip $ map (fromPair . fromTuple) $ snd $ fromArray x
      where
        fromPair (Just [x, y]) = (x, y)
        fromPair _ = error "Not a pair"
    def "zip" = Just $
      fun2 $ \xs ys -> do
        let ShapeDim _ xs_rowshape = valueShape xs
            ShapeDim _ ys_rowshape = valueShape ys
        pure $
          toArray' (ShapeRecord (tupleFields [xs_rowshape, ys_rowshape])) $
            map toTuple $
              transpose [snd $ fromArray xs, snd $ fromArray ys]
    def "concat" = Just $
      fun2 $ \xs ys -> do
        let (ShapeDim _ rowshape, xs') = fromArray xs
            (_, ys') = fromArray ys
        pure $ toArray' rowshape $ xs' ++ ys'
    def "transpose" = Just $
      fun1 $ \xs -> do
        let (ShapeDim n (ShapeDim m shape), xs') = fromArray xs
        pure $
          toArray (ShapeDim m (ShapeDim n shape)) $
            map (toArray (ShapeDim n shape)) $
              -- Slight hack to work around empty dimensions.
              genericTake m $
                transpose (map (snd . fromArray) xs') ++ repeat []
    def "rotate" = Just $
      fun2 $ \i xs -> do
        let (shape, xs') = fromArray xs
        pure $
          let idx = if null xs' then 0 else rem (asInt i) (length xs')
           in if idx > 0
                then
                  let (bef, aft) = splitAt idx xs'
                   in toArray shape $ aft ++ bef
                else
                  let (bef, aft) = splitFromEnd (-idx) xs'
                   in toArray shape $ aft ++ bef
    def "flatten" = Just $
      fun1 $ \xs -> do
        let (ShapeDim n (ShapeDim m shape), xs') = fromArray xs
        pure $ toArray (ShapeDim (n * m) shape) $ concatMap (snd . fromArray) xs'
    def "unflatten" = Just $
      fun3 $ \n m xs -> do
        let (ShapeDim xs_size innershape, xs') = fromArray xs
            rowshape = ShapeDim (asInt64 m) innershape
            shape = ShapeDim (asInt64 n) rowshape
        if asInt64 n * asInt64 m /= xs_size
          then
            bad mempty mempty $
              "Cannot unflatten array of shape ["
                <> prettyText xs_size
                <> "] to array of shape ["
                <> prettyText (asInt64 n)
                <> "]["
                <> prettyText (asInt64 m)
                <> "]"
          else pure $ toArray shape $ map (toArray rowshape) $ chunk (asInt m) xs'
    def "vjp2" = Just $
      fun3 $
        \_ _ _ -> bad noLoc mempty "Interpreter does not support autodiff."
    def "jvp2" = Just $
      fun3 $
        \_ _ _ -> bad noLoc mempty "Interpreter does not support autodiff."
    def "acc" = Nothing
    def s | nameFromString s `M.member` namesToPrimTypes = Nothing
    def s = error $ "Missing intrinsic: " ++ s

    tdef s = do
      t <- nameFromString s `M.lookup` namesToPrimTypes
      pure $ T.TypeAbbr Unlifted [] $ RetType [] $ Scalar $ Prim t

    stream f arg@(ValueArray _ xs) =
      let n = ValuePrim $ SignedValue $ Int64Value $ arrayLength xs
       in apply2 noLoc mempty f n arg
    stream _ arg = error $ "Cannot stream: " <> show arg

interpretExp :: Ctx -> Exp -> F ExtOp Value
interpretExp ctx e = runEvalM (ctxImports ctx) $ eval (ctxEnv ctx) e

interpretDec :: Ctx -> Dec -> F ExtOp Ctx
interpretDec ctx d = do
  env <- runEvalM (ctxImports ctx) $ do
    env <- evalDec (ctxEnv ctx) d
    -- We need to extract any new existential sizes and add them as
    -- ordinary bindings to the context, or we will not be able to
    -- look up their values later.
    sizes <- extSizeEnv
    pure $ env <> sizes
  pure ctx {ctxEnv = env}

interpretImport :: Ctx -> (ImportName, Prog) -> F ExtOp Ctx
interpretImport ctx (fp, prog) = do
  env <- runEvalM (ctxImports ctx) $ foldM evalDec (ctxEnv ctx) $ progDecs prog
  pure ctx {ctxImports = M.insert fp env $ ctxImports ctx}

-- | Produce a context, based on the one passed in, where all of
-- the provided imports have been @open@ened in order.
ctxWithImports :: [Env] -> Ctx -> Ctx
ctxWithImports envs ctx = ctx {ctxEnv = mconcat (reverse envs) <> ctxEnv ctx}

valueType :: V.Value -> ValueType
valueType v =
  let V.ValueType shape pt = V.valueType v
   in arrayOf mempty (F.Shape (map fromIntegral shape)) (Scalar (Prim (toPrim pt)))
  where
    toPrim V.I8 = Signed Int8
    toPrim V.I16 = Signed Int16
    toPrim V.I32 = Signed Int32
    toPrim V.I64 = Signed Int64
    toPrim V.U8 = Unsigned Int8
    toPrim V.U16 = Unsigned Int16
    toPrim V.U32 = Unsigned Int32
    toPrim V.U64 = Unsigned Int64
    toPrim V.Bool = Bool
    toPrim V.F16 = FloatType Float16
    toPrim V.F32 = FloatType Float32
    toPrim V.F64 = FloatType Float64

checkEntryArgs :: VName -> [V.Value] -> StructType -> Either T.Text ()
checkEntryArgs entry args entry_t
  | args_ts == map snd param_ts =
      pure ()
  | otherwise =
      Left . docText $
        expected
          </> "Got input of types"
          </> indent 2 (stack (map pretty args_ts))
  where
    (param_ts, _) = unfoldFunType entry_t
    args_ts = map (valueStructType . valueType) args
    expected
      | null param_ts =
          "Entry point " <> dquotes (prettyName entry) <> " is not a function."
      | otherwise =
          "Entry point " <> dquotes (prettyName entry) <> " expects input of type(s)"
            </> indent 2 (stack (map pretty param_ts))

-- | Execute the named function on the given arguments; may fail
-- horribly if these are ill-typed.
interpretFunction :: Ctx -> VName -> [V.Value] -> Either T.Text (F ExtOp Value)
interpretFunction ctx fname vs = do
  ft <- case lookupVar (qualName fname) $ ctxEnv ctx of
    Just (TermValue (Just (T.BoundV _ t)) _) ->
      updateType (map valueType vs) t
    Just (TermPoly (Just (T.BoundV _ t)) _) ->
      updateType (map valueType vs) t
    _ ->
      Left $ "Unknown function `" <> nameToText (toName fname) <> "`."

  let vs' = map fromDataValue vs

  checkEntryArgs fname vs ft

  Right $
    runEvalM (ctxImports ctx) $ do
      f <- evalTermVar (ctxEnv ctx) (qualName fname) ft
      foldM (apply noLoc mempty) f vs'
  where
    updateType (vt : vts) (Scalar (Arrow als pn d pt (RetType dims rt))) = do
      checkInput vt pt
      Scalar . Arrow als pn d (valueStructType vt) . RetType dims <$> updateType vts rt
    updateType _ t =
      Right t

    -- FIXME: we don't check array sizes.
    checkInput :: ValueType -> StructType -> Either T.Text ()
    checkInput (Scalar (Prim vt)) (Scalar (Prim pt))
      | vt /= pt = badPrim vt pt
    checkInput (Array _ _ _ (Prim vt)) (Array _ _ _ (Prim pt))
      | vt /= pt = badPrim vt pt
    checkInput _ _ =
      Right ()

    badPrim vt pt =
      Left . docText $
        "Invalid argument type."
          </> "Expected:"
          <+> align (pretty pt)
          </> "Got:     "
          <+> align (pretty vt)
