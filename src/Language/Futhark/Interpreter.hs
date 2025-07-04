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
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor (($>), (<&>))
import Data.List
  ( find,
    foldl',
    genericLength,
    genericTake,
    transpose,
  )
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid hiding (Sum)
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Data qualified as V
import Futhark.Util (chunk)
import Futhark.Util.Loc
import Futhark.Util.Pretty hiding (apply)
import Language.Futhark hiding (Shape, matchDims)
import Language.Futhark qualified as F
import Language.Futhark.Interpreter.AD qualified as AD
import Language.Futhark.Interpreter.Values hiding (Value)
import Language.Futhark.Interpreter.Values qualified
import Language.Futhark.Primitive (floatValue, intValue)
import Language.Futhark.Primitive qualified as P
import Language.Futhark.Semantic qualified as T
import Language.Futhark.TypeChecker.Types (Subst (..), applySubst)
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

type Exts = M.Map VName Value

-- | The monad in which evaluation takes place.
newtype EvalM a
  = EvalM
      ( ReaderT
          (Stack, M.Map ImportName Env)
          (StateT (Exts, AD.Counter) (F ExtOp))
          a
      )
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadFree ExtOp,
      MonadReader (Stack, M.Map ImportName Env),
      MonadState (Exts, AD.Counter)
    )

runEvalM :: M.Map ImportName Env -> EvalM a -> F ExtOp a
runEvalM imports (EvalM m) = evalStateT (runReaderT m (mempty, imports)) (mempty, AD.Counter 0)

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

-- | Instead of tracking the actual depth of AD, we just use the size
-- of the stack as a proxy.
adDepth :: EvalM AD.Depth
adDepth = AD.Depth . length <$> stacktrace

lookupImport :: ImportName -> EvalM (Maybe Env)
lookupImport f = asks $ M.lookup f . snd

putExtSize :: VName -> Value -> EvalM ()
putExtSize v x = modify $ first $ M.insert v x

getExts :: EvalM Exts
getExts = gets fst

putCounter :: AD.Counter -> EvalM ()
putCounter i = modify $ second $ const i

getCounter :: EvalM AD.Counter
getCounter = gets snd

-- | Disregard any existential sizes computed during this action.
-- This is used so that existentials computed during one iteration of
-- a loop or a function call are not remembered the next time around.
localExts :: EvalM a -> EvalM a
localExts m = do
  e <- getExts
  x <- m
  i <- getCounter
  put (e, i)
  pure x

extEnv :: EvalM Env
extEnv = valEnv . M.map (Nothing,) <$> getExts

valueStructType :: ValueType -> StructType
valueStructType = first $ flip sizeFromInteger mempty . toInteger

-- | An expression along with an environment in which to evaluate that
-- expression. Used to represent non-interpreted size expressions,
-- which may still be in reference to some environment.
data SizeClosure = SizeClosure Env Size
  deriving (Show)

instance Pretty SizeClosure where
  pretty (SizeClosure _ e) = pretty e

instance Pretty (F.Shape SizeClosure) where
  pretty = mconcat . map (brackets . pretty) . shapeDims

-- | A type where the sizes are unevaluated expressions.
type EvalType = TypeBase SizeClosure NoUniqueness

structToEval :: Env -> StructType -> EvalType
structToEval env = first (SizeClosure env)

evalToStruct :: EvalType -> StructType
evalToStruct = first (\(SizeClosure _ e) -> e)

resolveTypeParams ::
  [VName] ->
  StructType ->
  EvalType ->
  ([(VName, ([VName], EvalType))], [(VName, SizeClosure)])
resolveTypeParams names orig_t1 orig_t2 =
  execState (match mempty orig_t1 orig_t2) mempty
  where
    addType v t = modify $ first $ L.insertBy (comparing fst) (v, t)
    addDim v e = modify $ second $ L.insertBy (comparing fst) (v, e)

    match bound (Scalar (TypeVar _ tn _)) t
      | qualLeaf tn `elem` names = addType (qualLeaf tn) (bound, t)
    match bound (Scalar (Record poly_fields)) (Scalar (Record fields)) =
      sequence_ . M.elems $
        M.intersectionWith (match bound) poly_fields fields
    match bound (Scalar (Sum poly_fields)) (Scalar (Sum fields)) =
      sequence_ . mconcat . M.elems $
        M.intersectionWith (zipWith $ match bound) poly_fields fields
    match
      bound
      (Scalar (Arrow _ p1 _ poly_t1 (RetType dims1 poly_t2)))
      (Scalar (Arrow _ p2 _ t1 (RetType dims2 t2))) = do
        let bound' = mapMaybe paramName [p1, p2] <> dims1 <> dims2 <> bound
        match bound' poly_t1 t1
        match bound' (toStruct poly_t2) (toStruct t2)
    match bound poly_t t
      | d1 : _ <- shapeDims (arrayShape poly_t),
        d2 : _ <- shapeDims (arrayShape t) = do
          matchDims bound d1 d2
          match bound (stripArray 1 poly_t) (stripArray 1 t)
    match bound t1 t2
      | Just t1' <- isAccType t1,
        Just t2' <- isAccType t2 =
          match bound t1' t2'
    match _ _ _ = pure mempty

    matchDims bound e1 (SizeClosure env e2)
      | e1 == anySize || e2 == anySize = pure mempty
      | otherwise = matchExps bound env e1 e2

    matchExps bound env (Var (QualName _ d1) _ _) e
      | d1 `elem` names,
        not $ any problematic $ fvVars $ freeInExp e =
          addDim d1 (SizeClosure env e)
      where
        problematic v = v `elem` bound || v `elem` names
    matchExps bound env e1 e2
      | Just es <- similarExps e1 e2 =
          mapM_ (uncurry $ matchExps bound env) es
    matchExps _ _ _ _ = pure mempty

evalWithExts :: Env -> Exp -> EvalM Value
evalWithExts env e = do
  size_env <- extEnv
  eval (size_env <> env) e

evalResolved ::
  ([(VName, ([VName], EvalType))], [(VName, SizeClosure)]) ->
  EvalM Env
evalResolved (ts, ds) = do
  ts' <- mapM (traverse $ \(bound, t) -> first onDim <$> evalType (S.fromList bound) t) ts
  ds' <- mapM (traverse $ \(SizeClosure env e) -> asInt64 <$> evalWithExts env e) ds
  pure $ typeEnv (M.fromList ts') <> i64Env (M.fromList ds')
  where
    onDim (Left x) = sizeFromInteger (toInteger x) mempty
    onDim (Right (SizeClosure _ e)) = e -- FIXME

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

    matchDims (Var (QualName _ d1) _ _) d2
      | d1 `elem` names = M.singleton d1 d2
    matchDims _ _ = mempty

checkShape :: Shape Int64 -> ValueShape -> Maybe ValueShape
checkShape (ShapeDim d1 shape1) (ShapeDim d2 shape2) = do
  guard $ d1 == d2
  ShapeDim d2 <$> checkShape shape1 shape2
checkShape (ShapeDim d1 shape1) ShapeLeaf =
  -- This case is for handling polymorphism, when a function doesn't
  -- know that the array it produced actually has more dimensions.
  ShapeDim d1 <$> checkShape shape1 ShapeLeaf
checkShape (ShapeRecord shapes1) (ShapeRecord shapes2) =
  ShapeRecord <$> sequence (M.intersectionWith checkShape shapes1 shapes2)
checkShape (ShapeRecord shapes1) ShapeLeaf =
  Just $ ShapeRecord shapes1
checkShape (ShapeSum shapes1) (ShapeSum shapes2) =
  ShapeSum <$> sequence (M.intersectionWith (zipWithM checkShape) shapes1 shapes2)
checkShape (ShapeSum shapes1) ShapeLeaf =
  Just $ ShapeSum shapes1
checkShape _ shape2 =
  Just shape2

type Value = Language.Futhark.Interpreter.Values.Value EvalM

asInteger :: Value -> Integer
asInteger (ValuePrim (SignedValue v)) = P.valueIntegral v
asInteger (ValuePrim (UnsignedValue v)) =
  toInteger (P.valueIntegral (P.doZExt v Int64) :: Word64)
asInteger (ValueAD _ v)
  | P.IntValue v' <- AD.varPrimal v =
      P.valueIntegral v'
asInteger v = error $ "Unexpectedly not an integer: " <> show v

asInt :: Value -> Int
asInt = fromIntegral . asInteger

asSigned :: Value -> IntValue
asSigned (ValuePrim (SignedValue v)) = v
asSigned (ValueAD _ v)
  | P.IntValue v' <- AD.varPrimal v = v'
asSigned v = error $ "Unexpectedly not a signed integer: " <> show v

asInt64 :: Value -> Int64
asInt64 = fromIntegral . asInteger

asBool :: Value -> Bool
asBool (ValuePrim (BoolValue x)) = x
asBool (ValueAD _ v)
  | P.BoolValue v' <- AD.varPrimal v = v'
asBool v = error $ "Unexpectedly not a boolean: " <> show v

lookupInEnv ::
  (Env -> M.Map VName x) ->
  QualName VName ->
  Env ->
  Maybe x
lookupInEnv onEnv qv env = f env $ qualQuals qv
  where
    f m (q : qs) =
      case (M.lookup (qualLeaf qv) $ onEnv m, M.lookup q $ envTerm m) of
        (Just x, _) -> Just x
        (Nothing, Just (TermModule (Module mod))) -> f mod qs
        _ -> Nothing
    f m [] = M.lookup (qualLeaf qv) $ onEnv m

lookupVar :: QualName VName -> Env -> Maybe TermBinding
lookupVar = lookupInEnv envTerm

lookupType :: QualName VName -> Env -> Maybe TypeBinding
lookupType = lookupInEnv envType

-- | A TermValue with a 'Nothing' type annotation is an intrinsic or
-- an existential.
data TermBinding
  = TermValue (Maybe T.BoundV) Value
  | -- | A polymorphic value that must be instantiated.  The
    --  'StructType' provided is un-evaluated, but parts of it can be
    --  evaluated using the provided 'Eval' function.
    TermPoly (Maybe T.BoundV) (EvalType -> EvalM Value)
  | TermModule Module

instance Show TermBinding where
  show (TermValue bv v) = unwords ["TermValue", show bv, show v]
  show (TermPoly bv _) = unwords ["TermPoly", show bv]
  show (TermModule m) = unwords ["TermModule", show m]

data TypeBinding = TypeBinding Env [TypeParam] StructRetType
  deriving (Show)

data Module
  = Module Env
  | ModuleFun (Module -> EvalM Module)

instance Show Module where
  show (Module env) = "(" <> unwords ["Module", show env] <> ")"
  show (ModuleFun _) = "(ModuleFun _)"

-- | The actual type- and value environment.
data Env = Env
  { envTerm :: M.Map VName TermBinding,
    envType :: M.Map VName TypeBinding
  }
  deriving (Show)

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
    tbind = TypeBinding mempty [] . RetType []

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

matchPat :: Env -> Pat (TypeBase Size u) -> Value -> EvalM Env
matchPat env p v = do
  m <- runMaybeT $ patternMatch env p v
  case m of
    Nothing -> error $ "matchPat: missing case for " <> prettyString (toStruct <$> p) ++ " and " <> show v
    Just env' -> pure env'

patternMatch :: Env -> Pat (TypeBase Size u) -> Value -> MaybeT EvalM Env
patternMatch env (PatAttr _ p _) val =
  patternMatch env p val
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
    M.intersectionWith (,) (M.fromList $ map (first unLoc) ps) vs
patternMatch env (PatParens p _) v = patternMatch env p v
patternMatch env (PatAscription p _ _) v =
  patternMatch env p v
patternMatch env (PatLit l t _) v = do
  l' <- case l of
    PatLitInt x -> lift $ eval env $ IntLit x (toStruct <$> t) mempty
    PatLitFloat x -> lift $ eval env $ FloatLit x (toStruct <$> t) mempty
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
  (Monad m) =>
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
            <> prettyText (arrayValueShape arr)
            <> "."
  maybe oob pure $ indexArray is arr

-- | Expand type based on information that was not available at
-- type-checking time (the structure of abstract types).
expandType :: (Pretty u) => Env -> TypeBase Size u -> TypeBase SizeClosure u
expandType _ (Scalar (Prim pt)) = Scalar $ Prim pt
expandType env (Scalar (Record fs)) = Scalar $ Record $ fmap (expandType env) fs
expandType env (Scalar (Arrow u p d t1 (RetType dims t2))) =
  Scalar $ Arrow u p d (expandType env t1) (RetType dims (expandType env t2))
expandType env t@(Array u shape _) =
  let et = stripArray (shapeRank shape) t
      et' = expandType env et
      shape' = fmap (SizeClosure env) shape
   in second (const u) (arrayOf shape' $ toStruct et')
expandType env (Scalar (TypeVar u tn args)) =
  case lookupType tn env of
    Just (TypeBinding tn_env ps (RetType ext t')) ->
      let (substs, types) = mconcat $ zipWith matchPtoA ps args
          onDim (SizeClosure dim_env dim)
            | any (`elem` ext) $ fvVars $ freeInExp dim =
                -- The case can occur when a type with existential
                -- size has been hidden by a module ascription, e.g.
                -- tests/modules/sizeparams4.fut.
                SizeClosure mempty anySize
            | otherwise =
                SizeClosure (env <> dim_env) $
                  applySubst (`M.lookup` substs) dim
       in bimap onDim (const u) $ expandType (Env mempty types <> tn_env) t'
    Nothing ->
      -- This case only happens for built-in abstract types,
      -- e.g. accumulators.
      Scalar (TypeVar u tn $ map expandArg args)
  where
    matchPtoA (TypeParamDim p _) (TypeArgDim e) =
      (M.singleton p $ ExpSubst e, mempty)
    matchPtoA (TypeParamType _ p _) (TypeArgType t') =
      let t'' = evalToStruct $ expandType env t' -- FIXME, we are throwing away the closure here.
       in (mempty, M.singleton p (TypeBinding mempty [] $ RetType [] t''))
    matchPtoA _ _ = mempty
    expandArg (TypeArgDim s) = TypeArgDim $ SizeClosure env s
    expandArg (TypeArgType t) = TypeArgType $ expandType env t
expandType env (Scalar (Sum cs)) = Scalar $ Sum $ (fmap . fmap) (expandType env) cs

-- | Evaluate all possible sizes, except those that contain free
-- variables in the set of names.
evalType :: S.Set VName -> EvalType -> EvalM (TypeBase (Either Int64 SizeClosure) NoUniqueness)
evalType outer_bound t = do
  let evalDim bound _ (SizeClosure env e)
        | canBeEvaluated bound e =
            Left . asInt64 <$> evalWithExts env e
      evalDim _ _ e = pure $ Right e
  traverseDims evalDim t
  where
    canBeEvaluated bound e =
      let free = fvVars $ freeInExp e
       in not $ any (`S.member` bound) free || any (`S.member` outer_bound) free

-- | Evaluate all sizes, and it better work. This implies it must not be a
-- size-dependent function type, or one that has existentials.
evalTypeFully :: EvalType -> EvalM ValueType
evalTypeFully t = do
  let evalDim (SizeClosure env e) = asInt64 <$> evalWithExts env e
  bitraverse evalDim pure t

evalTermVar :: Env -> QualName VName -> StructType -> EvalM Value
evalTermVar env qv t =
  case lookupVar qv env of
    Just (TermPoly _ v) -> v $ expandType env t
    Just (TermValue _ v) -> pure v
    x -> do
      ss <- map (locText . srclocOf) <$> stacktrace
      error $
        prettyString qv
          <> " is not bound to a value.\n"
          <> T.unpack (prettyStacktrace 0 ss)
          <> "Bound to\n"
          <> show x

typeValueShape :: Env -> StructType -> EvalM ValueShape
typeValueShape env t = typeShape <$> evalTypeFully (expandType env t)

-- Sometimes type instantiation is not quite enough - then we connect
-- up the missing sizes here.  In particular used for eta-expanded
-- entry points.
linkMissingSizes :: [VName] -> Pat (TypeBase Size u) -> Value -> Env -> Env
linkMissingSizes [] _ _ env = env
linkMissingSizes missing_sizes p v env =
  env <> i64Env (resolveExistentials missing_sizes p_t (valueShape v))
  where
    p_t = evalToStruct $ expandType env $ patternStructType p

evalFunction :: Env -> [VName] -> [Pat ParamType] -> Exp -> ResType -> EvalM Value
-- We treat zero-parameter lambdas as simply an expression to
-- evaluate immediately.  Note that this is *not* the same as a lambda
-- that takes an empty tuple '()' as argument!  Zero-parameter lambdas
-- can never occur in a well-formed Futhark program, but they are
-- convenient in the interpreter.
evalFunction env missing_sizes [] body rettype =
  -- Eta-expand the rest to make any sizes visible.
  etaExpand [] env rettype
  where
    etaExpand vs env' (Scalar (Arrow _ _ _ p_t (RetType _ rt))) = do
      pure . ValueFun $ \v -> do
        let p = Wildcard (Info p_t) noLoc
        env'' <- linkMissingSizes missing_sizes p v <$> matchPat env' p v
        etaExpand (v : vs) env'' rt
    etaExpand vs env' _ = do
      f <- eval env' body
      foldM (apply noLoc mempty) f $ reverse vs
evalFunction env missing_sizes (p : ps) body rettype =
  pure . ValueFun $ \v -> do
    env' <- linkMissingSizes missing_sizes p v <$> matchPat env p v
    evalFunction env' missing_sizes ps body rettype

evalFunctionBinding ::
  Env ->
  [TypeParam] ->
  [Pat ParamType] ->
  ResRetType ->
  Exp ->
  EvalM TermBinding
evalFunctionBinding env tparams ps ret fbody = do
  let ftype = funType ps ret
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
      let resolved = resolveTypeParams (map typeParamName tparams) ftype ftype'
      tparam_env <- evalResolved resolved
      let env' = tparam_env <> env
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
    Just ext' -> putExtSize ext' v
    _ -> pure ()
  pure v

returned :: Env -> TypeBase Size u -> [VName] -> Value -> EvalM Value
returned _ _ [] v = pure v
returned env ret retext v = do
  mapM_ (uncurry putExtSize . second (ValuePrim . SignedValue . Int64Value))
    . M.toList
    $ resolveExistentials retext (evalToStruct $ expandType env $ toStruct ret)
    $ valueShape v
  pure v

evalAppExp :: Env -> AppExp -> EvalM Value
evalAppExp env (Range start maybe_second end loc) = do
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
evalAppExp env (LetPat sizes p e body _) = do
  v <- eval env e
  env' <- matchPat env p v
  let p_t = evalToStruct $ expandType env $ patternStructType p
      v_s = valueShape v
      env'' = env' <> i64Env (resolveExistentials (map sizeName sizes) p_t v_s)
  eval env'' body
evalAppExp env (LetFun f (tparams, ps, _, Info ret, fbody) body _) = do
  binding <- evalFunctionBinding env tparams ps ret fbody
  eval (env {envTerm = M.insert f binding $ envTerm env}) body
evalAppExp env (BinOp (op, _) op_t (x, Info xext) (y, Info yext) loc)
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
      x' <- evalArg env x xext
      y' <- evalArg env y yext
      op' <- eval env $ Var op op_t loc
      apply2 loc env op' x' y'
evalAppExp env (If cond e1 e2 _) = do
  cond' <- asBool <$> eval env cond
  if cond' then eval env e1 else eval env e2
evalAppExp env (Apply f args loc) = do
  -- It is important that 'arguments' are evaluated in reverse order
  -- in order to bring any sizes into scope that may be used in the
  -- type of the functions.
  args' <- reverse <$> mapM evalArg' (reverse $ NE.toList args)
  f' <- eval env f
  foldM (apply loc env) f' args'
  where
    evalArg' (Info ext, x) = evalArg env x ext
evalAppExp env (Index e is loc) = do
  is' <- mapM (evalDimIndex env) is
  arr <- eval env e
  evalIndex loc env is' arr
evalAppExp env (LetWith dest src is v body loc) = do
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
evalAppExp env (Loop sparams pat loopinit form body _) = do
  init_v <- eval env $ loopInitExp loopinit
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
            resolveExistentials sparams (patternStructType pat) (valueShape v)
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
evalAppExp env (Match e cs _) = do
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
    evalField (RecordFieldExplicit (L _ k) e _) = do
      v <- eval env e
      pure (k, v)
    evalField (RecordFieldImplicit (L _ k) t loc) = do
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
eval _ (ArrayVal vs _ _) =
  -- Probably will not ever be used.
  pure $ toArray' ShapeLeaf $ map ValuePrim vs
eval env (AppExp e (Info (AppRes t retext))) = do
  v <- evalAppExp env e
  returned env (toStruct t) retext v
eval env (Var qv (Info t) _) = evalTermVar env qv (toStruct t)
eval env (Ascript e _ _) = eval env e
eval env (Coerce e te (Info t) loc) = do
  v <- eval env e
  t' <- evalTypeFully $ expandType env t
  case checkShape (typeShape t') (valueShape v) of
    Just _ -> pure v
    Nothing ->
      bad loc env . docText $
        "Value"
          <+> dquotes (prettyValue v)
          <+> "of shape"
          <+> dquotes (pretty (valueShape v))
          <+> "cannot match shape of type"
          <+> dquotes (pretty te)
          <+> parens (dquotes (pretty t'))
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
eval env (Negate e loc) = do
  ev <- eval env e
  apply loc env intrinsicsNeg ev
eval env (Not e loc) =
  apply loc env intrinsicsNot =<< eval env e
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
eval env (Lambda ps body _ (Info (RetType _ rt)) _) =
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
    replaceM f m = M.fromList $ do
      (k, v) <- M.toList m
      k' <- replace k
      pure (k', f v)
    onEnv (Env terms types) =
      Env (replaceM onTerm terms) (replaceM onType types)
    onModule (Module env) =
      Module $ onEnv env
    onModule (ModuleFun f) =
      ModuleFun $ \m -> onModule <$> f (substituteInModule substs m)
    onTerm (TermValue t v) = TermValue t v
    onTerm (TermPoly t v) = TermPoly t v
    onTerm (TermModule m) = TermModule $ onModule m
    onType (TypeBinding env ps t) = TypeBinding (onEnv env) ps t

evalModuleVar :: Env -> QualName VName -> EvalM Module
evalModuleVar env qv =
  case lookupVar qv env of
    Just (TermModule m) -> pure m
    _ -> error $ prettyString qv <> " is not bound to a module."

-- We also return a new Env here, because we want the definitions
-- inside any constructed modules to also be in scope at the top
-- level. This is because types may contain un-qualified references to
-- definitions in modules, and sometimes those definitions may not
-- actually *have* any qualified name!  See tests/modules/sizes7.fut.
-- This occurs solely because of evalType.
evalModExp :: Env -> ModExp -> EvalM (Env, Module)
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
    Just m -> pure (mempty, Module m)
evalModExp env (ModDecs ds _) = do
  (_, built_env) <- evalDecs env ds
  pure (built_env, Module built_env)
evalModExp env (ModVar qv _) =
  (mempty,) <$> evalModuleVar env qv
evalModExp env (ModAscript me _ (Info substs) _) =
  bimap substituteInEnv (substituteInModule substs) <$> evalModExp env me
  where
    substituteInEnv env' =
      let Module env'' = substituteInModule substs (Module env') in env''
evalModExp env (ModParens me _) =
  evalModExp env me
evalModExp env (ModLambda p ret e loc) =
  pure
    ( mempty,
      ModuleFun $ \am -> do
        let env' = env {envTerm = M.insert (modParamName p) (TermModule am) $ envTerm env}
        fmap snd . evalModExp env' $ case ret of
          Nothing -> e
          Just (se, rsubsts) -> ModAscript e se rsubsts loc
    )
evalModExp env (ModApply f e (Info psubst) (Info rsubst) _) = do
  (f_env, f') <- evalModExp env f
  (e_env, e') <- evalModExp env e
  case f' of
    ModuleFun f'' -> do
      res_mod <- substituteInModule rsubst <$> f'' (substituteInModule psubst e')
      let res_env = case res_mod of
            Module x -> x
            _ -> mempty
      -- The following environment handles the case where rsubst refers to names
      -- that are not actually defined in the module itself, but merely
      -- inherited from an outer environment (see #2273).
      let env_substs = (`Env` mempty) $ M.fromList $ do
            (to, from) <- M.toList rsubst
            x <- maybeToList $ M.lookup from $ envTerm env
            pure (to, x)
      pure (f_env <> e_env <> res_env <> env_substs, res_mod)
    _ -> error "Expected ModuleFun."

evalDec :: Env -> Dec -> EvalM Env
evalDec env (ValDec (ValBind _ v _ (Info ret) tparams ps fbody _ _ _)) = localExts $ do
  binding <- evalFunctionBinding env tparams ps ret fbody
  sizes <- extEnv
  pure $ mempty {envTerm = M.singleton v binding} <> sizes
evalDec env (OpenDec me _) = do
  (me_env, me') <- evalModExp env me
  case me' of
    Module me'' -> pure $ me'' <> me_env
    _ -> error "Expected Module"
evalDec env (ImportDec name name' loc) =
  evalDec env $ LocalDec (OpenDec (ModImport name name' loc) loc) loc
evalDec env (LocalDec d _) = evalDec env d
evalDec _env ModTypeDec {} = pure mempty
evalDec env (TypeDec (TypeBind v _ ps _ (Info (RetType dims t)) _ _)) = do
  let abbr = TypeBinding env ps $ RetType dims t
  pure mempty {envType = M.singleton v abbr}
evalDec env (ModDec (ModBind v ps ret body _ loc)) = do
  (mod_env, mod) <- evalModExp env $ wrapInLambda ps
  pure $ modEnv (M.singleton v mod) <> mod_env
  where
    wrapInLambda [] = case ret of
      Just (se, substs) -> ModAscript body se substs loc
      Nothing -> body
    wrapInLambda [p] = ModLambda p ret body loc
    wrapInLambda (p : ps') = ModLambda p Nothing (wrapInLambda ps') loc

evalDecs :: Env -> [Dec] -> EvalM (Env, Env)
evalDecs env = foldM evalDec' (env, mempty)
  where
    evalDec' (env', built_env) dec = do
      dec_env <- evalDec env' dec
      pure (dec_env <> env', dec_env <> built_env)

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
            (VName (nameFromText "intrinsics") 0)
            (TermModule (Module $ Env terms types))
            terms
        )
        types
    )
    mempty
  where
    terms = M.mapMaybeWithKey (const . def . baseText) intrinsics
    types = M.mapMaybeWithKey (const . tdef . baseName) intrinsics

    sintOp f =
      [ (getS, putS, P.doBinOp (f Int8), adBinOp $ AD.OpBin (f Int8)),
        (getS, putS, P.doBinOp (f Int16), adBinOp $ AD.OpBin (f Int16)),
        (getS, putS, P.doBinOp (f Int32), adBinOp $ AD.OpBin (f Int32)),
        (getS, putS, P.doBinOp (f Int64), adBinOp $ AD.OpBin (f Int64))
      ]
    uintOp f =
      [ (getU, putU, P.doBinOp (f Int8), adBinOp $ AD.OpBin (f Int8)),
        (getU, putU, P.doBinOp (f Int16), adBinOp $ AD.OpBin (f Int16)),
        (getU, putU, P.doBinOp (f Int32), adBinOp $ AD.OpBin (f Int32)),
        (getU, putU, P.doBinOp (f Int64), adBinOp $ AD.OpBin (f Int64))
      ]
    intOp f = sintOp f ++ uintOp f
    floatOp f =
      [ (getF, putF, P.doBinOp (f Float16), adBinOp $ AD.OpBin (f Float16)),
        (getF, putF, P.doBinOp (f Float32), adBinOp $ AD.OpBin (f Float32)),
        (getF, putF, P.doBinOp (f Float64), adBinOp $ AD.OpBin (f Float64))
      ]
    arithOp f g = Just $ bopDef $ intOp f ++ floatOp g

    flipCmps = map (\(f, g, h, o) -> (f, g, flip h, flip o))
    sintCmp f =
      [ (getS, Just . BoolValue, P.doCmpOp (f Int8), adBinOp $ AD.OpCmp (f Int8)),
        (getS, Just . BoolValue, P.doCmpOp (f Int16), adBinOp $ AD.OpCmp (f Int16)),
        (getS, Just . BoolValue, P.doCmpOp (f Int32), adBinOp $ AD.OpCmp (f Int32)),
        (getS, Just . BoolValue, P.doCmpOp (f Int64), adBinOp $ AD.OpCmp (f Int64))
      ]
    uintCmp f =
      [ (getU, Just . BoolValue, P.doCmpOp (f Int8), adBinOp $ AD.OpCmp (f Int8)),
        (getU, Just . BoolValue, P.doCmpOp (f Int16), adBinOp $ AD.OpCmp (f Int16)),
        (getU, Just . BoolValue, P.doCmpOp (f Int32), adBinOp $ AD.OpCmp (f Int32)),
        (getU, Just . BoolValue, P.doCmpOp (f Int64), adBinOp $ AD.OpCmp (f Int64))
      ]
    floatCmp f =
      [ (getF, Just . BoolValue, P.doCmpOp (f Float16), adBinOp $ AD.OpCmp (f Float16)),
        (getF, Just . BoolValue, P.doCmpOp (f Float32), adBinOp $ AD.OpCmp (f Float32)),
        (getF, Just . BoolValue, P.doCmpOp (f Float64), adBinOp $ AD.OpCmp (f Float64))
      ]
    boolCmp f = [(getB, Just . BoolValue, P.doCmpOp f, adBinOp $ AD.OpCmp f)]

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

    getAD (ValuePrim v) = AD.Constant <$> getV v
    getAD (ValueAD d v) = Just $ AD.Variable d v
    getAD _ = Nothing
    putAD (AD.Variable d s) = ValueAD d s
    putAD (AD.Constant v) = ValuePrim $ putV v

    adToPrim v = putV $ AD.primitive v

    adBinOp op x y i =
      either (const Nothing) Just $ AD.doOp op [x, y] i
    adUnOp op x i = either (const Nothing) Just $ AD.doOp op [x] i

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

    bopDef fs = fun2 $ \x y -> do
      i <- getCounter
      case (x, y) of
        (ValuePrim x', ValuePrim y')
          | Just z <- msum $ map (`bopDef'` (x', y')) fs -> do
              breakOnNaN [x', y'] z
              pure $ ValuePrim z
        _
          | Just x' <- getAD x,
            Just y' <- getAD y,
            Just (z, i') <- msum $ map (`bopDefAD` (x', y', i)) fs -> do
              putCounter i'
              breakOnNaN [adToPrim x', adToPrim y'] $ adToPrim z
              pure $ putAD z
        _ ->
          bad noLoc mempty . docText $
            "Cannot apply operator to arguments"
              <+> dquotes (prettyValue x)
              <+> "and"
              <+> dquotes (prettyValue y)
              <> "."
      where
        bopDef' (valf, retf, op, _) (x, y) = do
          x' <- valf x
          y' <- valf y
          retf =<< op x' y'
        bopDefAD (_, _, _, dop) (x, y, i) = dop x y i

    unopDef fs = fun1 $ \x -> do
      i <- getCounter
      case x of
        (ValuePrim x')
          | Just r <- msum $ map (`unopDef'` x') fs -> do
              breakOnNaN [x'] r
              pure $ ValuePrim r
        _
          | Just x' <- getAD x,
            Just (r, i') <- msum $ map (`unopDefAD'` (x', i)) fs -> do
              putCounter i'
              breakOnNaN [adToPrim x'] $ adToPrim r
              pure $ putAD r
        _ ->
          bad noLoc mempty . docText $
            "Cannot apply function to argument"
              <+> dquotes (prettyValue x)
              <> "."
      where
        unopDef' (valf, retf, op, _) x = do
          x' <- valf x
          retf =<< op x'
        unopDefAD' (_, _, _, dop) (x, i) = dop x i

    tbopDef op f = fun1 $ \v -> do
      i <- getCounter
      case fromTuple v of
        Just [ValuePrim x, ValuePrim y]
          | Just x' <- getV x,
            Just y' <- getV y,
            Just z <- putV <$> f x' y' -> do
              breakOnNaN [x, y] z
              pure $ ValuePrim z
        Just [x, y]
          | Just x' <- getAD x,
            Just y' <- getAD y,
            Right (z, i') <- AD.doOp op [x', y'] i -> do
              putCounter i'
              breakOnNaN [adToPrim x', adToPrim y'] $ adToPrim z
              pure $ putAD z
        _ ->
          bad noLoc mempty . docText $
            "Cannot apply operator to argument"
              <+> dquotes (prettyValue v)
              <> "."

    def :: T.Text -> Maybe TermBinding
    def "!" =
      Just $
        unopDef
          [ (getS, putS, P.doUnOp $ P.Complement Int8, adUnOp $ AD.OpUn $ P.Complement Int8),
            (getS, putS, P.doUnOp $ P.Complement Int16, adUnOp $ AD.OpUn $ P.Complement Int16),
            (getS, putS, P.doUnOp $ P.Complement Int32, adUnOp $ AD.OpUn $ P.Complement Int32),
            (getS, putS, P.doUnOp $ P.Complement Int64, adUnOp $ AD.OpUn $ P.Complement Int64),
            (getU, putU, P.doUnOp $ P.Complement Int8, adUnOp $ AD.OpUn $ P.Complement Int8),
            (getU, putU, P.doUnOp $ P.Complement Int16, adUnOp $ AD.OpUn $ P.Complement Int16),
            (getU, putU, P.doUnOp $ P.Complement Int32, adUnOp $ AD.OpUn $ P.Complement Int32),
            (getU, putU, P.doUnOp $ P.Complement Int64, adUnOp $ AD.OpUn $ P.Complement Int64),
            (getB, putB, P.doUnOp $ P.Neg P.Bool, adUnOp $ AD.OpUn $ P.Neg P.Bool)
          ]
    def "neg" =
      Just $
        unopDef
          [ (getS, putS, P.doUnOp $ P.Neg $ P.IntType Int8, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int8),
            (getS, putS, P.doUnOp $ P.Neg $ P.IntType Int16, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int16),
            (getS, putS, P.doUnOp $ P.Neg $ P.IntType Int32, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int32),
            (getS, putS, P.doUnOp $ P.Neg $ P.IntType Int64, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int64),
            (getU, putU, P.doUnOp $ P.Neg $ P.IntType Int8, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int8),
            (getU, putU, P.doUnOp $ P.Neg $ P.IntType Int16, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int16),
            (getU, putU, P.doUnOp $ P.Neg $ P.IntType Int32, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int32),
            (getU, putU, P.doUnOp $ P.Neg $ P.IntType Int64, adUnOp $ AD.OpUn $ P.Neg $ P.IntType Int64),
            (getF, putF, P.doUnOp $ P.Neg $ P.FloatType Float16, adUnOp $ AD.OpUn $ P.Neg $ P.FloatType Float16),
            (getF, putF, P.doUnOp $ P.Neg $ P.FloatType Float32, adUnOp $ AD.OpUn $ P.Neg $ P.FloatType Float32),
            (getF, putF, P.doUnOp $ P.Neg $ P.FloatType Float64, adUnOp $ AD.OpUn $ P.Neg $ P.FloatType Float64),
            (getB, putB, P.doUnOp $ P.Neg P.Bool, adUnOp $ AD.OpUn $ P.Neg P.Bool)
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
      | Just bop <- find ((s ==) . prettyText) P.allBinOps =
          Just $ tbopDef (AD.OpBin bop) $ P.doBinOp bop
      | Just unop <- find ((s ==) . prettyText) P.allCmpOps =
          Just $ tbopDef (AD.OpCmp unop) $ \x y -> P.BoolValue <$> P.doCmpOp unop x y
      | Just cop <- find ((s ==) . prettyText) P.allConvOps =
          Just $ unopDef [(getV, Just . putV, P.doConvOp cop, adUnOp $ AD.OpConv cop)]
      | Just unop <- find ((s ==) . prettyText) P.allUnOps =
          Just $ unopDef [(getV, Just . putV, P.doUnOp unop, adUnOp $ AD.OpUn unop)]
      | Just (pts, _, f) <- M.lookup s P.primFuns =
          case length pts of
            1 -> Just $ unopDef [(getV, Just . putV, f . pure, adUnOp $ AD.OpFn s)]
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
      | "sign_" `T.isPrefixOf` s =
          Just $
            fun1 $ \x ->
              case x of
                (ValuePrim (UnsignedValue x')) ->
                  pure $ ValuePrim $ SignedValue x'
                ValueAD {} -> pure x -- FIXME: these do not carry signs.
                _ -> error $ "Cannot sign: " <> show x
      | "unsign_" `T.isPrefixOf` s =
          Just $
            fun1 $ \x ->
              case x of
                (ValuePrim (SignedValue x')) ->
                  pure $ ValuePrim $ UnsignedValue x'
                ValueAD {} -> pure x -- FIXME: these do not carry signs.
                _ -> error $ "Cannot unsign: " <> show x
    def "map" = Just $
      TermPoly Nothing $ \t -> do
        t' <- evalTypeFully t
        pure $ ValueFun $ \f -> pure . ValueFun $ \xs ->
          case unfoldFunType t' of
            ([_, _], ret_t)
              | rowshape <- typeShape $ stripArray 1 ret_t ->
                  toArray' rowshape <$> mapM (apply noLoc mempty f) (snd $ fromArray xs)
            _ ->
              error $
                "Invalid arguments to map intrinsic:\n"
                  ++ unlines [prettyString t, show f, show xs]
    def s | "reduce" `T.isPrefixOf` s = Just $
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
              let acc = ValueAcc dest_shape (\_ x -> pure x) dest_arr
              acc' <- foldM (apply2 noLoc mempty f) acc vs_arr
              case acc' of
                ValueAcc _ _ dest_arr' ->
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
              let acc = ValueAcc dest_shape (apply2 noLoc mempty op) dest_arr
              acc' <- foldM (apply2 noLoc mempty f) acc vs_arr
              case acc' of
                ValueAcc _ _ dest_arr' ->
                  pure $ ValueArray dest_shape dest_arr'
                _ ->
                  error $ "hist_stream produced: " <> show acc'
          _ ->
            error $ "hist_stream expects array, but got: " <> prettyString (show dest, show vs)
    def "acc_write" = Just $
      fun3 $ \acc i v ->
        case (acc, i) of
          ( ValueAcc shape op acc_arr,
            ValuePrim (SignedValue (Int64Value i'))
            ) ->
              write acc v shape op acc_arr i'
          ( ValueAcc shape op acc_arr,
            adv@(ValueAD {})
            )
              | Just (SignedValue (Int64Value i')) <- putV . AD.primitive <$> getAD adv ->
                  write acc v shape op acc_arr i'
          _ ->
            error $ "acc_write invalid arguments: " <> prettyString (show acc, show i, show v)
      where
        write acc v shape op acc_arr i' =
          if i' >= 0 && i' < arrayLength acc_arr
            then do
              let x = acc_arr ! fromIntegral i'
              res <- op x v
              pure $ ValueAcc shape op $ acc_arr // [(fromIntegral i', res)]
            else pure acc
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
    def "flatten" = Just $
      fun1 $ \xs -> do
        let (ShapeDim n (ShapeDim m shape), xs') = fromArray xs
        pure $ toArray (ShapeDim (n * m) shape) $ concatMap (snd . fromArray) xs'
    def "unflatten" = Just $
      fun3 $ \n m xs -> do
        let (ShapeDim xs_size innershape, xs') = fromArray xs
            rowshape = ShapeDim (asInt64 m) innershape
            shape = ShapeDim (asInt64 n) rowshape
        if asInt64 n * asInt64 m /= xs_size || asInt64 n < 0 || asInt64 m < 0
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
    def "manifest" = Just $ fun1 pure
    def "vjp2" = Just $
      -- TODO: This could be much better. Currently, it is very inefficient
      -- Perhaps creating VJPValues could be abstracted into a function
      -- exposed by the AD module?
      fun3 $ \f v s -> do
        -- Get the depth
        depth <- adDepth

        -- Augment the values
        let v' =
              fromMaybe (error $ "vjp: invalid values " ++ show v) $
                modifyValueM (\i lv -> ValueAD depth . AD.VJP . AD.VJPValue . AD.TapeID i <$> getAD lv) v
        -- Turn the seeds into a list of ADValues
        let s' =
              fromMaybe (error $ "vjp: invalid seeds " ++ show s) $
                mapM getAD $
                  fst $
                    valueAccum (\a b -> (b : a, b)) [] s

        -- Run the function, and turn its outputs into a list of Values
        o <- apply noLoc mempty f v'
        let o' = fst $ valueAccum (\a b -> (b : a, b)) [] o

        -- For each output..
        m <-
          forM
            (zip o' s')
            ( \(on, sn) -> case on of
                -- If it is a VJP variable of the correct depth, run
                -- deriveTape on it- and its corresponding seed
                (ValueAD d (AD.VJP (AD.VJPValue t)))
                  | d == depth ->
                      getCounter
                        >>= either
                          (pure . Left)
                          (\(m', i) -> putCounter i $> Right (putAD $ AD.tapePrimal t, m'))
                          . AD.deriveTape t sn
                -- Otherwise, its partial derivatives are all 0
                _ -> pure $ Right (on, M.empty)
            )
            <&> either (error . show) id . sequence

        -- Add together every derivative
        drvs' <- AD.unionsWithM add (map snd m)
        let drvs = M.map (Just . putAD) drvs'

        -- Extract the output values, and the partial derivatives
        let ov = modifyValue (\i _ -> fst $ m !! (length m - 1 - i)) o
        let od =
              fromMaybe (error "vjp: differentiation failed") $
                modifyValueM (\i vo -> M.findWithDefault (ValuePrim . putV . P.blankPrimValue . P.primValueType . AD.primitive <$> getAD vo) i drvs) v

        -- Return a tuple of the output values, and partial derivatives
        pure $ toTuple [ov, od]
      where
        modifyValue f v = snd $ valueAccum (\a b -> (a + 1, f a b)) 0 v
        modifyValueM f v =
          snd
            <$> valueAccumLM
              ( \a b -> do
                  b' <- f a b
                  pure (a + 1, b')
              )
              0
              v

        -- TODO: Perhaps this could be fully abstracted by AD?
        -- Making addFor private would be nice..
        add x y =
          getCounter
            >>= either
              (error . show)
              (\(a, b) -> putCounter b >> pure a)
              . AD.doOp (AD.OpBin $ AD.addFor $ P.primValueType $ AD.primitive x) [x, y]
    def "jvp2" = Just $
      -- TODO: This could be much better. Currently, it is very inefficient
      -- Perhaps creating JVPValues could be abstracted into a function
      -- exposed by the AD module?
      fun3 $ \f v s -> do
        depth <- adDepth

        -- Turn the seeds into a list of ADValues
        let s' =
              expectJust ("jvp: invalid seeds " ++ show s) $
                mapM getAD $
                  fst $
                    valueAccum (\a b -> (b : a, b)) [] s
        -- Augment the values
        let v' =
              expectJust ("jvp: invalid values " ++ show v) $
                modifyValueM
                  ( \i lv -> do
                      lv' <- getAD lv
                      pure $ ValueAD depth . AD.JVP . AD.JVPValue lv' $ s' !! (length s' - 1 - i)
                  )
                  v

        -- Run the function, and turn its outputs into a list of Values
        o <- apply noLoc mempty f v'
        let o' = fst $ valueAccum (\a b -> (b : a, b)) [] o

        -- For each output..
        let m =
              expectJust "jvp: differentiation failed" $
                mapM
                  ( \on -> case on of
                      -- If it is a JVP variable of the correct depth, return
                      -- its primal and derivative
                      (ValueAD d (AD.JVP (AD.JVPValue pv dv)))
                        | d == depth -> Just (putAD pv, putAD dv)
                      -- Otherwise, its partial derivatives are all 0
                      _ ->
                        (on,)
                          . ValuePrim
                          . putV
                          . P.blankPrimValue
                          . P.primValueType
                          . AD.primitive
                          <$> getAD on
                  )
                  o'

        -- Extract the output values, and the partial derivatives
        let ov = modifyValue (\i _ -> fst $ m !! (length m - 1 - i)) o
            od = modifyValue (\i _ -> snd $ m !! (length m - 1 - i)) o

        -- Return a tuple of the output values, and partial derivatives
        pure $ toTuple [ov, od]
      where
        modifyValue f v = snd $ valueAccum (\a b -> (a + 1, f a b)) 0 v
        modifyValueM f v = snd <$> valueAccumLM step 0 v
          where
            step a b = do
              b' <- f a b
              pure (a + 1, b')

        expectJust _ (Just v) = v
        expectJust s Nothing = error s
    def "acc" = Nothing
    def s | nameFromText s `M.member` namesToPrimTypes = Nothing
    def s = error $ "Missing intrinsic: " ++ T.unpack s

    tdef :: Name -> Maybe TypeBinding
    tdef s = do
      t <- s `M.lookup` namesToPrimTypes
      pure $ TypeBinding mempty [] $ RetType [] $ Scalar $ Prim t

intrinsicVal :: Name -> Value
intrinsicVal name =
  case M.lookup (intrinsicVar name) $ envTerm $ ctxEnv initialCtx of
    Just (TermValue _ v) -> v
    _ -> error $ "intrinsicVal: " <> prettyString name

intrinsicsNeg :: Value
intrinsicsNeg = intrinsicVal "neg"

intrinsicsNot :: Value
intrinsicsNot = intrinsicVal "!"

interpretExp :: Ctx -> Exp -> F ExtOp Value
interpretExp ctx e = runEvalM (ctxImports ctx) $ eval (ctxEnv ctx) e

interpretDecs :: Ctx -> [Dec] -> F ExtOp Env
interpretDecs ctx decs =
  runEvalM (ctxImports ctx) $ do
    (env, _) <- evalDecs (ctxEnv ctx) decs
    -- We need to extract any new existential sizes and add them as
    -- ordinary bindings to the context, or we will not be able to
    -- look up their values later.
    sizes <- extEnv
    pure $ env <> sizes

interpretDec :: Ctx -> Dec -> F ExtOp Ctx
interpretDec ctx d = do
  env <- interpretDecs ctx [d]
  pure ctx {ctxEnv = env}

interpretImport :: Ctx -> (ImportName, Prog) -> F ExtOp Ctx
interpretImport ctx (fp, prog) = do
  env <- interpretDecs ctx $ progDecs prog
  pure ctx {ctxImports = M.insert fp env $ ctxImports ctx}

-- | Produce a context, based on the one passed in, where all of
-- the provided imports have been @open@ened in order.
ctxWithImports :: [Env] -> Ctx -> Ctx
ctxWithImports envs ctx = ctx {ctxEnv = mconcat (reverse envs) <> ctxEnv ctx}

valueType :: V.Value -> ValueType
valueType v =
  let V.ValueType shape pt = V.valueType v
   in arrayOf (F.Shape (map fromIntegral shape)) (Scalar (Prim (toPrim pt)))
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
  | args_ts == map toStruct param_ts =
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
          "Entry point "
            <> dquotes (prettyName entry)
            <> " expects input of type(s)"
              </> indent 2 (stack (map pretty param_ts))

-- | Execute the named function on the given arguments; may fail
-- horribly if these are ill-typed.
interpretFunction :: Ctx -> VName -> [V.Value] -> Either T.Text (F ExtOp Value)
interpretFunction ctx fname vs = do
  let env = ctxEnv ctx

  (ft, mkf) <- case lookupVar (qualName fname) env of
    Just (TermValue (Just (T.BoundV _ t)) v) -> do
      ft <- updateType (map valueType vs) t
      pure (ft, pure v)
    Just (TermPoly (Just (T.BoundV _ t)) v) -> do
      ft <- updateType (map valueType vs) t
      pure (ft, v (structToEval env ft))
    _ ->
      Left $ "Unknown function `" <> nameToText (toName fname) <> "`."

  let vs' = map fromDataValue vs

  checkEntryArgs fname vs ft

  Right $ runEvalM (ctxImports ctx) $ do
    f <- mkf
    foldM (apply noLoc mempty) f vs'
  where
    updateType (vt : vts) (Scalar (Arrow als pn d pt (RetType dims rt))) = do
      checkInput vt pt
      Scalar . Arrow als pn d (valueStructType vt) . RetType dims . toRes Nonunique
        <$> updateType vts (toStruct rt)
    updateType _ t =
      Right t

    checkInput :: ValueType -> StructType -> Either T.Text ()
    checkInput (Scalar (Prim vt)) (Scalar (Prim pt))
      | vt /= pt = badPrim vt pt
    checkInput (Array _ _ (Prim vt)) (Array _ _ (Prim pt))
      | vt /= pt = badPrim vt pt
    checkInput vArr@(Array _ (F.Shape vd) _) pArr@(Array _ (F.Shape pd) _)
      | length vd /= length pd = badDim vArr pArr
      | not . and $ zipWith sameShape vd pd = badDim vArr pArr
      where
        sameShape :: Int64 -> Size -> Bool
        sameShape shape0 (IntLit shape1 _ _) = fromIntegral shape0 == shape1
        sameShape _ _ = True
    checkInput _ _ =
      Right ()

    badPrim vt pt =
      Left . docText $
        "Invalid argument type."
          </> "Expected:"
          <+> align (pretty pt)
          </> "Got:     "
          <+> align (pretty vt)

    badDim vd pd =
      Left . docText $
        "Invalid argument dimensions."
          </> "Expected:"
          <+> align (pretty pd)
          </> "Got:     "
          <+> align (pretty vd)
