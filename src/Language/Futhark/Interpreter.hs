{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Futhark.Interpreter
  ( Ctx(..)
  , Env(..)
  , InterpreterError
  , initialCtx
  , interpretExp
  , interpretDec
  , interpretImport
  , interpretFunction
  , ExtOp(..)
  , typeEnv
  , Value (ValuePrim, ValueArray, ValueRecord)
  , mkArray
  , fromTuple
  , isEmptyArray
  ) where

import Control.Monad.Free.Church
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.Fail as Fail
import Data.Array
import Data.Bifunctor (bimap)
import Data.List hiding (break)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import Data.Monoid
import Data.Loc

import Language.Futhark hiding (Value)
import Futhark.Representation.Primitive (intValue, floatValue)
import qualified Futhark.Representation.Primitive as P
import qualified Language.Futhark.Semantic as T

import Futhark.Util.Pretty hiding (apply, bool, stack)
import Futhark.Util (chunk, splitFromEnd, maybeHead)

import Prelude hiding (mod, break)

data ExtOp a = ExtOpTrace SrcLoc String a
             | ExtOpBreak [SrcLoc] Ctx T.Env a
             | ExtOpError InterpreterError

instance Functor ExtOp where
  fmap f (ExtOpTrace w s x) = ExtOpTrace w s $ f x
  fmap f (ExtOpBreak w ctx env x) = ExtOpBreak w ctx env $ f x
  fmap _ (ExtOpError err) = ExtOpError err

data StackFrame = StackFrame { stackFrameSrcLoc :: SrcLoc
                             , stackFrameEnv :: Env
                             }

type Stack = [StackFrame]

-- | The monad in which evaluation takes place.
newtype EvalM a = EvalM (ReaderT (Stack, M.Map FilePath Env)
                         (F ExtOp) a)
  deriving (Monad, Applicative, Functor,
            MonadFree ExtOp,
            MonadReader (Stack, M.Map FilePath Env))

instance Fail.MonadFail EvalM where
  fail = error

runEvalM :: M.Map FilePath Env -> EvalM a -> F ExtOp a
runEvalM imports (EvalM m) = runReaderT m (mempty, imports)

stacking :: SrcLoc -> Env -> EvalM a -> EvalM a
stacking loc env = local $ \(ss, imports) ->
  if isNoLoc loc then (ss, imports) else (StackFrame loc env:ss, imports)
  where isNoLoc :: SrcLoc -> Bool
        isNoLoc = (==NoLoc) . locOf

stacktrace :: EvalM [SrcLoc]
stacktrace = asks $ map stackFrameSrcLoc . reverse . fst

stacktraceTop :: EvalM SrcLoc
stacktraceTop = fromMaybe noLoc . maybeHead <$> stacktrace

lookupImport :: FilePath -> EvalM (Maybe Env)
lookupImport f = asks $ M.lookup f . snd

-- | A fully evaluated Futhark value.
data Value = ValuePrim !PrimValue
           | ValueArray !(Array Int Value)
           | ValueRecord (M.Map Name Value)
           | ValueFun (Value -> EvalM Value)

instance Eq Value where
  ValuePrim x == ValuePrim y = x == y
  ValueArray x == ValueArray y = x == y
  ValueRecord x == ValueRecord y = x == y
  _ == _ = False

prettyRecord :: Pretty a => M.Map Name a -> Doc
prettyRecord m
  | Just vs <- areTupleFields m =
      parens $ commasep $ map ppr vs
  | otherwise =
      braces $ commasep $ map field $ M.toList m
      where field (k, v) = ppr k <+> equals <+> ppr v

instance Pretty Value where
  ppr (ValuePrim v) = ppr v
  ppr (ValueArray a) = brackets $ commasep $ map ppr $ elems a
  ppr (ValueRecord m) = prettyRecord m
  ppr ValueFun{} = text "#<fun>"

-- | Create an array value; failing if that would result in an
-- irregular array.
mkArray :: [Value] -> Maybe Value
mkArray vs =
  case vs of [] -> Just $ toArray' vs
             v:_ | all ((==valueShape v) . valueShape) vs -> Just $ toArray' vs
                 | otherwise -> Nothing

isEmptyArray :: Value -> Bool
isEmptyArray (ValueArray arr) =
  arrayLength arr == (0::Int32) || any isEmptyArray (elems arr)
isEmptyArray _ = False

-- | A shape is a tree to accomodate the case of records.
data Shape = ShapeDim Int32 Shape
           | ShapeLeaf
           | ShapeRecord (M.Map Name Shape)
           deriving (Eq, Show)

instance Pretty Shape where
  ppr ShapeLeaf = mempty
  ppr (ShapeDim d s) = brackets (ppr d) <> ppr s
  ppr (ShapeRecord m) = prettyRecord m

emptyShape :: Shape -> Bool
emptyShape ShapeLeaf = False
emptyShape (ShapeDim d s) = d == 0 || emptyShape s
emptyShape (ShapeRecord fs) = any emptyShape fs

valueShape :: Value -> Shape
valueShape (ValueArray arr) = ShapeDim (arrayLength arr) $
                              case elems arr of
                                []  -> ShapeLeaf
                                v:_ -> valueShape v
valueShape (ValueRecord fs) = ShapeRecord $ M.map valueShape fs
valueShape _ = ShapeLeaf

arrayLength :: Integral int => Array Int Value -> int
arrayLength = fromIntegral . (+1) . snd . bounds

toTuple :: [Value] -> Value
toTuple = ValueRecord . M.fromList . zip tupleFieldNames

fromTuple :: Value -> Maybe [Value]
fromTuple (ValueRecord m) = areTupleFields m
fromTuple _ = Nothing

asInteger :: Value -> Integer
asInteger (ValuePrim (SignedValue v)) = P.valueIntegral v
asInteger (ValuePrim (UnsignedValue v)) =
  toInteger (P.valueIntegral (P.doZExt v Int64) :: Word64)
asInteger v = error $ "Unexpectedly not an integer: " ++ pretty v

asInt :: Value -> Int
asInt = fromIntegral . asInteger

asSigned :: Value -> IntValue
asSigned (ValuePrim (SignedValue v)) = v
asSigned v = error $ "Unexpected not a signed integer: " ++ pretty v

asInt32 :: Value -> Int32
asInt32 = fromIntegral . asInteger

asBool :: Value -> Bool
asBool (ValuePrim (BoolValue x)) = x
asBool v = error $ "Unexpectedly not an integer: " ++ pretty v

lookupInEnv :: (Env -> M.Map VName x)
            -> QualName VName -> Env -> Maybe x
lookupInEnv onEnv qv env = f env $ qualQuals qv
  where f m (q:qs) =
          case M.lookup q $ envTerm m of
            Just (TermModule (Module mod)) -> f mod qs
            _ -> Nothing
        f m [] = M.lookup (qualLeaf qv) $ onEnv m

lookupVar :: QualName VName -> Env -> Maybe TermBinding
lookupVar = lookupInEnv envTerm

lookupType :: QualName VName -> Env -> Maybe T.TypeBinding
lookupType = lookupInEnv envType

-- | A TermValue with a 'Nothing' type annotation is an intrinsic.
data TermBinding = TermValue (Maybe T.BoundV) Value
                 | TermModule Module

data Module = Module Env
            | ModuleFun (Module -> EvalM Module)

data Env = Env { envTerm :: M.Map VName TermBinding
               , envType :: M.Map VName T.TypeBinding
               }

instance Monoid Env where
  mempty = Env mempty mempty
  mappend = (Sem.<>)

instance Sem.Semigroup Env where
  Env vm1 tm1 <> Env vm2 tm2 = Env (vm1 <> vm2) (tm1 <> tm2)

newtype InterpreterError = InterpreterError String

valEnv :: M.Map VName (Maybe T.BoundV, Value) -> Env
valEnv m = Env { envTerm = M.map (uncurry TermValue) m
               , envType = mempty
               }

modEnv :: M.Map VName Module -> Env
modEnv m = Env { envTerm = M.map TermModule m
               , envType = mempty
               }

instance Show InterpreterError where
  show (InterpreterError s) = s

bad :: SrcLoc -> Env -> String -> EvalM a
bad loc env s = stacking loc env $ do
  ss <- map locStr <$> stacktrace
  liftF $ ExtOpError $ InterpreterError $ "Error at " ++ intercalate " -> " ss ++ ": " ++ s

trace :: Value -> EvalM ()
trace v = do
  top <- stacktraceTop
  liftF $ ExtOpTrace top (pretty v) ()

typeEnv :: Env -> T.Env
typeEnv env =
  -- FIXME: some shadowing issues are probably not right here.
  let valMap (TermValue (Just t) _) = Just t
      valMap _ = Nothing
      vtable = M.mapMaybe valMap $ envTerm env
      nameMap k | k `M.member` vtable = Just ((T.Term, baseName k), qualName k)
                | otherwise = Nothing
  in mempty { T.envNameMap = M.fromList $ mapMaybe nameMap $ M.keys $ envTerm env
            , T.envVtable = vtable }

break :: EvalM ()
break = do
  -- We don't want the env of the function that is calling
  -- intrinsics.break, since that is just going to be the boring
  -- wrapper function (intrinsics are never called directly).
  -- This is why we go a step up the stack.
  stack <- asks $ drop 1 . fst
  case stack of
    [] -> return ()
    top:_ -> do
      let env = stackFrameEnv top
      imports <- asks snd
      liftF $ ExtOpBreak
        (map stackFrameSrcLoc $ reverse stack)
        (Ctx env imports) (typeEnv env) ()

fromArray :: Value -> [Value]
fromArray (ValueArray as) = elems as
fromArray v = error $ "Expected array value, but found: " ++ pretty v

-- | This is where we enforce the regularity constraint for arrays.
toArray :: [Value] -> EvalM Value
toArray = maybe (bad noLoc mempty "irregular array") return . mkArray

toArray' :: [Value] -> Value
toArray' vs = ValueArray (listArray (0, length vs - 1) vs)

apply :: SrcLoc -> Env -> Value -> Value -> EvalM Value
apply loc env (ValueFun f) v = stacking loc env $ f v
apply _ _ f _ = error $ "Cannot apply non-function: " ++ pretty f

apply2 :: SrcLoc -> Env -> Value -> Value -> Value -> EvalM Value
apply2 loc env f x y = stacking loc env $ do f' <- apply noLoc mempty f x
                                             apply noLoc mempty f' y

matchPattern :: Env -> Pattern -> Value
             -> EvalM (M.Map VName (Maybe T.BoundV, Value))
matchPattern env = matchPattern' env mempty

matchPattern' :: Env -> M.Map VName (Maybe T.BoundV, Value)
              -> Pattern -> Value
              -> EvalM (M.Map VName (Maybe T.BoundV, Value))
matchPattern' _ m (Id v (Info t) _) val =
  pure $ M.insert v (Just $ T.BoundV [] $ toStruct t, val) m
matchPattern' env m (PatternParens p _) val =
  matchPattern' env m p val
matchPattern' env m (TuplePattern ps _) (ValueRecord vs) =
  foldM (\m' (p,v) -> matchPattern' env m' p v) m $
  zip ps (map snd $ sortFields vs)
matchPattern' env m (RecordPattern ps _) (ValueRecord vs) =
  foldM (\m' (p,v) -> matchPattern' env m' p v) m $
  zip (map snd $ sortFields $ M.fromList ps) (map snd $ sortFields vs)
matchPattern' _ m Wildcard{} _ = pure m
matchPattern' env m (PatternAscription pat td loc) v = do
  t <- evalType env $ unInfo $ expandedType td
  case matchValueToType env m t v of
    Left err -> bad loc env err
    Right m' -> matchPattern' env m' pat v
matchPattern' _ _ pat v =
  error $ "matchPattern': missing case for " ++ pretty pat ++ " and " ++ pretty v

-- | For matching size annotations (the actual type will have been
-- verified by the type checker).  It is assumed that previously
-- unbound names are in binding position here.
matchValueToType :: Env -> M.Map VName (Maybe T.BoundV, Value)
                 -> StructType
                 -> Value
                 -> Either String (M.Map VName (Maybe T.BoundV, Value))

-- Empty arrays always match.
matchValueToType env m t@(Array _ (ShapeDecl ds@(d:_)) _) val@(ValueArray arr)
  | any zeroDim ds, emptyShape (valueShape val) =
      Right $ m <> mconcat (map namedAreZero ds)

  | otherwise =
      case d of
        NamedDim v
          | Just x <- look v ->
              if x == arr_n
              then continue m
              else wrong $ "`" <> pretty v <> "` (" <> pretty x <> ")"
          | otherwise ->
              continue $ M.insert (qualLeaf v)
              (Just $ T.BoundV [] $ Prim $ Signed Int32,
               ValuePrim $ SignedValue $ Int32Value arr_n)
              m
        AnyDim -> continue m
        ConstDim x
          | fromIntegral x == arr_n -> continue m
          | otherwise -> wrong $ pretty x
  where arr_n = arrayLength arr

        look v
          | Just (TermValue _ (ValuePrim (SignedValue (Int32Value x)))) <-
              lookupVar v env = Just x
          | Just (_, ValuePrim (SignedValue (Int32Value x))) <-
              M.lookup (qualLeaf v) m = Just x
          | otherwise = Nothing

        continue m' = case elems arr of
          [] -> return m'
          v:_ -> matchValueToType env m' (stripArray 1 t) v

        wrong x = Left $ "Size annotation " <> x <>
                  " does not match observed size " <> pretty arr_n <> "."

        zeroDim (NamedDim v) = Just 0 == look v
        zeroDim AnyDim = False
        zeroDim (ConstDim x) = x == 0

        namedAreZero (NamedDim v) =
          M.singleton (qualLeaf v) (Just $ T.BoundV [] $ Prim $ Signed Int32,
                                    ValuePrim $ SignedValue $ Int32Value 0)
        namedAreZero _ = mempty

matchValueToType env m (Record fs) (ValueRecord arr) =
  foldM (\m' (t, v) -> matchValueToType env m' t v) m $
  M.intersectionWith (,) fs arr

matchValueToType _ m _ _ = return m

data Indexing = IndexingFix Int32
              | IndexingSlice (Maybe Int32) (Maybe Int32) (Maybe Int32)

instance Pretty Indexing where
  ppr (IndexingFix i) = ppr i
  ppr (IndexingSlice i j (Just s)) =
    maybe mempty ppr i <> text ":" <>
    maybe mempty ppr j <> text ":" <>
    ppr s
  ppr (IndexingSlice i (Just j) s) =
    maybe mempty ppr i <> text ":" <>
    ppr j <>
    maybe mempty ((text ":" <>) . ppr) s
  ppr (IndexingSlice i Nothing Nothing) =
    maybe mempty ppr i <> text ":"

indexesFor :: Maybe Int32 -> Maybe Int32 -> Maybe Int32
           -> Array Int Value -> Maybe [Int]
indexesFor start end stride arr
  | (start', end', stride') <- slice,
    end' == start' || signum' (end' - start') == signum' stride',
    stride' /= 0,
    is <- [start', start'+stride' .. end'-signum stride'],
    all inBounds is =
      Just $ map fromIntegral is
  | otherwise =
      Nothing
  where n = arrayLength arr

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
              (if stride' > 0 then 0 else n-1,
               if stride' > 0 then n else -1,
               stride')
            (Nothing, Nothing, Nothing) ->
              (0, n, 1)

-- | 'signum', but with 0 as 1.
signum' :: (Eq p, Num p) => p -> p
signum' x = if x == 0 then 1 else signum x

indexArray :: [Indexing] -> Value -> Maybe Value
indexArray (IndexingFix i:is) (ValueArray arr)
  | i >= 0, i < n =
      indexArray is $ arr ! fromIntegral i
  | otherwise =
      Nothing
  where n = arrayLength arr
indexArray (IndexingSlice start end stride:is) (ValueArray arr) = do
  js <- indexesFor start end stride arr
  toArray' <$> mapM (indexArray is . (arr!)) js
indexArray _ v = Just v

updateArray :: [Indexing] -> Value -> Value -> Maybe Value
updateArray (IndexingFix i:is) (ValueArray arr) v
  | i >= 0, i < n = do
      v' <- updateArray is (arr ! i') v
      Just $ ValueArray $ arr // [(i', v')]
  | otherwise =
      Nothing
  where n = arrayLength arr
        i' = fromIntegral i
updateArray (IndexingSlice start end stride:is) (ValueArray arr) (ValueArray v) = do
  arr_is <- indexesFor start end stride arr
  guard $ length arr_is == arrayLength v
  let update arr' (i, v') = do
        x <- updateArray is (arr!i) v'
        return $ arr' // [(i, x)]
  fmap ValueArray $ foldM update arr $ zip arr_is $ elems v
updateArray _ _ v = Just v

evalDimIndex :: Env -> DimIndex -> EvalM Indexing
evalDimIndex env (DimFix x) =
  IndexingFix . asInt32 <$> eval env x
evalDimIndex env (DimSlice start end stride) =
  IndexingSlice <$> traverse (fmap asInt32 . eval env) start
                <*> traverse (fmap asInt32 . eval env) end
                <*> traverse (fmap asInt32 . eval env) stride

evalIndex :: SrcLoc -> Env -> [Indexing] -> Value -> EvalM Value
evalIndex loc env is arr = do
  let oob = bad loc env $ "Index [" <> intercalate ", " (map pretty is) <>
            "] out of bounds for array of shape " <>
            pretty (valueShape arr) <> "."
  maybe oob return $ indexArray is arr

evalTermVar :: Env -> QualName VName -> EvalM Value
evalTermVar env qv =
  case lookupVar qv env of
    Just (TermValue _ v) -> return v
    _ -> error $ "`" <> pretty qv <> "` is not bound to a value."

-- | Expand type based on information that was not available at
-- type-checking time (the structure of abstract types).
evalType :: Env -> StructType -> EvalM StructType
evalType _ (Prim pt) = return $ Prim pt
evalType env (Record fs) = Record <$> traverse (evalType env) fs
evalType env (Arrow () p t1 t2) =
  Arrow () p <$> evalType env t1 <*> evalType env t2
evalType env t@(Array _ shape u) = do
  let et = stripArray (shapeRank shape) t
  et' <- evalType env et
  shape' <- traverse evalDim shape
  return $
    fromMaybe (error "Cannot construct array after substitution") $
    arrayOf et' shape' u
  where evalDim (NamedDim qn)
          | Just (TermValue _ (ValuePrim (SignedValue (Int32Value x)))) <-
              lookupVar qn env =
              return $ ConstDim $ fromIntegral x
        evalDim d = return d
evalType env t@(TypeVar () _ tn args) =
  case lookupType (qualNameFromTypeName tn) env of
    Just (T.TypeAbbr _ ps t') -> do
      (substs, types) <- mconcat <$> zipWithM matchPtoA ps args
      let onDim (NamedDim v) = fromMaybe (NamedDim v) $ M.lookup (qualLeaf v) substs
          onDim d = d
      if null ps then return $ bimap onDim id t'
      else evalType (Env mempty types <> env) $ bimap onDim id t'
    Nothing -> return t
  where matchPtoA (TypeParamDim p _) (TypeArgDim (NamedDim qv) _) =
          return (M.singleton p $ NamedDim qv, mempty)
        matchPtoA (TypeParamDim p _) (TypeArgDim (ConstDim k) _) =
          return (M.singleton p $ ConstDim k, mempty)
        matchPtoA (TypeParamType l p _) (TypeArgType t' _) = do
          t'' <- evalType env t'
          return (mempty, M.singleton p $ T.TypeAbbr l [] t'')
        matchPtoA _ _ = return mempty

eval :: Env -> Exp -> EvalM Value

eval _ (Literal v _) = return $ ValuePrim v

eval env (Parens e _ ) = eval env e

eval env (QualParens _ e _ ) = eval env e

eval env (TupLit vs _) = toTuple <$> mapM (eval env) vs

eval env (RecordLit fields _) =
  ValueRecord . M.fromList <$> mapM evalField fields
  where evalField (RecordFieldExplicit k e _) = do
          v <- eval env e
          return (k, v)
        evalField (RecordFieldImplicit k t loc) = do
          v <- eval env $ Var (qualName k) (vacuousShapeAnnotations <$> t) loc
          return (baseName k, v)

eval env (ArrayLit vs _ _) = toArray =<< mapM (eval env) vs

eval env (Range start maybe_second end (Info t) _) = do
  start' <- asInteger <$> eval env start
  maybe_second' <- traverse (fmap asInteger . eval env) maybe_second
  (end', dir) <- case end of
    DownToExclusive e -> (,-1) . (+1) . asInteger <$> eval env e
    ToInclusive e -> (,maybe 1 (signum . subtract start') maybe_second') .
                     asInteger <$> eval env e
    UpToExclusive e -> (,1) . subtract 1 . asInteger <$> eval env e

  let second = fromMaybe (start' + dir) maybe_second'
      step = second - start'
  if step == 0 || dir /= signum step then toArray []
    else toArray $ map toInt [start',second..end']

  where toInt =
          case stripArray 1 t of
            Prim (Signed t') ->
              ValuePrim . SignedValue . intValue t'
            Prim (Unsigned t') ->
              ValuePrim . UnsignedValue . intValue t'
            _ -> error $ "Nonsensical range type: " ++ show t

eval env (Var qv _ _) = evalTermVar env qv

eval env (Ascript e td loc) = do
  v <- eval env e
  t <- evalType env $ unInfo $ expandedType td
  case matchValueToType env mempty t v of
    Right _ -> return v
    Left _ -> bad loc env $ "Value `" <> pretty v <> "` cannot match shape of type `" <>
              pretty (declaredType td) <> "` (`" <> pretty t <> "`)."

eval env (LetPat _ p e body _) = do
  v <- eval env e
  p_env <- valEnv <$> matchPattern env p v
  eval (p_env <> env) body

eval env (LetFun f (tparams, pats, _, Info ret, fbody) body loc) = do
  v <- eval env $ Lambda tparams pats fbody Nothing (Info (mempty, ret)) loc
  let ftype = T.BoundV [] $ foldr (uncurry (Arrow ()) . patternParam) ret pats
  eval (valEnv (M.singleton f (Just ftype, v)) <> env) body

eval _ (IntLit v (Info t) _) =
  case t of
    Prim (Signed it) ->
      return $ ValuePrim $ SignedValue $ intValue it v
    Prim (Unsigned it) ->
      return $ ValuePrim $ UnsignedValue $ intValue it v
    Prim (FloatType ft) ->
      return $ ValuePrim $ FloatValue $ floatValue ft v
    _ -> error $ "eval: nonsensical type for integer literal: " ++ pretty t

eval _ (FloatLit v (Info t) _) =
  case t of
    Prim (FloatType ft) ->
      return $ ValuePrim $ FloatValue $ floatValue ft v
    _ -> error $ "eval: nonsensical type for float literal: " ++ pretty t

eval env (BinOp op op_t (x, _) (y, _) _ loc)
  | baseString (qualLeaf op) == "&&" = do
      x' <- asBool <$> eval env x
      if x'
        then eval env y
        else return $ ValuePrim $ BoolValue False
  | baseString (qualLeaf op) == "||" = do
      x' <- asBool <$> eval env x
      if x'
        then return $ ValuePrim $ BoolValue True
        else eval env y
  | otherwise = do
      op' <- eval env $ Var op op_t loc
      x' <- eval env x
      y' <- eval env y
      apply2 loc env op' x' y'

eval env (If cond e1 e2 _ _) = do
  cond' <- asBool <$> eval env cond
  if cond' then eval env e1 else eval env e2

eval env (Apply f x _ _ loc) = do
  f' <- eval env f
  x' <- eval env x
  apply loc env f' x'

eval env (Negate e _) = do
  ev <- eval env e
  ValuePrim <$> case ev of
    ValuePrim (SignedValue (Int8Value v)) -> return $ SignedValue $ Int8Value (-v)
    ValuePrim (SignedValue (Int16Value v)) -> return $ SignedValue $ Int16Value (-v)
    ValuePrim (SignedValue (Int32Value v)) -> return $ SignedValue $ Int32Value (-v)
    ValuePrim (SignedValue (Int64Value v)) -> return $ SignedValue $ Int64Value (-v)
    ValuePrim (UnsignedValue (Int8Value v)) -> return $ UnsignedValue $ Int8Value (-v)
    ValuePrim (UnsignedValue (Int16Value v)) -> return $ UnsignedValue $ Int16Value (-v)
    ValuePrim (UnsignedValue (Int32Value v)) -> return $ UnsignedValue $ Int32Value (-v)
    ValuePrim (UnsignedValue (Int64Value v)) -> return $ UnsignedValue $ Int64Value (-v)
    ValuePrim (FloatValue (Float32Value v)) -> return $ FloatValue $ Float32Value (-v)
    ValuePrim (FloatValue (Float64Value v)) -> return $ FloatValue $ Float64Value (-v)
    _ -> fail $ "Cannot negate " ++ pretty ev

eval env (Index e is _ loc) = do
  is' <- mapM (evalDimIndex env) is
  arr <- eval env e
  evalIndex loc env is' arr

eval env (Update src is v loc) =
  maybe oob return =<<
  updateArray <$> mapM (evalDimIndex env) is <*> eval env src <*> eval env v
  where oob = bad loc env "Bad update"

eval env (LetWith dest src is v body loc) = do
  dest' <- maybe oob return =<<
    updateArray <$> mapM (evalDimIndex env) is <*>
    evalTermVar env (qualName $ identName src) <*> eval env v
  let t = T.BoundV [] $ vacuousShapeAnnotations $ toStruct $ unInfo $ identType dest
  eval (valEnv (M.singleton (identName dest) (Just t, dest')) <> env) body
  where oob = bad loc env "Bad update"

-- We treat zero-parameter lambdas as simply an expression to
-- evaluate immediately.  Note that this is *not* the same as a lambda
-- that takes an empty tuple '()' as argument!  Zero-parameter lambdas
-- can never occur in a well-formed Futhark program, but they are
-- convenient in the interpreter.
eval env (Lambda _ [] body _ (Info (_, t)) loc) = do
  v <- eval env body
  case (t, v) of
    (Arrow _ _ _ rt, ValueFun f) ->
      return $ ValueFun $ \arg -> do r <- f arg
                                     rt' <- evalType env rt
                                     match rt' r
    _ -> match t v
  where match vt v =
          case matchValueToType env mempty vt v of
            Right _ -> return v
            Left _ -> bad loc env $ "Value `" <> pretty v <>
                      "` cannot match type `" <> pretty vt <> "`."

eval env (Lambda tparams (p:ps) body mrd (Info (als, ret)) loc) =
  return $ ValueFun $ \v -> do
    p_env <- valEnv <$> matchPattern env p v
    eval (p_env <> env) $ Lambda tparams ps body mrd (Info (als, ret)) loc

eval env (OpSection qv _  _) = evalTermVar env qv

eval env (OpSectionLeft qv _ e _ _ loc) =
  join $ apply loc env <$> evalTermVar env qv <*> eval env e

eval env (OpSectionRight qv _ e _ _ loc) = do
  f <- evalTermVar env qv
  y <- eval env e
  return $ ValueFun $ \x -> join $ apply loc env <$> apply loc env f x <*> pure y

eval env (IndexSection is _ loc) = do
  is' <- mapM (evalDimIndex env) is
  return $ ValueFun $ evalIndex loc env is'

eval _ (ProjectSection ks _ _) = return $ ValueFun $ flip (foldM walk) ks
  where walk (ValueRecord fs) f
          | Just v' <- M.lookup f fs = return v'
        walk _ _ = fail "Value does not have expected field."

eval env (DoLoop _ pat init_e form body _) = do
  init_v <- eval env init_e
  case form of For iv bound -> do
                 bound' <- asSigned <$> eval env bound
                 forLoop (identName iv) bound' (zero bound') init_v
               ForIn in_pat in_e -> do
                 in_vs <- fromArray <$> eval env in_e
                 foldM (forInLoop in_pat) init_v in_vs
               While cond ->
                 whileLoop cond init_v
  where withLoopParams v = (<>env) . valEnv <$> matchPattern env pat v

        inc = (`P.doAdd` Int64Value 1)
        zero = (`P.doMul` Int64Value 0)

        forLoop iv bound i v
          | i == bound = return v
          | otherwise = do
              env' <- withLoopParams v
              forLoop iv bound (inc i) =<<
                eval (valEnv (M.singleton iv (Just $ T.BoundV [] $ Prim $ Signed Int32,
                                              ValuePrim (SignedValue i))) <> env') body

        whileLoop cond v = do
          env' <- withLoopParams v
          continue <- asBool <$> eval env' cond
          if continue
            then whileLoop cond =<< eval env' body
            else return v

        forInLoop in_pat v in_v = do
          env' <- withLoopParams v
          pat_env <- matchPattern env' in_pat in_v
          eval (valEnv pat_env <> env') body

eval env (Project f e _ _) = do
  v <- eval env e
  case v of
    ValueRecord fs | Just v' <- M.lookup f fs -> return v'
    _ -> fail "Value does not have expected field."

eval env (Unsafe e _) = eval env e

eval env (Assert what e (Info s) loc) = do
  cond <- asBool <$> eval env what
  unless cond $ bad loc env s
  eval env e

eval _ e = error $ "eval not yet: " ++ show e

substituteInModule :: M.Map VName VName -> Module -> Module
substituteInModule substs = onModule
  where
    rev_substs = reverseSubstitutions substs
    replace v = fromMaybe v $ M.lookup v rev_substs
    replaceQ v = maybe v qualName $ M.lookup (qualLeaf v) rev_substs
    replaceM f m = M.fromList $ do
      (k, v) <- M.toList m
      return (replace k, f v)
    onModule (Module (Env terms types)) =
      Module $ Env (replaceM onTerm terms) (replaceM onType types)
    onModule (ModuleFun f) =
      ModuleFun $ \m -> onModule <$> f (substituteInModule rev_substs m)
    onTerm (TermValue t v) = TermValue t v
    onTerm (TermModule m) = TermModule $ onModule m
    onType (T.TypeAbbr l ps t) = T.TypeAbbr l ps $ bimap onDim id t
    onDim (NamedDim v) = NamedDim $ replaceQ v
    onDim (ConstDim x) = ConstDim x
    onDim AnyDim = AnyDim

reverseSubstitutions :: M.Map VName VName -> M.Map VName VName
reverseSubstitutions = M.fromList . map (uncurry $ flip (,)) . M.toList

evalModuleVar :: Env -> QualName VName -> EvalM Module
evalModuleVar env qv =
  case lookupVar qv env of
    Just (TermModule m) -> return m
    _ -> error $ "`" <> pretty qv <> "` is not bound to a module."

evalModExp :: Env -> ModExp -> EvalM Module

evalModExp _ (ModImport _ (Info f) _) = do
  f' <- lookupImport f
  case f' of Nothing -> error $ "Unknown import " ++ show f
             Just m -> return $ Module m

evalModExp env (ModDecs ds _) = do
  Env terms types <- foldM evalDec env ds
  -- Remove everything that was present in the original Env.
  return $ Module $ Env (terms `M.difference` envTerm env)
                        (types `M.difference` envType env)

evalModExp env (ModVar qv _) =
  evalModuleVar env qv

evalModExp env (ModAscript me _ (Info substs) _) =
  substituteInModule substs <$> evalModExp env me

evalModExp env (ModParens me _) = evalModExp env me

evalModExp env (ModLambda p ret e loc) =
  return $ ModuleFun $ \am -> do
  let env' = env { envTerm = M.insert (modParamName p) (TermModule am) $ envTerm env }
  evalModExp env' $ case ret of
    Nothing -> e
    Just (se, rsubsts) -> ModAscript e se rsubsts loc

evalModExp env (ModApply f e (Info psubst) (Info rsubst) _) = do
  ModuleFun f' <- evalModExp env f
  e' <- evalModExp env e
  substituteInModule rsubst <$> f' (substituteInModule psubst e')

evalDec :: Env -> Dec -> EvalM Env

evalDec env (ValDec (ValBind _ v _ (Info t) tps ps def _ loc)) = do
  t' <- evalType env t
  let ftype = T.BoundV tps $ foldr (uncurry (Arrow ()) . patternParam) t' ps
  val <- eval env $ Lambda tps ps def Nothing (Info (mempty, t')) loc
  return $ valEnv (M.singleton v (Just ftype, val)) <> env

evalDec env (OpenDec me (Info _) _) = do
  Module me' <- evalModExp env me
  return $ me' <> env

evalDec env (LocalDec d _) = evalDec env d
evalDec env SigDec{} = return env
evalDec env (TypeDec (TypeBind v ps t _ _)) = do
  t' <- evalType env $ unInfo $ expandedType t
  let abbr = T.TypeAbbr Lifted ps t'
  return env { envType = M.insert v abbr $ envType env }
evalDec env (ModDec (ModBind v ps ret body _ loc)) = do
  mod <- evalModExp env $ wrapInLambda ps
  return $ modEnv (M.singleton v mod) <> env
  where wrapInLambda [] = case ret of
                            Just (se, substs) -> ModAscript body se substs loc
                            Nothing           -> body
        wrapInLambda [p] = ModLambda p ret body loc
        wrapInLambda (p:ps') = ModLambda p Nothing (wrapInLambda ps') loc

data Ctx = Ctx { ctxEnv :: Env
               , ctxImports :: M.Map FilePath Env
               }

-- | The initial environment contains definitions of the various intrinsic functions.
initialCtx :: Ctx
initialCtx =
  Ctx (Env (M.insert (VName (nameFromString "intrinsics") 0)
            (TermModule (Module $ Env terms types)) terms)
        types)
      mempty
  where
    terms = M.mapMaybeWithKey (const . def . baseString) intrinsics
    types = M.mapMaybeWithKey (const . tdef . baseString) intrinsics

    sintOp f = [ (getS, putS, P.doBinOp (f Int8))
               , (getS, putS, P.doBinOp (f Int16))
               , (getS, putS, P.doBinOp (f Int32))
               , (getS, putS, P.doBinOp (f Int64))]
    uintOp f = [ (getU, putU, P.doBinOp (f Int8))
               , (getU, putU, P.doBinOp (f Int16))
               , (getU, putU, P.doBinOp (f Int32))
               , (getU, putU, P.doBinOp (f Int64))]
    intOp f = sintOp f ++ uintOp f
    floatOp f = [ (getF, putF, P.doBinOp (f Float32))
                , (getF, putF, P.doBinOp (f Float64))]
    arithOp f g = Just $ bopDef $ intOp f ++ floatOp g

    flipCmps = map (\(f, g, h) -> (f, g, flip h))
    sintCmp f = [ (getS, Just . BoolValue, P.doCmpOp (f Int8))
                , (getS, Just . BoolValue, P.doCmpOp (f Int16))
                , (getS, Just . BoolValue, P.doCmpOp (f Int32))
                , (getS, Just . BoolValue, P.doCmpOp (f Int64))]
    uintCmp f = [ (getU, Just . BoolValue, P.doCmpOp (f Int8))
                , (getU, Just . BoolValue, P.doCmpOp (f Int16))
                , (getU, Just . BoolValue, P.doCmpOp (f Int32))
                , (getU, Just . BoolValue, P.doCmpOp (f Int64))]
    floatCmp f = [ (getF, Just . BoolValue, P.doCmpOp (f Float32))
                 , (getF, Just . BoolValue, P.doCmpOp (f Float64))]
    boolCmp f = [ (getB, Just . BoolValue, P.doCmpOp f) ]

    getV (SignedValue x) = Just $ P.IntValue x
    getV (UnsignedValue x) = Just $ P.IntValue x
    getV (FloatValue x) = Just $ P.FloatValue x
    getV (BoolValue x) = Just $ P.BoolValue x
    putV (P.IntValue x) = SignedValue x
    putV (P.FloatValue x) = FloatValue x
    putV (P.BoolValue x) = BoolValue x
    putV P.Checked = BoolValue True

    getS (SignedValue x) = Just $ P.IntValue x
    getS _               = Nothing
    putS (P.IntValue x) = Just $ SignedValue x
    putS _              = Nothing

    getU (UnsignedValue x) = Just $ P.IntValue x
    getU _                 = Nothing
    putU (P.IntValue x) = Just $ UnsignedValue x
    putU _              = Nothing

    getF (FloatValue x) = Just $ P.FloatValue x
    getF _              = Nothing
    putF (P.FloatValue x) = Just $ FloatValue x
    putF _                = Nothing

    getB (BoolValue x) = Just $ P.BoolValue x
    getB _             = Nothing

    fun1 f =
      TermValue Nothing $ ValueFun $ \x -> f x
    fun2 f =
      TermValue Nothing $ ValueFun $ \x -> return $ ValueFun $ \y -> f x y
    fun2t f =
      TermValue Nothing $ ValueFun $ \v ->
      case fromTuple v of Just [x,y] -> f x y
                          _ -> error $ "Expected pair; got: " ++ pretty v
    fun3t f =
      TermValue Nothing $ ValueFun $ \v ->
      case fromTuple v of Just [x,y,z] -> f x y z
                          _ -> error $ "Expected triple; got: " ++ pretty v

    fun5t f =
      TermValue Nothing $ ValueFun $ \v ->
      case fromTuple v of Just [x,y,z,a,b] -> f x y z a b
                          _ -> error $ "Expected quintuple; got: " ++ pretty v

    bopDef fs = fun2 $ \x y ->
      case (x, y) of
        (ValuePrim x', ValuePrim y')
          | Just z <- msum $ map (`bopDef'` (x', y')) fs ->
              return $ ValuePrim z
        _ ->
          bad noLoc mempty $ "Cannot apply operator to arguments `" <>
          pretty x <> "` and `" <> pretty y <> "`."
      where bopDef' (valf, retf, op) (x, y) = do
              x' <- valf x
              y' <- valf y
              retf =<< op x' y'

    unopDef fs = fun1 $ \x ->
      case x of
        (ValuePrim x')
          | Just r <- msum $ map (`unopDef'` x') fs ->
              return $ ValuePrim r
        _ ->
          bad noLoc mempty $ "Cannot apply function to argument `" <>
          pretty x <> "`."
      where unopDef' (valf, retf, op) x = do
              x' <- valf x
              retf =<< op x'

    def "~" = Just $ unopDef [ (getS, putS, P.doUnOp $ P.Complement Int8)
                             , (getS, putS, P.doUnOp $ P.Complement Int16)
                             , (getS, putS, P.doUnOp $ P.Complement Int32)
                             , (getS, putS, P.doUnOp $ P.Complement Int64)
                             , (getU, putU, P.doUnOp $ P.Complement Int8)
                             , (getU, putU, P.doUnOp $ P.Complement Int16)
                             , (getU, putU, P.doUnOp $ P.Complement Int32)
                             , (getU, putU, P.doUnOp $ P.Complement Int64)]
    def "!" = Just $ fun1 $ return . ValuePrim . BoolValue . not . asBool

    def "+" = arithOp P.Add P.FAdd
    def "-" = arithOp P.Sub P.FSub
    def "*" = arithOp P.Mul P.FMul
    def "**" = arithOp P.Pow P.FPow
    def "/" = Just $ bopDef $ sintOp P.SDiv ++ uintOp P.UDiv ++ floatOp P.FDiv
    def "%" = Just $ bopDef $ sintOp P.SMod ++ uintOp P.UMod
    def "//" = Just $ bopDef $ sintOp P.SQuot ++ uintOp P.UDiv
    def "%%" = Just $ bopDef $ sintOp P.SRem ++ uintOp P.UMod
    def "^" = Just $ bopDef $ intOp P.Xor
    def "&" = Just $ bopDef $ intOp P.And
    def "|" = Just $ bopDef $ intOp P.Or
    def ">>" = Just $ bopDef $ sintOp P.AShr ++ uintOp P.LShr
    def "<<" = Just $ bopDef $ intOp P.Shl
    def ">>>" = Just $ bopDef $ sintOp P.LShr ++ uintOp P.LShr
    def "==" = Just $ fun2 $ \xs ys -> return $ ValuePrim $ BoolValue $ xs == ys
    def "!=" = Just $ fun2 $ \xs ys -> return $ ValuePrim $ BoolValue $ xs /= ys

    -- The short-circuiting is handled directly in 'eval'; these cases
    -- are only used when partially applying and such.
    def "&&" = Just $ fun2 $ \x y ->
      return $ ValuePrim $ BoolValue $ asBool x && asBool y
    def "||" = Just $ fun2 $ \x y ->
      return $ ValuePrim $ BoolValue $ asBool x || asBool y

    def "<" = Just $ bopDef $
              sintCmp P.CmpSlt ++ uintCmp P.CmpUlt ++
              floatCmp P.FCmpLt ++ boolCmp P.CmpLlt
    def ">" = Just $ bopDef $ flipCmps $
              sintCmp P.CmpSlt ++ uintCmp P.CmpUlt ++
              floatCmp P.FCmpLt ++ boolCmp P.CmpLlt
    def "<=" = Just $ bopDef $
               sintCmp P.CmpSle ++ uintCmp P.CmpUle ++
               floatCmp P.FCmpLe ++ boolCmp P.CmpLle
    def ">=" = Just $ bopDef $ flipCmps $
               sintCmp P.CmpSle ++ uintCmp P.CmpUle ++
               floatCmp P.FCmpLe ++ boolCmp P.CmpLle

    def s
      | Just bop <- find ((s==) . pretty) P.allBinOps =
          Just $ bopDef [(getV, Just . putV, P.doBinOp bop)]
      | Just cop <- find ((s==) . pretty) P.allConvOps =
          Just $ unopDef [(getV, Just . putV, P.doConvOp cop)]
      | Just unop <- find ((s==) . pretty) P.allUnOps =
          Just $ unopDef [(getV, Just . putV, P.doUnOp unop)]
      | Just unop <- find ((s==) . pretty) P.allCmpOps =
          Just $ bopDef [(getV, bool, P.doCmpOp unop)]

      | Just (pts, _, f) <- M.lookup s P.primFuns =
          case length pts of
            1 -> Just $ unopDef [(getV, Just . putV, f . pure)]
            _ -> Just $ bopDef [(getV, Just . putV, \x y -> f [x,y])]

      | "sign_" `isPrefixOf` s =
          Just $ fun1 $ \x ->
          case x of (ValuePrim (UnsignedValue x')) ->
                      return $ ValuePrim $ SignedValue x'
                    _ -> error $ "Cannot sign: " ++ pretty x
      | "unsign_" `isPrefixOf` s =
          Just $ fun1 $ \x ->
          case x of (ValuePrim (SignedValue x')) ->
                      return $ ValuePrim $ UnsignedValue x'
                    _ -> error $ "Cannot unsign: " ++ pretty x
      where bool = Just . BoolValue

    def "map" = Just $ fun2t $ \f xs ->
      toArray =<< mapM (apply noLoc mempty f) (fromArray xs)

    def s | "reduce" `isPrefixOf` s = Just $ fun3t $ \f ne xs ->
      foldM (apply2 noLoc mempty f) ne $ fromArray xs

    def "scan" = Just $ fun3t $ \f ne xs -> do
      let next (out, acc) x = do
            x' <- apply2 noLoc mempty f acc x
            return (x':out, x')
      toArray . reverse . fst =<< foldM next ([], ne) (fromArray xs)

    def s | "stream_map" `isPrefixOf` s =
              Just $ fun2t $ apply noLoc mempty

    def s | "stream_red" `isPrefixOf` s =
              Just $ fun3t $ \_ f xs -> apply noLoc mempty f xs

    def "scatter" = Just $ fun3t $ \arr is vs ->
      case arr of
        ValueArray arr' ->
          return $ ValueArray $ foldl' update arr'
          (zip (map asInt $ fromArray is) (fromArray vs))
        _ ->
          error $ "scatter expects array, but got: " ++ pretty arr
      where update arr' (i, v) =
              if i >= 0 && i < arrayLength arr'
              then arr' // [(i, v)] else arr'

    def "gen_reduce" = Just $ fun5t $ \arr fun _ is vs ->
      case arr of
        ValueArray arr' ->
          ValueArray <$> foldM (update fun) arr'
          (zip (map asInt $ fromArray is) (fromArray vs))
        _ ->
          error $ "gen_reduce expects array, but got: " ++ pretty arr
      where update fun arr' (i, v) =
              if i >= 0 && i < arrayLength arr'
              then do
                v' <- apply2 noLoc mempty fun (arr' ! i) v
                return $ arr' // [(i, v')]
              else return arr'

    def "partition" = Just $ fun3t $ \k f xs ->
      let next outs x = do
            i <- asInt <$> apply noLoc mempty f x
            return $ insertAt i x outs
          pack parts =
            toTuple [toArray' $ concat parts,
                     toArray' $
                     map (ValuePrim . SignedValue . Int32Value . genericLength) parts]
      in pack . map reverse <$>
         foldM next (replicate (asInt k) []) (fromArray xs)
      where insertAt 0 x (l:ls) = (x:l):ls
            insertAt i x (l:ls) = l:insertAt (i-1) x ls
            insertAt _ _ ls = ls

    def "cmp_threshold" = Just $ fun2t $ \_ _ ->
      return $ ValuePrim $ BoolValue True

    def "unzip" = Just $ fun1 $ \x ->
      toTuple <$> listPair (unzip $ map (fromPair . fromTuple) $ fromArray x)
      where fromPair (Just [x,y]) = (x,y)
            fromPair l = error $ "Not a pair: " ++ pretty l
            listPair (xs, ys) = do
              xs' <- toArray xs
              ys' <- toArray ys
              return [xs', ys']

    def "zip" = Just $ fun2t $ \xs ys ->
      toArray $ map toTuple $ transpose [fromArray xs, fromArray ys]

    def "concat" = Just $ fun2t $ \xs ys ->
      toArray $ fromArray xs ++ fromArray ys

    def "transpose" = Just $ fun1 $
      (toArray <=< mapM toArray) . transpose . map fromArray . fromArray

    def "rotate" = Just $ fun2t $ \i xs ->
      if asInt i > 0
      then let (bef, aft) = splitAt (asInt i) $ fromArray xs
           in toArray $ aft ++ bef
      else let (bef, aft) = splitFromEnd (-asInt i) $ fromArray xs
           in toArray $ aft ++ bef

    def "flatten" = Just $ fun1 $
      toArray . concatMap fromArray . fromArray

    def "unflatten" = Just $ fun3t $ \_ m xs ->
      toArray =<< mapM toArray (chunk (asInt m) $ fromArray xs)

    def "opaque" = Just $ fun1 return

    def "trace" = Just $ fun1 $ \v -> trace v >> return v

    def "break" = Just $ fun1 $ \v -> do
      break
      return v

    def s | nameFromString s `M.member` namesToPrimTypes = Nothing

    def s = error $ "Missing intrinsic: " ++ s

    tdef s = do
      t <- nameFromString s `M.lookup` namesToPrimTypes
      return $ T.TypeAbbr Unlifted [] $ Prim t

interpretExp :: Ctx -> Exp -> F ExtOp Value
interpretExp ctx e = runEvalM (ctxImports ctx) $ eval (ctxEnv ctx) e

interpretDec :: Ctx -> Dec -> F ExtOp Ctx
interpretDec ctx d = do
  env <- runEvalM (ctxImports ctx) $ evalDec (ctxEnv ctx) d
  return ctx { ctxEnv = env }

interpretImport :: Ctx -> (FilePath, Prog) -> F ExtOp Ctx
interpretImport ctx (fp, prog) = do
  env <- runEvalM (ctxImports ctx) $ foldM evalDec (ctxEnv ctx) $ progDecs prog
  return ctx { ctxImports = M.insert fp env $ ctxImports ctx }

-- | Execute the named function on the given arguments; will fail
-- horribly if these are ill-typed.
interpretFunction :: Ctx -> VName -> [Value] -> F ExtOp Value
interpretFunction ctx fname vs = runEvalM (ctxImports ctx) $ do
  f <- evalTermVar (ctxEnv ctx) $ qualName fname
  foldM (apply noLoc mempty) f vs
