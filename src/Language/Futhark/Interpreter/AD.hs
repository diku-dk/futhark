module Language.Futhark.Interpreter.AD
  ( Op (..),
    ADVariable (..),
    ADValue (..),
    Tape (..),
    VJPValue (..),
    JVPValue (..),
    Counter (..),
    Depth (..),
    doOp,
    addFor,
    tapePrimal,
    primitive,
    varPrimal,
    deriveTape,
    unionWithM,
    unionsWithM,
  )
where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, modify, runState)
import Data.Either (isRight)
import Data.Foldable (find, foldlM)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Futhark.AD.Derivatives (pdBinOp, pdBuiltin, pdUnOp)
import Futhark.Analysis.PrimExp (PrimExp (..))
import Language.Futhark.Core (VName (..), nameFromString, nameFromText)
import Language.Futhark.Primitive
  ( BinOp (Add, FAdd, FMul, LogAnd, LogOr, Mul),
    CmpOp,
    ConvOp,
    Overflow (OverflowWrap),
    PrimType (Bool, FloatType, IntType),
    PrimValue (BoolValue),
    UnOp,
    binOpType,
    blankPrimValue,
    cmpOpType,
    convOpType,
    doBinOp,
    doCmpOp,
    doConvOp,
    doUnOp,
    flipConvOp,
    primFuns,
    primValueType,
    unOpType,
  )

-- | Used to uniquely identify values.
newtype Counter = Counter Int
  deriving (Eq, Ord, Num, Show)

type ADMonad = ExceptT String (State Counter)

incCounter :: ADMonad ()
incCounter = lift $ modify $ \i -> i + 1

-- Mathematical operations subject to AD.
data Op
  = OpBin BinOp
  | OpCmp CmpOp
  | OpUn UnOp
  | OpFn T.Text
  | OpConv ConvOp
  deriving (Show)

-- Checks if an operation matches the types of its operands
opTypeMatch :: Op -> [PrimType] -> Bool
opTypeMatch (OpBin op) p = all (\x -> binOpType op == x) p
opTypeMatch (OpCmp op) p = all (\x -> cmpOpType op == x) p
opTypeMatch (OpUn op) p = all (\x -> unOpType op == x) p
opTypeMatch (OpConv op) p = all (\x -> fst (convOpType op) == x) p
opTypeMatch (OpFn fn) p = case M.lookup fn primFuns of
  Just (t, _, _) -> and $ zipWith (==) t p
  Nothing -> error "opTypeMatch" -- It is assumed that the function exists

-- Gets the return type of an operation
opReturnType :: Op -> PrimType
opReturnType (OpBin op) = binOpType op
opReturnType (OpCmp op) = cmpOpType op
opReturnType (OpUn op) = unOpType op
opReturnType (OpConv op) = snd $ convOpType op
opReturnType (OpFn fn) = case M.lookup fn primFuns of
  Just (_, t, _) -> t
  Nothing -> error "opReturnType" -- It is assumed that the function exists

-- Returns the operation which performs addition (or an
-- equivalent operation) on the given type
addFor :: PrimType -> BinOp
addFor (IntType t) = Add t OverflowWrap
addFor (FloatType t) = FAdd t
addFor Bool = LogOr
addFor t = error $ "addFor: " ++ show t

-- Returns the function which performs multiplication
-- (or an equivalent operation) on the given type
mulFor :: PrimType -> BinOp
mulFor (IntType t) = Mul t OverflowWrap
mulFor (FloatType t) = FMul t
mulFor Bool = LogAnd
mulFor t = error $ "mulFor: " ++ show t

-- | An indication of the nesting depth of AD. This is used to avoid
-- pertubation confusion.
newtype Depth = Depth Int
  deriving (Ord, Eq, Show)

-- Types and utility functions--
-- When taking the partial derivative of a function, we
-- must differentiate between the values which are kept
-- constant, and those which are not
data ADValue
  = Variable Depth ADVariable
  | Constant PrimValue
  deriving (Show)

-- When performing automatic differentiation, each derived
-- variable must be augmented with additional data. This
-- value holds the primitive value of the variable, as well
-- as its data
data ADVariable
  = VJP VJPValue
  | JVP JVPValue
  deriving (Show)

depth :: ADValue -> Depth
depth (Variable d _) = d
depth (Constant _) = Depth 0

primal :: ADValue -> ADValue
primal (Variable _ (VJP (VJPValue t))) = tapePrimal t
primal (Variable _ (JVP (JVPValue v _))) = primal v
primal (Constant v) = Constant v

primalFor :: Depth -> ADValue -> ADValue
primalFor cur v@(Variable tag _) | cur /= tag = v
primalFor _ (Variable _ (VJP (VJPValue t))) = tapePrimal t
primalFor cur (Variable _ (JVP (JVPValue v _))) = primalFor cur v
primalFor _ (Constant v) = Constant v

primitive :: ADValue -> PrimValue
primitive (Variable _ v) = varPrimal v
primitive (Constant v) = v

varPrimal :: ADVariable -> PrimValue
varPrimal (VJP (VJPValue t)) = primitive $ tapePrimal t
varPrimal (JVP (JVPValue v _)) = primitive $ primal v

-- Evaluates a PrimExp using doOp'
evalPrimExp :: M.Map VName ADValue -> PrimExp VName -> ADMonad ADValue
evalPrimExp m (LeafExp n _) =
  maybe (throwE $ "Unknown variable " <> show n) pure $ M.lookup n m
evalPrimExp _ (ValueExp pv) =
  pure $ Constant pv
evalPrimExp m (BinOpExp op x y) = do
  x' <- evalPrimExp m x
  y' <- evalPrimExp m y
  doOp' (OpBin op) [x', y']
evalPrimExp m (CmpOpExp op x y) = do
  x' <- evalPrimExp m x
  y' <- evalPrimExp m y
  doOp' (OpCmp op) [x', y']
evalPrimExp m (UnOpExp op x) = do
  x' <- evalPrimExp m x
  doOp' (OpUn op) [x']
evalPrimExp m (ConvOpExp op x) = do
  x' <- evalPrimExp m x
  doOp' (OpConv op) [x']
evalPrimExp m (FunExp fn p _) = do
  p' <- mapM (evalPrimExp m) p
  doOp' (OpFn fn) p'

-- Returns a list of PrimExps calculating the partial
-- derivative of each operands of a given operation
lookupPDs :: Op -> [PrimExp VName] -> Maybe [PrimExp VName]
lookupPDs (OpBin op) [x, y] = Just $ do
  let (a, b) = pdBinOp op x y
  [a, b]
lookupPDs (OpUn op) [x] = Just [pdUnOp op x]
lookupPDs (OpFn fn) p = pdBuiltin (nameFromText fn) p
lookupPDs _ _ = Nothing

-- Shared AD logic--
-- This function performs a mathematical operation on a
-- list of operands, performing automatic differentiation
-- if one or more operands is a Variable (of depth > 0)
doOp :: Op -> [ADValue] -> Counter -> Either String (ADValue, Counter)
doOp op o uid = case runState (runExceptT $ doOp' op o) uid of
  (Left s, _) -> Left s
  (Right v, uid') -> Right (v, uid')

doOp' :: Op -> [ADValue] -> ADMonad ADValue
doOp' op o
  | not $ opTypeMatch op (map primValueType pv) =
      -- This function may be called with arguments of invalid types,
      -- because it is used as part of an overloaded operator.
      throwE $ unwords ["invalid types for op", show op, "and operands", show o]
  | otherwise = do
      let dep = case op of
            OpCmp _ -> Depth 0 -- AD is not well-defined for comparason operations
            -- There are no derivatives for those written in
            -- PrimExp (check lookupPDs)
            _ -> maximum (map depth o)
      if dep == Depth 0
        then maybe (throwE "failed to evaluate const") pure constCase <* incCounter
        else nonconstCase dep
  where
    pv = map primitive o

    divideDepths :: Depth -> ADValue -> Either ADValue ADVariable
    divideDepths _ v@(Constant {}) = Left v
    divideDepths d v@(Variable d' v') = if d' < d then Left v else Right v'

    -- TODO: There may be a more graceful way of
    -- doing this
    extractVJP :: Either ADValue ADVariable -> Either ADValue VJPValue
    extractVJP (Right (VJP v)) = Right v
    extractVJP (Left v) = Left v
    extractVJP _ =
      -- This will never be called when the maximum depth layer is JVP
      error "extractVJP"

    -- TODO: There may be a more graceful way of
    -- doing this
    extractJVP :: Either ADValue ADVariable -> Either ADValue JVPValue
    extractJVP (Right (JVP v)) = Right v
    extractJVP (Left v) = Left v
    extractJVP _ =
      -- This will never be called when the maximum depth layer is VJP
      error "extractJVP"

    -- In this case, every operand is a constant, and the
    -- mathematical operation can be applied as it would be
    -- otherwise
    constCase =
      Constant <$> case (op, pv) of
        (OpBin op', [x, y]) -> doBinOp op' x y
        (OpCmp op', [x, y]) -> BoolValue <$> doCmpOp op' x y
        (OpUn op', [x]) -> doUnOp op' x
        (OpConv op', [x]) -> doConvOp op' x
        (OpFn fn, _) -> do
          (_, _, f) <- M.lookup fn primFuns
          f pv
        _ -> error "doOp': opTypeMatch"

    nonconstCase dep = do
      -- In this case, some values are variables. We therefore
      -- have to perform the necessary steps for AD

      -- First, we calculate the value for the previous depth
      let oprev = map (primalFor dep) o
      vprev <- doOp' op oprev

      -- Then we separate the values of the maximum depth from
      -- those of a lower depth
      let o' = map (divideDepths dep) o
      -- Then we find out what type of AD is being performed
      case find isRight o' of
        -- Finally, we perform the necessary steps for the given
        -- type of AD
        Just (Right (VJP {})) ->
          Variable dep . VJP . VJPValue
            <$> vjpHandleOp op (map extractVJP o') vprev
        Just (Right (JVP {})) ->
          Variable dep . JVP . JVPValue vprev
            <$> jvpHandleOp op (map extractJVP o')
        _ ->
          -- Since the maximum depth is non-zero, there must be at
          -- least one variable of depth > 0
          error "find isRight"

calculatePDs :: Op -> [ADValue] -> ADMonad [ADValue]
calculatePDs op args =
  -- Create a unique VName for each operand
  let n = map (\i -> VName (nameFromString $ "x" ++ show i) i) [1 .. length args]
      -- Put the operands in the environment
      m = M.fromList $ zip n args

      -- Look up, and calculate the partial derivative
      -- of the operation with respect to each operand
      pde =
        fromMaybe (error "lookupPDs failed") $
          lookupPDs op $
            zipWith (\v val -> LeafExp v $ primValueType $ primitive val) n args
      res = mapM (\x -> catchE (evalPrimExp m x) $ error . ("evalPrimExp failed: " <>)) pde
   in res

-- VJP / Reverse mode automatic differentiation--
-- In reverse mode AD, the entire computation
-- leading up to a variable must be saved
-- This is represented as a Tape
newtype VJPValue = VJPValue Tape
  deriving (Show)

-- | Represents a computation tree, as well as every intermediate
-- value in its evaluation.
data Tape
  = -- | This represents a variable. Each variable is given a unique ID,
    -- and has an initial value
    TapeID Counter ADValue
  | -- | This represents a constant.
    TapeConst ADValue
  | -- | This represents the application of a mathematical operation.
    -- Each parameter is given by its Tape, and the return value of
    -- the operation is saved
    TapeOp Op [Tape] Counter ADValue
  deriving (Show)

-- | Returns the primal value of a Tape.
tapePrimal :: Tape -> ADValue
tapePrimal (TapeID _ v) = v
tapePrimal (TapeConst v) = v
tapePrimal (TapeOp _ _ _ v) = v

-- This updates Tape of a VJPValue with a new operation,
-- treating all operands of a lower depth as constants
vjpHandleOp :: Op -> [Either ADValue VJPValue] -> ADValue -> ADMonad Tape
vjpHandleOp op p v = do
  i <- lift get
  pure $ TapeOp op (map toTape p) i v
  where
    toTape (Left v') = TapeConst v'
    toTape (Right (VJPValue t)) = t

unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f m1 m2 = do
  let m = M.union (M.difference m1 m2) (M.difference m2 m1)
  let k = M.keys $ M.intersection m1 m2
  v <- mapM (\k' -> f (fromJust $ M.lookup k' m1) (fromJust $ M.lookup k' m2)) k
  pure $ foldl (\m' (k', v') -> M.insert k' v' m') m (zip k v)

unionsWithM :: (Foldable f, Monad m, Ord k) => (a -> a -> m a) -> f (M.Map k a) -> m (M.Map k a)
unionsWithM f = foldM (unionWithM f) M.empty

-- | This calculates every partial derivative of a 'Tape'. The result
-- is a map of the partial derivatives, each key corresponding to the
-- ID of a free variable (see TapeID).
deriveTape :: Tape -> ADValue -> Counter -> Either String (M.Map Counter ADValue, Counter)
deriveTape tp s uid = case runState (runExceptT $ deriveTape' tp s) uid of
  (Left e, _) -> Left e
  (Right v, uid') -> Right (v, uid')

deriveTape' :: Tape -> ADValue -> ADMonad (M.Map Counter ADValue)
deriveTape' (TapeID i _) s = pure $ M.singleton i s
deriveTape' (TapeConst _) _ = pure M.empty
deriveTape' tp@(TapeOp op p uid _) s =
  fst <$> derive tp s M.empty (countReferences p $ M.singleton (-uid - 1) 1)
  where
    add x y = doOp' (OpBin $ addFor $ opReturnType op) [x, y]
    mul x y = doOp' (OpBin $ mulFor $ opReturnType op) [x, y]
    madd :: Counter -> ADValue -> M.Map Counter ADValue -> ADMonad (M.Map Counter ADValue)
    madd i a m = case M.lookup i m of
      Just b -> add a b <&> (\x -> M.insert i x m)
      Nothing -> pure $ M.insert i a m
    derive ::
      Tape ->
      ADValue ->
      M.Map Counter ADValue ->
      M.Map Counter Int ->
      ADMonad (M.Map Counter ADValue, M.Map Counter Int)
    derive (TapeID i _) s' ss rs = madd i s' ss <&> (,rs)
    derive (TapeConst _) _ ss rs = pure (ss, rs)
    derive (TapeOp op' p' uid' _) s' ss rs = do
      -- Decrease the reference counter
      let r = fromJust (M.lookup (-uid' - 1) rs) - 1
          rs' = M.insert (-uid' - 1) r rs
      -- Add the sensitivity
      ss' <- madd (-uid' - 1) s' ss
      -- If there are still more references left, do nothing
      if r > 0
        then pure (ss', rs')
        else -- Otherwise, derive the tape
          if r == 0
            then do
              let s'' = fromJust (M.lookup (-uid' - 1) ss')

              -- Calculate the new sensitivities
              s''' <- case op' of
                OpConv op'' ->
                  -- In case of type conversion, simply convert the sensitivity
                  sequence [doOp' (OpConv $ flipConvOp op'') [s'']]
                _ -> calculatePDs op' (map tapePrimal p') >>= mapM (mul s'')

              -- Propagate the new sensitivities
              foldlM (\(ss'', rs'') (p'', s'''') -> derive p'' s'''' ss'' rs'') (ss', rs') $ zip p' s'''
            else error "TODO: This branch is unreachable unless `countReferences` undercounts"
    countReferences :: [Tape] -> M.Map Counter Int -> M.Map Counter Int
    countReferences p' d' = foldl f d' p'
    f d'' x =
      case x of
        (TapeOp _ p'' uid'' _) -> case M.lookup (-uid'' - 1) d'' of
          Just v -> M.insert (-uid'' - 1) (v + 1) d''
          Nothing -> countReferences p'' $ M.insert (-uid'' - 1) 1 d''
        _ -> d''

-- JVP / Forward mode automatic differentiation--

-- | In JVP, the derivative of the variable must be saved. This is
-- represented as a second value.
data JVPValue = JVPValue ADValue ADValue
  deriving (Show)

-- | This calculates the tangent part of the JVPValue resulting
-- from the application of a mathematical operation on one or more
-- JVPValues.
jvpHandleOp :: Op -> [Either ADValue JVPValue] -> ADMonad ADValue
jvpHandleOp op p = do
  case op of
    OpConv _ ->
      -- In case of type conversion, simply convert
      -- the old tangent
      doOp' op [tangent $ head p]
    _ -> do
      -- Calculate the new tangent using the chain rule
      pds <- calculatePDs op $ map primal' p
      vs <- zipWithM mul pds $ map tangent p
      foldM add (Constant $ blankPrimValue op_t) vs
  where
    op_t = opReturnType op
    primal' (Left v) = v
    primal' (Right (JVPValue v _)) = v
    tangent (Left _) = Constant $ blankPrimValue $ opReturnType op
    tangent (Right (JVPValue _ d)) = d
    add x y = doOp' (OpBin $ addFor $ opReturnType op) [x, y]
    mul x y = doOp' (OpBin $ mulFor $ opReturnType op) [x, y]
