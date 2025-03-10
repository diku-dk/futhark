module Language.Futhark.Interpreter.AD
  ( Op (..),
    ADVariable (..),
    ADValue (..),
    Tape (..),
    VJPValue (..),
    JVPValue (..),
    doOp,
    addFor,
    tapePrimal,
    primitive,
    varPrimal,
    deriveTape,
  )
where

import Control.Monad (foldM, zipWithM)
import Data.Either (isRight)
import Data.List (find, foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Futhark.AD.Derivatives (pdBinOp, pdBuiltin, pdUnOp)
import Futhark.Analysis.PrimExp (PrimExp (..))
import Language.Futhark.Core (VName (..), nameFromString, nameFromText)
import Language.Futhark.Primitive

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

-- Types and utility functions--
-- When taking the partial derivative of a function, we
-- must differentiate between the values which are kept
-- constant, and those which are not
data ADValue
  = Variable Int ADVariable
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

depth :: ADValue -> Int
depth (Variable d _) = d
depth (Constant _) = 0

primal :: ADValue -> ADValue
primal (Variable _ (VJP (VJPValue t))) = tapePrimal t
primal (Variable _ (JVP (JVPValue v _))) = primal v
primal (Constant v) = Constant v

primitive :: ADValue -> PrimValue
primitive (Variable _ v) = varPrimal v
primitive (Constant v) = v

varPrimal :: ADVariable -> PrimValue
varPrimal (VJP (VJPValue t)) = primitive $ tapePrimal t
varPrimal (JVP (JVPValue v _)) = primitive $ primal v

-- Evaluates a PrimExp using doOp
evalPrimExp :: M.Map VName ADValue -> PrimExp VName -> Maybe ADValue
evalPrimExp m (LeafExp n _) = M.lookup n m
evalPrimExp _ (ValueExp pv) = Just $ Constant pv
evalPrimExp m (BinOpExp op x y) = do
  x' <- evalPrimExp m x
  y' <- evalPrimExp m y
  doOp (OpBin op) [x', y']
evalPrimExp m (CmpOpExp op x y) = do
  x' <- evalPrimExp m x
  y' <- evalPrimExp m y
  doOp (OpCmp op) [x', y']
evalPrimExp m (UnOpExp op x) = do
  x' <- evalPrimExp m x
  doOp (OpUn op) [x']
evalPrimExp m (ConvOpExp op x) = do
  x' <- evalPrimExp m x
  doOp (OpConv op) [x']
evalPrimExp m (FunExp fn p _) = do
  p' <- mapM (evalPrimExp m) p
  doOp (OpFn fn) p'

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
doOp :: Op -> [ADValue] -> Maybe ADValue
doOp op o
  | not $ opTypeMatch op (map primValueType pv) =
      -- This function may be called with arguments of invalid types,
      -- because it is used as part of an overloaded operator.
      Nothing
  | otherwise = do
      let dep = case op of
            OpCmp _ -> 0 -- AD is not well-defined for comparason operations
            -- There are no derivatives for those written in
            -- PrimExp (check lookupPDs)
            _ -> maximum (map depth o)
      if dep == 0 then constCase else nonconstCase dep
  where
    pv = map primitive o

    divideDepths :: Int -> ADValue -> Either ADValue ADVariable
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
        _ -> error "doOp: opTypeMatch"

    nonconstCase dep = do
      -- In this case, some values are variables. We therefore
      -- have to perform the necessary steps for AD

      -- First, we calculate the value for the previous depth
      let oprev = map primal o
      vprev <- doOp op oprev

      -- Then we separate the values of the maximum depth from
      -- those of a lower depth
      let o' = map (divideDepths dep) o
      -- Then we find out what type of AD is being performed
      case find isRight o' of
        -- Finally, we perform the necessary steps for the given
        -- type of AD
        Just (Right (VJP {})) ->
          Just . Variable dep . VJP . VJPValue $
            vjpHandleOp op (map extractVJP o') vprev
        Just (Right (JVP {})) ->
          Variable dep . JVP . JVPValue vprev
            <$> jvpHandleFn op (map extractJVP o')
        _ ->
          -- Since the maximum depth is non-zero, there must be at
          -- least one variable of depth > 0
          error "find isRight"

calculatePDs :: Op -> [ADValue] -> [ADValue]
calculatePDs op p =
  -- Create a unique VName for each operand
  let n = map (\i -> VName (nameFromString $ "x" ++ show i) i) [1 .. length p]
      -- Put the operands in the environment
      m = M.fromList $ zip n p

      -- Look up, and calculate the partial derivative
      -- of the operation with respect to each operand
      pde =
        fromMaybe (error "lookupPDs failed") $
          lookupPDs op $
            map (`LeafExp` opReturnType op) n
   in map (fromMaybe (error "evalPrimExp failed") . evalPrimExp m) pde

-- VJP / Reverse mode automatic differentiation--
-- In reverse mode AD, the entire computation
-- leading up to a variable must be saved
-- This is represented as a Tape
newtype VJPValue = VJPValue Tape
  deriving (Show)

-- | Represents a computation tree, as well as every intermediate
-- value in its evaluation. TODO: make this a graph.
data Tape
  = -- | This represents a variable. Each variable is given a unique ID,
    -- and has an initial value
    TapeID Int ADValue
  | -- | This represents a constant.
    TapeConst ADValue
  | -- | This represents the application of a mathematical operation.
    -- Each parameter is given by its Tape, and the return value of
    -- the operation is saved
    TapeOp Op [Tape] ADValue
  deriving (Show)

-- | Returns the primal value of a Tape.
tapePrimal :: Tape -> ADValue
tapePrimal (TapeID _ v) = v
tapePrimal (TapeConst v) = v
tapePrimal (TapeOp _ _ v) = v

-- This updates Tape of a VJPValue with a new operation,
-- treating all operands of a lower depth as constants
vjpHandleOp :: Op -> [Either ADValue VJPValue] -> ADValue -> Tape
vjpHandleOp op p v = do
  TapeOp op (map toTape p) v
  where
    toTape (Left v') = TapeConst v'
    toTape (Right (VJPValue t)) = t

-- | This calculates every partial derivative of a 'Tape'. The result
-- is a map of the partial derivatives, each key corresponding to the
-- ID of a free variable (see TapeID).
deriveTape :: Tape -> ADValue -> M.Map Int ADValue
deriveTape (TapeID i _) s = M.fromList [(i, s)]
deriveTape (TapeConst _) _ = M.empty
deriveTape (TapeOp op p _) s =
  -- Calculate the new sensitivities
  let s'' = case op of
        OpConv op' ->
          -- In case of type conversion, simply convert the sensitivity
          [ fromMaybe (error "deriveTape: doOp failed") $
              doOp (OpConv $ flipConvOp op') [s]
          ]
        _ ->
          map (mul s) $ calculatePDs op $ map tapePrimal p

      -- Propagate the new sensitivities
      pd = zipWith deriveTape p s''
   in -- Add up the results
      foldl' (M.unionWith add) M.empty pd
  where
    add x y =
      fromMaybe (error "deriveTape: add failed") $
        doOp (OpBin $ addFor $ opReturnType op) [x, y]
    mul x y =
      fromMaybe (error "deriveTape: mul failed") $
        doOp (OpBin $ mulFor $ opReturnType op) [x, y]

-- JVP / Forward mode automatic differentiation--

-- | In JVP, the derivative of the variable must be saved. This is
-- represented as a second value.
data JVPValue = JVPValue ADValue ADValue
  deriving (Show)

-- | This calculates the derivative part of the JVPValue resulting
-- from the application of a mathematical operation on one or more
-- JVPValues.
jvpHandleFn :: Op -> [Either ADValue JVPValue] -> Maybe ADValue
jvpHandleFn op p = do
  case op of
    OpConv _ ->
      -- In case of type conversion, simply convert
      -- the old derivative
      doOp op [derivative $ head p]
    _ -> do
      -- Calculate the new derivative using the chain
      -- rule
      let pds = calculatePDs op $ map primal' p
      vs <- zipWithM mul pds $ map derivative p
      foldM add (Constant $ blankPrimValue $ opReturnType op) vs
  where
    primal' (Left v) = v
    primal' (Right (JVPValue v _)) = v
    derivative (Left v) = Constant $ blankPrimValue $ primValueType $ primitive v
    derivative (Right (JVPValue _ d)) = d

    add x y = doOp (OpBin $ addFor $ opReturnType op) [x, y]
    mul x y = doOp (OpBin $ mulFor $ opReturnType op) [x, y]
