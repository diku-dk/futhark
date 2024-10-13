module Language.Futhark.Interpreter.AD
  ( Op (..),
    ADVariable (..),
    ADValue (..),
    Tape (..),
    VJPValue (..),
    JVPValue (..),
    doOp,
    addFor,
    primal,
    tapePrimal,
    primitive,
    deriveTape,
  )
where

import Control.Monad (foldM, zipWithM)
import Data.Either (isRight)
import Data.List (find)
import Data.Map qualified as M
-- These are needed to reuse the definitions of the derivatives
-- used by the compiler
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp (PrimExp (..))
import Language.Futhark.Core (VName (..), nameFromString)
-- As the mathematical functions of Futhark are implemented
-- for the primitive type used in the compiler, said type is
-- used for AD for simplicity
import Language.Futhark.Primitive

-- Mathematical operations, and type checking--
-- A mathematical operation
data Op
  = OpBin BinOp
  | OpCmp CmpOp
  | OpUn UnOp
  | OpFn String
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
  Nothing -> error "TODO: IMPOSSIBLE" -- It is assumed that the function exists

-- Gets the return type of an operation
opReturnType :: Op -> PrimType
opReturnType (OpBin op) = binOpType op
opReturnType (OpCmp op) = cmpOpType op
opReturnType (OpUn op) = unOpType op
opReturnType (OpConv op) = snd $ convOpType op
opReturnType (OpFn fn) = case M.lookup fn primFuns of
  Just (_, t, _) -> t
  Nothing -> error "TODO: IMPOSSIBLE" -- It is assumed that the function exists

-- Returns the operation which performs addition (or an
-- equivalent operation) on the given type
addFor :: PrimType -> BinOp
addFor (IntType t) = Add t OverflowUndef -- TODO: Overflow?
addFor (FloatType t) = FAdd t
addFor Bool = LogOr
addFor t = error $ "No notion of addition exists for type `" ++ show t ++ "`"

-- Returns the function which performs multiplication
-- (or an equivalent operation) on the given type
multiplyFor :: PrimType -> BinOp
multiplyFor (IntType t) = Mul t OverflowUndef -- TODO: Overflow?
multiplyFor (FloatType t) = FMul t
multiplyFor Bool = LogAnd
multiplyFor t = error $ "No notion of multiplication exists for type `" ++ show t ++ "`"

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
primitive v@(Variable _ _) = primitive $ primal v
primitive (Constant v) = v

-- Code reuse from compiler AD--
-- Evaluates a PrimExp using doOp
evalPrimExp :: PrimExp VName -> M.Map VName ADValue -> Maybe ADValue
evalPrimExp (LeafExp n _) m = M.lookup n m
evalPrimExp (ValueExp pv) _ = Just $ Constant pv
evalPrimExp (BinOpExp op x y) m = do
  x' <- evalPrimExp x m
  y' <- evalPrimExp y m
  doOp (OpBin op) [x', y']
evalPrimExp (CmpOpExp op x y) m = do
  x' <- evalPrimExp x m
  y' <- evalPrimExp y m
  doOp (OpCmp op) [x', y']
evalPrimExp (UnOpExp op x) m = do
  x' <- evalPrimExp x m
  doOp (OpUn op) [x']
evalPrimExp (ConvOpExp op x) m = do
  x' <- evalPrimExp x m
  doOp (OpConv op) [x']
evalPrimExp (FunExp fn p _) m = do
  p' <- mapM (`evalPrimExp` m) p
  doOp (OpFn fn) p'

-- Returns a list of PrimExps calculating the partial
-- derivative of each operands of a given operation
lookupPDs :: Op -> [PrimExp VName] -> Maybe [PrimExp VName]
lookupPDs (OpBin op) [x, y] = Just $ do
  let (a, b) = pdBinOp op x y
  [a, b]
lookupPDs (OpUn op) [x] = Just [pdUnOp op x]
lookupPDs (OpFn fn) p = pdBuiltin (nameFromString fn) p
lookupPDs _ _ = Nothing

-- Shared AD logic--
-- This function performs a mathematical operation on a
-- list of operands, performing automatic differentiation
-- if one or more operands is a Variable (of depth > 0)
doOp :: Op -> [ADValue] -> Maybe ADValue
doOp op o =
  let dep = case op of
        OpCmp _ -> 0 -- AD is not well-defined for comparason operations
        -- There are no derivatives for those written in
        -- PrimExp (check lookupPDs)
        _ -> maximum (map depth o)
   in if dep == 0
        then -- In this case, every value is a constant, and
        -- the mathematical operation can be applied as
        -- it would be otherwise

        -- First, we make sure that the types of the
        -- operands match those of the operation

          let o' = map primitive o
           in if opTypeMatch op (map primValueType o')
                then do
                  -- If they do, we perform the operation, and
                  -- return a Constant
                  Constant <$> case (op, o') of
                    (OpBin op', [x, y]) -> doBinOp op' x y
                    (OpCmp op', [x, y]) -> BoolValue <$> doCmpOp op' x y
                    (OpUn op', [x]) -> doUnOp op' x
                    (OpConv op', [x]) -> doConvOp op' x
                    (OpFn fn, _) -> do
                      (_, _, f) <- M.lookup fn primFuns
                      f o'
                    _ -> error "TODO: IMPOSSIBLE" -- This is needed due to the fact that the function
                    -- takes an array, yet some of the operations have a
                    -- fixed number of operands
                else -- If the types do not match, we return Nothing
                  Nothing
        else do
          -- In this case, some values are variables.
          -- We therefore have to perform the necessary
          -- steps for AD

          -- First, we calculate the value for the
          -- previous depth
          let oprev = map primal o
          vprev <- doOp op oprev

          -- Then we separate the values of the maximum
          -- depth from those of a lower depth
          let o' = map (divideDepths dep) o
          -- Then we find out what type of AD is being
          -- performed
          case find isRight o' of
            -- Finally, we perform the necessary steps
            -- for the given type of AD
            Just (Right (VJP {})) -> Just . Variable dep . VJP . VJPValue $ vjpHandleOp op (map extractVJP o') vprev
            Just (Right (JVP {})) -> Variable dep . JVP . JVPValue vprev <$> jvpHandleFn op (map extractJVP o')
            _ -> error "TODO: IMPOSSIBLE" -- Since the maximum depth is non-zero, there must
            -- be at least one variable of depth > 0
  where
    divideDepths :: Int -> ADValue -> Either ADValue ADVariable
    divideDepths _ v@(Constant {}) = Left v
    divideDepths d v@(Variable d' v') = if d' < d then Left v else Right v'

    -- TODO: There may be a more graceful way of
    -- doing this
    extractVJP :: Either ADValue ADVariable -> Either ADValue VJPValue
    extractVJP (Right (VJP v)) = Right v
    extractVJP (Left v) = Left v
    extractVJP _ = error "TODO: IMPOSSIBLE" -- This will never be called when the maximum depth layer is JVP

    -- TODO: There may be a more graceful way of
    -- doing this
    extractJVP :: Either ADValue ADVariable -> Either ADValue JVPValue
    extractJVP (Right (JVP v)) = Right v
    extractJVP (Left v) = Left v
    extractJVP _ = error "TODO: IMPOSSIBLE" -- This will never be called when the maximum depth layer is VJP

calculatePDs :: Op -> [ADValue] -> Maybe [ADValue]
calculatePDs op p = do
  -- Create a unique VName for each operand
  let n = map (\i -> VName (nameFromString $ "x" ++ show i) i) [1 .. length p]
  -- Put the operands in the environment
  let m = M.fromList $ zip n p

  -- Look up, and calculate the partial derivative
  -- of the operation with respect to each operand
  pde <- lookupPDs op $ map (`LeafExp` opReturnType op) n
  mapM (`evalPrimExp` m) pde

-- VJP / Reverse mode automatic differentiation--
-- In reverse mode AD, the entire computation
-- leading up to a variable must be saved
-- This is represented as a Tape
newtype VJPValue = VJPValue Tape
  deriving (Show)

-- Represents a computation tree, as well as every
-- intermediate value in its evaluation
-- TODO: Consider making this a graph
data Tape
  = -- This represents a variable. Each variable is given
    -- a unique ID, and has an initial value
    TapeID Int ADValue
  | -- this represents a constant
    TapeConst ADValue
  | -- This represents the application of a mathematical
    -- operation. Each parameter is given by its Tape, and
    -- the return value of the operation is saved
    TapeOp Op [Tape] ADValue
  deriving (Show)

-- Returns the primal value of a Tape
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

-- This calculates every partial derivative of a Tape
-- The result is a map of the partial derivatives, each
-- key corresponding to the ID of a free variable (see
-- TapeID)
deriveTape :: Tape -> ADValue -> Maybe (M.Map Int ADValue)
deriveTape (TapeID i _) s = Just $ M.fromList [(i, s)]
deriveTape (TapeConst _) _ = Just M.empty
deriveTape (TapeOp op p _) s = do
  -- Calculate the new sensitivities
  s'' <- case op of
    OpConv op' -> do
      -- In case of type conversion, simply convert
      -- the sensitivity
      s' <- doOp (OpConv $ flipConvOp op') [s]
      Just [s']
    _ -> do
      pds <- calculatePDs op $ map tapePrimal p
      mapM (mul s) pds

  -- Propagate the new sensitivities
  pd <- zipWithM deriveTape p s''
  -- Add up the results
  Just $ foldl (M.unionWith add) M.empty pd
  where
    add x y = expectJust "TODO: Remove me" $ doOp (OpBin $ addFor $ opReturnType op) [x, y]
    mul x y = doOp (OpBin $ multiplyFor $ opReturnType op) [x, y]

    -- TODO: This should be removed if possible.
    -- It is only here, as there is no monadic
    -- version of unionWith :(
    expectJust :: String -> Maybe a -> a
    expectJust _ (Just v) = v
    expectJust e Nothing = error e

-- JVP / Forward mode automatic differentiation--

-- In JVP, the derivative of the variable must be saved.
-- This is represented as a second value
data JVPValue = JVPValue ADValue ADValue
  deriving (Show)

-- This calculates the derivative part of the JVPValue
-- resulting from the application of a mathematical
-- operation on one or more JVPValues
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
      pds <- calculatePDs op $ map primal' p
      vs <- zipWithM mul pds $ map derivative p
      foldM add (Constant $ blankPrimValue $ opReturnType op) vs
  where
    primal' (Left v) = v
    primal' (Right (JVPValue v _)) = v
    derivative (Left v) = Constant $ blankPrimValue $ primValueType $ primitive v
    derivative (Right (JVPValue _ d)) = d

    add x y = doOp (OpBin $ addFor $ opReturnType op) [x, y]
    mul x y = doOp (OpBin $ multiplyFor $ opReturnType op) [x, y]
