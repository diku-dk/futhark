{-# LANGUAGE LambdaCase #-}
module Language.Futhark.Interpreter.AD
  (
    ADSeed (..),
    ADValue (..),
    Op (..),
    Tape (..),

    value,
    depth,
    primal,

    deriveVjp,

    valueAsType,
    prettytp,

    addFor,

    doOp,
    handleOp,
  )
where

import Language.Futhark.Primitive
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S

import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp
import Language.Futhark.Core

import Debug.Trace qualified as DBG
import Control.Monad (zipWithM)
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)

data ADSeed
  = VjpSeed Int Tape
  | JvpSeed Int ADValue (M.Map Int ADValue)

instance Show ADSeed where
  show :: ADSeed -> String
  show (VjpSeed d v) = "VjpSeed d" <> show d <> " " <> show v
  show (JvpSeed d v dv) = "JvpSeed d" <> show d <> " " <> show v <> " " <> show dv

data ADValue
  = Primal PrimValue
  | Seed ADSeed

instance Show ADValue where
  show (Primal v) = "Primal " <> show v
  show (Seed s) = "Seed " <> show s

data Op
  = OpBin  BinOp
  | OpCmp  CmpOp
  | OpUn   UnOp
  | OpFn   String
  | OpConv ConvOp

instance Show Op where
  show (OpBin  op) = show op
  show (OpCmp  op) = show op
  show (OpUn   op) = show op
  show (OpFn   fn) = show fn
  show (OpConv op) = show op

value :: ADValue -> PrimValue
value (Primal v) = v
value (Seed s) = value $ primal s

depth :: ADSeed -> Int
depth (VjpSeed d _) = d
depth (JvpSeed d _ _) = d

primal :: ADSeed -> ADValue
primal (VjpSeed _ v) = tapeValue v
primal (JvpSeed _ v _) = v

valueAsType :: PrimValue -> Rational -> PrimValue
valueAsType v c = case v of
  IntValue (Int8Value _)      -> IntValue $ Int8Value $ round c
  IntValue (Int16Value _)     -> IntValue $ Int16Value $ round c
  IntValue (Int32Value _)     -> IntValue $ Int32Value $ round c
  IntValue (Int64Value _)     -> IntValue $ Int64Value $ round c
  FloatValue (Float16Value _) -> FloatValue $ Float16Value $ fromRational c
  FloatValue (Float32Value _) -> FloatValue $ Float32Value $ fromRational c
  FloatValue (Float64Value _) -> FloatValue $ Float64Value $ fromRational c
  _ -> error $ "No valid prim value for " ++ show v

opTypeMatch :: Op -> [PrimValue] -> Bool
opTypeMatch (OpBin  op) p = all (\x ->      binOpType  op  == primValueType x) p
opTypeMatch (OpCmp  op) p = all (\x ->      cmpOpType  op  == primValueType x) p
opTypeMatch (OpUn   op) p = all (\x ->      unOpType   op  == primValueType x) p
opTypeMatch (OpConv op) p = all (\x -> fst (convOpType op) == primValueType x) p
opTypeMatch (OpFn   fn) p = case M.lookup fn primFuns of
                          Just (t, _, _) -> and $ zipWith (\x y -> x == primValueType y) t p
                          Nothing -> False

opReturnType :: Op -> PrimType
opReturnType (OpBin  op) = binOpType op
opReturnType (OpCmp  op) = cmpOpType op
opReturnType (OpUn   op) = unOpType  op
opReturnType (OpConv op) = snd $ convOpType op
opReturnType (OpFn   fn) = fromJust $ do
                            (_, t, _) <- M.lookup fn primFuns
                            Just t

addFor :: Op -> Op
addFor op = OpBin $ fromJust $ case opReturnType op of
  IntType t -> Just $ Add t OverflowUndef
  FloatType t -> Just $ FAdd t
  _ -> Nothing

multiplyFor :: Op -> Op
multiplyFor op = OpBin $ fromJust $ case opReturnType op of
  IntType t -> Just $ Mul t OverflowUndef
  FloatType t -> Just $ FMul t
  _ -> Nothing

doOp :: Op -> [ADValue] -> Maybe ADValue
doOp op p = do
  let p' = map value p
  if opTypeMatch op p' then do
    v <- case (op, p') of
      (OpBin  op', [x, y]) -> doBinOp op' x y
      (OpCmp  op', [x, y]) -> BoolValue <$> doCmpOp op' x y
      (OpUn   op', [x])    -> doUnOp op' x
      (OpConv op', [x])    -> doConvOp op' x
      (OpFn   fn,  _)      -> do
        (_, _, f) <- M.lookup fn primFuns
        f p'
      _ -> error "Not implemented"

    case op of
      (OpBin _)  -> handleOp op p v
      (OpCmp _)  -> Just $ Primal v
      (OpUn _)   -> handleOp op p v
      (OpFn _)   -> handleOp op p v
      (OpConv _) -> Nothing

  else Nothing

deriveOp :: Op -> [PrimExp VName] -> Maybe [PrimExp VName]
deriveOp (OpBin op) [x, y] = Just $ do
  let (a, b) = pdBinOp op x y
  [a, b]
deriveOp (OpUn  op) [x] = Just [pdUnOp op x]
deriveOp (OpFn  fn) p   = pdBuiltin (nameFromString fn) p
deriveOp _ _ = Nothing

handleOp :: Op -> [ADValue] -> PrimValue -> Maybe ADValue
handleOp op p v = do
  -- Find the maximum depth
  let pDepth = map (\case
        Primal _ -> 0
        Seed s -> depth s) p
  let maxDepth = maximum pDepth

  if maxDepth == 0 then Just $ Primal v
  else do
    -- Get all the top level parameters
    let d = map (\v' -> case v' of
          Primal _ -> Left v'
          Seed se -> if depth se == maxDepth then Right se
                                             else Left v') p

    -- Handle the previous level
    let d' = map (\case
          Right se -> primal se
          Left v' -> v') d
    v' <- handleOp op d' v

    -- Call the top level handler
    case find (\case Right _ -> True
                     Left _  -> False) d of
      Just (Right (VjpSeed {})) -> Seed . VjpSeed maxDepth <$> vjpHandleFn op d v'
      Just (Right (JvpSeed {})) -> jvpHandleFn op d maxDepth v'
      _ -> Just $ Primal v -- ?TODO: This never happens, so maybe remove it in some way?

runPrimExp :: PrimExp VName -> M.Map VName ADValue -> Maybe ADValue
runPrimExp (LeafExp n _) m = M.lookup n m
runPrimExp (ValueExp pv) _ = Just $ Primal pv
runPrimExp (BinOpExp op x y) m = do
  x' <- runPrimExp x m
  y' <- runPrimExp y m
  doOp (OpBin op) [x', y']
runPrimExp (CmpOpExp op x y) m = do
  x' <- runPrimExp x m
  y' <- runPrimExp y m
  doOp (OpCmp op) [x', y']
runPrimExp (UnOpExp op x) m = do
  x' <- runPrimExp x m
  doOp (OpUn op) [x']
runPrimExp (ConvOpExp op x) m = do
  x' <- runPrimExp x m
  doOp (OpConv op) [x']
runPrimExp (FunExp fn p _) m = do
  p' <- mapM (`runPrimExp` m) p
  doOp (OpFn fn) p'


-- VJP

data Tape
  = TapeId Int ADValue
  | TapePrim ADValue
  | TapeOp Op [Tape] ADValue

prettytp :: Tape -> String
prettytp (TapeId i _) = "d" <> show i
prettytp (TapePrim v) = show v
prettytp (TapeOp op [a] _) = show op <> "(" <> prettytp a <> ")"
prettytp (TapeOp op [a, b] _) = "(" <> prettytp a <> " " <> show op <> " " <> prettytp b <> ")"
prettytp _ = error "no prettytp"

instance Show Tape where
  show (TapeId i v) = "TapeId " <> show i <> " " <> show v
  show (TapePrim v) = "TapePrim " <> show v
  show (TapeOp fn p o) = "TapeOp " <> show fn <> " " <> show p <> " " ++ show o

vjpHandleFn :: Op -> [Either ADValue ADSeed] -> ADValue -> Maybe Tape
vjpHandleFn op p v = do
  let p' = map (\case
        Right (VjpSeed _ t) -> t
        Left v' -> TapePrim v'
        _ -> error "And unknown error occured") p -- ?TODO: This is impossible
  Just $ TapeOp op p' v

tapeValue :: Tape -> ADValue
tapeValue (TapeId _ v) = v
tapeValue (TapePrim v) = v
tapeValue (TapeOp _ _ o) = o

deriveVjp :: Tape -> ADValue -> Maybe (M.Map Int ADValue)
deriveVjp (TapeId i _) v = Just $ M.fromList [(i, v)]
deriveVjp (TapePrim _) _ = Just M.empty
deriveVjp (TapeOp op p _) v = do
  -- Create a unique name for each parameter
  let n = map (VName "d") [1..length p]
  let m = M.fromList $ zip n $ map tapeValue p

  -- Derive the function using the parameters
  op' <- deriveOp op $ map (`LeafExp` FloatType Float64) n -- TODO: Correct type
  v' <- mapM (`runPrimExp` m) op'

  -- Derive parameters and combine
  pd <- zipWithM deriveVjp p v'
  combineDerivatives pd v op

combineDerivatives :: [M.Map Int ADValue] -> ADValue -> Op -> Maybe (M.Map Int ADValue)
combineDerivatives d v op = do
  let add x y = fromJust $ doOp (addFor      op) [x, y]
  let mul x y = fromJust $ doOp (multiplyFor op) [x, y]
  Just $ foldl (M.unionWith add) M.empty $ map (M.map $ mul v) d


-- JVP

jvpHandleFn :: Op -> [Either ADValue ADSeed] -> Int -> ADValue -> Maybe ADValue
jvpHandleFn op p d av = do
  -- Turn everything into jvp values
  let p' = map (\case
        Right (JvpSeed _ v' m) -> (v', m)
        Left  v' -> (v', M.empty)
        _ -> error "And unknown error occured") p -- ?TODO: This is impossible

  -- Create a unique name for each parameter
  let n = map (VName "d") [1..length p]
  let m = M.fromList $ zip n $ map fst p'

  -- Derive the function using the parameters
  op' <- deriveOp op $ map (`LeafExp` FloatType Float64) n -- TODO: Correct type
  v' <- mapM (`runPrimExp` m) op'

  let didx = S.toList $ foldl (\x (_, m') -> S.union x (S.fromList $ M.keys m')) S.empty p'
  case mapM (\idx -> do
        -- Get derivatives
        let mul a b = doOp (multiplyFor op) [a, b]
        vs <- zipWithM (\d' (t, m') ->
              case M.lookup idx m' of
                Just v'' -> mul d' v''
                _ -> Just $ Primal $ valueAsType (value t) 0) v' p'

        -- Sum them up
        let add a b = doOp (addFor op) [a, b]
        Just $ do
          k <- foldlM add (Primal $ FloatValue $ Float64Value 0 {- TODO -}) vs
          pure (idx, k)) didx of

    Just k -> do
      m <- M.fromList <$> sequence k
      Just $ Seed $ JvpSeed d av m
    Nothing -> Just $ Seed $ JvpSeed d av M.empty
