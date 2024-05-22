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
  | JvpSeed Int PrimValue (M.Map Int ADValue)

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
primal (JvpSeed _ v _) = Primal v

deriveVjp :: Tape -> Maybe (M.Map Int ADValue)
deriveVjp tp = deriveTape tp (Primal $ valueAsType (value $ tapeValue tp) 1)

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

doOp :: Op -> [ADValue] -> Maybe ADValue
doOp op p = do
  let p' = map value p
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
    (OpUn _)   -> handleOp op p v
    (OpFn _)   -> handleOp op p v
    --(OpConv _) -> TODO
    _ -> Just $ Primal v
    

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
      Just (Right (JvpSeed {})) -> Seed . JvpSeed maxDepth v <$> jvpHandleFn op d maxDepth
      _ -> Just $ Primal v -- TODO: This never happens, so maybe remove it in some way?

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
        _ -> error "TODO: This is not possible (28ruajio)") p
  Just $ TapeOp op p' v

tapeValue :: Tape -> ADValue
tapeValue (TapeId _ v) = v
tapeValue (TapePrim v) = v
tapeValue (TapeOp _ _ o) = o

deriveTape :: Tape -> ADValue -> Maybe (M.Map Int ADValue)
deriveTape (TapeId i _) v = Just $ M.fromList [(i, v)]
deriveTape (TapePrim _) _ = Just M.empty
deriveTape (TapeOp op p _) v = do
  -- Create a unique name for each parameter
  let n = map (VName "d") [1..length p]
  let m = M.fromList $ zip n $ map tapeValue p

  -- Derive the function using the parameters
  op' <- deriveOp op $ map (`LeafExp` FloatType Float64) n -- TODO: Correct type
  v' <- mapM (`runPrimExp` m) op'

  -- Derive parameters and combine
  pd <- zipWithM deriveTape p v'
  combineDerivatives pd v

combineDerivatives :: [M.Map Int ADValue] -> ADValue -> Maybe (M.Map Int ADValue)
combineDerivatives d v = do
  let add x y = fromJust $ doOp (OpBin $ FAdd Float64) [x, y]
  let mul x y = fromJust $ doOp (OpBin $ FMul Float64) [x, y] -- TODO: Remove fromJust?
  Just $ foldl (M.unionWith add) M.empty $ map (M.map $ mul v) d


-- JVP

jvpHandleFn :: Op -> [Either ADValue ADSeed] -> Int -> Maybe (M.Map Int ADValue) 
jvpHandleFn op p d = do
  -- Turn everything into jvp values
  let p' = map (\case
        Right (JvpSeed _ v' m) -> (v', m)
        Left  v' -> (value v', M.empty)
        _ -> error "TODO: This is impossible (98ruwik)") p

  -- Create a unique name for each parameter
  let n = map (VName "d") [1..length p]
  let m = M.fromList $ zip n $ map (\(a, b) -> Seed $ JvpSeed (d - 1) a b) p'

  -- Derive the function using the parameters
  op' <- deriveOp op $ map (`LeafExp` FloatType Float64) n -- TODO: Correct type
  v' <- mapM (`runPrimExp` m) op'
  
  let didx = S.toList $ foldl (\x (_, m') -> S.union x (S.fromList $ M.keys m')) S.empty p'
  case mapM (\idx -> do
        -- Get derivatives
        let mul a b = doOp (OpBin $ FMul Float64) [a, b] -- TODO: Correct type
        vs <- zipWithM (\d' (t, m') ->
              case M.lookup idx m' of
                Just v'' -> mul d' v''
                _ -> Just $ Primal $ valueAsType t 0) v' p'

        -- Sum them up
        let add a b = doOp (OpBin $ FAdd Float64) [a, b] -- TODO: Correct type
        Just $ do
          k <- foldlM add (Primal $ FloatValue $ Float64Value 0 {- TODO -}) vs
          pure (idx, k)) didx of

    Just k -> M.fromList <$> sequence k
    Nothing -> Just M.empty
