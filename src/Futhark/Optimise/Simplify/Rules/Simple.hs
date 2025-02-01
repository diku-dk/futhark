-- | Particularly simple simplification rules.
module Futhark.Optimise.Simplify.Rules.Simple
  ( TypeLookup,
    VarLookup,
    applySimpleRules,
  )
where

import Control.Monad
import Data.List (isSuffixOf)
import Data.List.NonEmpty qualified as NE
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR
import Futhark.Util (focusNth)

-- | A function that, given a variable name, returns its definition.
type VarLookup rep = VName -> Maybe (Exp rep, Certs)

-- | A function that, given a subexpression, returns its type.
type TypeLookup = SubExp -> Maybe Type

-- | A simple rule is a top-down rule that can be expressed as a pure
-- function.
type SimpleRule rep = VarLookup rep -> TypeLookup -> BasicOp -> Maybe (BasicOp, Certs)

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False

simplifyCmpOp :: SimpleRule rep
simplifyCmpOp _ _ (CmpOp cmp e1 e2)
  | e1 == e2 = constRes $
      BoolValue $
        case cmp of
          CmpEq {} -> True
          CmpSlt {} -> False
          CmpUlt {} -> False
          CmpSle {} -> True
          CmpUle {} -> True
          FCmpLt {} -> False
          FCmpLe {} -> True
          CmpLlt -> False
          CmpLle -> True
simplifyCmpOp _ _ (CmpOp cmp (Constant v1) (Constant v2)) =
  constRes . BoolValue =<< doCmpOp cmp v1 v2
simplifyCmpOp look _ (CmpOp CmpEq {} (Constant (IntValue x)) (Var v))
  | Just (BasicOp (ConvOp BToI {} b), cs) <- look v =
      case valueIntegral x :: Int of
        1 -> Just (SubExp b, cs)
        0 -> Just (UnOp (Neg Bool) b, cs)
        _ -> Just (SubExp (Constant (BoolValue False)), cs)
simplifyCmpOp _ _ _ = Nothing

simplifyBinOp :: SimpleRule rep
simplifyBinOp _ _ (BinOp op (Constant v1) (Constant v2))
  | Just res <- doBinOp op v1 v2 =
      constRes res
-- By normalisation, constants are always on the left.
--
-- x+(y+z) = (x+y)+z (where x and y are constants).
simplifyBinOp look _ (BinOp op1 (Constant x1) (Var y1))
  | associativeBinOp op1,
    Just (BasicOp (BinOp op2 (Constant x2) y2), cs) <- look y1,
    op1 == op2,
    Just res <- doBinOp op1 x1 x2 =
      Just (BinOp op1 (Constant res) y2, cs)
simplifyBinOp look _ (BinOp (Add it ovf) e1 e2)
  | isCt0 e1 = resIsSubExp e2
  | isCt0 e2 = resIsSubExp e1
  -- x+(y-x) => y
  | Var v2 <- e2,
    Just (BasicOp (BinOp Sub {} e2_a e2_b), cs) <- look v2,
    e2_b == e1 =
      Just (SubExp e2_a, cs)
  -- x+(-1*y) => x-y
  | Var v2 <- e2,
    Just (BasicOp (BinOp Mul {} (Constant (IntValue x)) e3), cs) <- look v2,
    valueIntegral x == (-1 :: Int) =
      Just (BinOp (Sub it ovf) e1 e3, cs)
simplifyBinOp _ _ (BinOp FAdd {} e1 e2)
  | isCt0 e1 = resIsSubExp e2
  | isCt0 e2 = resIsSubExp e1
simplifyBinOp look _ (BinOp sub@(Sub t _) e1 e2)
  | isCt0 e2 = resIsSubExp e1
  | e1 == e2 = Just (SubExp (intConst t 0), mempty)
  --
  -- Below are cases for simplifying (a+b)-b and permutations.
  --
  -- (e1_a+e1_b)-e1_a == e1_b
  | Var v1 <- e1,
    Just (BasicOp (BinOp Add {} e1_a e1_b), cs) <- look v1,
    e1_a == e2 =
      Just (SubExp e1_b, cs)
  -- (e1_a+e1_b)-e1_b == e1_a
  | Var v1 <- e1,
    Just (BasicOp (BinOp Add {} e1_a e1_b), cs) <- look v1,
    e1_b == e2 =
      Just (SubExp e1_a, cs)
  -- e2_a-(e2_a+e2_b) == 0-e2_b
  | Var v2 <- e2,
    Just (BasicOp (BinOp Add {} e2_a e2_b), cs) <- look v2,
    e2_a == e1 =
      Just (BinOp sub (intConst t 0) e2_b, cs)
  -- e2_b-(e2_a+e2_b) == 0-e2_a
  | Var v2 <- e2,
    Just (BasicOp (BinOp Add {} e2_a e2_b), cs) <- look v2,
    e2_b == e1 =
      Just (BinOp sub (intConst t 0) e2_a, cs)
simplifyBinOp _ _ (BinOp FSub {} e1 e2)
  | isCt0 e2 = resIsSubExp e1
simplifyBinOp _ _ (BinOp Mul {} e1 e2)
  | isCt0 e1 = resIsSubExp e1
  | isCt0 e2 = resIsSubExp e2
  | isCt1 e1 = resIsSubExp e2
  | isCt1 e2 = resIsSubExp e1
simplifyBinOp _ _ (BinOp FMul {} e1 e2)
  | isCt1 e1 = resIsSubExp e2
  | isCt1 e2 = resIsSubExp e1
simplifyBinOp look _ (BinOp (SMod t _) e1 e2)
  | isCt1 e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | e1 == e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | Var v1 <- e1,
    Just (BasicOp (BinOp SMod {} _ e4), v1_cs) <- look v1,
    e4 == e2 =
      Just (SubExp e1, v1_cs)
simplifyBinOp _ _ (BinOp SDiv {} e1 e2)
  | isCt0 e1 = resIsSubExp e1
  | isCt1 e2 = resIsSubExp e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp SDivUp {} e1 e2)
  | isCt0 e1 = resIsSubExp e1
  | isCt1 e2 = resIsSubExp e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp FDiv {} e1 e2)
  | isCt0 e1 = resIsSubExp e1
  | isCt1 e2 = resIsSubExp e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp (SRem t _) e1 e2)
  | isCt1 e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | e1 == e2 = constRes $ IntValue $ intValue t (1 :: Int)
simplifyBinOp _ _ (BinOp SQuot {} e1 e2)
  | isCt1 e2 = resIsSubExp e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp (Pow t) e1 e2)
  | e1 == intConst t 2 =
      Just (BinOp (Shl t) (intConst t 1) e2, mempty)
simplifyBinOp _ _ (BinOp (FPow t) e1 e2)
  | isCt0 e2 = resIsSubExp $ floatConst t 1
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = resIsSubExp e1
simplifyBinOp _ _ (BinOp (Shl t) e1 e2)
  | isCt0 e2 = resIsSubExp e1
  | isCt0 e1 = resIsSubExp $ intConst t 0
simplifyBinOp _ _ (BinOp AShr {} e1 e2)
  | isCt0 e2 = resIsSubExp e1
simplifyBinOp _ _ (BinOp (And t) e1 e2)
  | isCt0 e1 = resIsSubExp $ intConst t 0
  | isCt0 e2 = resIsSubExp $ intConst t 0
  | e1 == e2 = resIsSubExp e1
simplifyBinOp _ _ (BinOp Or {} e1 e2)
  | isCt0 e1 = resIsSubExp e2
  | isCt0 e2 = resIsSubExp e1
  | e1 == e2 = resIsSubExp e1
simplifyBinOp _ _ (BinOp (Xor t) e1 e2)
  | isCt0 e1 = resIsSubExp e2
  | isCt0 e2 = resIsSubExp e1
  | e1 == e2 = resIsSubExp $ intConst t 0
simplifyBinOp defOf _ (BinOp LogAnd e1 e2)
  | isCt0 e1 = constRes $ BoolValue False
  | isCt0 e2 = constRes $ BoolValue False
  | isCt1 e1 = resIsSubExp e2
  | isCt1 e2 = resIsSubExp e1
  | Var v <- e1,
    Just (BasicOp (UnOp (Neg Bool) e1'), v_cs) <- defOf v,
    e1' == e2 =
      Just (SubExp $ Constant $ BoolValue False, v_cs)
  | Var v <- e2,
    Just (BasicOp (UnOp (Neg Bool) e2'), v_cs) <- defOf v,
    e2' == e1 =
      Just (SubExp $ Constant $ BoolValue False, v_cs)
simplifyBinOp defOf _ (BinOp LogOr e1 e2)
  | isCt0 e1 = resIsSubExp e2
  | isCt0 e2 = resIsSubExp e1
  | isCt1 e1 = constRes $ BoolValue True
  | isCt1 e2 = constRes $ BoolValue True
  | Var v <- e1,
    Just (BasicOp (UnOp (Neg Bool) e1'), v_cs) <- defOf v,
    e1' == e2 =
      Just (SubExp $ Constant $ BoolValue True, v_cs)
  | Var v <- e2,
    Just (BasicOp (UnOp (Neg Bool) e2'), v_cs) <- defOf v,
    e2' == e1 =
      Just (SubExp $ Constant $ BoolValue True, v_cs)
simplifyBinOp defOf _ (BinOp (SMax it) e1 e2)
  | e1 == e2 =
      resIsSubExp e1
  | Var v1 <- e1,
    Just (BasicOp (BinOp (SMax _) e1_1 e1_2), v1_cs) <- defOf v1,
    e1_1 == e2 =
      Just (BinOp (SMax it) e1_2 e2, v1_cs)
  | Var v1 <- e1,
    Just (BasicOp (BinOp (SMax _) e1_1 e1_2), v1_cs) <- defOf v1,
    e1_2 == e2 =
      Just (BinOp (SMax it) e1_1 e2, v1_cs)
  | Var v2 <- e2,
    Just (BasicOp (BinOp (SMax _) e2_1 e2_2), v2_cs) <- defOf v2,
    e2_1 == e1 =
      Just (BinOp (SMax it) e2_2 e1, v2_cs)
  | Var v2 <- e2,
    Just (BasicOp (BinOp (SMax _) e2_1 e2_2), v2_cs) <- defOf v2,
    e2_2 == e1 =
      Just (BinOp (SMax it) e2_1 e1, v2_cs)
simplifyBinOp _ _ _ = Nothing

constRes :: PrimValue -> Maybe (BasicOp, Certs)
constRes = Just . (,mempty) . SubExp . Constant

resIsSubExp :: SubExp -> Maybe (BasicOp, Certs)
resIsSubExp = Just . (,mempty) . SubExp

simplifyUnOp :: SimpleRule rep
simplifyUnOp _ _ (UnOp op (Constant v)) =
  constRes =<< doUnOp op v
simplifyUnOp defOf _ (UnOp (Neg Bool) (Var v))
  | Just (BasicOp (UnOp (Neg Bool) v2), v_cs) <- defOf v =
      Just (SubExp v2, v_cs)
simplifyUnOp _ _ _ =
  Nothing

simplifyConvOp :: SimpleRule rep
simplifyConvOp _ _ (ConvOp op (Constant v)) =
  constRes =<< doConvOp op v
simplifyConvOp _ _ (ConvOp op se)
  | (from, to) <- convOpType op,
    from == to =
      resIsSubExp se
simplifyConvOp lookupVar _ (ConvOp (SExt t2 t1) (Var v))
  | Just (BasicOp (ConvOp (SExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (SExt t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (ZExt t2 t1) (Var v))
  | Just (BasicOp (ConvOp (ZExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (ZExt t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (SIToFP t2 t1) (Var v))
  | Just (BasicOp (ConvOp (SExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (SIToFP t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (UIToFP t2 t1) (Var v))
  | Just (BasicOp (ConvOp (ZExt t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (UIToFP t3 t1) se, v_cs)
simplifyConvOp lookupVar _ (ConvOp (FPConv t2 t1) (Var v))
  | Just (BasicOp (ConvOp (FPConv t3 _) se), v_cs) <- lookupVar v,
    t2 >= t3 =
      Just (ConvOp (FPConv t3 t1) se, v_cs)
simplifyConvOp _ _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: SimpleRule rep
simplifyAssert _ _ (Assert (Constant (BoolValue True)) _ _) =
  constRes UnitValue
simplifyAssert _ _ _ =
  Nothing

-- No-op reshape.
simplifyIdentityReshape :: SimpleRule rep
simplifyIdentityReshape _ seType (Reshape _ newshape v)
  | Just t <- seType $ Var v,
    newshape == arrayShape t =
      resIsSubExp $ Var v
simplifyIdentityReshape _ _ _ = Nothing

simplifyReshapeReshape :: SimpleRule rep
simplifyReshapeReshape defOf _ (Reshape k1 newshape v)
  | Just (BasicOp (Reshape k2 _ v2), v_cs) <- defOf v =
      Just (Reshape (max k1 k2) newshape v2, v_cs)
simplifyReshapeReshape _ _ _ = Nothing

simplifyReshapeScratch :: SimpleRule rep
simplifyReshapeScratch defOf _ (Reshape _ newshape v)
  | Just (BasicOp (Scratch bt _), v_cs) <- defOf v =
      Just (Scratch bt $ shapeDims newshape, v_cs)
simplifyReshapeScratch _ _ _ = Nothing

simplifyReshapeReplicate :: SimpleRule rep
simplifyReshapeReplicate defOf seType (Reshape _ newshape v)
  | Just (BasicOp (Replicate _ se), v_cs) <- defOf v,
    Just oldshape <- arrayShape <$> seType se,
    shapeDims oldshape `isSuffixOf` shapeDims newshape =
      let new =
            take (length newshape - shapeRank oldshape) $
              shapeDims newshape
       in Just (Replicate (Shape new) se, v_cs)
simplifyReshapeReplicate _ _ _ = Nothing

simplifyReshapeIota :: SimpleRule rep
simplifyReshapeIota defOf _ (Reshape _ newshape v)
  | Just (BasicOp (Iota _ offset stride it), v_cs) <- defOf v,
    [n] <- shapeDims newshape =
      Just (Iota n offset stride it, v_cs)
simplifyReshapeIota _ _ _ = Nothing

simplifyReshapeConcat :: SimpleRule rep
simplifyReshapeConcat defOf seType (Reshape ReshapeCoerce newshape v) = do
  (BasicOp (Concat d arrs _), v_cs) <- defOf v
  (bef, w', aft) <- focusNth d $ shapeDims newshape
  (arr_bef, _, arr_aft) <-
    focusNth d <=< fmap arrayDims $ seType $ Var $ NE.head arrs
  guard $ arr_bef == bef
  guard $ arr_aft == aft
  Just (Concat d arrs w', v_cs)
simplifyReshapeConcat _ _ _ = Nothing

reshapeSlice :: [DimIndex d] -> [d] -> [DimIndex d]
reshapeSlice (DimFix i : slice') scs =
  DimFix i : reshapeSlice slice' scs
reshapeSlice (DimSlice x _ s : slice') (d : ds') =
  DimSlice x d s : reshapeSlice slice' ds'
reshapeSlice _ _ = []

-- If we are size-coercing a slice, then we might as well just use a
-- different slice instead.
simplifyReshapeIndex :: SimpleRule rep
simplifyReshapeIndex defOf _ (Reshape ReshapeCoerce newshape v)
  | Just (BasicOp (Index v' slice), v_cs) <- defOf v,
    slice' <- Slice $ reshapeSlice (unSlice slice) $ shapeDims newshape,
    slice' /= slice =
      Just (Index v' slice', v_cs)
simplifyReshapeIndex _ _ _ = Nothing

-- If we are updating a slice with the result of a size coercion, we
-- instead use the original array and update the slice dimensions.
simplifyUpdateReshape :: SimpleRule rep
simplifyUpdateReshape defOf seType (Update safety dest slice (Var v))
  | Just (BasicOp (Reshape ReshapeCoerce _ v'), v_cs) <- defOf v,
    Just ds <- arrayDims <$> seType (Var v'),
    slice' <- Slice $ reshapeSlice (unSlice slice) ds,
    slice' /= slice =
      Just (Update safety dest slice' $ Var v', v_cs)
simplifyUpdateReshape _ _ _ = Nothing

-- | If we are replicating a scratch array (possibly indirectly), just
-- turn it into a scratch by itself.
repScratchToScratch :: SimpleRule rep
repScratchToScratch defOf seType (Replicate shape (Var src)) = do
  t <- seType $ Var src
  cs <- isActuallyScratch src
  pure (Scratch (elemType t) (shapeDims shape <> arrayDims t), cs)
  where
    isActuallyScratch v =
      case defOf v of
        Just (BasicOp Scratch {}, cs) ->
          Just cs
        Just (BasicOp (Rearrange _ v'), cs) ->
          (cs <>) <$> isActuallyScratch v'
        Just (BasicOp (Reshape _ _ v'), cs) ->
          (cs <>) <$> isActuallyScratch v'
        _ -> Nothing
repScratchToScratch _ _ _ =
  Nothing

simpleRules :: [SimpleRule rep]
simpleRules =
  [ simplifyBinOp,
    simplifyCmpOp,
    simplifyUnOp,
    simplifyConvOp,
    simplifyAssert,
    repScratchToScratch,
    simplifyIdentityReshape,
    simplifyReshapeReshape,
    simplifyReshapeScratch,
    simplifyReshapeReplicate,
    simplifyReshapeIota,
    simplifyReshapeConcat,
    simplifyReshapeIndex,
    simplifyUpdateReshape
  ]

-- | Try to simplify the given t'BasicOp', returning a new t'BasicOp'
-- and certificates that it must depend on.
{-# NOINLINE applySimpleRules #-}
applySimpleRules ::
  VarLookup rep ->
  TypeLookup ->
  BasicOp ->
  Maybe (BasicOp, Certs)
applySimpleRules defOf seType op =
  msum [rule defOf seType op | rule <- simpleRules]
