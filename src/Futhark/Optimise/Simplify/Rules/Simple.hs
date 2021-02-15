{-# LANGUAGE TupleSections #-}

-- | Particularly simple simplification rules.
module Futhark.Optimise.Simplify.Rules.Simple
  ( TypeLookup,
    VarLookup,
    applySimpleRules,
  )
where

import Control.Monad
import Data.List (isSuffixOf)
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR

-- | A function that, given a variable name, returns its definition.
type VarLookup lore = VName -> Maybe (Exp lore, Certificates)

-- | A function that, given a subexpression, returns its type.
type TypeLookup = SubExp -> Maybe Type

-- | A simple rule is a top-down rule that can be expressed as a pure
-- function.
type SimpleRule lore = VarLookup lore -> TypeLookup -> BasicOp -> Maybe (BasicOp, Certificates)

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False

simplifyCmpOp :: SimpleRule lore
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
      0 -> Just (UnOp Not b, cs)
      _ -> Just (SubExp (Constant (BoolValue False)), cs)
simplifyCmpOp _ _ _ = Nothing

simplifyBinOp :: SimpleRule lore
simplifyBinOp _ _ (BinOp op (Constant v1) (Constant v2))
  | Just res <- doBinOp op v1 v2 =
    constRes res
simplifyBinOp look _ (BinOp Add {} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  -- x+(y-x) => y
  | Var v2 <- e2,
    Just (BasicOp (BinOp Sub {} e2_a e2_b), cs) <- look v2,
    e2_b == e1 =
    Just (SubExp e2_a, cs)
simplifyBinOp _ _ (BinOp FAdd {} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
simplifyBinOp look _ (BinOp sub@(Sub t _) e1 e2)
  | isCt0 e2 = subExpRes e1
  -- Cases for simplifying (a+b)-b and permutations.

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
  | isCt0 e2 = subExpRes e1
simplifyBinOp _ _ (BinOp Mul {} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt0 e2 = subExpRes e2
  | isCt1 e1 = subExpRes e2
  | isCt1 e2 = subExpRes e1
simplifyBinOp _ _ (BinOp FMul {} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt0 e2 = subExpRes e2
  | isCt1 e1 = subExpRes e2
  | isCt1 e2 = subExpRes e1
simplifyBinOp look _ (BinOp (SMod t _) e1 e2)
  | isCt1 e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | e1 == e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | Var v1 <- e1,
    Just (BasicOp (BinOp SMod {} _ e4), v1_cs) <- look v1,
    e4 == e2 =
    Just (SubExp e1, v1_cs)
simplifyBinOp _ _ (BinOp SDiv {} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp SDivUp {} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp FDiv {} e1 e2)
  | isCt0 e1 = subExpRes e1
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp (SRem t _) e1 e2)
  | isCt1 e2 = constRes $ IntValue $ intValue t (0 :: Int)
  | e1 == e2 = constRes $ IntValue $ intValue t (1 :: Int)
simplifyBinOp _ _ (BinOp SQuot {} e1 e2)
  | isCt1 e2 = subExpRes e1
  | isCt0 e2 = Nothing
simplifyBinOp _ _ (BinOp (FPow t) e1 e2)
  | isCt0 e2 = subExpRes $ floatConst t 1
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = subExpRes e1
simplifyBinOp _ _ (BinOp (Shl t) e1 e2)
  | isCt0 e2 = subExpRes e1
  | isCt0 e1 = subExpRes $ intConst t 0
simplifyBinOp _ _ (BinOp AShr {} e1 e2)
  | isCt0 e2 = subExpRes e1
simplifyBinOp _ _ (BinOp (And t) e1 e2)
  | isCt0 e1 = subExpRes $ intConst t 0
  | isCt0 e2 = subExpRes $ intConst t 0
  | e1 == e2 = subExpRes e1
simplifyBinOp _ _ (BinOp Or {} e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  | e1 == e2 = subExpRes e1
simplifyBinOp _ _ (BinOp (Xor t) e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  | e1 == e2 = subExpRes $ intConst t 0
simplifyBinOp defOf _ (BinOp LogAnd e1 e2)
  | isCt0 e1 = constRes $ BoolValue False
  | isCt0 e2 = constRes $ BoolValue False
  | isCt1 e1 = subExpRes e2
  | isCt1 e2 = subExpRes e1
  | Var v <- e1,
    Just (BasicOp (UnOp Not e1'), v_cs) <- defOf v,
    e1' == e2 =
    Just (SubExp $ Constant $ BoolValue False, v_cs)
  | Var v <- e2,
    Just (BasicOp (UnOp Not e2'), v_cs) <- defOf v,
    e2' == e1 =
    Just (SubExp $ Constant $ BoolValue False, v_cs)
simplifyBinOp defOf _ (BinOp LogOr e1 e2)
  | isCt0 e1 = subExpRes e2
  | isCt0 e2 = subExpRes e1
  | isCt1 e1 = constRes $ BoolValue True
  | isCt1 e2 = constRes $ BoolValue True
  | Var v <- e1,
    Just (BasicOp (UnOp Not e1'), v_cs) <- defOf v,
    e1' == e2 =
    Just (SubExp $ Constant $ BoolValue True, v_cs)
  | Var v <- e2,
    Just (BasicOp (UnOp Not e2'), v_cs) <- defOf v,
    e2' == e1 =
    Just (SubExp $ Constant $ BoolValue True, v_cs)
simplifyBinOp defOf _ (BinOp (SMax it) e1 e2)
  | e1 == e2 =
    subExpRes e1
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

constRes :: PrimValue -> Maybe (BasicOp, Certificates)
constRes = Just . (,mempty) . SubExp . Constant

subExpRes :: SubExp -> Maybe (BasicOp, Certificates)
subExpRes = Just . (,mempty) . SubExp

simplifyUnOp :: SimpleRule lore
simplifyUnOp _ _ (UnOp op (Constant v)) =
  constRes =<< doUnOp op v
simplifyUnOp defOf _ (UnOp Not (Var v))
  | Just (BasicOp (UnOp Not v2), v_cs) <- defOf v =
    Just (SubExp v2, v_cs)
simplifyUnOp _ _ _ =
  Nothing

simplifyConvOp :: SimpleRule lore
simplifyConvOp _ _ (ConvOp op (Constant v)) =
  constRes =<< doConvOp op v
simplifyConvOp _ _ (ConvOp op se)
  | (from, to) <- convOpType op,
    from == to =
    subExpRes se
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
simplifyAssert :: SimpleRule lore
simplifyAssert _ _ (Assert (Constant (BoolValue True)) _ _) =
  constRes Checked
simplifyAssert _ _ _ =
  Nothing

simplifyIdentityReshape :: SimpleRule lore
simplifyIdentityReshape _ seType (Reshape newshape v)
  | Just t <- seType $ Var v,
    newDims newshape == arrayDims t -- No-op reshape.
    =
    subExpRes $ Var v
simplifyIdentityReshape _ _ _ = Nothing

simplifyReshapeReshape :: SimpleRule lore
simplifyReshapeReshape defOf _ (Reshape newshape v)
  | Just (BasicOp (Reshape oldshape v2), v_cs) <- defOf v =
    Just (Reshape (fuseReshape oldshape newshape) v2, v_cs)
simplifyReshapeReshape _ _ _ = Nothing

simplifyReshapeScratch :: SimpleRule lore
simplifyReshapeScratch defOf _ (Reshape newshape v)
  | Just (BasicOp (Scratch bt _), v_cs) <- defOf v =
    Just (Scratch bt $ newDims newshape, v_cs)
simplifyReshapeScratch _ _ _ = Nothing

simplifyReshapeReplicate :: SimpleRule lore
simplifyReshapeReplicate defOf seType (Reshape newshape v)
  | Just (BasicOp (Replicate _ se), v_cs) <- defOf v,
    Just oldshape <- arrayShape <$> seType se,
    shapeDims oldshape `isSuffixOf` newDims newshape =
    let new =
          take (length newshape - shapeRank oldshape) $
            newDims newshape
     in Just (Replicate (Shape new) se, v_cs)
simplifyReshapeReplicate _ _ _ = Nothing

simplifyReshapeIota :: SimpleRule lore
simplifyReshapeIota defOf _ (Reshape newshape v)
  | Just (BasicOp (Iota _ offset stride it), v_cs) <- defOf v,
    [n] <- newDims newshape =
    Just (Iota n offset stride it, v_cs)
simplifyReshapeIota _ _ _ = Nothing

improveReshape :: SimpleRule lore
improveReshape _ seType (Reshape newshape v)
  | Just t <- seType $ Var v,
    newshape' <- informReshape (arrayDims t) newshape,
    newshape' /= newshape =
    Just (Reshape newshape' v, mempty)
improveReshape _ _ _ = Nothing

-- | If we are copying a scratch array (possibly indirectly), just turn it into a scratch by
-- itself.
copyScratchToScratch :: SimpleRule lore
copyScratchToScratch defOf seType (Copy src) = do
  t <- seType $ Var src
  if isActuallyScratch src
    then Just (Scratch (elemType t) (arrayDims t), mempty)
    else Nothing
  where
    isActuallyScratch v =
      case asBasicOp . fst =<< defOf v of
        Just Scratch {} -> True
        Just (Rearrange _ v') -> isActuallyScratch v'
        Just (Reshape _ v') -> isActuallyScratch v'
        _ -> False
copyScratchToScratch _ _ _ =
  Nothing

simpleRules :: [SimpleRule lore]
simpleRules =
  [ simplifyBinOp,
    simplifyCmpOp,
    simplifyUnOp,
    simplifyConvOp,
    simplifyAssert,
    copyScratchToScratch,
    simplifyIdentityReshape,
    simplifyReshapeReshape,
    simplifyReshapeScratch,
    simplifyReshapeReplicate,
    simplifyReshapeIota,
    improveReshape
  ]

-- | Try to simplify the given 'BasicOp', returning a new 'BasicOp'
-- and certificates that it must depend on.
{-# NOINLINE applySimpleRules #-}
applySimpleRules ::
  VarLookup lore ->
  TypeLookup ->
  BasicOp ->
  Maybe (BasicOp, Certificates)
applySimpleRules defOf seType op =
  msum [rule defOf seType op | rule <- simpleRules]
