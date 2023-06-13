module Futhark.Internalise.Refinement (transformProg) where

import Control.Monad
import Control.Monad.RWS (MonadReader (..), MonadWriter (..), RWS, asks, lift, runRWS)
import Data.List (find)
import Futhark.Analysis.PrimExp (PrimExp)
import Futhark.Analysis.PrimExp qualified as PE
import Futhark.Internalise.TypesValues (internalisePrimType, internalisePrimValue)
import Futhark.MonadFreshNames
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.PrimExp
import Futhark.SoP.Refine
import Futhark.SoP.SoP
import Futhark.SoP.ToFromSoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Prop
import Language.Futhark.Semantic hiding (Env)

type Env = ()

newtype RefineM a
  = RefineM (SoPMT VName (RWS Env () VNameSource) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadSoP VName
    )

instance MonadFreshNames RefineM where
  getNameSource = RefineM $ getNameSource
  putNameSource = RefineM . putNameSource

convertBinOp :: BinOp -> PrimExp VName -> PrimExp VName -> PrimType -> PrimType -> Maybe (PrimExp VName)
convertBinOp LogAnd x y Bool _ =
  simpleBinOp PE.LogAnd x y
convertBinOp LogOr x y Bool _ =
  simpleBinOp PE.LogOr x y
convertBinOp Plus x y (Signed t) _ =
  simpleBinOp (PE.Add t PE.OverflowWrap) x y
convertBinOp Plus x y (Unsigned t) _ =
  simpleBinOp (PE.Add t PE.OverflowWrap) x y
convertBinOp Plus x y (FloatType t) _ =
  simpleBinOp (PE.FAdd t) x y
convertBinOp Minus x y (Signed t) _ =
  simpleBinOp (PE.Sub t PE.OverflowWrap) x y
convertBinOp Minus x y (Unsigned t) _ =
  simpleBinOp (PE.Sub t PE.OverflowWrap) x y
convertBinOp Minus x y (FloatType t) _ =
  simpleBinOp (PE.FSub t) x y
convertBinOp Times x y (Signed t) _ =
  simpleBinOp (PE.Mul t PE.OverflowWrap) x y
convertBinOp Times x y (Unsigned t) _ =
  simpleBinOp (PE.Mul t PE.OverflowWrap) x y
convertBinOp Times x y (FloatType t) _ =
  simpleBinOp (PE.FMul t) x y
convertBinOp Equal x y t _ =
  simpleCmpOp (PE.CmpEq $ internalisePrimType t) x y
convertBinOp NotEqual x y t _ = do
  Just $ PE.UnOpExp PE.Not $ PE.CmpOpExp (PE.CmpEq $ internalisePrimType t) x y
convertBinOp Less x y (Signed t) _ =
  simpleCmpOp (PE.CmpSlt t) x y
convertBinOp Less x y (Unsigned t) _ =
  simpleCmpOp (PE.CmpUlt t) x y
convertBinOp Leq x y (Signed t) _ =
  simpleCmpOp (PE.CmpSle t) x y
convertBinOp Leq x y (Unsigned t) _ =
  simpleCmpOp (PE.CmpUle t) x y
convertBinOp Greater x y (Signed t) _ =
  simpleCmpOp (PE.CmpSlt t) y x -- Note the swapped x and y
convertBinOp Greater x y (Unsigned t) _ =
  simpleCmpOp (PE.CmpUlt t) y x -- Note the swapped x and y
convertBinOp Geq x y (Signed t) _ =
  simpleCmpOp (PE.CmpSle t) y x -- Note the swapped x and y
convertBinOp Geq x y (Unsigned t) _ =
  simpleCmpOp (PE.CmpUle t) y x -- Note the swapped x and y
convertBinOp Less x y (FloatType t) _ =
  simpleCmpOp (PE.FCmpLt t) x y
convertBinOp Leq x y (FloatType t) _ =
  simpleCmpOp (PE.FCmpLe t) x y
convertBinOp Greater x y (FloatType t) _ =
  simpleCmpOp (PE.FCmpLt t) y x -- Note the swapped x and y
convertBinOp Geq x y (FloatType t) _ =
  simpleCmpOp (PE.FCmpLe t) y x -- Note the swapped x and y
convertBinOp Less x y Bool _ =
  simpleCmpOp PE.CmpLlt x y
convertBinOp Leq x y Bool _ =
  simpleCmpOp PE.CmpLle x y
convertBinOp Greater x y Bool _ =
  simpleCmpOp PE.CmpLlt y x -- Note the swapped x and y
convertBinOp Geq x y Bool _ =
  simpleCmpOp PE.CmpLle y x -- Note the swapped x and y
convertBinOp _ _ _ _ _ = Nothing

simpleBinOp op x y = Just $ PE.BinOpExp op x y

simpleCmpOp op x y = Just $ PE.CmpOpExp op x y

expToPrimExp :: Exp -> Maybe (PrimExp VName)
expToPrimExp (Literal v _) = Just $ PE.ValueExp $ internalisePrimValue v
expToPrimExp (IntLit v (Info t) _) =
  case t of
    Scalar (Prim (Signed it)) -> Just $ PE.ValueExp $ PE.IntValue $ PE.intValue it v
    Scalar (Prim (Unsigned it)) -> Just $ PE.ValueExp $ PE.IntValue $ PE.intValue it v
    Scalar (Prim (FloatType ft)) -> Just $ PE.ValueExp $ PE.FloatValue $ PE.floatValue ft v
    _ -> Nothing
expToPrimExp (FloatLit v (Info t) _) =
  case t of
    Scalar (Prim (FloatType ft)) -> Just $ PE.ValueExp $ PE.FloatValue $ PE.floatValue ft v
    _ -> Nothing
expToPrimExp (AppExp (BinOp (op, _) _ (e_x, _) (e_y, _) _) _) = do
  x <- expToPrimExp e_x
  y <- expToPrimExp e_y
  guard $ baseTag (qualLeaf op) <= maxIntrinsicTag
  let name = baseString $ qualLeaf op
  bop <- find ((name ==) . prettyString) [minBound .. maxBound :: BinOp]
  t_x <- getPrimType $ typeOf e_x
  t_y <- getPrimType $ typeOf e_y
  convertBinOp bop x y t_x t_y
  where
    getPrimType (Scalar (Prim t)) = Just t
    getPrimType _ = Nothing
expToPrimExp _ = Nothing

checkExp :: Exp -> RefineM Bool
checkExp e =
  case expToPrimExp e of
    Just pe -> checkPrimExp pe
    Nothing -> pure False

checkPrimExp :: PrimExp VName -> RefineM Bool
checkPrimExp (PE.BinOpExp PE.LogAnd x y) =
  (&&) <$> checkPrimExp x <*> checkPrimExp y
checkPrimExp (PE.BinOpExp PE.LogOr x y) =
  (||) <$> checkPrimExp x <*> checkPrimExp y
checkPrimExp pe@(PE.CmpOpExp cop x y) = do
  (_, sop) <- toNumSoPCmp pe
  sop $>=$ zeroSoP
checkPrimExp pe = pure False

runRefineM :: VNameSource -> RefineM a -> (a, AlgEnv VName, VNameSource)
runRefineM src (RefineM m) =
  let ((a, algenv), src', _) = runRWS (runSoPMT_ m) mempty src
   in (a, algenv, src')

mkBinOp :: Name -> PatType -> Exp -> Exp -> Exp
mkBinOp op t x y =
  AppExp
    ( BinOp
        (qualName (intrinsicVar op), mempty)
        (Info t)
        (x, Info Nothing)
        (y, Info Nothing)
        mempty
    )
    (Info $ AppRes t [])

mkAnd, mkOr, mkLt, mkLe, mkEq, mkNotEq :: Exp -> Exp -> Exp
mkAnd = mkBinOp "&&" $ Scalar $ Prim Bool
mkOr = mkBinOp "||" $ Scalar $ Prim Bool
mkLt = mkBinOp "<" $ Scalar $ Prim Bool
mkLe = mkBinOp "<=" $ Scalar $ Prim Bool
mkEq = mkBinOp "==" $ Scalar $ Prim Bool
mkNotEq = mkBinOp "!=" $ Scalar $ Prim Bool

mkAdd, mkSub, mkMul, mkSQuot :: Exp -> Exp -> Exp
mkAdd = mkBinOp "+" $ Scalar $ Prim $ Signed Int64
mkSub = mkBinOp "-" $ Scalar $ Prim $ Signed Int64
mkMul = mkBinOp "*" $ Scalar $ Prim $ Signed Int64
mkSQuot = mkBinOp "%" $ Scalar $ Prim $ Signed Int64

mkSignum :: Exp -> Exp
mkSignum e =
  mkApply
    (Var (qualName (intrinsicVar "ssignum64")) (Info funt) mempty)
    [(Observe, Nothing, e)]
    (AppRes i64 [])
  where
    funt = Scalar $ Arrow mempty Unnamed Observe i64 (RetType [] i64)
    i64 = Scalar $ Prim $ Signed Int64

-- Something like a division-rounding-up, but accomodating negative
-- operands in a contrived way.
mkDivRound :: Exp -> Exp -> Exp
mkDivRound x y =
  (x `mkAdd` (y `mkSub` mkSignum y)) `mkSQuot` y

sizeInteger x = IntLit x (Info <$> Scalar $ Prim $ Signed Int64)

zero, one, negone :: Exp
zero = sizeInteger 0 mempty
one = sizeInteger 1 mempty
negone = sizeInteger (-1) mempty

considerSlice :: PatType -> Slice -> RefineM Bool
considerSlice (Array _ _ (Shape ds) _) is =
  and <$> zipWithM check ds is
  where
    inBounds d i =
      mkLe (sizeInteger 0 mempty) i `mkAnd` mkLt i d
    check _ (DimSlice Nothing Nothing Nothing) =
      pure True
    check d (DimFix i) =
      checkExp $ inBounds d i
    check d (DimSlice (Just start) (Just end) Nothing) =
      checkExp $
        inBounds d start `mkAnd` (end `mkLe` d) `mkAnd` (start `mkLe` end)
    check d (DimSlice (Just i) Nothing Nothing) =
      checkExp $
        inBounds d i
    check d (DimSlice Nothing (Just j) Nothing) =
      checkExp $
        j `mkLe` d
    check _ (DimSlice Nothing Nothing (Just stride)) =
      checkExp $
        mkNotEq stride (sizeInteger 0 mempty)
    check d (DimSlice (Just i) Nothing (Just s)) =
      checkExp $
        inBounds d i `mkAnd` (zero `mkLe` s)
    check d (DimSlice Nothing (Just j) (Just s)) =
      checkExp $
        j `mkLe` d `mkAnd` (zero `mkLe` s)
    check d (DimSlice (Just i) (Just j) (Just s)) =
      -- This case is super nasty.
      checkExp $
        nonzero_stride `mkAnd` ok_or_empty
      where
        nonzero_stride = mkNotEq s (sizeInteger 0 mempty)
        ok_or_empty = (n `mkEq` zero) `mkOr` slice_ok
        slice_ok = (backwards `mkAnd` backwards_ok) `mkOr` forwards_ok
        backwards_ok =
          (negone `mkLe` j)
            `mkAnd` (j `mkLe` i)
            `mkAnd` (zero `mkLe` i_p_m_t_s)
            `mkAnd` (i_p_m_t_s `mkLe` d)
        forwards_ok =
          (zero `mkLe` i)
            `mkAnd` (i `mkLe` j)
            `mkAnd` (zero `mkLe` i_p_m_t_s)
            `mkAnd` (i_p_m_t_s `mkLt` d)
        backwards = mkSignum s `mkEq` negone
        i_p_m_t_s = i `mkAdd` mkMul m s
        m = n `mkSub` one
        n = (j `mkSub` i) `mkDivRound` s
considerSlice t _ = error $ "considerSlice: not an array " <> show t

mkUnsafe :: Exp -> Exp
mkUnsafe e =
  Attr (AttrAtom (AtomName "unsafe") mempty) e mempty

transformExp :: Exp -> RefineM Exp
transformExp (Assert cond e t loc) = do
  e' <- transformExp e
  safe <- checkExp cond
  if safe
    then pure e'
    else pure $ Assert cond e' t loc
transformExp e@(AppExp (Index arr slice loc) res) = do
  arr' <- transformExp arr
  b <- considerSlice (typeOf arr) slice
  let e' = AppExp (Index arr' slice loc) res
  if b
    then pure $ mkUnsafe e'
    else pure e'
transformExp e = pure e

transformValBind :: ValBind -> RefineM ValBind
transformValBind vb = do
  body <- transformExp $ valBindBody vb
  pure $ vb {valBindBody = body}

transformDec :: Dec -> RefineM Dec
transformDec (ValDec vb) = ValDec <$> transformValBind vb
transformDec d = pure d

transformImport :: (ImportName, FileModule) -> RefineM (ImportName, FileModule)
transformImport (name, imp) = do
  let p = fileProg imp
  decs <- mapM transformDec $ progDecs p
  pure $ (name, imp {fileProg = p {progDecs = decs}})

transformProg :: MonadFreshNames m => Imports -> m Imports
transformProg prog = modifyNameSource $ \namesrc ->
  let (prog', _, namesrc') = runRefineM namesrc $ mapM transformImport prog
   in (prog', namesrc')
