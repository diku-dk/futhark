module Futhark.Internalise.Refinement (transformProg) where

import Control.Monad
import Control.Monad.RWS (MonadReader (..), MonadWriter (..), RWS, asks, lift, runRWS)
import Data.List (find)
import Futhark.Analysis.PrimExp (PrimExp)
import Futhark.Analysis.PrimExp qualified as PE
import Futhark.Internalise.TypesValues (internalisePrimType, internalisePrimValue)
import Futhark.MonadFreshNames
import Futhark.SoP.Convert
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.Refine
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Prop
import Language.Futhark.Semantic hiding (Env)

type Env = ()

newtype RefineM a
  = RefineM (SoPMT VName Exp (RWS Env () VNameSource) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadSoP VName Exp
    )

instance MonadFreshNames RefineM where
  getNameSource = RefineM $ getNameSource
  putNameSource = RefineM . putNameSource

checkExp :: Exp -> RefineM Bool
checkExp e = do
  (_, sop) <- toSoPCmp e
  sop $>=$ zeroSoP

runRefineM :: VNameSource -> RefineM a -> (a, AlgEnv VName Exp, VNameSource)
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
