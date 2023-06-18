module Futhark.Analysis.Refinement (refineProg) where

import Control.Monad
import Control.Monad.RWS (MonadReader (..), MonadWriter (..), RWS, asks, lift, runRWS)
import Data.List (find)
import Data.Maybe
import Debug.Trace
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
  = RefineM (SoPMT VName Exp (RWS Env [String] VNameSource) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadSoP VName Exp,
      MonadWriter [String]
    )

instance MonadFreshNames RefineM where
  getNameSource = RefineM $ getNameSource
  putNameSource = RefineM . putNameSource

data Constraint
  = (:<:) (SoP VName) (SoP VName)
  | (:<=:) (SoP VName) (SoP VName)
  | (:>:) (SoP VName) (SoP VName)
  | (:>=:) (SoP VName) (SoP VName)
  | (:==:) (SoP VName) (SoP VName)
  | (:/=:) (SoP VName) (SoP VName)
  | (:&&:) Constraint Constraint
  | (:||:) Constraint Constraint
  deriving (Show)

infixr 4 :<:

infixr 4 :<=:

infixr 4 :>:

infixr 4 :>=:

infixr 4 :==:

infixr 4 :/=:

infixr 3 :&&:

infixr 2 :||:

instance Pretty Constraint where
  pretty c =
    case c of
      x :<: y -> op "<" x y
      x :<=: y -> op "<=" x y
      x :>: y -> op ">" x y
      x :>=: y -> op ">=" x y
      x :==: y -> op "==" x y
      x :/=: y -> op "/=" x y
      x :&&: y -> op "&&" x y
      x :||: y -> op "||" x y
    where
      op s x y = pretty x <+> s <+> pretty y

checkConstraint :: Constraint -> RefineM Bool
checkConstraint (x :<: y) = x $<$ y
checkConstraint (x :<=: y) = x $<=$ y
checkConstraint (x :>: y) = x $>$ y
checkConstraint (x :>=: y) = x $>=$ y
checkConstraint (x :==: y) = x $==$ y
checkConstraint (x :/=: y) = x $/=$ y
checkConstraint (x :&&: y) = checkConstraint x ^&& checkConstraint y
checkConstraint (x :||: y) = checkConstraint x ^|| checkConstraint y

mustBeTrue :: Loc -> Constraint -> RefineM Bool
mustBeTrue loc c = do
  b <- checkConstraint c
  unless b $
    tell
      [prettyString (locText loc) <> ": " <> prettyString c]
  pure b

checkExp :: Exp -> RefineM Bool
checkExp e = do
  (_, sop) <- toSoPCmp e
  sop $>=$ zeroSoP

runRefineM :: VNameSource -> RefineM a -> (a, AlgEnv VName Exp, VNameSource, [String])
runRefineM src (RefineM m) =
  let ((a, algenv), src', w) = runRWS (runSoPMT_ m) mempty src
   in (a, algenv, src', w)

considerSlice :: PatType -> Slice -> RefineM Bool
considerSlice (Array _ _ (Shape ds) _) is =
  and <$> zipWithM check ds is
  where
    inBounds :: SoP VName -> SoP VName -> Constraint
    inBounds d' i' =
      zeroSoP :<=: i' :&&: i' :<: d'
    check :: Size -> DimIndexBase Info VName -> RefineM Bool
    check _ (DimSlice Nothing Nothing Nothing) =
      pure True
    check d (DimFix i) = do
      d' <- toSoPNum_ d
      i' <- toSoPNum_ i
      mustBeTrue (locOf i) $
        inBounds d' i'
    check d (DimSlice (Just start) (Just end) Nothing) = do
      d' <- toSoPNum_ d
      start' <- toSoPNum_ start
      end' <- toSoPNum_ end
      mustBeTrue (locOf start) $
        inBounds d' start'
          :&&: end'
          :<=: d'
          :&&: start'
          :<=: end'
    check d (DimSlice (Just i) Nothing Nothing) = do
      d' <- toSoPNum_ d
      i' <- toSoPNum_ i
      mustBeTrue (locOf i) $
        inBounds d' i'
    check d (DimSlice Nothing (Just j) Nothing) = do
      d' <- toSoPNum_ d
      j' <- toSoPNum_ j
      mustBeTrue (locOf j) $
        j' :<=: d'
    check _ (DimSlice Nothing Nothing (Just stride)) = do
      stride' <- toSoPNum_ stride
      mustBeTrue (locOf stride) $
        stride' :/=: zeroSoP
    check d (DimSlice (Just i) Nothing (Just s)) = do
      d' <- toSoPNum_ d
      i' <- toSoPNum_ i
      s' <- toSoPNum_ s
      mustBeTrue (locOf i) $
        inBounds d' i'
          :&&: zeroSoP
          :<=: s'
    check d (DimSlice Nothing (Just j) (Just s)) = do
      d' <- toSoPNum_ d
      j' <- toSoPNum_ j
      s' <- toSoPNum_ s
      mustBeTrue (locOf j) $
        j'
          :<=: d'
          :&&: zeroSoP
          :<=: s'
    check d (DimSlice (Just i) (Just j) (Just s)) = do
      d' <- toSoPNum_ d
      i' <- toSoPNum_ i
      j' <- toSoPNum_ j
      s' <- toSoPNum_ s
      let nonzero_stride = s' :/=: zeroSoP
          ok_or_empty = n :==: zeroSoP :||: slice_ok
          slice_ok = backwards :&&: backwards_ok :||: forwards_ok
          backwards_ok =
            int2SoP (-1)
              :<=: j'
              :&&: j'
              :<=: i'
              :&&: zeroSoP
              :<=: i_p_m_t_s
              :&&: i_p_m_t_s
              :<=: d'
          forwards_ok =
            zeroSoP
              :<=: i'
              :&&: i'
              :<=: j'
              :&&: zeroSoP
              :<=: i_p_m_t_s
              :&&: i_p_m_t_s
              :<: d'
          backwards = fromJust (signumSoP s') :==: int2SoP (-1)
          i_p_m_t_s = i' .+. m .*. s'
          m = n .-. int2SoP 1
          n = fromJust $ (j' .-. i') `divSoPInt` s'
      mustBeTrue (locOf i) $
        nonzero_stride
          :&&: ok_or_empty
considerSlice t _ = error $ "considerSlice: not an array " <> show t

mkUnsafe :: Exp -> Exp
mkUnsafe e =
  Attr (AttrAtom (AtomName "unsafe") mempty) e mempty

refineExp :: Exp -> RefineM Exp
refineExp (Assert cond e t loc) = do
  e' <- refineExp e
  safe <- checkExp cond
  if safe
    then pure e'
    else pure $ Assert cond e' t loc
refineExp e@(AppExp (Index arr slice loc) res) = do
  arr' <- refineExp arr
  b <- considerSlice (typeOf arr) slice
  let e' = AppExp (Index arr' slice loc) res
  if b
    then pure $ mkUnsafe e'
    else do
      pure e'
refineExp e = pure e

refineValBind :: ValBind -> RefineM ValBind
refineValBind vb = do
  body <- refineExp $ valBindBody vb
  pure $ vb {valBindBody = body}

refineDec :: Dec -> RefineM Dec
refineDec (ValDec vb) = ValDec <$> refineValBind vb
refineDec d = pure d

refineImport :: (ImportName, FileModule) -> RefineM (ImportName, FileModule)
refineImport (name, imp) = do
  let p = fileProg imp
  decs <- mapM refineDec $ progDecs p
  pure $ (name, imp {fileProg = p {progDecs = decs}})

refineProg :: VNameSource -> Imports -> (Imports, VNameSource, [String])
refineProg namesrc prog =
  let (prog', _, namesrc', ws) = runRefineM namesrc $ mapM refineImport prog
   in (prog', namesrc', ws)
