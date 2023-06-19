module Futhark.Analysis.Refinement (refineProg) where

import Control.Monad
import Control.Monad.RWS (MonadReader (..), MonadWriter (..), RWS, asks, lift, runRWS)
import Data.Either
import Data.List (find)
import Data.Maybe
import Debug.Trace
import Futhark.Analysis.PrimExp (PrimExp)
import Futhark.Analysis.PrimExp qualified as PE
import Futhark.Internalise.TypesValues (internalisePrimType, internalisePrimValue)
import Futhark.MonadFreshNames
import Futhark.SoP.Constraint
import Futhark.SoP.Convert
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.Refine
import Futhark.SoP.RefineEquivs
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark hiding (Range)
import Language.Futhark qualified as E
import Language.Futhark.Prop
import Language.Futhark.Semantic hiding (Env)
import Language.Futhark.Traversals

type Env = ()

type Log = Either String String

newtype RefineM a
  = RefineM (SoPMT VName Exp (RWS Env [Log] VNameSource) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadSoP VName Exp,
      MonadWriter [Log]
    )

instance MonadFreshNames RefineM where
  getNameSource = RefineM $ getNameSource
  putNameSource = RefineM . putNameSource

checkConstraint :: Constraint VName -> RefineM Bool
checkConstraint (x :<: y) = x $<$ y
checkConstraint (x :<=: y) = x $<=$ y
checkConstraint (x :>: y) = x $>$ y
checkConstraint (x :>=: y) = x $>=$ y
checkConstraint (x :==: y) = x $==$ y
checkConstraint (x :/=: y) = x $/=$ y
checkConstraint (x :&&: y) = checkConstraint x ^&& checkConstraint y
checkConstraint (x :||: y) = checkConstraint x ^|| checkConstraint y

mustBeTrue :: Loc -> Constraint VName -> RefineM ()
mustBeTrue loc c = do
  b <- checkConstraint c
  if b
    then
      tell
        [Right $ prettyString (locText loc) <> ": " <> prettyString c]
    else
      tell
        [Left $ prettyString (locText loc) <> ": " <> prettyString c]

-- checkCmpExp :: Exp -> RefineM ()
-- checkCmpExp e = do
-- (_, sop) <- toSoPCmp e

runRefineM :: VNameSource -> RefineM a -> (a, AlgEnv VName Exp, VNameSource, [String], [String])
runRefineM src (RefineM m) =
  let ((a, algenv), src', w) = runRWS (runSoPMT_ m) mempty src
   in (a, algenv, src', lefts w, rights w)

considerSlice :: PatType -> Slice -> RefineM ()
considerSlice (Array _ _ (Shape ds) _) is =
  zipWithM_ check ds is
  where
    inBounds :: SoP VName -> SoP VName -> Constraint VName
    inBounds d' i' =
      andC
        [ zeroSoP :<=: i',
          i' :<: d'
        ]
    check :: Size -> DimIndexBase Info VName -> RefineM ()
    check _ (DimSlice Nothing Nothing Nothing) =
      pure ()
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
        andC
          [ inBounds d' start',
            end' :<=: d',
            start' :<=: end'
          ]
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
        andC
          [ inBounds d' i',
            zeroSoP :<=: s'
          ]
    check d (DimSlice Nothing (Just j) (Just s)) = do
      d' <- toSoPNum_ d
      j' <- toSoPNum_ j
      s' <- toSoPNum_ s
      mustBeTrue (locOf j) $
        andC
          [ j' :<=: d',
            zeroSoP :<=: s'
          ]
    check d (DimSlice (Just i) (Just j) (Just s)) = do
      d' <- toSoPNum_ d
      i' <- toSoPNum_ i
      j' <- toSoPNum_ j
      s' <- toSoPNum_ s
      let nonzero_stride = s' :/=: zeroSoP
          ok_or_empty = n :==: zeroSoP :||: slice_ok
          slice_ok = backwards :&&: backwards_ok :||: forwards_ok
          backwards_ok =
            andC
              [ int2SoP (-1) :<=: j',
                j' :<=: i',
                zeroSoP :<=: i_p_m_t_s,
                i_p_m_t_s :<=: d'
              ]
          forwards_ok =
            andC
              [ zeroSoP :<=: i',
                i' :<=: j',
                zeroSoP :<=: i_p_m_t_s,
                i_p_m_t_s :<: d'
              ]
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

considerCoerce :: Loc -> PatType -> PatType -> RefineM ()
considerCoerce loc t1 t2 = void $ matchDims onDims t1 t2
  where
    onDims _bound d1 d2
      | d1 /= d2 = do
          d1' <- toSoPNum_ d1
          d2' <- toSoPNum_ d2
          mustBeTrue loc $ d1' :==: d2'
          pure d1
    onDims _ d1 _ =
      pure d1

considerRange :: Loc -> Exp -> Maybe Exp -> Inclusiveness Exp -> RefineM ()
considerRange loc start Nothing (ToInclusive end) = do
  start' <- toSoPNum_ start
  end' <- toSoPNum_ end
  mustBeTrue loc $ start' :<=: end'
considerRange loc start Nothing (UpToExclusive end) = do
  start' <- toSoPNum_ start
  end' <- toSoPNum_ end
  mustBeTrue loc $ start' :<=: end'
considerRange loc start Nothing (DownToExclusive end) = do
  start' <- toSoPNum_ start
  end' <- toSoPNum_ end
  mustBeTrue loc $ end' :<=: start'
considerRange loc start (Just stride) (ToInclusive end) = do
  start' <- toSoPNum_ start
  stride' <- toSoPNum_ stride
  end' <- toSoPNum_ end
  mustBeTrue loc $
    (zeroSoP :<=: stride')
      :&&: (start' :<=: end')
      :||: (end' :<=: start')
considerRange loc start (Just stride) (UpToExclusive end) = do
  start' <- toSoPNum_ start
  stride' <- toSoPNum_ stride
  end' <- toSoPNum_ end
  mustBeTrue loc $
    (start' :<=: end')
      :&&: (zeroSoP :<: stride')
considerRange loc start (Just stride) (DownToExclusive end) = do
  start' <- toSoPNum_ start
  stride' <- toSoPNum_ stride
  end' <- toSoPNum_ end
  mustBeTrue loc $
    (end' :<=: start')
      :&&: (stride' :<: zeroSoP)

expMapper :: ASTMapper RefineM
expMapper = identityMapper {mapOnExp = \e' -> refineExp e' >> pure e'}

refineExp :: Exp -> RefineM ()
-- refineExp (Assert cond e t loc) = do
-- (_, sop) <- toSoPCmp e
-- mustBeTrue loc $ sop :>=: zeroSoP
refineExp (AppExp (Index e slice loc) res) = do
  refineExp e
  considerSlice (typeOf e) slice
refineExp (Update e slice v _) = do
  refineExp e
  considerSlice (typeOf e) slice
  refineExp v
refineExp (Coerce e _ (Info t) loc) = do
  refineExp e
  considerCoerce (locOf loc) (typeOf e) t
refineExp (AppExp (Index e slice _) _) = do
  refineExp e
  void $ astMap expMapper slice
  considerSlice (typeOf e) slice
refineExp (AppExp (LetWith _ src slice e body _) _) = do
  refineExp e
  considerSlice (unInfo $ identType src) slice
  refineExp body
refineExp (AppExp (E.Range start stride end loc) _) = do
  refineExp start
  void $ traverse refineExp stride
  void $ traverse refineExp end
  considerRange (locOf loc) start stride end
refineExp e'@(AppExp (LetPat _ (Id x _ _) e body _) _) = do
  refineExp e
  e' <- toSoPNum_ e
  addConstraint $ sym2SoP x :==: e'
  refineExp body
refineExp e = void $ astMap expMapper e

refineValBind :: ValBind -> RefineM ()
refineValBind = refineExp . valBindBody

refineDec :: Dec -> RefineM ()
refineDec (ValDec vb) = refineValBind vb
refineDec d = pure ()

refineImport :: (ImportName, FileModule) -> RefineM ()
refineImport (name, imp) = do
  let p = fileProg imp
  mapM_ refineDec $ progDecs p

refineProg :: VNameSource -> Imports -> (VNameSource, AlgEnv VName Exp, [String], [String])
refineProg namesrc prog =
  let (_, algenv, namesrc', failed, checked) = runRefineM namesrc $ mapM refineImport prog
   in (namesrc', algenv, failed, checked)
