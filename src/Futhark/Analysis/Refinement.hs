module Futhark.Analysis.Refinement (refineProg) where

import Control.Monad
import Control.Monad.RWS hiding (Sum)
import Data.Either
import Data.List (find, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace
import Futhark.Analysis.PrimExp (PrimExp)
import Futhark.Analysis.PrimExp qualified as PE
import Futhark.Internalise.TypesValues (internalisePrimType, internalisePrimValue)
import Futhark.MonadFreshNames
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
import Language.Futhark.Tuple

data Env = Env
  { scope :: Set VName
  }

type Log = Either String String

data SEnv = SEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv VName Exp,
    valbinds :: Map VName ValBind
  }

newtype RefineM a
  = RefineM (RWS Env [Log] SEnv a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter [Log],
      MonadState SEnv,
      MonadFreshNames
    )

instance MonadFreshNames (RWS Env [Log] SEnv) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

instance MonadSoP VName Exp RefineM where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

runRefineM :: VNameSource -> RefineM a -> (a, AlgEnv VName Exp, VNameSource, [String], [String])
runRefineM src (RefineM m) =
  let (a, s, w) = runRWS m (Env mempty) (SEnv src mempty mempty)
   in (a, algenv s, vnamesource s, lefts w, rights w)

refineProg :: VNameSource -> Imports -> (VNameSource, AlgEnv VName Exp, [String], [String])
refineProg namesrc prog =
  let (_, algenv, namesrc', failed, checked) = runRefineM namesrc $ refineImports prog
   in (namesrc', algenv, failed, checked)

refineImports :: [(ImportName, FileModule)] -> RefineM ()
refineImports [] = pure ()
refineImports ((name, imp) : imps) = do
  refineDecs $ progDecs $ fileProg imp
  refineImports imps

refineDecs :: [Dec] -> RefineM ()
refineDecs [] = pure ()
refineDecs (ValDec vb : rest) = do
  refineValBind vb
  refineDecs rest
refineDecs (d : ds) = refineDecs ds

refineValBind :: ValBind -> RefineM ()
refineValBind vb = do
  modify $ \env -> env {valbinds = M.insert (valBindName vb) vb $ valbinds env}
  refineFunc $ valBindBody vb

refineFunc :: Exp -> RefineM ()
refineFunc f = do
  collectExp f
  checkExp f

-----------------------------------------------------------------
-- Collecting
--
-- This is the "forward" pass that traverses a function and populates
-- the algebraic environment with basic equalities and ranges.
--
-- No checking is done here; only information gathering.
-----------------------------------------------------------------

collectExp :: Exp -> RefineM ()
collectExp e'@(AppExp (LetPat _ p e body _) _) = do
  collectExp e
  addPat p e
  collectExp body
collectExp e@(AppExp (Apply f args _) _)
  | f `isFun` "iota",
    [(_, n)] <- NE.toList args,
    (Array _ _ (Shape (m : _)) _) <- typeOf e = do
      n' <- toSoPNum_ n
      m' <- toSoPNum_ m
      addRel $ zeroSoP :<=: m' :&&: m' :<: n'
collectExp (AppExp (DoLoop sizes p e (For (Ident i _ _) iters) body _) _) = do
  iters' <- toSoPNum_ iters
  addRel $ zeroSoP :<=: sym2SoP i :&&: sym2SoP i :<: iters'
collectExp e = void $ astMap collectMapper e

collectMapper :: ASTMapper RefineM
collectMapper = identityMapper {mapOnExp = \e' -> collectExp e' >> pure e'}

-- A total hack
isFun :: Exp -> String -> Bool
isFun (Var (QualName [] vn) _ _) fname = fname == baseString vn
isFun _ _ = False

addPat :: Pat -> Exp -> RefineM ()
addPat (PatParens p _) e = addPat p e
addPat (PatAscription p _ _) e = addPat p e
addPat p (Parens e _) = addPat p e
addPat (Id x _ _) e = do
  e' <- toSoPNum_ e
  addRel $ sym2SoP x :==: e'
addPat (TuplePat ps _) (TupLit es _) = zipWithM_ addPat ps es
addPat _ _ = pure ()

-----------------------------------------------------------------
-- Checking
--
-- This is the "reverse" pass that traverses a function bottom-up and
-- checks post-conditions (which don't exist yet...) along with things
-- like access bounds.
-----------------------------------------------------------------
checkExp :: Exp -> RefineM ()
checkExp (AppExp (Index e slice loc) res) = do
  checkExp e
  checkSlice (typeOf e) slice
checkExp (Update e slice v _) = do
  checkExp e
  checkSlice (typeOf e) slice
  checkExp v
checkExp (Coerce e _ (Info t) loc) = do
  checkExp e
  checkCoerce (locOf loc) (typeOf e) t
checkExp (AppExp (Index e slice _) _) = do
  checkExp e
  void $ astMap checkMapper slice
  checkSlice (typeOf e) slice
checkExp (AppExp (LetWith _ src slice e body _) _) = do
  checkExp e
  checkSlice (unInfo $ identType src) slice
  checkExp body
checkExp (AppExp (E.Range start stride end loc) _) = do
  checkExp start
  void $ traverse checkExp stride
  void $ traverse checkExp end
  checkRange (locOf loc) start stride end
checkExp e'@(AppExp (LetPat _ p e body _) _) = do
  checkExp body
  checkExp e
checkExp e = void $ astMap checkMapper e

checkMapper :: ASTMapper RefineM
checkMapper = identityMapper {mapOnExp = \e' -> checkExp e' >> pure e'}

checkSlice :: PatType -> Slice -> RefineM ()
checkSlice (Array _ _ (Shape ds) _) is =
  zipWithM_ check ds is
  where
    inBounds :: SoP VName -> SoP VName -> Rel VName
    inBounds d' i' =
      andRel
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
        andRel
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
        andRel
          [ inBounds d' i',
            zeroSoP :<=: s'
          ]
    check d (DimSlice Nothing (Just j) (Just s)) = do
      d' <- toSoPNum_ d
      j' <- toSoPNum_ j
      s' <- toSoPNum_ s
      mustBeTrue (locOf j) $
        andRel
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
            andRel
              [ int2SoP (-1) :<=: j',
                j' :<=: i',
                zeroSoP :<=: i_p_m_t_s,
                i_p_m_t_s :<=: d'
              ]
          forwards_ok =
            andRel
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
checkSlice t _ = error $ "checkSlice: not an array " <> show t

checkRange :: Loc -> Exp -> Maybe Exp -> Inclusiveness Exp -> RefineM ()
checkRange loc start Nothing (ToInclusive end) = do
  start' <- toSoPNum_ start
  end' <- toSoPNum_ end
  mustBeTrue loc $ start' :<=: end'
checkRange loc start Nothing (UpToExclusive end) = do
  start' <- toSoPNum_ start
  end' <- toSoPNum_ end
  mustBeTrue loc $ start' :<=: end'
checkRange loc start Nothing (DownToExclusive end) = do
  start' <- toSoPNum_ start
  end' <- toSoPNum_ end
  mustBeTrue loc $ end' :<=: start'
checkRange loc start (Just stride) (ToInclusive end) = do
  start' <- toSoPNum_ start
  stride' <- toSoPNum_ stride
  end' <- toSoPNum_ end
  mustBeTrue loc $
    (zeroSoP :<=: stride')
      :&&: (start' :<=: end')
      :||: (end' :<=: start')
checkRange loc start (Just stride) (UpToExclusive end) = do
  start' <- toSoPNum_ start
  stride' <- toSoPNum_ stride
  end' <- toSoPNum_ end
  mustBeTrue loc $
    (start' :<=: end')
      :&&: (zeroSoP :<: stride')
checkRange loc start (Just stride) (DownToExclusive end) = do
  start' <- toSoPNum_ start
  stride' <- toSoPNum_ stride
  end' <- toSoPNum_ end
  mustBeTrue loc $
    (end' :<=: start')
      :&&: (stride' :<: zeroSoP)

checkCoerce :: Loc -> PatType -> PatType -> RefineM ()
checkCoerce loc t1 t2 = void $ matchDims onDims t1 t2
  where
    onDims _bound d1 d2
      | d1 /= d2 = do
          d1' <- toSoPNum_ d1
          d2' <- toSoPNum_ d2
          mustBeTrue loc $ d1' :==: d2'
          pure d1
    onDims _ d1 _ =
      pure d1

mustBeTrue :: Loc -> Rel VName -> RefineM ()
mustBeTrue loc c = do
  b <- checkRel c
  if b
    then
      tell
        [Right $ prettyString (locText loc) <> ": " <> prettyString c]
    else
      tell
        [Left $ prettyString (locText loc) <> ": " <> prettyString c]

checkRel :: MonadSoP u e m => Rel u -> m Bool
checkRel (x :<: y) = x $<$ y
checkRel (x :<=: y) = x $<=$ y
checkRel (x :>: y) = x $>$ y
checkRel (x :>=: y) = x $>=$ y
checkRel (x :==: y) = x $==$ y
checkRel (x :/=: y) = x $/=$ y
checkRel (x :&&: y) = checkRel x ^&& checkRel y
checkRel (x :||: y) = checkRel x ^|| checkRel y
