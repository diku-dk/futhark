{-# LANGUAGE FlexibleContexts #-}
-- | Facilities for answering queries about a program, such as "what
-- appears at this source location", or "where is this name bound".
-- The intent is that this is used as a building block for IDE-like
-- functionality.
module Language.Futhark.Query
  ( BoundTo(..)
  , boundLoc
  , allBindings
  , AtPos(..)
  , atPos
  , Pos(..)
  )
  where

import Control.Monad
import Control.Monad.State
import Data.List (find)
import Data.Loc (SrcLoc, Pos(..), Located(..), Loc(..))
import qualified Data.Map as M
import qualified System.FilePath.Posix as Posix

import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.Traversals

-- | What a name is bound to.
data BoundTo = BoundTerm StructType Loc -- ^ Term-level variable.
  deriving (Eq, Show)

-- | Where was a bound variable actually bound?  That is, what is the
-- location of its definition?
boundLoc :: BoundTo -> Loc
boundLoc (BoundTerm _ loc) = loc

patternBindings :: Pattern -> M.Map VName BoundTo
patternBindings (Id vn (Info t) loc) =
  M.singleton vn $ BoundTerm (toStruct t) (locOf loc)
patternBindings (TuplePattern pats _) =
  mconcat $ map patternBindings pats
patternBindings (RecordPattern fields _) =
  mconcat $ map (patternBindings . snd) fields
patternBindings (PatternParens pat _) =
  patternBindings pat
patternBindings Wildcard{} = mempty
patternBindings PatternLit{} = mempty
patternBindings (PatternAscription pat _ _) =
  patternBindings pat
patternBindings (PatternConstr _ _ pats _) =
  mconcat $ map patternBindings pats

expBindings :: Exp -> M.Map VName BoundTo
expBindings (LetPat pat e1 e2 _ _) =
  patternBindings pat <> expBindings e1 <> expBindings e2
expBindings e =
  execState (astMap mapper e) mempty
  where mapper = ASTMapper { mapOnExp = onExp
                           , mapOnName = pure
                           , mapOnQualName = pure
                           , mapOnStructType = pure
                           , mapOnPatternType = pure
                           }
        onExp e' = do
          modify (<>expBindings e')
          return e'

valBindBindings :: ValBind -> M.Map VName BoundTo
valBindBindings vbind =
  M.insert (valBindName vbind) (BoundTerm vbind_t (locOf vbind)) $
  mconcat (map patternBindings (valBindParams vbind)) <>
  expBindings (valBindBody vbind)
  where vbind_t =
          foldFunType (map patternStructType (valBindParams vbind)) $
          unInfo $ valBindRetType vbind

decBindings :: Dec -> M.Map VName BoundTo
decBindings (ValDec vbind) = valBindBindings vbind
decBindings _ = mempty

-- | All bindings of everything in the program.
progBindings :: Prog -> M.Map VName BoundTo
progBindings = mconcat . map decBindings . progDecs

allBindings :: Imports -> M.Map VName BoundTo
allBindings = mconcat . map (progBindings . fileProg . snd)

data RawAtPos = RawAtName (QualName VName) SrcLoc

contains :: Located a => a -> Pos -> Bool
contains a pos =
  case locOf a of
    Loc start end -> pos >= start && pos <= end
    NoLoc -> False

atPosInExp :: Exp -> Pos -> Maybe RawAtPos
atPosInExp (Var qn _ loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName qn loc

-- All the value cases are TODO - we need another RawAtPos constructor.
atPosInExp Literal{} _ = Nothing
atPosInExp IntLit{} _ = Nothing
atPosInExp FloatLit{} _ = Nothing

atPosInExp e pos =
  -- Use the Either monad for short-circuiting for efficiency reasons.
  -- The first hit is going to be the only one.
  case astMap mapper e of
    Left atpos -> Just atpos
    Right _ -> Nothing
  where mapper = ASTMapper { mapOnExp = onExp
                           , mapOnName = pure
                           , mapOnQualName = pure
                           , mapOnStructType = pure
                           , mapOnPatternType = pure
                           }
        onExp e' =
          case atPosInExp e' pos of
            Just atpos -> Left atpos
            Nothing -> Right e'

atPosInValBind :: ValBind -> Pos -> Maybe RawAtPos
atPosInValBind = atPosInExp . valBindBody

atPosInDec :: Dec -> Pos -> Maybe RawAtPos
atPosInDec dec pos = do
  guard $ dec `contains` pos
  case dec of
    ValDec vbind -> atPosInValBind vbind pos
    _ -> Nothing

atPosInProg :: Prog -> Pos -> Maybe RawAtPos
atPosInProg prog pos =
  msum $ map (`atPosInDec` pos) (progDecs prog)

containingModule :: Imports -> Pos -> Maybe FileModule
containingModule imports (Pos file _ _ _) =
  snd <$> find ((==file') . fst) imports
  where file' = includeToString $ mkInitialImport $
                fst $ Posix.splitExtension file

-- | Information about what is at the given source location.
data AtPos = AtName (QualName VName) (Maybe BoundTo) SrcLoc
  deriving (Eq, Show)

-- | Information about what's at the given source position.  Returns
-- 'Nothing' if there is nothing there, including if the source
-- position is invalid.
atPos :: Imports -> Pos -> Maybe AtPos
atPos imports pos = do
  prog <- fileProg <$> containingModule imports pos
  RawAtName qn loc <- atPosInProg prog pos
  Just $ AtName qn (qualLeaf qn `M.lookup` allBindings imports) loc
