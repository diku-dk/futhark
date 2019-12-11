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

typeParamBindings :: TypeParamBase VName -> M.Map VName BoundTo
typeParamBindings (TypeParamDim vn loc) =
  M.singleton vn $ BoundTerm (Scalar $ Prim $ Signed Int32) (locOf loc)
typeParamBindings TypeParamType{} =
  mempty

expBindings :: Exp -> M.Map VName BoundTo
expBindings (LetPat pat e1 e2 _ _) =
  patternBindings pat <> expBindings e1 <> expBindings e2
expBindings (Lambda params body _ _ _) =
  mconcat (map patternBindings params) <> expBindings body
expBindings (LetFun name (tparams, params, _, Info ret,  e1) e2 loc) =
  M.singleton name (BoundTerm name_t (locOf loc)) <>
  mconcat (map typeParamBindings tparams) <>
  mconcat (map patternBindings params) <>
  expBindings e1 <> expBindings e2
  where name_t = foldFunType (map patternStructType params) ret
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
  mconcat (map typeParamBindings (valBindTypeParams vbind)) <>
  mconcat (map patternBindings (valBindParams vbind)) <>
  expBindings (valBindBody vbind)
  where vbind_t =
          foldFunType (map patternStructType (valBindParams vbind)) $
          unInfo $ valBindRetType vbind

modExpBindings :: ModExp -> M.Map VName BoundTo
modExpBindings ModVar{} =
  mempty
modExpBindings (ModParens me _) =
  modExpBindings me
modExpBindings ModImport{} =
  mempty
modExpBindings (ModDecs decs _) =
  mconcat $ map decBindings decs
modExpBindings (ModApply e1 e2 _ _ _) =
  modExpBindings e1 <> modExpBindings e2
modExpBindings (ModAscript e _ _ _) =
  modExpBindings e
modExpBindings (ModLambda _ _ e _) =
  modExpBindings e

modBindBindings :: ModBind -> M.Map VName BoundTo
modBindBindings = modExpBindings . modExp

decBindings :: Dec -> M.Map VName BoundTo
decBindings (ValDec vbind) = valBindBindings vbind
decBindings (ModDec mbind) = modBindBindings mbind
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

atPosInTypeExp :: TypeExp VName -> Pos -> Maybe RawAtPos
atPosInTypeExp te pos =
  case te of
    TEVar qn loc -> do
      guard $ loc `contains` pos
      Just $ RawAtName qn loc
    TETuple es _ ->
      msum $ map (`atPosInTypeExp` pos) es
    TERecord fields _ ->
      msum $ map ((`atPosInTypeExp` pos) . snd) fields
    TEArray te' dim loc ->
      atPosInTypeExp te' pos `mplus` inDim dim loc
    TEUnique te' _ ->
      atPosInTypeExp te' pos
    TEApply e1 arg _ ->
      atPosInTypeExp e1 pos `mplus` inArg arg
    TEArrow _ e1 e2 _ ->
      atPosInTypeExp e1 pos `mplus` atPosInTypeExp e2 pos
    TESum cs _ ->
      msum $ map (`atPosInTypeExp` pos) $ concatMap snd cs
  where inArg (TypeArgExpDim dim loc) = inDim dim loc
        inArg (TypeArgExpType e2) = atPosInTypeExp e2 pos
        inDim (NamedDim qn) loc = do
          guard $ loc `contains` pos
          Just $ RawAtName qn loc
        inDim _ _ = Nothing

atPosInPattern :: Pattern -> Pos -> Maybe RawAtPos
atPosInPattern (Id vn _ loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName (qualName vn) loc
atPosInPattern (TuplePattern pats _) pos =
  msum $ map (`atPosInPattern` pos) pats
atPosInPattern (RecordPattern fields _) pos =
  msum $ map ((`atPosInPattern` pos) . snd) fields
atPosInPattern (PatternParens pat _) pos =
  atPosInPattern pat pos
atPosInPattern (PatternAscription pat tdecl _) pos =
  atPosInPattern pat pos `mplus` atPosInTypeExp (declaredType tdecl) pos
atPosInPattern (PatternConstr _ _ pats _) pos =
  msum $ map (`atPosInPattern` pos) pats
atPosInPattern PatternLit{} _ = Nothing
atPosInPattern Wildcard{} _ = Nothing

atPosInExp :: Exp -> Pos -> Maybe RawAtPos
atPosInExp (Var qn _ loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName qn loc

-- All the value cases are TODO - we need another RawAtPos constructor.
atPosInExp Literal{} _ = Nothing
atPosInExp IntLit{} _ = Nothing
atPosInExp FloatLit{} _ = Nothing

atPosInExp (LetPat pat _ _ _ _) pos
  | pat `contains` pos = atPosInPattern pat pos

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

atPosInModExp :: ModExp -> Pos -> Maybe RawAtPos
atPosInModExp (ModVar qn loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName qn loc
atPosInModExp (ModParens me _) pos =
  atPosInModExp me pos
atPosInModExp ModImport{} _ =
  Nothing
atPosInModExp (ModDecs decs _) pos =
  msum $ map (`atPosInDec` pos) decs
atPosInModExp (ModApply e1 e2 _ _ _) pos =
  atPosInModExp e1 pos `mplus` atPosInModExp e2 pos
atPosInModExp (ModAscript e _ _ _) pos =
  atPosInModExp e pos
atPosInModExp (ModLambda _ _ e _) pos =
  atPosInModExp e pos

atPosInValBind :: ValBind -> Pos -> Maybe RawAtPos
atPosInValBind vbind pos =
  msum (map (`atPosInPattern` pos) (valBindParams vbind)) `mplus`
  atPosInExp (valBindBody vbind) pos `mplus`
  join (atPosInTypeExp <$> valBindRetDecl vbind <*> pure pos)

atPosInTypeBind :: TypeBind -> Pos -> Maybe RawAtPos
atPosInTypeBind = atPosInTypeExp . declaredType . typeExp

atPosInModBind :: ModBind -> Pos -> Maybe RawAtPos
atPosInModBind = atPosInModExp . modExp

atPosInDec :: Dec -> Pos -> Maybe RawAtPos
atPosInDec dec pos = do
  guard $ dec `contains` pos
  case dec of
    ValDec vbind -> atPosInValBind vbind pos
    TypeDec tbind -> atPosInTypeBind tbind pos
    ModDec mbind -> atPosInModBind mbind pos
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
