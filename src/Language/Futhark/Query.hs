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
data BoundTo = BoundTerm StructType Loc
             | BoundModule Loc
             | BoundModuleType Loc
             | BoundType Loc
  deriving (Eq, Show)

data Def = DefBound BoundTo | DefIndirect VName
  deriving (Eq, Show)

-- | Where was a bound variable actually bound?  That is, what is the
-- location of its definition?
boundLoc :: BoundTo -> Loc
boundLoc (BoundTerm _ loc) = loc
boundLoc (BoundModule loc) = loc
boundLoc (BoundModuleType loc) = loc
boundLoc (BoundType loc) = loc

patternBindings :: Pattern -> M.Map VName Def
patternBindings (Id vn (Info t) loc) =
  M.singleton vn $ DefBound $ BoundTerm (toStruct t) (locOf loc)
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

typeParamBindings :: TypeParamBase VName -> M.Map VName Def
typeParamBindings (TypeParamDim vn loc) =
  M.singleton vn $ DefBound $ BoundTerm (Scalar $ Prim $ Signed Int32) (locOf loc)
typeParamBindings TypeParamType{} =
  mempty

expBindings :: Exp -> M.Map VName Def
expBindings (LetPat pat e1 e2 _ _) =
  patternBindings pat <> expBindings e1 <> expBindings e2
expBindings (Lambda params body _ _ _) =
  mconcat (map patternBindings params) <> expBindings body
expBindings (LetFun name (tparams, params, _, Info ret,  e1) e2 loc) =
  M.singleton name (DefBound $ BoundTerm name_t (locOf loc)) <>
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

valBindBindings :: ValBind -> M.Map VName Def
valBindBindings vbind =
  M.insert (valBindName vbind) (DefBound $ BoundTerm vbind_t (locOf vbind)) $
  mconcat (map typeParamBindings (valBindTypeParams vbind)) <>
  mconcat (map patternBindings (valBindParams vbind)) <>
  expBindings (valBindBody vbind)
  where vbind_t =
          foldFunType (map patternStructType (valBindParams vbind)) $
          unInfo $ valBindRetType vbind

typeBindBindings :: TypeBind -> M.Map VName Def
typeBindBindings tbind =
  M.singleton (typeAlias tbind) $ DefBound $ BoundType $ locOf tbind

modParamBindings :: ModParam -> M.Map VName Def
modParamBindings (ModParam p se _ loc) =
  M.singleton p (DefBound $ BoundModule $ locOf loc) <>
  sigExpBindings se

modExpBindings :: ModExp -> M.Map VName Def
modExpBindings ModVar{} =
  mempty
modExpBindings (ModParens me _) =
  modExpBindings me
modExpBindings ModImport{} =
  mempty
modExpBindings (ModDecs decs _) =
  mconcat $ map decBindings decs
modExpBindings (ModApply e1 e2 _ (Info substs) _) =
  modExpBindings e1 <> modExpBindings e2 <> M.map DefIndirect substs
modExpBindings (ModAscript e _ (Info substs) _) =
  modExpBindings e <> M.map DefIndirect substs
modExpBindings (ModLambda p _ e _) =
  modParamBindings p <> modExpBindings e

modBindBindings :: ModBind -> M.Map VName Def
modBindBindings mbind =
  M.singleton (modName mbind) (DefBound $ BoundModule $ locOf mbind) <>
  mconcat (map modParamBindings (modParams mbind)) <>
  modExpBindings (modExp mbind) <>
  case modSignature mbind of
    Nothing -> mempty
    Just (_, Info substs) ->
      M.map DefIndirect substs

specBindings :: Spec -> M.Map VName Def
specBindings spec =
  case spec of
    ValSpec v _ tdecl _ loc ->
      M.singleton v $ DefBound $
      BoundTerm (unInfo $ expandedType tdecl) (locOf loc)
    TypeAbbrSpec tbind -> typeBindBindings tbind
    TypeSpec _ v _ _ loc ->
      M.singleton v $ DefBound $ BoundType $ locOf loc
    ModSpec v se _ loc ->
      M.singleton v (DefBound $ BoundModuleType $ locOf loc) <>
      sigExpBindings se
    IncludeSpec se _ -> sigExpBindings se

sigExpBindings :: SigExp -> M.Map VName Def
sigExpBindings se =
  case se of
    SigVar _ (Info substs) _ -> M.map DefIndirect substs
    SigParens e _ -> sigExpBindings e
    SigSpecs specs _ -> mconcat $ map specBindings specs
    SigWith e _ _ -> sigExpBindings e
    SigArrow _ e1 e2 _ -> sigExpBindings e1 <> sigExpBindings e2

sigBindBindings :: SigBind -> M.Map VName Def
sigBindBindings sbind =
  M.singleton (sigName sbind) (DefBound $ BoundModuleType $ locOf sbind) <>
  sigExpBindings (sigExp sbind)


decBindings :: Dec -> M.Map VName Def
decBindings (ValDec vbind) = valBindBindings vbind
decBindings (TypeDec vbind) = typeBindBindings vbind
decBindings (ModDec mbind) = modBindBindings mbind
decBindings (SigDec mbind) = sigBindBindings mbind
decBindings _ = mempty

-- | All bindings of everything in the program.
progBindings :: Prog -> M.Map VName Def
progBindings = mconcat . map decBindings . progDecs

allBindings :: Imports -> M.Map VName BoundTo
allBindings imports = M.mapMaybe forward defs
  where defs = mconcat $ map (progBindings . fileProg . snd) imports
        forward (DefBound x) = Just x
        forward (DefIndirect v) = forward =<< M.lookup v defs

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
    TEArray te' dim _ ->
      atPosInTypeExp te' pos `mplus` inDim dim
    TEUnique te' _ ->
      atPosInTypeExp te' pos
    TEApply e1 arg _ ->
      atPosInTypeExp e1 pos `mplus` inArg arg
    TEArrow _ e1 e2 _ ->
      atPosInTypeExp e1 pos `mplus` atPosInTypeExp e2 pos
    TESum cs _ ->
      msum $ map (`atPosInTypeExp` pos) $ concatMap snd cs
  where inArg (TypeArgExpDim dim _) = inDim dim
        inArg (TypeArgExpType e2) = atPosInTypeExp e2 pos
        inDim (DimExpNamed qn loc) = do
          guard $ loc `contains` pos
          Just $ RawAtName qn loc
        inDim _ = Nothing

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

atPosInSpec :: Spec -> Pos -> Maybe RawAtPos
atPosInSpec spec pos =
  case spec of
    ValSpec _ _ tdecl _ _ -> atPosInTypeExp (declaredType tdecl) pos
    TypeAbbrSpec tbind -> atPosInTypeBind tbind pos
    TypeSpec{} -> Nothing
    ModSpec _ se _ _ -> atPosInSigExp se pos
    IncludeSpec se _ -> atPosInSigExp se pos

atPosInSigExp :: SigExp -> Pos -> Maybe RawAtPos
atPosInSigExp se pos =
  case se of
    SigVar qn _ loc -> do guard $ loc `contains` pos
                          Just $ RawAtName qn loc
    SigParens e _ -> atPosInSigExp e pos
    SigSpecs specs _ -> msum $ map (`atPosInSpec` pos) specs
    SigWith e _ _ -> atPosInSigExp e pos
    SigArrow _ e1 e2 _ -> atPosInSigExp e1 pos `mplus` atPosInSigExp e2 pos

atPosInValBind :: ValBind -> Pos -> Maybe RawAtPos
atPosInValBind vbind pos =
  msum (map (`atPosInPattern` pos) (valBindParams vbind)) `mplus`
  atPosInExp (valBindBody vbind) pos `mplus`
  join (atPosInTypeExp <$> valBindRetDecl vbind <*> pure pos)

atPosInTypeBind :: TypeBind -> Pos -> Maybe RawAtPos
atPosInTypeBind = atPosInTypeExp . declaredType . typeExp

atPosInModBind :: ModBind -> Pos -> Maybe RawAtPos
atPosInModBind (ModBind _ params sig e _ _) pos =
  msum (map inParam params) `mplus`
  atPosInModExp e pos `mplus`
  case sig of Nothing -> Nothing
              Just (se, _) -> atPosInSigExp se pos
  where inParam (ModParam _ se _ _) = atPosInSigExp se pos

atPosInSigBind :: SigBind -> Pos -> Maybe RawAtPos
atPosInSigBind = atPosInSigExp . sigExp

atPosInDec :: Dec -> Pos -> Maybe RawAtPos
atPosInDec dec pos = do
  guard $ dec `contains` pos
  case dec of
    ValDec vbind -> atPosInValBind vbind pos
    TypeDec tbind -> atPosInTypeBind tbind pos
    ModDec mbind -> atPosInModBind mbind pos
    SigDec sbind -> atPosInSigBind sbind pos
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
