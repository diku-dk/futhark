-- | Facilities for answering queries about a program, such as "what
-- appears at this source location", or "where is this name bound".
-- The intent is that this is used as a building block for IDE-like
-- functionality.
module Language.Futhark.Query
  ( BoundTo (..),
    boundLoc,
    AtPos (..),
    atPos,
    Pos (..),
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (find)
import Data.Map qualified as M
import Futhark.Util.Loc (Loc (..), Pos (..))
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.Traversals
import System.FilePath.Posix qualified as Posix

-- | What a name is bound to.
data BoundTo
  = BoundTerm StructType Loc
  | BoundModule Loc
  | BoundModuleType Loc
  | BoundType Loc
  deriving (Eq, Show)

data Def = DefBound BoundTo | DefIndirect VName
  deriving (Eq, Show)

type Defs = M.Map VName Def

-- | Where was a bound variable actually bound?  That is, what is the
-- location of its definition?
boundLoc :: BoundTo -> Loc
boundLoc (BoundTerm _ loc) = loc
boundLoc (BoundModule loc) = loc
boundLoc (BoundModuleType loc) = loc
boundLoc (BoundType loc) = loc

sizeDefs :: SizeBinder VName -> Defs
sizeDefs (SizeBinder v loc) =
  M.singleton v $ DefBound $ BoundTerm (Scalar (Prim (Signed Int64))) (locOf loc)

patternDefs :: Pat (TypeBase Size u) -> Defs
patternDefs (Id vn (Info t) loc) =
  M.singleton vn $ DefBound $ BoundTerm (toStruct t) (locOf loc)
patternDefs (TuplePat pats _) =
  mconcat $ map patternDefs pats
patternDefs (RecordPat fields _) =
  mconcat $ map (patternDefs . snd) fields
patternDefs (PatParens pat _) = patternDefs pat
patternDefs (PatAttr _ pat _) = patternDefs pat
patternDefs Wildcard {} = mempty
patternDefs PatLit {} = mempty
patternDefs (PatAscription pat _ _) =
  patternDefs pat
patternDefs (PatConstr _ _ pats _) =
  mconcat $ map patternDefs pats

typeParamDefs :: TypeParamBase VName -> Defs
typeParamDefs (TypeParamDim vn loc) =
  M.singleton vn $ DefBound $ BoundTerm (Scalar $ Prim $ Signed Int64) (locOf loc)
typeParamDefs (TypeParamType _ vn loc) =
  M.singleton vn $ DefBound $ BoundType $ locOf loc

expDefs :: Exp -> Defs
expDefs e =
  execState (astMap mapper e) extra
  where
    mapper =
      identityMapper {mapOnExp = onExp}
    onExp e' = do
      modify (<> expDefs e')
      pure e'

    identDefs (Ident v (Info vt) vloc) =
      M.singleton v $ DefBound $ BoundTerm (toStruct vt) $ locOf vloc

    extra =
      case e of
        AppExp (LetPat sizes pat _ _ _) _ ->
          foldMap sizeDefs sizes <> patternDefs pat
        Lambda params _ _ _ _ ->
          mconcat (map patternDefs params)
        AppExp (LetFun (name, _) (tparams, params, _, Info ret, _) _ loc) _ ->
          let name_t = funType params ret
           in M.singleton name (DefBound $ BoundTerm name_t (locOf loc))
                <> mconcat (map typeParamDefs tparams)
                <> mconcat (map patternDefs params)
        AppExp (LetWith v _ _ _ _ _) _ ->
          identDefs v
        AppExp (Loop _ merge _ form _ _) _ ->
          patternDefs merge
            <> case form of
              For i _ -> identDefs i
              ForIn pat _ -> patternDefs pat
              While {} -> mempty
        _ ->
          mempty

valBindDefs :: ValBind -> Defs
valBindDefs vbind =
  M.insert (valBindName vbind) (DefBound $ BoundTerm vbind_t (locOf vbind)) $
    mconcat (map typeParamDefs (valBindTypeParams vbind))
      <> mconcat (map patternDefs (valBindParams vbind))
      <> expDefs (valBindBody vbind)
  where
    vbind_t =
      funType (valBindParams vbind) $ unInfo $ valBindRetType vbind

typeBindDefs :: TypeBind -> Defs
typeBindDefs tbind =
  M.singleton (typeAlias tbind) $ DefBound $ BoundType $ locOf tbind

modParamDefs :: ModParam -> Defs
modParamDefs (ModParam p se _ loc) =
  M.singleton p (DefBound $ BoundModule $ locOf loc)
    <> modTypeExpDefs se

modExpDefs :: ModExp -> Defs
modExpDefs ModVar {} =
  mempty
modExpDefs (ModParens me _) =
  modExpDefs me
modExpDefs ModImport {} =
  mempty
modExpDefs (ModDecs decs _) =
  mconcat $ map decDefs decs
modExpDefs (ModApply e1 e2 _ (Info substs) _) =
  modExpDefs e1 <> modExpDefs e2 <> M.map DefIndirect substs
modExpDefs (ModAscript e _ (Info substs) _) =
  modExpDefs e <> M.map DefIndirect substs
modExpDefs (ModLambda p _ e _) =
  modParamDefs p <> modExpDefs e

modBindDefs :: ModBind -> Defs
modBindDefs mbind =
  M.singleton (modName mbind) (DefBound $ BoundModule $ locOf mbind)
    <> mconcat (map modParamDefs (modParams mbind))
    <> modExpDefs (modExp mbind)
    <> case modType mbind of
      Nothing -> mempty
      Just (_, Info substs) ->
        M.map DefIndirect substs

specDefs :: Spec -> Defs
specDefs spec =
  case spec of
    ValSpec v tparams _ (Info t) _ loc ->
      let vdef = DefBound $ BoundTerm t (locOf loc)
       in M.insert v vdef $ mconcat (map typeParamDefs tparams)
    TypeAbbrSpec tbind -> typeBindDefs tbind
    TypeSpec _ v _ _ loc ->
      M.singleton v $ DefBound $ BoundType $ locOf loc
    ModSpec v se _ loc ->
      M.singleton v (DefBound $ BoundModuleType $ locOf loc)
        <> modTypeExpDefs se
    IncludeSpec se _ -> modTypeExpDefs se

modTypeExpDefs :: ModTypeExp -> Defs
modTypeExpDefs se =
  case se of
    ModTypeVar _ (Info substs) _ -> M.map DefIndirect substs
    ModTypeParens e _ -> modTypeExpDefs e
    ModTypeSpecs specs _ -> mconcat $ map specDefs specs
    ModTypeWith e _ _ -> modTypeExpDefs e
    ModTypeArrow _ e1 e2 _ -> modTypeExpDefs e1 <> modTypeExpDefs e2

sigBindDefs :: ModTypeBind -> Defs
sigBindDefs sbind =
  M.singleton (modTypeName sbind) (DefBound $ BoundModuleType $ locOf sbind)
    <> modTypeExpDefs (modTypeExp sbind)

decDefs :: Dec -> Defs
decDefs (ValDec vbind) = valBindDefs vbind
decDefs (TypeDec vbind) = typeBindDefs vbind
decDefs (ModDec mbind) = modBindDefs mbind
decDefs (ModTypeDec mbind) = sigBindDefs mbind
decDefs (OpenDec me _) = modExpDefs me
decDefs (LocalDec dec _) = decDefs dec
decDefs ImportDec {} = mempty

-- | All bindings of everything in the program.
progDefs :: Prog -> Defs
progDefs = mconcat . map decDefs . progDecs

allBindings :: Imports -> M.Map VName BoundTo
allBindings imports = M.mapMaybe forward defs
  where
    defs = mconcat $ map (progDefs . fileProg . snd) imports
    forward (DefBound x) = Just x
    forward (DefIndirect v) = forward =<< M.lookup v defs

data RawAtPos = RawAtName (QualName VName) Loc

contains :: (Located a) => a -> Pos -> Bool
contains a pos =
  case locOf a of
    Loc start end -> pos >= start && pos <= end
    NoLoc -> False

atPosInTypeExp :: TypeExp Exp VName -> Pos -> Maybe RawAtPos
atPosInTypeExp te pos =
  case te of
    TEVar qn loc -> do
      guard $ loc `contains` pos
      Just $ RawAtName qn $ locOf loc
    TEParens te' _ ->
      atPosInTypeExp te' pos
    TETuple es _ ->
      msum $ map (`atPosInTypeExp` pos) es
    TERecord fields _ ->
      msum $ map ((`atPosInTypeExp` pos) . snd) fields
    TEArray dim te' _ ->
      atPosInTypeExp te' pos `mplus` inDim dim
    TEUnique te' _ ->
      atPosInTypeExp te' pos
    TEApply e1 arg _ ->
      atPosInTypeExp e1 pos `mplus` inArg arg
    TEArrow _ e1 e2 _ ->
      atPosInTypeExp e1 pos `mplus` atPosInTypeExp e2 pos
    TESum cs _ ->
      msum $ map (`atPosInTypeExp` pos) $ concatMap snd cs
    TEDim _ t _ ->
      atPosInTypeExp t pos
  where
    inArg (TypeArgExpSize dim) = inDim dim
    inArg (TypeArgExpType e2) = atPosInTypeExp e2 pos
    inDim (SizeExp e _) = atPosInExp e pos
    inDim SizeExpAny {} = Nothing

atPosInPat :: Pat (TypeBase Size u) -> Pos -> Maybe RawAtPos
atPosInPat (Id vn _ loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName (qualName vn) $ locOf loc
atPosInPat (TuplePat pats _) pos =
  msum $ map (`atPosInPat` pos) pats
atPosInPat (RecordPat fields _) pos =
  msum $ map ((`atPosInPat` pos) . snd) fields
atPosInPat (PatParens pat _) pos =
  atPosInPat pat pos
atPosInPat (PatAttr _ pat _) pos =
  atPosInPat pat pos
atPosInPat (PatAscription pat te _) pos =
  atPosInPat pat pos `mplus` atPosInTypeExp te pos
atPosInPat (PatConstr _ _ pats _) pos =
  msum $ map (`atPosInPat` pos) pats
atPosInPat PatLit {} _ = Nothing
atPosInPat Wildcard {} _ = Nothing

atPosInExp :: Exp -> Pos -> Maybe RawAtPos
atPosInExp (Var qn _ loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName qn $ locOf loc
atPosInExp (QualParens (qn, loc) _ _) pos
  | loc `contains` pos = Just $ RawAtName qn $ locOf loc
-- All the value cases are TODO - we need another RawAtPos constructor.
atPosInExp Literal {} _ = Nothing
atPosInExp IntLit {} _ = Nothing
atPosInExp FloatLit {} _ = Nothing
atPosInExp (AppExp (LetPat _ pat _ _ _) _) pos
  | pat `contains` pos = atPosInPat pat pos
atPosInExp (AppExp (LetWith a b _ _ _ _) _) pos
  | a `contains` pos = Just $ RawAtName (qualName $ identName a) (locOf a)
  | b `contains` pos = Just $ RawAtName (qualName $ identName b) (locOf b)
atPosInExp (AppExp (Loop _ merge _ _ _ _) _) pos
  | merge `contains` pos = atPosInPat merge pos
atPosInExp (Ascript _ te _) pos
  | te `contains` pos = atPosInTypeExp te pos
atPosInExp (Coerce _ te _ _) pos
  | te `contains` pos = atPosInTypeExp te pos
atPosInExp e pos = do
  guard $ e `contains` pos
  -- Use the Either monad for short-circuiting for efficiency reasons.
  -- The first hit is going to be the only one.
  case astMap mapper e of
    Left atpos -> Just atpos
    Right _ -> Nothing
  where
    mapper =
      identityMapper {mapOnExp = onExp}
    onExp e' =
      case atPosInExp e' pos of
        Just atpos -> Left atpos
        Nothing -> Right e'

atPosInModExp :: ModExp -> Pos -> Maybe RawAtPos
atPosInModExp (ModVar qn loc) pos = do
  guard $ loc `contains` pos
  Just $ RawAtName qn $ locOf loc
atPosInModExp (ModParens me _) pos =
  atPosInModExp me pos
atPosInModExp ModImport {} _ =
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
    ValSpec _ _ te _ _ _ -> atPosInTypeExp te pos
    TypeAbbrSpec tbind -> atPosInTypeBind tbind pos
    TypeSpec {} -> Nothing
    ModSpec _ se _ _ -> atPosInModTypeExp se pos
    IncludeSpec se _ -> atPosInModTypeExp se pos

atPosInModTypeExp :: ModTypeExp -> Pos -> Maybe RawAtPos
atPosInModTypeExp se pos =
  case se of
    ModTypeVar qn _ loc -> do
      guard $ loc `contains` pos
      Just $ RawAtName qn $ locOf loc
    ModTypeParens e _ -> atPosInModTypeExp e pos
    ModTypeSpecs specs _ -> msum $ map (`atPosInSpec` pos) specs
    ModTypeWith e _ _ -> atPosInModTypeExp e pos
    ModTypeArrow _ e1 e2 _ -> atPosInModTypeExp e1 pos `mplus` atPosInModTypeExp e2 pos

atPosInValBind :: ValBind -> Pos -> Maybe RawAtPos
atPosInValBind vbind pos =
  msum (map (`atPosInPat` pos) (valBindParams vbind))
    `mplus` atPosInExp (valBindBody vbind) pos
    `mplus` join (atPosInTypeExp <$> valBindRetDecl vbind <*> pure pos)

atPosInTypeBind :: TypeBind -> Pos -> Maybe RawAtPos
atPosInTypeBind = atPosInTypeExp . typeExp

atPosInModBind :: ModBind -> Pos -> Maybe RawAtPos
atPosInModBind (ModBind _ params sig e _ _) pos =
  msum (map inParam params)
    `mplus` atPosInModExp e pos
    `mplus` case sig of
      Nothing -> Nothing
      Just (se, _) -> atPosInModTypeExp se pos
  where
    inParam (ModParam _ se _ _) = atPosInModTypeExp se pos

atPosInModTypeBind :: ModTypeBind -> Pos -> Maybe RawAtPos
atPosInModTypeBind = atPosInModTypeExp . modTypeExp

atPosInDec :: Dec -> Pos -> Maybe RawAtPos
atPosInDec dec pos = do
  guard $ dec `contains` pos
  case dec of
    ValDec vbind -> atPosInValBind vbind pos
    TypeDec tbind -> atPosInTypeBind tbind pos
    ModDec mbind -> atPosInModBind mbind pos
    ModTypeDec sbind -> atPosInModTypeBind sbind pos
    OpenDec e _ -> atPosInModExp e pos
    LocalDec dec' _ -> atPosInDec dec' pos
    ImportDec {} -> Nothing

atPosInProg :: Prog -> Pos -> Maybe RawAtPos
atPosInProg prog pos =
  msum $ map (`atPosInDec` pos) (progDecs prog)

containingModule :: Imports -> Pos -> Maybe FileModule
containingModule imports (Pos file _ _ _) =
  snd <$> find ((== file') . fst) imports
  where
    file' = mkInitialImport $ fst $ Posix.splitExtension file

-- | Information about what is at the given source location.
data AtPos = AtName (QualName VName) (Maybe BoundTo) Loc
  deriving (Eq, Show)

-- | Information about what's at the given source position.  Returns
-- 'Nothing' if there is nothing there, including if the source
-- position is invalid.
atPos :: Imports -> Pos -> Maybe AtPos
atPos imports pos = do
  prog <- fileProg <$> containingModule imports pos
  RawAtName qn loc <- atPosInProg prog pos
  Just $ AtName qn (qualLeaf qn `M.lookup` allBindings imports) loc
