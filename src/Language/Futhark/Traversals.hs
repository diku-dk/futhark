{-# LANGUAGE FlexibleInstances #-}
-- |
--
-- Functions for generic traversals across Futhark syntax trees.  The
-- motivation for this module came from dissatisfaction with rewriting
-- the same trivial tree recursions for every module.  A possible
-- alternative would be to use normal \"Scrap your
-- boilerplate\"-techniques, but these are rejected for two reasons:
--
--    * They are too slow.
--
--    * More importantly, they do not tell you whether you have missed
--      some cases.
--
-- Instead, this module defines various traversals of the Futhark syntax
-- tree.  The implementation is rather tedious, but the interface is
-- easy to use.
--
-- A traversal of the Futhark syntax tree is expressed as a record of
-- functions expressing the operations to be performed on the various
-- types of nodes.
module Language.Futhark.Traversals
  ( ASTMapper(..)
  , ASTMappable(..)
  , identityMapper
  , bareExp
  ) where

import qualified Data.Set                as S
import qualified Data.List.NonEmpty               as NE

import           Language.Futhark.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data ASTMapper m = ASTMapper {
    mapOnExp         :: ExpBase Info VName -> m (ExpBase Info VName)
  , mapOnName        :: VName -> m VName
  , mapOnQualName    :: QualName VName -> m (QualName VName)
  , mapOnStructType  :: StructType -> m StructType
  , mapOnPatternType :: PatternType -> m PatternType
  }

-- | An 'ASTMapper' that just leaves its input unchanged.
identityMapper :: Monad m => ASTMapper m
identityMapper = ASTMapper { mapOnExp = return
                           , mapOnName = return
                           , mapOnQualName = return
                           , mapOnStructType = return
                           , mapOnPatternType = return
                           }

class ASTMappable x where
  -- | Map a monadic action across the immediate children of an
  -- object.  Importantly, the 'astMap' action is not invoked for
  -- the object itself, and the mapping does not descend recursively
  -- into subexpressions.  The mapping is done left-to-right.
  astMap :: Monad m => ASTMapper m -> x -> m x

instance ASTMappable (ExpBase Info VName) where
  astMap tv (Var name t loc) =
    Var <$> mapOnQualName tv name <*> traverse (mapOnPatternType tv) t <*>
    pure loc
  astMap _ (Literal val loc) =
    pure $ Literal val loc
  astMap _ (StringLit vs loc) =
    pure $ StringLit vs loc
  astMap tv (IntLit val t loc) =
    IntLit val <$> traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (FloatLit val t loc) =
    FloatLit val <$> traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (Parens e loc) =
    Parens <$> mapOnExp tv e <*> pure loc
  astMap tv (QualParens (name, nameloc) e loc) =
    QualParens <$> ((,) <$> mapOnQualName tv name <*> pure nameloc) <*>
    mapOnExp tv e <*> pure loc
  astMap tv (TupLit els loc) =
    TupLit <$> mapM (mapOnExp tv) els <*> pure loc
  astMap tv (RecordLit fields loc) =
    RecordLit <$> astMap tv fields <*> pure loc
  astMap tv (ArrayLit els t loc) =
    ArrayLit <$> mapM (mapOnExp tv) els <*> traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (Range start next end (t, ext) loc) =
    Range <$> mapOnExp tv start <*> traverse (mapOnExp tv) next <*>
    traverse (mapOnExp tv) end <*>
    ((,) <$> traverse (mapOnPatternType tv) t <*> pure ext) <*> pure loc
  astMap tv (Ascript e tdecl loc) =
    Ascript <$> mapOnExp tv e <*> astMap tv tdecl <*> pure loc
  astMap tv (Coerce e tdecl (t, ext) loc) =
    Coerce <$> mapOnExp tv e <*> astMap tv tdecl <*>
    ((,) <$> traverse (mapOnPatternType tv) t <*> pure ext) <*> pure loc
  astMap tv (BinOp (fname, fname_loc) t (x,Info(xt,xext)) (y,Info(yt,yext)) (Info rt) ext loc) =
    BinOp <$> ((,) <$> mapOnQualName tv fname <*> pure fname_loc) <*>
    traverse (mapOnPatternType tv) t <*>
    ((,) <$> mapOnExp tv x <*>
     (Info <$> ((,) <$> mapOnStructType tv xt <*> pure xext))) <*>
    ((,) <$> mapOnExp tv y <*>
     (Info <$> ((,) <$> mapOnStructType tv yt <*> pure yext))) <*>
    (Info <$> mapOnPatternType tv rt) <*> pure ext <*> pure loc
  astMap tv (Negate x loc) =
    Negate <$> mapOnExp tv x <*> pure loc
  astMap tv (If c texp fexp (t, ext) loc) =
    If <$> mapOnExp tv c <*> mapOnExp tv texp <*> mapOnExp tv fexp <*>
    ((,) <$> traverse (mapOnPatternType tv) t <*> pure ext) <*> pure loc
  astMap tv (Apply f arg d (Info t, ext) loc) =
    Apply <$> mapOnExp tv f <*> mapOnExp tv arg <*> pure d <*>
    ((,) <$> (Info <$> mapOnPatternType tv t) <*> pure ext) <*> pure loc
  astMap tv (LetPat pat e body (t, ext) loc) =
    LetPat <$> astMap tv pat <*> mapOnExp tv e <*> mapOnExp tv body <*>
    ((,) <$> traverse (mapOnPatternType tv) t <*> pure ext) <*> pure loc
  astMap tv (LetFun name (fparams, params, ret, t, e) body loc) =
    LetFun <$> mapOnName tv name <*>
    ((,,,,) <$> mapM (astMap tv) fparams <*> mapM (astMap tv) params <*>
     traverse (astMap tv) ret <*> traverse (mapOnStructType tv) t <*>
     mapOnExp tv e) <*>
    mapOnExp tv body <*> pure loc
  astMap tv (LetWith dest src idxexps vexp body t loc) =
    LetWith <$>
    astMap tv dest <*> astMap tv src <*>
    mapM (astMap tv) idxexps <*> mapOnExp tv vexp <*>
    mapOnExp tv body <*> traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (Update src slice v loc) =
    Update <$> mapOnExp tv src <*> mapM (astMap tv) slice <*>
    mapOnExp tv v <*> pure loc
  astMap tv (RecordUpdate src fs v (Info t) loc) =
    RecordUpdate <$> mapOnExp tv src <*> pure fs <*>
    mapOnExp tv v <*> (Info <$> mapOnPatternType tv t) <*> pure loc
  astMap tv (Project field e t loc) =
    Project field <$> mapOnExp tv e <*> traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (Index arr idxexps (t, ext) loc) =
    Index <$> mapOnExp tv arr <*> mapM (astMap tv) idxexps <*>
    ((,) <$> traverse (mapOnPatternType tv) t <*> pure ext) <*> pure loc
  astMap tv (Unsafe e loc) =
    Unsafe <$> mapOnExp tv e <*> pure loc
  astMap tv (Assert e1 e2 desc loc) =
    Assert <$> mapOnExp tv e1 <*> mapOnExp tv e2 <*> pure desc <*> pure loc
  astMap tv (Lambda params body ret t loc) =
    Lambda <$> mapM (astMap tv) params <*>
    astMap tv body <*> traverse (astMap tv) ret <*>
    traverse (traverse $ mapOnStructType tv) t <*> pure loc
  astMap tv (OpSection name t loc) =
    OpSection <$> mapOnQualName tv name <*>
    traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (OpSectionLeft name t arg (Info (t1a, argext), t1b) (t2, retext) loc) =
    OpSectionLeft <$> mapOnQualName tv name <*>
    traverse (mapOnPatternType tv) t <*> mapOnExp tv arg <*>
    ((,) <$>
     (Info <$> ((,) <$> mapOnStructType tv t1a <*> pure argext)) <*>
     traverse (mapOnStructType tv) t1b) <*>
    ((,) <$> traverse (mapOnPatternType tv) t2 <*> pure retext) <*> pure loc
  astMap tv (OpSectionRight name t arg (t1a, Info (t1b,argext)) t2 loc) =
    OpSectionRight <$> mapOnQualName tv name <*>
    traverse (mapOnPatternType tv) t <*> mapOnExp tv arg <*>
    ((,) <$>
     traverse (mapOnStructType tv) t1a <*>
     (Info <$> ((,) <$> mapOnStructType tv t1b <*> pure argext))) <*>
    traverse (mapOnPatternType tv) t2 <*> pure loc
  astMap tv (ProjectSection fields t loc) =
    ProjectSection fields <$> traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (IndexSection idxs t loc) =
    IndexSection <$> mapM (astMap tv) idxs <*>
    traverse (mapOnPatternType tv) t <*> pure loc
  astMap tv (DoLoop sparams mergepat mergeexp form loopbody (Info (ret, ext)) loc) =
    DoLoop <$> mapM (mapOnName tv) sparams <*> astMap tv mergepat <*>
    mapOnExp tv mergeexp <*> astMap tv form <*> mapOnExp tv loopbody <*>
    (Info <$> ((,) <$> mapOnPatternType tv ret <*> pure ext)) <*> pure loc
  astMap tv (Constr name es ts loc) =
    Constr name <$> traverse (mapOnExp tv) es <*> traverse (mapOnPatternType tv) ts <*> pure loc
  astMap tv (Match e cases (t, ext) loc) =
    Match <$> mapOnExp tv e <*> astMap tv cases
          <*> ((,) <$> traverse (mapOnPatternType tv) t <*> pure ext) <*> pure loc

instance ASTMappable (LoopFormBase Info VName) where
  astMap tv (For i bound) = For <$> astMap tv i <*> astMap tv bound
  astMap tv (ForIn pat e) = ForIn <$> astMap tv pat <*> mapOnExp tv e
  astMap tv (While e)     = While <$> mapOnExp tv e

instance ASTMappable (TypeExp VName) where
  astMap tv (TEVar qn loc) = TEVar <$> mapOnQualName tv qn <*> pure loc
  astMap tv (TETuple ts loc) = TETuple <$> traverse (astMap tv) ts <*> pure loc
  astMap tv (TERecord ts loc) =
    TERecord <$> traverse (traverse $ astMap tv) ts <*> pure loc
  astMap tv (TEArray te dim loc) =
    TEArray <$> astMap tv te <*> astMap tv dim <*> pure loc
  astMap tv (TEUnique t loc) = TEUnique <$> astMap tv t <*> pure loc
  astMap tv (TEApply t1 t2 loc) =
    TEApply <$> astMap tv t1 <*> astMap tv t2 <*> pure loc
  astMap tv (TEArrow v t1 t2 loc) =
    TEArrow v <$> astMap tv t1 <*> astMap tv t2 <*> pure loc
  astMap tv (TESum cs loc) =
    TESum <$> traverse (traverse $ astMap tv) cs <*> pure loc

instance ASTMappable (TypeArgExp VName) where
  astMap tv (TypeArgExpDim dim loc) =
    TypeArgExpDim <$> astMap tv dim <*> pure loc
  astMap tv (TypeArgExpType te) =
    TypeArgExpType <$> astMap tv te

instance ASTMappable (DimExp VName) where
  astMap tv (DimExpNamed vn loc) =
    DimExpNamed <$> mapOnQualName tv vn <*> pure loc
  astMap _ (DimExpConst k loc) = pure $ DimExpConst k loc
  astMap _ DimExpAny = pure DimExpAny

instance ASTMappable (DimDecl VName) where
  astMap tv (NamedDim vn) = NamedDim <$> mapOnQualName tv vn
  astMap _ (ConstDim k)   = pure $ ConstDim k
  astMap _ AnyDim         = pure AnyDim

instance ASTMappable (TypeParamBase VName) where
  astMap = traverse . mapOnName

instance ASTMappable (DimIndexBase Info VName) where
  astMap tv (DimFix j) = DimFix <$> astMap tv j
  astMap tv (DimSlice i j stride) =
    DimSlice <$>
    maybe (return Nothing) (fmap Just . astMap tv) i <*>
    maybe (return Nothing) (fmap Just . astMap tv) j <*>
    maybe (return Nothing) (fmap Just . astMap tv) stride

instance ASTMappable Alias where
  astMap tv (AliasBound v) = AliasBound <$> mapOnName tv v
  astMap tv (AliasFree v) = AliasFree <$> mapOnName tv v

instance ASTMappable Aliasing where
  astMap tv = fmap S.fromList . traverse (astMap tv) . S.toList

type TypeTraverser f t dim1 als1 dim2 als2 =
  (TypeName -> f TypeName) -> (dim1 -> f dim2) -> (als1 -> f als2) ->
  t dim1 als1 -> f (t dim2 als2)

traverseScalarType :: Applicative f =>
                      TypeTraverser f ScalarTypeBase dim1 als1 dims als2
traverseScalarType _ _ _ (Prim t) = pure $ Prim t
traverseScalarType f g h (Record fs) = Record <$> traverse (traverseType f g h) fs
traverseScalarType f g h (TypeVar als u t args) =
  TypeVar <$> h als <*> pure u <*> f t <*> traverse (traverseTypeArg f g) args
traverseScalarType f g h (Arrow als v t1 t2) =
  Arrow <$> h als <*> pure v <*> traverseType f g h t1 <*> traverseType f g h t2
traverseScalarType f g h (Sum cs) = Sum <$> (traverse . traverse) (traverseType f g h) cs

traverseType :: Applicative f =>
                TypeTraverser f TypeBase dim1 als1 dims als2
traverseType f g h (Array als u et shape) =
  Array <$> h als <*> pure u <*> traverseScalarType f g pure et <*> traverse g shape
traverseType f g h (Scalar t) =
  Scalar <$> traverseScalarType f g h t

traverseTypeArg :: Applicative f =>
                   (TypeName -> f TypeName) -> (dim1 -> f dim2)
                -> TypeArg dim1 -> f (TypeArg dim2)
traverseTypeArg _ g (TypeArgDim d loc) = TypeArgDim <$> g d <*> pure loc
traverseTypeArg f g (TypeArgType t loc) = TypeArgType <$> traverseType f g pure t <*> pure loc

instance ASTMappable StructType where
  astMap tv = traverseType f (astMap tv) pure
    where f = fmap typeNameFromQualName . mapOnQualName tv . qualNameFromTypeName

instance ASTMappable PatternType where
  astMap tv = traverseType f (astMap tv) (astMap tv)
    where f = fmap typeNameFromQualName . mapOnQualName tv . qualNameFromTypeName

instance ASTMappable (TypeDeclBase Info VName) where
  astMap tv (TypeDecl dt (Info et)) =
    TypeDecl <$> astMap tv dt <*> (Info <$> mapOnStructType tv et)

instance ASTMappable (IdentBase Info VName) where
  astMap tv (Ident name (Info t) loc) =
    Ident <$> mapOnName tv name <*> (Info <$> mapOnPatternType tv t) <*> pure loc

instance ASTMappable (PatternBase Info VName) where
  astMap tv (Id name (Info t) loc) =
    Id <$> mapOnName tv name <*> (Info <$> mapOnPatternType tv t) <*> pure loc
  astMap tv (TuplePattern pats loc) =
    TuplePattern <$> mapM (astMap tv) pats <*> pure loc
  astMap tv (RecordPattern fields loc) =
    RecordPattern <$> mapM (traverse $ astMap tv) fields <*> pure loc
  astMap tv (PatternParens pat loc) =
    PatternParens <$> astMap tv pat <*> pure loc
  astMap tv (PatternAscription pat t loc) =
    PatternAscription <$> astMap tv pat <*> astMap tv t <*> pure loc
  astMap tv (Wildcard (Info t) loc) =
    Wildcard <$> (Info <$> mapOnPatternType tv t) <*> pure loc
  astMap tv (PatternLit e (Info t) loc) =
    PatternLit <$> astMap tv e <*> (Info <$> mapOnPatternType tv t) <*>  pure loc
  astMap tv (PatternConstr n (Info t) ps loc) =
    PatternConstr n <$> (Info <$> mapOnPatternType tv t) <*> mapM (astMap tv) ps <*> pure loc

instance ASTMappable (FieldBase Info VName) where
  astMap tv (RecordFieldExplicit name e loc) =
    RecordFieldExplicit name <$> mapOnExp tv e <*> pure loc
  astMap tv (RecordFieldImplicit name t loc) =
    RecordFieldImplicit <$> mapOnName tv name
    <*> traverse (mapOnPatternType tv) t <*> pure loc

instance ASTMappable (CaseBase Info VName) where
  astMap tv (CasePat pat e loc) =
    CasePat <$> astMap tv pat <*> astMap tv e <*> pure loc

instance ASTMappable a => ASTMappable (Info a) where
  astMap tv = traverse $ astMap tv

instance ASTMappable a => ASTMappable [a] where
  astMap tv = traverse $ astMap tv

instance ASTMappable a => ASTMappable (NE.NonEmpty a) where
  astMap tv = traverse $ astMap tv

instance (ASTMappable a, ASTMappable b) => ASTMappable (a,b) where
  astMap tv (x,y) = (,) <$> astMap tv x <*> astMap tv y

instance (ASTMappable a, ASTMappable b, ASTMappable c) => ASTMappable (a,b,c) where
  astMap tv (x,y,z) = (,,) <$> astMap tv x <*> astMap tv y <*> astMap tv z

-- It would be lovely if the following code would be written in terms
-- of ASTMappable, but unfortunately it involves changing the Info
-- functor.  For simplicity, the general traversals do not support
-- that.  Sometimes a little duplication is better than an overly
-- complex abstraction.  The types ensure that this will be correct
-- anyway, so it's just tedious, and not actually fragile.

bareTypeDecl :: TypeDeclBase Info VName -> TypeDeclBase NoInfo VName
bareTypeDecl (TypeDecl te _) = TypeDecl te NoInfo

bareField :: FieldBase Info VName -> FieldBase NoInfo VName
bareField (RecordFieldExplicit name e loc) =
  RecordFieldExplicit name (bareExp e) loc
bareField (RecordFieldImplicit name _ loc) =
  RecordFieldImplicit name NoInfo loc

barePat :: PatternBase Info VName -> PatternBase NoInfo VName
barePat (TuplePattern ps loc) = TuplePattern (map barePat ps) loc
barePat (RecordPattern fs loc) = RecordPattern (map (fmap barePat) fs) loc
barePat (PatternParens p loc) = PatternParens (barePat p) loc
barePat (Id v _ loc) = Id v NoInfo loc
barePat (Wildcard _ loc) = Wildcard NoInfo loc
barePat (PatternAscription pat (TypeDecl t _) loc) =
  PatternAscription (barePat pat) (TypeDecl t NoInfo) loc
barePat (PatternLit e _ loc) = PatternLit (bareExp e) NoInfo loc
barePat (PatternConstr c _ ps loc) = PatternConstr c NoInfo (map barePat ps) loc

bareDimIndex :: DimIndexBase Info VName -> DimIndexBase NoInfo VName
bareDimIndex (DimFix e) =
  DimFix $ bareExp e
bareDimIndex (DimSlice x y z) =
  DimSlice (bareExp <$> x) (bareExp <$> y) (bareExp <$> z)

bareLoopForm :: LoopFormBase Info VName -> LoopFormBase NoInfo VName
bareLoopForm (For (Ident i _ loc) e) = For (Ident i NoInfo loc) (bareExp e)
bareLoopForm (ForIn pat e) = ForIn (barePat pat) (bareExp e)
bareLoopForm (While e) = While (bareExp e)

bareCase :: CaseBase Info VName -> CaseBase NoInfo VName
bareCase (CasePat pat e loc) = CasePat (barePat pat) (bareExp e) loc

-- | Remove all annotations from an expression, but retain the
-- name/scope information.
bareExp :: ExpBase Info VName -> ExpBase NoInfo VName
bareExp (Var name _ loc) = Var name NoInfo loc
bareExp (Literal v loc) = Literal v loc
bareExp (IntLit val _ loc) = IntLit val NoInfo loc
bareExp (FloatLit val _ loc) = FloatLit val NoInfo loc
bareExp (Parens e loc) = Parens (bareExp e) loc
bareExp (QualParens name e loc) = QualParens name (bareExp e) loc
bareExp (TupLit els loc) = TupLit (map bareExp els) loc
bareExp (StringLit vs loc) = StringLit vs loc
bareExp (RecordLit fields loc) = RecordLit (map bareField fields) loc
bareExp (ArrayLit els _ loc) = ArrayLit (map bareExp els) NoInfo loc
bareExp (Range start next end _ loc) =
  Range (bareExp start) (fmap bareExp next)
  (fmap bareExp end) (NoInfo, NoInfo) loc
bareExp (Ascript e tdecl loc) =
  Ascript (bareExp e) (bareTypeDecl tdecl) loc
bareExp (Coerce e tdecl _ loc) =
  Coerce (bareExp e) (bareTypeDecl tdecl) (NoInfo, NoInfo) loc
bareExp (BinOp fname _ (x,_) (y,_) _ _ loc) =
  BinOp fname NoInfo (bareExp x, NoInfo) (bareExp y, NoInfo) NoInfo NoInfo loc
bareExp (Negate x loc) = Negate (bareExp x) loc
bareExp (If c texp fexp _ loc) =
  If (bareExp c) (bareExp texp) (bareExp fexp) (NoInfo, NoInfo) loc
bareExp (Apply f arg _ _ loc) =
  Apply (bareExp f) (bareExp arg) NoInfo (NoInfo, NoInfo) loc
bareExp (LetPat pat e body _ loc) =
  LetPat (barePat pat) (bareExp e) (bareExp body) (NoInfo, NoInfo) loc
bareExp (LetFun name (fparams, params, ret, _, e) body loc) =
  LetFun name (fparams, map barePat params, ret, NoInfo, bareExp e) (bareExp body) loc
bareExp (LetWith (Ident dest _ destloc) (Ident src _ srcloc) idxexps vexp body _ loc) =
  LetWith (Ident dest NoInfo destloc) (Ident src NoInfo srcloc)
  (map bareDimIndex idxexps) (bareExp vexp)
  (bareExp body) NoInfo loc
bareExp (Update src slice v loc) =
  Update (bareExp src) (map bareDimIndex slice) (bareExp v) loc
bareExp (RecordUpdate src fs v _ loc) =
  RecordUpdate (bareExp src) fs (bareExp v) NoInfo loc
bareExp (Project field e _ loc) = Project field (bareExp e) NoInfo loc
bareExp (Index arr slice _ loc) =
  Index (bareExp arr) (map bareDimIndex slice) (NoInfo, NoInfo) loc
bareExp (Unsafe e loc) = Unsafe (bareExp e) loc
bareExp (Assert e1 e2 _ loc) = Assert (bareExp e1) (bareExp e2) NoInfo loc
bareExp (Lambda params body ret _ loc) =
  Lambda (map barePat params) (bareExp body) ret NoInfo loc
bareExp (OpSection name _ loc) = OpSection name NoInfo loc
bareExp (OpSectionLeft name _ arg _ _ loc) =
  OpSectionLeft name NoInfo (bareExp arg) (NoInfo, NoInfo) (NoInfo, NoInfo) loc
bareExp (OpSectionRight name _ arg _ _ loc) =
  OpSectionRight name NoInfo (bareExp arg) (NoInfo, NoInfo) NoInfo loc
bareExp (ProjectSection fields _ loc) = ProjectSection fields NoInfo loc
bareExp (IndexSection slice _ loc) =
  IndexSection (map bareDimIndex slice) NoInfo loc
bareExp (DoLoop _ mergepat mergeexp form loopbody _ loc) =
  DoLoop [] (barePat mergepat) (bareExp mergeexp) (bareLoopForm form)
  (bareExp loopbody) NoInfo loc
bareExp (Constr name es _ loc) =
  Constr name (map bareExp es) NoInfo loc
bareExp (Match e cases _ loc) =
  Match (bareExp e) (fmap bareCase cases) (NoInfo,NoInfo) loc
