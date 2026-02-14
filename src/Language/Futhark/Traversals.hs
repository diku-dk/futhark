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
  ( ASTMapper (..),
    ASTMappable (..),
    identityMapper,
    bareExp,
  )
where

import Data.Bifunctor
import Data.Bitraversable
import Data.List.NonEmpty qualified as NE
import Language.Futhark.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data ASTMapper m = ASTMapper
  { mapOnExp :: ExpBase Info VName -> m (ExpBase Info VName),
    mapOnName :: QualName VName -> m (QualName VName),
    mapOnStructType :: StructType -> m StructType,
    mapOnParamType :: ParamType -> m ParamType,
    mapOnResRetType :: ResRetType -> m ResRetType
  }

-- | An 'ASTMapper' that just leaves its input unchanged.
identityMapper :: (Monad m) => ASTMapper m
identityMapper =
  ASTMapper
    { mapOnExp = pure,
      mapOnName = pure,
      mapOnStructType = pure,
      mapOnParamType = pure,
      mapOnResRetType = pure
    }

-- | The class of things that we can map an 'ASTMapper' across.
class ASTMappable x where
  -- | Map a monadic action across the immediate children of an
  -- object.  Importantly, the 'astMap' action is not invoked for
  -- the object itself, and the mapping does not descend recursively
  -- into subexpressions.  The mapping is done left-to-right.
  astMap :: (Monad m) => ASTMapper m -> x -> m x

instance ASTMappable (AppExpBase Info VName) where
  astMap tv (Range start next end loc) =
    Range
      <$> mapOnExp tv start
      <*> traverse (mapOnExp tv) next
      <*> traverse (mapOnExp tv) end
      <*> pure loc
  astMap tv (If c texp fexp loc) =
    If <$> mapOnExp tv c <*> mapOnExp tv texp <*> mapOnExp tv fexp <*> pure loc
  astMap tv (Match e cases loc) =
    Match <$> mapOnExp tv e <*> astMap tv cases <*> pure loc
  astMap tv (Apply f args loc) = do
    f' <- mapOnExp tv f
    args' <- traverse (traverse $ mapOnExp tv) args
    -- Safe to disregard return type because existentials cannot be
    -- instantiated here, as the return is necessarily a function.
    pure $ case f' of
      AppExp (Apply f_inner args_inner _) _ ->
        Apply f_inner (args_inner <> args') loc
      _ ->
        Apply f' args' loc
  astMap tv (LetPat sizes pat e body loc) =
    LetPat sizes <$> astMap tv pat <*> mapOnExp tv e <*> mapOnExp tv body <*> pure loc
  astMap tv (LetFun name (tparams, params, ret, t, e) body loc) =
    LetFun name
      <$> ( (tparams,,,,)
              <$> mapM (astMap tv) params
              <*> traverse (astMap tv) ret
              <*> traverse (mapOnResRetType tv) t
              <*> mapOnExp tv e
          )
      <*> mapOnExp tv body
      <*> pure loc
  astMap tv (LetWith dest src idxexps vexp body loc) =
    LetWith
      <$> astMap tv dest
      <*> astMap tv src
      <*> mapM (astMap tv) idxexps
      <*> mapOnExp tv vexp
      <*> mapOnExp tv body
      <*> pure loc
  astMap tv (LetWithField dest src fields vexp body loc) =
    LetWithField
      <$> astMap tv dest
      <*> astMap tv src
      <*> pure fields
      <*> mapOnExp tv vexp
      <*> mapOnExp tv body
      <*> pure loc
  astMap tv (BinOp (fname, fname_loc) t (x, xext) (y, yext) loc) =
    BinOp
      <$> ((,) <$> mapOnName tv fname <*> pure fname_loc)
      <*> traverse (mapOnStructType tv) t
      <*> ((,) <$> mapOnExp tv x <*> pure xext)
      <*> ((,) <$> mapOnExp tv y <*> pure yext)
      <*> pure loc
  astMap tv (Loop sparams mergepat loopinit form loopbody loc) =
    Loop sparams
      <$> astMap tv mergepat
      <*> astMap tv loopinit
      <*> astMap tv form
      <*> mapOnExp tv loopbody
      <*> pure loc
  astMap tv (Index arr idxexps loc) =
    Index <$> mapOnExp tv arr <*> mapM (astMap tv) idxexps <*> pure loc

instance ASTMappable (ExpBase Info VName) where
  astMap tv (Var name t loc) =
    Var
      <$> mapOnName tv name
      <*> traverse (mapOnStructType tv) t
      <*> pure loc
  astMap tv (Hole t loc) =
    Hole <$> traverse (mapOnStructType tv) t <*> pure loc
  astMap _ (Literal val loc) =
    pure $ Literal val loc
  astMap _ (StringLit vs loc) =
    pure $ StringLit vs loc
  astMap tv (IntLit val t loc) =
    IntLit val <$> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (FloatLit val t loc) =
    FloatLit val <$> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (Parens e loc) =
    Parens <$> mapOnExp tv e <*> pure loc
  astMap tv (QualParens (name, nameloc) e loc) =
    QualParens
      <$> ((,) <$> mapOnName tv name <*> pure nameloc)
      <*> mapOnExp tv e
      <*> pure loc
  astMap tv (TupLit els loc) =
    TupLit <$> mapM (mapOnExp tv) els <*> pure loc
  astMap tv (RecordLit fields loc) =
    RecordLit <$> astMap tv fields <*> pure loc
  astMap _ (ArrayVal vs t loc) =
    pure $ ArrayVal vs t loc
  astMap tv (ArrayLit els t loc) =
    ArrayLit <$> mapM (mapOnExp tv) els <*> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (Ascript e tdecl loc) =
    Ascript <$> mapOnExp tv e <*> astMap tv tdecl <*> pure loc
  astMap tv (Coerce e tdecl t loc) =
    Coerce <$> mapOnExp tv e <*> astMap tv tdecl <*> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (Negate x loc) =
    Negate <$> mapOnExp tv x <*> pure loc
  astMap tv (Not x loc) =
    Not <$> mapOnExp tv x <*> pure loc
  astMap tv (Update src slice v loc) =
    Update
      <$> mapOnExp tv src
      <*> mapM (astMap tv) slice
      <*> mapOnExp tv v
      <*> pure loc
  astMap tv (RecordUpdate src fs v (Info t) loc) =
    RecordUpdate
      <$> mapOnExp tv src
      <*> pure fs
      <*> mapOnExp tv v
      <*> (Info <$> mapOnStructType tv t)
      <*> pure loc
  astMap tv (UpdateFieldInRecArray src slice fields v (Info t) loc) =
    UpdateFieldInRecArray
      <$> mapOnExp tv src
      <*> mapM (astMap tv) slice
      <*> pure fields
      <*> mapOnExp tv v
      <*> (Info <$> mapOnStructType tv t)
      <*> pure loc
  astMap tv (Project field e t loc) =
    Project field <$> mapOnExp tv e <*> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (Assert e1 e2 desc loc) =
    Assert <$> mapOnExp tv e1 <*> mapOnExp tv e2 <*> pure desc <*> pure loc
  astMap tv (Lambda params body ret t loc) =
    Lambda
      <$> mapM (astMap tv) params
      <*> mapOnExp tv body
      <*> traverse (astMap tv) ret
      <*> traverse (mapOnResRetType tv) t
      <*> pure loc
  astMap tv (OpSection name t loc) =
    OpSection
      <$> mapOnName tv name
      <*> traverse (mapOnStructType tv) t
      <*> pure loc
  astMap tv (OpSectionLeft name t arg (Info (pa, t1a, argext), Info (pb, t1b)) (ret, retext) loc) =
    OpSectionLeft
      <$> mapOnName tv name
      <*> traverse (mapOnStructType tv) t
      <*> mapOnExp tv arg
      <*> ( (,)
              <$> (Info <$> ((pa,,) <$> mapOnParamType tv t1a <*> pure argext))
              <*> (Info <$> ((pb,) <$> mapOnParamType tv t1b))
          )
      <*> ((,) <$> traverse (mapOnResRetType tv) ret <*> pure retext)
      <*> pure loc
  astMap tv (OpSectionRight name t arg (Info (pa, t1a), Info (pb, t1b, argext)) t2 loc) =
    OpSectionRight
      <$> mapOnName tv name
      <*> traverse (mapOnStructType tv) t
      <*> mapOnExp tv arg
      <*> ( (,)
              <$> (Info <$> ((pa,) <$> mapOnParamType tv t1a))
              <*> (Info <$> ((pb,,) <$> mapOnParamType tv t1b <*> pure argext))
          )
      <*> traverse (mapOnResRetType tv) t2
      <*> pure loc
  astMap tv (ProjectSection fields t loc) =
    ProjectSection fields <$> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (IndexSection idxs t loc) =
    IndexSection
      <$> mapM (astMap tv) idxs
      <*> traverse (mapOnStructType tv) t
      <*> pure loc
  astMap tv (Constr name es t loc) =
    Constr name <$> traverse (mapOnExp tv) es <*> traverse (mapOnStructType tv) t <*> pure loc
  astMap tv (Attr attr e loc) =
    Attr attr <$> mapOnExp tv e <*> pure loc
  astMap tv (AppExp e res) =
    AppExp <$> astMap tv e <*> astMap tv res

instance ASTMappable (LoopInitBase Info VName) where
  astMap tv (LoopInitExplicit e) =
    LoopInitExplicit <$> mapOnExp tv e
  astMap tv (LoopInitImplicit (Info e)) =
    LoopInitImplicit . Info <$> mapOnExp tv e

instance ASTMappable (LoopFormBase Info VName) where
  astMap tv (For i bound) = For <$> astMap tv i <*> mapOnExp tv bound
  astMap tv (ForIn pat e) = ForIn <$> astMap tv pat <*> mapOnExp tv e
  astMap tv (While e) = While <$> mapOnExp tv e

instance ASTMappable (TypeExp (ExpBase Info VName) VName) where
  astMap tv (TEVar qn loc) =
    TEVar <$> mapOnName tv qn <*> pure loc
  astMap tv (TEParens te loc) =
    TEParens <$> astMap tv te <*> pure loc
  astMap tv (TETuple ts loc) =
    TETuple <$> traverse (astMap tv) ts <*> pure loc
  astMap tv (TERecord ts loc) =
    TERecord <$> traverse (traverse $ astMap tv) ts <*> pure loc
  astMap tv (TEArray te dim loc) =
    TEArray <$> astMap tv te <*> astMap tv dim <*> pure loc
  astMap tv (TEUnique t loc) =
    TEUnique <$> astMap tv t <*> pure loc
  astMap tv (TEApply t1 t2 loc) =
    TEApply <$> astMap tv t1 <*> astMap tv t2 <*> pure loc
  astMap tv (TEArrow v t1 t2 loc) =
    TEArrow v <$> astMap tv t1 <*> astMap tv t2 <*> pure loc
  astMap tv (TESum cs loc) =
    TESum <$> traverse (traverse $ astMap tv) cs <*> pure loc
  astMap tv (TEDim dims t loc) =
    TEDim dims <$> astMap tv t <*> pure loc

instance ASTMappable (TypeArgExp (ExpBase Info VName) VName) where
  astMap tv (TypeArgExpSize dim) = TypeArgExpSize <$> astMap tv dim
  astMap tv (TypeArgExpType te) = TypeArgExpType <$> astMap tv te

instance ASTMappable (SizeExp (ExpBase Info VName)) where
  astMap tv (SizeExp e loc) = SizeExp <$> mapOnExp tv e <*> pure loc
  astMap _ (SizeExpAny loc) = pure $ SizeExpAny loc

instance ASTMappable (DimIndexBase Info VName) where
  astMap tv (DimFix j) = DimFix <$> mapOnExp tv j
  astMap tv (DimSlice i j stride) =
    DimSlice
      <$> maybe (pure Nothing) (fmap Just . mapOnExp tv) i
      <*> maybe (pure Nothing) (fmap Just . mapOnExp tv) j
      <*> maybe (pure Nothing) (fmap Just . mapOnExp tv) stride

instance ASTMappable AppRes where
  astMap tv (AppRes t ext) =
    AppRes <$> mapOnStructType tv t <*> pure ext

type TypeTraverser f t dim1 als1 dim2 als2 =
  (QualName VName -> f (QualName VName)) ->
  (dim1 -> f dim2) ->
  (als1 -> f als2) ->
  t dim1 als1 ->
  f (t dim2 als2)

traverseScalarType ::
  (Applicative f) =>
  TypeTraverser f ScalarTypeBase dim1 als1 dims als2
traverseScalarType _ _ _ (Prim t) = pure $ Prim t
traverseScalarType f g h (Record fs) = Record <$> traverse (traverseType f g h) fs
traverseScalarType f g h (TypeVar als t args) =
  TypeVar <$> h als <*> f t <*> traverse (traverseTypeArg f g) args
traverseScalarType f g h (Arrow als v u t1 (RetType dims t2)) =
  Arrow
    <$> h als
    <*> pure v
    <*> pure u
    <*> traverseType f g pure t1
    <*> (RetType dims <$> traverseType f g pure t2)
traverseScalarType f g h (Sum cs) =
  Sum <$> (traverse . traverse) (traverseType f g h) cs

traverseType :: (Applicative f) => TypeTraverser f TypeBase dim1 als1 dims als2
traverseType f g h (Array als shape et) =
  Array <$> h als <*> traverse g shape <*> traverseScalarType f g pure et
traverseType f g h (Scalar t) =
  Scalar <$> traverseScalarType f g h t

traverseTypeArg ::
  (Applicative f) =>
  (QualName VName -> f (QualName VName)) ->
  (dim1 -> f dim2) ->
  TypeArg dim1 ->
  f (TypeArg dim2)
traverseTypeArg _ g (TypeArgDim d) =
  TypeArgDim <$> g d
traverseTypeArg f g (TypeArgType t) =
  TypeArgType <$> traverseType f g pure t

instance ASTMappable StructType where
  astMap tv = traverseType (mapOnName tv) (mapOnExp tv) pure

instance ASTMappable ParamType where
  astMap tv = traverseType (mapOnName tv) (mapOnExp tv) pure

instance ASTMappable (TypeBase Size Uniqueness) where
  astMap tv = traverseType (mapOnName tv) (mapOnExp tv) pure

instance ASTMappable ResRetType where
  astMap tv (RetType ext t) = RetType ext <$> astMap tv t

instance ASTMappable (IdentBase Info VName StructType) where
  astMap tv (Ident name (Info t) loc) =
    Ident name <$> (Info <$> mapOnStructType tv t) <*> pure loc

traversePat ::
  (Monad m) =>
  (t1 -> m t2) ->
  (ExpBase Info VName -> m (ExpBase Info VName)) ->
  PatBase Info VName t1 ->
  m (PatBase Info VName t2)
traversePat f _ (Id name (Info t) loc) =
  Id name <$> (Info <$> f t) <*> pure loc
traversePat f g (TuplePat pats loc) =
  TuplePat <$> mapM (traversePat f g) pats <*> pure loc
traversePat f g (RecordPat fields loc) =
  RecordPat <$> mapM (traverse $ traversePat f g) fields <*> pure loc
traversePat f g (PatParens pat loc) =
  PatParens <$> traversePat f g pat <*> pure loc
traversePat f g (PatAscription pat t loc) =
  PatAscription <$> traversePat f g pat <*> bitraverse g pure t <*> pure loc
traversePat f _ (Wildcard (Info t) loc) =
  Wildcard <$> (Info <$> f t) <*> pure loc
traversePat f _ (PatLit v (Info t) loc) =
  PatLit v <$> (Info <$> f t) <*> pure loc
traversePat f g (PatConstr n (Info t) ps loc) =
  PatConstr n <$> (Info <$> f t) <*> mapM (traversePat f g) ps <*> pure loc
traversePat f g (PatAttr attr p loc) =
  PatAttr attr <$> traversePat f g p <*> pure loc

instance ASTMappable (PatBase Info VName StructType) where
  astMap tv = traversePat (mapOnStructType tv) (mapOnExp tv)

instance ASTMappable (PatBase Info VName ParamType) where
  astMap tv = traversePat (mapOnParamType tv) (mapOnExp tv)

instance ASTMappable (FieldBase Info VName) where
  astMap tv (RecordFieldExplicit name e loc) =
    RecordFieldExplicit name <$> mapOnExp tv e <*> pure loc
  astMap tv (RecordFieldImplicit (L nameloc name) t loc) =
    RecordFieldImplicit
      <$> (L nameloc <$> (qualLeaf <$> mapOnName tv (QualName [] name)))
      <*> traverse (mapOnStructType tv) t
      <*> pure loc

instance ASTMappable (CaseBase Info VName) where
  astMap tv (CasePat pat e loc) =
    CasePat <$> astMap tv pat <*> mapOnExp tv e <*> pure loc

instance (ASTMappable a) => ASTMappable (Info a) where
  astMap tv = traverse $ astMap tv

instance (ASTMappable a) => ASTMappable [a] where
  astMap tv = traverse $ astMap tv

instance (ASTMappable a) => ASTMappable (NE.NonEmpty a) where
  astMap tv = traverse $ astMap tv

instance (ASTMappable a, ASTMappable b) => ASTMappable (a, b) where
  astMap tv (x, y) = (,) <$> astMap tv x <*> astMap tv y

instance (ASTMappable a, ASTMappable b, ASTMappable c) => ASTMappable (a, b, c) where
  astMap tv (x, y, z) = (,,) <$> astMap tv x <*> astMap tv y <*> astMap tv z

-- It would be lovely if the following code would be written in terms
-- of ASTMappable, but unfortunately it involves changing the Info
-- functor.  For simplicity, the general traversals do not support
-- that.  Sometimes a little duplication is better than an overly
-- complex abstraction.  The types ensure that this will be correct
-- anyway, so it's just tedious, and not actually fragile.

bareField :: FieldBase Info VName -> FieldBase NoInfo VName
bareField (RecordFieldExplicit name e loc) =
  RecordFieldExplicit name (bareExp e) loc
bareField (RecordFieldImplicit name _ loc) =
  RecordFieldImplicit name NoInfo loc

barePat :: PatBase Info VName t -> PatBase NoInfo VName t
barePat (TuplePat ps loc) = TuplePat (map barePat ps) loc
barePat (RecordPat fs loc) = RecordPat (map (fmap barePat) fs) loc
barePat (PatParens p loc) = PatParens (barePat p) loc
barePat (Id v _ loc) = Id v NoInfo loc
barePat (Wildcard _ loc) = Wildcard NoInfo loc
barePat (PatAscription pat t loc) = PatAscription (barePat pat) (bareTypeExp t) loc
barePat (PatLit v _ loc) = PatLit v NoInfo loc
barePat (PatConstr c _ ps loc) = PatConstr c NoInfo (map barePat ps) loc
barePat (PatAttr attr p loc) = PatAttr attr (barePat p) loc

bareDimIndex :: DimIndexBase Info VName -> DimIndexBase NoInfo VName
bareDimIndex (DimFix e) =
  DimFix $ bareExp e
bareDimIndex (DimSlice x y z) =
  DimSlice (bareExp <$> x) (bareExp <$> y) (bareExp <$> z)

bareLoopInit :: LoopInitBase Info VName -> LoopInitBase NoInfo VName
bareLoopInit (LoopInitExplicit e) = LoopInitExplicit $ bareExp e
bareLoopInit (LoopInitImplicit _) = LoopInitImplicit NoInfo

bareLoopForm :: LoopFormBase Info VName -> LoopFormBase NoInfo VName
bareLoopForm (For (Ident i _ loc) e) = For (Ident i NoInfo loc) (bareExp e)
bareLoopForm (ForIn pat e) = ForIn (barePat pat) (bareExp e)
bareLoopForm (While e) = While (bareExp e)

bareCase :: CaseBase Info VName -> CaseBase NoInfo VName
bareCase (CasePat pat e loc) = CasePat (barePat pat) (bareExp e) loc

bareSizeExp :: SizeExp (ExpBase Info VName) -> SizeExp (ExpBase NoInfo VName)
bareSizeExp (SizeExp e loc) = SizeExp (bareExp e) loc
bareSizeExp (SizeExpAny loc) = SizeExpAny loc

bareTypeExp :: TypeExp (ExpBase Info VName) VName -> TypeExp (ExpBase NoInfo VName) VName
bareTypeExp (TEVar qn loc) = TEVar qn loc
bareTypeExp (TEParens te loc) = TEParens (bareTypeExp te) loc
bareTypeExp (TETuple tys loc) = TETuple (map bareTypeExp tys) loc
bareTypeExp (TERecord fs loc) = TERecord (map (second bareTypeExp) fs) loc
bareTypeExp (TEArray size ty loc) = TEArray (bareSizeExp size) (bareTypeExp ty) loc
bareTypeExp (TEUnique ty loc) = TEUnique (bareTypeExp ty) loc
bareTypeExp (TEApply ty ta loc) = TEApply (bareTypeExp ty) (bareTypeArgExp ta) loc
  where
    bareTypeArgExp (TypeArgExpSize size) =
      TypeArgExpSize $ bareSizeExp size
    bareTypeArgExp (TypeArgExpType tya) =
      TypeArgExpType $ bareTypeExp tya
bareTypeExp (TEArrow arg tya tyr loc) = TEArrow arg (bareTypeExp tya) (bareTypeExp tyr) loc
bareTypeExp (TESum cs loc) = TESum (map (second $ map bareTypeExp) cs) loc
bareTypeExp (TEDim names ty loc) = TEDim names (bareTypeExp ty) loc

-- | Remove all annotations from an expression, but retain the
-- name/scope information.
bareExp :: ExpBase Info VName -> ExpBase NoInfo VName
bareExp (Var name _ loc) = Var name NoInfo loc
bareExp (Hole _ loc) = Hole NoInfo loc
bareExp (Literal v loc) = Literal v loc
bareExp (IntLit val _ loc) = IntLit val NoInfo loc
bareExp (FloatLit val _ loc) = FloatLit val NoInfo loc
bareExp (Parens e loc) = Parens (bareExp e) loc
bareExp (QualParens name e loc) = QualParens name (bareExp e) loc
bareExp (TupLit els loc) = TupLit (map bareExp els) loc
bareExp (StringLit vs loc) = StringLit vs loc
bareExp (RecordLit fields loc) = RecordLit (map bareField fields) loc
bareExp (ArrayVal vs t loc) = ArrayVal vs t loc
bareExp (ArrayLit els _ loc) = ArrayLit (map bareExp els) NoInfo loc
bareExp (Ascript e te loc) = Ascript (bareExp e) (bareTypeExp te) loc
bareExp (Coerce e te _ loc) = Coerce (bareExp e) (bareTypeExp te) NoInfo loc
bareExp (Negate x loc) = Negate (bareExp x) loc
bareExp (Not x loc) = Not (bareExp x) loc
bareExp (Update src slice v loc) =
  Update (bareExp src) (map bareDimIndex slice) (bareExp v) loc
bareExp (RecordUpdate src fs v _ loc) =
  RecordUpdate (bareExp src) fs (bareExp v) NoInfo loc
bareExp (Project field e _ loc) =
  Project field (bareExp e) NoInfo loc
bareExp (Assert e1 e2 _ loc) = Assert (bareExp e1) (bareExp e2) NoInfo loc
bareExp (Lambda params body ret _ loc) =
  Lambda (map barePat params) (bareExp body) (fmap bareTypeExp ret) NoInfo loc
bareExp (OpSection name _ loc) = OpSection name NoInfo loc
bareExp (OpSectionLeft name _ arg _ _ loc) =
  OpSectionLeft name NoInfo (bareExp arg) (NoInfo, NoInfo) (NoInfo, NoInfo) loc
bareExp (OpSectionRight name _ arg _ _ loc) =
  OpSectionRight name NoInfo (bareExp arg) (NoInfo, NoInfo) NoInfo loc
bareExp (ProjectSection fields _ loc) = ProjectSection fields NoInfo loc
bareExp (IndexSection slice _ loc) =
  IndexSection (map bareDimIndex slice) NoInfo loc
bareExp (Constr name es _ loc) =
  Constr name (map bareExp es) NoInfo loc
bareExp (AppExp appexp _) =
  AppExp appexp' NoInfo
  where
    appexp' =
      case appexp of
        Match e cases loc ->
          Match (bareExp e) (fmap bareCase cases) loc
        Loop _ mergepat loopinit form loopbody loc ->
          Loop
            []
            (barePat mergepat)
            (bareLoopInit loopinit)
            (bareLoopForm form)
            (bareExp loopbody)
            loc
        LetWith (Ident dest _ destloc) (Ident src _ srcloc) idxexps vexp body loc ->
          LetWith
            (Ident dest NoInfo destloc)
            (Ident src NoInfo srcloc)
            (map bareDimIndex idxexps)
            (bareExp vexp)
            (bareExp body)
            loc
        LetWithField (Ident dest _ destloc) (Ident src _ srcloc) fields vexp body loc ->
          LetWithField
            (Ident dest NoInfo destloc)
            (Ident src NoInfo srcloc)
            fields
            (bareExp vexp)
            (bareExp body)
            loc
        BinOp fname _ (x, _) (y, _) loc ->
          BinOp fname NoInfo (bareExp x, NoInfo) (bareExp y, NoInfo) loc
        If c texp fexp loc ->
          If (bareExp c) (bareExp texp) (bareExp fexp) loc
        Apply f args loc ->
          Apply (bareExp f) (fmap ((NoInfo,) . bareExp . snd) args) loc
        LetPat sizes pat e body loc ->
          LetPat sizes (barePat pat) (bareExp e) (bareExp body) loc
        LetFun name (fparams, params, ret, _, e) body loc ->
          LetFun
            name
            (fparams, map barePat params, fmap bareTypeExp ret, NoInfo, bareExp e)
            (bareExp body)
            loc
        Range start next end loc ->
          Range (bareExp start) (fmap bareExp next) (fmap bareExp end) loc
        Index arr slice loc ->
          Index (bareExp arr) (map bareDimIndex slice) loc
bareExp (Attr attr e loc) =
  Attr attr (bareExp e) loc
bareExp (UpdateFieldInRecArray src slice fs v _ loc) =
  UpdateFieldInRecArray
    (bareExp src)
    (map bareDimIndex slice)
    fs
    (bareExp v)
    NoInfo
    loc