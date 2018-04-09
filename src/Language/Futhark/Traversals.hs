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
-- A traversal of the Futhark syntax tree is expressed as a tuple of
-- functions expressing the operations to be performed on the various
-- types of nodes.
module Language.Futhark.Traversals
  ( ASTMapper(..)
  , ASTMappable(..)
  ) where

import qualified Data.Set                as S

import           Language.Futhark.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data ASTMapper m = ASTMapper {
    mapOnExp         :: ExpBase Info VName -> m (ExpBase Info VName)
  , mapOnName        :: VName -> m VName
  , mapOnQualName    :: QualName VName -> m (QualName VName)
  , mapOnType        :: TypeBase () () -> m (TypeBase () ())
  , mapOnCompType    :: CompType -> m CompType
  , mapOnStructType  :: StructType -> m StructType
  , mapOnPatternType :: PatternType -> m PatternType
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
  astMap tv (Parens e loc) =
    Parens <$> mapOnExp tv e <*> pure loc
  astMap tv (QualParens name e loc) =
    QualParens <$> mapOnQualName tv name <*> mapOnExp tv e <*> pure loc
  astMap tv (TupLit els loc) =
    TupLit <$> mapM (mapOnExp tv) els <*> pure loc
  astMap tv (RecordLit fields loc) =
    RecordLit <$> astMap tv fields <*> pure loc
  astMap tv (ArrayLit els t loc) =
    ArrayLit <$> mapM (mapOnExp tv) els <*> traverse (mapOnCompType tv) t <*> pure loc
  astMap tv (Range start next end t loc) =
    Range <$> mapOnExp tv start <*> traverse (mapOnExp tv) next <*>
    traverse (mapOnExp tv) end <*> traverse (mapOnCompType tv) t <*> pure loc
  astMap tv (Ascript e tdecl loc) =
    Ascript <$> mapOnExp tv e <*> astMap tv tdecl <*> pure loc
  astMap tv (Empty tdecl t loc) =
    Empty <$> astMap tv tdecl <*> traverse (astMap tv) t <*> pure loc
  astMap tv (BinOp fname t (x,xt) (y,yt) (Info rt) loc) =
    BinOp <$> mapOnQualName tv fname <*> traverse (mapOnPatternType tv) t <*>
    ((,) <$> mapOnExp tv x <*> traverse (mapOnStructType tv) xt) <*>
    ((,) <$> mapOnExp tv y <*> traverse (mapOnStructType tv) yt) <*>
    (Info <$> mapOnPatternType tv rt) <*> pure loc
  astMap tv (Negate x loc) =
    Negate <$> mapOnExp tv x <*> pure loc
  astMap tv (If c texp fexp t loc) =
    If <$> mapOnExp tv c <*> mapOnExp tv texp <*> mapOnExp tv fexp <*>
    traverse (mapOnCompType tv) t <*> pure loc
  astMap tv (Apply f arg d (Info t) loc) =
    Apply <$> mapOnExp tv f <*> mapOnExp tv arg <*>
    pure d <*> (Info <$> mapOnPatternType tv t) <*>
    pure loc
  astMap tv (LetPat tparams pat e body loc) =
    LetPat <$> mapM (astMap tv) tparams <*>
    astMap tv pat <*> mapOnExp tv e <*>
    mapOnExp tv body <*> pure loc
  astMap tv (LetFun name (fparams, params, ret, t, e) body loc) =
    LetFun <$> mapOnName tv name <*>
    ((,,,,) <$> mapM (astMap tv) fparams <*> mapM (astMap tv) params <*>
     traverse (astMap tv) ret <*> traverse (mapOnStructType tv) t <*>
     mapOnExp tv e) <*>
    mapOnExp tv body <*> pure loc
  astMap tv (LetWith dest src idxexps vexp body loc) =
    pure LetWith <*>
         astMap tv dest <*> astMap tv src <*>
         mapM (astMap tv) idxexps <*> mapOnExp tv vexp <*>
         mapOnExp tv body <*> pure loc
  astMap tv (Update src slice v loc) =
    Update <$> mapOnExp tv src <*> mapM (astMap tv) slice <*>
    mapOnExp tv v <*> pure loc
  astMap tv (Project field e t loc) =
    Project field <$> mapOnExp tv e <*> traverse (mapOnCompType tv) t <*> pure loc
  astMap tv (Index arr idxexps t loc) =
    pure Index <*>
         astMap tv arr <*>
         mapM (astMap tv) idxexps <*>
         traverse (mapOnCompType tv) t <*>
         pure loc
  astMap tv (Reshape shape arrexp info loc) =
    Reshape <$> mapOnExp tv shape <*> mapOnExp tv arrexp <*> pure info <*> pure loc
  astMap tv (Rotate d e a loc) =
    Rotate d <$> mapOnExp tv e <*> mapOnExp tv a <*> pure loc
  astMap tv (Rearrange perm e loc) =
    pure Rearrange <*> pure perm <*> mapOnExp tv e <*> pure loc
  astMap tv (Map fun e t loc) =
    Map <$> mapOnExp tv fun <*> mapOnExp tv e <*>
    traverse (mapOnCompType tv) t <*> pure loc
  astMap tv (Reduce comm fun startexp arrexp loc) =
    Reduce comm <$> mapOnExp tv fun <*>
    mapOnExp tv startexp <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Zip i e es t loc) =
    Zip i <$> mapOnExp tv e <*> mapM (mapOnExp tv) es <*>
    traverse (mapOnCompType tv) t <*> pure loc
  astMap tv (Unzip e ts loc) =
    Unzip <$> mapOnExp tv e <*> mapM (traverse $ mapOnCompType tv) ts <*> pure loc
  astMap tv (Unsafe e loc) =
    Unsafe <$> mapOnExp tv e <*> pure loc
  astMap tv (Scan fun startexp arrexp loc) =
    pure Scan <*> mapOnExp tv fun <*>
         mapOnExp tv startexp <*> mapOnExp tv arrexp <*>
         pure loc
  astMap tv (Filter fun arrexp loc) =
    pure Filter <*> mapOnExp tv fun <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Partition k fun arrexp loc) =
    Partition k <$> mapOnExp tv fun <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Stream form fun arr loc) =
    pure Stream <*> mapOnStreamForm form <*> mapOnExp tv fun <*>
         mapOnExp tv arr <*> pure loc
    where mapOnStreamForm (MapLike o) = pure $ MapLike o
          mapOnStreamForm (RedLike o comm lam) =
              RedLike o comm <$> mapOnExp tv lam
  astMap tv (Concat i x y loc) =
    Concat i <$> mapOnExp tv x <*> mapOnExp tv y <*> pure loc
  astMap tv (Lambda tparams params body ret t loc) =
    Lambda <$> mapM (astMap tv) tparams <*> mapM (astMap tv) params <*>
    astMap tv body <*> traverse (astMap tv) ret <*>
    traverse (traverse $ mapOnStructType tv) t <*> pure loc
  astMap tv (OpSection name t t1 t2 t3 loc) =
    OpSection <$> mapOnQualName tv name <*>
    traverse (mapOnPatternType tv) t <*>
    traverse (mapOnStructType tv) t1 <*> traverse (mapOnStructType tv) t2 <*>
    traverse (mapOnPatternType tv) t3 <*> pure loc
  astMap tv (OpSectionLeft name t arg (t1a, t1b) t2 loc) =
    OpSectionLeft <$> mapOnQualName tv name <*>
    traverse (mapOnPatternType tv) t <*> mapOnExp tv arg <*>
    ((,) <$> traverse (mapOnStructType tv) t1a <*>
      traverse (mapOnStructType tv) t1b) <*>
    traverse (mapOnPatternType tv) t2 <*> pure loc
  astMap tv (OpSectionRight name t arg (t1a, t1b) t2 loc) =
    OpSectionRight <$> mapOnQualName tv name <*>
    traverse (mapOnPatternType tv) t <*> mapOnExp tv arg <*>
    ((,) <$> traverse (mapOnStructType tv) t1a <*>
     traverse (mapOnStructType tv) t1b) <*>
    traverse (mapOnPatternType tv) t2 <*> pure loc
  astMap tv (DoLoop tparams mergepat mergeexp form loopbody loc) =
    DoLoop <$> mapM (astMap tv) tparams <*> astMap tv mergepat <*>
    mapOnExp tv mergeexp <*> astMap tv form <*>
    mapOnExp tv loopbody <*> pure loc

instance ASTMappable (LoopFormBase Info VName) where
  astMap tv (For i bound) = For <$> astMap tv i <*> astMap tv bound
  astMap tv (ForIn pat e) = ForIn <$> astMap tv pat <*> astMap tv e
  astMap tv (While e)     = While <$> astMap tv e

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

instance ASTMappable (TypeArgExp VName) where
  astMap tv (TypeArgExpDim dim loc) =
    TypeArgExpDim <$> astMap tv dim <*> pure loc
  astMap tv (TypeArgExpType te) =
    TypeArgExpType <$> astMap tv te

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

instance ASTMappable Names where
  astMap tv = fmap S.fromList . traverse (mapOnName tv) . S.toList

type TypeTraverser f t dim1 als1 dim2 als2 =
  (TypeName -> f TypeName) -> (dim1 -> f dim2) -> (als1 -> f als2) ->
  t dim1 als1 -> f (t dim2 als2)

traverseType :: Applicative f =>
                TypeTraverser f TypeBase dim1 als1 dims als2
traverseType _ _ _ (Prim t) = pure $ Prim t
traverseType f g h (Array et shape u) =
  Array <$> traverseArrayElemType f g h et <*> traverse g shape <*> pure u
traverseType f g h (Record fs) = Record <$> traverse (traverseType f g h) fs
traverseType f g h (TypeVar t args) = TypeVar <$> f t <*> traverse (traverseTypeArg f g h) args
traverseType f g h (Arrow als v t1 t2) =
  Arrow <$> h als <*> pure v <*> traverseType f g h t1 <*> traverseType f g h t2

traverseArrayElemType :: Applicative f =>
                         TypeTraverser f ArrayElemTypeBase dim1 als1 dim2 als2
traverseArrayElemType _ _ h (ArrayPrimElem t as) =
  ArrayPrimElem t <$> h as
traverseArrayElemType f g h (ArrayPolyElem t args as) =
  ArrayPolyElem <$> f t <*> traverse (traverseTypeArg f g h) args <*> h as
traverseArrayElemType f g h (ArrayRecordElem fs) =
  ArrayRecordElem <$> traverse (traverseRecordArrayElemType f g h) fs

traverseRecordArrayElemType :: Applicative f =>
                               TypeTraverser f RecordArrayElemTypeBase dim1 als1 dim2 als2
traverseRecordArrayElemType f g h (RecordArrayElem et) =
  RecordArrayElem <$> traverseArrayElemType f g h et
traverseRecordArrayElemType f g h (RecordArrayArrayElem et shape u) =
  RecordArrayArrayElem <$> traverseArrayElemType f g h et <*>
  traverse g shape <*> pure u

traverseTypeArg :: Applicative f =>
                   TypeTraverser f TypeArg dim1 als1 dim2 als2
traverseTypeArg _ g _ (TypeArgDim d loc) = TypeArgDim <$> g d <*> pure loc
traverseTypeArg f g h (TypeArgType t loc) = TypeArgType <$> traverseType f g h t <*> pure loc

instance ASTMappable (TypeBase () ()) where
  astMap tv = traverseType f pure pure
    where f = fmap typeNameFromQualName . mapOnQualName tv . qualNameFromTypeName

instance ASTMappable CompType where
  astMap tv = traverseType f pure (astMap tv)
    where f = fmap typeNameFromQualName . mapOnQualName tv . qualNameFromTypeName

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
    Ident <$> mapOnName tv name <*> (Info <$> mapOnCompType tv t) <*> pure loc

instance ASTMappable (PatternBase Info VName) where
  astMap tv (Id name (Info t) loc) =
    Id <$> mapOnName tv name <*> (Info <$> mapOnPatternType tv t) <*> pure loc
  astMap tv (TuplePattern pats loc) =
    TuplePattern <$> mapM (astMap tv) pats <*> pure loc
  astMap tv (RecordPattern fields loc) =
    RecordPattern <$> mapM (traverse $ astMap tv) fields <*> pure loc
  astMap tv (PatternParens pat loc) =
    PatternParens <$> astMap tv pat <*> pure loc
  astMap tv (PatternAscription pat t) =
    PatternAscription <$> astMap tv pat <*> astMap tv t
  astMap tv (Wildcard (Info t) loc) =
    Wildcard <$> (Info <$> mapOnPatternType tv t) <*> pure loc

instance ASTMappable (FieldBase Info VName) where
  astMap tv (RecordFieldExplicit name e loc) =
    RecordFieldExplicit name <$> mapOnExp tv e <*> pure loc
  astMap tv (RecordFieldImplicit name t loc) =
    RecordFieldImplicit <$> mapOnName tv name
    <*> traverse (mapOnCompType tv) t <*> pure loc

instance ASTMappable a => ASTMappable (Info a) where
  astMap tv = traverse $ astMap tv

instance ASTMappable a => ASTMappable [a] where
  astMap tv = traverse $ astMap tv

instance (ASTMappable a, ASTMappable b) => ASTMappable (a,b) where
  astMap tv (x,y) = (,) <$> astMap tv x <*> astMap tv y

instance (ASTMappable a, ASTMappable b, ASTMappable c) => ASTMappable (a,b,c) where
  astMap tv (x,y,z) = (,,) <$> astMap tv x <*> astMap tv y <*> astMap tv z
