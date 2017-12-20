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
    mapOnExp      :: ExpBase Info VName -> m (ExpBase Info VName)
  , mapOnLambda   :: LambdaBase Info VName -> m (LambdaBase Info VName)
  , mapOnName     :: VName -> m VName
  , mapOnQualName :: QualName VName -> m (QualName VName)
  }

class ASTMappable x where
  -- | Map a monadic action across the immediate children of an
  -- object.  Importantly, the 'astMap' action is not invoked for
  -- the object itself, and the mapping does not descend recursively
  -- into subexpressions.  The mapping is done left-to-right.
  astMap :: Monad m => ASTMapper m -> x -> m x

instance ASTMappable (ExpBase Info VName) where
  astMap tv (Var name t loc) =
    Var <$> mapOnQualName tv name <*> traverse (astMap tv) t <*> pure loc
  astMap _ (Literal val loc) =
    pure $ Literal val loc
  astMap tv (Parens e loc) =
    Parens <$> mapOnExp tv e <*> pure loc
  astMap tv (QualParens name e loc) =
    QualParens <$> mapOnQualName tv name <*> mapOnExp tv e <*> pure loc
  astMap tv (TupLit els loc) =
    TupLit <$> mapM (mapOnExp tv) els <*> pure loc
  astMap tv (RecordLit fields loc) =
    RecordLit <$> mapM (astMap tv) fields <*> pure loc
  astMap tv (ArrayLit els elt loc) =
    ArrayLit <$> mapM (mapOnExp tv) els <*> traverse (astMap tv) elt <*> pure loc
  astMap tv (Range start next end loc) =
    Range <$> mapOnExp tv start <*> traverse (mapOnExp tv) next <*>
    traverse (mapOnExp tv) end <*> pure loc
  astMap tv (Ascript e tdecl loc) =
    Ascript <$> mapOnExp tv e <*> astMap tv tdecl <*> pure loc
  astMap tv (Empty tdecl loc) =
    Empty <$> astMap tv tdecl <*> pure loc
  astMap tv (BinOp fname (x,xd) (y,yd) t loc) =
    BinOp <$> mapOnQualName tv fname <*>
    ((,) <$> mapOnExp tv x <*> pure xd) <*> ((,) <$> mapOnExp tv y <*> pure yd)  <*>
    traverse (astMap tv) t <*> pure loc
  astMap tv (Negate x loc) =
    Negate <$> mapOnExp tv x <*> pure loc
  astMap tv (If c texp fexp t loc) =
    If <$> mapOnExp tv c <*> mapOnExp tv texp <*> mapOnExp tv fexp <*>
    traverse (astMap tv) t <*> pure loc
  astMap tv (Apply fname args t loc) =
    Apply <$> mapOnQualName tv fname <*> mapM mapOnArg args <*>
    traverse (astMap tv) t <*> pure loc
    where mapOnArg (arg,d) = (,) <$> mapOnExp tv arg <*> pure d
  astMap tv (LetPat tparams pat e body loc) =
    LetPat <$> mapM (astMap tv) tparams <*>
    astMap tv pat <*> mapOnExp tv e <*>
    mapOnExp tv body <*> pure loc
  astMap tv (LetFun name (fparams, params, ret, t, e) body loc) =
    LetFun <$> mapOnName tv name <*>
    ((,,,,) <$> mapM (astMap tv) fparams <*> mapM (astMap tv) params <*>
     traverse (astMap tv) ret <*> traverse (astMap tv) t <*> mapOnExp tv e) <*>
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
    Project field <$> mapOnExp tv e <*> astMap tv t <*> pure loc
  astMap tv (Index arr idxexps loc) =
    pure Index <*>
         astMap tv arr <*>
         mapM (astMap tv) idxexps <*>
         pure loc
  astMap tv (Reshape shape arrexp loc) =
    pure Reshape <*> mapOnExp tv shape <*>
                     mapOnExp tv arrexp <*> pure loc
  astMap tv (Rotate d e a loc) =
    Rotate d <$> mapOnExp tv e <*> mapOnExp tv a <*> pure loc
  astMap tv (Rearrange perm e loc) =
    pure Rearrange <*> pure perm <*> mapOnExp tv e <*> pure loc
  astMap tv (Map fun e loc) =
    pure Map <*> mapOnLambda tv fun <*> mapM (mapOnExp tv) e <*> pure loc
  astMap tv (Reduce comm fun startexp arrexp loc) =
    Reduce comm <$> mapOnLambda tv fun <*>
         mapOnExp tv startexp <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Zip i e es loc) =
    Zip i <$> mapOnExp tv e <*> mapM (mapOnExp tv) es <*> pure loc
  astMap tv (Unzip e ts loc) =
    Unzip <$> mapOnExp tv e <*> mapM (traverse $ astMap tv) ts <*> pure loc
  astMap tv (Unsafe e loc) =
    Unsafe <$> mapOnExp tv e <*> pure loc
  astMap tv (Scan fun startexp arrexp loc) =
    pure Scan <*> mapOnLambda tv fun <*>
         mapOnExp tv startexp <*> mapOnExp tv arrexp <*>
         pure loc
  astMap tv (Filter fun arrexp loc) =
    pure Filter <*> mapOnLambda tv fun <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Partition funs arrexp loc) =
    pure Partition <*> mapM (mapOnLambda tv) funs <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Stream form fun arr loc) =
    pure Stream <*> mapOnStreamForm form <*> mapOnLambda tv fun <*>
         mapOnExp tv arr <*> pure loc
    where mapOnStreamForm (MapLike o) = pure $ MapLike o
          mapOnStreamForm (RedLike o comm lam) =
              RedLike o comm <$> mapOnLambda tv lam
  astMap tv (Split i splitexps arrexp loc) =
    Split i <$> mapOnExp tv splitexps <*> mapOnExp tv arrexp <*> pure loc
  astMap tv (Concat i x ys loc) =
    Concat i <$> mapOnExp tv x <*> mapM (mapOnExp tv) ys <*> pure loc
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
  astMap tv (TEApply qn ts loc) =
    TEApply <$> mapOnQualName tv qn <*> traverse (astMap tv) ts <*> pure loc

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
traverseType f g h (Array a) = Array <$> traverseArrayType f g h a
traverseType f g h (Record fs) = Record <$> traverse (traverseType f g h) fs
traverseType f g h (TypeVar t args) = TypeVar <$> f t <*> traverse (traverseTypeArg f g h) args

traverseArrayType :: Applicative f =>
                     TypeTraverser f ArrayTypeBase dim1 als1 dim2 als2
traverseArrayType _ g h (PrimArray t dims u as) =
  PrimArray t <$> traverse g dims <*> pure u <*> h as
traverseArrayType f g h (PolyArray t args dims u as) =
  PolyArray <$> f t <*> traverse (traverseTypeArg f g h) args <*>
  traverse g dims <*> pure u <*> h as
traverseArrayType f g h (RecordArray fs dims u) =
  RecordArray <$> traverse (traverseRecordArrayElemType f g h) fs <*> traverse g dims <*> pure u

traverseRecordArrayElemType :: Applicative f =>
                               TypeTraverser f RecordArrayElemTypeBase dim1 als1 dim2 als2
traverseRecordArrayElemType _ _ h (PrimArrayElem t as) = PrimArrayElem t <$> h as
traverseRecordArrayElemType f g h (ArrayArrayElem a) = ArrayArrayElem <$> traverseArrayType f g h a
traverseRecordArrayElemType f g h (PolyArrayElem t args as u) =
  PolyArrayElem <$> f t <*> traverse (traverseTypeArg f g h) args <*> h as <*> pure u
traverseRecordArrayElemType f g h (RecordArrayElem fs) =
  RecordArrayElem <$> traverse (traverseRecordArrayElemType f g h) fs

traverseTypeArg :: Applicative f =>
                   TypeTraverser f TypeArg dim1 als1 dim2 als2
traverseTypeArg _ g _ (TypeArgDim d loc) = TypeArgDim <$> g d <*> pure loc
traverseTypeArg f g h (TypeArgType t loc) = TypeArgType <$> traverseType f g h t <*> pure loc

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
  astMap tv (TypeDecl dt et) = TypeDecl <$> astMap tv dt <*> astMap tv et

instance ASTMappable (IdentBase Info VName) where
  astMap tv (Ident name t loc) =
    Ident <$> mapOnName tv name <*> traverse (astMap tv) t <*> pure loc

instance ASTMappable (PatternBase Info VName) where
  astMap tv (Id name t loc) =
    Id <$> mapOnName tv name <*> astMap tv t <*> pure loc
  astMap tv (TuplePattern pats loc) =
    TuplePattern <$> mapM (astMap tv) pats <*> pure loc
  astMap tv (RecordPattern fields loc) =
    RecordPattern <$> mapM (traverse $ astMap tv) fields <*> pure loc
  astMap tv (PatternParens pat loc) =
    PatternParens <$> astMap tv pat <*> pure loc
  astMap tv (PatternAscription pat t) =
    PatternAscription <$> astMap tv pat <*> astMap tv t
  astMap tv (Wildcard t loc) =
    Wildcard <$> astMap tv t <*> pure loc

instance ASTMappable (LambdaBase Info VName) where
  astMap tv (AnonymFun tparams params body ret t loc) =
    AnonymFun <$> mapM (astMap tv) tparams <*> mapM (astMap tv) params <*>
    astMap tv body <*> traverse (astMap tv) ret <*> astMap tv t <*> pure loc
  astMap tv (CurryFun name args t loc) =
    CurryFun <$> mapOnQualName tv name <*> astMap tv args <*> astMap tv t <*> pure loc
  astMap tv (BinOpFun name t1 t2 t3 loc) =
    BinOpFun <$> mapOnQualName tv name <*>
    astMap tv t1 <*> astMap tv t2 <*> astMap tv t3 <*> pure loc
  astMap tv (CurryBinOpLeft name arg t1 t2 loc) =
    CurryBinOpLeft <$> mapOnQualName tv name <*> mapOnExp tv arg <*>
    astMap tv t1 <*> astMap tv t2 <*> pure loc
  astMap tv (CurryBinOpRight name arg t1 t2 loc) =
    CurryBinOpRight <$> mapOnQualName tv name <*> mapOnExp tv arg <*>
    astMap tv t1 <*> astMap tv t2 <*> pure loc
  astMap tv (CurryProject name t loc) =
    CurryProject name <$> astMap tv t <*> pure loc

instance ASTMappable (FieldBase Info VName) where
  astMap tv (RecordFieldExplicit name e loc) =
    RecordFieldExplicit name <$> mapOnExp tv e <*> pure loc
  astMap tv (RecordFieldImplicit name t loc) =
    RecordFieldImplicit <$> mapOnName tv name
    <*> traverse (astMap tv) t <*> pure loc

instance ASTMappable a => ASTMappable (Info a) where
  astMap tv = traverse $ astMap tv

instance ASTMappable a => ASTMappable [a] where
  astMap tv = traverse $ astMap tv

instance (ASTMappable a, ASTMappable b) => ASTMappable (a,b) where
  astMap tv (x,y) = (,) <$> astMap tv x <*> astMap tv y
