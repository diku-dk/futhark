-- | Facilities for computing free term variables in various syntactic
-- constructs.
module Language.Futhark.FreeVars
  ( freeInExp,
    freeInPat,
    freeInType,
    freeWithout,
    FV,
    fvVars,
  )
where

import Data.Set qualified as S
import Language.Futhark.Prop
import Language.Futhark.Syntax

-- | A set of names.
newtype FV = FV {unFV :: S.Set VName}
  deriving (Show)

-- | The set of names in an 'FV'.
fvVars :: FV -> S.Set VName
fvVars = unFV

instance Semigroup FV where
  FV x <> FV y = FV $ x <> y

instance Monoid FV where
  mempty = FV mempty

-- | Set subtraction.  Do not consider those variables as free.
freeWithout :: FV -> S.Set VName -> FV
freeWithout (FV x) y = FV $ x `S.difference` y

-- | As 'freeWithout', but for lists.
freeWithoutL :: FV -> [VName] -> FV
freeWithoutL fv y = fv `freeWithout` S.fromList y

ident :: Ident t -> FV
ident = FV . S.singleton . identName

-- | Compute the set of free variables of an expression.
freeInExp :: ExpBase Info VName -> FV
freeInExp expr = case expr of
  Literal {} -> mempty
  IntLit {} -> mempty
  FloatLit {} -> mempty
  StringLit {} -> mempty
  Hole {} -> mempty
  Parens e _ -> freeInExp e
  QualParens _ e _ -> freeInExp e
  TupLit es _ -> foldMap freeInExp es
  RecordLit fs _ -> foldMap freeInExpField fs
    where
      freeInExpField (RecordFieldExplicit _ e _) = freeInExp e
      freeInExpField (RecordFieldImplicit (L _ vn) t _) = ident $ Ident vn t mempty
  ArrayVal {} -> mempty
  ArrayLit es t _ ->
    foldMap freeInExp es <> freeInType (unInfo t)
  AppExp (Range e me incl _) _ ->
    freeInExp e <> foldMap freeInExp me <> foldMap freeInExp incl
  Var qn _ _ -> FV $ S.singleton $ qualLeaf qn
  Ascript e _ _ -> freeInExp e
  Coerce e _ (Info t) _ ->
    freeInExp e <> freeInType t
  AppExp (LetPat let_sizes pat e1 e2 _) _ ->
    freeInExp e1
      <> ( (freeInPat pat <> freeInExp e2)
             `freeWithoutL` (patNames pat <> map sizeName let_sizes)
         )
  AppExp (LetFun vn (tparams, pats, _, _, e1) e2 _) _ ->
    ( (freeInExp e1 <> foldMap freeInPat pats)
        `freeWithoutL` (foldMap patNames pats <> map typeParamName tparams)
    )
      <> (freeInExp e2 `freeWithout` S.singleton vn)
  AppExp (If e1 e2 e3 _) _ -> freeInExp e1 <> freeInExp e2 <> freeInExp e3
  AppExp (Apply f args _) _ -> freeInExp f <> foldMap (freeInExp . snd) args
  Negate e _ -> freeInExp e
  Not e _ -> freeInExp e
  Lambda pats e0 _ (Info (RetType dims t)) _ ->
    (foldMap freeInPat pats <> freeInExp e0 <> freeInType t)
      `freeWithoutL` (foldMap patNames pats <> dims)
  OpSection {} -> mempty
  OpSectionLeft _ _ e _ _ _ -> freeInExp e
  OpSectionRight _ _ e _ _ _ -> freeInExp e
  ProjectSection {} -> mempty
  IndexSection idxs _ _ -> foldMap freeInDimIndex idxs
  AppExp (Loop sparams pat e1 form e3 _) _ ->
    let (e2fv, e2ident) = formVars form
     in freeInExp (loopInitExp e1)
          <> ( (e2fv <> freeInExp e3)
                 `freeWithoutL` (sparams <> patNames pat <> e2ident)
             )
    where
      formVars (For v e2) = (freeInExp e2, [identName v])
      formVars (ForIn p e2) = (freeInExp e2, patNames p)
      formVars (While e2) = (freeInExp e2, mempty)
  AppExp (BinOp (qn, _) _ (e1, _) (e2, _) _) _ ->
    FV (S.singleton (qualLeaf qn))
      <> freeInExp e1
      <> freeInExp e2
  Project _ e _ _ -> freeInExp e
  AppExp (LetWith id1 id2 idxs e1 e2 _) _ ->
    ident id2
      <> foldMap freeInDimIndex idxs
      <> freeInExp e1
      <> (freeInExp e2 `freeWithout` S.singleton (identName id1))
  AppExp (Index e idxs _) _ -> freeInExp e <> foldMap freeInDimIndex idxs
  Update e1 idxs e2 _ -> freeInExp e1 <> foldMap freeInDimIndex idxs <> freeInExp e2
  RecordUpdate e1 _ e2 _ _ -> freeInExp e1 <> freeInExp e2
  Assert e1 e2 _ _ -> freeInExp e1 <> freeInExp e2
  Constr _ es _ _ -> foldMap freeInExp es
  Attr _ e _ -> freeInExp e
  AppExp (Match e cs _) _ -> freeInExp e <> foldMap caseFV cs
    where
      caseFV (CasePat p eCase _) =
        (freeInPat p <> freeInExp eCase)
          `freeWithoutL` patNames p

freeInDimIndex :: DimIndexBase Info VName -> FV
freeInDimIndex (DimFix e) = freeInExp e
freeInDimIndex (DimSlice me1 me2 me3) =
  foldMap (foldMap freeInExp) [me1, me2, me3]

-- | Free variables in pattern (including types of the bound identifiers).
freeInPat :: Pat (TypeBase Size u) -> FV
freeInPat = foldMap freeInType

-- | Free variables in the type (meaning those that are used in size expression).
freeInType :: TypeBase Size u -> FV
freeInType t =
  case t of
    Array _ s a ->
      freeInType (Scalar a) <> foldMap freeInExp (shapeDims s)
    Scalar (Record fs) ->
      foldMap freeInType fs
    Scalar Prim {} ->
      mempty
    Scalar (Sum cs) ->
      foldMap (foldMap freeInType) cs
    Scalar (Arrow _ v _ t1 (RetType dims t2)) ->
      FV . S.filter (\k -> notV v k && notElem k dims) $
        unFV (freeInType t1 <> freeInType t2)
    Scalar (TypeVar _ _ targs) ->
      foldMap typeArgDims targs
  where
    typeArgDims (TypeArgDim d) = freeInExp d
    typeArgDims (TypeArgType at) = freeInType at

    notV Unnamed = const True
    notV (Named v) = (/= v)
