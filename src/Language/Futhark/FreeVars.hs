-- | Facilities for computing free term variables in various syntactic
-- constructs.
module Language.Futhark.FreeVars
  ( freeInExp,
    freeInPat,
    freeInType,
    freeWithout,
    FV (..),
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Language.Futhark.Prop
import Language.Futhark.Syntax

-- | A set of names where we also track their type.
newtype FV = FV {unFV :: M.Map VName StructType}
  deriving (Show)

instance Semigroup FV where
  FV x <> FV y = FV $ M.unionWith max x y

instance Monoid FV where
  mempty = FV mempty

-- | Set subtraction.  Do not consider those variables as free.
freeWithout :: FV -> S.Set VName -> FV
freeWithout (FV x) y = FV $ M.filterWithKey keep x
  where
    keep k _ = k `S.notMember` y

ident :: IdentBase Info VName -> FV
ident v = FV $ M.singleton (identName v) (toStruct $ unInfo (identType v))

size :: VName -> FV
size v = FV $ M.singleton v $ Scalar $ Prim $ Signed Int64

-- | A 'FV' with these names, considered to be sizes.
sizes :: S.Set VName -> FV
sizes = foldMap size

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
      freeInExpField (RecordFieldImplicit vn t _) = ident $ Ident vn t mempty
  ArrayLit es t _ ->
    foldMap freeInExp es <> sizes (freeInType $ unInfo t)
  AppExp (Range e me incl _) _ ->
    freeInExp e <> foldMap freeInExp me <> foldMap freeInExp incl
  Var qn (Info t) _ -> FV $ M.singleton (qualLeaf qn) $ toStruct t
  Ascript e _ _ -> freeInExp e
  AppExp (Coerce e _ _) (Info ar) ->
    freeInExp e <> sizes (freeInType (appResType ar))
  AppExp (LetPat let_sizes pat e1 e2 _) _ ->
    freeInExp e1
      <> ( (sizes (freeInPat pat) <> freeInExp e2)
             `freeWithout` (patNames pat <> S.fromList (map sizeName let_sizes))
         )
  AppExp (LetFun vn (tparams, pats, _, _, e1) e2 _) _ ->
    ( (freeInExp e1 <> sizes (foldMap freeInPat pats))
        `freeWithout` ( foldMap patNames pats
                          <> S.fromList (map typeParamName tparams)
                      )
    )
      <> (freeInExp e2 `freeWithout` S.singleton vn)
  AppExp (If e1 e2 e3 _) _ -> freeInExp e1 <> freeInExp e2 <> freeInExp e3
  AppExp (Apply f args _) _ -> freeInExp f <> foldMap (freeInExp . snd) args
  Negate e _ -> freeInExp e
  Not e _ -> freeInExp e
  Lambda pats e0 _ (Info (_, RetType dims t)) _ ->
    (sizes (foldMap freeInPat pats) <> freeInExp e0 <> sizes (freeInType t))
      `freeWithout` (foldMap patNames pats <> S.fromList dims)
  OpSection {} -> mempty
  OpSectionLeft _ _ e _ _ _ -> freeInExp e
  OpSectionRight _ _ e _ _ _ -> freeInExp e
  ProjectSection {} -> mempty
  IndexSection idxs _ _ -> foldMap freeInDimIndex idxs
  AppExp (DoLoop sparams pat e1 form e3 _) _ ->
    let (e2fv, e2ident) = formVars form
     in freeInExp e1
          <> ( (e2fv <> freeInExp e3)
                 `freeWithout` (S.fromList sparams <> patNames pat <> e2ident)
             )
    where
      formVars (For v e2) = (freeInExp e2, S.singleton $ identName v)
      formVars (ForIn p e2) = (freeInExp e2, patNames p)
      formVars (While e2) = (freeInExp e2, mempty)
  AppExp (BinOp (qn, _) (Info qn_t) (e1, _) (e2, _) _) _ ->
    FV (M.singleton (qualLeaf qn) $ toStruct qn_t)
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
        (sizes (freeInPat p) <> freeInExp eCase)
          `freeWithout` patNames p

freeInDimIndex :: DimIndexBase Info VName -> FV
freeInDimIndex (DimFix e) = freeInExp e
freeInDimIndex (DimSlice me1 me2 me3) =
  foldMap (foldMap freeInExp) [me1, me2, me3]

-- | Free variables in pattern (including types of the bound identifiers).
freeInPat :: PatBase Info VName -> S.Set VName
freeInPat (TuplePat ps _) = foldMap freeInPat ps
freeInPat (RecordPat fs _) = foldMap (freeInPat . snd) fs
freeInPat (PatParens p _) = freeInPat p
freeInPat (Id _ (Info tp) _) = freeInType tp
freeInPat (Wildcard (Info tp) _) = freeInType tp
freeInPat (PatAscription p _ _) = freeInPat p
freeInPat (PatLit _ (Info tp) _) = freeInType tp
freeInPat (PatConstr _ _ ps _) = foldMap freeInPat ps
freeInPat (PatAttr _ p _) = freeInPat p

-- | Free variables in the type (meaning those that are used in size expression).
freeInType :: TypeBase Size as -> S.Set VName
freeInType t =
  case t of
    Array _ _ s a ->
      freeInType (Scalar a) <> foldMap onSize (shapeDims s)
    Scalar (Record fs) ->
      foldMap freeInType fs
    Scalar Prim {} ->
      mempty
    Scalar (Sum cs) ->
      foldMap (foldMap freeInType) cs
    Scalar (Arrow _ v _ t1 (RetType dims t2)) ->
      S.filter (notV v) $ S.filter (`notElem` dims) $ freeInType t1 <> freeInType t2
    Scalar (TypeVar _ _ _ targs) ->
      foldMap typeArgDims targs
  where
    typeArgDims (TypeArgDim d _) = onSize d
    typeArgDims (TypeArgType at _) = freeInType at

    notV Unnamed = const True
    notV (Named v) = (/= v)

    onSize (NamedSize qn) = S.singleton $ qualLeaf qn
    onSize _ = mempty
