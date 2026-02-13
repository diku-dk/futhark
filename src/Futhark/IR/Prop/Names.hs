{-# LANGUAGE UndecidableInstances #-}

-- | Facilities for determining which names are used in some syntactic
-- construct.  The most important interface is the 'FreeIn' class and
-- its instances, but for reasons related to the Haskell type system,
-- some constructs have specialised functions.
module Futhark.IR.Prop.Names
  ( -- * Free names
    Names,
    namesIntMap,
    namesIntSet,
    nameIn,
    notNameIn,
    oneName,
    namesFromList,
    namesToList,
    namesIntersection,
    namesIntersect,
    namesSubtract,
    mapNames,
    allNames,

    -- * Class
    FreeIn (..),
    freeIn,

    -- * Specialised Functions
    freeInStmsAndRes,

    -- * Bound Names
    boundInBody,
    boundByStm,
    boundByStms,
    boundByLambda,

    -- * Efficient computation
    FreeDec (..),
    FV,
    fvBind,
    fvName,
    fvNames,
  )
where

import Control.Category
import Control.Monad.State.Strict
import Data.Foldable
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.IR.Prop.Pat
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.Util.Pretty
import Prelude hiding (id, (.))

-- | A set of names.  Note that the 'Ord' instance is a dummy that
-- treats everything as 'EQ' if '==', and otherwise 'LT'.
newtype Names = Names (IM.IntMap VName)
  deriving (Eq, Show)

-- | Retrieve the data structure underlying the names representation.
namesIntMap :: Names -> IM.IntMap VName
namesIntMap (Names m) = m

-- | Retrieve the set of tags in the names set.
namesIntSet :: Names -> IS.IntSet
namesIntSet (Names m) = IM.keysSet m

instance Ord Names where
  x `compare` y = if x == y then EQ else LT

instance Semigroup Names where
  vs1 <> vs2 = Names $ namesIntMap vs1 <> namesIntMap vs2

instance Monoid Names where
  mempty = Names mempty

instance Pretty Names where
  pretty = pretty . namesToList

-- | Does the set of names contain this name?
nameIn :: VName -> Names -> Bool
nameIn v (Names vs) = baseTag v `IM.member` vs

-- | Does the set of names not contain this name?
notNameIn :: VName -> Names -> Bool
notNameIn v (Names vs) = baseTag v `IM.notMember` vs

-- | Construct a name set from a list.  Slow.
namesFromList :: [VName] -> Names
namesFromList vs = Names $ IM.fromList $ zip (map baseTag vs) vs

-- | Turn a name set into a list of names.  Slow.
namesToList :: Names -> [VName]
namesToList = IM.elems . namesIntMap

-- | Construct a name set from a single name.
oneName :: VName -> Names
oneName v = Names $ IM.singleton (baseTag v) v

-- | The intersection of two name sets.
namesIntersection :: Names -> Names -> Names
namesIntersection (Names vs1) (Names vs2) = Names $ IM.intersection vs1 vs2

-- | Do the two name sets intersect?
namesIntersect :: Names -> Names -> Bool
namesIntersect vs1 vs2 = not $ IM.disjoint (namesIntMap vs1) (namesIntMap vs2)

-- | Subtract the latter name set from the former.
namesSubtract :: Names -> Names -> Names
namesSubtract (Names vs1) (Names vs2) = Names $ IM.difference vs1 vs2

-- | Map over the names in a set.
mapNames :: (VName -> VName) -> Names -> Names
mapNames f vs = namesFromList $ map f $ namesToList vs

-- | Check if predicate holds for all names in set.
allNames :: (VName -> Bool) -> Names -> Bool
allNames f (Names vs) = all f vs

-- | A computation to build a free variable set.
newtype FV = FV {unFV :: Names}

-- Right now the variable set is just stored explicitly, without the
-- fancy functional representation that GHC uses.  Turns out it's
-- faster this way.

instance Monoid FV where
  mempty = FV mempty

instance Semigroup FV where
  FV fv1 <> FV fv2 = FV $ fv1 <> fv2

-- | Consider a variable to be bound in the given 'FV' computation.
fvBind :: Names -> FV -> FV
fvBind vs (FV fv) = FV $ fv `namesSubtract` vs

-- | Take note of a variable reference.
fvName :: VName -> FV
fvName v = FV $ oneName v

-- | Take note of a set of variable references.
fvNames :: Names -> FV
fvNames = FV

freeWalker ::
  ( FreeDec (ExpDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (LetDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeIn (Op rep)
  ) =>
  Walker rep (State FV)
freeWalker =
  Walker
    { walkOnSubExp = modify . (<>) . freeIn',
      walkOnBody = \scope body -> do
        modify $ (<>) $ freeIn' body
        modify $ fvBind (namesFromList (M.keys scope)),
      walkOnVName = modify . (<>) . fvName,
      walkOnOp = modify . (<>) . freeIn',
      walkOnFParam = modify . (<>) . freeIn',
      walkOnLParam = modify . (<>) . freeIn',
      walkOnRetType = modify . (<>) . freeIn',
      walkOnBranchType = modify . (<>) . freeIn'
    }

-- | Return the set of variable names that are free in the given
-- statements and result.  Filters away the names that are bound by
-- the statements.
freeInStmsAndRes ::
  ( FreeIn (Op rep),
    FreeIn (LetDec rep),
    FreeIn (LParamInfo rep),
    FreeIn (FParamInfo rep),
    FreeDec (BodyDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeDec (ExpDec rep),
    FreeIn res
  ) =>
  Stms rep ->
  res ->
  FV
freeInStmsAndRes stms res =
  fvBind (boundByStms stms) $ foldMap freeIn' stms <> freeIn' res

-- | A class indicating that we can obtain free variable information
-- from values of this type.
class FreeIn a where
  freeIn' :: a -> FV
  freeIn' = fvNames . freeIn

-- | The free variables of some syntactic construct.
freeIn :: (FreeIn a) => a -> Names
freeIn = unFV . freeIn'

instance FreeIn FV where
  freeIn' = id

instance FreeIn () where
  freeIn' () = mempty

instance FreeIn Int where
  freeIn' = const mempty

instance (FreeIn a, FreeIn b) => FreeIn (a, b) where
  freeIn' (a, b) = freeIn' a <> freeIn' b

instance (FreeIn a, FreeIn b, FreeIn c) => FreeIn (a, b, c) where
  freeIn' (a, b, c) = freeIn' a <> freeIn' b <> freeIn' c

instance (FreeIn a, FreeIn b, FreeIn c, FreeIn d) => FreeIn (a, b, c, d) where
  freeIn' (a, b, c, d) = freeIn' a <> freeIn' b <> freeIn' c <> freeIn' d

instance (FreeIn a, FreeIn b) => FreeIn (Either a b) where
  freeIn' = either freeIn' freeIn'

instance (FreeIn a) => FreeIn [a] where
  freeIn' = foldMap freeIn'

instance (FreeIn a) => FreeIn (S.Set a) where
  freeIn' = foldMap freeIn'

instance FreeIn (NoOp rep) where
  freeIn' NoOp = mempty

instance
  ( FreeDec (ExpDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (LetDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeIn (Op rep)
  ) =>
  FreeIn (FunDef rep)
  where
  freeIn' (FunDef _ _ _ rettype params body) =
    fvBind (namesFromList $ map paramName params) $
      foldMap (freeIn' . fst) rettype <> freeIn' params <> freeIn' body

instance
  ( FreeDec (ExpDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (LetDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeIn (Op rep)
  ) =>
  FreeIn (Lambda rep)
  where
  freeIn' (Lambda params body rettype) =
    fvBind (namesFromList $ map paramName params) $
      freeIn' rettype <> freeIn' params <> freeIn' body

instance
  ( FreeDec (ExpDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (LetDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeIn (Op rep),
    FreeIn res
  ) =>
  FreeIn (GBody rep res)
  where
  freeIn' (Body dec stms res) =
    precomputed dec $ freeIn' dec <> freeInStmsAndRes stms res

instance
  ( FreeDec (ExpDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (LetDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeIn (Op rep)
  ) =>
  FreeIn (Exp rep)
  where
  freeIn' (Loop merge form loopbody) =
    let (params, args) = unzip merge
        bound_here =
          case form of
            WhileLoop {} -> namesFromList $ map paramName params
            ForLoop i _ _ -> namesFromList $ i : map paramName params
     in fvBind bound_here $
          freeIn' args <> freeIn' form <> freeIn' params <> freeIn' loopbody
  freeIn' (WithAcc inputs lam) =
    freeIn' inputs <> freeIn' lam
  freeIn' e = execState (walkExpM freeWalker e) mempty

instance
  ( FreeDec (ExpDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (LetDec rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    FreeIn (Op rep)
  ) =>
  FreeIn (Stm rep)
  where
  freeIn' (Let pat (StmAux cs attrs _ dec) e) =
    freeIn' cs
      <> freeIn' attrs
      <> precomputed dec (freeIn' dec <> freeIn' e <> freeIn' pat)

instance (FreeIn (Stm rep)) => FreeIn (Stms rep) where
  freeIn' = foldMap freeIn'

instance (FreeIn body) => FreeIn (Case body) where
  freeIn' = freeIn' . caseBody

instance FreeIn Names where
  freeIn' = fvNames

instance FreeIn Bool where
  freeIn' _ = mempty

instance (FreeIn a) => FreeIn (Maybe a) where
  freeIn' = maybe mempty freeIn'

instance FreeIn VName where
  freeIn' = fvName

instance FreeIn Ident where
  freeIn' = freeIn' . identType

instance FreeIn SubExp where
  freeIn' (Var v) = freeIn' v
  freeIn' Constant {} = mempty

instance FreeIn Space where
  freeIn' (ScalarSpace d _) = freeIn' d
  freeIn' DefaultSpace = mempty
  freeIn' (Space _) = mempty

instance (FreeIn d) => FreeIn (ShapeBase d) where
  freeIn' = freeIn' . shapeDims

instance (FreeIn d) => FreeIn (NewShape d) where
  freeIn' = foldMap freeIn'

instance (FreeIn d) => FreeIn (Ext d) where
  freeIn' (Free x) = freeIn' x
  freeIn' (Ext _) = mempty

instance FreeIn PrimType where
  freeIn' _ = mempty

instance (FreeIn shape) => FreeIn (TypeBase shape u) where
  freeIn' (Array t shape _) = freeIn' t <> freeIn' shape
  freeIn' (Mem s) = freeIn' s
  freeIn' Prim {} = mempty
  freeIn' (Acc acc ispace ts _) = freeIn' (acc, ispace, ts)

instance (FreeIn dec) => FreeIn (Param dec) where
  freeIn' (Param attrs _ dec) = freeIn' attrs <> freeIn' dec

instance (FreeIn dec) => FreeIn (PatElem dec) where
  freeIn' (PatElem _ dec) = freeIn' dec

instance FreeIn LoopForm where
  freeIn' (ForLoop _ _ bound) = freeIn' bound
  freeIn' (WhileLoop cond) = freeIn' cond

instance (FreeIn d) => FreeIn (DimIndex d) where
  freeIn' = Data.Foldable.foldMap freeIn'

instance (FreeIn d) => FreeIn (Slice d) where
  freeIn' = Data.Foldable.foldMap freeIn'

instance (FreeIn d) => FreeIn (FlatDimIndex d) where
  freeIn' = Data.Foldable.foldMap freeIn'

instance (FreeIn d) => FreeIn (FlatSlice d) where
  freeIn' = Data.Foldable.foldMap freeIn'

instance FreeIn SubExpRes where
  freeIn' (SubExpRes cs se) = freeIn' cs <> freeIn' se

instance (FreeIn dec) => FreeIn (Pat dec) where
  freeIn' (Pat xs) =
    fvBind bound_here $ freeIn' xs
    where
      bound_here = namesFromList $ map patElemName xs

instance FreeIn Certs where
  freeIn' (Certs cs) = freeIn' cs

instance FreeIn Attrs where
  freeIn' (Attrs _) = mempty

instance (FreeIn dec) => FreeIn (StmAux dec) where
  freeIn' (StmAux cs attrs _ dec) = freeIn' cs <> freeIn' attrs <> freeIn' dec

instance (FreeIn a) => FreeIn (MatchDec a) where
  freeIn' (MatchDec r _) = freeIn' r

-- | Either return precomputed free names stored in the attribute, or
-- the freshly computed names.  Relies on lazy evaluation to avoid the
-- work.
class (FreeIn dec) => FreeDec dec where
  precomputed :: dec -> FV -> FV
  precomputed _ = id

instance FreeDec ()

instance (FreeDec a, FreeIn b) => FreeDec (a, b) where
  precomputed (a, _) = precomputed a

instance (FreeDec a) => FreeDec [a] where
  precomputed [] = id
  precomputed (a : _) = precomputed a

instance (FreeDec a) => FreeDec (Maybe a) where
  precomputed Nothing = id
  precomputed (Just a) = precomputed a

instance FreeDec Names where
  precomputed _ fv = fv

-- | The names bound by the bindings immediately in a t'Body'.
boundInBody :: Body rep -> Names
boundInBody = boundByStms . bodyStms

-- | The names bound by a binding.
boundByStm :: Stm rep -> Names
boundByStm = namesFromList . patNames . stmPat

-- | The names bound by the bindings.
boundByStms :: Stms rep -> Names
boundByStms = foldMap boundByStm

-- | The names of the lambda parameters plus the index parameter.
boundByLambda :: Lambda rep -> [VName]
boundByLambda lam = map paramName (lambdaParams lam)
