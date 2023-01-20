{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Building blocks for defining representations where every array
-- is given information about which memory block is it based in, and
-- how array elements map to memory block offsets.
--
-- There are two primary concepts you will need to understand:
--
--  1. Memory blocks, which are Futhark values of type v'Mem'
--     (parametrized with their size).  These correspond to arbitrary
--     blocks of memory, and are created using the 'Alloc' operation.
--
--  2. Index functions, which describe a mapping from the index space
--     of an array (eg. a two-dimensional space for an array of type
--     @[[int]]@) to a one-dimensional offset into a memory block.
--     Thus, index functions describe how arbitrary-dimensional arrays
--     are mapped to the single-dimensional world of memory.
--
-- At a conceptual level, imagine that we have a two-dimensional array
-- @a@ of 32-bit integers, consisting of @n@ rows of @m@ elements
-- each.  This array could be represented in classic row-major format
-- with an index function like the following:
--
-- @
--   f(i,j) = i * m + j
-- @
--
-- When we want to know the location of element @a[2,3]@, we simply
-- call the index function as @f(2,3)@ and obtain @2*m+3@.  We could
-- also have chosen another index function, one that represents the
-- array in column-major (or "transposed") format:
--
-- @
--   f(i,j) = j * n + i
-- @
--
-- Index functions are not Futhark-level functions, but a special
-- construct that the final code generator will eventually use to
-- generate concrete access code.  By modifying the index functions we
-- can change how an array is represented in memory, which can permit
-- memory access pattern optimisations.
--
-- Every time we bind an array, whether in a @let@-binding, @loop@
-- merge parameter, or @lambda@ parameter, we have an annotation
-- specifying a memory block and an index function.  In some cases,
-- such as @let@-bindings for many expressions, we are free to specify
-- an arbitrary index function and memory block - for example, we get
-- to decide where 'Copy' stores its result - but in other cases the
-- type rules of the expression chooses for us.  For example, 'Index'
-- always produces an array in the same memory block as its input, and
-- with the same index function, except with some indices fixed.
module Futhark.IR.Mem
  ( LetDecMem,
    FParamMem,
    LParamMem,
    RetTypeMem,
    BranchTypeMem,
    MemOp (..),
    traverseMemOpStms,
    MemInfo (..),
    MemBound,
    MemBind (..),
    MemReturn (..),
    IxFun,
    ExtIxFun,
    isStaticIxFun,
    ExpReturns,
    BodyReturns,
    FunReturns,
    noUniquenessReturns,
    bodyReturnsToExpReturns,
    Mem,
    HasLetDecMem (..),
    OpReturns (..),
    varReturns,
    expReturns,
    extReturns,
    lookupMemInfo,
    subExpMemInfo,
    lookupArraySummary,
    lookupMemSpace,
    existentialiseIxFun,

    -- * Type checking parts
    matchBranchReturnType,
    matchPatToExp,
    matchFunctionReturnType,
    matchLoopResultMem,
    bodyReturnsFromPat,
    checkMemInfo,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
    module Futhark.Analysis.PrimExp.Convert,
  )
where

import Control.Category
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind qualified
import Data.List (elemIndex, find)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.Analysis.Metrics
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.PrimExp.Simplify
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.IR.Aliases
  ( Aliases,
    CanBeAliased (..),
    removeExpAliases,
    removePatAliases,
    removeScopeAliases,
  )
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.Prop.Aliases
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util
import Futhark.Util.Pretty (docText, indent, ppTupleLines', pretty, (<+>), (</>))
import Futhark.Util.Pretty qualified as PP
import Prelude hiding (id, (.))

type LetDecMem = MemInfo SubExp NoUniqueness MemBind

type FParamMem = MemInfo SubExp Uniqueness MemBind

type LParamMem = MemInfo SubExp NoUniqueness MemBind

type RetTypeMem = FunReturns

type BranchTypeMem = BodyReturns

-- | The class of pattern element decorators that contain memory
-- information.
class HasLetDecMem t where
  letDecMem :: t -> LetDecMem

instance HasLetDecMem LetDecMem where
  letDecMem = id

instance HasLetDecMem b => HasLetDecMem (a, b) where
  letDecMem = letDecMem . snd

type Mem rep inner =
  ( FParamInfo rep ~ FParamMem,
    LParamInfo rep ~ LParamMem,
    HasLetDecMem (LetDec rep),
    RetType rep ~ RetTypeMem,
    BranchType rep ~ BranchTypeMem,
    ASTRep rep,
    OpReturns (inner rep),
    RephraseOp inner,
    Op rep ~ MemOp inner rep
  )

instance IsRetType FunReturns where
  primRetType = MemPrim
  applyRetType = applyFunReturns

instance IsBodyType BodyReturns where
  primBodyType = MemPrim

data MemOp (inner :: Data.Kind.Type -> Data.Kind.Type) (rep :: Data.Kind.Type)
  = -- | Allocate a memory block.
    Alloc SubExp Space
  | Inner (inner rep)
  deriving (Eq, Ord, Show)

-- | A helper for defining 'TraverseOpStms'.
traverseMemOpStms ::
  Monad m =>
  OpStmsTraverser m (inner rep) rep ->
  OpStmsTraverser m (MemOp inner rep) rep
traverseMemOpStms _ _ op@Alloc {} = pure op
traverseMemOpStms onInner f (Inner inner) = Inner <$> onInner f inner

instance RephraseOp inner => RephraseOp (MemOp inner) where
  rephraseInOp _ (Alloc e space) = pure (Alloc e space)
  rephraseInOp r (Inner x) = Inner <$> rephraseInOp r x

instance FreeIn (inner rep) => FreeIn (MemOp inner rep) where
  freeIn' (Alloc size _) = freeIn' size
  freeIn' (Inner k) = freeIn' k

instance TypedOp (inner rep) => TypedOp (MemOp inner rep) where
  opType (Alloc _ space) = pure [Mem space]
  opType (Inner k) = opType k

instance AliasedOp (inner rep) => AliasedOp (MemOp inner rep) where
  opAliases Alloc {} = [mempty]
  opAliases (Inner k) = opAliases k

  consumedInOp Alloc {} = mempty
  consumedInOp (Inner k) = consumedInOp k

instance CanBeAliased inner => CanBeAliased (MemOp inner) where
  addOpAliases _ (Alloc se space) = Alloc se space
  addOpAliases aliases (Inner k) = Inner $ addOpAliases aliases k

instance Rename (inner rep) => Rename (MemOp inner rep) where
  rename (Alloc size space) = Alloc <$> rename size <*> pure space
  rename (Inner k) = Inner <$> rename k

instance Substitute (inner rep) => Substitute (MemOp inner rep) where
  substituteNames subst (Alloc size space) = Alloc (substituteNames subst size) space
  substituteNames subst (Inner k) = Inner $ substituteNames subst k

instance PP.Pretty (inner rep) => PP.Pretty (MemOp inner rep) where
  pretty (Alloc e DefaultSpace) = "alloc" <> PP.apply [PP.pretty e]
  pretty (Alloc e s) = "alloc" <> PP.apply [PP.pretty e, PP.pretty s]
  pretty (Inner k) = PP.pretty k

instance OpMetrics (inner rep) => OpMetrics (MemOp inner rep) where
  opMetrics Alloc {} = seen "Alloc"
  opMetrics (Inner k) = opMetrics k

instance IsOp (inner rep) => IsOp (MemOp inner rep) where
  safeOp (Alloc (Constant (IntValue (Int64Value k))) _) = k >= 0
  safeOp Alloc {} = False
  safeOp (Inner k) = safeOp k
  cheapOp (Inner k) = cheapOp k
  cheapOp Alloc {} = True

instance CanBeWise inner => CanBeWise (MemOp inner) where
  addOpWisdom (Alloc size space) = Alloc size space
  addOpWisdom (Inner k) = Inner $ addOpWisdom k

instance ST.IndexOp (inner rep) => ST.IndexOp (MemOp inner rep) where
  indexOp vtable k (Inner op) is = ST.indexOp vtable k op is
  indexOp _ _ _ _ = Nothing

-- | The index function representation used for memory annotations.
type IxFun = IxFun.IxFun (TPrimExp Int64 VName)

-- | An index function that may contain existential variables.
type ExtIxFun = IxFun.IxFun (TPrimExp Int64 (Ext VName))

-- | A summary of the memory information for every let-bound
-- identifier, function parameter, and return value.  Parameterisered
-- over uniqueness, dimension, and auxiliary array information.
data MemInfo d u ret
  = -- | A primitive value.
    MemPrim PrimType
  | -- | A memory block.
    MemMem Space
  | -- | The array is stored in the named memory block, and with the
    -- given index function.  The index function maps indices in the
    -- array to /element/ offset, /not/ byte offsets!  To translate to
    -- byte offsets, multiply the offset with the size of the array
    -- element type.
    MemArray PrimType (ShapeBase d) u ret
  | -- | An accumulator, which is not stored anywhere.
    MemAcc VName Shape [Type] u
  deriving (Eq, Show, Ord) --- XXX Ord?

type MemBound u = MemInfo SubExp u MemBind

instance FixExt ret => DeclExtTyped (MemInfo ExtSize Uniqueness ret) where
  declExtTypeOf (MemPrim pt) = Prim pt
  declExtTypeOf (MemMem space) = Mem space
  declExtTypeOf (MemArray pt shape u _) = Array pt shape u
  declExtTypeOf (MemAcc acc ispace ts u) = Acc acc ispace ts u

instance FixExt ret => ExtTyped (MemInfo ExtSize NoUniqueness ret) where
  extTypeOf (MemPrim pt) = Prim pt
  extTypeOf (MemMem space) = Mem space
  extTypeOf (MemArray pt shape u _) = Array pt shape u
  extTypeOf (MemAcc acc ispace ts u) = Acc acc ispace ts u

instance FixExt ret => FixExt (MemInfo ExtSize u ret) where
  fixExt _ _ (MemPrim pt) = MemPrim pt
  fixExt _ _ (MemMem space) = MemMem space
  fixExt i se (MemArray pt shape u ret) =
    MemArray pt (fixExt i se shape) u (fixExt i se ret)
  fixExt _ _ (MemAcc acc ispace ts u) = MemAcc acc ispace ts u

instance Typed (MemInfo SubExp Uniqueness ret) where
  typeOf = fromDecl . declTypeOf

instance Typed (MemInfo SubExp NoUniqueness ret) where
  typeOf (MemPrim pt) = Prim pt
  typeOf (MemMem space) = Mem space
  typeOf (MemArray bt shape u _) = Array bt shape u
  typeOf (MemAcc acc ispace ts u) = Acc acc ispace ts u

instance DeclTyped (MemInfo SubExp Uniqueness ret) where
  declTypeOf (MemPrim bt) = Prim bt
  declTypeOf (MemMem space) = Mem space
  declTypeOf (MemArray bt shape u _) = Array bt shape u
  declTypeOf (MemAcc acc ispace ts u) = Acc acc ispace ts u

instance (FreeIn d, FreeIn ret) => FreeIn (MemInfo d u ret) where
  freeIn' (MemArray _ shape _ ret) = freeIn' shape <> freeIn' ret
  freeIn' (MemMem s) = freeIn' s
  freeIn' MemPrim {} = mempty
  freeIn' (MemAcc acc ispace ts _) = freeIn' (acc, ispace, ts)

instance (Substitute d, Substitute ret) => Substitute (MemInfo d u ret) where
  substituteNames subst (MemArray bt shape u ret) =
    MemArray
      bt
      (substituteNames subst shape)
      u
      (substituteNames subst ret)
  substituteNames substs (MemAcc acc ispace ts u) =
    MemAcc
      (substituteNames substs acc)
      (substituteNames substs ispace)
      (substituteNames substs ts)
      u
  substituteNames _ (MemMem space) =
    MemMem space
  substituteNames _ (MemPrim bt) =
    MemPrim bt

instance (Substitute d, Substitute ret) => Rename (MemInfo d u ret) where
  rename = substituteRename

simplifyIxFun ::
  Engine.SimplifiableRep rep =>
  IxFun ->
  Engine.SimpleM rep IxFun
simplifyIxFun = traverse $ fmap isInt64 . simplifyPrimExp . untyped

simplifyExtIxFun ::
  Engine.SimplifiableRep rep =>
  ExtIxFun ->
  Engine.SimpleM rep ExtIxFun
simplifyExtIxFun = traverse $ fmap isInt64 . simplifyExtPrimExp . untyped

isStaticIxFun :: ExtIxFun -> Maybe IxFun
isStaticIxFun = traverse $ traverse inst
  where
    inst Ext {} = Nothing
    inst (Free x) = Just x

instance
  (Engine.Simplifiable d, Engine.Simplifiable ret) =>
  Engine.Simplifiable (MemInfo d u ret)
  where
  simplify (MemPrim bt) =
    pure $ MemPrim bt
  simplify (MemMem space) =
    pure $ MemMem space
  simplify (MemArray bt shape u ret) =
    MemArray bt <$> Engine.simplify shape <*> pure u <*> Engine.simplify ret
  simplify (MemAcc acc ispace ts u) =
    MemAcc <$> Engine.simplify acc <*> Engine.simplify ispace <*> Engine.simplify ts <*> pure u

instance
  ( PP.Pretty (ShapeBase d),
    PP.Pretty (TypeBase (ShapeBase d) u),
    PP.Pretty d,
    PP.Pretty u,
    PP.Pretty ret
  ) =>
  PP.Pretty (MemInfo d u ret)
  where
  pretty (MemPrim bt) = PP.pretty bt
  pretty (MemMem DefaultSpace) = "mem"
  pretty (MemMem s) = "mem" <> PP.pretty s
  pretty (MemArray bt shape u ret) =
    PP.pretty (Array bt shape u) <+> "@" <+> PP.pretty ret
  pretty (MemAcc acc ispace ts u) =
    PP.pretty u <> PP.pretty (Acc acc ispace ts NoUniqueness :: Type)

-- | Memory information for an array bound somewhere in the program.
data MemBind
  = -- | Located in this memory block with this index
    -- function.
    ArrayIn VName IxFun
  deriving (Show)

instance Eq MemBind where
  _ == _ = True

instance Ord MemBind where
  _ `compare` _ = EQ

instance Rename MemBind where
  rename = substituteRename

instance Substitute MemBind where
  substituteNames substs (ArrayIn ident ixfun) =
    ArrayIn (substituteNames substs ident) (substituteNames substs ixfun)

instance PP.Pretty MemBind where
  pretty (ArrayIn mem ixfun) =
    PP.pretty mem <+> "->" PP.</> PP.pretty ixfun

instance FreeIn MemBind where
  freeIn' (ArrayIn mem ixfun) = freeIn' mem <> freeIn' ixfun

-- | A description of the memory properties of an array being returned
-- by an operation.
data MemReturn
  = -- | The array is located in a memory block that is
    -- already in scope.
    ReturnsInBlock VName ExtIxFun
  | -- | The operation returns a new (existential) memory
    -- block.
    ReturnsNewBlock Space Int ExtIxFun
  deriving (Show)

instance Eq MemReturn where
  _ == _ = True

instance Ord MemReturn where
  _ `compare` _ = EQ

instance Rename MemReturn where
  rename = substituteRename

instance Substitute MemReturn where
  substituteNames substs (ReturnsInBlock ident ixfun) =
    ReturnsInBlock (substituteNames substs ident) (substituteNames substs ixfun)
  substituteNames substs (ReturnsNewBlock space i ixfun) =
    ReturnsNewBlock space i (substituteNames substs ixfun)

instance FixExt MemReturn where
  fixExt i (Var v) (ReturnsNewBlock _ j ixfun)
    | j == i =
        ReturnsInBlock v $
          fixExtIxFun
            i
            (primExpFromSubExp int64 (Var v))
            ixfun
  fixExt i se (ReturnsNewBlock space j ixfun) =
    ReturnsNewBlock
      space
      j'
      (fixExtIxFun i (primExpFromSubExp int64 se) ixfun)
    where
      j'
        | i < j = j - 1
        | otherwise = j
  fixExt i se (ReturnsInBlock mem ixfun) =
    ReturnsInBlock mem (fixExtIxFun i (primExpFromSubExp int64 se) ixfun)

fixExtIxFun :: Int -> PrimExp VName -> ExtIxFun -> ExtIxFun
fixExtIxFun i e = fmap $ isInt64 . replaceInPrimExp update . untyped
  where
    update (Ext j) t
      | j > i = LeafExp (Ext $ j - 1) t
      | j == i = fmap Free e
      | otherwise = LeafExp (Ext j) t
    update (Free x) t = LeafExp (Free x) t

leafExp :: Int -> TPrimExp Int64 (Ext a)
leafExp i = isInt64 $ LeafExp (Ext i) int64

existentialiseIxFun :: [VName] -> IxFun -> ExtIxFun
existentialiseIxFun ctx = IxFun.substituteInIxFun ctx' . fmap (fmap Free)
  where
    ctx' = M.map leafExp $ M.fromList $ zip (map Free ctx) [0 ..]

instance PP.Pretty MemReturn where
  pretty (ReturnsInBlock v ixfun) =
    pretty v <+> "->" PP.</> PP.pretty ixfun
  pretty (ReturnsNewBlock space i ixfun) =
    "?" <> pretty i <> PP.pretty space <+> "->" PP.</> PP.pretty ixfun

instance FreeIn MemReturn where
  freeIn' (ReturnsInBlock v ixfun) = freeIn' v <> freeIn' ixfun
  freeIn' (ReturnsNewBlock space _ ixfun) = freeIn' space <> freeIn' ixfun

instance Engine.Simplifiable MemReturn where
  simplify (ReturnsNewBlock space i ixfun) =
    ReturnsNewBlock space i <$> simplifyExtIxFun ixfun
  simplify (ReturnsInBlock v ixfun) =
    ReturnsInBlock <$> Engine.simplify v <*> simplifyExtIxFun ixfun

instance Engine.Simplifiable MemBind where
  simplify (ArrayIn mem ixfun) =
    ArrayIn <$> Engine.simplify mem <*> simplifyIxFun ixfun

instance Engine.Simplifiable [FunReturns] where
  simplify = mapM Engine.simplify

-- | The memory return of an expression.  An array is annotated with
-- @Maybe MemReturn@, which can be interpreted as the expression
-- either dictating exactly where the array is located when it is
-- returned (if 'Just'), or able to put it whereever the binding
-- prefers (if 'Nothing').
--
-- This is necessary to capture the difference between an expression
-- that is just an array-typed variable, in which the array being
-- "returned" is located where it already is, and a @copy@ expression,
-- whose entire purpose is to store an existing array in some
-- arbitrary location.  This is a consequence of the design decision
-- never to have implicit memory copies.
type ExpReturns = MemInfo ExtSize NoUniqueness (Maybe MemReturn)

-- | The return of a body, which must always indicate where
-- returned arrays are located.
type BodyReturns = MemInfo ExtSize NoUniqueness MemReturn

-- | The memory return of a function, which must always indicate where
-- returned arrays are located.
type FunReturns = MemInfo ExtSize Uniqueness MemReturn

maybeReturns :: MemInfo d u r -> MemInfo d u (Maybe r)
maybeReturns (MemArray bt shape u ret) =
  MemArray bt shape u $ Just ret
maybeReturns (MemPrim bt) =
  MemPrim bt
maybeReturns (MemMem space) =
  MemMem space
maybeReturns (MemAcc acc ispace ts u) =
  MemAcc acc ispace ts u

noUniquenessReturns :: MemInfo d u r -> MemInfo d NoUniqueness r
noUniquenessReturns (MemArray bt shape _ r) =
  MemArray bt shape NoUniqueness r
noUniquenessReturns (MemPrim bt) =
  MemPrim bt
noUniquenessReturns (MemMem space) =
  MemMem space
noUniquenessReturns (MemAcc acc ispace ts _) =
  MemAcc acc ispace ts NoUniqueness

funReturnsToExpReturns :: FunReturns -> ExpReturns
funReturnsToExpReturns = noUniquenessReturns . maybeReturns

bodyReturnsToExpReturns :: BodyReturns -> ExpReturns
bodyReturnsToExpReturns = noUniquenessReturns . maybeReturns

varInfoToExpReturns :: MemInfo SubExp NoUniqueness MemBind -> ExpReturns
varInfoToExpReturns (MemArray et shape u (ArrayIn mem ixfun)) =
  MemArray et (fmap Free shape) u $
    Just $
      ReturnsInBlock mem $
        existentialiseIxFun [] ixfun
varInfoToExpReturns (MemPrim pt) = MemPrim pt
varInfoToExpReturns (MemAcc acc ispace ts u) = MemAcc acc ispace ts u
varInfoToExpReturns (MemMem space) = MemMem space

matchRetTypeToResult ::
  (Mem rep inner, TC.Checkable rep) =>
  [FunReturns] ->
  Result ->
  TC.TypeM rep ()
matchRetTypeToResult rettype result = do
  scope <- askScope
  result_ts <- runReaderT (mapM (subExpMemInfo . resSubExp) result) $ removeScopeAliases scope
  matchReturnType rettype (map resSubExp result) result_ts

matchFunctionReturnType ::
  (Mem rep inner, TC.Checkable rep) =>
  [FunReturns] ->
  Result ->
  TC.TypeM rep ()
matchFunctionReturnType rettype result = do
  matchRetTypeToResult rettype result
  mapM_ (checkResultSubExp . resSubExp) result
  where
    checkResultSubExp Constant {} =
      pure ()
    checkResultSubExp (Var v) = do
      dec <- varMemInfo v
      case dec of
        MemPrim _ -> pure ()
        MemMem {} -> pure ()
        MemAcc {} -> pure ()
        MemArray _ _ _ (ArrayIn _ ixfun)
          | IxFun.isLinear ixfun ->
              pure ()
          | otherwise ->
              TC.bad . TC.TypeError $
                "Array "
                  <> prettyText v
                  <> " returned by function, but has nontrivial index function "
                  <> prettyText ixfun

matchLoopResultMem ::
  (Mem rep inner, TC.Checkable rep) =>
  [FParam (Aliases rep)] ->
  Result ->
  TC.TypeM rep ()
matchLoopResultMem params = matchRetTypeToResult rettype
  where
    param_names = map paramName params

    -- Invent a ReturnType so we can pretend that the loop body is
    -- actually returning from a function.
    rettype = map (toRet . paramDec) params

    toExtV v
      | Just i <- v `elemIndex` param_names = Ext i
      | otherwise = Free v

    toExtSE (Var v) = Var <$> toExtV v
    toExtSE (Constant v) = Free $ Constant v

    toRet (MemPrim t) =
      MemPrim t
    toRet (MemMem space) =
      MemMem space
    toRet (MemAcc acc ispace ts u) =
      MemAcc acc ispace ts u
    toRet (MemArray pt shape u (ArrayIn mem ixfun))
      | Just i <- mem `elemIndex` param_names,
        Param _ _ (MemMem space) : _ <- drop i params =
          MemArray pt shape' u $ ReturnsNewBlock space i ixfun'
      | otherwise =
          MemArray pt shape' u $ ReturnsInBlock mem ixfun'
      where
        shape' = fmap toExtSE shape
        ixfun' = existentialiseIxFun param_names ixfun

matchBranchReturnType ::
  (Mem rep inner, TC.Checkable rep) =>
  [BodyReturns] ->
  Body (Aliases rep) ->
  TC.TypeM rep ()
matchBranchReturnType rettype (Body _ stms res) = do
  scope <- askScope
  ts <- runReaderT (mapM (subExpMemInfo . resSubExp) res) $ removeScopeAliases (scope <> scopeOf stms)
  matchReturnType rettype (map resSubExp res) ts

-- | Helper function for index function unification.
--
-- The first return value maps a VName (wrapped in 'Free') to its Int
-- (wrapped in 'Ext').  In case of duplicates, it is mapped to the
-- *first* Int that occurs.
--
-- The second return value maps each Int (wrapped in an 'Ext') to a
-- 'LeafExp' 'Ext' with the Int at which its associated VName first
-- occurs.
getExtMaps ::
  [(VName, Int)] ->
  ( M.Map (Ext VName) (TPrimExp Int64 (Ext VName)),
    M.Map (Ext VName) (TPrimExp Int64 (Ext VName))
  )
getExtMaps ctx_lst_ids =
  ( M.map leafExp $ M.mapKeys Free $ M.fromListWith (const id) ctx_lst_ids,
    M.fromList $
      mapMaybe
        ( traverse
            ( fmap (\i -> isInt64 $ LeafExp (Ext i) int64)
                . (`lookup` ctx_lst_ids)
            )
            . uncurry (flip (,))
            . fmap Ext
        )
        ctx_lst_ids
  )

matchReturnType ::
  PP.Pretty u =>
  [MemInfo ExtSize u MemReturn] ->
  [SubExp] ->
  [MemInfo SubExp NoUniqueness MemBind] ->
  TC.TypeM rep ()
matchReturnType rettype res ts = do
  let existentialiseIxFun0 :: IxFun -> ExtIxFun
      existentialiseIxFun0 = fmap $ fmap Free

      fetchCtx i = case maybeNth i $ zip res ts of
        Nothing ->
          throwError $ "Cannot find variable #" <> prettyText i <> " in results: " <> prettyText res
        Just (se, t) -> pure (se, t)

      checkReturn (MemPrim x) (MemPrim y)
        | x == y = pure ()
      checkReturn (MemMem x) (MemMem y)
        | x == y = pure ()
      checkReturn (MemAcc xacc xispace xts _) (MemAcc yacc yispace yts _)
        | (xacc, xispace, xts) == (yacc, yispace, yts) =
            pure ()
      checkReturn
        (MemArray x_pt x_shape _ x_ret)
        (MemArray y_pt y_shape _ y_ret)
          | x_pt == y_pt,
            shapeRank x_shape == shapeRank y_shape = do
              zipWithM_ checkDim (shapeDims x_shape) (shapeDims y_shape)
              checkMemReturn x_ret y_ret
      checkReturn x y =
        throwError $ T.unwords ["Expected", prettyText x, "but got", prettyText y]

      checkDim (Free x) y
        | x == y = pure ()
        | otherwise =
            throwError $ T.unwords ["Expected dim", prettyText x, "but got", prettyText y]
      checkDim (Ext i) y = do
        (x, _) <- fetchCtx i
        unless (x == y) . throwError . T.unwords $
          ["Expected ext dim", prettyText i, "=>", prettyText x, "but got", prettyText y]

      checkMemReturn (ReturnsInBlock x_mem x_ixfun) (ArrayIn y_mem y_ixfun)
        | x_mem == y_mem =
            unless (IxFun.closeEnough x_ixfun $ existentialiseIxFun0 y_ixfun) $
              throwError . T.unwords $
                [ "Index function unification failed (ReturnsInBlock)",
                  "\nixfun of body result: ",
                  prettyText y_ixfun,
                  "\nixfun of return type: ",
                  prettyText x_ixfun
                ]
      checkMemReturn
        (ReturnsNewBlock x_space x_ext x_ixfun)
        (ArrayIn y_mem y_ixfun) = do
          (x_mem, x_mem_type) <- fetchCtx x_ext
          unless (IxFun.closeEnough x_ixfun $ existentialiseIxFun0 y_ixfun) $
            throwError . docText $
              "Index function unification failed (ReturnsNewBlock)"
                </> "Ixfun of body result:"
                </> indent 2 (pretty y_ixfun)
                </> "Ixfun of return type:"
                </> indent 2 (pretty x_ixfun)
          case x_mem_type of
            MemMem y_space ->
              unless (x_space == y_space) . throwError . T.unwords $
                [ "Expected memory",
                  prettyText y_mem,
                  "in space",
                  prettyText x_space,
                  "but actually in space",
                  prettyText y_space
                ]
            t ->
              throwError . T.unwords $
                ["Expected memory", prettyText x_ext, "=>", prettyText x_mem, "but but has type", prettyText t]
      checkMemReturn x y =
        throwError . docText $
          "Expected array in"
            </> indent 2 (pretty x)
            </> "but array returned in"
            </> indent 2 (pretty y)

      bad s =
        TC.bad . TC.TypeError . docText $
          "Return type"
            </> indent 2 (ppTupleLines' $ map pretty rettype)
            </> "cannot match returns of results"
            </> indent 2 (ppTupleLines' $ map pretty ts)
            </> pretty s

  either bad pure =<< runExceptT (zipWithM_ checkReturn rettype ts)

matchPatToExp ::
  (Mem rep inner, LetDec rep ~ LetDecMem, TC.Checkable rep) =>
  Pat (LetDec (Aliases rep)) ->
  Exp (Aliases rep) ->
  TC.TypeM rep ()
matchPatToExp pat e = do
  scope <- asksScope removeScopeAliases
  rt <- runReaderT (expReturns $ removeExpAliases e) scope

  let (ctx_ids, val_ts) = unzip $ bodyReturnsFromPat $ removePatAliases pat
      (ctx_map_ids, ctx_map_exts) = getExtMaps $ zip ctx_ids [0 .. 1]
      ok =
        length val_ts == length rt
          && and (zipWith (matches ctx_map_ids ctx_map_exts) val_ts rt)

  unless ok . TC.bad . TC.TypeError . docText $
    "Expression type:"
      </> indent 2 (ppTupleLines' $ map pretty rt)
      </> "cannot match pattern type:"
      </> indent 2 (ppTupleLines' $ map pretty val_ts)
  where
    matches _ _ (MemPrim x) (MemPrim y) = x == y
    matches _ _ (MemMem x_space) (MemMem y_space) =
      x_space == y_space
    matches _ _ (MemAcc x_accs x_ispace x_ts _) (MemAcc y_accs y_ispace y_ts _) =
      (x_accs, x_ispace, x_ts) == (y_accs, y_ispace, y_ts)
    matches ctxids ctxexts (MemArray x_pt x_shape _ x_ret) (MemArray y_pt y_shape _ y_ret) =
      x_pt == y_pt
        && x_shape == y_shape
        && case (x_ret, y_ret) of
          (ReturnsInBlock _ x_ixfun, Just (ReturnsInBlock _ y_ixfun)) ->
            let x_ixfun' = IxFun.substituteInIxFun ctxids x_ixfun
                y_ixfun' = IxFun.substituteInIxFun ctxexts y_ixfun
             in IxFun.closeEnough x_ixfun' y_ixfun'
          ( ReturnsInBlock _ x_ixfun,
            Just (ReturnsNewBlock _ _ y_ixfun)
            ) ->
              let x_ixfun' = IxFun.substituteInIxFun ctxids x_ixfun
                  y_ixfun' = IxFun.substituteInIxFun ctxexts y_ixfun
               in IxFun.closeEnough x_ixfun' y_ixfun'
          ( ReturnsNewBlock _ x_i x_ixfun,
            Just (ReturnsNewBlock _ y_i y_ixfun)
            ) ->
              let x_ixfun' = IxFun.substituteInIxFun ctxids x_ixfun
                  y_ixfun' = IxFun.substituteInIxFun ctxexts y_ixfun
               in x_i == y_i && IxFun.closeEnough x_ixfun' y_ixfun'
          (_, Nothing) -> True
          _ -> False
    matches _ _ _ _ = False

varMemInfo ::
  Mem rep inner =>
  VName ->
  TC.TypeM rep (MemInfo SubExp NoUniqueness MemBind)
varMemInfo name = do
  dec <- TC.lookupVar name

  case dec of
    LetName (_, summary) -> pure $ letDecMem summary
    FParamName summary -> pure $ noUniquenessReturns summary
    LParamName summary -> pure summary
    IndexName it -> pure $ MemPrim $ IntType it

nameInfoToMemInfo :: Mem rep inner => NameInfo rep -> MemBound NoUniqueness
nameInfoToMemInfo info =
  case info of
    FParamName summary -> noUniquenessReturns summary
    LParamName summary -> summary
    LetName summary -> letDecMem summary
    IndexName it -> MemPrim $ IntType it

lookupMemInfo ::
  (HasScope rep m, Mem rep inner) =>
  VName ->
  m (MemInfo SubExp NoUniqueness MemBind)
lookupMemInfo = fmap nameInfoToMemInfo . lookupInfo

subExpMemInfo ::
  (HasScope rep m, Mem rep inner) =>
  SubExp ->
  m (MemInfo SubExp NoUniqueness MemBind)
subExpMemInfo (Var v) = lookupMemInfo v
subExpMemInfo (Constant v) = pure $ MemPrim $ primValueType v

lookupArraySummary ::
  (Mem rep inner, HasScope rep m, Monad m) =>
  VName ->
  m (VName, IxFun.IxFun (TPrimExp Int64 VName))
lookupArraySummary name = do
  summary <- lookupMemInfo name
  case summary of
    MemArray _ _ _ (ArrayIn mem ixfun) ->
      pure (mem, ixfun)
    _ ->
      error . T.unpack $
        "Expected "
          <> prettyText name
          <> " to be array but bound to:\n"
          <> prettyText summary

lookupMemSpace ::
  (Mem rep inner, HasScope rep m, Monad m) =>
  VName ->
  m Space
lookupMemSpace name = do
  summary <- lookupMemInfo name
  case summary of
    MemMem space ->
      pure space
    _ ->
      error . T.unpack $
        "Expected "
          <> prettyText name
          <> " to be memory but bound to:\n"
          <> prettyText summary

checkMemInfo ::
  TC.Checkable rep =>
  VName ->
  MemInfo SubExp u MemBind ->
  TC.TypeM rep ()
checkMemInfo _ (MemPrim _) = pure ()
checkMemInfo _ (MemMem (ScalarSpace d _)) = mapM_ (TC.require [Prim int64]) d
checkMemInfo _ (MemMem _) = pure ()
checkMemInfo _ (MemAcc acc ispace ts u) =
  TC.checkType $ Acc acc ispace ts u
checkMemInfo name (MemArray _ shape _ (ArrayIn v ixfun)) = do
  t <- lookupType v
  case t of
    Mem {} ->
      pure ()
    _ ->
      TC.bad $
        TC.TypeError $
          "Variable "
            <> prettyText v
            <> " used as memory block, but is of type "
            <> prettyText t
            <> "."

  TC.context ("in index function " <> prettyText ixfun) $ do
    traverse_ (TC.requirePrimExp int64 . untyped) ixfun
    let ixfun_rank = IxFun.rank ixfun
        ident_rank = shapeRank shape
    unless (ixfun_rank == ident_rank) $
      TC.bad $
        TC.TypeError $
          "Arity of index function ("
            <> prettyText ixfun_rank
            <> ") does not match rank of array "
            <> prettyText name
            <> " ("
            <> prettyText ident_rank
            <> ")"

bodyReturnsFromPat ::
  Pat (MemBound NoUniqueness) -> [(VName, BodyReturns)]
bodyReturnsFromPat pat =
  map asReturns $ patElems pat
  where
    ctx = patElems pat

    ext (Var v)
      | Just (i, _) <- find ((== v) . patElemName . snd) $ zip [0 ..] ctx =
          Ext i
    ext se = Free se

    asReturns pe =
      ( patElemName pe,
        case patElemDec pe of
          MemPrim pt -> MemPrim pt
          MemMem space -> MemMem space
          MemArray pt shape u (ArrayIn mem ixfun) ->
            MemArray pt (Shape $ map ext $ shapeDims shape) u $
              case find ((== mem) . patElemName . snd) $ zip [0 ..] ctx of
                Just (i, PatElem _ (MemMem space)) ->
                  ReturnsNewBlock space i $
                    existentialiseIxFun (map patElemName ctx) ixfun
                _ -> ReturnsInBlock mem $ existentialiseIxFun [] ixfun
          MemAcc acc ispace ts u -> MemAcc acc ispace ts u
      )

extReturns :: [ExtType] -> [ExpReturns]
extReturns ets =
  evalState (mapM addDec ets) 0
  where
    addDec (Prim bt) =
      pure $ MemPrim bt
    addDec (Mem space) =
      pure $ MemMem space
    addDec t@(Array bt shape u)
      | existential t = do
          i <- get <* modify (+ 1)
          pure $
            MemArray bt shape u $
              Just $
                ReturnsNewBlock DefaultSpace i $
                  IxFun.iota $
                    map convert $
                      shapeDims shape
      | otherwise =
          pure $ MemArray bt shape u Nothing
    addDec (Acc acc ispace ts u) =
      pure $ MemAcc acc ispace ts u
    convert (Ext i) = le64 (Ext i)
    convert (Free v) = Free <$> pe64 v

arrayVarReturns ::
  (HasScope rep m, Monad m, Mem rep inner) =>
  VName ->
  m (PrimType, Shape, VName, IxFun)
arrayVarReturns v = do
  summary <- lookupMemInfo v
  case summary of
    MemArray et shape _ (ArrayIn mem ixfun) ->
      pure (et, Shape $ shapeDims shape, mem, ixfun)
    _ ->
      error . T.unpack $ "arrayVarReturns: " <> prettyText v <> " is not an array."

varReturns ::
  (HasScope rep m, Monad m, Mem rep inner) =>
  VName ->
  m ExpReturns
varReturns v = do
  summary <- lookupMemInfo v
  case summary of
    MemPrim bt ->
      pure $ MemPrim bt
    MemArray et shape _ (ArrayIn mem ixfun) ->
      pure $
        MemArray et (fmap Free shape) NoUniqueness $
          Just $
            ReturnsInBlock mem $
              existentialiseIxFun [] ixfun
    MemMem space ->
      pure $ MemMem space
    MemAcc acc ispace ts u ->
      pure $ MemAcc acc ispace ts u

subExpReturns :: (HasScope rep m, Monad m, Mem rep inner) => SubExp -> m ExpReturns
subExpReturns (Var v) =
  varReturns v
subExpReturns (Constant v) =
  pure $ MemPrim $ primValueType v

-- | The return information of an expression.  This can be seen as the
-- "return type with memory annotations" of the expression.
expReturns ::
  (LocalScope rep m, Mem rep inner) =>
  Exp rep ->
  m [ExpReturns]
expReturns (BasicOp (SubExp se)) =
  pure <$> subExpReturns se
expReturns (BasicOp (Opaque _ (Var v))) =
  pure <$> varReturns v
expReturns (BasicOp (Reshape k newshape v)) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  pure
    [ MemArray et (fmap Free newshape) NoUniqueness $
        Just . ReturnsInBlock mem . existentialiseIxFun [] $
          reshaper ixfun $
            map pe64 (shapeDims newshape)
    ]
  where
    reshaper = case k of
      ReshapeArbitrary -> IxFun.reshape
      ReshapeCoerce -> IxFun.coerce
expReturns (BasicOp (Rearrange perm v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let ixfun' = IxFun.permute ixfun perm
      dims' = rearrangeShape perm dims
  pure
    [ MemArray et (Shape $ map Free dims') NoUniqueness $
        Just $
          ReturnsInBlock mem $
            existentialiseIxFun [] ixfun'
    ]
expReturns (BasicOp (Index v slice)) = do
  pure . varInfoToExpReturns <$> sliceInfo v slice
expReturns (BasicOp (Update _ v _ _)) =
  pure <$> varReturns v
expReturns (BasicOp (FlatIndex v slice)) = do
  pure . varInfoToExpReturns <$> flatSliceInfo v slice
expReturns (BasicOp (FlatUpdate v _ _)) =
  pure <$> varReturns v
expReturns (BasicOp op) =
  extReturns . staticShapes <$> basicOpType op
expReturns e@(DoLoop merge _ _) = do
  t <- expExtType e
  zipWithM typeWithDec t $ map fst merge
  where
    typeWithDec t p =
      case (t, paramDec p) of
        ( Array pt shape u,
          MemArray _ _ _ (ArrayIn mem ixfun)
          )
            | Just (i, mem_p) <- isMergeVar mem,
              Mem space <- paramType mem_p ->
                pure $ MemArray pt shape u $ Just $ ReturnsNewBlock space i ixfun'
            | otherwise ->
                pure $ MemArray pt shape u $ Just $ ReturnsInBlock mem ixfun'
            where
              ixfun' = existentialiseIxFun (map paramName mergevars) ixfun
        (Array {}, _) ->
          error "expReturns: Array return type but not array merge variable."
        (Acc acc ispace ts u, _) ->
          pure $ MemAcc acc ispace ts u
        (Prim pt, _) ->
          pure $ MemPrim pt
        (Mem space, _) ->
          pure $ MemMem space
    isMergeVar v = find ((== v) . paramName . snd) $ zip [0 ..] mergevars
    mergevars = map fst merge
expReturns (Apply _ _ ret _) =
  pure $ map funReturnsToExpReturns ret
expReturns (Match _ _ _ (MatchDec ret _)) =
  pure $ map bodyReturnsToExpReturns ret
expReturns (Op op) =
  opReturns op
expReturns (WithAcc inputs lam) =
  (<>)
    <$> (concat <$> mapM inputReturns inputs)
    <*>
    -- XXX: this is a bit dubious because it enforces extra copies.  I
    -- think WithAcc should perhaps have a return annotation like If.
    pure (extReturns $ staticShapes $ drop num_accs $ lambdaReturnType lam)
  where
    inputReturns (_, arrs, _) = mapM varReturns arrs
    num_accs = length inputs

sliceInfo ::
  (Monad m, HasScope rep m, Mem rep inner) =>
  VName ->
  Slice SubExp ->
  m (MemInfo SubExp NoUniqueness MemBind)
sliceInfo v slice = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  case sliceDims slice of
    [] -> pure $ MemPrim et
    dims ->
      pure $
        MemArray et (Shape dims) NoUniqueness . ArrayIn mem $
          IxFun.slice ixfun (fmap pe64 slice)

flatSliceInfo ::
  (Monad m, HasScope rep m, Mem rep inner) =>
  VName ->
  FlatSlice SubExp ->
  m (MemInfo SubExp NoUniqueness MemBind)
flatSliceInfo v slice@(FlatSlice offset idxs) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  map (fmap pe64) idxs
    & FlatSlice (pe64 offset)
    & IxFun.flatSlice ixfun
    & ArrayIn mem
    & MemArray et (Shape (flatSliceDims slice)) NoUniqueness
    & pure

class IsOp op => OpReturns op where
  opReturns :: (Mem rep inner, Monad m, HasScope rep m) => op -> m [ExpReturns]
  opReturns op = extReturns <$> opType op

instance OpReturns (inner rep) => OpReturns (MemOp inner rep) where
  opReturns (Alloc _ space) = pure [MemMem space]
  opReturns (Inner op) = opReturns op

instance OpReturns (NoOp rep) where
  opReturns NoOp = pure []

applyFunReturns ::
  Typed dec =>
  [FunReturns] ->
  [Param dec] ->
  [(SubExp, Type)] ->
  Maybe [FunReturns]
applyFunReturns rets params args
  | Just _ <- applyRetType rettype params args =
      Just $ map correctDims rets
  | otherwise =
      Nothing
  where
    rettype = map declExtTypeOf rets
    parammap :: M.Map VName (SubExp, Type)
    parammap =
      M.fromList $
        zip (map paramName params) args

    substSubExp (Var v)
      | Just (se, _) <- M.lookup v parammap = se
    substSubExp se = se

    correctDims (MemPrim t) =
      MemPrim t
    correctDims (MemMem space) =
      MemMem space
    correctDims (MemArray et shape u memsummary) =
      MemArray et (correctShape shape) u $
        correctSummary memsummary
    correctDims (MemAcc acc ispace ts u) =
      MemAcc acc ispace ts u

    correctShape = Shape . map correctDim . shapeDims
    correctDim (Ext i) = Ext i
    correctDim (Free se) = Free $ substSubExp se

    correctSummary (ReturnsNewBlock space i ixfun) =
      ReturnsNewBlock space i ixfun
    correctSummary (ReturnsInBlock mem ixfun) =
      -- FIXME: we should also do a replacement in ixfun here.
      ReturnsInBlock mem' ixfun
      where
        mem' = case M.lookup mem parammap of
          Just (Var v, _) -> v
          _ -> mem
