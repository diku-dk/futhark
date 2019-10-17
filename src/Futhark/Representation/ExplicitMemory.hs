{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
-- | This representation requires that every array is given
-- information about which memory block is it based in, and how array
-- elements map to memory block offsets.  The representation is based
-- on the kernels representation, so nested parallelism does not
-- occur.
--
-- There are two primary concepts you will need to understand:
--
--  1. Memory blocks, which are Futhark values of type 'Mem'
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
module Futhark.Representation.ExplicitMemory
       ( -- * The Lore definition
         ExplicitMemory
       , MemOp (..)
       , MemInfo (..)
       , MemBound
       , MemBind (..)
       , MemReturn (..)
       , IxFun
       , ExtIxFun
       , isStaticIxFun
       , ExpReturns
       , BodyReturns
       , FunReturns
       , noUniquenessReturns
       , bodyReturnsToExpReturns
       , ExplicitMemorish
       , expReturns
       , extReturns
       , sliceInfo
       , lookupMemInfo
       , subExpMemInfo
       , lookupArraySummary
       , fullyLinear
       , ixFunMatchesInnerShape
       , existentialiseIxFun

       , scalarMemory
       , allScalarMemory

         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , module Futhark.Representation.Kernels.Kernel
       , module Futhark.Analysis.PrimExp.Convert
       )
where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Foldable (traverse_)
import Data.List

import Futhark.Analysis.Metrics
import Futhark.Representation.AST.Syntax
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import qualified Futhark.TypeCheck as TC
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.PrimExp.Simplify
import Futhark.Util
import Futhark.Util.IntegralExp
import qualified Futhark.Util.Pretty as PP
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Lore
import Futhark.Representation.Aliases
  (Aliases, removeScopeAliases, removeExpAliases, removePatternAliases)
import Futhark.Representation.AST.Attributes.Ranges
import qualified Futhark.Analysis.SymbolTable as ST

-- | A lore containing explicit memory information.
data ExplicitMemory

type ExplicitMemorish lore = (SameScope lore ExplicitMemory,
                              RetType lore ~ FunReturns,
                              BranchType lore ~ BodyReturns,
                              CanBeAliased (Op lore),
                              Attributes lore, Annotations lore,
                              TC.Checkable lore,
                              OpReturns lore)

instance IsRetType FunReturns where
  primRetType = MemPrim
  applyRetType = applyFunReturns

instance IsBodyType BodyReturns where
  primBodyType = MemPrim

data MemOp inner = Alloc SubExp Space
                   -- ^ Allocate a memory block.  This really should not be an
                   -- expression, but what are you gonna do...
                 | Inner inner
            deriving (Eq, Ord, Show)

instance FreeIn inner => FreeIn (MemOp inner) where
  freeIn' (Alloc size _) = freeIn' size
  freeIn' (Inner k) = freeIn' k

instance TypedOp inner => TypedOp (MemOp inner) where
  opType (Alloc _ space) = pure [Mem space]
  opType (Inner k) = opType k

instance AliasedOp inner => AliasedOp (MemOp inner) where
  opAliases Alloc{} = [mempty]
  opAliases (Inner k) = opAliases k

  consumedInOp Alloc{} = mempty
  consumedInOp (Inner k) = consumedInOp k

instance CanBeAliased inner => CanBeAliased (MemOp inner) where
  type OpWithAliases (MemOp inner) = MemOp (OpWithAliases inner)
  removeOpAliases (Alloc se space) = Alloc se space
  removeOpAliases (Inner k) = Inner $ removeOpAliases k

  addOpAliases (Alloc se space) = Alloc se space
  addOpAliases (Inner k) = Inner $ addOpAliases k

instance RangedOp inner => RangedOp (MemOp inner) where
  opRanges (Alloc _ _) =
    [unknownRange]
  opRanges (Inner k) =
    opRanges k

instance CanBeRanged inner => CanBeRanged (MemOp inner) where
  type OpWithRanges (MemOp inner) = MemOp (OpWithRanges inner)
  removeOpRanges (Alloc size space) = Alloc size space
  removeOpRanges (Inner k) = Inner $ removeOpRanges k

  addOpRanges (Alloc size space) = Alloc size space
  addOpRanges (Inner k) = Inner $ addOpRanges k

instance Rename inner => Rename (MemOp inner) where
  rename (Alloc size space) = Alloc <$> rename size <*> pure space
  rename (Inner k) = Inner <$> rename k

instance Substitute inner => Substitute (MemOp inner) where
  substituteNames subst (Alloc size space) = Alloc (substituteNames subst size) space
  substituteNames subst (Inner k) = Inner $ substituteNames subst k

instance PP.Pretty inner => PP.Pretty (MemOp inner) where
  ppr (Alloc e DefaultSpace) = PP.text "alloc" <> PP.apply [PP.ppr e]
  ppr (Alloc e (Space sp)) = PP.text "alloc" <> PP.apply [PP.ppr e, PP.text sp]
  ppr (Inner k) = PP.ppr k

instance OpMetrics inner => OpMetrics (MemOp inner) where
  opMetrics Alloc{} = seen "Alloc"
  opMetrics (Inner k) = opMetrics k

instance IsOp inner => IsOp (MemOp inner) where
  safeOp Alloc{} = False
  safeOp (Inner k) = safeOp k
  cheapOp (Inner k) = cheapOp k
  cheapOp Alloc{} = True

instance CanBeWise inner => CanBeWise (MemOp inner) where
  type OpWithWisdom (MemOp inner) = MemOp (OpWithWisdom inner)
  removeOpWisdom (Alloc size space) = Alloc size space
  removeOpWisdom (Inner k) = Inner $ removeOpWisdom k

instance ST.IndexOp inner => ST.IndexOp (MemOp inner) where
  indexOp vtable k (Inner op) is = ST.indexOp vtable k op is
  indexOp _ _ _ _ = Nothing

instance Annotations ExplicitMemory where
  type LetAttr    ExplicitMemory = MemInfo SubExp NoUniqueness MemBind
  type FParamAttr ExplicitMemory = MemInfo SubExp Uniqueness MemBind
  type LParamAttr ExplicitMemory = MemInfo SubExp NoUniqueness MemBind
  type RetType    ExplicitMemory = FunReturns
  type BranchType ExplicitMemory = BodyReturns
  type Op         ExplicitMemory = MemOp (HostOp ExplicitMemory ())

-- | The index function representation used for memory annotations.
type IxFun = IxFun.IxFun (PrimExp VName)

-- | An index function that may contain existential variables.
type ExtIxFun = IxFun.IxFun (PrimExp (Ext VName))

-- | A summary of the memory information for every let-bound
-- identifier, function parameter, and return value.  Parameterisered
-- over uniqueness, dimension, and auxiliary array information.
data MemInfo d u ret = MemPrim PrimType
                     -- ^ A primitive value.
                     | MemMem Space
                     -- ^ A memory block.
                     | MemArray PrimType (ShapeBase d) u ret
                     -- ^ The array is stored in the named memory block,
                     -- and with the given index function.  The index
                     -- function maps indices in the array to /element/
                     -- offset, /not/ byte offsets!  To translate to byte
                     -- offsets, multiply the offset with the size of the
                     -- array element type.
                     deriving (Eq, Show, Ord) --- XXX Ord?

type MemBound u = MemInfo SubExp u MemBind

instance FixExt ret => DeclExtTyped (MemInfo ExtSize Uniqueness ret) where
  declExtTypeOf (MemPrim pt) = Prim pt
  declExtTypeOf (MemMem space) = Mem space
  declExtTypeOf (MemArray pt shape u _) = Array pt shape u

instance FixExt ret => ExtTyped (MemInfo ExtSize NoUniqueness ret) where
  extTypeOf (MemPrim pt) = Prim pt
  extTypeOf (MemMem space) = Mem space
  extTypeOf (MemArray pt shape u _) = Array pt shape u

instance FixExt ret => FixExt (MemInfo ExtSize u ret) where
  fixExt _ _ (MemPrim pt) = MemPrim pt
  fixExt _ _ (MemMem space) = MemMem space
  fixExt i se (MemArray pt shape u ret) =
    MemArray pt (fixExt i se shape) u (fixExt i se ret)

instance Typed (MemInfo SubExp Uniqueness ret) where
  typeOf = fromDecl . declTypeOf

instance Typed (MemInfo SubExp NoUniqueness ret) where
  typeOf (MemPrim pt) = Prim pt
  typeOf (MemMem space) = Mem space
  typeOf (MemArray bt shape u _) = Array bt shape u

instance DeclTyped (MemInfo SubExp Uniqueness ret) where
  declTypeOf (MemPrim bt) = Prim bt
  declTypeOf (MemMem space) = Mem space
  declTypeOf (MemArray bt shape u _) = Array bt shape u

instance (FreeIn d, FreeIn ret) => FreeIn (MemInfo d u ret) where
  freeIn' (MemArray _ shape _ ret) = freeIn' shape <> freeIn' ret
  freeIn' MemMem{} = mempty
  freeIn' MemPrim{} = mempty

instance (Substitute d, Substitute ret) => Substitute (MemInfo d u ret) where
  substituteNames subst (MemArray bt shape u ret) =
    MemArray bt
    (substituteNames subst shape) u
    (substituteNames subst ret)
  substituteNames _ (MemMem space) =
    MemMem space
  substituteNames _ (MemPrim bt) =
    MemPrim bt

instance (Substitute d, Substitute ret) => Rename (MemInfo d u ret) where
  rename = substituteRename

simplifyIxFun :: Engine.SimplifiableLore lore =>
                 IxFun -> Engine.SimpleM lore IxFun
simplifyIxFun = traverse simplifyPrimExp

simplifyExtIxFun :: Engine.SimplifiableLore lore =>
                    ExtIxFun -> Engine.SimpleM lore ExtIxFun
simplifyExtIxFun = traverse simplifyExtPrimExp

isStaticIxFun :: ExtIxFun -> Maybe IxFun
isStaticIxFun = traverse $ traverse inst
  where inst Ext{} = Nothing
        inst (Free x) = Just x

instance (Engine.Simplifiable d, Engine.Simplifiable ret) =>
         Engine.Simplifiable (MemInfo d u ret) where
  simplify (MemPrim bt) =
    return $ MemPrim bt
  simplify (MemMem space) =
    pure $ MemMem space
  simplify (MemArray bt shape u ret) =
    MemArray bt <$> Engine.simplify shape <*> pure u <*> Engine.simplify ret

instance (PP.Pretty (TypeBase (ShapeBase d) u),
          PP.Pretty d, PP.Pretty u, PP.Pretty ret) => PP.Pretty (MemInfo d u ret) where
  ppr (MemPrim bt) = PP.ppr bt
  ppr (MemMem DefaultSpace) =
    PP.text "mem"
  ppr (MemMem (Space sp)) =
    PP.text "mem" <> PP.text "@" <> PP.text sp
  ppr (MemArray bt shape u ret) =
    PP.ppr (Array bt shape u) <> PP.text "@" <> PP.ppr ret

instance PP.Pretty (Param (MemInfo SubExp Uniqueness ret)) where
  ppr = PP.ppr . fmap declTypeOf

instance PP.Pretty (Param (MemInfo SubExp NoUniqueness ret)) where
  ppr = PP.ppr . fmap typeOf

instance PP.Pretty (PatElemT (MemInfo SubExp NoUniqueness ret)) where
  ppr = PP.ppr . fmap typeOf

-- | Memory information for an array bound somewhere in the program.
data MemBind = ArrayIn VName IxFun
             -- ^ Located in this memory block with this index
             -- function.
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
  ppr (ArrayIn mem ixfun) =
    PP.text "@" <> PP.ppr mem <> PP.text "->" <> PP.ppr ixfun

instance FreeIn MemBind where
  freeIn' (ArrayIn mem ixfun) = freeIn' mem <> freeIn' ixfun

-- | A description of the memory properties of an array being returned
-- by an operation.
data MemReturn = ReturnsInBlock VName ExtIxFun
                 -- ^ The array is located in a memory block that is
                 -- already in scope.
               | ReturnsNewBlock Space Int ExtIxFun
                 -- ^ The operation returns a new (existential) memory
                 -- block.
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
    | j == i = ReturnsInBlock v $ fixExtIxFun i
               (primExpFromSubExp int32 (Var v)) ixfun
  fixExt i se (ReturnsNewBlock space j ixfun) =
    ReturnsNewBlock space j'
    (fixExtIxFun i (primExpFromSubExp int32 se) ixfun)
    where j' | i < j     = j-1
             | otherwise = j
  fixExt i se (ReturnsInBlock mem ixfun) =
    ReturnsInBlock mem (fixExtIxFun i (primExpFromSubExp int32 se) ixfun)

fixExtIxFun :: Int -> PrimExp VName -> ExtIxFun -> ExtIxFun
fixExtIxFun i e = fmap $ replaceInPrimExp update
  where update (Ext j) t | j > i     = LeafExp (Ext $ j - 1) t
                         | j == i    = fmap Free e
                         | otherwise = LeafExp (Ext j) t
        update (Free x) t = LeafExp (Free x) t

leafExp :: Int -> PrimExp (Ext a)
leafExp i = LeafExp (Ext i) int32

existentialiseIxFun :: [VName] -> IxFun -> ExtIxFun
existentialiseIxFun ctx = IxFun.substituteInIxFun ctx' . fmap (fmap Free)
  where ctx' = M.map leafExp $ M.fromList $ zip (map Free ctx) [0..]

instance PP.Pretty MemReturn where
  ppr (ReturnsInBlock v ixfun) =
    PP.parens $ PP.text (pretty v) <> PP.text "->" <> PP.ppr ixfun
  ppr (ReturnsNewBlock space i ixfun) =
    PP.text ("?" ++ show i) <> space' <> PP.text "->" <> PP.ppr ixfun
    where space' = case space of DefaultSpace -> mempty
                                 Space s -> PP.text $ "@" ++ s

instance FreeIn MemReturn where
  freeIn' (ReturnsInBlock v ixfun) = freeIn' v <> freeIn' ixfun
  freeIn' _                        = mempty

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

noUniquenessReturns :: MemInfo d u r -> MemInfo d NoUniqueness r
noUniquenessReturns (MemArray bt shape _ r) =
  MemArray bt shape NoUniqueness r
noUniquenessReturns (MemPrim bt) =
  MemPrim bt
noUniquenessReturns (MemMem space) =
  MemMem space

funReturnsToExpReturns :: FunReturns -> ExpReturns
funReturnsToExpReturns = noUniquenessReturns . maybeReturns

bodyReturnsToExpReturns :: BodyReturns -> ExpReturns
bodyReturnsToExpReturns = noUniquenessReturns . maybeReturns

instance TC.CheckableOp ExplicitMemory where
  checkOp = typeCheckExplicitMemoryOp Nothing
    where typeCheckExplicitMemoryOp _ (Alloc size _) =
            TC.require [Prim int64] size
          typeCheckExplicitMemoryOp lvl (Inner op) =
            typeCheckHostOp (typeCheckExplicitMemoryOp . Just) lvl (const $ return ()) op

instance TC.Checkable ExplicitMemory where
  checkFParamLore = checkMemInfo
  checkLParamLore = checkMemInfo
  checkLetBoundLore = checkMemInfo
  checkRetType = mapM_ TC.checkExtType . retTypeValues
  primFParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType
  matchBranchType = matchBranchReturnType

matchFunctionReturnType :: ExplicitMemorish lore =>
                           [FunReturns] -> Result -> TC.TypeM lore ()
matchFunctionReturnType rettype result = do
  TC.matchExtReturnType (fromDecl <$> ts) result
  scope <- askScope
  result_ts <- runReaderT (mapM subExpMemInfo result) $ removeScopeAliases scope
  matchReturnType rettype result result_ts
  mapM_ checkResultSubExp result
  where ts = map declExtTypeOf rettype
        checkResultSubExp Constant{} =
          return ()
        checkResultSubExp (Var v) = do
          attr <- varMemInfo v
          case attr of
            MemPrim _ -> return ()
            MemMem{} -> return ()
            MemArray _ _ _ (ArrayIn _ ixfun)
              | IxFun.isLinear ixfun ->
                return ()
              | otherwise ->
                  TC.bad $ TC.TypeError $
                  "Array " ++ pretty v ++
                  " returned by function, but has nontrivial index function " ++
                  pretty ixfun

matchBranchReturnType :: ExplicitMemorish lore =>
                         [BodyReturns]
                      -> Body (Aliases lore)
                      -> TC.TypeM lore ()
matchBranchReturnType rettype (Body _ stms res) = do
  scope <- askScope
  ts <- runReaderT (mapM subExpMemInfo res) $ removeScopeAliases (scope <> scopeOf stms)
  matchReturnType rettype res ts

-- | Helper function for index function unification.
--
-- The first return value maps a VName (wrapped in 'Free') to its Int
-- (wrapped in 'Ext').  In case of duplicates, it is mapped to the
-- *first* Int that occurs.
--
-- The second return value maps each Int (wrapped in an 'Ext') to a
-- 'LeafExp' 'Ext' with the Int at which its associated VName first
-- occurs.
getExtMaps :: [(VName,Int)] -> (M.Map (Ext VName) (PrimExp (Ext VName)),
                                M.Map (Ext VName) (PrimExp (Ext VName)))
getExtMaps ctx_lst_ids =
  (M.map leafExp $ M.mapKeys Free $ M.fromListWith (flip const) ctx_lst_ids,
   M.fromList $
   mapMaybe (traverse (fmap (\i -> LeafExp (Ext i) int32) .
                       (`lookup` ctx_lst_ids)) .
             uncurry (flip (,)) . fmap Ext) ctx_lst_ids)

matchReturnType :: PP.Pretty u =>
                   [MemInfo ExtSize u MemReturn]
                -> [SubExp]
                -> [MemInfo SubExp NoUniqueness MemBind]
                -> TC.TypeM lore ()
matchReturnType rettype res ts = do
  let (ctx_ts, val_ts) = splitFromEnd (length rettype) ts
      (ctx_res, _val_res) = splitFromEnd (length rettype) res

      getId :: (SubExp,Int) -> Maybe (VName,Int)
      getId (Var ii, i) = Just (ii,i)
      getId (Constant _, _) = Nothing

      (ctx_map_ids, ctx_map_exts) =
        getExtMaps $ mapMaybe getId $ zip ctx_res [0..length ctx_res - 1]

      existentialiseIxFun0 :: IxFun -> ExtIxFun
      existentialiseIxFun0 = IxFun.substituteInIxFun ctx_map_ids . fmap (fmap Free)

      getCt :: (Int,SubExp) -> Maybe (Ext VName, PrimExp (Ext VName))
      getCt (_, Var _) = Nothing
      getCt (i, Constant c) = Just (Ext i, ValueExp c)

      ctx_map_cts = M.fromList $ mapMaybe getCt $
                    zip [0..length ctx_res - 1] ctx_res

      substConstsInExtIndFun :: ExtIxFun -> ExtIxFun
      substConstsInExtIndFun = IxFun.substituteInIxFun (ctx_map_cts<>ctx_map_exts)

      fetchCtx i = case maybeNth i $ zip ctx_res ctx_ts of
                     Nothing -> throwError $ "Cannot find context variable " ++
                                show i ++ " in context results: " ++ pretty ctx_res
                     Just (se, t) -> return (se, t)

      checkReturn (MemPrim x) (MemPrim y)
        | x == y = return ()
      checkReturn (MemMem x) (MemMem y)
        | x == y = return ()
      checkReturn (MemArray x_pt x_shape _ x_ret)
                  (MemArray y_pt y_shape _ y_ret)
        | x_pt == y_pt, shapeRank x_shape == shapeRank y_shape = do
            zipWithM_ checkDim (shapeDims x_shape) (shapeDims y_shape)
            checkMemReturn x_ret y_ret
      checkReturn x y =
        throwError $ unwords ["Expected ", pretty x, " but got ", pretty y]

      checkDim (Free x) y
        | x == y = return ()
        | otherwise = throwError $ unwords ["Expected dim", pretty x,
                                            "but got", pretty y]
      checkDim (Ext i) y = do
        (x, _) <- fetchCtx i
        unless (x == y) $
          throwError $ unwords ["Expected ext dim", pretty i, "=>", pretty x,
                                "but got", pretty y]

      checkMemReturn (ReturnsInBlock x_mem x_ixfun) (ArrayIn y_mem y_ixfun)
          | x_mem == y_mem = do
              let x_ixfun' = substConstsInExtIndFun x_ixfun
                  y_ixfun' = existentialiseIxFun0   y_ixfun
              unless (x_ixfun' == y_ixfun') $
                throwError $ unwords  ["Index function unification failed (ReturnsInBlock)",
                    "\nixfun of body result: ", pretty y_ixfun',
                    "\nixfun of return type: ", pretty x_ixfun',
                    "\nand context elements: ", pretty ctx_res]
      checkMemReturn (ReturnsNewBlock x_space x_ext x_ixfun)
                     (ArrayIn y_mem y_ixfun) = do
        (x_mem, x_mem_type)  <- fetchCtx x_ext
        let x_ixfun' = substConstsInExtIndFun x_ixfun
            y_ixfun' = existentialiseIxFun0   y_ixfun
        unless (x_ixfun' == y_ixfun') $
          throwError $ unwords  ["Index function unification failed (ReturnsNewBlock)",
            "\nixfun of body result: ", pretty y_ixfun',
            "\nixfun of return type: ", pretty x_ixfun',
            "\nand context elements: ", pretty ctx_res]
        case x_mem_type of
          MemMem y_space -> do
            unless (x_mem == Var y_mem) $
              throwError $ unwords ["Expected memory", pretty x_ext, "=>", pretty x_mem,
                                    "but got", pretty y_mem]
            unless (x_space == y_space) $
              throwError $ unwords ["Expected memory", pretty y_mem, "in space", pretty x_space,
                                    "but actually in space", pretty y_space]
          t ->
            throwError $ unwords ["Expected memory", pretty x_ext, "=>", pretty x_mem,
                                  "but but has type", pretty t]
      checkMemReturn x y =
        throwError $ unwords ["Expected array in", pretty x,
                              "but array returned in", pretty y]

      bad :: String -> TC.TypeM lore a
      bad s = TC.bad $ TC.TypeError $
              unlines [ "Return type"
                      , "  " ++ prettyTuple rettype
                      , "cannot match returns of results"
                      , "  " ++ prettyTuple ts
                      , s
                      ]

  either bad return =<< runExceptT (zipWithM_ checkReturn rettype val_ts)

matchPatternToExp :: (ExplicitMemorish lore) =>
                     Pattern (Aliases lore)
                  -> Exp (Aliases lore)
                  -> TC.TypeM lore ()
matchPatternToExp pat e = do
  scope <- asksScope removeScopeAliases
  rt <- runReaderT (expReturns $ removeExpAliases e) scope

  let (ctxs, vals) = bodyReturnsFromPattern $ removePatternAliases pat
      (ctx_ids, _ctx_ts) = unzip ctxs
      (_val_ids, val_ts) = unzip vals
      (ctx_map_ids, ctx_map_exts) =
        getExtMaps $ zip ctx_ids [0..length ctx_ids - 1]

  unless (length val_ts == length rt &&
          and (zipWith (matches ctx_map_ids ctx_map_exts) val_ts rt)) $
    TC.bad $ TC.TypeError $ "Expression type:\n  " ++ prettyTuple rt ++
                            "\ncannot match pattern type:\n  " ++ prettyTuple val_ts ++
                            "\nwith context elements: " ++ pretty ctx_ids
  where matches _ _ (MemPrim x) (MemPrim y) = x == y
        matches _ _ (MemMem x_space) (MemMem y_space) =
          x_space == y_space
        matches ctxids ctxexts (MemArray x_pt x_shape _ x_ret) (MemArray y_pt y_shape _ y_ret) =
          x_pt == y_pt && x_shape == y_shape &&
          case (x_ret, y_ret) of
            (ReturnsInBlock x_mem x_ixfun, Just (ReturnsInBlock y_mem y_ixfun)) ->
              let x_ixfun' = IxFun.substituteInIxFun ctxids  x_ixfun
                  y_ixfun' = IxFun.substituteInIxFun ctxexts y_ixfun
              in  x_mem == y_mem && x_ixfun' == y_ixfun'
            (ReturnsInBlock _ x_ixfun,
             Just (ReturnsNewBlock _ _ y_ixfun)) ->
              let x_ixfun' = IxFun.substituteInIxFun ctxids  x_ixfun
                  y_ixfun' = IxFun.substituteInIxFun ctxexts y_ixfun
              in  x_ixfun' == y_ixfun'
            (ReturnsNewBlock x_space x_i x_ixfun,
             Just (ReturnsNewBlock y_space y_i y_ixfun)) ->
              let x_ixfun' = IxFun.substituteInIxFun  ctxids x_ixfun
                  y_ixfun' = IxFun.substituteInIxFun ctxexts y_ixfun
              in  x_space == y_space && x_i == y_i && x_ixfun' == y_ixfun'
            (_, Nothing) -> True
            _ -> False
        matches _ _ _ _ = False

varMemInfo :: ExplicitMemorish lore =>
              VName -> TC.TypeM lore (MemInfo SubExp NoUniqueness MemBind)
varMemInfo name = do
  attr <- TC.lookupVar name

  case attr of
    LetInfo (_, summary) -> return summary
    FParamInfo summary -> return $ noUniquenessReturns summary
    LParamInfo summary -> return summary
    IndexInfo it -> return $ MemPrim $ IntType it

nameInfoToMemInfo :: ExplicitMemorish lore => NameInfo lore -> MemBound NoUniqueness
nameInfoToMemInfo info =
  case info of
    FParamInfo summary -> noUniquenessReturns summary
    LParamInfo summary -> summary
    LetInfo summary -> summary
    IndexInfo it -> MemPrim $ IntType it

lookupMemInfo :: (HasScope lore m, ExplicitMemorish lore) =>
                  VName -> m (MemInfo SubExp NoUniqueness MemBind)
lookupMemInfo = fmap nameInfoToMemInfo . lookupInfo

subExpMemInfo :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
                 SubExp -> m (MemInfo SubExp NoUniqueness MemBind)
subExpMemInfo (Var v) = lookupMemInfo v
subExpMemInfo (Constant v) = return $ MemPrim $ primValueType v

lookupArraySummary :: (ExplicitMemorish lore, HasScope lore m, Monad m) =>
                      VName -> m (VName, IxFun.IxFun (PrimExp VName))
lookupArraySummary name = do
  summary <- lookupMemInfo name
  case summary of
    MemArray _ _ _ (ArrayIn mem ixfun) ->
      return (mem, ixfun)
    _ ->
      error $ "Variable " ++ pretty name ++ " does not look like an array."

checkMemInfo :: TC.Checkable lore =>
                 VName -> MemInfo SubExp u MemBind
             -> TC.TypeM lore ()
checkMemInfo _ (MemPrim _) = return ()
checkMemInfo _ (MemMem _) = return ()
checkMemInfo name (MemArray _ shape _ (ArrayIn v ixfun)) = do
  t <- lookupType v
  case t of
    Mem{} ->
      return ()
    _        ->
      TC.bad $ TC.TypeError $
      "Variable " ++ pretty v ++
      " used as memory block, but is of type " ++
      pretty t ++ "."

  TC.context ("in index function " ++ pretty ixfun) $ do
    traverse_ (TC.requirePrimExp int32) ixfun
    let ixfun_rank = IxFun.rank ixfun
        ident_rank = shapeRank shape
    unless (ixfun_rank == ident_rank) $
      TC.bad $ TC.TypeError $
      "Arity of index function (" ++ pretty ixfun_rank ++
      ") does not match rank of array " ++ pretty name ++
      " (" ++ show ident_rank ++ ")"

instance Attributes ExplicitMemory where
  expTypesFromPattern = return . map snd . snd . bodyReturnsFromPattern

bodyReturnsFromPattern :: PatternT (MemBound NoUniqueness)
                       -> ([(VName,BodyReturns)], [(VName,BodyReturns)])
bodyReturnsFromPattern pat =
  (map asReturns $ patternContextElements pat,
   map asReturns $ patternValueElements pat)
  where ctx = patternContextElements pat

        ext (Var v)
          | Just (i, _) <- find ((==v) . patElemName . snd) $ zip [0..] ctx =
              Ext i
        ext se = Free se

        asReturns pe =
         (patElemName pe,
          case patElemAttr pe of
            MemPrim pt -> MemPrim pt
            MemMem space -> MemMem space
            MemArray pt shape u (ArrayIn mem ixfun) ->
              MemArray pt (Shape $ map ext $ shapeDims shape) u $
              case find ((==mem) . patElemName . snd) $ zip [0..] ctx  of
                Just (i, PatElem _ (MemMem space)) ->
                  ReturnsNewBlock space i $
                  existentialiseIxFun (map patElemName ctx) ixfun
                _ -> ReturnsInBlock mem $ existentialiseIxFun [] ixfun
         )

instance (PP.Pretty u, PP.Pretty r) => PrettyAnnot (PatElemT (MemInfo SubExp u r)) where
  ppAnnot = bindeeAnnot patElemName patElemAttr

instance (PP.Pretty u, PP.Pretty r) => PrettyAnnot (Param (MemInfo SubExp u r)) where
  ppAnnot = bindeeAnnot paramName paramAttr

instance PrettyLore ExplicitMemory where

bindeeAnnot :: (PP.Pretty u, PP.Pretty r) =>
               (a -> VName) -> (a -> MemInfo SubExp u r)
            -> a -> Maybe PP.Doc
bindeeAnnot bindeeName bindeeLore bindee =
  case bindeeLore bindee of
    attr@MemArray{} ->
      Just $
      PP.text "-- " <>
      PP.oneLine (PP.ppr (bindeeName bindee) <>
                  PP.text " : " <>
                  PP.ppr attr)
    MemMem {} ->
      Nothing
    MemPrim _ ->
      Nothing

extReturns :: [ExtType] -> [ExpReturns]
extReturns ts =
    evalState (mapM addAttr ts) 0
    where addAttr (Prim bt) =
            return $ MemPrim bt
          addAttr (Mem space) =
            return $ MemMem space
          addAttr t@(Array bt shape u)
            | existential t = do
              i <- get <* modify (+1)
              return $ MemArray bt shape u $ Just $
                ReturnsNewBlock DefaultSpace i $
                IxFun.iota $ map convert $ shapeDims shape
            | otherwise =
              return $ MemArray bt shape u Nothing
          convert (Ext i) = LeafExp (Ext i) int32
          convert (Free v) = Free <$> primExpFromSubExp int32 v

arrayVarReturns :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
                   VName
                -> m (PrimType, Shape, VName, IxFun.IxFun (PrimExp VName))
arrayVarReturns v = do
  summary <- lookupMemInfo v
  case summary of
    MemArray et shape _ (ArrayIn mem ixfun) ->
      return (et, Shape $ shapeDims shape, mem, ixfun)
    _ ->
      error $ "arrayVarReturns: " ++ pretty v ++ " is not an array."

varReturns :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
              VName -> m ExpReturns
varReturns v = do
  summary <- lookupMemInfo v
  case summary of
    MemPrim bt ->
      return $ MemPrim bt
    MemArray et shape _ (ArrayIn mem ixfun) ->
      return $ MemArray et (fmap Free shape) NoUniqueness $
               Just $ ReturnsInBlock mem $ existentialiseIxFun [] ixfun
    MemMem space ->
      return $ MemMem space

-- | The return information of an expression.  This can be seen as the
-- "return type with memory annotations" of the expression.
expReturns :: (Monad m, HasScope lore m,
               ExplicitMemorish lore) =>
              Exp lore -> m [ExpReturns]

expReturns (BasicOp (SubExp (Var v))) =
  pure <$> varReturns v

expReturns (BasicOp (Opaque (Var v))) =
  pure <$> varReturns v

expReturns (BasicOp (Repeat outer_shapes inner_shape v)) = do
  t <- repeatDims outer_shapes inner_shape <$> lookupType v
  (et, _, mem, ixfun) <- arrayVarReturns v
  let outer_shapes' = map (map (primExpFromSubExp int32) . shapeDims) outer_shapes
      inner_shape' = map (primExpFromSubExp int32) $ shapeDims inner_shape
  return [MemArray et (Shape $ map Free $ arrayDims t) NoUniqueness $
          Just $ ReturnsInBlock mem $ existentialiseIxFun [] $
          IxFun.repeat ixfun outer_shapes' inner_shape']

expReturns (BasicOp (Reshape newshape v)) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  return [MemArray et (Shape $ map (Free . newDim) newshape) NoUniqueness $
          Just $ ReturnsInBlock mem $ existentialiseIxFun [] $
          IxFun.reshape ixfun $ map (fmap $ primExpFromSubExp int32) newshape]

expReturns (BasicOp (Rearrange perm v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let ixfun' = IxFun.permute ixfun perm
      dims'  = rearrangeShape perm dims
  return [MemArray et (Shape $ map Free dims') NoUniqueness $
          Just $ ReturnsInBlock mem $ existentialiseIxFun [] ixfun']

expReturns (BasicOp (Rotate offsets v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let offsets' = map (primExpFromSubExp int32) offsets
      ixfun' = IxFun.rotate ixfun offsets'
  return [MemArray et (Shape $ map Free dims) NoUniqueness $
          Just $ ReturnsInBlock mem $ existentialiseIxFun [] ixfun']

expReturns (BasicOp (Index v slice)) = do
  info <- sliceInfo v slice
  case info of
    MemArray et shape u (ArrayIn mem ixfun) ->
      return [MemArray et (fmap Free shape) u $
              Just $ ReturnsInBlock mem $ existentialiseIxFun [] ixfun]
    MemPrim pt -> return [MemPrim pt]
    MemMem space -> return [MemMem space]

expReturns (BasicOp (Update v _ _)) =
  pure <$> varReturns v

expReturns (BasicOp op) =
  extReturns . staticShapes <$> primOpType op

expReturns e@(DoLoop ctx val _ _) = do
  t <- expExtType e
  zipWithM typeWithAttr t $ map fst val
    where typeWithAttr t p =
            case (t, paramAttr p) of
              (Array bt shape u, MemArray _ _ _ (ArrayIn mem ixfun))
                | Just (i, mem_p) <- isMergeVar mem,
                  Mem space <- paramType mem_p ->
                    return $ MemArray bt shape u $ Just $ ReturnsNewBlock space i ixfun'
                | otherwise ->
                  return (MemArray bt shape u $
                          Just $ ReturnsInBlock mem ixfun')
                  where ixfun' = existentialiseIxFun (map paramName mergevars) ixfun
              (Array{}, _) ->
                error "expReturns: Array return type but not array merge variable."
              (Prim bt, _) ->
                return $ MemPrim bt
              (Mem{}, _) ->
                error "expReturns: loop returns memory block explicitly."
          isMergeVar v = find ((==v) . paramName . snd) $ zip [0..] mergevars
          mergevars = map fst $ ctx ++ val

expReturns (Apply _ _ ret _) =
  return $ map funReturnsToExpReturns ret

expReturns (If _ _ _ (IfAttr ret _)) =
  return $ map bodyReturnsToExpReturns ret

expReturns (Op op) =
  opReturns op

sliceInfo :: (Monad m, HasScope lore m, ExplicitMemorish lore) =>
             VName
          -> Slice SubExp -> m (MemInfo SubExp NoUniqueness MemBind)
sliceInfo v slice = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  case sliceDims slice of
    [] -> return $ MemPrim et
    dims ->
      return $ MemArray et (Shape dims) NoUniqueness $
      ArrayIn mem $ IxFun.slice ixfun
      (map (fmap (primExpFromSubExp int32)) slice)

class TypedOp (Op lore) => OpReturns lore where
  opReturns :: (Monad m, HasScope lore m) =>
               Op lore -> m [ExpReturns]
  opReturns op = extReturns <$> opType op

segOpReturns :: (Monad m, HasScope ExplicitMemory m) =>
                SegOp ExplicitMemory -> m [ExpReturns]
segOpReturns k@(SegMap _ _ _ kbody) =
  kernelBodyReturns kbody =<< (extReturns <$> opType k)
segOpReturns k@(SegRed _ _ _ _ kbody) =
  kernelBodyReturns kbody =<< (extReturns <$> opType k)
segOpReturns k@(SegScan _ _ _ _ _ kbody) =
  kernelBodyReturns kbody =<< (extReturns <$> opType k)
segOpReturns (SegHist _ _ ops _ _) =
  concat <$> mapM (mapM varReturns . histDest) ops

kernelBodyReturns :: (HasScope ExplicitMemory m, Monad m) =>
                     KernelBody ExplicitMemory -> [ExpReturns] -> m [ExpReturns]
kernelBodyReturns = zipWithM correct . kernelBodyResult
  where correct (WriteReturns _ arr _) _ = varReturns arr
        correct _ ret = return ret

instance OpReturns ExplicitMemory where
  opReturns (Alloc _ space) =
    return [MemMem space]
  opReturns (Inner (SegOp op)) = segOpReturns op
  opReturns k = extReturns <$> opType k

applyFunReturns :: Typed attr =>
                   [FunReturns]
                -> [Param attr]
                -> [(SubExp,Type)]
                -> Maybe [FunReturns]
applyFunReturns rets params args
  | Just _ <- applyRetType rettype params args =
      Just $ map correctDims rets
  | otherwise =
      Nothing
  where rettype = map declExtTypeOf rets
        parammap :: M.Map VName (SubExp, Type)
        parammap = M.fromList $
                   zip (map paramName params) args

        substSubExp (Var v)
          | Just (se,_) <- M.lookup v parammap = se
        substSubExp se = se

        correctDims (MemPrim t) =
          MemPrim t
        correctDims (MemMem space) =
          MemMem space
        correctDims (MemArray et shape u memsummary) =
          MemArray et (correctShape shape) u $
          correctSummary memsummary

        correctShape = Shape . map correctDim . shapeDims
        correctDim (Ext i)   = Ext i
        correctDim (Free se) = Free $ substSubExp se

        correctSummary (ReturnsNewBlock space i ixfun) =
          ReturnsNewBlock space i ixfun
        correctSummary (ReturnsInBlock mem ixfun) =
          -- FIXME: we should also do a replacement in ixfun here.
          ReturnsInBlock mem' ixfun
          where mem' = case M.lookup mem parammap of
                  Just (Var v, _) -> v
                  _               -> mem

-- | Is an array of the given shape stored fully flat row-major with
-- the given index function?
fullyLinear :: (Eq num, IntegralExp num) =>
               ShapeBase num -> IxFun.IxFun num -> Bool
fullyLinear shape ixfun =
  IxFun.isLinear ixfun && ixFunMatchesInnerShape shape ixfun

ixFunMatchesInnerShape :: (Eq num, IntegralExp num) =>
                          ShapeBase num -> IxFun.IxFun num -> Bool
ixFunMatchesInnerShape shape ixfun =
  drop 1 (IxFun.shape ixfun) == drop 1 (shapeDims shape)

-- | Construct the scalar memory space corresponding to a given primitive type.
scalarMemory :: PrimType -> SpaceId
scalarMemory = ("scalar_"++) . pretty

-- | A mapping from all scalar memory spaces to the 'PrimType' they
-- store.
allScalarMemory :: M.Map SpaceId PrimType
allScalarMemory = M.fromList $ zip (map scalarMemory allPrimTypes) allPrimTypes
