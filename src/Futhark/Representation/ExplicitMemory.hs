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
       , InKernel
       , MemOp (..)
       , MemBound (..)
       , MemReturn (..)
       , Returns (..)
       , ExpReturns
       , BodyReturns
       , FunReturns
       , bodyReturnsToExpReturns
       , ExplicitMemorish
       , expReturns
       , bodyReturns
       , returnsToType
       , extReturns
       , lookupMemBound
       , lookupArraySummary
       , fullyDirect
       , ixFunMatchesInnerShape
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , module Futhark.Representation.Kernels.Kernel
       , module Futhark.Representation.Kernels.KernelExp
       )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Foldable (traverse_)
import Data.Maybe
import Data.List
import Data.Monoid
import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Kernels.KernelExp
import qualified Futhark.Analysis.ScalExp as SE

import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import qualified Futhark.Util.Pretty as PP
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Construct (fullSliceNum)
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Aliases
  (Aliases, removeScopeAliases, removeExpAliases, removePatternAliases)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Analysis.Usage

-- | A lore containing explicit memory information.
data ExplicitMemory
data InKernel

type ExplicitMemorish lore = (SameScope lore ExplicitMemory,
                              RetType lore ~ [FunReturns],
                              CanBeAliased (Op lore),
                              Attributes lore, Annotations lore,
                              TypeCheck.Checkable lore,
                              OpReturns lore)

instance IsRetType [FunReturns] where
  retTypeValues = map returnsToType

  primRetType t = [ReturnsScalar t]

  applyRetType = applyFunReturns

data MemOp inner = Alloc SubExp Space
                   -- ^ Allocate a memory block.  This really should not be an
                   -- expression, but what are you gonna do...
                 | Inner inner
            deriving (Eq, Ord, Show)

instance FreeIn inner => FreeIn (MemOp inner) where
  freeIn (Alloc size _) = freeIn size
  freeIn (Inner k) = freeIn k

instance TypedOp inner => TypedOp (MemOp inner) where
  opType (Alloc size space) = pure [Mem size space]
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

instance IsOp inner => IsOp (MemOp inner) where
  safeOp Alloc{} = True
  safeOp (Inner k) = safeOp k

instance UsageInOp inner => UsageInOp (MemOp inner) where
  usageInOp Alloc {} = mempty
  usageInOp (Inner k) = usageInOp k

instance CanBeWise inner => CanBeWise (MemOp inner) where
  type OpWithWisdom (MemOp inner) = MemOp (OpWithWisdom inner)
  removeOpWisdom (Alloc size space) = Alloc size space
  removeOpWisdom (Inner k) = Inner $ removeOpWisdom k

instance Engine.SimplifiableOp lore inner =>
         Engine.SimplifiableOp lore (MemOp inner) where
  simplifyOp (Alloc size space) = Alloc <$> Engine.simplify size <*> pure space
  simplifyOp (Inner k) = Inner <$> Engine.simplifyOp k

instance Annotations ExplicitMemory where
  type LetAttr    ExplicitMemory = MemBound NoUniqueness
  type FParamAttr ExplicitMemory = MemBound Uniqueness
  type LParamAttr ExplicitMemory = MemBound NoUniqueness
  type RetType    ExplicitMemory = [FunReturns]
  type Op         ExplicitMemory = MemOp (Kernel InKernel)

instance Annotations InKernel where
  type LetAttr    InKernel = MemBound NoUniqueness
  type FParamAttr InKernel = MemBound Uniqueness
  type LParamAttr InKernel = MemBound NoUniqueness
  type RetType    InKernel = [FunReturns]
  type Op         InKernel = MemOp (KernelExp InKernel)

-- | A summary of the memory information for every let-bound identifier
-- and function parameter.
data MemBound u = Scalar PrimType
                 -- ^ The corresponding identifier is a
                 -- scalar.  It must not be of array type.
               | MemMem SubExp Space
               | ArrayMem PrimType Shape u VName (IxFun.IxFun SE.ScalExp)
                 -- ^ The array is stored in the named memory block,
                 -- and with the given index function.  The index
                 -- function maps indices in the array to /element/
                 -- offset, /not/ byte offsets!  To translate to byte
                 -- offsets, multiply the offset with the size of the
                 -- array element type.
                deriving (Eq, Show)

instance Functor MemBound where
  fmap _ (Scalar bt) = Scalar bt
  fmap _ (MemMem size space) = MemMem size space
  fmap f (ArrayMem bt shape u mem ixfun) = ArrayMem bt shape (f u) mem ixfun

instance Typed (MemBound Uniqueness) where
  typeOf = fromDecl . declTypeOf

instance Typed (MemBound NoUniqueness) where
  typeOf (Scalar bt) =
    Prim bt
  typeOf (MemMem size space) =
    Mem size space
  typeOf (ArrayMem bt shape u _ _) =
    Array bt shape u

instance DeclTyped (MemBound Uniqueness) where
  declTypeOf (Scalar bt) =
    Prim bt
  declTypeOf (MemMem size space) =
    Mem size space
  declTypeOf (ArrayMem bt shape u _ _) =
    Array bt shape u

instance Ord u => Ord (MemBound u) where
  Scalar bt0 <= Scalar bt1 = bt0 <= bt1
  Scalar _ <= MemMem{} = True
  Scalar _ <= ArrayMem{} = True
  MemMem{} <= Scalar _ = False
  MemMem size0 _ <= MemMem size1 _ = size0 <= size1
  MemMem{} <= ArrayMem{} = True
  ArrayMem x _ _ _ _ <= ArrayMem y _ _ _ _ = x <= y
  ArrayMem{} <= Scalar _ = False
  ArrayMem{} <= MemMem{} = False

instance FreeIn (MemBound u) where
  freeIn (ArrayMem _ shape _ mem ixfun) =
    freeIn shape <> freeIn mem <> freeIn ixfun
  freeIn (MemMem size _) =
    freeIn size
  freeIn (Scalar _) = HS.empty

instance Substitute (MemBound u) where
  substituteNames subst (ArrayMem bt shape u mem f) =
    ArrayMem bt
    (substituteNames subst shape) u
    (substituteNames subst mem)
    (substituteNames subst f)
  substituteNames substs (MemMem size space) =
    MemMem (substituteNames substs size) space
  substituteNames _ (Scalar bt) =
    Scalar bt

instance Rename (MemBound u) where
  rename = substituteRename

instance Engine.Simplifiable (MemBound u) where
  simplify (Scalar bt) =
    return $ Scalar bt
  simplify (MemMem size space) =
    MemMem <$> Engine.simplify size <*> pure space
  simplify (ArrayMem bt shape u mem ixfun) =
    ArrayMem bt shape u <$> Engine.simplify mem <*> pure ixfun

instance PP.Pretty u => PP.Pretty (MemBound u) where
  ppr (Scalar bt) = PP.ppr bt
  ppr (MemMem size space) = PP.ppr (Mem size space :: Type)
  ppr (ArrayMem bt shape u mem ixfun) =
    PP.ppr (Array bt shape u) <> PP.text "@" <>
    PP.ppr mem <> PP.text "->" <> PP.ppr ixfun

instance PP.Pretty (Param (MemBound Uniqueness)) where
  ppr = PP.ppr . fmap declTypeOf

instance PP.Pretty (Param (MemBound NoUniqueness)) where
  ppr = PP.ppr . fmap typeOf

instance PP.Pretty (PatElemT (MemBound NoUniqueness)) where
  ppr = PP.ppr . fmap typeOf

-- | A description of the memory properties of an array being returned
-- by an operation.
data MemReturn = ReturnsInBlock VName (IxFun.IxFun SE.ScalExp)
                 -- ^ The array is located in a memory block that is
                 -- already in scope.
               | ReturnsNewBlock Int (Maybe SubExp)
                 -- ^ The operation returns a new (existential) block,
                 -- with an existential or known size.
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
  substituteNames substs (ReturnsNewBlock i size) =
    ReturnsNewBlock i $ substituteNames substs size

-- | A description of a value being returned from a construct,
-- parametrised across extra information stored for arrays.
data Returns u a = ReturnsArray PrimType ExtShape u a
                   -- ^ Returns an array of the given element type,
                   -- (existential) shape, and uniqueness.
                 | ReturnsScalar PrimType
                   -- ^ Returns a scalar of the given type.
                 | ReturnsMemory SubExp Space
                   -- ^ Returns a memory block of the given size.
                 deriving (Eq, Ord, Show)

instance FreeIn MemReturn where
  freeIn (ReturnsInBlock v ixfun) = freeIn v <> freeIn ixfun
  freeIn _                        = mempty

instance Substitute a => Substitute (Returns u a) where
  substituteNames substs (ReturnsArray bt shape u x) =
    ReturnsArray
    bt
    (substituteNames substs shape)
    u
    (substituteNames substs x)
  substituteNames _ (ReturnsScalar bt) =
    ReturnsScalar bt
  substituteNames substs (ReturnsMemory size space) =
    ReturnsMemory (substituteNames substs size) space

instance PP.Pretty u => PP.Pretty (Returns u (Maybe MemReturn)) where
  ppr ret@(ReturnsArray _ _ _ summary) =
    PP.ppr (returnsToType ret) <> PP.text "@" <> pp summary
    where pp Nothing =
            PP.text "any"
          pp (Just (ReturnsInBlock v ixfun)) =
            PP.parens $ PP.text (pretty v) <> PP.text "->" <> PP.ppr ixfun
          pp (Just (ReturnsNewBlock i (Just size))) =
            PP.text (show i) <> PP.parens (PP.ppr size)
          pp (Just (ReturnsNewBlock i Nothing)) =
            PP.text (show i)
  ppr ret =
    PP.ppr $ returnsToType ret

instance FreeIn a => FreeIn (Returns u a) where
  freeIn (ReturnsScalar _) =
    mempty
  freeIn (ReturnsMemory size _) =
    freeIn size
  freeIn (ReturnsArray _ shape _ summary) =
    freeIn shape <> freeIn summary

instance Substitute a => Rename (Returns u a) where
  rename = substituteRename

instance Engine.Simplifiable a => Engine.Simplifiable (Returns u a) where
  simplify (ReturnsScalar bt) =
    return $ ReturnsScalar bt
  simplify (ReturnsArray bt shape u ret) =
    ReturnsArray bt <$>
    Engine.simplify shape <*>
    pure u <*>
    Engine.simplify ret
  simplify (ReturnsMemory size space) =
    ReturnsMemory <$> Engine.simplify size <*> pure space

instance Engine.Simplifiable MemReturn where
  simplify (ReturnsNewBlock i size) =
    ReturnsNewBlock i <$> Engine.simplify size
  simplify (ReturnsInBlock v ixfun) =
    ReturnsInBlock <$> Engine.simplify v <*> pure ixfun

instance Engine.Simplifiable [FunReturns] where
  simplify = mapM Engine.simplify

instance PP.Pretty (Returns Uniqueness MemReturn) where
  ppr = PP.ppr . funReturnsToExpReturns

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
type ExpReturns = Returns NoUniqueness (Maybe MemReturn)

-- | The return of a body, which must always indicate where
-- returned arrays are located.
type BodyReturns = Returns NoUniqueness MemReturn

-- | The memory return of a function, which must always indicate where
-- returned arrays are located.
type FunReturns = Returns Uniqueness MemReturn

maybeReturns :: Returns u MemReturn -> Returns u (Maybe MemReturn)
maybeReturns (ReturnsArray bt shape u summary) =
  ReturnsArray bt shape u $ Just summary
maybeReturns (ReturnsScalar bt) =
  ReturnsScalar bt
maybeReturns (ReturnsMemory size space) =
  ReturnsMemory size space

noUniquenessReturns :: Returns u r -> Returns NoUniqueness r
noUniquenessReturns (ReturnsArray bt shape _ summary) =
  ReturnsArray bt shape NoUniqueness summary
noUniquenessReturns (ReturnsScalar bt) =
  ReturnsScalar bt
noUniquenessReturns (ReturnsMemory size space) =
  ReturnsMemory size space

funReturnsToExpReturns :: FunReturns -> ExpReturns
funReturnsToExpReturns = noUniquenessReturns . maybeReturns

bodyReturnsToExpReturns :: BodyReturns -> ExpReturns
bodyReturnsToExpReturns = noUniquenessReturns . maybeReturns


-- | Similar to 'generaliseExtTypes', but also generalises the
-- existentiality of memory returns.
generaliseReturns :: (HasScope lore m, Monad m,
                      ExplicitMemorish lore) =>
                     [BodyReturns] -> [BodyReturns] -> m [BodyReturns]
generaliseReturns r1s r2s =
  evalStateT (zipWithM generaliseReturns' r1s r2s) (0, HM.empty, HM.empty)
  where generaliseReturns'
          (ReturnsArray bt shape1 _ summary1)
          (ReturnsArray _  shape2 _ summary2) =
            ReturnsArray bt
            <$>
            (ExtShape <$>
             zipWithM unifyExtDims
             (extShapeDims shape1)
             (extShapeDims shape2))
            <*>
            pure NoUniqueness
            <*>
            generaliseSummaries summary1 summary2
        generaliseReturns' t1 _ =
          return t1 -- Must be prim then.

        unifyExtDims (Free se1) (Free se2)
          | se1 == se2 = return $ Free se1 -- Arbitrary
          | otherwise  = do (i, sizemap, memmap) <- get
                            put (i + 1, sizemap, memmap)
                            return $ Ext i
        unifyExtDims (Ext x) _ = Ext <$> newSize x
        unifyExtDims _ (Ext x) = Ext <$> newSize x

        generaliseSummaries
          (ReturnsInBlock mem1 ixfun1)
          (ReturnsInBlock mem2 ixfun2)
          | mem1 == mem2, ixfun1 == ixfun2 =
            return $ ReturnsInBlock mem1 ixfun1 -- Arbitrary
          | otherwise = do (i, sizemap, memmap) <- get
                           put (i + 1, sizemap, memmap)
                           mem1_info <- lift $ lookupMemBound mem1
                           mem2_info <- lift $ lookupMemBound mem2
                           case (mem1_info, mem2_info) of
                             (MemMem size1 space1, MemMem size2 space2)
                               | size1 == size2, space1 == space2 ->
                                   return $ ReturnsNewBlock i $ Just size1
                             _ ->
                               return $ ReturnsNewBlock i Nothing
        generaliseSummaries
          (ReturnsNewBlock x (Just size_x))
          (ReturnsNewBlock _ (Just size_y))
          | size_x == size_y =
            ReturnsNewBlock <$> newMem x <*> pure (Just size_x)

        generaliseSummaries (ReturnsNewBlock x _) _ =
          ReturnsNewBlock <$> newMem x <*> pure Nothing
        generaliseSummaries _ (ReturnsNewBlock x _) =
          ReturnsNewBlock <$> newMem x <*> pure Nothing

        newSize x = do (i, sizemap, memmap) <- get
                       put (i + 1, HM.insert x i sizemap, memmap)
                       return i
        newMem x = do (i, sizemap, memmap) <- get
                      put (i + 1, sizemap, HM.insert x i memmap)
                      return i

instance TypeCheck.Checkable ExplicitMemory where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore = checkMemBound
  checkLParamLore = checkMemBound
  checkLetBoundLore = checkMemBound
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  checkOp (Alloc size _) = TypeCheck.require [Prim int32] size
  checkOp (Inner k) = TypeCheck.subCheck $ typeCheckKernel k
  primFParam name t = return $ Param name (Scalar t)
  primLParam name t = return $ Param name (Scalar t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType

instance TypeCheck.Checkable InKernel where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore = checkMemBound
  checkLParamLore = checkMemBound
  checkLetBoundLore = checkMemBound
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  checkOp (Alloc size _) = TypeCheck.require [Prim int32] size
  checkOp (Inner k) = typeCheckKernelExp k
  primFParam name t = return $ Param name (Scalar t)
  primLParam name t = return $ Param name (Scalar t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType

matchFunctionReturnType :: (ExplicitMemorish lore) =>
                           Name
                        -> [Returns Uniqueness a]
                        -> Result
                        -> TypeCheck.TypeM lore ()
matchFunctionReturnType fname rettype result = do
  TypeCheck.matchExtReturnType fname (fromDecl <$> ts) result
  mapM_ checkResultSubExp result
  where ts = map returnsToType rettype
        checkResultSubExp Constant{} =
          return ()
        checkResultSubExp (Var v) = do
          attr <- varMemBound v
          case attr of
            Scalar _ -> return ()
            MemMem{} -> return ()
            ArrayMem _ _ _ _ ixfun
              | IxFun.isDirect ixfun ->
                return ()
              | otherwise ->
                  TypeCheck.bad $ TypeCheck.TypeError $
                  "Array " ++ pretty v ++
                  " returned by function, but has nontrivial index function" ++
                  pretty ixfun

matchPatternToExp :: (ExplicitMemorish lore) =>
                     Pattern (Aliases lore)
                  -> Exp (Aliases lore)
                  -> TypeCheck.TypeM lore ()
matchPatternToExp pat e = do
  scope <- asksScope removeScopeAliases
  rt <- runReaderT (expReturns $ removeExpAliases e) scope
  matchPatternToReturns (wrong rt) (removePatternAliases pat) rt
  where wrong rt s = TypeCheck.bad $ TypeCheck.TypeError $
                     ("Pattern\n" ++ TypeCheck.message "  " pat ++
                      "\ncannot match result type\n") ++
                     "  " ++ prettyTuple rt ++ "\n" ++ s

matchPatternToReturns :: Monad m =>
                         (String -> m ())
                      -> Pattern ExplicitMemory
                      -> [ExpReturns]
                      -> m ()
matchPatternToReturns wrong (Pattern ctxbindees valbindees) rt = do
  remaining <- execStateT (zipWithM matchBindee valbindees rt) ctxbindees
  unless (null remaining) $
    wrong $ "Unused parts of pattern: " ++
    intercalate ", " (map pretty remaining)
  where
    inCtx = (`elem` map patElemName ctxbindees)

    matchType bindee t
      | t' == bindeet =
        return ()
      | otherwise =
        lift $ wrong $ "Bindee " ++ pretty bindee ++
        " has type " ++ pretty bindeet ++
        ", but expression returns " ++ pretty t ++ "."
      where bindeet = rankShaped $ patElemRequires bindee
            t'      = rankShaped (t :: ExtType)


    matchBindee bindee (ReturnsScalar bt) =
      matchType bindee $ Prim bt
    matchBindee bindee (ReturnsMemory size@Constant{} space) =
      matchType bindee $ Mem size space
    matchBindee bindee (ReturnsMemory (Var size) space) = do
      popSizeIfInCtx size
      matchType bindee $ Mem (Var size) space
    matchBindee bindee (ReturnsArray et shape _ rets)
      | ArrayMem _ _ _ mem bindeeIxFun <- patElemAttr bindee = do
          case rets of
            Nothing -> return ()
            Just (ReturnsInBlock retmem retIxFun) -> do
              when (mem /= retmem) $
                if inCtx mem then
                  popMemFromCtx mem
                else
                  lift $ wrong $ "Array " ++ pretty bindee ++
                  " returned in memory block " ++ pretty retmem ++
                  " but annotation says block " ++ pretty mem ++
                  "."
              unless (bindeeIxFun == retIxFun) $
                lift $ wrong $ "Bindee index function is:\n  " ++
                pretty bindeeIxFun ++ "\nBut return index function is:\n  " ++
                pretty retIxFun

            Just (ReturnsNewBlock _ _) ->
              popMemFromCtx mem
          zipWithM_ matchArrayDim (arrayDims $ patElemType bindee) $
            extShapeDims shape
          matchType bindee $ Array et shape NoUniqueness
      | otherwise =
        lift $ wrong $ pretty bindee ++
        " is of array type, but has bad memory summary."

    popMemFromCtx name
      | inCtx name = popMem name
      | otherwise =
        lift $ wrong $ "Memory " ++ pretty name ++
        " is supposed to be existential, but not bound in pattern."

    popSizeFromCtx name
      | inCtx name = popSize name
      | otherwise =
        lift $ wrong $ "Size " ++ pretty name ++
        " is supposed to be existential, but not bound in pattern."

    popSizeIfInCtx name
      | inCtx name = popSize name
      | otherwise  = return () -- Must be free, then.

    popSize name = do
      ctxbindees' <- get
      case partition ((==name) . patElemName) ctxbindees' of
        ([nameBindee], ctxbindees'') -> do
          put ctxbindees''
          unless (patElemType nameBindee == Prim int32) $
            lift $ wrong $ "Size " ++ pretty name ++
            " is not an integer."
        _ ->
          return () -- OK, already seen.

    popMem name = do
      ctxbindees' <- get
      case partition ((==name) . patElemName) ctxbindees' of
        ([memBindee], ctxbindees'') -> do
          put ctxbindees''
          case patElemType memBindee of
            Mem (Var size) _ ->
              popSizeIfInCtx size
            Mem Constant{} _ ->
              return ()
            _ ->
              lift $ wrong $ pretty memBindee ++ " is not a memory block."
        _ ->
          return () -- OK, already seen.

    matchArrayDim (Var v) (Free _) =
      popSizeIfInCtx v --  *May* be bound here.
    matchArrayDim (Var v) (Ext _) =
      popSizeFromCtx v --  *Has* to be bound here.
    matchArrayDim Constant{} (Free _) =
      return ()
    matchArrayDim Constant{} (Ext _) =
      lift $ wrong
      "Existential dimension in expression return, but constant in pattern."

varMemBound :: ExplicitMemorish lore =>
               VName -> TypeCheck.TypeM lore (MemBound NoUniqueness)
varMemBound name = do
  attr <- TypeCheck.lookupVar name
  case attr of
    LetInfo (_, summary) -> return summary
    FParamInfo summary -> return $ fmap (const NoUniqueness) summary
    LParamInfo summary -> return summary
    IndexInfo -> return $ Scalar int32

lookupMemBound :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
                  VName -> m (MemBound NoUniqueness)
lookupMemBound name = do
  info <- lookupInfo name
  case info of
    FParamInfo summary -> return $ fmap (const NoUniqueness) summary
    LParamInfo summary -> return summary
    LetInfo summary -> return summary
    IndexInfo -> return $ Scalar int32

lookupArraySummary :: (ExplicitMemorish lore, HasScope lore m, Monad m) =>
                      VName -> m (VName, IxFun.IxFun SE.ScalExp)
lookupArraySummary name = do
  summary <- lookupMemBound name
  case summary of
    ArrayMem _ _ _ mem ixfun ->
      return (mem, ixfun)
    _ ->
      fail $ "Variable " ++ pretty name ++ " does not look like an array."

checkMemBound :: TypeCheck.Checkable lore =>
                 VName -> MemBound u
             -> TypeCheck.TypeM lore ()
checkMemBound _ (Scalar _) = return ()
checkMemBound _ (MemMem size _) =
  TypeCheck.require [Prim int32] size
checkMemBound name (ArrayMem _ shape _ v ixfun) = do
  t <- lookupType v
  case t of
    Mem{} ->
      return ()
    _        ->
      TypeCheck.bad $ TypeCheck.TypeError $
      "Variable " ++ textual v ++
      " used as memory block, but is of type " ++
      pretty t ++ "."

  TypeCheck.context ("in index function " ++ pretty ixfun) $ do
    traverse_ (TypeCheck.requireI [Prim int32]) $ freeIn ixfun
    let ixfun_rank = IxFun.rank ixfun
        ident_rank = shapeRank shape
    unless (ixfun_rank == ident_rank) $
      TypeCheck.bad $ TypeCheck.TypeError $
      "Arity of index function (" ++ pretty ixfun_rank ++
      ") does not match rank of array " ++ pretty name ++
      " (" ++ show ident_rank ++ ")"

instance Attributes ExplicitMemory where
  expContext pat e = do
    ext_context <- expExtContext pat e
    case e of
      If _ tbranch fbranch rettype -> do
        treturns <- bodyReturns rettype tbranch
        freturns <- bodyReturns rettype fbranch
        combreturns <- generaliseReturns treturns freturns
        let ext_mapping =
              returnsMapping pat (map patElemAttr $ patternValueElements pat) combreturns
        return $ map (`HM.lookup` ext_mapping) $ patternContextNames pat
      _ ->
        return ext_context

instance Attributes InKernel where

returnsMapping :: Pattern ExplicitMemory -> [MemBound NoUniqueness] -> [BodyReturns]
               -> HM.HashMap VName SubExp
returnsMapping pat bounds returns =
  mconcat $ zipWith inspect bounds returns
  where ctx = patternContextElements pat
        inspect
          (ArrayMem _ _ _ pat_mem _)
          (ReturnsArray _ _ _ (ReturnsNewBlock _ (Just size)))
            | Just pat_mem_elem <- find ((==pat_mem) . patElemName) ctx,
              Mem (Var pat_mem_elem_size) _ <- patElemType pat_mem_elem,
              Just pat_mem_elem_size_elem <- find ((==pat_mem_elem_size) . patElemName) ctx =
                HM.singleton (patElemName pat_mem_elem_size_elem) size
        inspect _ _ = mempty

instance PP.Pretty u => PrettyAnnot (PatElemT (MemBound u)) where
  ppAnnot = bindeeAnnot patElemName patElemAttr

instance PP.Pretty u => PrettyAnnot (ParamT (MemBound u)) where
  ppAnnot = bindeeAnnot paramName paramAttr

instance PrettyLore ExplicitMemory where
instance PrettyLore InKernel where

bindeeAnnot :: PP.Pretty u =>
               (a -> VName) -> (a -> MemBound u)
            -> a -> Maybe PP.Doc
bindeeAnnot bindeeName bindeeLore bindee =
  case bindeeLore bindee of
    attr@ArrayMem{} ->
      Just $
      PP.text "-- " <>
      PP.oneLine (PP.ppr (bindeeName bindee) <>
                  PP.text " : " <>
                  PP.ppr attr)
    MemMem {} ->
      Nothing
    Scalar _ ->
      Nothing

-- | Convet a 'Returns' to an 'Type' by throwing away memory
-- information.
returnsToType :: Returns u a -> TypeBase ExtShape u
returnsToType (ReturnsScalar bt) =
  Prim bt
returnsToType (ReturnsMemory size space) =
  Mem size space
returnsToType (ReturnsArray bt shape u _) =
  Array bt shape u

extReturns :: [ExtType] -> [ExpReturns]
extReturns ts =
    evalState (mapM addAttr ts) 0
    where addAttr (Prim bt) =
            return $ ReturnsScalar bt
          addAttr (Mem size space) =
            return $ ReturnsMemory size space
          addAttr t@(Array bt shape u)
            | existential t = do
              i <- get
              put $ i + 1
              return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i Nothing
            | otherwise =
              return $ ReturnsArray bt shape u Nothing

arrayVarReturns :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
                   VName
                -> m (PrimType, Shape, VName, IxFun.IxFun SE.ScalExp)
arrayVarReturns v = do
  summary <- lookupMemBound v
  case summary of
    ArrayMem et shape _ mem ixfun ->
      return (et, Shape $ shapeDims shape, mem, ixfun)
    _ ->
      fail $ "arrayVarReturns: " ++ pretty v ++ " is not an array."

varReturns :: (HasScope lore m, Monad m, ExplicitMemorish lore, Attributes lore) =>
              VName -> m ExpReturns
varReturns v = do
  summary <- lookupMemBound v
  case summary of
    Scalar bt ->
      return $ ReturnsScalar bt
    ArrayMem et shape _ mem ixfun ->
      return $ ReturnsArray et (ExtShape $ map Free $ shapeDims shape) NoUniqueness $
              Just $ ReturnsInBlock mem ixfun
    MemMem size space ->
      return $ ReturnsMemory size space

-- | The return information of an expression.  This can be seen as the
-- "return type with memory annotations" of the expression.
expReturns :: (Monad m, HasScope lore m,
               ExplicitMemorish lore) =>
              Exp lore -> m [ExpReturns]

expReturns (BasicOp (SubExp (Var v))) = do
  r <- varReturns v
  return [r]

expReturns (BasicOp (Reshape _ newshape v)) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  return [ReturnsArray et (ExtShape $ map (Free . newDim) newshape) NoUniqueness $
          Just $ ReturnsInBlock mem $
          IxFun.reshape ixfun $ map (fmap SE.intSubExpToScalExp) newshape]

expReturns (BasicOp (Rearrange _ perm v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let ixfun' = IxFun.permute ixfun perm
      dims'  = rearrangeShape perm dims
  return [ReturnsArray et (ExtShape $ map Free dims') NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns (BasicOp (Rotate _ offsets v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let offsets' = map (`SE.subExpToScalExp` int32) offsets
      ixfun' = IxFun.rotate ixfun offsets'
  return [ReturnsArray et (ExtShape $ map Free dims) NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns (BasicOp (Split _ i sizeexps v)) = do
  (et, shape, mem, ixfun) <- arrayVarReturns v
  let newShapes = map (shape `setOuterDim`) sizeexps
      offsets =  0 : scanl1 (+) (map (`SE.subExpToScalExp` int32) sizeexps)
      r = shapeRank shape
      perm = [i] ++ [0..i-1] ++ [i+1..r-1]
      perm_inv = rearrangeInverse perm
  return $ zipWith (\new_shape offset
                    -> ReturnsArray et (ExtShape $ map Free $ shapeDims new_shape)
                       NoUniqueness $
                       Just $ ReturnsInBlock mem $
                       IxFun.permute
                       (IxFun.offsetIndex (IxFun.permute ixfun perm) offset)
                       perm_inv)
           newShapes offsets

expReturns (BasicOp (Index _ v slice)) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  case sliceDims slice of
    []     ->
      return [ReturnsScalar et]
    dims ->
      return [ReturnsArray et (ExtShape $ map Free dims) NoUniqueness $
             Just $ ReturnsInBlock mem $
             IxFun.slice ixfun
             (map (fmap (`SE.subExpToScalExp` int32)) slice)]

expReturns (BasicOp op) =
  extReturns . staticShapes <$> primOpType op

expReturns (DoLoop ctx val _ _) =
    return $
    evalState (zipWithM typeWithAttr
               (loopExtType (map (paramIdent . fst) ctx) (map (paramIdent . fst) val)) $
               map fst val) 0
    where typeWithAttr t p =
            case (t, paramAttr p) of
              (Array bt shape u, ArrayMem _ _ _ mem ixfun)
                | isMergeVar mem -> do
                  i <- get
                  modify succ
                  return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i Nothing
                | otherwise ->
                  return (ReturnsArray bt shape u $
                          Just $ ReturnsInBlock mem ixfun)
              (Array{}, _) ->
                fail "expReturns: Array return type but not array merge variable."
              (Prim bt, _) ->
                return $ ReturnsScalar bt
              (Mem{}, _) ->
                fail "expReturns: loop returns memory block explicitly."
          isMergeVar = flip elem $ map paramName mergevars
          mergevars = map fst $ ctx ++ val

expReturns (Apply _ _ ret) =
  return $ map funReturnsToExpReturns ret

expReturns (If _ b1 b2 ts) = do
  b1t <- bodyReturns ts b1
  b2t <- bodyReturns ts b2
  map bodyReturnsToExpReturns <$>
    generaliseReturns b1t b2t

expReturns (Op op) =
  opReturns op

class TypedOp (Op lore) => OpReturns lore where
  opReturns :: (Monad m, HasScope lore m) =>
               Op lore -> m [ExpReturns]
  opReturns op = extReturns <$> opType op

instance OpReturns ExplicitMemory where
  opReturns (Alloc size space) =
    return [ReturnsMemory size space]
  opReturns (Inner k@(Kernel _ _ _ body)) =
    zipWithM correct (kernelBodyResult body) =<< (extReturns <$> opType k)
    where correct (WriteReturn _ arr _ _) _ = varReturns arr
          correct _ ret = return ret
  opReturns k =
    extReturns <$> opType k

instance OpReturns InKernel where
  opReturns (Alloc size space) =
    return [ReturnsMemory size space]
  opReturns (Inner (SplitArray o _ thread_id num_threads elems_per_thread arrs)) =
    forM arrs $ \arr -> do
      arr_t <- lookupType arr
      (mem, ixfun) <- lookupArraySummary arr
      let row_dims = drop 1 $ arrayDims arr_t
          bt = elemType arr_t
          num_threads' = SE.intSubExpToScalExp num_threads
          elems_per_thread' = SE.intSubExpToScalExp elems_per_thread
          ext_shape = ExtShape $ Ext 0 : map Free row_dims
      return $ ReturnsArray bt ext_shape NoUniqueness $ Just $ ReturnsInBlock mem $
        case o of
          InOrder ->
            let newshape = [DimNew num_threads', DimNew elems_per_thread'] ++
                           map (DimNew . SE.intSubExpToScalExp) row_dims
            in IxFun.slice (IxFun.reshape ixfun newshape) $
               fullSliceNum (newDims newshape) [DimFix $ SE.intSubExpToScalExp thread_id]
          Disorder ->
            let newshape = [DimNew elems_per_thread', DimNew num_threads'] ++
                           map (DimNew . SE.intSubExpToScalExp) row_dims
                perm = [1,0] ++ [2..IxFun.rank ixfun]
            in IxFun.slice
               (IxFun.permute (IxFun.reshape ixfun newshape) perm) $
               fullSliceNum (rearrangeShape perm $ newDims newshape)
               [DimFix $ SE.intSubExpToScalExp thread_id]

  opReturns (Inner (GroupStream _ _ lam _ _)) =
    forM (groupStreamAccParams lam) $ \param ->
      case paramAttr param of
        Scalar bt ->
          return $ ReturnsScalar bt
        ArrayMem et shape _ mem ixfun ->
          return $ ReturnsArray et (ExtShape $ map Free $ shapeDims shape) NoUniqueness $
          Just $ ReturnsInBlock mem ixfun
        MemMem size space ->
          return $ ReturnsMemory size space
  opReturns k =
    extReturns <$> opType k

-- | The return information of a body.  This can be seen as the
-- "return type with memory annotations" of the body.
bodyReturns :: (Monad m, HasScope lore m, ExplicitMemorish lore) =>
               [ExtType] -> Body lore
            -> m [BodyReturns]
bodyReturns ts (Body _ bnds res) = do
  let boundHere = boundInBindings bnds
      inspect _ (Constant val) =
        return $ ReturnsScalar $ primValueType val
      inspect (Prim bt) (Var _) =
        return $ ReturnsScalar bt
      inspect Mem{} (Var _) =
        -- FIXME
        fail "bodyReturns: cannot handle bodies returning memory yet."
      inspect (Array et shape u) (Var v) = do

        memsummary <- do
          summary <- case HM.lookup v boundHere of
            Nothing -> lift $ lookupMemBound v
            Just bindee -> return $ patElemAttr bindee

          case summary of
            Scalar _ ->
              fail "bodyReturns: inconsistent memory summary"
            MemMem{} ->
              fail "bodyReturns: inconsistent memory summary"

            ArrayMem _ _ NoUniqueness mem ixfun
              | mem `HM.member` boundHere -> do
                (i, memmap) <- get

                case HM.lookup mem memmap of
                  Nothing -> do
                    put (i+1, HM.insert mem (i+1) memmap)
                    return $ ReturnsNewBlock i Nothing

                  Just _ ->
                    fail "bodyReturns: same memory block used multiple times."
              | otherwise ->
                  return $ ReturnsInBlock mem ixfun
        return $ ReturnsArray et shape u memsummary
  evalStateT (zipWithM inspect ts res)
    (0, HM.empty)

boundInBindings :: [Binding lore] -> HM.HashMap VName (PatElem lore)
boundInBindings [] = HM.empty
boundInBindings (bnd:bnds) =
  boundInBinding `HM.union` boundInBindings bnds
  where boundInBinding =
          HM.fromList
          [ (patElemName bindee, bindee)
          | bindee <- patternElements $ bindingPattern bnd
          ]

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
  where rettype = ExtRetType $ map returnsToType rets
        parammap :: HM.HashMap VName (SubExp, Type)
        parammap = HM.fromList $
                   zip (map paramName params) args

        substSubExp (Var v)
          | Just (se,_) <- HM.lookup v parammap = se
        substSubExp se = se

        correctDims (ReturnsScalar t) =
          ReturnsScalar t
        correctDims (ReturnsMemory se space) =
          ReturnsMemory (substSubExp se) space
        correctDims (ReturnsArray et shape u memsummary) =
          ReturnsArray et (correctExtShape shape) u $
          correctSummary memsummary

        correctExtShape = ExtShape . map correctDim . extShapeDims
        correctDim (Ext i)   = Ext i
        correctDim (Free se) = Free $ substSubExp se

        correctSummary (ReturnsNewBlock i size) =
          ReturnsNewBlock i size
        correctSummary (ReturnsInBlock mem ixfun) =
          -- FIXME: we should also do a replacement in ixfun here.
          ReturnsInBlock mem' ixfun
          where mem' = case HM.lookup mem parammap of
                  Just (Var v, _) -> v
                  _               -> mem

-- | Is an array of the given shape stored fully flat row-major with
-- the given index function?
fullyDirect :: Shape -> IxFun.IxFun SE.ScalExp -> Bool
fullyDirect shape ixfun =
  IxFun.isDirect ixfun && ixFunMatchesInnerShape shape ixfun

ixFunMatchesInnerShape :: Shape -> IxFun.IxFun SE.ScalExp -> Bool
ixFunMatchesInnerShape shape ixfun =
  drop 1 (IxFun.shape ixfun) == drop 1 shape'
  where shape' = map SE.intSubExpToScalExp $ shapeDims shape
