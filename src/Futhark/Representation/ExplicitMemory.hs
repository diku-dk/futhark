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
       , InKernel
       , MemOp (..)
       , MemInfo (..)
       , MemBound
       , MemBind (..)
       , MemReturn (..)
       , IxFun
       , ExpReturns
       , BodyReturns
       , FunReturns
       , noUniquenessReturns
       , bodyReturnsToExpReturns
       , ExplicitMemorish
       , expReturns
       , bodyReturns
       , extReturns
       , lookupMemInfo
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
       , module Futhark.Analysis.PrimExp.Convert
       )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Foldable (traverse_)
import Data.Maybe
import Data.List
import Data.Monoid
import Prelude

import Futhark.Analysis.Metrics
import Futhark.Representation.AST.Syntax
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Kernels.KernelExp
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.PrimExp.Simplify
import qualified Futhark.Util.Pretty as PP
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Aliases
  (Aliases, removeScopeAliases, removeExpAliases, removePatternAliases)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Analysis.Usage
import qualified Futhark.Analysis.SymbolTable as ST

-- | A lore containing explicit memory information.
data ExplicitMemory
data InKernel

type ExplicitMemorish lore = (SameScope lore ExplicitMemory,
                              RetType lore ~ FunReturns,
                              CanBeAliased (Op lore),
                              Attributes lore, Annotations lore,
                              TypeCheck.Checkable lore,
                              OpReturns lore)

instance IsRetType FunReturns where
  primRetType = MemPrim

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

instance OpMetrics inner => OpMetrics (MemOp inner) where
  opMetrics Alloc{} = seen "Alloc"
  opMetrics (Inner k) = opMetrics k

instance IsOp inner => IsOp (MemOp inner) where
  safeOp Alloc{} = True
  safeOp (Inner k) = safeOp k
  cheapOp (Inner k) = cheapOp k
  cheapOp Alloc{} = True

instance UsageInOp inner => UsageInOp (MemOp inner) where
  usageInOp Alloc {} = mempty
  usageInOp (Inner k) = usageInOp k

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
  type Op         ExplicitMemory = MemOp (Kernel InKernel)

instance Annotations InKernel where
  type LetAttr    InKernel = MemInfo SubExp NoUniqueness MemBind
  type FParamAttr InKernel = MemInfo SubExp Uniqueness MemBind
  type LParamAttr InKernel = MemInfo SubExp NoUniqueness MemBind
  type RetType    InKernel = FunReturns
  type Op         InKernel = MemOp (KernelExp InKernel)

-- | The index function representation used for memory annotations.
type IxFun = IxFun.IxFun (PrimExp VName)

-- | A summary of the memory information for every let-bound
-- identifier, function parameter, and return value.  Parameterisered
-- over uniqueness, dimension, and auxiliary array information.
data MemInfo d u ret = MemPrim PrimType
                     -- ^ A primitive value.
                     | MemMem d Space
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

instance DeclExtTyped (MemInfo ExtDimSize Uniqueness ret) where
  declExtTypeOf (MemPrim pt) = Prim pt
  declExtTypeOf (MemMem (Free size) space) = Mem size space
  declExtTypeOf (MemMem Ext{} space) = Mem (intConst Int32 0) space -- XXX
  declExtTypeOf (MemArray pt shape u _) = Array pt shape u

instance Typed (MemInfo SubExp Uniqueness ret) where
  typeOf = fromDecl . declTypeOf

instance Typed (MemInfo SubExp NoUniqueness ret) where
  typeOf (MemPrim bt) =
    Prim bt
  typeOf (MemMem size space) =
    Mem size space
  typeOf (MemArray bt shape u _) =
    Array bt shape u

instance DeclTyped (MemInfo SubExp Uniqueness ret) where
  declTypeOf (MemPrim bt) =
    Prim bt
  declTypeOf (MemMem size space) =
    Mem size space
  declTypeOf (MemArray bt shape u _) =
    Array bt shape u

instance (FreeIn d, FreeIn ret) => FreeIn (MemInfo d u ret) where
  freeIn (MemArray _ shape _ ret) =
    freeIn shape <> freeIn ret
  freeIn (MemMem size _) =
    freeIn size
  freeIn (MemPrim _) =
    mempty

instance (Substitute d, Substitute ret) => Substitute (MemInfo d u ret) where
  substituteNames subst (MemArray bt shape u ret) =
    MemArray bt
    (substituteNames subst shape) u
    (substituteNames subst ret)
  substituteNames substs (MemMem size space) =
    MemMem (substituteNames substs size) space
  substituteNames _ (MemPrim bt) =
    MemPrim bt

instance (Substitute d, Substitute ret) => Rename (MemInfo d u ret) where
  rename = substituteRename

simplifyIxFun :: Engine.SimplifiableLore lore =>
                 IxFun.IxFun (PrimExp VName) -> Engine.SimpleM lore (IxFun.IxFun (PrimExp VName))
simplifyIxFun = traverse simplifyPrimExp

instance (Engine.Simplifiable d, Engine.Simplifiable ret) =>
         Engine.Simplifiable (MemInfo d u ret) where
  simplify (MemPrim bt) =
    return $ MemPrim bt
  simplify (MemMem size space) =
    MemMem <$> Engine.simplify size <*> pure space
  simplify (MemArray bt shape u ret) =
    MemArray bt <$> Engine.simplify shape <*> pure u <*> Engine.simplify ret

instance (PP.Pretty (TypeBase (ShapeBase d) u),
          PP.Pretty d, PP.Pretty u, PP.Pretty ret) => PP.Pretty (MemInfo d u ret) where
  ppr (MemPrim bt) = PP.ppr bt
  ppr (MemMem s DefaultSpace) =
    PP.text "mem" <> PP.parens (PP.ppr s)
  ppr (MemMem s (Space sp)) =
    PP.text "mem" <> PP.parens (PP.ppr s) <> PP.text "@" <> PP.text sp
  ppr (MemArray bt shape u ret) =
    PP.ppr (Array bt shape u) <> PP.ppr ret

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
  freeIn (ArrayIn mem ixfun) = freeIn mem <> freeIn ixfun

-- | A description of the memory properties of an array being returned
-- by an operation.
data MemReturn = ReturnsInBlock VName IxFun
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

instance PP.Pretty MemReturn where
  ppr (ReturnsInBlock v ixfun) =
    PP.parens $ PP.text (pretty v) <> PP.text "->" <> PP.ppr ixfun
  ppr (ReturnsNewBlock i (Just size)) =
    PP.text (show i) <> PP.parens (PP.ppr size)
  ppr (ReturnsNewBlock i Nothing) =
    PP.text (show i)

instance FreeIn MemReturn where
  freeIn (ReturnsInBlock v ixfun) = freeIn v <> freeIn ixfun
  freeIn _                        = mempty

instance Engine.Simplifiable MemReturn where
  simplify (ReturnsNewBlock i size) =
    ReturnsNewBlock i <$> Engine.simplify size
  simplify (ReturnsInBlock v ixfun) =
    ReturnsInBlock <$> Engine.simplify v <*> simplifyIxFun ixfun

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
type ExpReturns = MemInfo ExtDimSize NoUniqueness (Maybe MemReturn)

-- | The return of a body, which must always indicate where
-- returned arrays are located.
type BodyReturns = MemInfo ExtDimSize NoUniqueness MemReturn

-- | The memory return of a function, which must always indicate where
-- returned arrays are located.
type FunReturns = MemInfo ExtDimSize Uniqueness MemReturn

maybeReturns :: MemInfo d u r -> MemInfo d u (Maybe r)
maybeReturns (MemArray bt shape u ret) =
  MemArray bt shape u $ Just ret
maybeReturns (MemPrim bt) =
  MemPrim bt
maybeReturns (MemMem size space) =
  MemMem size space

noUniquenessReturns :: MemInfo d u r -> MemInfo d NoUniqueness r
noUniquenessReturns (MemArray bt shape _ r) =
  MemArray bt shape NoUniqueness r
noUniquenessReturns (MemPrim bt) =
  MemPrim bt
noUniquenessReturns (MemMem size space) =
  MemMem size space

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
  evalStateT (zipWithM generaliseReturns' r1s r2s) (0, M.empty, M.empty)
  where generaliseReturns'
          (MemArray bt shape1 _ summary1)
          (MemArray _  shape2 _ summary2) =
            MemArray bt
            <$>
            (Shape <$>
             zipWithM unifyExtDims
             (shapeDims shape1)
             (shapeDims shape2))
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
                           mem1_info <- lift $ lookupMemInfo mem1
                           mem2_info <- lift $ lookupMemInfo mem2
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
                       put (i + 1, M.insert x i sizemap, memmap)
                       return i
        newMem x = do (i, sizemap, memmap) <- get
                      put (i + 1, sizemap, M.insert x i memmap)
                      return i

instance TypeCheck.Checkable ExplicitMemory where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore = checkMemInfo
  checkLParamLore = checkMemInfo
  checkLetBoundLore = checkMemInfo
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  checkOp (Alloc size _) = TypeCheck.require [Prim int64] size
  checkOp (Inner k) = TypeCheck.subCheck $ typeCheckKernel k
  primFParam name t = return $ Param name (MemPrim t)
  primLParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType

instance TypeCheck.Checkable InKernel where
  checkExpLore = return
  checkBodyLore = return
  checkFParamLore = checkMemInfo
  checkLParamLore = checkMemInfo
  checkLetBoundLore = checkMemInfo
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  checkOp (Alloc size _) = TypeCheck.require [Prim int64] size
  checkOp (Inner k) = typeCheckKernelExp k
  primFParam name t = return $ Param name (MemPrim t)
  primLParam name t = return $ Param name (MemPrim t)
  matchPattern = matchPatternToExp
  matchReturnType = matchFunctionReturnType

matchFunctionReturnType :: (ExplicitMemorish lore) =>
                           Name
                        -> [MemInfo ExtDimSize Uniqueness r]
                        -> Result
                        -> TypeCheck.TypeM lore ()
matchFunctionReturnType fname rettype result = do
  TypeCheck.matchExtReturnType fname (fromDecl <$> ts) result
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
              | IxFun.isDirect ixfun ->
                return ()
              | otherwise ->
                  TypeCheck.bad $ TypeCheck.TypeError $
                  "Array " ++ pretty v ++
                  " returned by function, but has nontrivial index function " ++
                  pretty ixfun ++ " " ++ show ixfun

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
  remaining <- execStateT (zipWithM (matchBindeeEntry wrong ctxbindees) valbindees rt) ctxbindees
  unless (null remaining) $
    wrong $ "Unused parts of pattern: " ++
    intercalate ", " (map pretty remaining)

matchBindeeEntry :: (PP.Pretty (PatElemT (MemInfo d u MemBind)),
                     PP.Pretty (PatElemT attr),
                     Typed (MemInfo d u MemBind), Typed attr,
                     MonadTrans t, Monad m, MonadState [PatElemT attr] (t m))
                 => (String -> m ())
                 -> [PatElemT attr]
                 -> PatElemT (MemInfo d u MemBind)
                 -> ExpReturns
                 -> t m ()
matchBindeeEntry wrong ctxbindees = matchBindee
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

    matchBindee bindee (MemPrim bt) =
      matchType bindee $ Prim bt
    matchBindee bindee (MemMem (Free size@Constant{}) space) =
      matchType bindee $ Mem size space
    matchBindee bindee (MemMem Ext{} space) =
      matchType bindee $ Mem (intConst Int64 0) space
    matchBindee bindee (MemMem (Free (Var size)) space) = do
      popSizeIfInCtx int64 size
      matchType bindee $ Mem (Var size) space
    matchBindee bindee (MemArray et shape _ rets)
      | MemArray _ _ _ (ArrayIn mem bindeeIxFun) <- patElemAttr bindee = do
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
            shapeDims shape
          matchType bindee $ Array et shape NoUniqueness
      | otherwise =
        lift $ wrong $ pretty bindee ++
        " is of array type, but has bad memory summary."

    popMemFromCtx name
      | inCtx name = popMem name
      | otherwise =
        lift $ wrong $ "Memory " ++ pretty name ++
        " is supposed to be existential, but not bound in pattern."

    popSizeFromCtx t name
      | inCtx name = popSize t name
      | otherwise =
        lift $ wrong $ "Size " ++ pretty name ++
        " is supposed to be existential, but not bound in pattern."

    popSizeIfInCtx t name
      | inCtx name = popSize t name
      | otherwise  = return () -- Must be free, then.

    popSize t name = do
      ctxbindees' <- get
      case partition ((==name) . patElemName) ctxbindees' of
        ([nameBindee], ctxbindees'') -> do
          put ctxbindees''
          unless (patElemType nameBindee == Prim t) $
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
              popSizeIfInCtx int64 size
            Mem Constant{} _ ->
              return ()
            _ ->
              lift $ wrong $ pretty memBindee ++ " is not a memory block."
        _ ->
          return () -- OK, already seen.

    matchArrayDim (Var v) (Free _) =
      popSizeIfInCtx int32 v --  *May* be bound here.
    matchArrayDim (Var v) (Ext _) =
      popSizeFromCtx int32 v --  *Has* to be bound here.
    matchArrayDim Constant{} (Free _) =
      return ()
    matchArrayDim Constant{} (Ext _) =
      lift $ wrong
      "Existential dimension in expression return, but constant in pattern."

varMemInfo :: ExplicitMemorish lore =>
              VName -> TypeCheck.TypeM lore (MemInfo SubExp NoUniqueness MemBind)
varMemInfo name = do
  attr <- TypeCheck.lookupVar name
  case attr of
    LetInfo (_, summary) -> return summary
    FParamInfo summary -> return $ noUniquenessReturns summary
    LParamInfo summary -> return summary
    IndexInfo it -> return $ MemPrim $ IntType it

lookupMemInfo :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
                  VName -> m (MemInfo SubExp NoUniqueness MemBind)
lookupMemInfo name = do
  info <- lookupInfo name
  case info of
    FParamInfo summary -> return $ noUniquenessReturns summary
    LParamInfo summary -> return summary
    LetInfo summary -> return summary
    IndexInfo it -> return $ MemPrim $ IntType it

lookupArraySummary :: (ExplicitMemorish lore, HasScope lore m, Monad m) =>
                      VName -> m (VName, IxFun.IxFun (PrimExp VName))
lookupArraySummary name = do
  summary <- lookupMemInfo name
  case summary of
    MemArray _ _ _ (ArrayIn mem ixfun) ->
      return (mem, ixfun)
    _ ->
      fail $ "Variable " ++ pretty name ++ " does not look like an array."

checkMemInfo :: TypeCheck.Checkable lore =>
                 VName -> MemInfo SubExp u MemBind
             -> TypeCheck.TypeM lore ()
checkMemInfo _ (MemPrim _) = return ()
checkMemInfo _ (MemMem size _) =
  TypeCheck.require [Prim int64] size
checkMemInfo name (MemArray _ shape _ (ArrayIn v ixfun)) = do
  t <- lookupType v
  case t of
    Mem{} ->
      return ()
    _        ->
      TypeCheck.bad $ TypeCheck.TypeError $
      "Variable " ++ pretty v ++
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
      If _ tbranch fbranch (IfAttr rettype _) -> do
        treturns <- bodyReturns rettype tbranch
        freturns <- bodyReturns rettype fbranch
        combreturns <- generaliseReturns treturns freturns
        let ext_mapping =
              returnsMapping pat (map patElemAttr $ patternValueElements pat) combreturns
        return $ map (`M.lookup` ext_mapping) $ patternContextNames pat
      _ ->
        return ext_context

instance Attributes InKernel where

returnsMapping :: Pattern ExplicitMemory
               -> [MemInfo SubExp NoUniqueness MemBind] -> [BodyReturns]
               -> M.Map VName SubExp
returnsMapping pat bounds returns =
  mconcat $ zipWith inspect bounds returns
  where ctx = patternContextElements pat
        inspect
          (MemArray _ _ _ (ArrayIn pat_mem _))
          (MemArray _ _ _ (ReturnsNewBlock _ (Just size)))
            | Just pat_mem_elem <- find ((==pat_mem) . patElemName) ctx,
              Mem (Var pat_mem_elem_size) _ <- patElemType pat_mem_elem,
              Just pat_mem_elem_size_elem <- find ((==pat_mem_elem_size) . patElemName) ctx =
                M.singleton (patElemName pat_mem_elem_size_elem) size
        inspect _ _ = mempty

instance (PP.Pretty u, PP.Pretty r) => PrettyAnnot (PatElemT (MemInfo SubExp u r)) where
  ppAnnot = bindeeAnnot patElemName patElemAttr

instance (PP.Pretty u, PP.Pretty r) => PrettyAnnot (ParamT (MemInfo SubExp u r)) where
  ppAnnot = bindeeAnnot paramName paramAttr

instance PrettyLore ExplicitMemory where
instance PrettyLore InKernel where

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
          addAttr (Mem size space) =
            return $ MemMem (Free size) space
          addAttr t@(Array bt shape u)
            | existential t = do
              i <- get
              put $ i + 1
              return $ MemArray bt shape u $ Just $ ReturnsNewBlock i Nothing
            | otherwise =
              return $ MemArray bt shape u Nothing

arrayVarReturns :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
                   VName
                -> m (PrimType, Shape, VName, IxFun.IxFun (PrimExp VName))
arrayVarReturns v = do
  summary <- lookupMemInfo v
  case summary of
    MemArray et shape _ (ArrayIn mem ixfun) ->
      return (et, Shape $ shapeDims shape, mem, ixfun)
    _ ->
      fail $ "arrayVarReturns: " ++ pretty v ++ " is not an array."

varReturns :: (HasScope lore m, Monad m, ExplicitMemorish lore) =>
              VName -> m ExpReturns
varReturns v = do
  summary <- lookupMemInfo v
  case summary of
    MemPrim bt ->
      return $ MemPrim bt
    MemArray et shape _ (ArrayIn mem ixfun) ->
      return $ MemArray et (fmap Free shape) NoUniqueness $
               Just $ ReturnsInBlock mem ixfun
    MemMem size space ->
      return $ MemMem (Free size) space

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
  t <- modifyArrayShape repeatDims <$> lookupType v
  (et, _, mem, ixfun) <- arrayVarReturns v
  let outer_shapes' = map (map (primExpFromSubExp int32) . shapeDims) outer_shapes
      inner_shape' = map (primExpFromSubExp int32) $ shapeDims inner_shape
  return [MemArray et (Shape $ map Free $ arrayDims t) NoUniqueness $
          Just $ ReturnsInBlock mem $
          IxFun.repeat ixfun outer_shapes' inner_shape']
  where repeatDims (Shape ds) =
          Shape $ concat (zipWith (++) (map shapeDims outer_shapes) (map pure ds)) ++
          shapeDims inner_shape

expReturns (BasicOp (Reshape newshape v)) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  return [MemArray et (Shape $ map (Free . newDim) newshape) NoUniqueness $
          Just $ ReturnsInBlock mem $
          IxFun.reshape ixfun $ map (fmap $ primExpFromSubExp int32) newshape]

expReturns (BasicOp (Rearrange perm v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let ixfun' = IxFun.permute ixfun perm
      dims'  = rearrangeShape perm dims
  return [MemArray et (Shape $ map Free dims') NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns (BasicOp (Rotate offsets v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns v
  let offsets' = map (primExpFromSubExp int32) offsets
      ixfun' = IxFun.rotate ixfun offsets'
  return [MemArray et (Shape $ map Free dims) NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns (BasicOp (Split i sizeexps v)) = do
  (et, shape, mem, ixfun) <- arrayVarReturns v
  let offsets =  0 : scanl1 (+) (map (primExpFromSubExp int32) sizeexps)
      dims = map (primExpFromSubExp int32) $ shapeDims shape
      mkSlice offset n = map (unitSlice 0) (take i dims) ++
                         [unitSlice offset n] ++
                         map (unitSlice 0) (drop (i+1) dims)
  return $ zipWith (\offset dim ->
                      let new_shape = setDim i shape dim
                      in MemArray et (Shape $ map Free $ shapeDims new_shape)
                         NoUniqueness $ Just $ ReturnsInBlock mem $
                         IxFun.slice ixfun $ mkSlice offset $
                         primExpFromSubExp int32 dim)
    offsets sizeexps

expReturns (BasicOp (Index v slice)) = do
  (et, _, mem, ixfun) <- arrayVarReturns v
  case sliceDims slice of
    []     ->
      return [MemPrim et]
    dims ->
      return [MemArray et (Shape $ map Free dims) NoUniqueness $
             Just $ ReturnsInBlock mem $
             IxFun.slice ixfun
             (map (fmap (primExpFromSubExp int32)) slice)]

expReturns (BasicOp op) =
  extReturns . staticShapes <$> primOpType op

expReturns (DoLoop ctx val _ _) =
    return $
    evalState (zipWithM typeWithAttr
               (loopExtType (map (paramIdent . fst) ctx) (map (paramIdent . fst) val)) $
               map fst val) 0
    where typeWithAttr t p =
            case (t, paramAttr p) of
              (Array bt shape u, MemArray _ _ _ (ArrayIn mem ixfun))
                | isMergeVar mem -> do
                  i <- get
                  modify succ
                  return $ MemArray bt shape u $ Just $ ReturnsNewBlock i Nothing
                | otherwise ->
                  return (MemArray bt shape u $
                          Just $ ReturnsInBlock mem ixfun)
              (Array{}, _) ->
                fail "expReturns: Array return type but not array merge variable."
              (Prim bt, _) ->
                return $ MemPrim bt
              (Mem{}, _) ->
                fail "expReturns: loop returns memory block explicitly."
          isMergeVar = flip elem $ map paramName mergevars
          mergevars = map fst $ ctx ++ val

expReturns (Apply _ _ ret _) =
  return $ map funReturnsToExpReturns ret

expReturns (If _ b1 b2 (IfAttr ts _)) = do
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
    return [MemMem (Free size) space]
  opReturns (Inner k@(Kernel _ _ _ body)) =
    zipWithM correct (kernelBodyResult body) =<< (extReturns <$> opType k)
    where correct (WriteReturn _ arr _ _) _ = varReturns arr
          correct (KernelInPlaceReturn arr) _ =
            extendedScope (varReturns arr)
            (castScope $ scopeOf $ kernelBodyStms body)
          correct _ ret = return ret
  opReturns k =
    extReturns <$> opType k

instance OpReturns InKernel where
  opReturns (Alloc size space) =
    return [MemMem (Free size) space]

  opReturns (Inner (GroupStream _ _ lam _ _)) =
    forM (groupStreamAccParams lam) $ \param ->
      case paramAttr param of
        MemPrim bt ->
          return $ MemPrim bt
        MemArray et shape _ (ArrayIn mem ixfun) ->
          return $ MemArray et (Shape $ map Free $ shapeDims shape) NoUniqueness $
          Just $ ReturnsInBlock mem ixfun
        MemMem size space ->
          return $ MemMem (Free size) space

  opReturns (Inner (GroupScan _ _ input)) =
    mapM varReturns arrs
    where arrs = map snd input

  opReturns k =
    extReturns <$> opType k

-- | The return information of a body.  This can be seen as the
-- "return type with memory annotations" of the body.
bodyReturns :: (Monad m, HasScope lore m, ExplicitMemorish lore) =>
               [ExtType] -> Body lore
            -> m [BodyReturns]
bodyReturns ts (Body _ bnds res) = do
  let boundHere = boundInStms bnds
      inspect _ (Constant val) =
        return $ MemPrim $ primValueType val
      inspect (Prim bt) (Var _) =
        return $ MemPrim bt
      inspect Mem{} (Var _) =
        -- FIXME
        fail "bodyReturns: cannot handle bodies returning memory yet."
      inspect (Array et shape u) (Var v) = do

        memsummary <- do
          summary <- case M.lookup v boundHere of
            Nothing -> lift $ lookupMemInfo v
            Just bindee -> return $ patElemAttr bindee

          case summary of
            MemPrim _ ->
              fail "bodyReturns: inconsistent memory summary"
            MemMem{} ->
              fail "bodyReturns: inconsistent memory summary"

            MemArray _ _ NoUniqueness (ArrayIn mem ixfun)
              | mem `M.member` boundHere -> do
                (i, memmap) <- get

                case M.lookup mem memmap of
                  Nothing -> do
                    put (i+1, M.insert mem (i+1) memmap)
                    return $ ReturnsNewBlock i Nothing

                  Just _ ->
                    fail "bodyReturns: same memory block used multiple times."
              | otherwise ->
                  return $ ReturnsInBlock mem ixfun
        return $ MemArray et shape u memsummary
  evalStateT (zipWithM inspect ts res)
    (0, M.empty)

boundInStms :: [Stm lore] -> M.Map VName (PatElem lore)
boundInStms [] = M.empty
boundInStms (bnd:bnds) =
  boundInStm `M.union` boundInStms bnds
  where boundInStm =
          M.fromList
          [ (patElemName bindee, bindee)
          | bindee <- patternElements $ stmPattern bnd
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
  where rettype = map declExtTypeOf rets
        parammap :: M.Map VName (SubExp, Type)
        parammap = M.fromList $
                   zip (map paramName params) args

        substSubExp (Var v)
          | Just (se,_) <- M.lookup v parammap = se
        substSubExp se = se

        correctDims (MemPrim t) =
          MemPrim t
        correctDims (MemMem (Free se) space) =
          MemMem (Free $ substSubExp se) space
        correctDims (MemMem (Ext d) space) =
          MemMem (Ext d) space
        correctDims (MemArray et shape u memsummary) =
          MemArray et (correctShape shape) u $
          correctSummary memsummary

        correctShape = Shape . map correctDim . shapeDims
        correctDim (Ext i)   = Ext i
        correctDim (Free se) = Free $ substSubExp se

        correctSummary (ReturnsNewBlock i size) =
          ReturnsNewBlock i size
        correctSummary (ReturnsInBlock mem ixfun) =
          -- FIXME: we should also do a replacement in ixfun here.
          ReturnsInBlock mem' ixfun
          where mem' = case M.lookup mem parammap of
                  Just (Var v, _) -> v
                  _               -> mem

-- | Is an array of the given shape stored fully flat row-major with
-- the given index function?
fullyDirect :: Shape -> IxFun.IxFun (PrimExp VName) -> Bool
fullyDirect shape ixfun =
  IxFun.isDirect ixfun && ixFunMatchesInnerShape shape ixfun

ixFunMatchesInnerShape :: Shape -> IxFun.IxFun (PrimExp VName) -> Bool
ixFunMatchesInnerShape shape ixfun =
  drop 1 (IxFun.shape ixfun) == drop 1 shape'
  where shape' = map (primExpFromSubExp int32) $ shapeDims shape
