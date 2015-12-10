{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
-- | This representation requires that every array is given
-- information about which memory block is it based in, and how array
-- elements map to memory block offsets.  SOACs are not supported, so
-- you will have to convert them to explicit loops first.
module Futhark.Representation.ExplicitMemory
       ( -- * The Lore definition
         ExplicitMemory
       , MemOp (..)
       , MemBound (..)
       , MemReturn (..)
       , Returns (..)
       , ExpReturns
       , FunReturns
       , expReturns
       , bodyReturns
       , returnsToType
       , generaliseReturns
       , basicSize
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PatElem
       , PrimOp
       , LoopOp
       , Exp
       , Lambda
       , ExtLambda
       , FunDec
       , FParam
       , LParam
       , RetType
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.ExtLambdaT(ExtLambda)
       , AST.BodyT(Body)
       , AST.PatElemT(PatElem)
       , AST.PatternT(Pattern)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.ExpT(LoopOp)
       , AST.FunDecT(FunDec)
       , AST.ParamT(Param)
       )
where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Foldable (traverse_)
import Data.Maybe
import Data.List
import Data.Loc
import Data.Monoid

import Prelude

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Annotations as Annotations
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, PatElem, Lambda, ExtLambda, FunDec, FParam, LParam,
          RetType)
import qualified Futhark.Analysis.ScalExp as SE

import Futhark.TypeCheck.TypeError
import Futhark.Representation.AST.Attributes hiding (Lore)
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Binder
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import qualified Futhark.Util.Pretty as PP
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Aliases (Aliases)
import Futhark.Representation.Ranges (Ranges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Analysis.Usage

-- | A lore containing explicit memory information.
data ExplicitMemory = ExplicitMemory

type Prog = AST.Prog ExplicitMemory
type PrimOp = AST.PrimOp ExplicitMemory
type LoopOp = AST.LoopOp ExplicitMemory
type Exp = AST.Exp ExplicitMemory
type Body = AST.Body ExplicitMemory
type Binding = AST.Binding ExplicitMemory
type Pattern = AST.Pattern ExplicitMemory
type Lambda = AST.Lambda ExplicitMemory
type ExtLambda = AST.ExtLambda ExplicitMemory
type FunDec = AST.FunDec ExplicitMemory
type FParam = AST.FParam ExplicitMemory
type LParam = AST.LParam ExplicitMemory
type RetType = AST.RetType ExplicitMemory
type PatElem = AST.PatElem ExplicitMemory

instance IsRetType [FunReturns] where
  retTypeValues = map returnsToType

  basicRetType t = [ReturnsScalar t]

  applyRetType = applyFunReturns

data MemOp inner = Alloc SubExp Space
                   -- ^ Allocate a memory block.  This really should not be an
                   -- expression, but what are you gonna do...
            deriving (Eq, Ord, Show)

instance Proper inner => FreeIn (MemOp inner) where
  freeIn (Alloc size _) = freeIn size

instance Proper inner => TypedOp (MemOp inner) where
  opType (Alloc size space) = pure [Mem size space]

instance Aliased inner => AliasedOp (MemOp inner) where
  opAliases Alloc{} = [mempty]

  consumedInOp Alloc{} = mempty

instance CanBeAliased (MemOp ExplicitMemory) where
  type OpWithAliases (MemOp ExplicitMemory) = MemOp (Aliases ExplicitMemory)
  removeOpAliases (Alloc se space) = Alloc se space

  addOpAliases (Alloc se space) = Alloc se space

instance (Proper inner, Ranged inner) => RangedOp (MemOp inner) where
  opRanges (Alloc _ _) =
    [unknownRange]

instance CanBeRanged (MemOp ExplicitMemory) where
  type OpWithRanges (MemOp ExplicitMemory) = MemOp (Ranges ExplicitMemory)
  removeOpRanges (Alloc size space) = Alloc size space

  addOpRanges (Alloc size space) = Alloc size space

instance Renameable inner => Rename (MemOp inner) where
  rename (Alloc size space) = Alloc <$> rename size <*> pure space

instance Substitutable inner => Substitute (MemOp inner) where
  substituteNames subst (Alloc size space) = Alloc (substituteNames subst size) space

instance PrettyLore inner => PP.Pretty (MemOp inner) where
  ppr (Alloc e DefaultSpace) = PP.text "alloc" <> PP.apply [PP.ppr e]
  ppr (Alloc e (Space sp)) = PP.text "alloc" <> PP.apply [PP.ppr e, PP.text sp]

instance Proper inner => IsOp (MemOp inner) where
  safeOp Alloc{} = True

instance (Aliased lore, UsageInOp (Op lore)) => UsageInOp (MemOp lore) where
  usageInOp Alloc {} = mempty

instance CanBeWise (MemOp ExplicitMemory) where
  type OpWithWisdom (MemOp ExplicitMemory) = MemOp (Wise ExplicitMemory)
  removeOpWisdom (Alloc size space) = Alloc size space

instance Engine.SimplifiableOp ExplicitMemory (MemOp ExplicitMemory) where
  simplifyOp (Alloc size space) = Alloc <$> Engine.simplify size <*> pure space

instance Annotations.Annotations ExplicitMemory where
  type LetBound ExplicitMemory = MemBound NoUniqueness
  type FParam   ExplicitMemory = MemBound Uniqueness
  type LParam   ExplicitMemory = MemBound NoUniqueness
  type RetType  ExplicitMemory = [FunReturns]
  type Op       ExplicitMemory = MemOp ExplicitMemory

instance Lore.Lore ExplicitMemory where
  representative = ExplicitMemory

  loopResultContext _ res mergevars =
    let shapeContextIdents = loopShapeContext res $ map paramIdent mergevars
        memContextIdents = nub $ mapMaybe memIfNecessary res
        memSizeContextIdents = nub $ mapMaybe memSizeIfNecessary memContextIdents
    in memSizeContextIdents <> map identName memContextIdents <> shapeContextIdents
    where memIfNecessary var =
            case find ((==var) . paramName) mergevars of
              Just fparam | ArrayMem _ _ _ mem _ <- paramAttr fparam,
                            Just memparam <- isMergeParam mem ->
                Just $ paramIdent memparam
              _ ->
                Nothing
          memSizeIfNecessary ident
            | Mem (Var sizevar) _ <- identType ident,
              isJust $ isMergeParam sizevar =
              Just sizevar
            | otherwise =
              Nothing
          isMergeParam var =
            find ((==var) . paramName) mergevars

-- | A summary of the memory information for every let-bound identifier
-- and function parameter.
data MemBound u = Scalar BasicType
                 -- ^ The corresponding identifier is a
                 -- scalar.  It must not be of array type.
               | MemMem SubExp Space
               | ArrayMem BasicType Shape u VName IxFun.IxFun
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
    Basic bt
  typeOf (MemMem size space) =
    Mem size space
  typeOf (ArrayMem bt shape u _ _) =
    Array bt shape u

instance DeclTyped (MemBound Uniqueness) where
  declTypeOf (Scalar bt) =
    Basic bt
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
  rename (ArrayMem bt shape u mem f) =
    ArrayMem bt <$> rename shape <*> pure u <*> rename mem <*> rename f
  rename (MemMem size space) =
    MemMem <$> rename size <*> pure space
  rename (Scalar bt) =
    return (Scalar bt)

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
-- by an operation.  Note that the 'Eq' and 'Ord' instances are
-- useless (everything is equal).  This type is merely a building
-- block for 'Returns'.
data MemReturn = ReturnsInBlock VName IxFun.IxFun
                 -- ^ The array is located in a memory block that is
                 -- already in scope.
               | ReturnsNewBlock Int
                 -- ^ The operation returns a new (existential) block,
                 -- with an existential size.
               deriving (Show)

instance Eq MemReturn where
  _ == _ = True

instance Ord MemReturn where
  _ `compare` _ = EQ

instance Rename MemReturn where
  rename (ReturnsInBlock ident ixfun) =
    ReturnsInBlock <$> rename ident <*> rename ixfun
  rename (ReturnsNewBlock i) =
    return $ ReturnsNewBlock i

instance Substitute MemReturn where
  substituteNames substs (ReturnsInBlock ident ixfun) =
    ReturnsInBlock (substituteNames substs ident) (substituteNames substs ixfun)
  substituteNames _ (ReturnsNewBlock i) =
    ReturnsNewBlock i

-- | A description of a value being returned from a construct,
-- parametrised across extra information stored for arrays.
data Returns u a = ReturnsArray BasicType ExtShape u a
                   -- ^ Returns an array of the given element type,
                   -- (existential) shape, and uniqueness.
                 | ReturnsScalar BasicType
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
          pp (Just (ReturnsNewBlock i)) =
            PP.text $ show i
  ppr ret =
    PP.ppr $ returnsToType ret

instance FreeIn a => FreeIn (Returns u a) where
  freeIn (ReturnsScalar _) =
    mempty
  freeIn (ReturnsMemory size _) =
    freeIn size
  freeIn (ReturnsArray _ shape _ summary) =
    freeIn shape <> freeIn summary

instance Rename a => Rename (Returns u a) where
  rename (ReturnsScalar bt) =
    return $ ReturnsScalar bt
  rename (ReturnsMemory size space) =
    ReturnsMemory <$> rename size <*> pure space
  rename (ReturnsArray bt shape u summary) =
    ReturnsArray bt <$> rename shape <*> pure u <*> rename summary

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
  simplify (ReturnsNewBlock i) =
    return $ ReturnsNewBlock i
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
generaliseReturns :: [BodyReturns] -> [BodyReturns] -> [BodyReturns]
generaliseReturns r1s r2s =
  evalState (zipWithM generaliseReturns' r1s r2s) (0, HM.empty, HM.empty)
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
          return t1 -- Must be basic then.

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
                           return $ ReturnsNewBlock i
        generaliseSummaries (ReturnsNewBlock x) _ =
          ReturnsNewBlock <$> newMem x
        generaliseSummaries _ (ReturnsNewBlock x) =
          ReturnsNewBlock <$> newMem x

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
  checkOp (Alloc size _) = TypeCheck.require [Basic Int] size

  basicFParam _ name t =
    AST.Param name (Scalar t)
  basicLParam _ name t =
    AST.Param name (Scalar t)

  matchPattern pat e = do
    rt <- expReturns varMemBound e
    matchPatternToReturns (wrong rt) pat rt
    where wrong rt s = TypeCheck.bad $ TypeError noLoc $
                       ("Pattern\n" ++ TypeCheck.message "  " pat ++
                        "\ncannot match result type\n") ++
                       "  " ++ prettyTuple rt ++ "\n" ++ s

  matchReturnType fname rettype result = do
    TypeCheck.matchExtReturnType fname (fromDecl <$> ts) result
    mapM_ checkResultSubExp result
    where ts = map returnsToType rettype
          checkResultSubExp Constant{} =
            return ()
          checkResultSubExp (Var v) = do
            attr <- lookupSummary v
            case attr of
              Scalar _ -> return ()
              MemMem{} -> return ()
              ArrayMem _ _ _ _ ixfun
                | IxFun.isDirect ixfun ->
                  return ()
                | otherwise ->
                    TypeCheck.bad $ TypeError noLoc $
                    "Array " ++ pretty v ++
                    " returned by function, but has nontrivial index function" ++
                    pretty ixfun
          lookupSummary name = do
            (_, attr, _) <- TypeCheck.lookupVar name
            case attr of
              TypeCheck.LetBound summary -> return summary
              TypeCheck.FunBound summary -> return $ fmap (const NoUniqueness) summary
              TypeCheck.LambdaBound summary -> return $ fmap (const NoUniqueness) summary

matchPatternToReturns :: Monad m =>
                         (String -> m ())
                      -> Pattern
                      -> [ExpReturns]
                      -> m ()
matchPatternToReturns wrong pat rt = do
  remaining <- execStateT (zipWithM matchBindee valbindees rt) ctxbindees
  unless (null remaining) $
    wrong $ "Unused parts of pattern: " ++
    intercalate ", " (map pretty remaining)
  where
    (ctxbindees, valbindees) =
        splitAt (patternSize pat - length rt) $ patternElements pat
    inCtx = (`elem` map patElemName ctxbindees)

    matchType bindee t
      | t' == bindeet =
        return ()
      | otherwise =
        lift $ wrong $ "Bindee " ++ pretty (bindee :: PatElem) ++
        " has type " ++ pretty bindeet ++
        ", but expression returns " ++ pretty t ++ "."
      where bindeet = rankShaped $ patElemRequires bindee
            t'      = rankShaped (t :: ExtType)


    matchBindee bindee (ReturnsScalar bt) =
      matchType bindee $ Basic bt
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
                lift $ wrong $ "Bindee index function is " ++
                pretty bindeeIxFun ++ ", but return index function is " ++
                pretty retIxFun ++ "."

            Just (ReturnsNewBlock _) ->
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
          unless (patElemType nameBindee == Basic Int) $
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

varMemBound :: VName -> TypeCheck.TypeM ExplicitMemory (MemBound NoUniqueness)
varMemBound name = do
  (_, attr, _) <- TypeCheck.lookupVar name
  case attr of
    TypeCheck.LetBound summary -> return summary
    TypeCheck.FunBound summary -> return $ fmap (const NoUniqueness) summary
    TypeCheck.LambdaBound summary -> return summary

checkMemBound :: VName -> MemBound u
             -> TypeCheck.TypeM ExplicitMemory ()
checkMemBound _ (Scalar _) = return ()
checkMemBound _ (MemMem size _) =
  TypeCheck.require [Basic Int] size
checkMemBound name (ArrayMem _ shape _ v ixfun) = do
  t <- lookupType v
  case t of
    Mem{} ->
      return ()
    _        ->
      TypeCheck.bad $ TypeCheck.TypeError noLoc $
      "Variable " ++ textual v ++
      " used as memory block, but is of type " ++
      pretty t ++ "."

  TypeCheck.context ("in index function " ++ pretty ixfun) $ do
    traverse_ (TypeCheck.requireI [Basic Int]) $ freeIn ixfun
    let ixfun_rank = IxFun.rank ixfun
        ident_rank = shapeRank shape
    unless (ixfun_rank == ident_rank) $
      TypeCheck.bad $ TypeCheck.TypeError noLoc $
      "Arity of index function (" ++ pretty ixfun_rank ++
      ") does not match rank of array " ++ pretty name ++
      " (" ++ show ident_rank ++ ")"

instance Renameable ExplicitMemory where
instance Substitutable ExplicitMemory where
instance Proper ExplicitMemory where

instance PrettyLore ExplicitMemory where
  ppBindingLore binding =
    case mapMaybe patElemAnnot $ patternElements $ bindingPattern binding of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppFunDecLore fundec =
    case mapMaybe fparamAnnot $ funDecParams fundec of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppLambdaLore lam =
    case mapMaybe lparamAnnot $ lambdaParams lam of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppExpLore (AST.LoopOp (DoLoop _ merge _ _)) =
    case mapMaybe (fparamAnnot . fst) merge of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppExpLore _ = Nothing

bindeeAnnot :: PP.Pretty u =>
               (a -> VName) -> (a -> MemBound u)
            -> a -> Maybe PP.Doc
bindeeAnnot bindeeName bindeeLore bindee =
  case bindeeLore bindee of
    attr@ArrayMem{} ->
      Just $
      PP.text "-- " <>
      PP.ppr (bindeeName bindee) <>
      PP.text " :: " <>
      PP.ppr attr
    MemMem {} ->
      Nothing
    Scalar _ ->
      Nothing

fparamAnnot :: FParam -> Maybe PP.Doc
fparamAnnot = bindeeAnnot paramName paramAttr

lparamAnnot :: LParam -> Maybe PP.Doc
lparamAnnot = bindeeAnnot paramName paramAttr

patElemAnnot :: PatElem -> Maybe PP.Doc
patElemAnnot = bindeeAnnot patElemName patElemAttr

-- | Convet a 'Returns' to an 'Type' by throwing away memory
-- information.
returnsToType :: Returns u a -> TypeBase ExtShape u
returnsToType (ReturnsScalar bt) =
  Basic bt
returnsToType (ReturnsMemory size space) =
  Mem size space
returnsToType (ReturnsArray bt shape u _) =
  Array bt shape u

extReturns :: [ExtType] -> [ExpReturns]
extReturns ts =
    evalState (mapM addAttr ts) 0
    where addAttr (Basic bt) =
            return $ ReturnsScalar bt
          addAttr (Mem size space) =
            return $ ReturnsMemory size space
          addAttr t@(Array bt shape u)
            | existential t = do
              i <- get
              put $ i + 1
              return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i
            | otherwise =
              return $ ReturnsArray bt shape u Nothing

arrayVarReturns :: (Monad m, HasTypeEnv m) =>
                   (VName -> m (MemBound u))
                -> VName
                -> m (BasicType, Shape, VName, IxFun.IxFun)
arrayVarReturns look v = do
  summary <- look v
  case summary of
    ArrayMem et shape _ mem ixfun ->
      return (et, Shape $ shapeDims shape, mem, ixfun)
    _ ->
      fail $ "arrayVarReturns: " ++ pretty v ++ " is not an array."

varReturns :: (Monad m, HasTypeEnv m) =>
              (VName -> m (MemBound u))
           -> VName -> m ExpReturns
varReturns look v = do
  summary <- look v
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
expReturns :: (Monad m, HasTypeEnv m) =>
              (VName -> m (MemBound NoUniqueness)) -> Exp -> m [ExpReturns]

expReturns look (AST.PrimOp (SubExp (Var v))) = do
  r <- varReturns look v
  return [r]

expReturns look (AST.PrimOp (Reshape _ newshape v)) = do
  (et, _, mem, ixfun) <- arrayVarReturns look v
  return [ReturnsArray et (ExtShape $ map (Free . newDim) newshape) NoUniqueness $
          Just $ ReturnsInBlock mem $
          IxFun.reshape ixfun newshape]

expReturns look (AST.PrimOp (Rearrange _ perm v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns look v
  let ixfun' = IxFun.permute ixfun perm
      dims'  = rearrangeShape perm dims
  return [ReturnsArray et (ExtShape $ map Free dims') NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns look (AST.PrimOp (Stripe _ stride v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns look v
  let ixfun' = IxFun.stripe ixfun (SE.intSubExpToScalExp stride)
  return [ReturnsArray et (ExtShape $ map Free dims) NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns look (AST.PrimOp (Unstripe _ stride v)) = do
  (et, Shape dims, mem, ixfun) <- arrayVarReturns look v
  let ixfun' = IxFun.unstripe ixfun (SE.intSubExpToScalExp stride)
  return [ReturnsArray et (ExtShape $ map Free dims) NoUniqueness $
          Just $ ReturnsInBlock mem ixfun']

expReturns look (AST.PrimOp (Split _ sizeexps v)) = do
  (et, shape, mem, ixfun) <- arrayVarReturns look v
  let newShapes = map (shape `setOuterDim`) sizeexps
      offsets = scanl (\acc n -> SE.SPlus acc (SE.subExpToScalExp n Int))
                (SE.Val $ IntVal 0) sizeexps
  return $ zipWith (\new_shape offset
                    -> ReturnsArray et (ExtShape $ map Free $ shapeDims new_shape)
                       NoUniqueness $
                       Just $ ReturnsInBlock mem $ IxFun.offsetIndex ixfun offset)
           newShapes offsets

expReturns look (AST.PrimOp (Index _ v is)) = do
  (et, shape, mem, ixfun) <- arrayVarReturns look v
  case stripDims (length is) shape of
    Shape []     ->
      return [ReturnsScalar et]
    Shape dims ->
      return [ReturnsArray et (ExtShape $ map Free dims) NoUniqueness $
             Just $ ReturnsInBlock mem $
             IxFun.applyInd ixfun
             (map (`SE.subExpToScalExp` Int) is)]

expReturns _ (AST.PrimOp op) =
  extReturns <$> staticShapes <$> primOpType op

expReturns _ (AST.LoopOp (DoLoop res merge _ _)) =
    return $
    evalState (mapM typeWithAttr $
               zip res $
               loopExtType res $ map paramIdent mergevars) 0
    where typeWithAttr (resname, t) =
            case (t,
                  paramAttr <$> find ((==resname) . paramName) mergevars) of
              (Array bt shape u, Just (ArrayMem _ _ _ mem ixfun))
                | isMergeVar mem -> do
                  i <- get
                  put $ i + 1
                  return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i
                | otherwise ->
                  return (ReturnsArray bt shape u $
                          Just $ ReturnsInBlock mem ixfun)
              (Array{}, _) ->
                fail "expReturns: result is not a merge variable."
              (Basic bt, _) ->
                return $ ReturnsScalar bt
              (Mem{}, _) ->
                fail "expReturns: loop returns memory block explicitly."
          isMergeVar = flip elem $ map paramName mergevars
          mergevars = map fst merge

expReturns _ (AST.LoopOp op) =
  pure $ extReturns $ loopOpExtType op

expReturns _ (Apply _ _ ret) =
  return $ map funReturnsToExpReturns ret

expReturns look (If _ b1 b2 ts) = do
  b1t <- bodyReturns look ts b1
  b2t <- bodyReturns look ts b2
  return $
    map bodyReturnsToExpReturns $
    generaliseReturns b1t b2t

expReturns _ (Op (Alloc size space)) =
  return [ReturnsMemory size space]

-- | The return information of a body.  This can be seen as the
-- "return type with memory annotations" of the body.
bodyReturns :: Monad m =>
               (VName -> m (MemBound NoUniqueness))
            -> [ExtType] -> Body
            -> m [BodyReturns]
bodyReturns look ts (AST.Body _ bnds res) = do
  let boundHere = boundInBindings bnds
      inspect _ (Constant val) =
        return $ ReturnsScalar $ basicValueType val
      inspect (Basic bt) (Var _) =
        return $ ReturnsScalar bt
      inspect Mem{} (Var _) =
        -- FIXME
        fail "bodyReturns: cannot handle bodies returning memory yet."
      inspect (Array et shape u) (Var v) = do

        memsummary <- do
          summary <- case HM.lookup v boundHere of
            Nothing -> lift $ look v
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
                    return $ ReturnsNewBlock i

                  Just _ ->
                    fail "bodyReturns: same memory block used multiple times."
              | otherwise ->
                  return $ ReturnsInBlock mem ixfun
        return $ ReturnsArray et shape u memsummary
  evalStateT (zipWithM inspect ts res)
    (0, HM.empty)

boundInBindings :: [Binding] -> HM.HashMap VName PatElem
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

        correctSummary (ReturnsNewBlock i) =
          ReturnsNewBlock i
        correctSummary (ReturnsInBlock mem ixfun) =
          -- FIXME: we should also do a replacement in ixfun here.
          ReturnsInBlock mem' ixfun
          where mem' = case HM.lookup mem parammap of
                  Just (Var v, _) -> v
                  _               -> mem

-- | The size of a basic type in bytes.
basicSize :: Num a => BasicType -> a
basicSize Int = 4
basicSize Bool = 1
basicSize Char = 1
basicSize Float64 = 8
basicSize Float32 = 4
basicSize Cert = 1
