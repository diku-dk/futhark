{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Futhark.Representation.ExplicitMemory
       ( -- * The Lore definition
         ExplicitMemory
       , MemSummary (..)
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
       , AST.FParamT(FParam)
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
import qualified Text.PrettyPrint.Mainland as PP

import Prelude

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, PatElem, Lambda, ExtLambda, FunDec, FParam, RetType)
import qualified Futhark.Analysis.ScalExp as SE

import Futhark.TypeCheck.TypeError
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Substitute
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Optimise.Simplifier.Simplifiable

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
type RetType = AST.RetType ExplicitMemory
type PatElem = AST.PatElem ExplicitMemory

instance IsRetType [FunReturns] where
  retTypeValues = map returnsToType

  basicRetType t = [ReturnsScalar t]

instance Lore.Lore ExplicitMemory where
  type LetBound ExplicitMemory = MemSummary
  type FParam   ExplicitMemory = MemSummary
  type RetType  ExplicitMemory = [FunReturns]

  representative = ExplicitMemory

  loopResultContext _ res mergevars =
    let shapeContextIdents = loopShapeContext res $ map fparamIdent mergevars
        memContextIdents = nub $ mapMaybe memIfNecessary res
        memSizeContextIdents = nub $ mapMaybe memSizeIfNecessary memContextIdents
    in memSizeContextIdents <> memContextIdents <> shapeContextIdents
    where memIfNecessary ident =
            case find ((==ident) . fparamIdent) mergevars of
              Just fparam | MemSummary mem _ <- fparamLore fparam,
                            isMergeParam mem ->
                Just mem
              _ ->
                Nothing
          memSizeIfNecessary ident
            | Mem (Var sizeident) <- identType ident,
              isMergeParam sizeident =
              Just sizeident
            | otherwise =
              Nothing
          isMergeParam ident =
            any ((==ident) . fparamIdent) mergevars

  applyRetType _ = applyFunReturns

instance Ranged ExplicitMemory where
  bodyRanges body =
    replicate (length $ resultSubExps $ bodyResult body) (Nothing, Nothing)
  patternRanges pat =
    replicate (patternSize pat) (Nothing, Nothing)

instance Simplifiable ExplicitMemory where

data MemSummary = MemSummary Ident IxFun.IxFun
                | Scalar
                deriving (Show)

instance Eq MemSummary where
  _ == _ = True

instance Ord MemSummary where
  _ `compare` _ = EQ

instance FreeIn MemSummary where
  freeIn (MemSummary ident ixfun) = freeIn ident <> freeIn ixfun
  freeIn Scalar                   = HS.empty

instance Substitute MemSummary where
  substituteNames subst (MemSummary name f) =
    MemSummary (substituteNames subst name) f
  substituteNames _ Scalar =
    Scalar

instance Rename MemSummary where
  rename (MemSummary ident f) =
    MemSummary <$> rename ident <*> pure f
  rename Scalar =
    return Scalar

instance PP.Pretty MemSummary where
  ppr Scalar = PP.text "scalar"
  ppr (MemSummary mem ixfun) =
    PP.ppr (identName mem) <> PP.text "->" <> PP.ppr ixfun

data MemReturn = ReturnsInBlock Ident IxFun.IxFun
               | ReturnsNewBlock Int
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

data Returns a = ReturnsArray BasicType ExtShape Uniqueness a
               | ReturnsScalar BasicType
               | ReturnsMemory SubExp
               deriving (Eq, Ord, Show)

instance FreeIn MemReturn where
  freeIn (ReturnsInBlock v ixfun) = freeIn v <> freeIn ixfun
  freeIn _                        = mempty

instance Substitute a => Substitute (Returns a) where
  substituteNames substs (ReturnsArray bt shape u x) =
    ReturnsArray
    bt
    (substituteNames substs shape)
    u
    (substituteNames substs x)
  substituteNames _ (ReturnsScalar bt) =
    ReturnsScalar bt
  substituteNames substs (ReturnsMemory size) =
    ReturnsMemory $ substituteNames substs size

instance PP.Pretty (Returns (Maybe MemReturn)) where
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

instance FreeIn a => FreeIn (Returns a) where
  freeIn (ReturnsScalar _) =
    mempty
  freeIn (ReturnsMemory size) =
    freeIn size
  freeIn (ReturnsArray _ shape _ summary) =
    freeIn shape <> freeIn summary

instance Rename a => Rename (Returns a) where
  rename (ReturnsScalar bt) =
    return $ ReturnsScalar bt
  rename (ReturnsMemory size) =
    ReturnsMemory <$> rename size
  rename (ReturnsArray bt shape u summary) =
    ReturnsArray bt <$> rename shape <*> pure u <*> rename summary

instance PP.Pretty (Returns MemReturn) where
  ppr = PP.ppr . funReturnsToExpReturns

type ExpReturns = Returns (Maybe MemReturn)
type FunReturns = Returns MemReturn

funReturnsToExpReturns :: FunReturns -> ExpReturns
funReturnsToExpReturns (ReturnsArray bt shape u summary) =
  ReturnsArray bt shape u $ Just summary
funReturnsToExpReturns (ReturnsScalar bt) =
  ReturnsScalar bt
funReturnsToExpReturns (ReturnsMemory size) =
  ReturnsMemory size

generaliseReturns :: [FunReturns] -> [FunReturns] -> [FunReturns]
generaliseReturns r1s r2s =
  evalState (zipWithM generaliseReturns' r1s r2s) (0, HM.empty, HM.empty)
  where generaliseReturns'
          (ReturnsArray bt shape1 u1 summary1)
          (ReturnsArray _  shape2 u2 summary2) =
            ReturnsArray bt
            <$>
            (ExtShape <$>
             zipWithM unifyExtDims
             (extShapeDims shape1)
             (extShapeDims shape2))
            <*>
            pure (u1 <> u2)
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
  checkFParamLore = checkMemSummary
  checkRetType = mapM_ TypeCheck.checkExtType . retTypeValues
  basicFParam _ name t =
    AST.FParam (Ident name (AST.Basic t)) Scalar

  matchPattern pat e = do
    rt <- expReturns varMemSummary e
    matchPatternToReturns (wrong rt) pat rt
    where wrong rt s = TypeCheck.bad $ TypeError noLoc $
                       ("Pattern\n" ++ TypeCheck.message "  " pat ++
                        "\ncannot match result type\n") ++
                       "  " ++ prettyTuple rt ++ "\n" ++ s

  matchReturnType fname rettype result = do
    TypeCheck.matchExtReturnType fname ts result
    mapM_ checkResultSubExp $ resultSubExps result
    where ts = map returnsToType rettype
          checkResultSubExp (Constant {}) =
            return ()
          checkResultSubExp (Var v) = do
            attr <- lookupSummary $ identName v
            case attr of
              Scalar -> return ()
              MemSummary _ ixfun
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
              TypeCheck.FunBound summary -> return summary
              TypeCheck.LambdaBound      ->
                fail "ExplicitMemory.matchReturnType: asked to return something lambda-bound from a function.\nThis implies a bug in the type checker."

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
      | t' `subtypeOf` bindeet =
        return ()
      | otherwise =
        lift $ wrong $ "Bindee " ++ pretty bindee ++
        " has type " ++ pretty bindeet ++
        ", but expression returns " ++ pretty t' ++ "."
      where bindeet = toDecl $ patElemRequires bindee
            t'      = toDecl t


    matchBindee bindee (ReturnsScalar bt) =
      matchType bindee $ Basic bt
    matchBindee bindee (ReturnsMemory size@(Constant {})) =
      matchType bindee $ Mem size
    matchBindee bindee (ReturnsMemory (Var size)) = do
      popSizeIfInCtx $ identName size
      matchType bindee $ Mem $ Var size
    matchBindee bindee (ReturnsArray et shape u rets)
      | MemSummary mem bindeeIxFun <- patElemLore bindee,
        Mem size <- identType mem = do
          case size of
            Var size' -> popSizeIfInCtx $ identName size'
            _         -> return ()
          case rets of
            Nothing -> return ()
            Just (ReturnsInBlock retmem retIxFun) -> do
              when (mem /= retmem) $
                if inCtx $ identName mem then
                  popMemFromCtx $ identName mem
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
              popMemFromCtx $ identName mem
          zipWithM_ matchArrayDim (arrayDims $ patElemType bindee) $
            extShapeDims shape
          matchType bindee $ Array et shape u
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
            Mem (Var size) ->
              popSizeIfInCtx $ identName size
            Mem (Constant {}) ->
              return ()
            _ ->
              lift $ wrong $ pretty memBindee ++ " is not a memory block."
        _ ->
          return () -- OK, already seen.

    matchArrayDim (Var v) (Free _) =
      popSizeIfInCtx $ identName v --  *May* be bound here.
    matchArrayDim (Var v) (Ext _) =
      popSizeFromCtx $ identName v --  *Has* to be bound here.
    matchArrayDim (Constant {}) (Free _) =
      return ()
    matchArrayDim (Constant {}) (Ext _) =
      lift $ wrong
      "Existential dimension in expression return, but constant in pattern."

varMemSummary :: VName -> TypeCheck.TypeM ExplicitMemory MemSummary
varMemSummary name = do
  (_, attr, _) <- TypeCheck.lookupVar name
  case attr of
    TypeCheck.LetBound summary -> return summary
    TypeCheck.FunBound summary -> return summary
    TypeCheck.LambdaBound ->
      TypeCheck.bad $ TypeError noLoc $
      "Variable " ++ pretty name ++
      " is lambda-bound.\nI cannot deal with this yet."

checkMemSummary :: MemSummary
                -> TypeCheck.TypeM ExplicitMemory ()
checkMemSummary Scalar = return ()
checkMemSummary (MemSummary v ixfun) = do
  _ <- checkMemIdent v
  traverse_ TypeCheck.checkIdent $ freeIn ixfun
  where checkMemIdent ident = do
          _ <- TypeCheck.checkIdent ident
          case identType ident of
            Mem size ->
              TypeCheck.require [Basic Int] size
            t        ->
              TypeCheck.bad $ TypeCheck.TypeError noLoc $
              "Variable " ++ textual (identName v) ++
              " used as memory block, but is of type " ++
              pretty t ++ "."

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
  ppExpLore (AST.LoopOp (DoLoop _ merge _ _)) =
    case mapMaybe (fparamAnnot . fst) merge of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppExpLore _ = Nothing

bindeeAnnot :: (a -> VName) -> (a -> MemSummary)
                -> a -> Maybe PP.Doc
bindeeAnnot bindeeName bindeeLore bindee =
  case bindeeLore bindee of
    MemSummary ident fun ->
      Just $
      PP.text "// " <>
      PP.ppr (bindeeName bindee) <>
      PP.text "@" <>
      PP.ppr (identName ident) <>
      PP.text "->" <>
      PP.ppr fun
    Scalar ->
      Nothing

fparamAnnot :: FParam -> Maybe PP.Doc
fparamAnnot = bindeeAnnot fparamName fparamLore

patElemAnnot :: PatElem -> Maybe PP.Doc
patElemAnnot = bindeeAnnot patElemName patElemLore

returnsToType :: Returns a -> ExtType
returnsToType (ReturnsScalar bt) = Basic bt
returnsToType (ReturnsMemory size) = Mem size
returnsToType (ReturnsArray bt shape u _) =
  Array bt shape u

extReturns :: [ExtType] -> [ExpReturns]
extReturns ts =
    evalState (mapM addAttr ts) 0
    where addAttr (Basic bt) =
            return $ ReturnsScalar bt
          addAttr (Mem size) =
            return $ ReturnsMemory size
          addAttr t@(Array bt shape u)
            | existential t = do
              i <- get
              put $ i + 1
              return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i
            | otherwise =
              return $ ReturnsArray bt shape u Nothing

arrayIdentReturns :: Monad m => (VName -> m MemSummary)
                  -> Ident -> m (BasicType, Shape, Uniqueness,
                                 Ident, IxFun.IxFun)
arrayIdentReturns look v = do
  summary <- look $ identName v
  case (summary, identType v) of
    (MemSummary mem ixfun, Array et shape u) ->
      return (et, Shape $ shapeDims shape, u,
              mem, ixfun)
    _ ->
      fail $ "arrayIdentReturns: " ++ pretty v ++ " is not an array."

identReturns :: Monad m => (VName -> m MemSummary)
             -> Ident -> m ExpReturns
identReturns look v = do
  summary <- look $ identName v
  case (summary, identType v) of
    (Scalar, Basic bt) ->
      return $ ReturnsScalar bt
    (MemSummary mem ixfun, Array et shape u) ->
      return $ ReturnsArray et (ExtShape $ map Free $ shapeDims shape) u $
              Just $ ReturnsInBlock mem ixfun
    (Scalar, Mem size) ->
      return $ ReturnsMemory size
    _ ->
      fail "Something went very wrong in identReturns."

expReturns :: Monad m => (VName -> m MemSummary) -> Exp -> m [ExpReturns]

expReturns look (AST.PrimOp (SubExp (Var v))) = do
  r <- identReturns look v
  return [r]

expReturns look (AST.PrimOp (Reshape _ newshape v)) = do
  (et, _, u, mem, ixfun) <- arrayIdentReturns look v
  return [ReturnsArray et (ExtShape $ map Free newshape) u $
          Just $ ReturnsInBlock mem $
          IxFun.reshape ixfun newshape]

expReturns look (AST.PrimOp (Rearrange _ perm v)) = do
  (et, Shape dims, u, mem, ixfun) <- arrayIdentReturns look v
  let ixfun' = IxFun.permute ixfun perm
      dims'  = permuteShape perm dims
  return [ReturnsArray et (ExtShape $ map Free dims') u $
          Just $ ReturnsInBlock mem ixfun']

expReturns look (AST.PrimOp (Split _ sizeexps v)) = do
  (et, shape, u, mem, ixfun) <- arrayIdentReturns look v
  let newShapes = map (shape `setOuterDim`) sizeexps
      offsets = scanl (\acc n -> SE.SPlus acc (SE.subExpToScalExp n))
                (SE.Val $ IntVal 0) sizeexps
      slcOffsets = map (\offset -> sliceOffset shape [offset]) offsets
  return $ zipWith (\newShape slcOffset
                    -> ReturnsArray et (ExtShape $ map Free $ shapeDims newShape) u $
                       Just $ ReturnsInBlock mem $ IxFun.offset ixfun slcOffset)
           newShapes slcOffsets

expReturns look (AST.PrimOp (Index _ v is)) = do
  (et, shape, u, mem, ixfun) <- arrayIdentReturns look v
  case stripDims (length is) shape of
    Shape []     ->
      return [ReturnsScalar et]
    Shape dims ->
      return [ReturnsArray et (ExtShape $ map Free dims) u $
             Just $ ReturnsInBlock mem $
             IxFun.applyInd ixfun
             (map SE.subExpToScalExp is)]

expReturns _ (AST.PrimOp (Alloc size)) =
  return [ReturnsMemory size]

expReturns _ (AST.PrimOp op) =
  return $ extReturns $ staticShapes $ primOpType op

expReturns _ (AST.LoopOp (DoLoop res merge _ _)) =
    return $
    evalState (mapM typeWithAttr $
               zip (map identName res) $
               loopExtType res $ map fparamIdent mergevars) 0
    where typeWithAttr (resname, t) =
            case (t,
                  fparamLore <$> find ((==resname) . fparamName) mergevars) of
              (Array bt shape u, Just (MemSummary mem ixfun))
                | isMergeVar $ identName mem -> do
                  i <- get
                  put $ i + 1
                  return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i
                | otherwise ->
                  return (ReturnsArray bt shape u $
                          Just $ ReturnsInBlock mem ixfun)
              (Array {}, _) ->
                fail "expReturns: result is not a merge variable."
              (Basic bt, _) ->
                return $ ReturnsScalar bt
              (Mem _, _) ->
                fail "expReturns: loop returns memory block explicitly."
          isMergeVar = flip elem $ map fparamName mergevars
          mergevars = map fst merge

expReturns _ (AST.LoopOp op) =
  return $ extReturns $ loopOpExtType op

expReturns _ (Apply _ _ ret) =
  return $ map funReturnsToExpReturns ret

expReturns look (If _ b1 b2 ts) = do
  b1t <- bodyReturns look ts b1
  b2t <- bodyReturns look ts b2
  return $
    map funReturnsToExpReturns $
    generaliseReturns b1t b2t

bodyReturns :: Monad m =>
               (VName -> m MemSummary)
            -> [ExtType] -> Body
            -> m [FunReturns]
bodyReturns look ts (AST.Body _ bnds res) = do
  let boundHere = boundInBindings bnds
      inspect _ (Constant val) =
        return $ ReturnsScalar $ basicValueType val
      inspect (Basic bt) (Var _) =
        return $ ReturnsScalar bt
      inspect (Mem _) (Var _) =
        -- FIXME
        fail "bodyReturns: cannot handle bodies returning memory yet."
      inspect (Array et shape u) (Var v) = do
        let name = identName v

        memsummary <- do
          summary <- case HM.lookup name boundHere of
            Nothing -> lift $ look name
            Just bindee -> return $ patElemLore bindee

          case summary of
            Scalar ->
              fail "bodyReturns: inconsistent memory summary"

            MemSummary mem ixfun -> do
              let memname = identName mem

              if memname `HM.member` boundHere then do
                (i, memmap) <- get

                case HM.lookup memname memmap of
                  Nothing -> do
                    put (i+1, HM.insert memname (i+1) memmap)
                    return $ ReturnsNewBlock i

                  Just _ ->
                    fail "bodyReturns: same memory block used multiple times."
              else return $ ReturnsInBlock mem ixfun
        return $ ReturnsArray et shape u memsummary
  evalStateT (zipWithM inspect ts $ resultSubExps res)
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

applyFunReturns :: [FunReturns]
                -> [FParam]
                -> [SubExp]
                -> Maybe [FunReturns]
applyFunReturns rets params args
  | Just _ <- applyExtType rettype (map fparamIdent params) args =
    Just $ map correctDims rets
  | otherwise =
    Nothing
  where rettype = ExtRetType $ map returnsToType rets
        parammap :: HM.HashMap VName SubExp
        parammap = HM.fromList $
                   zip (map fparamName params) args

        substSubExp (Var v)
          | Just se <- HM.lookup (identName v) parammap = se
        substSubExp se = se

        correctDims (ReturnsScalar t) =
          ReturnsScalar t
        correctDims (ReturnsMemory se) =
          ReturnsMemory $ substSubExp se
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
          where mem' = case HM.lookup (identName mem) parammap of
                  Just (Var v) -> v
                  _            -> mem

-- | The size of a basic type in bytes.
basicSize :: BasicType -> Int
basicSize Int = 4
basicSize Bool = 1
basicSize Char = 1
basicSize Real = 8
basicSize Cert = 1

-- | The size of an array slice in elements.
sliceOffset :: Shape -> [SE.ScalExp] -> SE.ScalExp
sliceOffset shape is =
  SE.ssum $ zipWith SE.STimes is sliceSizes
  where sliceSizes =
          map SE.sproduct $
          drop 1 $ tails $ map SE.subExpToScalExp $ shapeDims shape
