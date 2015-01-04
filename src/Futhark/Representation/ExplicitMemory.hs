{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
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
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PatBindee
       , PrimOp
       , LoopOp
       , Exp
       , Lambda
       , FunDec
       , FParam
       , RetType
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.ExpT(LoopOp)
       , AST.FunDecT(FunDec)
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

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, PatBindee, Lambda, FunDec, FParam, RetType)

import Futhark.TypeCheck.TypeError
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Substitute
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun

-- | A lore containing explicit memory information.
data ExplicitMemory = ExplicitMemory

type Prog = AST.Prog ExplicitMemory
type PrimOp = AST.PrimOp ExplicitMemory
type LoopOp = AST.LoopOp ExplicitMemory
type Exp = AST.Exp ExplicitMemory
type Body = AST.Body ExplicitMemory
type Binding = AST.Binding ExplicitMemory
type Pattern = AST.Pattern ExplicitMemory
type PatBindee = AST.PatBindee ExplicitMemory
type Lambda = AST.Lambda ExplicitMemory
type FunDec = AST.FunDec ExplicitMemory
type FParam = AST.FParam ExplicitMemory
type RetType = AST.RetType ExplicitMemory

instance IsRetType [FunReturns] where
  resTypeValues = map returnsToType

  basicRetType t = [ReturnsScalar t]

instance Lore.Lore ExplicitMemory where
  type LetBound ExplicitMemory = MemSummary
  type FParam   ExplicitMemory = MemSummary
  type RetType  ExplicitMemory = [FunReturns]

  representative = ExplicitMemory

  loopResultContext _ res mergevars =
    let shapeContextIdents = loopShapeContext res $ map bindeeIdent mergevars
        memContextIdents = nub $ mapMaybe memIfNecessary res
        memSizeContextIdents = nub $ mapMaybe memSizeIfNecessary memContextIdents
    in memSizeContextIdents <> memContextIdents <> shapeContextIdents
    where memIfNecessary ident =
            case find ((==ident) . bindeeIdent) mergevars of
              Just fparam | MemSummary mem _ <- bindeeLore fparam,
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
            any ((==ident) . bindeeIdent) mergevars

data MemSummary = MemSummary Ident IxFun.IxFun
                | Scalar
                deriving (Show)

instance Eq MemSummary where
  _ == _ = True

instance Ord MemSummary where
  _ `compare` _ = EQ

instance FreeIn MemSummary where
  freeIn (MemSummary ident ixfun) = HS.singleton ident <> freeIn ixfun
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

data MemReturn = ReturnsInBlock Ident
               | ReturnsNewBlock Int
               deriving (Eq, Ord, Show)

instance Rename MemReturn where
  rename (ReturnsInBlock ident) =
    ReturnsInBlock <$> rename ident
  rename (ReturnsNewBlock i) =
    return $ ReturnsNewBlock i

instance Substitute MemReturn where
  substituteNames substs (ReturnsInBlock ident) =
    ReturnsInBlock $ substituteNames substs ident
  substituteNames _ (ReturnsNewBlock i) =
    ReturnsNewBlock i

data Returns a = ReturnsArray BasicType ExtShape Uniqueness a
               | ReturnsScalar BasicType
               | ReturnsMemory SubExp
               deriving (Eq, Ord, Show)

instance FreeIn MemReturn where
  freeIn (ReturnsInBlock v) = HS.singleton v
  freeIn _                  = mempty

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
    where pp Nothing                    = PP.text "any"
          pp (Just (ReturnsInBlock v))  = PP.parens $ PP.text $ pretty v
          pp (Just (ReturnsNewBlock i)) = PP.text $ show i
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

        generaliseSummaries (ReturnsInBlock mem1) (ReturnsInBlock mem2)
          | mem1 == mem2 = return $ ReturnsInBlock mem1 -- Arbitrary
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
  checkRetType = mapM_ TypeCheck.checkExtType . resTypeValues
  basicFParam name t loc =
    return $ Bindee (Ident name (AST.Basic t) loc) Scalar

  matchPattern loc pat e = do
    rt <- expReturns (varMemSummary loc) e
    let (nmemsizes, nmems, nvalsizes) = analyseExpReturns rt
        (memsizes, mems, valsizes,
         AST.Pattern valbindees) = dividePattern pat nmemsizes nmems nvalsizes
    checkMems memsizes mems
    checkVals rt mems valsizes valbindees
    where wrong s = TypeCheck.bad $ TypeError loc $
                    "Pattern\n  " ++ pretty pat ++ "\ncannot match expression\n  " ++
                    pretty e ++ "\n" ++ s
          mustBeEmpty _ [] = return ()
          mustBeEmpty s  _ = wrong $ "Guess: unused " ++ s ++ " bindees"
          checkMems memsizes mems =
            mustBeEmpty "memory block size" =<<
            execStateT (mapM_ checkMem mems) memsizes
          checkVals rt mems valsizes valbindees = do
            ((mems', _), (valsizes', _)) <-
              execStateT
              (zipWithM_ checkVal valbindees rt)
              ((mems, []), (valsizes, []))
            mustBeEmpty "memory block" mems'
            mustBeEmpty "value size"  valsizes'
          checkMem ident
            | Mem (Var size) <- identType ident = do
              memsizes <- get
              case extract size memsizes of
                Nothing        ->
                  lift $ wrong $ "No memory block size " ++ sname ++ "."
                Just memsizes' ->
                  put memsizes'
            | otherwise = lift $ wrong $ sname ++ " is not a memory block."
            where sname = pretty ident
          checkVal valbindee ret = do
            let t = returnsToType ret
            zipWithM_ checkShape
              (shapeDims $ arrayShape $ bindeeType valbindee)
              (extShapeDims $ arrayShape t)
            checkMemReturn (bindeeType valbindee) (bindeeLore valbindee) ret

          -- If the return size is existentially quantified, then the
          -- size must be bound in the pattern itself.
          checkShape (Var v)       (Ext i) = isPatternBoundSize v i
          -- If the return size is free, then the bound size must be
          -- the same a runtime, but we cannot check this statically.
          checkShape _ (Free _) = return ()
          -- Otherwise we have an error.
          checkShape _ _ = lift $ wrong "Shape of binding is nonsensical."

          isPatternBoundSize v i = do
            (mems, (valsizes, seen)) <- get
            -- Either 'i' has been seen before or 'v' must be in
            -- 'valsizes'.
            case (lookup i seen, extract v valsizes) of
              -- Hasn't been seen before, and not remaining in pattern.
              (Nothing, Nothing) -> lift $ wrong $ "Unknown size variable " ++
                                    pretty v
              -- Has been seen before, although possibly with another
              -- existential identifier.
              (Just iv, _)
                | iv /= v   ->
                   -- Inconsistent use of 'i'!
                  lift $ wrong $ pretty v ++ " and " ++
                  pretty iv ++ " used to refer to same size."
                | otherwise -> put (mems, (valsizes, (i, v) : seen))
              -- Was remaining in pattern.
              (_, Just valsizes') -> put (mems, (valsizes', (i, v) : seen))

          checkMemReturn (Mem _) Scalar (ReturnsMemory _) = return ()
          checkMemReturn (Basic _) Scalar (ReturnsScalar _) = return ()
          checkMemReturn
            (Array {})
            (MemSummary {})
            (ReturnsArray _ _ _ Nothing) =
            return ()
          checkMemReturn
            (Array {})
            (MemSummary memv1 ixfun)
            (ReturnsArray _ _ _ (Just (ReturnsInBlock memv2)))
            | memv1 == memv2,
              IxFun.isLinear ixfun = return ()
          checkMemReturn
            (Array {})
            (MemSummary memv ixfun)
            (ReturnsArray _ _ _ (Just (ReturnsNewBlock i)))
            | IxFun.isLinear ixfun = do
              ((mems, seen), sizes) <- get
              case (i `elem` seen, extract memv mems) of
                (True, _) ->
                  -- Cannot store multiple arrays in same block.
                  lift $ wrong "Trying to store multiple arrays in same block"
                (_, Nothing) ->
                  -- memv is not bound in this pattern.
                  lift $ wrong $ pretty memv ++ " is not bound or available in this pattern."
                (False, Just mems') -> put ((mems', i : seen), sizes)
          checkMemReturn _ _ _ = lift $ wrong "Nonsensical memory summary"

  matchReturnType fname rettype =
    TypeCheck.matchExtReturnType fname ts
    where ts = map returnsToType rettype

varMemSummary :: SrcLoc -> VName -> TypeCheck.TypeM ExplicitMemory MemSummary
varMemSummary loc name = do
  (_, attr, _) <- TypeCheck.lookupVar name loc
  case attr of
    TypeCheck.LetBound summary -> return summary
    TypeCheck.FunBound summary -> return summary
    TypeCheck.LambdaBound ->
      TypeCheck.bad $ TypeError loc $
      "Variable " ++ pretty name ++
      " is lambda-bound.\nI cannot deal with this yet."

extract :: Eq a => a -> [a] -> Maybe [a]
extract _ [] = Nothing
extract x (y:ys)
  | x == y    = Just ys
  | otherwise = (y:) <$> extract x ys

analyseExpReturns :: [ExpReturns] -> (Int, Int, Int)
analyseExpReturns rts =
  let (memsizes, mems, valsizes) =
        foldl sumtriple (mempty,mempty,mempty) . map analyse $ rts
  in (HS.size memsizes, HS.size mems, HS.size valsizes)
  where sumtriple (x1,y1,z1) (x2,y2,z2) = (x1 `mappend` x2,
                                           y1 `mappend` y2,
                                           z1 `mappend` z2)
        analyse ret = let (memsize,mem) = analyseAttr ret
                      in (memsize, mem, analyseType $ returnsToType ret)
        analyseType t = shapeContext [t]
        analyseAttr (ReturnsArray _ _ _ (Just (ReturnsInBlock _))) =
          (mempty, mempty)
        analyseAttr (ReturnsArray _ _ _ (Just (ReturnsNewBlock i))) =
          (HS.singleton i, HS.singleton i)
        analyseAttr (ReturnsArray _ _ _ Nothing) =
          (mempty, mempty)
        analyseAttr (ReturnsMemory _) =
          (mempty, mempty)
        analyseAttr (ReturnsScalar _) =
          (mempty, mempty)

dividePattern :: Pattern -> Int -> Int -> Int
              -> ([Ident], [Ident], [Ident], Pattern)
dividePattern (AST.Pattern bindees) memsizes mems valsizes =
  let (memsizebindees, bindees') = splitAt memsizes bindees
      (membindees, bindees'') = splitAt mems bindees'
      (valsizebindees, valbindees) = splitAt valsizes bindees''
  in (map bindeeIdent memsizebindees,
      map bindeeIdent membindees,
      map bindeeIdent valsizebindees,
      AST.Pattern valbindees)

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
              TypeCheck.require [Basic Int] <$> TypeCheck.checkSubExp size
            t        ->
              TypeCheck.bad $ TypeCheck.TypeError (srclocOf ident) $
              "Variable " ++ textual (identName v) ++
              " used as memory block, but is of type " ++
              pretty t ++ "."

instance Renameable ExplicitMemory where
instance Substitutable ExplicitMemory where
instance Proper ExplicitMemory where

instance PrettyLore ExplicitMemory where
  ppBindingLore binding =
    case mapMaybe bindeeAnnotation $ patternBindees $ bindingPattern binding of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppFunDecLore fundec =
    case mapMaybe bindeeAnnotation $ funDecParams fundec of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppExpLore (AST.LoopOp (DoLoop _ merge _ _ _ _)) =
    case mapMaybe (bindeeAnnotation . fst) merge of
      []     -> Nothing
      annots -> Just $ PP.folddoc (PP.</>) annots
  ppExpLore _ = Nothing

bindeeAnnotation :: PatBindee -> Maybe PP.Doc
bindeeAnnotation bindee =
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

expReturns :: Monad m => (VName -> m MemSummary) -> Exp -> m [ExpReturns]

expReturns look (AST.PrimOp (SubExp (Var v))) = do
  summary <- look $ identName v
  case (summary, identType v) of
    (Scalar, Basic bt) ->
      return [ReturnsScalar bt]
    (MemSummary mem ixfun, Array et shape u) ->
      if IxFun.isLinear ixfun then
        return [ReturnsArray et (ExtShape $ map Free $ shapeDims shape) u $
               Just $ ReturnsInBlock mem]
      else
        fail "Sadly, Troels is too bad of a hacker to handle this right now."
    (Scalar, Mem size) ->
      return [ReturnsMemory size]
    _ ->
      fail "Something went very wrong in expReturns."

expReturns _ (AST.PrimOp (Alloc size _)) =
  return [ReturnsMemory size]

expReturns _ (AST.PrimOp op) =
  return $ extReturns $ staticShapes $ primOpType op

expReturns _ (AST.LoopOp (DoLoop res merge _ _ _ _)) =
    return $
    evalState (mapM typeWithAttr $
               zip (map identName res) $
               loopExtType res $ map bindeeIdent mergevars) 0
    where typeWithAttr (resname, t) =
            case (t,
                  bindeeLore <$> find ((==resname) . bindeeName) mergevars) of
              (Array bt shape u, Just (MemSummary mem _))
                | isMergeVar $ identName mem -> do
                  i <- get
                  put $ i + 1
                  return $ ReturnsArray bt shape u $ Just $ ReturnsNewBlock i
                | otherwise ->
                  return (ReturnsArray bt shape u $ Just $ ReturnsInBlock mem)
              (Array {}, _) ->
                fail "expReturns: result is not a merge variable."
              (Basic bt, _) ->
                return $ ReturnsScalar bt
              (Mem _, _) ->
                fail "expReturns: loop returns memory block explicitly."
          isMergeVar = flip elem $ map bindeeName mergevars
          mergevars = map fst merge

expReturns _ (AST.LoopOp op) =
  return $ extReturns $ loopOpExtType op

expReturns _ (Apply _ _ ret _) =
  return $ map funReturnsToExpReturns ret

expReturns look (If _ b1 b2 _ _) = do
  b1t <- bodyReturns look b1
  b2t <- bodyReturns look b2
  return $
    map funReturnsToExpReturns $
    generaliseReturns b1t b2t

bodyReturns :: Monad m =>
               (VName -> m MemSummary)
            -> Body
            -> m [FunReturns]
bodyReturns look body@(AST.Body _ bnds res) = do
  let boundHere = boundInBindings bnds
      inspect _ (Constant val _) =
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
            Just bindee -> return $ bindeeLore bindee

          case summary of
            Scalar ->
              fail "bodyReturns: inconsistent memory summary"

            MemSummary mem ixfun -> do
              let memname = identName mem

              unless (IxFun.isLinear ixfun) $
                fail "bodyReturns: cannot handle non-linear index function yet."

              if memname `HM.member` boundHere then do
                (i, memmap) <- get

                case HM.lookup memname memmap of
                  Nothing -> do
                    put (i+1, HM.insert memname (i+1) memmap)
                    return $ ReturnsNewBlock i

                  Just _ ->
                    fail "bodyReturns: same memory block used multiple times."
              else return $ ReturnsInBlock mem
        return $ ReturnsArray et shape u memsummary
  evalStateT (zipWithM inspect (bodyExtType body) $ resultSubExps res)
    (0, HM.empty)

boundInBindings :: [Binding] -> HM.HashMap VName PatBindee
boundInBindings [] = HM.empty
boundInBindings (bnd:bnds) =
  boundInBinding `HM.union` boundInBindings bnds
  where boundInBinding =
          HM.fromList
          [ (bindeeName bindee, bindee)
          | bindee <- patternBindees $ bindingPattern bnd
          ]
