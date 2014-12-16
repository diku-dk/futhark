{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Futhark.Representation.ExplicitMemory
       ( -- * The Lore definition
         ExplicitMemory
       , MemSummary (..)
       , MemReturn (..)
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
       , ResType
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
       , AST.ResTypeT(ResType)
       )
where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
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
          Pattern, PatBindee, Lambda, FunDec, FParam, ResType)

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
type ResType = AST.ResType ExplicitMemory

data MemSummary = MemSummary Ident IxFun.IxFun
                | Scalar
                deriving (Show)

instance Eq MemSummary where
  _ == _ = True

instance Ord MemSummary where
  _ `compare` _ = EQ

instance FreeIn MemSummary where
  freeIn (MemSummary ident _) = HS.singleton ident
  freeIn Scalar               = HS.empty

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

data MemReturn = ReturnsInBlock Ident
               | ReturnsInAnyBlock
               | ReturnsNewBlock Int
               | ReturnsScalar
               deriving (Eq, Ord, Show)

instance FreeIn MemReturn where
  freeIn (ReturnsInBlock v) = HS.singleton v
  freeIn _                  = mempty

instance Lore.ResType (ResTypeT MemReturn) where
  resTypeValues = map fst . resTypeElems

  simpleType = mapM simple . resTypeElems
    where simple (_, ReturnsNewBlock {}) = Nothing
          simple (t, _)                  = hasStaticShape t

  staticResType = extResType . staticShapes

  generaliseResTypes rt1 rt2 =
    let (ts1,attrs1) = unzip $ resTypeElems rt1
        (ts2,attrs2) = unzip $ resTypeElems rt2
        ts = generaliseExtTypes ts1 ts2
        i = startOfFreeIDRange ts
        attrs = evalState (zipWithM generaliseMemReturn attrs1 attrs2) i
    in AST.ResType $ zip ts attrs
    where generaliseMemReturn (ReturnsNewBlock _) _ = newBlock
          generaliseMemReturn _ (ReturnsNewBlock _) = newBlock
          generaliseMemReturn r               _     = return r

  extResType ts =
    AST.ResType $ evalState (mapM addAttr ts) $ startOfFreeIDRange ts
    where addAttr (Basic t) = return (Basic t, ReturnsScalar)
          addAttr (Mem sz)  = return (Mem sz,  ReturnsScalar)
          addAttr t
            | existential t = do i <- get
                                 put $ i + 1
                                 return (t, ReturnsNewBlock i)
            | otherwise = return (t, ReturnsInAnyBlock)

  bodyResType inaccessible (AST.ResType xs) =
    let (ts,attrs) = unzip xs
        ts'        = existentialiseExtTypes inaccessible ts
        attrs'     = evalState (zipWithM replaceAttr ts' attrs) (0,HM.empty,HM.empty)
    in AST.ResType $ zip ts' attrs'
    where replaceAttr _ (ReturnsInBlock v)
            | identName v `HS.member` inaccessible =
              replaceVar $ identName v
            | otherwise =
              return $ ReturnsInBlock v
          replaceAttr t ReturnsInAnyBlock
            | existential t = do
              (i, extmap, varmap) <- get
              put (i+1, extmap, varmap)
              return $ ReturnsNewBlock i
            | otherwise =
              return ReturnsInAnyBlock
          replaceAttr _ ReturnsScalar =
            return ReturnsScalar
          replaceAttr _ (ReturnsNewBlock j) = do
            (i, extmap, varmap) <- get
            case HM.lookup j extmap of
              Nothing   -> do put (i+1, HM.insert j i extmap, varmap)
                              return $ ReturnsNewBlock i
              Just repl -> return $ ReturnsNewBlock repl
          replaceVar name = do
            (i, extmap, varmap) <- get
            case HM.lookup name varmap of
              Nothing   -> do put (i+1, extmap, HM.insert name i varmap)
                              return $ ReturnsNewBlock i
              Just repl -> return $ ReturnsNewBlock repl

startOfFreeIDRange :: [ExtType] -> Int
startOfFreeIDRange = (1+) . HS.foldl' max 0 . shapeContext

newBlock :: State Int MemReturn
newBlock = do
  i <- get
  put (i+1)
  return $ ReturnsNewBlock i

instance Monoid (ResTypeT MemReturn) where
  mempty = AST.ResType []
  AST.ResType xs `mappend` AST.ResType ys =
    AST.ResType $ xs <> ys

instance Rename MemReturn where
  rename = return

instance Substitute MemReturn where
  substituteNames = const id

instance PP.Pretty (ResTypeT MemReturn) where
  ppr = PP.braces . PP.commasep . map pp . resTypeElems
    where pp (t, ReturnsScalar)     = PP.ppr t
          pp (t, ReturnsInBlock v)  = PP.ppr t <> PP.parens (PP.text $ pretty v)
          pp (t, ReturnsInAnyBlock) = PP.ppr t <> PP.text "@" <> PP.text "any"
          pp (t, ReturnsNewBlock i) = PP.ppr t <> PP.text "@" <> PP.text (show i)

instance Lore.Lore ExplicitMemory where
  type LetBound ExplicitMemory = MemSummary
  type FParam   ExplicitMemory = MemSummary
  type ResTypeAttr ExplicitMemory = MemReturn

  representative = ExplicitMemory

  loopResultContext _ res mergevars =
    let shapeContextIdents = loopShapeContext res $ map bindeeIdent mergevars
        memContextIdents = nub $ mapMaybe memIfNecessary res
        memSizeContextIdents = nub $ mapMaybe memSizeIfNecessary memContextIdents
    in memSizeContextIdents <> memContextIdents <> shapeContextIdents
    where memIfNecessary ident =
            case find ((==ident) . bindeeIdent) mergevars of
              Just fparam | MemSummary mem _ <- bindeeLore fparam ->
                Just mem
              _ ->
                Nothing
          memSizeIfNecessary ident =
            bindeeIdent <$> find ((==ident) . bindeeIdent) mergevars

  loopResType _ res mergevars =
    AST.ResType $ evalState (mapM typeWithAttr $
                             zip (map identName res) $
                             loopExtType res $ map bindeeIdent mergevars) 0
    where typeWithAttr (resname, t) =
            case (t,
                  bindeeLore <$> find ((==resname) . bindeeName) mergevars) of
              (Array {}, Just (MemSummary mem _))
                | isMergeVar resname -> do
                  i <- get
                  put $ i + 1
                  return (t, ReturnsNewBlock i)
                | otherwise ->
                  return (t, ReturnsInBlock mem)
              _ -> return (t, ReturnsScalar)
          isMergeVar = flip elem $ map bindeeName mergevars

instance TypeCheck.Checkable ExplicitMemory where
  checkExpLore = return
  checkBindingLore = checkMemSummary
  checkBodyLore = return
  checkFParamLore = checkMemSummary
  checkResType = mapM_ TypeCheck.checkExtType . resTypeValues
  matchPattern loc pat rt = do
    let (nmemsizes, nmems, nvalsizes) = analyseResType rt
        (memsizes, mems, valsizes,
         AST.Pattern valbindees) = dividePattern pat nmemsizes nmems nvalsizes
    checkMems memsizes mems
    checkVals mems valsizes valbindees
    where wrong s = TypeCheck.bad $ TypeError loc $
                    "Pattern\n" ++ pretty pat ++ "\ncannot match result type\n" ++
                    pretty rt ++ ":\n" ++ s
          mustBeEmpty _ [] = return ()
          mustBeEmpty s  _ = wrong $ "unused " ++ s ++ " bindees"
          checkMems memsizes mems =
            mustBeEmpty "memory block size" =<<
            execStateT (mapM_ checkMem mems) memsizes
          checkVals mems valsizes valbindees = do
            ((mems', _), (valsizes', _)) <-
              execStateT
              (zipWithM_ checkVal valbindees $ resTypeElems rt)
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
          checkVal valbindee (t,attr) = do
            zipWithM_ checkShape
              (shapeDims $ arrayShape $ bindeeType valbindee)
              (extShapeDims $ arrayShape t)
            checkMemReturn (bindeeType valbindee) (bindeeLore valbindee) attr

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

          checkMemReturn (Mem _) Scalar ReturnsScalar = return ()
          checkMemReturn (Basic _) Scalar ReturnsScalar = return ()
          checkMemReturn (Array {}) (MemSummary memv1 ixfun) (ReturnsInBlock memv2)
            | memv1 == memv2,
              IxFun.isLinear ixfun = return ()
          checkMemReturn (Array {}) (MemSummary {}) ReturnsInAnyBlock =
            return ()
          checkMemReturn (Array {}) (MemSummary memv ixfun) (ReturnsNewBlock i)
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
          checkMemReturn _ _ _ = lift $ wrong $ "Nonsensical memory summary\n" ++ show rt

extract :: Eq a => a -> [a] -> Maybe [a]
extract _ [] = Nothing
extract x (y:ys)
  | x == y    = Just ys
  | otherwise = (y:) <$> extract x ys

analyseResType :: ResType -> (Int, Int, Int)
analyseResType rt =
  let (memsizes, mems, valsizes) =
        foldl sumtriple (mempty,mempty,mempty) . map analyse . resTypeElems $ rt
  in (HS.size memsizes, HS.size mems, HS.size valsizes)
  where sumtriple (x1,y1,z1) (x2,y2,z2) = (x1 `mappend` x2,
                                           y1 `mappend` y2,
                                           z1 `mappend` z2)
        analyse (t,attr) = let (memsize,mem) = analyseAttr attr
                           in (memsize, mem, analyseType t)
        analyseType t = shapeContext [t]
        analyseAttr (ReturnsInBlock _)  = (mempty, mempty)
        analyseAttr (ReturnsInAnyBlock) = (mempty, mempty)
        analyseAttr (ReturnsNewBlock i) = (HS.singleton i, HS.singleton i)
        analyseAttr ReturnsScalar       =   (mempty, mempty)

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
