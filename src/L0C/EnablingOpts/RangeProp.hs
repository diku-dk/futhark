{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.RangeProp (
    -- * Range Data Types
    RangeDict
  , Inequality(..)
  , RangeInequality
  , rangeCompare
  , rangeCompareZero

    -- * Range Propagation
  , rangeProp
  , dummyUsingRangeProp
  , emptyRangeDict

)
where

import qualified Data.Loc as L
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Writer

import L0C.EnablingOpts.EnablingOptErrors

import L0C.L0
import L0C.EnablingOpts.Simplify

import Debug.Trace
import L0C.EscapeColor

import qualified Data.Traversable

----------------------------------------
-- Data types
----------------------------------------

data RExp = RExp Exp | Pinf | Ninf
    deriving (Show, Eq)

type Range = (RExp, RExp)

type RangeDict = M.Map VName (Range, RangeSign)

data Sign = Neg | NonPos | Zero | NonNeg | Pos
          deriving (Show, Eq, Ord)

type RangeSign = Maybe Sign

data Inequality = ILT | ILTE | IEQ | IGTE | IGT
                deriving (Show, Eq)

type RangeInequality = Maybe Inequality

----------------------------------------

data RangeEnv = RangeEnv {
    dict  :: RangeDict
  }

newtype RangeM a = RangeM (ReaderT RangeEnv (Either EnablingOptError) a)
  deriving (MonadReader RangeEnv, Monad)

----------------------------------------
-- Monad helpers
----------------------------------------

runRangeM :: RangeM a -> RangeEnv -> Either EnablingOptError a
runRangeM (RangeM a) = runReaderT a

badRangeM :: EnablingOptError -> RangeM a
badRangeM = RangeM . lift . Left

simplExp :: Exp -> RangeM Exp
simplExp e =
    case simplify e of
      Left err -> badRangeM err
      Right e' -> return e'


----------------------------------------
-- Range Propagation
----------------------------------------

dummyUsingRangeProp :: Prog -> RangeDict -> Either EnablingOptError Prog
dummyUsingRangeProp prog _ = return prog

rangeProp :: Prog -> Either EnablingOptError RangeDict
rangeProp prog = do
  let asList = filter (\(_,t,_) -> isElemInt t) $ allIdentsAsList prog
  newDict <- foldM keepAddingToRangeDict emptyRangeDict asList
  trace (prettyPrint prog ++ "\n" ++ ppDict newDict ++ "\n")
    return newDict
  where
    isElemInt (Elem Int) = True
    isElemInt _ = False

    keepAddingToRangeDict :: RangeDict -> (VName, DeclType, Maybe Exp) -> Either EnablingOptError RangeDict
    keepAddingToRangeDict acc (i,_,e) = do
      let env = RangeEnv { dict = acc }
      res <- runRangeM (createRangeAndSign e) env
      return $ acc `M.union` M.singleton i res

-- stolen from Traversals.progNames
allIdentsAsList :: Prog -> [ (VName, DeclType, Maybe Exp) ]
allIdentsAsList = execWriter . mapM funIdents . progFunctions
  where
    tellParam :: Parameter -> Writer [ (VName, DeclType, Maybe Exp) ] ()
    tellParam (Ident name tp _) =
      tell [(name, tp, Nothing)]

    tellLet :: Ident -> Exp ->  Writer [ (VName, DeclType, Maybe Exp) ] ()
    tellLet (Ident name tp _) toExp =
      tell [(name, toDecl tp, Just toExp)]

    idents = identityWalker {
                  walkOnExp = expIdents
                , walkOnLambda = lambdaIdents
                }

    funIdents (_, _, params, body, _) =
      mapM_ tellParam params >> expIdents body

    expIdents (LetPat (Id ident) toExp inExp _) =
      expIdents toExp >> tellLet ident toExp >> expIdents inExp
    --expIdents e@(LetWith dest _ _ _ _ _) =
      --tellParam dest >> walkExpM idents e
    --expIdents e@(DoLoop _ _ i _ _ _ _) =
      --tellParam i >> walkExpM idents e
    expIdents e = walkExpM idents e

    lambdaIdents (AnonymFun params body _ _) =
      mapM_ tellParam params >> expIdents body
    lambdaIdents (CurryFun _ exps _ _) =
          mapM_ expIdents exps

----------------------------------------

-- TODO: Right now it just simplifies as much as it can.
-- I think we would just like to keep the old range
createRangeAndSign :: Maybe Exp -> RangeM (Range, RangeSign)
createRangeAndSign (Just e)  = do
  simplifiedRange <- rangeSimplify (RExp e, RExp e)
  sign <- calculateRangeSign simplifiedRange
  return ( simplifiedRange , sign )
createRangeAndSign Nothing = return ( (Ninf, Pinf), Nothing )

----------------------------------------
-- Comparisons based on range dict
----------------------------------------

rangeCompare :: RangeDict -> Exp -> Exp -> Either EnablingOptError RangeInequality
rangeCompare rdict e1 e2 = do
  let env = RangeEnv { dict = rdict }
  runRangeM (rangeRExpCompare (RExp e1) (RExp e2) (L.srclocOf e1) ) env

-- Same as doing exp `rangeCompare` 0
rangeCompareZero :: RangeDict -> Exp -> Either EnablingOptError RangeInequality
rangeCompareZero rdict e = do
  let env = RangeEnv { dict = rdict }
  runRangeM (rangeRExpCompareZero $ RExp e) env

rangeRExpCompare :: RExp -> RExp -> L.SrcLoc -> RangeM RangeInequality
rangeRExpCompare Ninf Ninf _ = return $ Just IEQ
rangeRExpCompare Pinf Pinf _ = return $ Just IEQ
rangeRExpCompare Ninf _ _ = return $ Just ILT
rangeRExpCompare _ Pinf _ = return $ Just ILT
rangeRExpCompare Pinf _ _ = return $ Just IGT
rangeRExpCompare _ Ninf _ = return $ Just IGT
rangeRExpCompare (RExp e1) (RExp e2) _ =
  rangeRExpCompareZero . RExp $ BinOp Minus e1 e2 (typeOf e1) (L.srclocOf e1)

-- same as doing RExp `rangeRExpCompareZero` 0
rangeRExpCompareZero :: RExp -> RangeM RangeInequality
rangeRExpCompareZero Ninf = return $ Just ILT
rangeRExpCompareZero Pinf = return $ Just IGT
rangeRExpCompareZero e@(RExp _) = do
  sign <- determineRExpSign e
  case sign of
    (Just Neg)     -> return $ Just ILT
    (Just NonPos)  -> return $ Just ILTE
    (Just Zero)    -> return $ Just IEQ
    (Just NonNeg)  -> return $ Just IGTE
    (Just Pos)     -> return $ Just IGT
    Nothing        -> return Nothing

----------------------------------------
-- Making ranges comparable
----------------------------------------

-- Is the range currently in a state,
--   where we can say something about it's sign?
isComparable :: Range -> RangeM Bool
isComparable range = do
  sign <- atomicRangeSign range
  case sign of
    Nothing -> return False
    _       -> return True

-- Transform the range to a state, where we can
--   say something about it's sign
makeRangeComparable :: Range -> RangeM Range
makeRangeComparable range = do
  dictAsList  <- liftM M.toDescList $ asks dict
  range' <- foldM foldingFun range dictAsList
  isComp <- isComparable range'
  return ( if isComp then range' else (Ninf, Pinf) )

  where
    foldingFun :: Range -> (VName , (Range, RangeSign)) -> RangeM Range
    foldingFun (a,b) (ident, (idRange,_)) = do
      isComp <- isComparable (a,b)
      if isComp
      then return (a,b)
      else do (a',_) <- substitute ident idRange a
              (_,b') <- substitute ident idRange b
              -- Enable for seeing what steps are taken
              -- trace ("make " ++ ppRange(a,b) ++ " ~~> " ++ ppRange(a',b') ++ " by sub " ++ textual ident )
              return (a',b')

----------------------------------------
-- Calculate range signs
----------------------------------------

-- Calculates the sign for the range supplied,
--   by first making the range comparable
calculateRangeSign :: Range -> RangeM RangeSign
calculateRangeSign range = atomicRangeSign =<< makeRangeComparable range

-- Calculates the sign for the RExp supplied,
--   by first making the range (e,e) comparable
determineRExpSign :: RExp -> RangeM RangeSign
determineRExpSign e = calculateRangeSign (e,e)

-- Tries to calculate the sign for the range supplied
--   without making modifications to it.
-- ie will return Nothing for the range (1+2, 1+3)
-- TODO: make sanity check, that we don't have something like Pos, Neg ?
atomicRangeSign :: Range -> RangeM RangeSign
atomicRangeSign (lb,ub) = do
  s1 <- atomicRExpSign lb
  s2 <- atomicRExpSign ub
  if s1 == s2
  then return s1
  else case (s1,s2) of
    (_,Just Neg)     -> return $ Just Neg
    (_,Just NonPos)  -> return $ Just NonPos
    (_,Just Zero)    -> return $ Just NonPos
    (Just Zero,_)    -> return $ Just NonNeg
    (Just NonNeg,_)  -> return $ Just NonNeg
    (Just Pos,_)     -> return $ Just Pos
    _           -> return Nothing

  where
    atomicRExpSign :: RExp -> RangeM RangeSign
    atomicRExpSign Pinf = return $ Just Pos
    atomicRExpSign Ninf = return $ Just Neg
    atomicRExpSign (RExp (Literal (IntVal v) _) )
      | v < 0     = return $ Just Neg
      | v == 0    = return $ Just Zero
      | otherwise = return $ Just Pos
    atomicRExpSign (RExp (Literal _ pos) ) =
      badRangeM $ RangePropError pos "atomicRExpSign: Encountered non integer literal"
    atomicRExpSign (RExp (Var (Ident vname (Elem Int) p))) = do
      bnd <- asks $ M.lookup vname . dict
      case bnd of
        Just (_,sign) -> return sign
        Nothing       -> badRangeM $ RangePropError p $
            "atomicRExpSign: Identifier was not in range dict: " ++ textual vname
    atomicRExpSign _ = return Nothing

----------------------------------------
-- Range substitution
----------------------------------------

rangeSimplify :: Range -> RangeM Range
rangeSimplify (a,b) = do
  (a', _ ) <- substitute dummyVName (Ninf, Pinf) a
  (_ , b') <- substitute dummyVName (Ninf, Pinf) b
  return (a', b')

substitute :: VName -> Range -> RExp -> RangeM Range
substitute _ _ l@(RExp (Literal{})) = return (l,l)
substitute i r v@(RExp (Var e)) = return (if identName e == i then r else (v,v))
substitute i r (RExp (BinOp Plus e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- addRExp a c
  bd <- addRExp b d
  return(ac,bd)

  where
    addRExp :: RExp -> RExp -> RangeM RExp
    addRExp (RExp x) (RExp y) = liftM RExp $ simplExp (BinOp Plus x y ty pos)
    addRExp Ninf Pinf = badRangeM $ RangePropError pos "addRExp: Trying to add Ninf and Pinf"
    addRExp Pinf Ninf = badRangeM $ RangePropError pos "addRExp: Trying to add Ninf and Pinf"
    addRExp Pinf _ = return Pinf
    addRExp _ Pinf = return Pinf
    addRExp Ninf _ = return Ninf
    addRExp _ Ninf = return Ninf

substitute i r (RExp (BinOp Minus e1 e2 ty pos)) = do
    let min_1 = createIntLit (-1) pos
    let e2' = BinOp Times min_1 e2 ty pos
    substitute i r . RExp $ BinOp Plus e1 e2' ty pos

substitute i r (RExp (BinOp Times e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  e1Sign <- calculateRangeSign(a,b)
  e2Sign <- calculateRangeSign(c,d)
  case (e1Sign, e2Sign) of
    (Just Zero,_) -> let z = createRExpIntLit 0 pos in return (z,z)
    (_,Just Zero) -> let z = createRExpIntLit 0 pos in return (z,z)
    (Nothing, _)  -> return (Ninf, Pinf)
    (_, Nothing)  -> return (Ninf, Pinf)
    _             -> case (Just Zero < e1Sign, Just Zero < e2Sign) of
                       (True,True)   -> do ac <- multRExp a c
                                           bd <- multRExp b d
                                           return (ac,bd)
                       (False,False) -> do ac <- multRExp a c
                                           bd <- multRExp b d
                                           return (bd,ac)
                       (True,False)  -> do ad <- multRExp a d
                                           bc <- multRExp b c
                                           return (bc,ad)
                       (False,True)  -> do ad <- multRExp a d
                                           bc <- multRExp b c
                                           return (ad,bc)

  where
    multRExp :: RExp -> RExp -> RangeM RExp
    multRExp (RExp x) (RExp y) = liftM RExp $ simplExp (BinOp Times x y ty pos)
    multRExp Pinf x = do
      xSign <- determineRExpSign x
      case xSign of
        Nothing     -> badRangeM $ RangePropError pos "multRExp: Multiplying with Nothing"
        (Just Zero) -> return $ createRExpIntLit 0 pos
        (Just s)    -> return (if Zero < s then Pinf else Ninf)
    multRExp Ninf x = do
      xSign <- determineRExpSign x
      case xSign of
        Nothing     -> badRangeM $ RangePropError pos "multRExp: Multiplying with Nothing"
        (Just Zero) -> return $ createRExpIntLit 0 pos
        (Just s)    -> return (if Zero < s then Ninf else Pinf)
    multRExp x y = multRExp y x

substitute i r (RExp (Min e1 e2 _ pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- minRExp a c pos
  bd <- minRExp b d pos
  return (ac, bd)

substitute i r (RExp (Max e1 e2 _ pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- maxRExp a c pos
  bd <- maxRExp b d pos
  return (ac, bd)

-- Resolve nested let, example:
-- let x = (let y = 5 in y+3) in x+2
-- First we process y, settings it's range to [5:5]
-- Then we process x, encountering (let y = 5 in y+3)
-- when trying to find it's range
-- therefore we only need to look at the part after in, in this case y+3
substitute i r (RExp (LetPat _ _ inExp _)) = substitute i r (RExp inExp)

substitute _ _ _ = return (Ninf, Pinf)

----------------------------------------
-- Extract from cond
----------------------------------------

extractFromCond :: Exp -> RangeM ( M.Map VName (Maybe Range, Maybe Range) )
extractFromCond (Not e _) = do
  res <- extractFromCond e
  return $ M.map (\(a, b) -> (b, a)) res

extractFromCond (BinOp Less e1 e2 (Elem Int) _) =
  liftM2 M.union e1Info e2Info
  where
    e1Info = case e1 of
              (Var ident) -> do e2Minus1 <- simplExp $ expMinusOne e2
                                thenRange <- rangeIntersectIfValid (Ninf, RExp e2Minus1) e1
                                elseRange <- rangeIntersectIfValid (RExp e2, Pinf) e1
                                return $ M.singleton (identName ident) (thenRange, elseRange)
              _ -> return M.empty

    e2Info = case e2 of
              (Var ident) -> do e1Plus1 <- simplExp $ expPlusOne e1
                                thenRange <- rangeIntersectIfValid (RExp e1Plus1, Pinf) e2
                                elseRange <- rangeIntersectIfValid (Ninf, RExp e1) e2
                                return $ M.singleton (identName ident) (thenRange, elseRange)
              _ -> return M.empty

    rangeIntersectIfValid :: Range -> Exp -> RangeM (Maybe Range)
    rangeIntersectIfValid range e = do
      tmp <- rangeIntersect range (RExp e, RExp e) (L.srclocOf e)
      isTmpValid <- isValid tmp (L.srclocOf e)
      return $ if isTmpValid then Just tmp else Nothing

extractFromCond (BinOp Leq e1 e2 (Elem Int) pos) =
  extractFromCond $ Not (BinOp Less e2 e1 (Elem Int) pos) pos

extractFromCond (BinOp Equal e1 e2 (Elem Int) pos) =
  extractFromCond $ And (BinOp Leq e1 e2 (Elem Int) pos)
                        (BinOp Leq e2 e1 (Elem Int) pos)
                        pos

extractFromCond (And e1 e2 pos) = do
  e1Info <- extractFromCond e1
  e2Info <- extractFromCond e2
  unionWithM unionFunc e1Info e2Info
  where
    unionFunc (thenA, elseA) (thenB, elseB) = do
      thenRange <- intersectIfDeinfed thenA thenB pos
      elseRange <- unionIfDefined elseA elseB pos
      return (thenRange, elseRange)

extractFromCond (Or e1 e2 pos) = do
  e1Info <- extractFromCond e1
  e2Info <- extractFromCond e2
  unionWithM unionFunc e1Info e2Info
  where
    unionFunc (thenA, elseA) (thenB, elseB) = do
      thenRange <- unionIfDefined thenA thenB pos
      elseRange <- unionIfDefined elseA elseB pos
      return (thenRange, elseRange)

extractFromCond _ = return M.empty

----------------------------------------

unionIfDefined :: Maybe Range -> Maybe Range -> L.SrcLoc -> RangeM (Maybe Range)
unionIfDefined Nothing Nothing _ = return Nothing
unionIfDefined (Just a) (Just b) pos = do
  ab <- rangeUnion a b pos
  return $ Just ab
unionIfDefined (Just a) Nothing _ = return $ Just a
unionIfDefined Nothing (Just b) _ = return $ Just b

intersectIfDeinfed :: Maybe Range -> Maybe Range -> L.SrcLoc -> RangeM (Maybe Range)
intersectIfDeinfed (Just a) (Just b) pos = do
  ab <- rangeIntersect a b pos
  return $ Just ab
intersectIfDeinfed _ _ _ = return Nothing

isValid :: Range -> L.SrcLoc -> RangeM Bool
isValid (Pinf, Pinf) pos =
  badRangeM $ RangePropError pos "isValid: Illegal range [Pinf, Pinf]"
isValid (Ninf, Ninf) pos =
  badRangeM $ RangePropError pos "isValid: Illegal range [Ninf, Ninf]"
isValid (Pinf, Ninf) pos =
  badRangeM $ RangePropError pos "isValid: Illegal range [Pinf, Ninf]"
isValid (e1, e2) pos = do
  ineq <- rangeRExpCompare e1 e2 pos
  case ineq of
    (Just IGT)  -> return False
    _           -> return True

----------------------------------------
-- Monadic Data.Map functions
----------------------------------------

unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f mapA mapB =
  Data.Traversable.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

----------------------------------------
-- Union + Intersection
----------------------------------------

rangeUnion :: Range -> Range ->  L.SrcLoc -> RangeM Range
rangeUnion (a,b) (c,d) pos = do
  ac <- minRExp a c pos
  bd <- maxRExp b d pos
  return (ac, bd)

rangeIntersect :: Range -> Range ->  L.SrcLoc -> RangeM Range
rangeIntersect (a,b) (c,d) pos = do
  ac <- maxRExp a c pos
  bd <- minRExp b d pos
  return (ac, bd)

----------------------------------------
-- Helper functions
----------------------------------------

minRExp :: RExp -> RExp -> L.SrcLoc -> RangeM RExp
minRExp Pinf Ninf pos = badRangeM $ RangePropError pos "minRExp: Taking min of Pinf Ninf"
minRExp Ninf Pinf pos = badRangeM $ RangePropError pos "minRExp: Taking min of Ninf Pinf"
minRExp Pinf x _ = return x
minRExp x Pinf _ = return x
minRExp Ninf _ _ = return Ninf
minRExp _ Ninf _ = return Ninf
minRExp (RExp x) (RExp y) pos = do
  comp <- rangeRExpCompare (RExp x) (RExp y) pos
  case comp of
    (Just IGT) -> return (RExp y)
    (Just IEQ) -> return (RExp x)
    (Just ILT) -> return (RExp x)
    _ -> liftM RExp $ simplExp (Min x y (Elem Int) pos)

maxRExp :: RExp -> RExp -> L.SrcLoc -> RangeM RExp
maxRExp Pinf Ninf pos = badRangeM $ RangePropError pos "maxRExp: Taking Max of Pinf Ninf"
maxRExp Ninf Pinf pos = badRangeM $ RangePropError pos "maxRExp: Taking Max of Ninf Pinf"
maxRExp Ninf x _ = return x
maxRExp x Ninf _ = return x
maxRExp Pinf _ _ = return Pinf
maxRExp _ Pinf _ = return Pinf
maxRExp (RExp x) (RExp y) pos = do
  comp <- rangeRExpCompare (RExp x) (RExp y) pos
  case comp of
    (Just IGT) -> return (RExp x)
    (Just IEQ) -> return (RExp x)
    (Just ILT) -> return (RExp y)
    _ -> liftM RExp $ simplExp (Max x y (Elem Int) pos)

----------------------------------------
-- Constants
----------------------------------------

createIntLit :: Int -> L.SrcLoc -> Exp
createIntLit n = Literal (IntVal n)

createRExpIntLit :: Int -> L.SrcLoc -> RExp
createRExpIntLit n pos = RExp $ createIntLit n pos

expPlusOne :: Exp -> Exp
expPlusOne e =
  BinOp Plus e (createIntLit 1 (L.srclocOf e)) (Elem Int) (L.srclocOf e)

expMinusOne :: Exp -> Exp
expMinusOne e =
  BinOp Minus e (createIntLit 1 (L.srclocOf e)) (Elem Int) (L.srclocOf e)

-- as the substitute function needs an identifier and range to work,
-- we have to supply a dummy value for it to work on the first variable passed to it.

dummyVName :: VName
dummyVName = ID (nameFromString "dummy",-1)

emptyRangeDict :: RangeDict
emptyRangeDict = M.singleton dummyVName ((Ninf,Pinf), Nothing)

----------------------------------------
-- Pretty printing
----------------------------------------

ppRExp :: RExp -> String
ppRExp Ninf = "-Inf"
ppRExp Pinf = "Inf"
ppRExp (RExp e) = ppExp e

ppRange :: Range -> String
ppRange (l,u) = "[ " ++ ppRExp l ++ " , " ++ ppRExp u ++ " ]"

ppSign :: RangeSign -> String
ppSign (Just s) = show s
ppSign Nothing = "Any"

ppDict :: RangeDict -> String
ppDict rdict = foldr ((++) . (++ "\n") . ppDictElem) "" (M.toList $ M.delete dummyVName rdict)
              where
                ppDictElem :: (VName, (Range, RangeSign)) -> String
                ppDictElem (vname, (range, sign)) =
                  escapeColorize Green (textual vname) ++ " " ++
                  escapeColorize Blue (ppRange range) ++ " " ++
                  escapeColorize White (helper range) ++ " " ++
                  escapeColorize Yellow (ppSign sign)

                -- makes the range comparable, so it's understandable for us humans
                helper :: Range -> String
                helper range = do
                  let env = RangeEnv { dict = rdict }
                  case runRangeM (makeRangeComparable range) env of
                    Right asdf -> ppRange asdf
                    Left e     -> show e

----------------------------------------
-- TESTING
----------------------------------------

dummyPos = L.Pos "DummyPos" 0 0 0
dummySrcLoc = L.SrcLoc (L.Loc dummyPos dummyPos)

xId = ID (nameFromString "x",0)
x = Ident {identName = xId,
           identType = Elem Int,
           identSrcLoc = dummySrcLoc }

xRange = (createRExpIntLit 2 dummySrcLoc, createRExpIntLit 15 dummySrcLoc)

dictWithX = M.insert xId (xRange, Just Pos) emptyRangeDict

yId = ID (nameFromString "y",1)
y = Ident {identName = yId,
           identType = Elem Int,
           identSrcLoc = dummySrcLoc }

yRange = (createRExpIntLit 0 dummySrcLoc, createRExpIntLit 7 dummySrcLoc)

dictWithXY = M.insert yId (yRange, Just Pos) dictWithX

----------------------------------------
-- Simple makeRangeComparable test
----------------------------------------

testSimple = do
  let env = RangeEnv { dict = dictWithX }
  case runRangeM (makeRangeComparable myRange ) env of
    Right r -> ppRange r
    Left e -> error $ show e
  where
    tmp = RExp $ BinOp Minus (Var x) (Min (createIntLit 2 dummySrcLoc) (Var x) (Elem Int) dummySrcLoc) (Elem Int) dummySrcLoc
    myRange = (tmp,tmp)

----------------------------------------
-- Basic test of rangeCompare
----------------------------------------

comp5to0 = rangeCompareZero emptyRangeDict $ createIntLit 5 dummySrcLoc
comp4to5 = rangeCompare emptyRangeDict  (createIntLit 4 dummySrcLoc) (createIntLit 5 dummySrcLoc)
comp10to5 = rangeCompare emptyRangeDict (createIntLit 10 dummySrcLoc) (createIntLit 5 dummySrcLoc)

----------------------------------------
-- tests for isValid
----------------------------------------

isValidTests =
  mapM_ print [test0, test1, test2, test3, test4, test5]
  where
    testIsValidOnRange res range = do
      let env = RangeEnv { dict = dictWithX }
      case runRangeM (isValid range dummySrcLoc) env of
        Right b -> show (b == res)
        Left e -> error $ show e

    test0 = testIsValidOnRange True (RExp $ Var x, RExp $ Min (createIntLit 2 dummySrcLoc) (Var x) (Elem Int) dummySrcLoc)
    test1 = testIsValidOnRange True (RExp $ Var x, RExp $ createIntLit 2 dummySrcLoc)
    test2 = testIsValidOnRange True (RExp (createIntLit 2 dummySrcLoc), RExp (createIntLit 2 dummySrcLoc))
    test3 = testIsValidOnRange False (RExp (createIntLit 2 dummySrcLoc), RExp (createIntLit (-2) dummySrcLoc))
    test4 = testIsValidOnRange True (RExp (createIntLit (-2) dummySrcLoc), RExp (createIntLit 2 dummySrcLoc))
    test5 = testIsValidOnRange True (RExp (createIntLit (-2) dummySrcLoc), RExp (createIntLit (-2) dummySrcLoc))

----------------------------------------
-- Test for extractFromCond
----------------------------------------
createXTest' :: BinOp -> Ident -> Int -> String
createXTest' op ident n =
  createTest op (Var ident) (createIntLit n dummySrcLoc)

createXTest :: BinOp -> Int -> Ident -> String
createXTest op n ident =
  createTest op (createIntLit n dummySrcLoc) (Var ident)

createTest :: BinOp -> Exp -> Exp -> String
createTest op e1 e2 =
  let xExp = BinOp op e1 e2 (Elem Int) dummySrcLoc
  in testRange xExp

  where
    printCondInfoDictElem (vname, (ifRange, thenRange)) =
      textual vname ++ ": " ++ printMaybeRange ifRange ++ " " ++ printMaybeRange thenRange
    printMaybeRange (Just range) = ppRange range
    printMaybeRange Nothing = "[ Nothing ]"
    testRange e = do
         let env = RangeEnv { dict = dictWithXY }
         case runRangeM (extractFromCond e) env of
           Right ifdict -> foldr ((++) . (++ "\n") . printCondInfoDictElem) "" (M.toList ifdict)
           Left e -> error $ show e

lessTest = mapM_ putStrLn [createXTest Less 0 x, createXTest Less 5 x, createXTest Less 20 x]
lessEqTest = mapM_ putStrLn [createXTest Leq 0 x, createXTest Leq 5 x, createXTest Leq 20 x]
eqTest = mapM_ putStrLn [createXTest Equal 0 x, createXTest Equal 2 x, createXTest Equal 20 x]

lessTest2 = mapM_ putStrLn [createXTest' Less x 0, createXTest' Less x 5, createXTest' Less x 20]
lessEqTest2 = mapM_ putStrLn [createXTest' Leq x 0, createXTest' Leq x 5, createXTest' Leq x 20]
eqTest2 = mapM_ putStrLn [createXTest' Equal x 0, createXTest' Equal x 2, createXTest' Equal x 20]

xyTest = mapM_ putStrLn [createTest Less (Var x) (Var y), createTest Leq (Var x) (Var y), createTest Equal (Var x) (Var y)]
yxTest = mapM_ putStrLn [createTest Less (Var y) (Var x), createTest Leq (Var y) (Var x), createTest Equal (Var y) (Var x)]

