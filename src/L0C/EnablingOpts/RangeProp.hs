{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.RangeProp (
    -- * Range Data Types
    RangeDict
  , Inequality(..)
  , RangeInequality

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
  computedRange <- doSomeFixPointIteration (RExp e, RExp e)
  sign <- calculateRangeSign computedRange (L.SrcLoc $ L.locOf e)
  return ( computedRange , sign )
createRangeAndSign Nothing = return ( (Ninf, Pinf), Nothing )

-- TODO: Use Control.Monad.Fix ?
doSomeFixPointIteration :: Range -> RangeM Range
doSomeFixPointIteration range = do
  newRange <- replaceWithWholeDict range
  if newRange == range
  then return range
  else doSomeFixPointIteration newRange

replaceWithWholeDict :: Range -> RangeM Range
replaceWithWholeDict range = do
  dictAsList  <- liftM M.toAscList $ asks dict
  foldM foldingFun range dictAsList

  where
    foldingFun :: Range -> (VName , (Range, RangeSign)) -> RangeM Range
    foldingFun (a,b) (ident, (idRange,_)) = do
      (a',_) <- substitute ident idRange a
      (_,b') <- substitute ident idRange b
      return (a',b')

----------------------------------------
-- Range substitution
----------------------------------------

rangeCompare :: Exp -> Exp -> RangeM RangeInequality
rangeCompare e1 e2 = do
  let e1SrcLoc = L.SrcLoc $ L.locOf e1
  range <- expToComparableRange $ BinOp Minus e1 e2 (typeOf e1) e1SrcLoc
  sign <- calculateRangeSign range e1SrcLoc
  case sign of
    (Just Neg)     -> return $ Just ILT
    (Just NonPos)  -> return $ Just ILTE
    (Just Zero)    -> return $ Just IEQ
    (Just NonNeg)  -> return $ Just IGTE
    (Just Pos)     -> return $ Just IGT
    Nothing -> return Nothing

----------------------------------------


----------------------------------------
-- Does not work recursively?
---------------------------------------

-- TODO: make sanity check, that we don't have something like Pos, Neg ?
calculateRangeSign :: Range -> L.SrcLoc -> RangeM RangeSign
calculateRangeSign (lb,ub) p = do
  s1 <- determineRExpSign lb
  s2 <- determineRExpSign ub
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

determineRExpSign :: RExp -> RangeM RangeSign
determineRExpSign Pinf = return $ Just Pos
determineRExpSign Ninf = return $ Just Neg
determineRExpSign (RExp (Literal (IntVal v) _) )
  | v < 0     = return $ Just Neg
  | v == 0    = return $ Just Zero
  | otherwise = return $ Just Pos
determineRExpSign (RExp (Var (Ident vname (Elem Int) p))) = do
  bnd <- asks $ M.lookup vname . dict
  case bnd of
    Just (_,sign) -> return sign
    Nothing       -> badRangeM $ RangePropError p $
        "determineRExpSign: Identifier was not in range dict: " ++ textual vname
determineRExpSign _ = return Nothing


expToComparableRange :: Exp -> RangeM Range
expToComparableRange e = return (Ninf, Pinf)

----------------------------------------

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
  e1Sign <- calculateRangeSign(a,b) pos
  e2Sign <- calculateRangeSign(c,d) pos
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

substitute i r (RExp (Min e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- minRExp a c
  bd <- minRExp b d
  return (ac, bd)

  where
    minRExp :: RExp -> RExp -> RangeM RExp
    minRExp Pinf Ninf = badRangeM $ RangePropError pos "minRExp: Taking min of Pinf Ninf"
    minRExp Ninf Pinf = badRangeM $ RangePropError pos "minRExp: Taking min of Ninf Pinf"
    minRExp Pinf x = return x
    minRExp x Pinf = return x
    minRExp Ninf _ = return Ninf
    minRExp _ Ninf = return Ninf
    minRExp (RExp x) (RExp y) = liftM RExp $ simplExp (Min x y ty pos)

substitute i r (RExp (Max e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- maxRExp a c
  bd <- maxRExp b d
  return (ac, bd)

  where
    maxRExp :: RExp -> RExp -> RangeM RExp
    maxRExp Pinf Ninf = badRangeM $ RangePropError pos "maxRExp: Taking Max of Pinf Ninf"
    maxRExp Ninf Pinf = badRangeM $ RangePropError pos "maxRExp: Taking Max of Ninf Pinf"
    maxRExp Ninf x = return x
    maxRExp x Ninf = return x
    maxRExp Pinf _ = return Pinf
    maxRExp _ Pinf = return Pinf
    maxRExp (RExp x) (RExp y) = liftM RExp $ simplExp (Max x y ty pos)

-- Resolve nested let, example:
-- let x = (let y = 5 in y+3) in x+2
-- First we process y, settings it's range to [5:5]
-- Then we process x, encountering (let y = 5 in y+3)
-- when trying to find it's range
-- therefore we only need to look at the part after in, in this case y+3
substitute i r (RExp (LetPat _ _ inExp _)) = substitute i r (RExp inExp)

substitute _ _ _ = return (Ninf, Pinf)

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
ppSign Nothing = "Unknown"

ppDict :: RangeDict -> String
ppDict dict = foldr ((++) . (++ "\n") . ppDictElem) "" (M.toList $ M.delete dummyVName dict)
              where
                ppDictElem :: (VName, (Range, RangeSign)) -> String
                ppDictElem (vname, (range, sign)) =
                  escapeColorize Green (textual vname) ++ " " ++
                  escapeColorize Blue (ppRange range) ++ " " ++
                  escapeColorize Yellow (ppSign sign)

----------------------------------------
-- Helpers / Constants
----------------------------------------

createIntLit :: Int -> L.SrcLoc -> Exp
createIntLit n = Literal (IntVal n)

createRExpIntLit :: Int -> L.SrcLoc -> RExp
createRExpIntLit n pos = RExp $ createIntLit n pos

-- as the substitute function needs an identifier and range to work,
-- we have to supply a dummy value for it to work on the first variable passed to it.

dummyVName :: VName
dummyVName = ID (nameFromString "dummy",-1)

emptyRangeDict :: RangeDict
emptyRangeDict = M.singleton dummyVName ((Ninf,Pinf), Nothing)

----------------------------------------
-- TESTING
----------------------------------------

dummyPos = L.Pos "DummyPos" 0 0 0
dummySrcLoc = L.SrcLoc (L.Loc dummyPos dummyPos)

xId = ID (nameFromString "x",0)
x = Ident {identName = xId,
           identType = Elem Int,
           identSrcLoc = dummySrcLoc }

xRange = (createRExpIntLit 1 dummySrcLoc, createRExpIntLit 2 dummySrcLoc)

xMultNeg2 = BinOp Times (Var x) (Literal (IntVal (-2)) dummySrcLoc) (Elem Int) dummySrcLoc
xMultXMultNeg2 = BinOp Times (Var x) xMultNeg2 (Elem Int) dummySrcLoc


testRange = do
  let env = RangeEnv { dict = emptyRangeDict }
  case runRangeM (substitute xId xRange (RExp xMultXMultNeg2) ) env of
    Right r -> ppRange r
    Left _ -> error "Fail!"
