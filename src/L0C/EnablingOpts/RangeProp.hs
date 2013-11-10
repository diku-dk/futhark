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
  e' <- simplExp e
  let range = (RExp e', RExp e')
  sign <- calculateRangeSign range
  return ( range , sign )
createRangeAndSign Nothing = return ( (Ninf, Pinf), Nothing )

----------------------------------------
-- Comparisons based on range dict
----------------------------------------

rangeCompare :: RangeDict -> Exp -> Exp -> Either EnablingOptError RangeInequality
rangeCompare rdict e1 e2 = do
  let e1SrcLoc = L.SrcLoc $ L.locOf e1
  rangeCompareZero rdict $ BinOp Minus e2 e1 (typeOf e1) e1SrcLoc

-- Same as doing 0 `rangeCompare` exp
rangeCompareZero :: RangeDict -> Exp -> Either EnablingOptError RangeInequality
rangeCompareZero rdict e = do
  let env = RangeEnv { dict = rdict }
  sign <- runRangeM (determineRExpSign $ RExp e) env
  case sign of
    (Just Neg)     -> return $ Just IGT
    (Just NonPos)  -> return $ Just IGTE
    (Just Zero)    -> return $ Just IEQ
    (Just NonNeg)  -> return $ Just ILTE
    (Just Pos)     -> return $ Just ILT
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
                       -- [2:5] * [3:6] ~> [2*3 : 5*6]
                       (True,True)   -> do ac <- multRExp a c
                                           bd <- multRExp b d
                                           return (ac,bd)
                       -- [-5:-2] * [-6:-3] ~> [-2*-3 : -5*-6]
                       (False,False) -> do ac <- multRExp a c
                                           bd <- multRExp b d
                                           return (bd,ac)
                       -- [2:5] * [-6:-3] ~> [5*-6 : 2*-3]
                       (True,False)  -> do ad <- multRExp a d
                                           bc <- multRExp b c
                                           return (bc,ad)
                       -- [-5:-2] * [3:6] ~> [-5*6 : -2*3]
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

substitute i r (RExp (BinOp Divide e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  e1Sign <- calculateRangeSign(a,b)
  e2Sign <- calculateRangeSign(c,d)
  if canBeZero e1Sign || canBeZero e2Sign
  then return (Ninf, Pinf)
  else case (Just Zero < e1Sign, Just Zero < e2Sign) of
          -- [2:5] / [3:6] ~> [2/6 : 5/3]
          (True,True)   -> do ad <- divRExp a d
                              bc <- divRExp b c
                              return (ad,bc)
          -- [-5:-2] / [-6:-3] ~> [-2/-6 : -5/-3]
          (False,False) -> do ad <- divRExp a d
                              bc <- divRExp b c
                              return (bc,ad)
          -- [2:5] / [-6:-3] ~> [5/-3 : 2/-6]
          (True,False)  -> do ac <- divRExp a c
                              bd <- divRExp b d
                              return (ac,bd)
          -- [-5:-2] / [3:6] ~> [-5/3 : -2/6]
          (False,True)  -> do ac <- divRExp a c
                              bd <- divRExp b d
                              return (bd,ac)

    where
      canBeZero :: RangeSign -> Bool
      canBeZero (Just Neg) = False
      canBeZero (Just Pos) = False
      canBeZero _ = True

      divRExp :: RExp -> RExp -> RangeM RExp
      divRExp (RExp x) (RExp y) = liftM RExp $ simplExp (BinOp Divide x y ty pos)
      divRExp Pinf x = do
        xSign <- determineRExpSign x
        case xSign of
          (Just Pos) -> return Pinf
          (Just Neg) -> return Ninf
          _     -> badRangeM $ RangePropError pos "divRExp: Dividing with something that could be 0"
      divRExp Ninf x = do
        xSign <- determineRExpSign x
        case xSign of
          (Just Pos) -> return Ninf
          (Just Neg) -> return Pinf
          _     -> badRangeM $ RangePropError pos "divRExp: Dividing with something that could be 0"
      divRExp x y = divRExp y x

substitute i r (RExp (BinOp Pow e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  case (c,d) of
    ( RExp (Literal (IntVal v) _) , RExp (Literal (IntVal v') _) )
      | v /= v' -> return (Ninf, Pinf)
      | even v -> do
          aSign <- determineRExpSign a
          bSign <- determineRExpSign b
          case (Just Zero <= aSign, Nothing <= bSign && bSign <= Just Zero) of
            (True, _)    -> do av <- powRExp a v
                               bv <- powRExp b v
                               return (av, bv)
            (_, False)   -> do av <- powRExp a v
                               bv <- powRExp b v
                               return (bv, av)
            _            -> return (Ninf, Pinf)
      | otherwise -> do
          av <- powRExp a v
          bv <- powRExp b v
          return (av, bv)

    _ -> return (Ninf, Pinf)

  where
    powRExp :: RExp -> Int -> RangeM RExp
    powRExp _ 0 = return $ createRExpIntLit 1 pos
    powRExp Pinf _ = return Pinf
    powRExp Ninf _ = return Ninf
    divRExp (RExp x) v = liftM RExp $ simplExp (BinOp Pow x (createIntLit v pos) ty pos)

substitute i r (RExp (Min e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- minRExp a c pos
  bd <- minRExp b d pos
  return (ac, bd)

substitute i r (RExp (Max e1 e2 ty pos)) = do
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
minRExp (RExp x) (RExp y) pos = liftM RExp $ simplExp (Min x y (Elem Int) pos)

maxRExp :: RExp -> RExp -> L.SrcLoc -> RangeM RExp
maxRExp Pinf Ninf pos = badRangeM $ RangePropError pos "maxRExp: Taking Max of Pinf Ninf"
maxRExp Ninf Pinf pos = badRangeM $ RangePropError pos "maxRExp: Taking Max of Ninf Pinf"
maxRExp Ninf x _ = return x
maxRExp x Ninf _ = return x
maxRExp Pinf _ _ = return Pinf
maxRExp _ Pinf _ = return Pinf
maxRExp (RExp x) (RExp y) pos = liftM RExp $ simplExp (Max x y (Elem Int) pos)

----------------------------------------
-- Constants
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
                  escapeColorize White (ppRangeAsComp range) ++ " " ++
                  escapeColorize Yellow (ppSign sign)

                -- makes the range comparable, so it's understandable for us humans
                ppRangeAsComp :: Range -> String
                ppRangeAsComp range = do
                  let env = RangeEnv { dict = rdict }
                  case runRangeM (makeRangeComparable range) env of
                    Right range' -> ppRange range'
                    Left err     -> show err

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

comp0to5 = rangeCompareZero emptyRangeDict $ createIntLit 5 dummySrcLoc
comp4to5 = rangeCompare emptyRangeDict  (createIntLit 4 dummySrcLoc) (createIntLit 5 dummySrcLoc)
comp10to5 = rangeCompare emptyRangeDict (createIntLit 10 dummySrcLoc) (createIntLit 5 dummySrcLoc)
