{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.RangeProp (
    -- * Range Data Types
    RangeDict
  , InequalityRelationship(..)

    -- * Range Propagation
  , rangeProp
  , dummyUsingRangeProp
  , emptyRangeDict

)
where

import qualified Data.Loc as L
import qualified Data.Map as M

import Control.Monad.Fix
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

type RangeDict = M.Map VName (Range, Sign)

data Sign = Neg | NonPos | Zero | NonNeg | Pos | AnySign
          deriving (Show, Eq, Ord)

data InequalityRelationship = RLT | RLTE | REQ | RGTE | RGT | RUnknown
                            deriving (Show, Eq)


data RangeEnv = RangeEnv {
    dict  :: RangeDict
  }

newtype RangeM a = RangeM (ReaderT RangeEnv (Either EnablingOptError) a)
  deriving (MonadReader RangeEnv, Monad)

----------------------------------------
--
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

emptyRangeDict :: RangeDict
emptyRangeDict = M.empty


----------------------------------------
-- Range Propagation
----------------------------------------

dummyUsingRangeProp :: Prog -> RangeDict -> Either EnablingOptError Prog
dummyUsingRangeProp prog _ = return prog

rangeProp :: Prog -> Either EnablingOptError RangeDict
rangeProp prog = do
    let asList = filter (\(_,t,_) -> isElemInt t) $ allIdentsAsList prog
        hahaBadName = foldl keepAddingToRangeDict M.empty asList
    trace ( foldr ((++) . (++ "\n") . ppDictElem) "" (M.toList hahaBadName) ++ "\n" ++
           prettyPrint prog ++ "\n"
           ) return hahaBadName
    where isElemInt (Elem Int) = True
          isElemInt _ = False
          keepAddingToRangeDict acc (i,_,e) =
            acc `M.union` M.singleton i (createRangeAndSign acc e)

-- stolen from Traversals.progNames
allIdentsAsList :: Prog -> [ (VName, DeclType, Maybe Exp) ]
allIdentsAsList = execWriter . mapM funNames . progFunctions
  where
    tellParam :: Parameter -> Writer [ (VName, DeclType, Maybe Exp) ] ()
    tellParam (Ident name tp _) =
      tell [(name, tp, Nothing)]

    tellLet :: Ident -> Exp ->  Writer [ (VName, DeclType, Maybe Exp) ] ()
    tellLet (Ident name tp _) toExp =
      tell [(name, toDecl tp, Just toExp)]

    names = identityWalker {
                  walkOnExp = expNames
                , walkOnLambda = lambdaNames
                }

    funNames (_, _, params, body, _) =
      mapM_ tellParam params >> expNames body

    expNames e@(LetPat (Id ident) toExp _ _) =
      tellLet ident toExp >> walkExpM names e
    --expNames e@(LetWith dest _ _ _ _ _) =
      --tellParam dest >> walkExpM names e
    --expNames e@(DoLoop _ _ i _ _ _ _) =
      --tellParam i >> walkExpM names e
    expNames e = walkExpM names e

    lambdaNames (AnonymFun params body _ _) =
      mapM_ tellParam params >> expNames body
    lambdaNames (CurryFun _ exps _ _) =
          mapM_ expNames exps

----------------------------------------

createRangeAndSign :: RangeDict -> Maybe Exp -> (Range, Sign)
createRangeAndSign _ _ = ((Ninf,Pinf),AnySign)

----------------------------------------
-- Range substitution
----------------------------------------

rangeCompare :: Exp -> Exp -> RangeM InequalityRelationship
rangeCompare e1 e2 = do
  let e1SrcLoc = L.SrcLoc $ L.locOf e1
  range <- expToComparableRange $ BinOp Minus e1 e2 (typeOf e1) e1SrcLoc
  sign <- calculateRangeSign range e1SrcLoc
  case sign of
    Neg     -> return RLT
    NonPos  -> return RLTE
    Zero    -> return REQ
    NonNeg  -> return RGTE
    Pos     -> return RGT
    AnySign -> return RUnknown

----------------------------------------


----------------------------------------
-- Does not work recursively?
---------------------------------------

calculateRangeSign :: Range -> L.SrcLoc -> RangeM Sign
calculateRangeSign (lb,ub) p = do
  s1 <- determineRExpSign lb
  s2 <- determineRExpSign ub
  if s2 < s1
  then badRangeM $ RangePropError p "Something like Pos, Neg"
  else if s1 == s2
  then return s1
  else case (s1,s2) of
    (_,Neg)     -> return Neg
    (_,NonPos)  -> return NonPos
    (_,Zero)    -> return NonPos
    (Pos,_)     -> return Pos
    (NonNeg,_)  -> return NonNeg
    (Zero,_)    -> return NonNeg
    _           -> return AnySign

determineRExpSign :: RExp -> RangeM Sign
determineRExpSign Pinf = return Pos
determineRExpSign Ninf = return Neg
determineRExpSign (RExp (Literal (IntVal v) _) )
  | v < 0     = return Neg
  | v == 0    = return Zero
  | otherwise = return Pos
determineRExpSign (RExp (Var (Ident vname (Elem Int) p))) = do
  bnd <- asks $ M.lookup vname . dict
  case bnd of
    Just (_,sign) -> return sign
    Nothing       -> badRangeM $ RangePropError p $
        "Identifier was not in range dict" ++ textual vname
determineRExpSign _ = return AnySign


expToComparableRange :: Exp -> RangeM Range
expToComparableRange e = return (Ninf, Pinf)

----------------------------------------

substitute :: Ident -> Range -> RExp -> RangeM Range
substitute _ _ l@(RExp (Literal{})) = return (l,l)
substitute i r v@(RExp (Var e)) = return (if e == i then r else (v,v))
substitute i r (RExp (BinOp Plus e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  ac <- addRExp a c
  bd <- addRExp b d
  return(ac,bd)

  where
    addRExp :: RExp -> RExp -> RangeM RExp
    addRExp (RExp x) (RExp y) = liftM RExp $ simplExp (BinOp Plus x y ty pos)
    addRExp Ninf Pinf = badRangeM $ RangePropError pos "Trying to add Ninf and Pinf"
    addRExp Pinf Ninf = badRangeM $ RangePropError pos "Trying to add Ninf and Pinf"
    addRExp Pinf _ = return Pinf
    addRExp _ Pinf = return Pinf
    addRExp Ninf _ = return Ninf
    addRExp _ Ninf = return Ninf

substitute i r (RExp (BinOp Minus e1 e2 ty pos)) = do
    let min_1 = Literal (IntVal (-1)) pos
    let e2' = BinOp Times min_1 e2 ty pos
    substitute i r . RExp $ BinOp Plus e1 e2' ty pos

substitute i r (RExp (BinOp Times e1 e2 ty pos)) = do
  (a, b) <- substitute i r (RExp e1)
  (c, d) <- substitute i r (RExp e2)
  e1Sign <- calculateRangeSign(a,b) pos
  e2Sign <- calculateRangeSign(c,d) pos
  case (e1Sign, e2Sign) of
    (Zero,_)     -> let z = RExp $ Literal (IntVal 0) pos in return (z,z)
    (_,Zero)     -> let z = RExp $ Literal (IntVal 0) pos in return (z,z)
    (AnySign, _) -> return (Ninf, Pinf)
    (_, AnySign) -> return (Ninf, Pinf)
    _            -> case (isPos e1Sign, isPos e2Sign) of
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
    isPos :: Sign -> Bool
    isPos Pos = True
    isPos NonNeg = True
    isPos Neg = False
    isPos NonPos = False
    isPos _ = error "isPos on Zero or AnySign"

    multRExp :: RExp -> RExp -> RangeM RExp
    multRExp (RExp x) (RExp y) = liftM RExp $ simplExp (BinOp Times x y ty pos)
    multRExp Pinf x = do
      xSign <- determineRExpSign x
      case xSign of
        Zero -> return $ createRExpInt 0 pos
        AnySign -> badRangeM $ RangePropError pos "Multiplying with AnySign"
        _ -> return (if isPos xSign then Pinf else Ninf)
    multRExp Ninf x = do
      xSign <- determineRExpSign x
      case xSign of
        Zero -> return $ createRExpInt 0 pos
        AnySign -> badRangeM $ RangePropError pos "Multiplying with AnySign"
        _ -> return (if isPos xSign then Ninf else Pinf)
    multRExp x y = multRExp y x

substitute _ _ _ = return (Ninf, Pinf)

----------------------------------------
-- Pretty printing
----------------------------------------

ppRExp :: RExp -> String
ppRExp Ninf = "-Inf"
ppRExp Pinf = "-Pinf"
ppRExp (RExp e) = ppExp e

ppRange :: Range -> String
ppRange (l,u) = "[ " ++ ppRExp l ++ " , " ++ ppRExp u ++ " ]"

ppDictElem :: (VName, (Range, Sign)) -> String
ppDictElem (vname, (range, sign)) =
  escapeColorize Green (textual vname) ++ " " ++
  escapeColorize Blue (show range) ++ " " ++
  escapeColorize Yellow (show sign)

----------------------------------------
-- Helper
----------------------------------------

createRExpInt :: Int -> L.SrcLoc -> RExp
createRExpInt n pos = RExp $ Literal (IntVal n) pos

----------------------------------------
-- TESTING
----------------------------------------

res = substitute x ra rexp

dummyPos = L.Pos "DummyPos" 10 0 0
dummySrcLoc = L.SrcLoc (L.Loc dummyPos dummyPos)

x = Ident {identName = ID (nameFromString "x",0),
                 identType = Elem Int,
                 identSrcLoc = dummySrcLoc }

y = Ident {identName = ID (nameFromString "y",1),
                 identType = Elem Int,
                 identSrcLoc = dummySrcLoc }

ra = (RExp $ Literal (IntVal 1) dummySrcLoc,
      RExp $ Var y)

rexp =
    let x' = Var x in
    --let x'' = BinOp Pow (Var x) (Literal (IntVal 2) dummySrcLoc) (Elem Int) dummySrcLoc in
    let x'' = Literal (IntVal 2) dummySrcLoc in
    RExp $ BinOp Plus x'' x' (Elem Int) dummySrcLoc

xRange = (createRExpInt 1 dummySrcLoc, createRExpInt 2 dummySrcLoc)

xMultNeg2 = BinOp Times (Var x) (Literal (IntVal (-2)) dummySrcLoc) (Elem Int) dummySrcLoc
xMultXMultNeg2 = BinOp Times (Var x) xMultNeg2 (Elem Int) dummySrcLoc

asdf = do
  let env = RangeEnv { dict = emptyRangeDict }
  case runRangeM (substitute x xRange (RExp xMultXMultNeg2) ) env of
    Right r -> ppRange r
