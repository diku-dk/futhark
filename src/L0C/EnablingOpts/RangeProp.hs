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

-- only for the fix function
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

----------------------------------------

data RExp = RExp Exp | Pinf | Ninf
      deriving (Show, Eq)

type Range = (RExp, RExp)

type RangeDict = M.Map VName (Range, Sign)

data Sign = Neg
          -- ^ < 0
          | NonPos
          -- ^ <= 0
          | Zero
          -- ^ = 0
          | NonNeg
          -- ^ >= 0
          | Pos
          -- ^ > 0
          | AnySign
          -- ^ No idea about the sign
          deriving (Show, Eq, Ord)

data InequalityRelationship = RLT
                            | RLTE
                            | REQ
                            | RGTE
                            | RGT
                            | RUnknown
                            deriving (Show, Eq)

----------------------------------------

data RangeEnv = RangeEnv {
    dict  :: RangeDict
  }

newtype RangeM a = RangeM (ReaderT RangeEnv (Either EnablingOptError) a)
  deriving (MonadReader RangeEnv,
            Monad)

runRangeM :: RangeM a -> RangeEnv -> Either EnablingOptError a
runRangeM (RangeM a) = runReaderT a

badRangeM :: EnablingOptError -> RangeM a
badRangeM = RangeM . lift . Left

----------------------------------------

rangeUnknown :: (Range, Sign)
rangeUnknown = ( (Ninf, Pinf) , AnySign )

emptyRangeDict :: RangeDict
emptyRangeDict = M.empty


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
createRangeAndSign _ _ = rangeUnknown

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

calculateRExpSign :: RExp -> RangeM Sign
calculateRExpSign Pinf = return Pos
calculateRExpSign Ninf = return Neg
calculateRExpSign (RExp (Literal (IntVal v) _) )
  | v < 0     = return Neg
  | v == 0    = return Zero
  | otherwise = return Pos
calculateRExpSign (RExp (Var (Ident vname (Elem Int) p))) = do
  bnd <- asks $ M.lookup vname . dict
  case bnd of
    Just (_,sign) -> return sign
    Nothing       -> badRangeM $ RangePropError p $ "Identifier was not in range dict" ++ textual vname
calculateRExpSign _ = return AnySign

calculateRangeSign :: Range -> L.SrcLoc -> RangeM Sign
calculateRangeSign (lb,ub) p = do
  s1 <- calculateRExpSign lb
  s2 <- calculateRExpSign ub
  if s2 < s1 then badRangeM $ RangePropError p "Something like Pos, Neg"
  else case (s1,s2) of
    (Neg,Neg)       -> return Neg
    (NonPos,NonPos) -> return NonPos
    (Zero, Zero)    -> return Zero
    (NonNeg,NonNeg) -> return NonNeg
    (Pos,Pos)       -> return Pos
    (NonNeg, _)     -> return NonNeg
    (_, Zero)       -> return NonPos
    (Zero, _)       -> return NonNeg
    (Neg, NonPos)   -> return NonPos
    (Neg, _)        -> return AnySign
    (NonPos, _)     -> return AnySign
    (Pos, _)        -> return Pos

expToComparableRange :: Exp -> RangeM Range
expToComparableRange e = return (Ninf, Pinf)

----------------------------------------

simplExp :: Exp -> RangeM Exp
simplExp e =
    case simplify e of
      Left err -> badRangeM err
      Right e' -> return e'

substitute :: Ident -> Range -> RExp -> RangeM Range
substitute _ _ l@(RExp (Literal{})) = return (l,l)
substitute i r v@(RExp (Var e)) = return (if e == i
                                          then r
                                          else (v,v) )

substitute i r (RExp (BinOp Plus e1 e2 ty pos)) = do
    (e1lb, e1ub) <- substitute i r (RExp e1)
    (e2lb, e2ub) <- substitute i r (RExp e2)
    lb <- case (e1lb, e2lb) of
            (RExp e1,RExp e2) -> do le <- simplExp (BinOp Plus e1 e2 ty pos)
                                    return $ RExp le
            _ -> return Ninf
    ub <- case (e1ub, e2ub) of
            (RExp e1,RExp e2) -> do ue <- simplExp (BinOp Plus e1 e2 ty pos)
                                    return $ RExp ue
            _ -> return Pinf
    return (lb,ub)

substitute i r (RExp (BinOp Minus e1 e2 ty pos)) = do
    (e1lb, e1ub) <- substitute i r (RExp e1)
    (e2lb, e2ub) <- substitute i r (RExp e2)
    lb <- case (e1lb, e2lb) of
            (RExp e1,RExp e2) -> do le <- simplExp (BinOp Minus e1 e2 ty pos)
                                    return $ RExp le
            _ -> return Ninf
    ub <- case (e1ub, e2ub) of
            (RExp e1,RExp e2) -> do ue <- simplExp (BinOp Minus e1 e2 ty pos)
                                    return $ RExp ue
            _ -> return Pinf
    return (lb,ub)

substitute i r (RExp (BinOp Times e1 e2 ty pos)) = do
    (e1lb, e1ub) <- substitute i r (RExp e1)
    (e2lb, e2ub) <- substitute i r (RExp e2)
    case (e1lb, e1ub, e2lb, e2ub) of
      (RExp e1lb', RExp e1ub', RExp e2lb', RExp e2ub') -> do
        e1Sign <- calculateRangeSign(e1lb,e1ub) pos
        e2Sign <- calculateRangeSign(e2lb,e2ub) pos
        case (e1Sign, e2Sign) of
          (Zero,_) -> let z = RExp $ Literal (IntVal 0) pos in return (z,z)
          (_,Zero) -> let z = RExp $ Literal (IntVal 0) pos in return (z,z)
          (AnySign, _) -> return (Ninf, Pinf)
          (_, AnySign) -> return (Ninf, Pinf)
          _ -> case (isNeg e1Sign, isNeg e2Sign) of
                  (True,True)   -> do lb <- simplExp (BinOp Times e1ub' e2ub' ty pos)
                                      ub <- simplExp (BinOp Times e1lb' e2lb' ty pos)
                                      return (RExp lb, RExp ub)
                  (False,False) -> do lb <- simplExp (BinOp Times e1lb' e2lb' ty pos)
                                      ub <- simplExp (BinOp Times e1ub' e2ub' ty pos)
                                      return (RExp lb, RExp ub)
                  (True,False)  -> do lb <- simplExp (BinOp Times e1lb' e2ub' ty pos)
                                      ub <- simplExp (BinOp Times e1ub' e2lb' ty pos)
                                      return (RExp lb, RExp ub)
                  (False,True)  -> do lb <- simplExp (BinOp Times e1ub' e2lb' ty pos)
                                      ub <- simplExp (BinOp Times e1lb' e2ub' ty pos)
                                      return (RExp lb, RExp ub)
      _ -> return (Ninf, Pinf)

    where
      isNeg :: Sign -> Bool
      isNeg Neg = True
      isNeg NonPos = True
      isNeg _ = False

substitute _ _ _ = return (Ninf, Pinf)

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

createRExpInt n = RExp $ Literal (IntVal n) dummySrcLoc

xRange = (createRExpInt 1, createRExpInt 2)

xMultNeg2 = BinOp Times (Var x) (Literal (IntVal (-2)) dummySrcLoc) (Elem Int) dummySrcLoc
xMultXMultNeg2 = BinOp Times (Var x) xMultNeg2 (Elem Int) dummySrcLoc

asdf = do
  let env = RangeEnv { dict = emptyRangeDict }
  case runRangeM (substitute x xRange (RExp xMultXMultNeg2) ) env of
    Right r -> ppRange r
