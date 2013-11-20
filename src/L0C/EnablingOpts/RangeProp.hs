{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.RangeProp (
    rangeProp
)
where

import qualified Data.Loc as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List
import Data.Maybe

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative

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

type RangeDictInfo = (Range, Range, S.Set VName)
type RangeDict = M.Map VName RangeDictInfo

data RangeSign = AnySign | Neg | NonPos | Zero | NonNeg | Pos
          deriving (Show, Eq, Ord)

data RangeInequality = IANY | ILT | ILTE | IEQ | IGTE | IGT
                deriving (Show, Eq, Ord)

----------------------------------------

data RangeEnv = RangeEnv {
    dict    :: RangeDict
  }

newtype RangeM a = RangeM (ReaderT RangeEnv (Either EnablingOptError) a)
  deriving (MonadReader RangeEnv, Monad, Applicative, Functor)

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

rangeProp :: Prog -> Either EnablingOptError Prog
rangeProp prog = do
    let funs = trace ("RANGE PROP START\n" ++ prettyPrint prog) progFunctions prog
    res <- mapM rangePropFun funs
    return $ Prog res
  where
    rangePropFun (fname, rettype, params, body, pos) = do
        let env = RangeEnv { dict = foldl tellParam emptyRangeDict params }
        body' <- runRangeM (rangePropExp body) env
        return (fname, rettype, params, body', pos)

    tellParam :: RangeDict -> Parameter -> RangeDict
    tellParam rdict (Ident vname (Elem Int) _) = M.insert vname noInfo rdict
    tellParam rdict _ = rdict

    rangePropMapper = identityMapper { mapOnExp = rangePropExp }

    rangePropExp :: Exp -> RangeM Exp
    rangePropExp (LetPat i@(Id (Ident vname (Elem Int) _)) toExp inExp pos) = do
      toExp' <- rangePropExp toExp
      info <- createRangeInfo toExp'
      let debugText = unlines [ escapeColorize Black (locStr pos)
                              , escapeColorize Red ("----- LetPat " ++ textual vname ++ " -----")
                              , ppDict (M.singleton vname info)
                              ]
      inExp' <- trace debugText $ mergeRangeEnvWithDict (M.singleton vname info) $ rangePropExp inExp
      return $ LetPat i toExp' inExp' pos

    rangePropExp (If cond thenE elseE ty pos) = do
      cond' <- rangePropExp cond
      (thenInfo, elseInfo) <- realExtractFromCond cond'
      let debugText = unlines [ escapeColorize Black (locStr pos)
                              , escapeColorize Red "----- If ----- "
                              , ppExp cond
                              , ""
                              , "Then:"
                              , ppDict thenInfo
                              , "Else:"
                              , ppDict elseInfo
                              , escapeColorize Red "----- ^^ -----"
                              ]
      thenE' <- trace debugText $ mergeRangeEnvWithDict thenInfo $ rangePropExp thenE
      elseE' <- mergeRangeEnvWithDict elseInfo $ rangePropExp elseE
      return $ If cond' thenE' elseE' ty pos

    rangePropExp (BinOp Less e1 e2 ty pos) = do
      e1' <- rangePropExp e1
      e2' <- rangePropExp e2
      if typeOf e1 /= Elem Int then return $ BinOp Less e1' e2' ty pos
      else do
        ineq <- rangeCompare (RExp e1) (RExp e2) pos
        case () of _ | ineq == ILT -> return $ Literal (LogVal True) pos
                     | ineq >= IEQ -> return $ Literal (LogVal False) pos
                     | otherwise -> return $ BinOp Less e1' e2' ty pos

    rangePropExp e =
      mapExpM rangePropMapper e

mergeRangeEnvWithDict :: RangeDict -> RangeM a -> RangeM a
mergeRangeEnvWithDict newDict = local (\env -> env { dict = M.union newDict $ dict env })

----------------------------------------

createRangeInfo :: Exp -> RangeM RangeDictInfo
createRangeInfo e = do
  e' <- simplExp e
  let symbolic = (RExp e', RExp e')
  comp <- makeRangeComparable symbolic
  let depend = varsUsedInExp e'
  return (symbolic, comp, depend)

----------------------------------------
-- Comparisons based on range dict
----------------------------------------

rangeCompare :: RExp -> RExp -> L.SrcLoc -> RangeM RangeInequality
rangeCompare Ninf Ninf _ = return IEQ
rangeCompare Pinf Pinf _ = return IEQ
rangeCompare Ninf _ _ = return ILT
rangeCompare _ Pinf _ = return ILT
rangeCompare Pinf _ _ = return IGT
rangeCompare _ Ninf _ = return IGT
rangeCompare (RExp e1) (RExp e2) _ =
  rangeCompareZero . RExp $ BinOp Minus e1 e2 (typeOf e1) (L.srclocOf e1)

-- same as doing RExp `rangeCompareZero` 0
rangeCompareZero :: RExp -> RangeM RangeInequality
rangeCompareZero Ninf = return ILT
rangeCompareZero Pinf = return IGT
rangeCompareZero re@(RExp e) = do
  sign <- calculateRExpSign re (L.srclocOf e)
  case sign of
    Neg     -> return ILT
    NonPos  -> return ILTE
    Zero    -> return IEQ
    NonNeg  -> return IGTE
    Pos     -> return IGT
    AnySign -> return IANY

----------------------------------------
-- Making ranges comparable
----------------------------------------

-- Is the range currently in a state,
--   where we can say something about it's sign?
isComparable :: Range -> RangeM Bool
isComparable (Ninf, Pinf) = return True
isComparable range = do
  sign <- atomicRangeSign range
  return $ isJust sign

-- Transform the range to a state, where we can
--   say something about it's sign
makeRangeComparable :: Range -> RangeM Range
makeRangeComparable (Ninf, Pinf) = return (Ninf, Pinf)
makeRangeComparable range = do
  dictAsList  <- --trace ("- makeComp "++ ppRange range)
                 liftM M.keys $ asks dict
  foldingFun range (Data.List.sortBy (flip compare) dictAsList)

  where
    foldingFun :: Range -> [VName] -> RangeM Range
    foldingFun (a,b) [] = --trace ("+ makeEndOfList " ++ ppRange(a,b))
                          return (a,b)
    foldingFun (a,b) (ident : rest) = do
      isComp <- isComparable (a,b)
      if isComp
      then --trace("+ makeIsComp " ++ ppRange(a,b))
           return (a,b)
      else if a == b
      then do (a', b') <- substitute ident a
              --trace ("# make (eq) " ++ ppRExp a ++ " ~~> " ++ ppRange(a',b') ++ " by sub " ++ textual ident )
              foldingFun (a',b') rest
      else do (a',_) <- substitute ident a
              (_,b') <- substitute ident b
              --trace ("# make " ++ ppRange(a,b) ++ " ~~> " ++ ppRange(a',b') ++ " by sub " ++ textual ident )
              foldingFun (a',b') rest

varsUsedInExp :: Exp -> S.Set VName
varsUsedInExp ex = execWriter $ expVars ex
  where
    vars = identityWalker { walkOnExp = expVars }

    expVars e@(Var ident ) =
      tell (S.singleton $ identName ident) >> walkExpM vars e

    expVars e = walkExpM vars e

----------------------------------------
-- Calculate range signs
----------------------------------------

-- Calculates the sign for the range supplied,
--   by first making the range comparable
calculateRangeSign :: Range -> L.SrcLoc ->  RangeM RangeSign
calculateRangeSign range p = do
  atomSign <- atomicRangeSign =<< {-trace ("* calc range sign " ++ ppRange range)-} makeRangeComparable range
  case atomSign of
    Just s -> return s
    _ -> badRangeM $ RangePropError p "calculateRangeSign: Nothing returned by atomicRangeSign =<< makeRangeComparable"

-- Calculates the sign for the RExp supplied,
--   by first making the range (e,e) comparable
calculateRExpSign :: RExp -> L.SrcLoc -> RangeM RangeSign
calculateRExpSign e = calculateRangeSign (e,e)

-- Tries to calculate the sign for the range supplied
--   without making modifications to it.
-- ie will return Nothing for the range (1+2, 1+3)
-- TODO: make sanity check, that we don't have something like Pos, Neg ?
atomicRangeSign :: Range -> RangeM (Maybe RangeSign)
atomicRangeSign (lb,ub) = do
  s1 <- atomicRExpSign lb
  s2 <- atomicRExpSign ub
  if s1 == s2
  then return s1
  else case (s1, s2) of
    (_, Just Neg)     -> return $ Just Neg
    (_, Just NonPos)  -> return $ Just NonPos
    (_, Just Zero)    -> return $ Just NonPos
    (Just Zero, _)    -> return $ Just NonNeg
    (Just NonNeg, _)  -> return $ Just NonNeg
    (Just Pos, _)     -> return $ Just Pos
    (Just _, Just _)  -> return $ Just AnySign
    _                 -> return Nothing

atomicRExpSign :: RExp -> RangeM (Maybe RangeSign)
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
    Just (_,comp,_) -> atomicRangeSign comp
    Nothing       -> badRangeM $ RangePropError p $
        "atomicRExpSign: Identifier was not in range dict: " ++ textual vname
atomicRExpSign _ = return Nothing

----------------------------------------
-- Range substitution
----------------------------------------

substitute :: VName -> RExp -> RangeM Range
substitute _ l@(RExp (Literal{})) = return (l,l)
substitute i v@(RExp (Var (Ident vname _ p))) =
  if vname /= i then return (v,v)
  else do
    bnd <- asks $ M.lookup vname . dict
    case bnd of
      Just (range,_,_) -> return range
      Nothing       -> badRangeM $ RangePropError p $ "substitute: Identifier was not in range dict: " ++ textual vname

substitute i (RExp (BinOp Plus e1 e2 ty pos)) = do
  (a, b) <- substitute i (RExp e1)
  (c, d) <- substitute i (RExp e2)
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

substitute i (RExp (BinOp Minus e1 e2 ty pos)) = do
    let min_1 = createIntLit (-1) pos
    let e2' = BinOp Times min_1 e2 ty pos
    substitute i . RExp $ BinOp Plus e1 e2' ty pos

substitute i (RExp (BinOp Times e1 e2 ty pos)) = do
  (a, b) <- substitute i (RExp e1)
  (c, d) <- substitute i (RExp e2)
  e1Sign <- calculateRangeSign(a,b) pos
  e2Sign <- calculateRangeSign(c,d) pos

  lb <- liftM (fromMaybe Ninf) $ possibleLBTerm e1Sign (a,b) e2Sign (c,d)
  ub <- liftM (fromMaybe Pinf) $ possibleUBTerm e1Sign (a,b) e2Sign (c,d)
  return (lb,ub)

  where
      -- General case:
    -- [a:b]   * [c:d]   ~> [min(ac,ad,bc,bd), max(ac,ad,bc,bd)]

    -- Simple examples:
    -- [2:5]   * [3:6]   ~> [2*3 : 5*6]     (ac, bd)
    -- [2:5]   * [-6:-3] ~> [5*-6 : 2*-3]   (bc, ad)
    -- [-5:-2] * [3:6]   ~> [-5*6 : -2*3]   (ad, bc)
    -- [-5:-2] * [-6:-3] ~> [-2*-3 : -5*-6] (bd, ac)

    -- More complex example:
    -- [-2:5]  * [3:6]   ~> [min(-2*6, : 5*6]     (ad, bd)

    possibleLBTerm sign1 (a,b) sign2 (c,d)
      | sign1 >= Zero      , sign2 >= Zero     = multRExp a c
      | sign1 >= Zero      , sign2 >  AnySign  = multRExp b c
      | sign1 >= Zero      , sign2 == AnySign  = multRExp b c -- c < 0 , 0 <= a <= b ~> bc < ac
      | sign1 >  AnySign   , sign2 >= Zero     = multRExp a d
      | sign1 >  AnySign   , sign2 >  AnySign  = multRExp b d
      | sign1 >  AnySign   , sign2 == AnySign  = multRExp a d -- 0 < d , a <= b <= 0 ~> ad < bd
      | sign1 == AnySign   , sign2 >= Zero     = multRExp a d -- a < 0 , 0 <= c <= d ~> ad < ac
      | sign1 == AnySign   , sign2 >  AnySign  = multRExp b c -- 0 < b , c <= d <= 0 ~> bc < bd
      | otherwise                              = return Nothing
                                                -- TODO: Only enable again when we substitute
                                                -- with identifiers present in expressions
                                                 {-do ad <- multRExp a d -- a < 0, b < 0 , c < 0, d < 0
                                                    bc <- multRExp b c
                                                    case (ad,bc) of
                                                      (Just ad', Just bc') -> liftM Just $ minRExp ad' bc' pos
                                                      _                    -> return Nothing-}

    possibleUBTerm sign1 (a,b) sign2 (c,d)
      | sign1 >= Zero     , sign2 >= Zero     = multRExp b d
      | sign1 >= Zero     , sign2 >  AnySign  = multRExp a d
      | sign1 >= Zero     , sign2 == AnySign  = multRExp b d -- 0 < d , 0 <= a <= b ~> ad < bd
      | sign1 >  AnySign  , sign2 >= Zero     = multRExp b c
      | sign1 >  AnySign  , sign2 >  AnySign  = multRExp a c
      | sign1 >  AnySign  , sign2 == AnySign  = multRExp a c -- c < 0, a <= b <= 0 ~> bc < ac
      | sign1 == AnySign  , sign2 >= Zero     = multRExp b d -- 0 < b , 0 <= c <= d ~> bc < bd
      | sign1 == AnySign  , sign2 >  AnySign  = multRExp a c -- a < 0 , c <= d <= 0 ~> ad < ac
      | otherwise                             = return Nothing
                                                -- TODO: Only enable again when we substitute
                                                -- with identifiers present in expressions
                                                {-do ac <- multRExp a c -- a < 0, b < 0 , c < 0, b < 0
                                                   bd <- multRExp b d
                                                   case (ac,bd) of
                                                     (Just ac', Just bd') -> liftM Just $ maxRExp ac' bd' pos
                                                     _                    -> return Nothing-}

    multRExp :: RExp -> RExp -> RangeM (Maybe RExp)
    multRExp (RExp x) (RExp y) = liftM (Just . RExp) $ simplExp (BinOp Times x y ty pos)
    multRExp Pinf x = do
      xSign <- calculateRExpSign x pos
      case xSign of
        AnySign     -> return Nothing --badRangeM $ RangePropError pos "multRExp: Multiplying Pinf with Nothing"
        Zero -> return $ Just $ createRExpIntLit 0 pos
        s    -> return $ Just (if Zero < s then Pinf else Ninf)
    multRExp Ninf x = do
      xSign <- calculateRExpSign x pos
      case xSign of
        AnySign     -> return Nothing --badRangeM $ RangePropError pos "multRExp: Multiplying Ninf with Nothing"
        Zero -> return $ Just $ createRExpIntLit 0 pos
        s    -> return $ Just (if Zero < s then Ninf else Pinf)
    multRExp x y = multRExp y x

substitute i (RExp (BinOp Divide e1 e2 ty pos)) = do
  (a, b) <- substitute i (RExp e1)
  (c, d) <- substitute i (RExp e2)
  e1Sign <- calculateRangeSign(a,b) pos
  e2Sign <- calculateRangeSign(c,d) pos
  if canBeZero e2Sign then return (Ninf, Pinf)
  else do lb <- calcLB e1Sign (a,b) e2Sign (c,d)
          ub <- calcUB e1Sign (a,b) e2Sign (c,d)
          return (lb,ub)
  where
    canBeZero :: RangeSign -> Bool
    canBeZero Neg = False
    canBeZero Pos = False
    canBeZero _ = True

    -- [2:5] / [3:6]     ~> [2/6 : 5/3] (a/d, b/c)
    -- [2:5] / [-6:-3]   ~> [5/-3 : 2/-6] (b/d, a/c)
    -- [-5:-2] / [3:6]   ~> [-5/3 : -2/6] (a/c, b/d)
    -- [-5:-2] / [-6:-3] ~> [-2/-6 : -5/-3] (b/c, a/d)
    calcLB sign1 (a,b) sign2 (c,d)
      | sign1 >= Zero     , sign2 > Zero      = divRExp a d
      | sign1 >= Zero     , sign2 > AnySign   = divRExp b d
      | sign1 >  AnySign  , sign2 > Zero      = divRExp a c
      | sign1 >  AnySign  , sign2 > AnySign   = divRExp b c
      | sign1 == AnySign  , sign2 > Zero      = divRExp a c -- a < 0 , 0 <= c <= d ~> a/c <= a/d
      | sign1 == AnySign  , sign2 > AnySign   = divRExp b d -- 0 < b , c <= d <= 0 ~> b/d <= b/c
      | otherwise                             = badRangeM $ RangePropError pos "divRExp: Dividing with something that could be 0"

    calcUB sign1 (a,b) sign2 (c,d)
      | sign1 >= Zero     , sign2 > Zero      = divRExp b c
      | sign1 >= Zero     , sign2 > AnySign   = divRExp a c
      | sign1 >  AnySign  , sign2 > Zero      = divRExp b d
      | sign1 >  AnySign  , sign2 > AnySign   = divRExp a d
      | sign1 == AnySign  , sign2 > Zero      = divRExp b c -- 0 < b , 0 <= c <= d ~> b/d < b/c
      | sign1 == AnySign  , sign2 > AnySign   = divRExp a d -- a < 0 , c <= d <= 0 ~> a/c < a/d
      | otherwise                             = badRangeM $ RangePropError pos "divRExp: Dividing with something that could be 0"

    divRExp :: RExp -> RExp -> RangeM RExp
    divRExp (RExp x) (RExp y) = liftM RExp $ simplExp (BinOp Divide x y ty pos)
    divRExp Pinf Ninf = badRangeM $ RangePropError pos "divRExp: Dividing Pinf with Ninf"
    divRExp Pinf Pinf = badRangeM $ RangePropError pos "divRExp: Dividing Pinf with Pinf"
    divRExp Ninf Pinf = badRangeM $ RangePropError pos "divRExp: Dividing Pinf with Pinf"
    divRExp Pinf x = do
      xSign <- calculateRExpSign x pos
      case xSign of
        Pos -> return Pinf
        Neg -> return Ninf
        _   -> badRangeM $ RangePropError pos "divRExp: Dividing with something that could be 0"
    divRExp Ninf x = do
      xSign <- calculateRExpSign x pos
      case xSign of
        Pos -> return Ninf
        Neg -> return Pinf
        _   -> badRangeM $ RangePropError pos "divRExp: Dividing with something that could be 0"
    divRExp _ Pinf = return $ RExp $ createIntLit 0 pos
    divRExp _ Ninf = return $ RExp $ createIntLit 0 pos

substitute i (RExp (BinOp Pow e1 e2 ty pos)) = do
  (a, b) <- substitute i (RExp e1)
  (c, d) <- substitute i (RExp e2)
  case (c,d) of
    ( RExp (Literal (IntVal v) _) , RExp (Literal (IntVal v') _) )
      | v /= v' -> return (Ninf, Pinf)
      | even v -> do
          aSign <- calculateRExpSign a pos
          bSign <- calculateRExpSign b pos
          case (Zero <= aSign, AnySign <= bSign && bSign <= Zero) of
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
    powRExp (RExp x) v = liftM RExp $ simplExp (BinOp Pow x (createIntLit v pos) ty pos)

substitute i (RExp (Min e1 e2 _ pos)) = do
  (a, b) <- substitute i (RExp e1)
  (c, d) <- substitute i (RExp e2)
  ac <- minRExp a c pos
  bd <- minRExp b d pos
  return (ac, bd)

substitute i (RExp (Max e1 e2 _ pos)) = do
  (a, b) <- substitute i (RExp e1)
  (c, d) <- substitute i (RExp e2)
  ac <- maxRExp a c pos
  bd <- maxRExp b d pos
  return (ac, bd)

-- Resolve nested let, example:
-- let x = (let y = 5 in y+3) in x+2
-- First we process y, settings it's range to [5:5]
-- Then we process x, encountering (let y = 5 in y+3)
-- when trying to find it's range
-- therefore we only need to look at the part after in, in this case y+3
-- TODO: This is actually handled by let normalization
substitute i (RExp (LetPat _ _ inExp _)) = substitute i (RExp inExp)

substitute i (RExp (If cond te ee (Elem Int) pos)) = do
  -- TODO: fucking does not work!
  (thenInfo, elseInfo) <- realExtractFromCond cond
  tr <- mergeRangeEnvWithDict thenInfo $ substitute i (RExp te)
  er <- mergeRangeEnvWithDict elseInfo $ substitute i (RExp ee)
  rangeUnion tr er pos

substitute _ _ = return (Ninf, Pinf)

----------------------------------------
-- Extract from cond
----------------------------------------

realExtractFromCond :: Exp -> RangeM (RangeDict, RangeDict)
realExtractFromCond e = do
  condInfo <- extractFromCond e
  thenInfo <- addRangeSign $ M.mapMaybe fst condInfo
  elseInfo <- addRangeSign $ M.mapMaybe snd condInfo

  return (thenInfo, elseInfo)
  where
    addRangeSign m = Data.Traversable.sequence $ M.mapWithKey monadHelper m
    monadHelper vname (a, b) = do
      (a', _) <- substitute vname a
      (_, b') <- substitute vname b
      comp <- makeRangeComparable (a',b')
      let depend = case (a',b') of
                  (RExp e1, RExp e2) -> varsUsedInExp e1 `S.union` varsUsedInExp e2
                  (RExp e1, _)       -> varsUsedInExp e1
                  (_, RExp e2)       -> varsUsedInExp e2
                  _                  -> S.empty
      return ((a',b'), comp, depend)

extractFromCond :: Exp -> RangeM ( M.Map VName (Maybe Range, Maybe Range) )
extractFromCond (Not e _) = do
  res <- extractFromCond e
  return $ M.map (\(a, b) -> (b, a)) res

extractFromCond (BinOp Less e1 e2 _ _) =
  liftM2 M.union e1Info e2Info
  where
    e1Info = case e1 of
              (Var (Ident vname (Elem Int) _)) -> do
                                e2Minus1 <- simplExp $ expMinusOne e2
                                thenRange <- rangeIntersectIfValid (Ninf, RExp e2Minus1) e1
                                elseRange <- rangeIntersectIfValid (RExp e2, Pinf) e1
                                return $ M.singleton vname (thenRange, elseRange)
              _ -> return M.empty

    e2Info = case e2 of
              (Var (Ident vname (Elem Int) _)) -> do
                                e1Plus1 <- simplExp $ expPlusOne e1
                                thenRange <- rangeIntersectIfValid (RExp e1Plus1, Pinf) e2
                                elseRange <- rangeIntersectIfValid (Ninf, RExp e1) e2
                                return $ M.singleton vname (thenRange, elseRange)
              _ -> return M.empty

    rangeIntersectIfValid :: Range -> Exp -> RangeM (Maybe Range)
    rangeIntersectIfValid range e = do
      tmp <- rangeIntersect range (RExp e, RExp e) (L.srclocOf e)
      isTmpValid <- isValid tmp (L.srclocOf e)
      return $ if isTmpValid then Just tmp else Nothing

extractFromCond (BinOp Leq e1 e2 ty pos) =
  extractFromCond $ Not (BinOp Less e2 e1 ty pos) pos

extractFromCond (BinOp Equal e1 e2 ty pos) =
  extractFromCond $ And (BinOp Leq e1 e2 ty pos)
                        (BinOp Leq e2 e1 ty pos)
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
      elseRange <- intersectIfDeinfed elseA elseB pos
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
  ineq <- rangeCompare e1 e2 pos
  case ineq of
    IGT  -> return False
    _    -> return True

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
emptyRangeDict = M.singleton dummyVName noInfo

noInfo :: RangeDictInfo
noInfo = ((Ninf, Pinf), (Ninf, Pinf), S.empty)

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
ppSign = show

ppDict :: RangeDict -> String
ppDict rdict = foldr ((++) . (++ "\n") . ppDictElem) "" (M.toList $ M.delete dummyVName rdict)
              where
                ppDictElem :: (VName, RangeDictInfo) -> String
                ppDictElem (vname, (range, comp, depend)) =
                  escapeColorize Green (textual vname) ++ " " ++
                  escapeColorize Blue (ppRange range) ++ " " ++
                  escapeColorize White (ppRange comp) ++ " " ++
                  escapeColorize Yellow (show . map textual $ S.toList depend)

----------------------------------------
-- TESTING
----------------------------------------

{-
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
    tmp = RExp $ Max (createIntLit 10 dummySrcLoc) (Var x) (Elem Int) dummySrcLoc
    myRange = (tmp, RExp $ Var x)

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

shittyShit =
  let testExp = BinOp Less (Var x) (createIntLit 10 dummySrcLoc) (Elem Bool) dummySrcLoc
  in putStrLn $ testRange testExp
  where
    testRange e = do
         let env = RangeEnv { dict = dictWithXY }
         case runRangeM (realExtractFromCond e) env of
           Right (thenR, elseR) -> unlines ["then", ppDict thenR, "else", ppDict elseR]
           Left e -> error $ show e
--}
