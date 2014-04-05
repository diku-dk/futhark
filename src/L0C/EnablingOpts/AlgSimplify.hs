{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module L0C.EnablingOpts.AlgSimplify
  ( ScalExp
  , simplify
  , canSimplify
  , RangesRep
  , ppScalExp
  )
  where

import Data.Loc

import qualified Data.Set as S
import qualified Data.HashMap.Lazy as HM
import Data.List
import Control.Monad
import Control.Monad.Reader
--import Control.Applicative

import L0C.Dev(tident) -- for debugging: tident "int x"

--import Debug.Trace

import L0C.InternalRep
import L0C.EnablingOpts.EnablingOptErrors
import L0C.EnablingOpts.ScalExp

type RangesRep = HM.HashMap VName (Int, Maybe ScalExp, Maybe ScalExp)

-- | environment recording the position and 
--   a list of variable-to-range bindings.
data AlgSimplifyEnv = AlgSimplifyEnv { pos :: SrcLoc, cheap :: Bool, ranges :: RangesRep }

type AlgSimplifyM = ReaderT AlgSimplifyEnv (Either EnablingOptError)

runAlgSimplifier :: AlgSimplifyM a -> SrcLoc -> Bool -> RangesRep -> Either EnablingOptError a
runAlgSimplifier x p c r = runReaderT x (AlgSimplifyEnv{ pos = p, cheap = c, ranges = r })

badAlgSimplifyM :: EnablingOptError -> AlgSimplifyM a
badAlgSimplifyM = lift . Left


-----------------------------------------------------------
-- A Scalar Expression, i.e., ScalExp, is simplified to: --
--   1. if numeric: to a normalized sum-of-products form,--
--      in which on the outermost level there are N-ary  --
--      Min/Max nodes, and the next two levels are a sum --
--      of products.                                     --
--   2. if boolean: to disjunctive normal form           --
--                                                       --
-- Corresponding Helper Representations are:             --
--   1. NNumExp, i.e., NSum of NProd of ScalExp          --
--   2. DNF                                              --
-----------------------------------------------------------

data NNumExp = NSum   [NNumExp]  BasicType
             | NProd  [ScalExp]  BasicType
               deriving (Eq, Ord, Show)

compareSofP :: NNumExp -> NNumExp -> Ordering 
compareSofP n1 n2 = compare n1 n2


data BTerm   = NRelExp RelOp0 NNumExp
             | LogCt  !Bool
             | PosId   Ident
             | NegId   Ident
               deriving (Eq, Ord, Show)
type NAnd    = [BTerm]
type DNF     = [NAnd ]
--type NOr     = [BTerm]
--type CNF     = [NOr  ]

-----------------------------------------------
--- Publicly exposed functions:             ---
--- simplify, canSimplify                   ---
-----------------------------------------------

-- | Applies Simplification at Expression level:
simplify :: ScalExp -> SrcLoc -> Bool -> RangesRep -> Either EnablingOptError ScalExp
simplify e p c rm = runAlgSimplifier (simplifyScal e) p c rm


-- | Test if Simplification engine can handle this kind of expression
canSimplify :: Int -> Either EnablingOptError ScalExp
canSimplify i = do
    let e = mkIntExp i
    runAlgSimplifier (simplifyScal e) noLoc True HM.empty

-------------------------------------------------------
--- Returns a sufficient-condition predicate for     --
---    the given relational expression; unimplemented--
--- Assumes the input scalar expression is simplified--
-------------------------------------------------------
simplifyNRel :: Bool -> BTerm -> AlgSimplifyM BTerm
simplifyNRel only_half inp_term = do
    not_aggr <- asks cheap
    term <- cheapSimplifyNRel inp_term
    if not_aggr || (isTrivialNRel term)
    then return term
    else do ednf <- gaussEliminateNRel term
            if isTrueDNF ednf
            then return $ LogCt True
            else if only_half then return term
            else do nterm <- negateBTerm term
                    nednf <- gaussEliminateNRel nterm
                    if isTrueDNF nednf
                    then return $ LogCt False
                    else return term
    where
        isTrivialNRel (NRelExp _ (NProd [Val _] _)) = True
        isTrivialNRel (NRelExp{}                  ) = False
        isTrivialNRel  _                            = True
                     

cheapSimplifyNRel :: BTerm -> AlgSimplifyM BTerm
cheapSimplifyNRel (NRelExp rel (NProd [Val v] _)) = do
    succ' <- valLTHEQ0 rel v; return $ LogCt succ'
cheapSimplifyNRel e = return e 


gaussEliminateNRel :: BTerm -> AlgSimplifyM DNF 
gaussEliminateNRel _ = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "gaussElimNRel: unimplemented!"
{-
gaussAllLTH0 :: ScalExp -> AlgSimplify DNF
gaussAllLTH0 e0 =
    pos<- asks pos 
    let tp = scalExpType e
    e <- if not (tp == Int) 
         then badAlgSimplifyM $ SimplifyError pos "gaussLTH: not an Int expression!"
         else return e0
    ranges <- asks ranges
    let ids     = getIds e
    let id_cands= filter (\i->(Just True) == (lookup i ranges)) ids
    
gaussOneLTH0 :: Ident -> ScalExp -> AlgSimplify DNF
gaussOneLTH0 i e = do
    (e_dnfs, es) <- predSimplifyScal i [] e
    return $ NRelExp LTH0 e

predSimplifyScal :: Ident -> ScalExp -> AlgSimplify ([([ScalExp], ScalExp)])
predSimplifyScal i e@(Val _) = return ([[], e])
predSimplifyScal i e@(Id  _) = return ([[], e])
predSimplifyScal i (SNeg e) = 
    pse' <- predSimplifyScal i e
    return map (\(ps,e)->case e of 
                          MinMax m es -> ps, SNeg e)
predSimplifyScal i ps (SPlus x y) =
    (psx, ex) <- predSimplifyScal i x
    (psy, ey) <- predSimplifyScal i y
    case (ex, ey) of
        (MinMax ismin exs, _) -> 
            (ps', e) <- predSimplifyScal $ MinMax ismin $ map (\x -> SPlus x ey) exs
            return (psx++psy++ps', e
predSimplifyScal i ps _ =  
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos (
      "predSimplifyScal: found bool exp")
-}
------------------------------------------------
------------------------------------------------
-- Main Helper Function: takes a scalar exp,  --
-- normalizes and simplifies it               -- 
------------------------------------------------
------------------------------------------------

simplifyScal :: ScalExp -> AlgSimplifyM ScalExp

simplifyScal (Val v)       = return $ Val v
simplifyScal (Id  x)       = return $ Id  x

simplifyScal e@(SNot   {}) = fromDNF =<< simplifyDNF =<< toDNF e
simplifyScal e@(SLogAnd{}) = fromDNF =<< simplifyDNF =<< toDNF e
simplifyScal e@(SLogOr {}) = fromDNF =<< simplifyDNF =<< toDNF e
simplifyScal e@(RelExp {}) = fromDNF =<< simplifyDNF =<< toDNF e

--------------------------------------
--- MaxMin related simplifications ---
--------------------------------------
simplifyScal (MaxMin _ []) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos (
      "Scalar MaxMin expression with empty arglist.")
simplifyScal (MaxMin _ [e]) = simplifyScal e
simplifyScal (MaxMin ismin es) = do -- helperMinMax ismin  es pos
    -- pos <- asks pos 
    es' <- mapM simplifyScal es
    -- flatten the result and remove duplicates, 
    -- e.g., Max(Max(e1,e2), e1) -> Max(e1,e2,e3)
    return $ MaxMin ismin $ remDups $ foldl flatop [] es'
    -- ToDo: This can prove very expensive as compile time
    --       but, IF e2-e1 <= 0 simplifies to True THEN
    --       Min(e1,e2) = e2.   Code example:
    -- e1me2 <- if isMin 
    --          then simplifyScal $ AlgOp MINUS e1 e2 pos
    --          else simplifyScal $ AlgOp MINUS e2 e1 pos
    -- e1me2leq0 <- simplifyNRel $ NRelExp LEQ0 e1me2 pos
    -- case e1me2leq0 of
    --    NAnd [LogCt True  _] _ -> simplifyAlgN e1
    --    NAnd [LogCt False _] _ -> simplifyAlgN e2
    where 
        flatop :: [ScalExp] -> ScalExp -> [ScalExp]
        flatop a e@(MaxMin ismin' ses) = 
            if ismin == ismin' then a++ses else a++[e]
        flatop a e = a++[e]
        remDups :: [ScalExp] -> [ScalExp]
        remDups l = S.toList (S.fromList l)

---------------------------------------------------
--- Plus/Minus related simplifications          ---
--- BUG: the MinMax pattern matching should     ---
---      be performed on the simplified subexps ---
---------------------------------------------------
simplifyScal (SPlus e1o e2o) = do
    e1' <- simplifyScal e1o
    e2' <- simplifyScal e2o
    if (isMaxMin e1') || (isMaxMin e2')
    then helperPlusMinMax $ SPlus e1' e2'
    else normalPlus e1' e2'

    where
      normalPlus :: ScalExp -> ScalExp -> AlgSimplifyM ScalExp
      normalPlus e1 e2 = do
        let tp = scalExpType e1
        e1' <- toNumSofP e1
        e2' <- toNumSofP e2
        let terms = getTerms e1' ++ getTerms e2'
        splittedTerms <- mapM splitTerm terms
        let sortedTerms = sortBy (\(n1,_) (n2,_) -> compareSofP n1 n2) splittedTerms
        -- foldM discriminate: adds together identical terms, and
        -- we reverse the list, to keep it in a ascending order.
        merged <- liftM reverse $ foldM discriminate [] sortedTerms
        let filtered = filter (\(_,v) -> not $ isZero v ) merged
        if null filtered
        then do
            zero <- getZero tp
            fromNumSofP $ NProd [Val zero] tp
        else do
            terms' <- mapM joinTerm filtered
            fromNumSofP $ NSum terms' tp

simplifyScal (SMinus e1 e2) = do
    if e1 == e2 
    then do zero <- getZero tp; return $ Val zero  
    else do min_1 <- getNeg1 $ scalExpType e1
            simplifyScal $ SPlus e1 $ STimes (Val min_1) e2
  where tp = scalExpType e1

simplifyScal (SNeg e) = do
    negOne <- getNeg1 $ scalExpType e
    simplifyScal $ STimes (Val negOne) e

---------------------------------------------------
--- Times        related simplifications        ---
--- BUG: the MinMax pattern matching should     ---
---      be performed on the simplified subexps ---
---------------------------------------------------
simplifyScal (STimes e1o e2o) = do
    e1'' <- simplifyScal e1o
    e2'' <- simplifyScal e2o
    if (isMaxMin e1'') || (isMaxMin e2'')
    then helperMultMinMax $ STimes e1'' e2''
    else normalTimes e1'' e2''

    where
      normalTimes :: ScalExp -> ScalExp -> AlgSimplifyM ScalExp
      normalTimes e1 e2 = do
        let tp = scalExpType e1
        e1' <- toNumSofP e1
        e2' <- toNumSofP e2
        case (e1', e2') of
            (NProd xs _, y@(NProd{}) ) -> fromNumSofP =<< makeProds xs y
            (NProd xs _, y) -> do
                    prods <- mapM (makeProds xs) $ getTerms y
                    fromNumSofP $ NSum (sort prods) tp
            (x, NProd ys _) -> do
                    prods <- mapM (makeProds ys) $ getTerms x
                    fromNumSofP $ NSum (sort prods) tp
            (NSum xs _, NSum ys _) -> do
                    xsMultChildren <- mapM getMultChildren xs
                    prods <- mapM (\x -> mapM (makeProds x) ys) xsMultChildren
                    fromNumSofP $ NSum (sort $ concat prods) tp

      makeProds :: [ScalExp] -> NNumExp -> AlgSimplifyM NNumExp
      makeProds [] _ = do
           pos <- asks pos
           badAlgSimplifyM $ SimplifyError pos
              " In simplifyAlgN, makeProds: 1st arg is the empty list! "
      makeProds _ (NProd [] _) = do
          pos <- asks pos
          badAlgSimplifyM $ SimplifyError pos
            " In simplifyAlgN, makeProds: 2nd arg is the empty list! "
      makeProds _ (NSum{}) = do
          pos <- asks pos
          badAlgSimplifyM $ SimplifyError pos
            " In simplifyAlgN, makeProds: e1 * e2: e2 is a sum of sums! "
      makeProds ((Val v1):exs) (NProd ((Val v2):ys) tp1) = do
          v <- mulVals v1 v2
          return $ NProd ((Val v) : sort (ys++exs) ) tp1
      makeProds ((Val v):exs) (NProd ys tp1) =
          return $ NProd ( (Val v): sort (ys++exs) ) tp1
      makeProds exs (NProd ((Val v): ys) tp1) =
          return $ NProd ( (Val v): sort (ys++exs) ) tp1
      makeProds exs (NProd ys tp1) =
          return $ NProd (sort (ys++exs)) tp1

---------------------------------------------------
---------------------------------------------------
--- DIvide        related simplifications       ---
---------------------------------------------------
---------------------------------------------------

simplifyScal (SDivide e1o e2o) = do
    e1' <- simplifyScal e1o
    e2' <- simplifyScal e2o

    if (isMaxMin e1') || (isMaxMin e2')
    then helperMultMinMax $ SDivide e1' e2'
    else normalDivide e1' e2'

    where
      normalDivide :: ScalExp -> ScalExp -> AlgSimplifyM ScalExp
      normalDivide e1 e2 
        | (e1 == e2)                  = do one <- getPos1 $ scalExpType e1
                                           return $ Val one
--        | e1 == (negateSimplified e2) = do mone<- getNeg1 $ scalExpType e1
--                                           return $ Val mone
        | otherwise = do
            e1' <- toNumSofP e1
            e2' <- toNumSofP e2
            case e2' of
              NProd fs tp -> do
                e1Split <- mapM splitTerm (getTerms e1')
                case e1Split of
                  []  -> do zero <- getZero tp
                            return $ Val zero
                  _   -> do (fs', e1Split')  <- trySimplifyDivRec fs [] e1Split
                            if (length fs') == (length fs)
                            then turnBackAndDiv e1' e2' -- insuccess
                            else do terms_e1' <- mapM joinTerm e1Split'
                                    e1'' <- fromNumSofP $ NSum terms_e1' tp 
                                    case fs' of
                                      [] -> return e1'' 
                                      _  -> do e2'' <- fromNumSofP $ NProd fs' tp
                                               return $ SDivide e1'' e2''  
 
              _ -> turnBackAndDiv e1' e2'

      turnBackAndDiv :: NNumExp -> NNumExp -> AlgSimplifyM ScalExp
      turnBackAndDiv ee1 ee2 = do
        ee1' <- fromNumSofP ee1
        ee2' <- fromNumSofP ee2
        return $ SDivide ee1' ee2'

---------------------------------------------------
---------------------------------------------------
--- Power        related simplifications        ---
---------------------------------------------------
---------------------------------------------------

-- cannot handle 0^a, because if a < 0 then it's an error.
-- Could be extented to handle negative exponents better, if needed
simplifyScal (SPow e1 e2) = do
    let tp = scalExpType e1
    e1' <- simplifyScal e1
    e2' <- simplifyScal e2

    if isCt1 e1' || isCt0 e2'
    then do one <- getPos1 tp
            return $ Val one
    else if isCt1 e2'
    then return e1'
    else case (e1', e2') of
            (Val v1, Val v2)    -> do
                v <- powVals v1 v2 
                return $ Val v
            (_, Val (IntVal n)) -> 
                if n >= 1 
                then -- simplifyScal =<< fromNumSofP $ NProd (replicate n e1') tp
                        do new_e <- fromNumSofP $ NProd (replicate n e1') tp
                           simplifyScal new_e
                else do return $ SPow e1' e2'
            (_, _) -> do return $ SPow e1' e2'

    where
        powVals :: BasicValue -> BasicValue -> AlgSimplifyM BasicValue
        powVals (IntVal v1) (IntVal v2)
            | v2 < 0, v1 == 0 = do pos <- asks pos
                                   badAlgSimplifyM $ SimplifyError pos "powVals: Negative exponential with zero base"
            | v2 < 0          = return $ IntVal ( 1 `div` (v1 ^ (-v2)) )
            | otherwise       = return $ IntVal ( v1 ^ v2 )
        powVals (RealVal v1) (RealVal v2) = return $ RealVal (v1 ** v2)
        powVals _ _ = do pos <- asks pos
                         badAlgSimplifyM $ SimplifyError pos  "powVals: operands not of (the same) numeral type! "

-----------------------------------------------------
--- Helpers for simplifyScal: MinMax related, etc ---
-----------------------------------------------------

isMaxMin :: ScalExp -> Bool
isMaxMin (MaxMin{}) = True
isMaxMin _          = False

helperPlusMinMax :: ScalExp -> AlgSimplifyM ScalExp
helperPlusMinMax (SPlus (MaxMin ismin es) e) = 
    simplifyScal $ MaxMin ismin $ map (\x -> SPlus x e) es 
helperPlusMinMax (SPlus e (MaxMin ismin es)) = 
    simplifyScal $ MaxMin ismin $ map (\x -> SPlus e x) es 
helperPlusMinMax _ = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "helperPlusMinMax: Reached unreachable case!"

{-
helperMinusMinMax :: ScalExp -> AlgSimplifyM ScalExp
helperMinusMinMax (SMinus (MaxMin ismin es) e) = 
    simplifyScal $ MaxMin ismin $ map (\x -> SMinus x e) es 
helperMinusMinMax (SMinus e (MaxMin ismin es)) = 
    simplifyScal $ MaxMin ismin $ map (\x -> SMinus e x) es 
helperMinusMinMax _ = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "helperMinusMinMax: Reached unreachable case!"
-}
helperMultMinMax :: ScalExp -> AlgSimplifyM ScalExp
helperMultMinMax (STimes  e em@(MaxMin{})) = helperTimesDivMinMax True  True  em e
helperMultMinMax (STimes  em@(MaxMin{}) e) = helperTimesDivMinMax True  False em e 
helperMultMinMax (SDivide e em@(MaxMin{})) = helperTimesDivMinMax False True  em e
helperMultMinMax (SDivide em@(MaxMin{}) e) = helperTimesDivMinMax False False em e
helperMultMinMax _ = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "helperMultMinMax: Reached unreachable case!"

helperTimesDivMinMax :: Bool -> Bool -> ScalExp -> ScalExp -> AlgSimplifyM ScalExp
helperTimesDivMinMax isTimes isRev emo@(MaxMin{}) e = do
    em <- simplifyScal emo
    case em of 
        MaxMin ismin es -> do
            e' <- simplifyScal e
            e'_sop <- toNumSofP e'
            p' <- simplifyNRel False $ NRelExp LTH0 e'_sop
            case p' of
                LogCt ctbool -> do
                    let cond = (not isTimes) && isRev
                    let cond'= if ctbool then cond  else not cond
                    let ismin'= if cond' then ismin else not ismin
                    simplifyScal $ MaxMin ismin' $ map (\x -> mkTimesDiv x e') es
                _  -> return $ mkTimesDiv em e'
        _ -> simplifyScal $ mkTimesDiv em e
    where
        mkTimesDiv :: ScalExp -> ScalExp -> ScalExp
        mkTimesDiv e1 e2 = if isTimes 
                           then if isRev then STimes  e2 e1 else STimes  e1 e2 
                           else if isRev then SDivide e2 e1 else SDivide e1 e2

helperTimesDivMinMax _ _ _ _ = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "helperTimesDivMinMax: Reached unreachable case!"


---------------------------------------------------
---------------------------------------------------
--- translating to and simplifying the          ---
--- disjunctive normal form: toDNF, simplifyDNF ---
---------------------------------------------------
---------------------------------------------------
isTrueDNF :: DNF -> Bool
isTrueDNF [[LogCt True]] = True
isTrueDNF _              = False

negateBTerm :: BTerm -> AlgSimplifyM BTerm
negateBTerm (LogCt v) = return $ LogCt (not v)
negateBTerm (PosId i) = return $ NegId i
negateBTerm (NegId i) = return $ PosId i
negateBTerm (NRelExp rel e) = do 
    let tp = typeOfNAlg e
    case (tp, rel) of
        (Int, LTH0) -> do
            se <- fromNumSofP e
            ne <- toNumSofP =<< simplifyScal (SNeg $ SPlus se (Val $ IntVal 1))
            return $ NRelExp LTH0 ne 
        _ -> do        
            e' <- toNumSofP =<< negateSimplified =<< fromNumSofP e; 
            return $ NRelExp (if rel == LEQ0 then LTH0 else LEQ0) e'

bterm2ScalExp :: BTerm -> AlgSimplifyM ScalExp
bterm2ScalExp (LogCt v) = return $ Val $ LogVal v
bterm2ScalExp (PosId i) = return $ Id i
bterm2ScalExp (NegId i) = return $ SNot $ Id i
bterm2ScalExp (NRelExp rel e) = do e' <- fromNumSofP e; return $ RelExp rel e'

-- translates from DNF to ScalExp
fromDNF :: DNF -> AlgSimplifyM ScalExp
fromDNF [] = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "fromDNF: empty DNF!"
fromDNF (t:ts) = do
    t' <- translFact t
    foldM (\acc x -> do x' <- translFact x; return $ SLogOr x' acc) t' ts
    where 
        translFact [] = do 
            pos <- asks pos
            badAlgSimplifyM $ SimplifyError pos "fromDNF, translFact empty DNF factor!"
        translFact (f:fs) = do
            f' <- bterm2ScalExp f
            foldM (\acc x -> do x' <- bterm2ScalExp x; return $ SLogAnd x' acc) f' fs

-- translates (and simplifies numeric expressions?) to DNF form. 
toDNF :: ScalExp -> AlgSimplifyM DNF
toDNF (Val  (LogVal v)) = return $ [[LogCt v]]
toDNF (Id      idd    ) = return $ [[PosId idd]]
toDNF (RelExp  rel  e ) = do
    let btp = scalExpType e
    (e', rel')<- if (btp == Int) && (rel == LEQ0) 
                 then do m1 <- getNeg1 Int; return (SPlus e $ Val m1, LTH0)
                 else return (e, rel)   
    ne    <- toNumSofP =<< simplifyScal e'
    nrel  <- simplifyNRel False $ NRelExp rel' ne
    return [[nrel]]
--
toDNF (SNot (SNot     e)) = toDNF e
toDNF (SNot (Val (LogVal v))) = return $ [[LogCt $ not v]]
toDNF (SNot (Id     idd)) = return $ [[NegId idd]]
toDNF (SNot (RelExp rel e)) = do
    let not_rel = if rel == LEQ0 then LTH0 else LEQ0
    toDNF $ RelExp not_rel (SNeg e)
--
toDNF (SLogOr  e1 e2  ) = do
    e1s <- toDNF e1
    e2s <- toDNF e2
    return $ sort $ e1s ++ e2s
toDNF (SLogAnd e1 e2  ) = do 
    -- [t1 ++ t2 | t1 <- toDNF e1, t2 <- toDNF e2]
    e1s <- toDNF e1
    e2s <- toDNF e2
    let lll = map (\t2-> map (\t1 -> t1++t2) e1s) e2s
    return $ sort $ foldl (++) [] lll
toDNF (SNot (SLogAnd e1 e2)) = do 
    e1s <- toDNF (SNot e1)
    e2s <- toDNF (SNot e2)
    return $ sort $ e1s ++ e2s
toDNF (SNot (SLogOr e1 e2)) = do
    -- [t1 ++ t2 | t1 <- dnf $ SNot e1, t2 <- dnf $ SNot e2] 
    e1s <- toDNF $ SNot e1
    e2s <- toDNF $ SNot e2
    let lll = map (\t2-> map (\t1 -> t1++t2) e1s) e2s
    return $ sort $ foldl (++) [] lll
toDNF _            = do 
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "toDNF: not a boolean expression!"

------------------------------------------------------
--- Simplifying Boolean Expressions:               ---
---  0. p     AND p == p;       p     OR p == p    ---
---  1. False AND p == False;   True  OR p == True ---
---  2. True  AND p == p;       False OR p == p    ---
---  3.(not p)AND p == FALSE;  (not p)OR p == True ---
---  4. ToDo: p1 AND p2 == p1 if p1 => p2          ---
---           p1 AND p2 == False if p1 => not p2 or---
---                                 p2 => not p1   ---
---     Also: p1 OR p2 == p2 if p1 => p2           ---
---           p1 OR p2 == True if not p1 => p2 or  ---
---                               not p2 => p1     ---
---     This boils down to relations:              ---
---      e1 < 0 => e2 < 0 if e2 <= e1              ---
------------------------------------------------------
simplifyDNF :: DNF -> AlgSimplifyM DNF
simplifyDNF terms0 = do
    terms1 <- mapM (simplifyAndOr True) terms0
    let terms' = if any (== [LogCt True]) terms1 then [[LogCt True]] 
                 else S.toList $ S.fromList $
                        filter (not . (== [LogCt False])) terms1
    if null terms' then return [[LogCt False]] 
    else do
        let len1terms = (foldl (&&) True . map (\x -> 1 == length x)) terms'
        if not len1terms then return terms'
        else do let terms_flat = foldl (++) [] terms'
                terms'' <- simplifyAndOr False terms_flat
                return $ map (\x->[x]) terms''

-- big helper function for simplifyDNF
simplifyAndOr :: Bool -> [BTerm] -> AlgSimplifyM [BTerm]
simplifyAndOr _ [] = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "simplifyAndOr: not a boolean expression!"
simplifyAndOr is_and fs = do 
    if any (== (LogCt $ not is_and)) fs
         -- False AND p == False 
    then return [LogCt $ not is_and]   
                    -- (i) p AND p == p,        (ii) True AND p == p
    else do let fs' = ( S.toList . S.fromList . filter (not . (== (LogCt is_and))) ) fs
            if null fs' 
            then return [LogCt is_and]
            else do    -- IF p1 => p2 THEN   p1 AND p2 --> p1
                fs''<- foldM (\l x-> do (addx, l') <- trimImplies is_and x l
                                        if addx then return (x:l') else return l'
                             ) [] fs'
                       -- IF if p1 => not p2 THEN p1 AND p2 == False 
                isF <- foldM (\b x -> if b then return b 
                                      else do notx <- negateBTerm x
                                              impliesAny is_and x notx fs''
                             ) False fs''
                if not isF then return fs''
                else if is_and then return [LogCt False] 
                               else return [LogCt True ] 
    where
        -- e1 => e2 ?
        impliesRel :: BTerm -> BTerm -> AlgSimplifyM Bool
        impliesRel (LogCt False) _ = return True
        impliesRel _ (LogCt  True) = return True
        impliesRel (LogCt True)  e = do
            e' <- simplifyNRel True e
            if e' == (LogCt True) then return True else return False
        impliesRel e (LogCt False) = do
            e' <- (simplifyNRel True) =<< negateBTerm e
            if e' == (LogCt True) then return True else return False
        impliesRel (NRelExp rel1 e1) (NRelExp rel2 e2) = do
        -- ToDo: implement implies relation!
            --not_aggr <- asks cheap
            pos <- asks       pos
            let btp = typeOfNAlg e1
            one <- getPos1    btp
            e1' <- fromNumSofP e1
            e2' <- fromNumSofP e2
            case (rel1, rel2, btp) of
                (LTH0, LTH0, Int) -> do
                    e2me1m1 <- toNumSofP =<< simplifyScal (SMinus e2' $ SPlus e1' $ Val one)
                    diffrel <- simplifyNRel True $ NRelExp LTH0 e2me1m1
                    if diffrel == (LogCt True) then return True else return False
                (_, _, Int) -> badAlgSimplifyM $ SimplifyError pos "impliesRel: LEQ0 for Int!"
                (_, _, Real)-> do
                    e2me1 <- toNumSofP =<< simplifyScal (SMinus e2' e1')
                    let rel = if (rel1,rel2) == (LEQ0, LTH0) then LTH0 else LEQ0
                    diffrel <- simplifyNRel True $ NRelExp rel e2me1
                    if diffrel == (LogCt True) then return True else return False 
                (_, _, _) -> badAlgSimplifyM $ SimplifyError pos "impliesRel: exp of illegal type!"
        impliesRel p1 p2 
            | p1 == p2  = return True
            | otherwise = return False

        -- trimImplies(true,  x, l) performs: p1 AND p2 == p1 if p1 => p2,
        --   i.e., removes any p in l such that:
        --    (i) x => p orelse if
        --   (ii) p => x then indicates that p should not be added to the reuslt
        -- trimImplies(false, x, l) performs: p1 OR p2 == p2 if p1 => p2,
        --   i.e., removes any p from l such that:
        --    (i) p => x orelse if
        --   (ii) x => p then indicates that p should not be added to the result
        trimImplies :: Bool -> BTerm -> [BTerm] -> AlgSimplifyM (Bool, [BTerm])
        trimImplies _        _ []     = return (True, [])
        trimImplies and_case x (p:ps) = do
            succc <- impliesRel x p
            if succc
            then if and_case then trimImplies and_case x ps else return (False, p:ps)
            else do suc <- impliesRel p x
                    if suc then if and_case then return (False, p:ps) else trimImplies and_case x ps
                    else do (addx, newps) <- trimImplies and_case x ps
                            return (addx, p:newps)

        -- collapseImplies(true,  x, notx, l) performs: 
        --   x AND p == False if p => not x, where p in l,
        -- collapseImplies(true,  x, notx, l) performs:
        --   x OR p == True if not x => p
        -- BUT only when p != x, i.e., avoids comparing x with notx
        impliesAny :: Bool -> BTerm -> BTerm -> [BTerm] -> AlgSimplifyM Bool
        impliesAny _        _ _    []     = return False
        impliesAny and_case x notx (p:ps)
            | x == p = impliesAny and_case x notx ps
            | otherwise = do
                succ' <- if and_case then impliesRel p notx else impliesRel notx p
                if succ' then return True
                else impliesAny and_case x notx ps

------------------------------------------------
--- Syntax-Directed (Brainless) Translators  ---
---    scalExp <-> NNumExp                   ---
--- and negating a scalar expression         --- 
------------------------------------------------

-- negates an already simplified scalar expression,
--   presumably more efficient than negating and
--   then simplifying it.
negateSimplified :: ScalExp -> AlgSimplifyM ScalExp
negateSimplified (SNeg e) = return e
negateSimplified (SNot e) = return e
negateSimplified e@(Val v) = do 
    m1 <- getNeg1 $ scalExpType e
    v' <- mulVals m1 v; return $ Val v'
negateSimplified e@(Id{}) = do  
    m1 <- getNeg1 $ scalExpType e
    return $ STimes (Val m1) e
negateSimplified (SMinus e1 e2) = do -- return $ SMinus e2 e1
    e1' <- negateSimplified e1
    return $ SPlus e1' e2
negateSimplified (SPlus e1 e2) = do
    e1' <- negateSimplified e1
    e2' <- negateSimplified e2
    return $ SPlus e1' e2'
negateSimplified e@(SPow _ _) = do 
    m1 <- getNeg1 $ scalExpType e
    return $ STimes (Val m1) e
negateSimplified (STimes  e1 e2) = do 
    (e1', e2') <- helperNegateMult e1 e2; return $ STimes  e1' e2'
negateSimplified (SDivide e1 e2) = do 
    (e1', e2') <- helperNegateMult e1 e2; return $ SDivide e1' e2'
negateSimplified (MaxMin ismin ts) = do
    ts' <- mapM negateSimplified ts; return $ MaxMin (not ismin) ts'
negateSimplified (RelExp LEQ0 e) = do
    me <- negateSimplified e
    return $ RelExp LTH0 me
negateSimplified (RelExp LTH0 e) = do
    me <- negateSimplified e
    return $ RelExp LEQ0 me
negateSimplified (SLogAnd{}) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "negateSimplified: SLogAnd unimplemented!"
negateSimplified (SLogOr{}) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "negateSimplified: SLogOr  unimplemented!"

helperNegateMult :: ScalExp -> ScalExp -> AlgSimplifyM (ScalExp, ScalExp)
helperNegateMult e1 e2 = do
    case (e1, e2) of
        (Val _,              _) -> do e1'<- negateSimplified e1;       return (e1', e2)
        (STimes (Val v) e1r, _) -> do ev <- negateSimplified (Val v);  return (STimes ev e1r, e2)
        (_,              Val _) -> do e2'<- negateSimplified e2;       return (e1, e2')
        (_, STimes (Val v) e2r) -> do ev <- negateSimplified (Val v);  return (e1, STimes ev e2r) 
        (_,                  _) -> do e1'<- negateSimplified e1;       return (e1', e2)


toNumSofP :: ScalExp -> AlgSimplifyM NNumExp
toNumSofP e@(Val  _) = return $ NProd [e] $ scalExpType e
toNumSofP e@(Id   _) = return $ NProd [e] $ scalExpType e
toNumSofP e@(SDivide{})   = return $ NProd [e] $ scalExpType e
toNumSofP e@(SPow{}   )   = return $ NProd [e] $ scalExpType e
toNumSofP (SMinus _ _)  = do --toNumSofP $ SPlus e1 (SNeg e2)
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "toNumSofP: SMinus is not in SofP form!"
toNumSofP (SNeg _)        = do --toNumSofP =<< negateSimplified e -- Shouldn't it be (-1)*e ???
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "toNumSofP: SNeg is not in SofP form!"
toNumSofP (STimes e1 e2) = do 
    pos <- asks pos
    e2' <- toNumSofP e2
    case e2' of
        NProd es2 t -> return $ NProd (e1:es2) t
        _ -> badAlgSimplifyM $ SimplifyError pos "toNumSofP: STimes nor in SofP form!"
toNumSofP (SPlus  e1 e2)   = do 
    let t = scalExpType e1
    e1' <- toNumSofP  e1
    e2' <- toNumSofP  e2
    case (e1', e2') of
        (NSum es1 _, NSum es2 _) -> return $ NSum (es1++es2) t
        (NSum es1 _, NProd{}) -> return $ NSum (es1++[e2']) t
        (NProd{}, NSum es2 _) -> return $ NSum (e1':es2)    t
        (NProd{}, NProd{}   ) -> return $ NSum [e1', e2']   t
toNumSofP _ = do 
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "toNumSofP: unimplemented!"


fromNumSofP :: NNumExp -> AlgSimplifyM ScalExp
fromNumSofP (NSum [ ] t)    = do
    zero <- getZero t
    return $ Val zero
fromNumSofP (NSum [f] _)    = fromNumSofP f
fromNumSofP (NSum (f:fs) t) = do
    fs_e <- fromNumSofP $ NSum fs t
    f_e  <- fromNumSofP $ f
    return $ SPlus f_e fs_e
fromNumSofP (NProd [] _)    = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos
            " In fromNumSofP, empty NProd expression! "
fromNumSofP (NProd [f] _)    = return f
fromNumSofP (NProd (f:fs) t) = do
    fs_e <- fromNumSofP $ NProd fs t
    return $ STimes f fs_e
--fromNumSofP _ = do 
--    pos <- asks pos
--    badAlgSimplifyM $ SimplifyError pos "fromNumSofP: unimplemented!"
------------------------------------------------------------
--- Helpers for simplifyScal: getTerms, getMultChildren, ---
---   splitTerm, joinTerm, discriminate
------------------------------------------------------------


-- get the list of terms of an expression
-- BUG for NMinMax -> should convert it back to a ScalExp
getTerms :: NNumExp -> [NNumExp]
getTerms (NSum es _) = es
getTerms e@(NProd{}) = [e]

-- get the factors of a term
getMultChildren :: NNumExp -> AlgSimplifyM [ScalExp]
getMultChildren (NSum _ _) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "getMultChildren, NaryPlus should not be nested 2 levels deep "
getMultChildren (NProd xs _) = return xs

-- split a term into a (multiplicative) value and the rest of the factors.
splitTerm :: NNumExp -> AlgSimplifyM (NNumExp, BasicValue)
splitTerm (NProd [ ] _) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "splitTerm: Empty n-ary list of factors."
splitTerm (NProd [f] tp) = do
  one <- getPos1 tp
  case f of
      (Val v) -> return (NProd [Val one] tp, v  )
      e       -> return (NProd [e]       tp, one)
splitTerm ne@(NProd (f:fs) tp) =
  case f of
      (Val v) -> return (NProd fs tp, v)
      _       -> do one <- getPos1 tp
                    return (ne, one)
splitTerm e = do
  one <- getPos1 (typeOfNAlg e)
  return (e, one)

-- join a value with a list of factors into a term.
joinTerm :: (NNumExp, BasicValue) -> AlgSimplifyM NNumExp
joinTerm ( NSum _ _, _) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "joinTerm: NaryPlus two levels deep."
joinTerm ( NProd [] _, _) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos "joinTerm: Empty NaryProd."
joinTerm ( NProd ((Val l):fs) tp, v) = do
    v' <- mulVals v l
    let v'Lit = Val v'
    return $ NProd (v'Lit:(sort fs)) tp
joinTerm ( e@(NProd fs tp), v)
  | isOne v   = return e
  | otherwise = let vExp = Val v
                in return $ NProd (vExp:(sort fs)) tp

-- adds up the values corresponding to identical factors!
discriminate :: [(NNumExp, BasicValue)] -> (NNumExp, BasicValue) -> AlgSimplifyM [(NNumExp, BasicValue)]
discriminate []          e        = return [e]
discriminate e@((k,v):t) (k', v') =
  if k == k'
  then do v'' <- addVals v v'
          return ( (k, v'') : t )
  else return ( (k', v') : e )

------------------------------------------------------
--- Trivial Utility Functions                      ---
------------------------------------------------------

ppBType :: BasicType -> String
ppBType Int  = "Int"
ppBType Real = "Real"
ppBType Bool = "Bool"
ppBType Char = "Char"
ppBType Cert = "Cert"

getZero :: BasicType -> AlgSimplifyM BasicValue
getZero Int  = return $ IntVal 0
getZero Real = return $ RealVal 0.0
getZero tp   = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos ("getZero for type: "++ppBType tp)

getPos1 :: BasicType -> AlgSimplifyM BasicValue
getPos1 Int  = return $  IntVal 1
getPos1 Real = return $ RealVal 1.0
getPos1 tp   = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos ("getOne for type: "++ppBType tp)

getNeg1 :: BasicType -> AlgSimplifyM BasicValue
getNeg1 Int  = return $  IntVal (-1)
getNeg1 Real = return $ RealVal (-1.0)
getNeg1 tp   = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos ("getOne for type: "++ppBType tp)

isZero :: BasicValue -> Bool
isZero (IntVal  v) = v == 0
isZero (RealVal v) = v == 0.0
isZero (_)         = False

valLTHEQ0 :: RelOp0 -> BasicValue -> AlgSimplifyM Bool
valLTHEQ0 rel ( IntVal v) = if rel==LEQ0 then return $ v <= 0   else return $ v < 0
valLTHEQ0 rel (RealVal v) = if rel==LEQ0 then return $ v <= 0.0 else return $ v < 0.0
valLTHEQ0 _ _ = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos ("valLTHEQ0 for non-numeric type!")

isOne :: BasicValue -> Bool
isOne (IntVal  v) = v == 1
isOne (RealVal v) = v == 1.0
isOne (_)         = False

isCt1 :: ScalExp -> Bool
isCt1 e = case e of
            Val (IntVal  one) -> one == 1
            Val (RealVal one) -> one == 1.0
            _                       -> False
isCt0 :: ScalExp -> Bool
isCt0 e = case e of
            Val (IntVal  zr)  -> zr == 0
            Val (RealVal zr)  -> zr == 0.0
            _                 -> False


addVals :: BasicValue -> BasicValue -> AlgSimplifyM BasicValue
addVals e1 e2 = do
  pos <- asks pos
  case (e1, e2) of
    ( IntVal v1,  IntVal v2) -> return $  IntVal (v1+v2)
    (RealVal v1, RealVal v2) -> return $ RealVal (v1+v2)
    _ -> badAlgSimplifyM $ SimplifyError pos  "addVals: operands not of (the same) numeral type! "

mulVals :: BasicValue -> BasicValue -> AlgSimplifyM BasicValue
mulVals e1 e2 = do
  pos <- asks pos
  case (e1, e2) of
    ( IntVal v1,  IntVal v2) -> return $  IntVal (v1*v2)
    (RealVal v1, RealVal v2) -> return $ RealVal (v1*v2)
    _ -> badAlgSimplifyM $ SimplifyError pos "mulVals: operands not of (the same) numeral type! "

divVals :: BasicValue -> BasicValue -> AlgSimplifyM BasicValue
divVals e1 e2 = do
  pos <- asks pos
  case (e1, e2) of
    ( IntVal v1,  IntVal v2) -> return $ IntVal (v1 `div` v2)
    (RealVal v1, RealVal v2) -> return $ RealVal (v1/v2)
    _ -> badAlgSimplifyM $ SimplifyError pos  "divVals: operands not of (the same) numeral type! "

canDivValsEvenly :: BasicValue -> BasicValue -> AlgSimplifyM Bool
canDivValsEvenly e1 e2 = do
  pos <- asks pos
  case (e1, e2) of
    (IntVal v1,  IntVal v2) -> return $ v1 `mod` v2 == 0
    (RealVal _, RealVal _) -> return True
    _ -> badAlgSimplifyM $ SimplifyError pos  "canDivValsEvenly: operands not of (the same) numeral type!"

-------------------------------------------------------------
-------------------------------------------------------------
---- Helpers for the ScalExp and NNumRelLogExp Datatypes ----
-------------------------------------------------------------
-------------------------------------------------------------

--posOfScal :: ScalExp -> AlgSimplifyM SrcLoc
--posOfScal _ = do pos <- asks pos
--                 return pos


typeOfNAlg :: NNumExp -> BasicType
typeOfNAlg (NSum   _   t) = t
typeOfNAlg (NProd  _   t) = t

{-
isZeroScal :: ScalExp -> Bool
isZeroScal (Val ( IntVal 0   )) = True
isZeroScal (Val (RealVal 0.0 )) = True
isZeroScal (Val (LogVal False)) = True
isZeroScal _                    = False
-}
----------------------------------------
---- Helpers for Division and Power ----
----------------------------------------
trySimplifyDivRec :: [ScalExp] -> [ScalExp] -> [(NNumExp, BasicValue)] -> 
                     AlgSimplifyM ([ScalExp], [(NNumExp, BasicValue)])
trySimplifyDivRec [] fs' spl_terms = do
    return (fs', spl_terms)
trySimplifyDivRec (f:fs) fs' spl_terms = do
    res_tmp <- mapM (tryDivProdByOneFact f) spl_terms
    let (succs, spl_terms') = unzip res_tmp
    if all (==True) succs 
    then trySimplifyDivRec fs fs' spl_terms'
    else trySimplifyDivRec fs (fs'++[f]) spl_terms 


tryDivProdByOneFact :: ScalExp -> (NNumExp, BasicValue) -> AlgSimplifyM (Bool, (NNumExp, BasicValue))
tryDivProdByOneFact (Val f) (e, v) = do
    succc <- canDivValsEvenly v f
    if succc then do vres <- divVals v f
                     return (True, (e, vres))
             else return (False,(e, v) )

tryDivProdByOneFact _ pev@(NProd [] _, _) = return (False, pev)
tryDivProdByOneFact f pev@(NProd (t:tfs) tp, v) = do
    (succc, newt) <- tryDivTriv t f
    if not succc 
    then do (succ', (tfs', v')) <- tryDivProdByOneFact f (NProd tfs tp, v)
            case (succ', tfs') of
                (True,  NProd ((Val vv):tfs'') _) -> do 
                                    vres <- mulVals v' vv
                                    return $ (True, (NProd (t:tfs'') tp, vres))
                (True,  NProd tfs'' _) -> return $ (True, (NProd (t:tfs'') tp, v')) 
                (_, _) -> return $ (False, pev) 
    else do -- success 
            case (newt, tfs) of
                (Val vv, _) -> do vres <- mulVals vv v
                                  return $ (True, (NProd tfs tp, vres))
                (_,      _) -> return $ (True, (NProd (newt:tfs) tp, v)) 

tryDivProdByOneFact _ (NSum _ _, _) = do
    pos <- asks pos
    badAlgSimplifyM $ SimplifyError pos (
                     "tryDivProdByOneFact: unreachable case NSum reached!")


tryDivTriv :: ScalExp -> ScalExp -> AlgSimplifyM (Bool, ScalExp)
tryDivTriv (SPow a e1) (SPow d e2)
    | (a == d) && (e1 == e2) = do one <- getPos1 $ scalExpType a
                                  return $ (True, Val one)
    | (a == d) = do
          let tp = scalExpType a
          one <- getPos1 tp
          e1me2 <- simplifyScal $ SMinus e1 e2
          case (tp, e1me2) of
            (Int, Val (IntVal 0)) -> return (True, Val one)
            (Int, Val (IntVal 1)) -> return (True, a)
            (Int, _) -> do e2me1 <- negateSimplified e1me2
                           e2me1_sop <- toNumSofP e2me1
                           p' <- simplifyNRel True $ NRelExp LTH0 e2me1_sop
                           if p' == (LogCt True)
                           then return $ (True,  SPow a e1me2)
                           else return $ (False, SDivide (SPow a e1) (SPow d e2))

            (Real, Val (RealVal 0.0))   -> return (True, Val one)
            (Real, Val (RealVal 1.0))   -> return (True, a)
            (Real, Val (RealVal (-1.0)))-> return (True, SDivide (Val $ RealVal 1.0) a)
            (_, _) -> return $ (False, SDivide (SPow a e1) (SPow d e2))

    | otherwise = return $ (False, SDivide (SPow a e1) (SPow d e2))

tryDivTriv (SPow a e1) b 
    | a == b = do one <- getPos1 $ scalExpType a
                  tryDivTriv (SPow a e1) (SPow a (Val one))
    | otherwise = return (False, SDivide (SPow a e1) b)

tryDivTriv b (SPow a e1)
    | a == b = do one <- getPos1 $ scalExpType a
                  tryDivTriv (SPow a (Val one)) (SPow a e1)
    | otherwise = return (False, SDivide b (SPow a e1))

tryDivTriv t f 
    | t == f    = do one <- getPos1 $ scalExpType t
                     return (True,  Val one)
    | otherwise = do return (False, SDivide t f)

--------------------------------------------------------
---- TESTING
--------------------------------------------------------

mkIntExp :: Int -> ScalExp
mkIntExp 1 = 
    let (x',y',z',q') = (tident "int x", tident "int y", tident "int z", tident "int q")
        (x,y,z,q) = (Id x', Id y', Id z', Id q')
        up_term1 = SMinus (STimes (Val (IntVal 6)) (STimes x (STimes y z))) 
                          (STimes (Val (IntVal 4)) (STimes (STimes x q) z))
        up_term2 = SMinus (STimes x (STimes (Val (IntVal 4)) (STimes z q)))
                          (STimes (STimes (SPlus y q) (STimes z x) ) (Val (IntVal 3)) )
        dn_term  = STimes (STimes x (Val (IntVal 3))) z
    -- in dn_term
    -- in SPlus x  $ STimes (Val (IntVal (-1))) y 
    in  SDivide (SPlus up_term1 up_term2) dn_term

mkIntExp 2 = 
    let (x',y',z',q') = (tident "int x", tident "int y", tident "int z", tident "int q")
        (x,y,z,q) = (Id x', Id y', Id z', Id q')
        x2m3y    = SMinus (STimes (Val (IntVal 2)) x) (STimes y (Val (IntVal 3)))
        x2m3ylt0 = RelExp LTH0 x2m3y 
        y3m2xle0 = RelExp LEQ0 (SMinus (STimes y (Val (IntVal 3))) (STimes (Val (IntVal 2)) x))
    in SLogOr x2m3ylt0 $ SLogOr (Val (LogVal False)) $ SLogAnd z y3m2xle0
    --in SLogAnd (SLogAnd (SLogAnd x2m3ylt0 z) (SLogAnd q y3m2xle0)) (Val (LogVal True))

mkIntExp _ = Val (IntVal 3)

