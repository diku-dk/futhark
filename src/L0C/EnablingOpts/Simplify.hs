{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module L0C.EnablingOpts.Simplify ( simplify, canSimplify )
  where

import Data.Loc
import L0C.L0

import L0C.EnablingOpts.EnablingOptErrors

import qualified Data.List as L
import Control.Monad
import Control.Applicative

import Debug.Trace
import L0C.EscapeColor

newtype SimplifyM a = SimplifyM (Either EnablingOptError a)
    deriving (Monad, Applicative, Functor)

badSimplifyM :: EnablingOptError -> SimplifyM a
badSimplifyM = SimplifyM . Left

-- TODO: most complicated L0 program that is ``solved'' by expression simplification.

------------------------------------
-- The new type for an expression --
--    used for the simplification/--
--    normalization purpose.      --
-- The expression is translated to--
--   this form, simplified and    --
--   then translated back to a L0 --
--   expression.                  --
------------------------------------
data NaryExp = NaryPlus [NaryExp] Type SrcLoc
               -- ^ An n-ary plus expression,
               --   i.e., a1 + a2 + ... + an.
               -- Note that the expressions in
               --   the list are either (Nary Exp)
               --   or (NaryMult [Exp] _ _),
               --   i.e., an n-ary sum cannot
               --   contain a term that is itself
               --   an n-ary sum.

             | NaryMult [Exp] Type SrcLoc
               -- ^ An n-ary multiply expression,
               --   i.e., a1 * a2 * ... * an.
               -- Literals must be kept as the
               --   first element in the list
               -- An expression e that is not a
               --   multiplication is just:
               --   NaryMult [e] ...

               deriving (Eq, Ord, Show)

-----------------------------------------------
--- Publicly exposed functions:             ---
--- simplify, canSimplify                   ---
-----------------------------------------------

-- | Applies Simplification at Expression level:
simplify :: Exp -> Either EnablingOptError Exp
simplify e = case simplifyBothWays e of
        SimplifyM (Left  err) -> Left err
        SimplifyM (Right e' ) -> Right e'

-- | Test if Simplification engine can handle this kind of expression
canSimplify :: Exp -> Bool
canSimplify (e) = case typeOf e of
                    Elem Int  -> True
                    Elem Real -> True
                    _         -> False

-----------------------------------------------
--- Helper for publicly exposed functions:  ---
--- simplifyBothWays, simplifyBack          ---
-----------------------------------------------

simplifyBack :: NaryExp -> SimplifyM Exp
simplifyBack (NaryMult [] _ pos) =
  badSimplifyM $ SimplifyError pos
                    " In simplifyBack, NaryMult: empty exp list! "
simplifyBack (NaryPlus [] _ pos) =
  badSimplifyM $ SimplifyError pos
                    " In simplifyBack, NaryPlus: empty exp list! "
simplifyBack (NaryMult [f] _ _) = return f
simplifyBack (NaryPlus [t] _ _) = simplifyBack t
simplifyBack (NaryMult (f:fs) tp pos) = do
  fs' <- simplifyBack $ NaryMult fs tp pos
  return $ BinOp Times f fs' tp pos
simplifyBack (NaryPlus (f:fs) tp pos) = do
  f'  <- simplifyBack f
  fs' <- simplifyBack $ NaryPlus fs tp pos
  return $ BinOp Plus f' fs' tp pos

simplifyBothWays :: Exp -> SimplifyM Exp
simplifyBothWays e = do
  --{- No debug
  enary <- simplifyNary e
  simplifyBack enary
  --}
  {- Debug before/after simplification
  enary <- trace (escapeColorize Magenta $ "Before: " ++ ppExp e) simplifyNary e
  e' <- simplifyBack enary
  trace (escapeColorize Green $ "After: " ++ ppExp e') return e'
  --}

-----------------------------------------------
--- Main Function: simplifyNary             ---
-----------------------------------------------

simplifyNary :: Exp -> SimplifyM NaryExp

simplifyNary (Min e1 e2 tp pos) = do
    e1'<- (simplifyNary e1) >>= simplifyBack
    e2'<- (simplifyNary e2) >>= simplifyBack
    case (e1',e2') of
        (Literal (IntVal v1) _, Literal (IntVal v2) _) ->
            return $ NaryMult [(Literal (IntVal $ min v1 v2) pos)] tp pos
        _ -> 
            return $ NaryMult [(Min e1' e2' tp pos)] tp pos

simplifyNary (Max e1 e2 tp pos) = do
    e1'<- (simplifyNary e1) >>= simplifyBack
    e2'<- (simplifyNary e2) >>= simplifyBack
    case (e1',e2') of
        (Literal (IntVal v1) _, Literal (IntVal v2) _) ->
            return $ NaryMult [(Literal (IntVal $ max v1 v2) pos)] tp pos
        _ -> 
            return $ NaryMult [(Max e1' e2' tp pos)] tp pos

simplifyNary (BinOp Plus (Max e1' e2' _ _) e2 tp pos) = do
    let e1'' = BinOp Plus e1' e2 tp pos
    let e2'' = BinOp Plus e2' e2 tp pos
    simplifyNary $ Max e1'' e2'' tp pos

simplifyNary (BinOp Plus e1 e2@(Max _ _ _ _) tp pos) =
    simplifyNary (BinOp Plus e2 e1 tp pos)


simplifyNary (BinOp Plus (Min e1' e2' _ _) e2 tp pos) = do
    let e1'' = BinOp Plus e1' e2 tp pos
    let e2'' = BinOp Plus e2' e2 tp pos
    simplifyNary $ Min e1'' e2'' tp pos

simplifyNary (BinOp Plus e1 e2@(Min _ _ _ _) tp pos) =
    simplifyNary (BinOp Plus e2 e1 tp pos)


simplifyNary (BinOp Plus e1 e2 tp pos) = do
     e1' <- simplifyNary e1
     e2' <- simplifyNary e2
     let terms = getTerms e1' ++ getTerms e2'
     splittedTerms <- mapM splitTerm terms
     let sortedTerms = L.sortBy (\(n1,_) (n2,_) -> compare n1 n2) splittedTerms
     -- foldM discriminate, reverses the list, we would like to keep it in a ascending order.
     merged <- liftM reverse $ foldM discriminate [] sortedTerms
     let filtered = filter (\(_,v) -> not $ isValue0 v ) merged
     if null filtered
     then do
        zero <- get0 tp pos
        return $ NaryMult [Literal zero pos] tp pos
     else do
         terms' <- mapM joinTerm filtered
         return $ NaryPlus terms' tp pos

simplifyNary (BinOp Minus e1 e2 tp pos) = do
    min_1 <- getNeg1 tp pos
    let e2' = BinOp Times (Literal min_1 pos) e2 tp pos
    simplifyNary $ BinOp Plus e1 e2' tp pos

-- TODO: sorting function that ensures literals are first for use in BinOp times
--

---------------------------------------------------------------------
-- Uncommented until we a working function for sign determination. -- 
---------------------------------------------------------------------
--
-- simplifyNary (BinOp Times e1 e2@(Max _ _ _ _) tp pos) =
--     simplifyNary (BinOp Times e2 e1 tp pos)
-- 
-- simplifyNary (BinOp Times (Max e1' e2' _ _) e2 tp pos) = do
--     let e1'' = BinOp Times e1' e2 tp pos
--     let e2'' = BinOp Times e2' e2 tp pos
--     -- TODO: has to return bool value, based on the sign of e2
--     let op   = if True then Max else Min 
--     simplifyNary $ op e1'' e2'' tp pos
-- 
-- simplifyNary (BinOp Times e1 e2@(Min _ _ _ _) tp pos) =
--     simplifyNary (BinOp Times e2 e1 tp pos)
-- 
-- simplifyNary (BinOp Times (Min e1' e2' _ _) e2 tp pos) = do
--     let e1'' = BinOp Times e1' e2 tp pos
--     let e2'' = BinOp Times e2' e2 tp pos
--     -- TODO: has to return bool value, based on the sign of e2
--     let op   = if True then Min else Max
--     simplifyNary $ op e1'' e2'' tp pos

simplifyNary (BinOp Times e1 e2 tp pos) = do
     e1' <- simplifyNary e1
     e2' <- simplifyNary e2
     case (e1', e2') of
          (NaryMult xs _ _, y@(NaryMult{}) ) -> makeProds xs y
          (NaryMult xs _ _, y) ->
              do prods <- mapM (makeProds xs) $ getTerms y
                 return $ NaryPlus (L.sort prods) tp pos
          (x, NaryMult ys _ _) ->
              do prods <- mapM (makeProds ys) $ getTerms x
                 return $ NaryPlus (L.sort prods) tp pos
          (NaryPlus xs _ _, NaryPlus ys _ _) ->
                              do xsMultChildren <- mapM getMultChildren xs
                                 prods <- mapM (\x -> mapM (makeProds x) ys) xsMultChildren
                                 return $ NaryPlus (L.sort $ concat prods) tp pos

    where
        makeProds :: [Exp] -> NaryExp -> SimplifyM NaryExp
        makeProds [] _ =
          badSimplifyM $ SimplifyError pos
              " In simplifyNary, makeProds: 1st arg is the empty list! "
        makeProds _ (NaryMult [] _ _) =
          badSimplifyM $ SimplifyError pos
            " In simplifyNary, makeProds: 2nd arg is the empty list! "
        makeProds _ (NaryPlus{}) =
          badSimplifyM $ SimplifyError pos
            " In simplifyNary, makeProds: e1 * e2: e2 is a sum of sums! "
        makeProds (Literal v1 _ :exs) (NaryMult (Literal v2 pval : ys) tp1 pos1) = do
          v <- mulVals v1 v2 pval
          return $ NaryMult ( Literal v pval : L.sort (ys++exs) ) tp1 pos1
        makeProds (Literal v pval : exs) (NaryMult ys tp1 pos1) =
          return $ NaryMult ( Literal v pval : L.sort (ys++exs) ) tp1 pos1
        makeProds exs (NaryMult (Literal v pval : ys) tp1 pos1) =
          return $ NaryMult ( Literal v pval : L.sort (ys++exs) ) tp1 pos1
        makeProds exs (NaryMult ys tp1 pos1) =
          return $ NaryMult (L.sort (ys++exs)) tp1 pos1

simplifyNary (BinOp Divide e1 e2 tp pos) = do
    e1' <- simplifyNary e1
    e2' <- simplifyNary e2
    let numeratorTerms = getTerms e1'
    let denominatorTerms = getTerms e2'
    if numeratorTerms == denominatorTerms
    then do one <- getPos1 tp pos
            return $ NaryMult [Literal one pos] tp pos
    else do numeSplitted <- mapM splitTerm numeratorTerms
            denomSplitted <- mapM splitTerm denominatorTerms
            case (numeSplitted, denomSplitted) of
              (_,[]) -> badSimplifyM $ SimplifyError pos
                          "In simplifyNary, BinOp Divide: trying to divide by zero! "

              ([],_) -> do zero <- get0 tp pos
                           return $ NaryMult [Literal zero pos] tp pos

              -- This clause implies both the numerator and denominator are NaryMult
              -- as NaryPlus cannot be nested two levels deep
              ([numeKV],[demonKV]) -> divTwoNaryMults numeKV demonKV

              (_, _) -> return $ NaryMult [BinOp Divide e1 e2 tp pos] tp pos

    where
          divTwoNaryMults :: (NaryExp, Value) -> (NaryExp, Value) -> SimplifyM NaryExp
          divTwoNaryMults (NaryMult xs _ _, numeFactor) (NaryMult ys _ _, denomFactor) = do
              res <- divVals numeFactor denomFactor pos
              let resLit = Literal res pos

              goodDiv <- canDivValsEvenly numeFactor denomFactor pos
              if xs == ys
              -- a `div` b == a * x `div` b * x
              then return $ NaryMult [resLit] tp pos
              else let (numeExps, denomExps) = divTwoProducts(xs, ys) in
                   if null denomExps
                   -- (numeFactor * a0*..*an) / denomFactor
                   then if goodDiv
                        -- Clean divide, ie 4 * a0*..*an / 2 = 2 * a0..*an
                        then return $ NaryMult (resLit : numeExps) tp pos
                        -- Unclean divide, ie 4 * a0*..*an / 3, result will depend on a0*..*an. If 3, then 4 * 3 / 3 = 4, if 1 then 4 / 3 = 1
                        else do numeExp' <- simplifyBack $ NaryMult (numeFactorLit : numeExps) tp pos
                                let divExp = BinOp Divide numeExp' denomFactorLit tp pos
                                return $ NaryMult [divExp] tp pos
                   -- create divide expression from (hopefully more) simplified subexpressions
                   else do allNum <- simplifyBack $ NaryMult (numeFactorLit : numeExps) tp pos
                           allDenom <- simplifyBack $ NaryMult (denomFactorLit : denomExps) tp pos
                           let divExp = BinOp Divide allNum allDenom tp pos
                           return $ NaryMult [divExp] tp pos

              where numeFactorLit = Literal numeFactor pos
                    denomFactorLit = Literal denomFactor pos

          divTwoNaryMults _ _ = badSimplifyM $ SimplifyError pos "divTwoNaryMults, not for NaryPlus "

simplifyNary (Negate e tp pos) = do
    negOne <- getNeg1 tp pos
    simplifyNary $ BinOp Times (Literal negOne pos) e tp pos


------------------------------------------------
-- TODO: Any other possible simplification,
--   e.g., a heuristic for simplifying
--   division, or some other operator of L0?
------------------------------------------------

simplifyNary e = return $ NaryMult [e] (typeOf e) (srclocOf e)

----------------------------------------------
--- Accessor Functions for n-ary exprs     ---
--- typeOf, srclocOf, getTerms, addVals,   ---
--- mulVals, divVals, canDivValsEvenly     ---
--- divTwoProducts                         ---
----------------------------------------------

typeOfNary :: NaryExp -> Type
typeOfNary (NaryPlus _ tp _) = tp
typeOfNary (NaryMult _ tp _) = tp

srclocOfNary :: NaryExp -> SrcLoc
srclocOfNary (NaryPlus _ _ pos) = pos
srclocOfNary (NaryMult _ _ pos) = pos

getMultChildren :: NaryExp -> SimplifyM [Exp]
getMultChildren (NaryPlus _ _ pos) = badSimplifyM $ SimplifyError pos "getMultChildren, NaryPlus should not be nested 2 levels deep "
getMultChildren (NaryMult xs _ _) = return xs

getTerms :: NaryExp -> [NaryExp]
getTerms (NaryPlus es  _ _)  = es
getTerms e                   = [e]

addVals :: Value -> Value -> SrcLoc -> SimplifyM Value
addVals e1 e2 pos =
  case (e1, e2) of
    (IntVal v1, IntVal v2) -> return $ IntVal (v1+v2)
    (RealVal v1, RealVal v2) -> return $ RealVal (v1+v2)
    _ -> badSimplifyM $ SimplifyError pos  "addVals: operands not of (the same) numeral type! "

mulVals :: Value -> Value -> SrcLoc -> SimplifyM Value
mulVals e1 e2 pos =
  case (e1, e2) of
    (IntVal v1, IntVal v2) -> return $ IntVal (v1*v2)
    (RealVal v1, RealVal v2) -> return $ RealVal (v1*v2)
    _ -> badSimplifyM $ SimplifyError pos "mulVals: operands not of (the same) numeral type! "

divVals :: Value -> Value -> SrcLoc -> SimplifyM Value
divVals e1 e2 pos =
  case (e1, e2) of
    (IntVal v1, IntVal v2) -> return $ IntVal (v1 `div` v2)
    (RealVal v1, RealVal v2) -> return $ RealVal (v1/v2)
    _ -> badSimplifyM $ SimplifyError pos  "divVals: operands not of (the same) numeral type! "

canDivValsEvenly :: Value -> Value -> SrcLoc -> SimplifyM Bool
canDivValsEvenly e1 e2 pos =
  case (e1, e2) of
    (IntVal v1,  IntVal v2) -> return $ v1 `mod` v2 == 0
    (RealVal _, RealVal _) -> return True
    _ -> badSimplifyM $ SimplifyError pos  "canDivValsEvenly: operands not of (the same) numeral type! "

-- Simplify the expression x0*..*xn / y0*..*ym
-- Both lists must be sorted for this to work
divTwoProducts :: ([Exp], [Exp]) -> ([Exp], [Exp])
divTwoProducts ([], []) = ([], [])
divTwoProducts ([], ys) = ([], ys)
divTwoProducts (xs, []) = (xs, [])
divTwoProducts (x:xs, y:ys) =
  case x `compare` y of
    EQ -> divTwoProducts(xs,ys)
    LT -> let (xs',ys') = divTwoProducts(xs,y:ys)
          in (x:xs',ys')
    GT -> let (xs',ys') = divTwoProducts(x:xs,ys)
          in (xs',y:ys')

--------------------------------------------
--- Helper Simplification Functions      ---
--- get0, getPos1, getNeg1,              ---
--- isValue0, isValue1                   ---
--- splitTerm, joinTerm, discriminate    ---
--------------------------------------------

get0 :: Type -> SrcLoc -> SimplifyM Value
get0 (Elem Int  ) _ = return $ IntVal 0
get0 (Elem Real ) _ = return $ RealVal 0.0
get0 tp             p = badSimplifyM $ SimplifyError p ("get0 for type: "++ppType tp)

getPos1 :: Type -> SrcLoc -> SimplifyM Value
getPos1 (Elem Int  ) _ = return $  IntVal 1
getPos1 (Elem Real ) _ = return $ RealVal 1.0
getPos1 tp             p = badSimplifyM $ SimplifyError p ("getPos1 for type: "++ppType tp)

getNeg1 :: Type -> SrcLoc -> SimplifyM Value
getNeg1 (Elem Int  ) _ = return $  IntVal (-1)
getNeg1 (Elem Real ) _ = return $ RealVal (-1.0)
getNeg1 tp             p = badSimplifyM $ SimplifyError p ("getNeg1 for type: "++ppType tp)

isValue0 :: Value -> Bool
isValue0 (IntVal v)  = v == 0
isValue0 (RealVal v) = v == 0.0
isValue0 (_)         = False

isValue1 :: Value -> Bool
isValue1 (IntVal v)  = v == 1
isValue1 (RealVal v) = v == 1.0
isValue1 (_)         = False

splitTerm :: NaryExp -> SimplifyM (NaryExp, Value)
splitTerm (NaryMult [ ] _ pos) =
    badSimplifyM $ SimplifyError pos "splitTerm: Empty n-ary list of factors."
splitTerm (NaryMult [f] tp pos) = do
  one <- getPos1 tp pos
  case f of
      (Literal v _) -> return (NaryMult [Literal one pos] tp pos, v  )
      e             -> return (NaryMult [e]               tp pos, one)
splitTerm ne@(NaryMult (f:fs) tp pos) =
  case f of
      (Literal v _) -> return (NaryMult fs tp pos, v)
      _             -> do one <- getPos1 tp pos
                          return (ne, one)
splitTerm e = do
  one <- getPos1 (typeOfNary e) (srclocOfNary e)
  return (e, one)

joinTerm :: (NaryExp, Value) -> SimplifyM NaryExp
joinTerm ( NaryPlus _ _ pos, _) =
    badSimplifyM $ SimplifyError pos "joinTerm: NaryPlus two levels deep."
joinTerm ( NaryMult [] _ pos, _) =
    badSimplifyM $ SimplifyError pos "joinTerm: Empty n-ary mult."
joinTerm ( NaryMult (Literal l lp:fs) tp pos, v) = do
    v' <- mulVals v l lp
    let v'Lit = Literal v' lp
    return $ NaryMult (v'Lit:fs) tp pos
joinTerm ( e@(NaryMult fs tp pos), v)
  | isValue1 v = return e
  | otherwise = let vExp = Literal v pos
                in return $ NaryMult (vExp:fs) tp pos

discriminate :: [(NaryExp, Value)] -> (NaryExp, Value) -> SimplifyM [(NaryExp, Value)]
discriminate []          e        = return [e]
discriminate e@((k,v):t) (k', v') =
  if k == k'
  then do v'' <- addVals v v' (srclocOfNary k')
          return ( (k, v'') : t )
  else return ( (k', v') : e )
