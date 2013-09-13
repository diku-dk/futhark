{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module L0C.EnablingOpts.Simplify ( simplify )
  where

import Data.Loc
import L0C.L0

import L0C.EnablingOpts.EnablingOptErrors

import qualified Data.List as L
import Control.Monad
import Control.Applicative

newtype SimplifyM a = SimplifyM (Either EnablingOptError a)
    deriving (Monad, Applicative, Functor)

badSimplifyM :: EnablingOptError -> SimplifyM a
badSimplifyM = SimplifyM . Left


---------------------------------------------------------------------
-- RASMUS & JONAS: Feel free to come up with your
--                   own type for n-ary expressions, and
--                   a different implementation,
--                 I placed fill-in-the-blank
--                   comments in several places in the
--                   implementation of `simplifyNary',
--                 I made the program compile but no
--                   actual simplification is performed ...
--
--  Remember: 1. Place this file in `EnablingOpts' folder
--
--            2. In `EnablingOpts/CopyCtPropFold.hs'
--               a) add at the beginning the import statement:
--                     `import L0C.EnablingOpts.Simplify'
--
--               b) add before the definition of `ctFoldBinOp'
--                  the following wrapper function for simplify:
--                      simplExp :: Exp -> CPropM Exp
--                      simplExp e = do
--                        case simplify e of
--                          Left err -> badCPropM $ err
--                          Right e' -> return e'
--
--               c) Modify the implementation of `ctFoldBinOp'
--                    to use your simplifier
--
--               d) write your most complicated L0 program that
--                    is ``solved'' by expression simplification.
--
--            3. If you want to do a map with a function
--                 that returns a monad use mapM, e.g.,
--                   `kvs <- mapM  splitTerm terms'
---------------------------------------------------------------------

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
               -- An expression e that is not a
               --   multiplication is just:
               --   NaryMult [e] ...

               deriving (Eq, Ord, Show)

-----------------------------------------------
--- Main Function: simplifyNary             ---
-----------------------------------------------

-- | Applies Simplification at Expression level:
simplify :: Exp -> Either EnablingOptError Exp
simplify e = case simplifyBothWays e of
        SimplifyM (Left  err) -> Left err
        SimplifyM (Right e' ) -> Right e'


simplifyNary :: Exp -> SimplifyM NaryExp

simplifyNary (BinOp Plus e1 e2 tp pos) =
  do e1' <- simplifyNary e1
     e2' <- simplifyNary e2
     let terms = (getTerms e1') ++ (getTerms e2')

    --------------------------
    --- Fill in this part:
    --- My Pseudocode (you may come with your own or improve)
    --  1. split the terms, e.g., via splitTerm, into key-value pais,
    --     where the key is an N-ary expressions, and the value is a
    --     value factor, if no such factor than 1.
    --  2. Sort the result after the key; use L.sortBy
    --  3. Sum-up the values corresponding to the same key and remove
    --     the redundant keys -- this should be a segmented reduce.
    --  4. transform the resulted key-value list into a list of n-ary
    --     expressions by multiplying the key with its corresponding value.
    --  5. the result is supposed to be stored in terms'
     let terms' = terms
     return $ NaryPlus terms' tp pos

simplifyNary (BinOp Minus e1 e2 tp pos) = do
    min_1 <- getm1 tp pos
    let e2' = BinOp Times (Literal min_1 pos) e2 tp pos
    simplifyNary $ BinOp Plus e1 e2' tp pos

simplifyNary (BinOp Times e1 e2 tp pos) = do
    -------------------------------
    -- Fill in this part:
    -- My Pseudocode (you may come with your own or improve)
    -- 1. simplify recursively e1 and e2 and get their terms
    -- 2. ``multiply'' them, i.e., (a1 + .. + an) * (b1 + .. + bm) =
    --     a1*b1 + .. a1*bm + .. + an*b1 + .. + an*bm
    --    Here you may use makeProds if you figure out what it is
    --     doing or write your own.
    -- 3. would be nice to get a term value if any either at
    --    the begining or end of the result list
    --    the result is supposed to be stored in f12'
    -- 4. The next line is garbage:
    return $ NaryMult [e1, e2] tp pos
    where
        makeProds :: [Exp] -> NaryExp -> SimplifyM NaryExp
        makeProds [] _ = do
            badSimplifyM $ SimplifyError pos
                             " In simplifyNary, makeProds: 1st arg is the empty list! "
        makeProds _ (NaryMult [] _ _) = do
            badSimplifyM $ SimplifyError pos
                             " In simplifyNary, makeProds: 2nd arg is the empty list! "
        makeProds _ (NaryPlus _ _ _) = do
            badSimplifyM $ SimplifyError pos
                             " In simplifyNary, makeProds: e1 * e2: e2 is a sum of sums! "
        makeProds ((Literal v1 _):exs) (NaryMult ((Literal v2 pval):ys) tp1 pos1) = do
            v <- mulVals v1 v2 pval
            return $ NaryMult ( (Literal v pval) : (L.sort (ys++exs)) ) tp1 pos1
        makeProds ((Literal v pval):exs) (NaryMult ys tp1 pos1) = do
            return $ NaryMult ( (Literal v pval) : (L.sort (ys++exs)) ) tp1 pos1
        makeProds exs (NaryMult ((Literal v pval):ys) tp1 pos1) = do
            return $ NaryMult ( (Literal v pval) : (L.sort (ys++exs)) ) tp1 pos1
        makeProds exs (NaryMult ys tp1 pos1) = do
            return $ NaryMult (L.sort (ys++exs)) tp1 pos1

------------------------------------------------
-- Any other possible simplification, e.g., a
--   heuristic for simplifying division, or some
--   other operator of L0?
------------------------------------------------

simplifyNary e = return $ NaryMult [e] (typeOf e) (srclocOf e)


simplifyBack :: NaryExp -> SimplifyM Exp
simplifyBack (NaryMult [] _ pos) =
    badSimplifyM $ SimplifyError pos
                    " In simplifyBack, NaryMult: empty exp list! "
simplifyBack (NaryPlus [] _ pos) =
    badSimplifyM $ SimplifyError pos
                    " In simplifyBack, NaryPlus: empty exp list! "
simplifyBack (NaryMult [f] _ _) = do
    return f
simplifyBack (NaryPlus [t] _ _) = do
    simplifyBack t
simplifyBack (NaryMult (f:fs) tp pos) = do
    fs' <- simplifyBack $ NaryMult fs tp pos
    return $ BinOp Times f fs' tp pos
simplifyBack (NaryPlus (f:fs) tp pos) = do
    f'  <- simplifyBack f
    fs' <- simplifyBack $ NaryPlus fs tp pos
    return $ BinOp Plus f' fs' tp pos

simplifyBothWays :: Exp -> SimplifyM Exp
simplifyBothWays e = do
    enary <- simplifyNary e
    simplifyBack enary

----------------------------------------------
--- Accessor Functions for n-ary exprs     ---
--- typeOf, srclocOf, getTerms, addVals,   ---
--- mulVals, mulWithVal                    ---
----------------------------------------------

typeOfNary :: NaryExp -> Type
typeOfNary (NaryPlus _ tp _) = tp
typeOfNary (NaryMult _ tp _) = tp

srclocOfNary :: NaryExp -> SrcLoc
srclocOfNary (NaryPlus _ _ pos) = pos
srclocOfNary (NaryMult _ _ pos) = pos

getTerms :: NaryExp -> [NaryExp]
getTerms (NaryPlus es  _ _)  = es
getTerms e                   = [e]

addVals :: Value -> Value -> SrcLoc -> SimplifyM Value
addVals e1 e2 pos = do
    case (e1, e2) of
      ( IntVal v1,  IntVal v2) -> return $  IntVal (v1+v2)
      (RealVal v1, RealVal v2) -> return $ RealVal (v1+v2)
      _ -> badSimplifyM $ SimplifyError pos  " + operands not of (the same) numeral type! "

mulVals :: Value -> Value -> SrcLoc -> SimplifyM Value
mulVals e1 e2 pos = do
    case (e1, e2) of
      ( IntVal v1,  IntVal v2) -> return $  IntVal (v1*v2)
      (RealVal v1, RealVal v2) -> return $ RealVal (v1*v2)
      _ -> badSimplifyM $ SimplifyError pos (" * operands not of (the same) numeral type! ")

multWithVal :: (NaryExp, Value) -> SimplifyM NaryExp
multWithVal (e, v) = do
    let pos = srclocOfNary e
    let tp  = typeOfNary e
    one    <- getp1 tp pos
    if (v == one)
    then return e
    else do let vt = (Literal v pos)
            case e of
                NaryMult [] _ _ -> badSimplifyM $ SimplifyError pos
                                                    (" In Simplify.hs, multWithVal: NaryMult is empty! ")
                NaryMult (f:fs) _ _ -> do
                    case f of
                        Literal v' _ -> do
                            vv' <- mulVals v v' pos
                            return $ NaryMult ( Literal vv' pos : fs ) tp pos
                        _ ->return $ NaryMult (vt:f:fs) tp pos
                NaryPlus ts _ _ -> do
                    ts' <- mapM ( \x -> multWithVal (x, v) ) ts
                    return $ NaryPlus ts' tp pos


--------------------------------------------
--- Helper Simplification Functions      ---
--- get0, getp1, getm1, splitTerm,       ---
--- splitFactor, discriminate            ---
--------------------------------------------

{-
get0 :: Type -> SrcLoc -> SimplifyM Value
get0 (Elem (Int  )) _ = return $  IntVal 0
get0 (Elem (Real )) _ = return $ RealVal 0.0
get0 tp             p = badSimplifyM $ SimplifyError p ("get0 for type: "++ppType tp)
-}

getp1 :: Type -> SrcLoc -> SimplifyM Value
getp1 (Elem Int  ) _ = return $  IntVal 1
getp1 (Elem Real ) _ = return $ RealVal 1.0
getp1 tp             p = badSimplifyM $ SimplifyError p ("getp1 for type: "++ppType tp)

getm1 :: Type -> SrcLoc -> SimplifyM Value
getm1 (Elem Int  ) _ = return $  IntVal (-1)
getm1 (Elem Real ) _ = return $ RealVal (-1.0)
getm1 tp             p = badSimplifyM $ SimplifyError p ("getm1 for type: "++ppType tp)


splitTerm :: NaryExp -> SimplifyM (NaryExp, Value)
splitTerm (NaryMult [ ] _ pos) =
    badSimplifyM $ SimplifyError pos "Empty n-ary list of factors."
splitTerm (NaryMult [f] tp pos) = do
    one <- getp1 tp pos
    case f of
        (Literal v _) -> return (NaryMult [Literal one pos] tp pos, v  )
        e             -> return (NaryMult [e]               tp pos, one)
splitTerm ne@(NaryMult (f:fs) tp pos) =
    case f of
        (Literal v _) -> return (NaryMult fs tp pos, v)
        _             -> do one <- getp1 tp pos
                            return (ne, one)
splitTerm e = do
    one <- getp1 (typeOfNary e) (srclocOfNary e)
    return (e, one)


discriminate :: [(NaryExp, Value)] -> (NaryExp, Value) -> SimplifyM [(NaryExp, Value)]
discriminate []          e        = return [e]
discriminate e@((k,v):t) (k', v') =
    if k == k'
    then do v'' <- addVals v v' (srclocOfNary k')
            return ( (k, v'') : t )
    else return ( (k', v') : e )
