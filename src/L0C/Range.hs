-- module L0C.EnablingOpts.Range (
--     Range
--   , RExp
--   , substitute
--     )
-- where
-- 
-- Exp = P
-- 

import L0C.L0
import Debug.Trace
import qualified Data.Loc
import Control.Monad
import Control.Applicative
import Data.Either

-- import L0C.L0
import L0C.EnablingOpts.Simplify
-- import qualified Data.Loc
-- import qualified Data.Map as Map

-- data Exp = Plus Exp Exp | Ident Char | IntVal Int
--     deriving (Show, Eq)
-- 
data RExp = RExp Exp | Pinf | Ninf
      deriving (Show, Eq)
-- 
type Range = (RExp, RExp)

extractExp :: RExp -> Exp
extractExp (RExp e) = e

simplExp :: Exp -> Exp
simplExp e = 
    case simplify e of
        Right e' -> e'

substitute :: Ident-> Range -> RExp -> Range
substitute i r l@(RExp (Literal v sl)) = (l,l) 
substitute i r v@(RExp (Var e)) = if e == i then r else (v,v)
substitute i r (RExp (BinOp Plus e1 e2 ty pos)) =
    let (e1lb, e1ub) = substitute i r (RExp e1) in
    let (e2lb, e2ub) = substitute i r (RExp e2) in
    let lb = simplExp(BinOp Plus (extractExp e1lb) (extractExp e2lb) ty pos) in
    let ub = simplExp(BinOp Plus (extractExp e1ub) (extractExp e2ub) ty pos) in
    (RExp lb, RExp ub) 

substitute i r (RExp (BinOp Minus e1 e2 ty pos)) =
    let (e1lb, e1ub) = substitute i r (RExp e1) in
    let (e2lb, e2ub) = substitute i r (RExp e2) in
    let lb = simplExp(BinOp Minus (extractExp e1lb) (extractExp e2lb) ty pos) in
    let ub = simplExp(BinOp Minus (extractExp e1ub) (extractExp e2ub) ty pos) in
    (RExp lb, RExp ub) 

substitute i r (RExp (BinOp Mul e1 e2 ty pos)) =
    let (e1lb, e1ub) = substitute i r (RExp e1) in
    let (e2lb, e2ub) = substitute i r (RExp e2) in
    let lb = simplExp(BinOp Plus (extractExp e1lb) (extractExp e2lb) ty pos) in
    let ub = simplExp(BinOp Plus (extractExp e1ub) (extractExp e2ub) ty pos) in
    (RExp lb, RExp ub) 

-- TESTING

res = substitute x ra rexp

dummyPos = Data.Loc.Pos "DummyPos" 10 0 0
dummySrcLoc = Data.Loc.SrcLoc (Data.Loc.Loc dummyPos dummyPos)

x = Ident {identName = ID (nameFromString "x",0),
                 identType = Elem Int,
                 identSrcLoc = dummySrcLoc }

y = Ident {identName = ID (nameFromString "y",1),
                 identType = Elem Int,
                 identSrcLoc = dummySrcLoc }

ra = (RExp $ Literal (IntVal 1) dummySrcLoc,
      RExp $ Var $ y)

rexp =
    let x' = Var x in
    --let x'' = BinOp Pow (Var x) (Literal (IntVal 2) dummySrcLoc) (Elem Int) dummySrcLoc in 
    let x'' = Literal (IntVal 2) dummySrcLoc in
    RExp $ BinOp Plus x'' x' (Elem Int) dummySrcLoc
    

-- empty :: Range a
-- empty = Empty
-- 
-- 
-- singleton :: a -> Range a
-- singleton x = Bounds x x
-- 
-- 
-- range :: (Ord a) => a -> a -> Range a
-- range l u = if l <= u
--     then Bounds l u
--     else Empty
-- 
-- 
-- bounds :: Range a -> Maybe (a, a)
-- bounds Empty = Nothing
-- bounds (Bounds l u) = Just (l, u)
-- 
-- 
-- contains :: (Ord a) => Range a -> a -> Bool
-- contains Empty _ = False
-- contains (Bounds l u) x = l <= x && x <= u
-- 
-- 
-- extendTo :: (Ord a) => Range a -> a -> Range a
-- extendTo Empty x = singleton x
-- extendTo (Bounds l u) x
--     | x < l = Bounds x u
--     | x > u = Bounds l x
--     | otherwise = Bounds l u
-- 
-- 
-- intersect :: (Ord a) => Range a -> Range a -> Range a
-- intersect Empty _ = Empty
-- intersect _ Empty = Empty
-- intersect (Bounds l1 u1) (Bounds l2 u2) = range l u
--     where
--         l = max l1 l2
--         u = min u1 u2
-- 
-- 
-- union :: (Ord a) => Range a -> Range a -> Maybe (Range a)
-- union Empty range = Just range
-- union range Empty = Just range
-- union b1@(Bounds l1 u1) b2@(Bounds l2 u2) = case intersect b1 b2 of
--     Empty -> Nothing
--     Bounds{} -> Just $ Bounds l u
--     where
--         l = min l1 l2
--         u = max u1 u2
-- 
-- 
-- 
