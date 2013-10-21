module L0C.EnablingOpts.Range (
     Range
   , RangeDict
   , RExp(..)
   , Sign(..)
   , substitute
   , createRangeAndSign
   , emptyRangeDict
)
where
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
import qualified Data.Map as Map

-- data Exp = Plus Exp Exp | Ident Char | IntVal Int
--     deriving (Show, Eq)
--
data RExp = RExp Exp | Pinf | Ninf
      deriving (Show, Eq)

----------------------------------------

type Range = (RExp, RExp)

type RangeDict = Map.Map VName (Range, Sign)

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
          deriving (Show, Eq)

data InequalityRelationship = LT
                            | LTE
                            | EQ
                            | GTE
                            | GT
                            | Unknown

----------------------------------------

rangeUnknown :: (Range, Sign)
rangeUnknown = ( (Ninf, Pinf) , AnySign )

emptyRangeDict :: RangeDict
emptyRangeDict = Map.empty

-----------------------------------------

createRangeAndSign :: RangeDict -> Maybe Exp -> (Range, Sign)
createRangeAndSign _ Nothing = rangeUnknown
createRangeAndSign _ _ = rangeUnknown


simplExp :: Exp -> Exp
simplExp e =
    case simplify e of
        Right e' -> e'

substitute :: Ident-> Range -> RExp -> Range
substitute i r l@(RExp (Literal v sl)) = (l,l)
substitute i r v@(RExp (Var e)) = if e == i then r else (v,v)
substitute i r (RExp (BinOp Plus e1 e2 ty pos)) =
  let
    (e1lb, e1ub) = substitute i r (RExp e1)
    (e2lb, e2ub) = substitute i r (RExp e2)
    lb = case (e1lb, e2lb) of
        (RExp e1,RExp e2) -> RExp $ simplExp (BinOp Plus e1 e2 ty pos)
        otherwise -> Ninf
    ub = case (e1ub, e2ub) of
        (RExp e1,RExp e2) -> RExp $ simplExp (BinOp Plus e1 e2 ty pos)
        otherwise -> Pinf
  in
    (lb,ub)

substitute i r (RExp (BinOp Minus e1 e2 ty pos)) =
  let
    (e1lb, e1ub) = substitute i r (RExp e1)
    (e2lb, e2ub) = substitute i r (RExp e2)
    lb = case (e1lb, e2lb) of
        (RExp e1,RExp e2) -> RExp $ simplExp (BinOp Minus e1 e2 ty pos)
        otherwise -> Ninf
    ub = case (e1ub, e2ub) of
        (RExp e1,RExp e2) -> RExp $ simplExp (BinOp Minus e1 e2 ty pos)
        otherwise -> Pinf
  in
    (lb,ub)

substitute i r (RExp (BinOp Times e1 e2 ty pos)) =
  let
    (e1lb, e1ub) = substitute i r (RExp e1)
    (e2lb, e2ub) = substitute i r (RExp e2)
    lb = case (e1lb, e1ub, e2lb) of
        (RExp e1lb',RExp e1ub',RExp (e2lb'@(Literal (IntVal n) sl))) ->
            if n >= 0
            then RExp $ simplExp (BinOp Times e1lb' e2lb' ty pos)
            else RExp $ simplExp (BinOp Times e1ub' e2lb' ty pos)
        otherwise -> Ninf
    ub = case (e2lb, e2ub, e2ub) of
        (RExp e2lb',RExp e2ub',RExp (e1ub'@(Literal (IntVal n) sl))) ->
            if n >= 0
            then RExp $ simplExp (BinOp Times e1ub' e2ub' ty pos)
            else RExp $ simplExp (BinOp Times e1ub' e2lb' ty pos)
        otherwise -> Pinf
  in
    (lb,ub)


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
      RExp $ Var y)

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
