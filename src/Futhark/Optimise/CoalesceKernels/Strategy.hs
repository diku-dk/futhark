module Futhark.Optimise.CoalesceKernels.Strategy where

-- Imports-- {{{
import Control.Monad
import qualified Data.Map.Lazy as Map
import Data.List (nub, sort, minimumBy)
import Data.Maybe

import qualified Data.HashSet as HS

import Futhark.Representation.AST.Syntax
-- }}}

import Futhark.Util.Pretty

-- Types-- {{{
type Var = VName

type In a = Maybe a
type Out a = [a]
type Tr = Int
type ArrMap = Map.Map Var Tr

data Access = Access VName [Names]
  deriving (Show)

instance Pretty (Access) where
  ppr = text . show


data Strategy = Strategy { interchange :: (In Var, Out Var)
                         , transposes :: ArrMap
                         } deriving (Show, Eq)

-- }}}

-- The real meat. -- {{{
chooseStrategy :: [VName] -> [Access] -> Strategy
chooseStrategy lvs stms = case makeStrategy (length lvs) init_strats of
                            [] -> unit -- Nil transformation
                            ss -> minimumBy accessCmp ss
  where init_strats = map (generate lvs) stms


-- Fold over all array accesses generated
makeStrategy :: Int -> [[Strategy]] -> [Strategy]
makeStrategy allout = filter notAllOut . foldr combine [unit]
  where combine xs ys = catMaybes [ plus x y | x <- xs, y <- ys ]
        notAllOut (Strategy (_,o) _) = length o < allout 

-- Generate all the possible startegy choices for a single array lookup
-- [LoopVars] ArrStrategy -> [Initial As]
generate :: [Var] -> Access -> [Strategy]
generate lvs a@(Access _ is) = catMaybes (out : ins) -- Just put them together
  where 
    variants = filter (isVariantTo is) lvs                  -- Find all lvs variant in the Index
    ins = map (pushIn a) variants                           -- Coalesced access
    out = Just $ Strategy (Nothing, variants) Map.empty     -- Invariant access

    pushIn :: Access -> VName -> Maybe Strategy
    pushIn (Access arr idxs) lv =
      case variantIndices idxs lv of
        []  -> Just $ Strategy (Just lv, []) Map.empty
                -- Strategy is invariant to lv
        [x] -> Just $ Strategy (Just lv, []) (Map.singleton arr x)
                -- Strategy is variant in one index.
        _   -> Nothing
                -- Strategy is variant in more indexes to lv. No use in pushing in.
-- }}}

-- Monoid structure for (Maybe Strategy)-- {{{
unit :: Strategy
unit = Strategy (Nothing, []) Map.empty

plus :: Strategy -> Strategy -> Maybe Strategy
a1 `plus` a2 = do
  interIn <- joinIn (fst $ interchange a1) (fst $ interchange a2)

  interOut <- joinOut (snd $ interchange a1) (snd $ interchange a2) 
  when (isJust interIn && fromJust interIn `elem` interOut) Nothing

  tr <- joinTr (transposes a1) (transposes a2)
  Just $ Strategy (interIn, interOut) tr

-- Helper functions for plus --
joinIn :: (Eq a) => In a -> In a -> Maybe (In a)
joinIn x Nothing = return x
joinIn Nothing y = return y
joinIn (Just a) (Just b) = if a == b then return (Just a) else Nothing

joinOut :: (Ord a) => Out a -> Out a -> Maybe (Out a)
joinOut xs ys = return zs
  where zs = sort . nub $ xs ++ ys

joinTr :: ArrMap -> ArrMap -> Maybe ArrMap
joinTr m1 m2 = foldM help m1 pairs
  where pairs = Map.toList m2
        help m (v,t1) = case Map.lookup v m of
                          Nothing -> return $ Map.insert v t1 m
                          Just t2 -> do t <- joinTransposes t1 t2
                                        return $ Map.insert v t m

joinTransposes :: Tr -> Tr -> Maybe Tr
joinTransposes i1 i2 = if i1 == i2 then Just i1 else Nothing
-- }}}

-- Variance functions-- {{{

-- Is a given variable variant in the index?
isVariantTo :: [Names] -> VName -> Bool
isVariantTo idxs var = all (`variantIn` var) idxs

-- Which indices are variant to a given variable?
variantIndices :: [Names] -> VName -> [Int]
variantIndices idxs var = filter (\i -> variantIn (idxs !! i) var) [0..length idxs - 1]

-- Is a given variable variant in a specifix Index?
variantIn :: Names -> VName -> Bool
variantIn names var = HS.member var names

-- }}}

--- Extra functions-- {{{

-- Order strategies on the number of transpositions made
accessCmp :: Strategy -> Strategy -> Ordering
accessCmp a b = a' `compare` b'
  where [a',b'] = map (Map.size . transposes) [a,b]
  -- }}}
