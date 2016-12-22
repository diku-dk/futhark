module Futhark.Optimise.CoalesceKernels.Strategy where

import Control.Monad
import qualified Data.Map.Lazy as Map
import Data.List (nub, sort, minimumBy)
import Data.Maybe

import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST.Syntax
import Futhark.Representation.ExplicitMemory

-- Types
type Var = VName

type In a = Maybe a
type Out a = [a]
type Tr = Int
type ArrMap = Map.Map Var Tr

data Access = Access VName (Slice SubExp)

data Strategy = Strategy { interchange :: (In Var, Out Var)
                         , transposes :: ArrMap
                         } deriving (Show, Eq)

type VarianceTable = HM.HashMap VName Names

-- Scope is mapping from var to which vars it is variant to
type StratM = Reader VarianceTable

-- The real meat. 
chooseStrategy :: VarianceTable -> [VName] -> [Stm InKernel] -> Strategy
chooseStrategy vtable lvs stms = case makeStrategy (length lvs) initStrats of
                                  [] -> unit -- Nil transformation
                                  ss -> minimumBy accessCmp ss
  where initStrats = runReader (mapM (generate lvs) as) vtable
        as = map makeAccess stms

        makeAccess :: Stm InKernel -> Access
        makeAccess (Let _ () (BasicOp (Index _ var idxs))) = Access var idxs
        makeAccess _ = error "CoalesceKernels.Strategy: non-Index in stms."

-- Fold over all array accesses generated
makeStrategy :: Int -> [[Strategy]] -> [Strategy]
makeStrategy allout = filter notAllOut . foldr combine [unit]
  where combine xs ys = catMaybes [ plus x y | x <- xs, y <- ys ]
        notAllOut (Strategy (_,o) _) = length o < allout 

-- Generate all the possible startegy choices for a single array lookup
-- [LoopVars] ArrStrategy -> [Initial As]
generate :: [Var] -> Access -> StratM [Strategy]
generate lvs a@(Access _ is) = do
  variants <- filterM (isVariantTo is) lvs                  -- Find all lvs variant in the Index
  ins <- mapM (pushIn a) variants                           -- Coalesced access
  let out = Just $ Strategy (Nothing, variants) Map.empty   -- Invariant access
  return $ catMaybes (out : ins)                            -- Just put them together
  where 
    pushIn :: Access -> VName -> StratM (Maybe Strategy)
    pushIn (Access arr idxs) lv = do
      vis <- variantIndices idxs lv
      case vis of
        []  -> return . Just $ Strategy (Just lv, []) Map.empty
                -- Strategy is invariant to lv
        [x] -> return . Just $ Strategy (Just lv, []) (Map.singleton arr x)
                -- Strategy is variant in one index.
        _   -> return Nothing
                -- Strategy is variant in more indexes to lv. No use in pushing in.


-- Monoid structure for (Maybe Strategy)
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

----

-- Variance functions

-- Is a given variable variant in the index?
isVariantTo :: Slice SubExp -> VName -> StratM Bool
isVariantTo idxs var = and <$> mapM (`variantIn` var) idxs

-- Which indices are variant to a given variable?
variantIndices :: Slice SubExp -> VName -> StratM [Int]
variantIndices idxs var = filterM (\i -> variantIn (idxs !! i) var) [0..length idxs - 1]

-- Is a given variable variant in a specifix DimIndex?
variantIn :: DimIndex SubExp -> VName -> StratM Bool
variantIn (DimFix e)        var = varInE e var
variantIn (DimSlice e1 e2)  var = (&&) <$> varInE e1 var <*> varInE e2 var --How does this work?!

-- Helper for variance functions
varInE :: SubExp -> VName -> StratM Bool
varInE (Constant _) _   = return False
varInE (Var      v) var = do
  vtable <- ask
  case HM.lookup var vtable of
    Nothing -> return False
    Just vs -> return $ v `elem` vs


--- Extra functions

-- Order strategies on the number of transpositions made
accessCmp :: Strategy -> Strategy -> Ordering
accessCmp a b = a' `compare` b'
  where [a',b'] = map (Map.size . transposes) [a,b]
