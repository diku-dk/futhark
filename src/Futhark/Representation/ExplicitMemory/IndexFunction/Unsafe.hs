{-# LANGUAGE ScopedTypeVariables, DataKinds, FlexibleContexts, ExistentialQuantification, TypeOperators, TypeFamilies #-}
module Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe
       (
         IxFun
       , Indices
       , Shape
       , rank
       , index
       , iota
       , offset
       , permute
       , reshape
       , applyInd
       , codomain
       , linearWithOffset
       , isDirect
         -- * Utility
       , shapeFromInts
       )
       where

import Control.Applicative

import Data.List (sort)
import Data.Singletons.Prelude
import Data.Type.Monomorphic
import Data.Type.Natural hiding (n1, n2)
import Data.Type.Ordinal
import Data.Vector.Sized hiding (index, map, unsafeFromInt)
import Proof.Equational
import Data.Type.Equality hiding (outer)
import qualified Text.PrettyPrint.Mainland as PP

import Futhark.Analysis.ScalExp
import Futhark.Representation.AST.Syntax (SubExp(..))
import Futhark.Substitute
import Futhark.Renamer
import Futhark.Util.Truths
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.ExplicitMemory.Permutation
  (Swap (..), Permutation (..))
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as Safe
import qualified Futhark.Representation.ExplicitMemory.SymSet as SymSet
import Language.Futhark.Core
import Futhark.Representation.AST.Pretty (pretty)

data IxFun = forall n . IxFun (SNat (S n)) (Safe.IxFun (S n))

instance Show IxFun where
  show (IxFun _ fun) = show fun

instance PP.Pretty IxFun where
  ppr (IxFun _ fun) = PP.ppr fun

instance Eq IxFun where
  IxFun (n1 :: SNat (S n1)) fun1 == IxFun (n2 :: SNat (S n2)) fun2 =
    case testEquality n1 n2 of
      Nothing   -> False
      Just Refl -> fun1 == fun2

instance Substitute IxFun where
  substituteNames subst (IxFun n ixfun) =
    IxFun n $ substituteNames subst ixfun

instance Rename IxFun where
  rename (IxFun n ixfun) =
    IxFun n <$> rename ixfun

type Indices = [ScalExp]
type Shape   = [SubExp]

shapeFromInts :: [Int] -> Shape
shapeFromInts = map (Constant . IntVal)

rank :: IxFun -> Int
rank (IxFun n _) = sNatToInt n

index :: IxFun -> Shape -> Indices -> ScalExp
index f dims is = case f of
  IxFun n f' -> Safe.index f' (unsafeFromList n dims) (unsafeFromList n is)

iota :: Int -> IxFun
iota n = case toSing (intToNat $ n-1) of
  SomeSing (sb::SNat n) -> IxFun (SS sb) $ Safe.iota $ SS sb

offset :: IxFun -> ScalExp -> IxFun
offset (IxFun n f) se =
  IxFun n $ Safe.offset f se

permute :: IxFun -> [Int] -> IxFun
permute (IxFun (n::SNat (S n)) f) perm
  | sort perm /= [0..n'-1] =
    error $ "IndexFunction.Unsafe.permute: " ++ show perm ++
    " is an invalid permutation for index function of rank " ++
    show n'
  | otherwise =
    IxFun n $ Safe.permute f $
    Prelude.foldl (flip (:>>:)) Identity $
    fst $ Prelude.foldl buildPermutation ([], [0..n'-1]) $
    Prelude.zip [0..] perm
    -- This is fairly hacky - a better way would be to find the cycles
    -- in the permutation.
  where buildPermutation (perm', dims) (at, wants) =
          let wants' = dims Prelude.!! wants
              has = dims Prelude.!! at
              sw :: Swap (S n)
              sw = withSingI n $
                   unsafeFromInt at :<->: unsafeFromInt wants'
          in if has /= wants
             then (sw : perm', update wants' has (update at wants' dims))
             else (perm', dims)
        n' = sNatToInt n

update :: Int -> a -> [a] -> [a]
update i x l =
  let (bef,_:aft) = Prelude.splitAt i l
  in bef ++ x : aft

reshape :: IxFun -> Int -> Shape -> IxFun
reshape (IxFun (m::SNat (S m)) ixfun) n oldshape =
  case toSing $ intToNat $ n-1 of
    SomeSing (sn::SNat n) ->
      IxFun (SS sn) $
      Safe.reshape (SS sn) ixfun $
      unsafeFromList m oldshape

applyInd :: IxFun -> Shape -> Indices -> IxFun
applyInd ixfun@(IxFun (snnat::SNat (S n)) (f::Safe.IxFun (S n))) dims is =
  case promote (Prelude.length is) :: Monomorphic (Sing :: Nat -> *) of
    Monomorphic (mnat::SNat m) ->
      case mnat %:<<= nnat of
        STrue ->
          let k :: SNat (S (n :- m))
              k = SS $ nnat %- mnat
              nmnat :: SNat (n :- m)
              nmnat = nnat %- mnat
              is' :: Safe.Indices m
              is' = unsafeFromList mnat is
              dims' :: Safe.Shape m
              dims' = unsafeFromList mnat dims
              proof :: S n :=: (m :+: S (n :- m))
              proof = sym $
                      trans (succPlusR mnat nmnat)
                      (succCongEq (minusPlusEqR nnat mnat))
              f' :: Safe.IxFun (m :+: S (n :- m))
              f' = coerce proof f
              ixfun' :: Safe.IxFun (S (n :- m))
              ixfun' = Safe.applyInd k f' dims' is'
          in IxFun k ixfun'
        SFalse ->
          error $
          unlines ["IndexFunction.Unsafe.applyInd: Too many indices given.",
                   "  Index function: " ++ pretty ixfun,
                   "  Indices" ++ pretty is]
  where nnat :: SNat n
        nnat = snnat %- sOne

codomain :: IxFun -> SymSet
codomain (IxFun n f) =
  SymSet n $ Safe.codomain f

isDirect :: IxFun -> Bool
isDirect = maybe False (==Val (IntVal 0)) . linearWithOffset

linearWithOffset :: IxFun -> Maybe ScalExp
linearWithOffset (IxFun _ ixfun) =
  Safe.linearWithOffset ixfun

data SymSet = forall n . SymSet (SNat n) (SymSet.SymSet n)

instance FreeIn IxFun where
  freeIn (IxFun _ ixfun) = freeIn ixfun
