{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, FlexibleContexts, ExistentialQuantification, TypeOperators, TypeFamilies #-}
module Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe
       (
         IxFun
       , index
       , iota
       , offset
       , permute
       , applyInd
       , codomain
       , isLinear
       , linearWithOffset
       , isDirect
         -- * Utility
       , shapeFromSubExps
       , shapeFromInts
       )
       where

import Control.Applicative

import Data.List (sort)
import Data.Loc
import Data.Maybe (isJust)
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
type Shape   = [ScalExp]

shapeFromSubExps :: [SubExp] -> Shape
shapeFromSubExps = map fromSubExp
  where fromSubExp (Var v)        = Id v
        fromSubExp (Constant v _) = Val v

shapeFromInts :: [Int] -> Shape
shapeFromInts = shapeFromSubExps . map (flip Constant noLoc . IntVal)

index :: IxFun -> Indices -> ScalExp
index f is = case f of
  IxFun n f' -> Safe.index f' $ unsafeFromList n is

iota :: Shape -> IxFun
iota shape = case toSing (n-1) of
  SomeSing (sb::SNat n) -> IxFun (SS sb) (Safe.iota $ unsafeFromList (SS sb) shape)
  where n = intToNat $ Prelude.length shape

offset :: IxFun -> ScalExp -> IxFun
offset (IxFun n f) se =
  IxFun n $ Safe.offset f se

permute :: IxFun -> [Int] -> IxFun
permute (IxFun (n::SNat (S n)) f) perm
  | sort perm /= [0..n'-1] =
    error "IndexFunction.Unsafe.permute: invalid permutation"
  | otherwise =
    IxFun n $ Safe.permute f $
    Prelude.foldr (:>>:) Identity $
    Prelude.foldr buildPermutation [] $
    Prelude.zip [0..] perm
    -- This is fairly hacky - a better way would be to find the cycles
    -- in the permutation.
  where buildPermutation (to,from) perm' =
          let sw :: Swap (S n)
              sw = withSingI n $
                   unsafeFromInt from :<->: unsafeFromInt to
          in if sw `Prelude.notElem` perm' && from /= to
             then sw : perm'
             else perm'
        n' = sNatToInt n

applyInd :: IxFun -> Indices -> IxFun
applyInd ixfun@(IxFun (snnat::SNat (S n)) (f::Safe.IxFun (S n))) is =
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
              proof :: S n :=: (m :+: S (n :- m))
              proof = sym $
                      trans (succPlusR mnat nmnat)
                      (succCongEq (minusPlusEqR nnat mnat))
              f' :: Safe.IxFun (m :+: S (n :- m))
              f' = coerce proof f
              ixfun' :: Safe.IxFun (S (n :- m))
              ixfun' = Safe.applyInd f' is'
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

isLinear :: IxFun -> Bool
isLinear = isJust . linearWithOffset

isDirect :: IxFun -> Bool
isDirect = maybe False (==Val (IntVal 0)) . linearWithOffset

linearWithOffset :: IxFun -> Maybe ScalExp
linearWithOffset (IxFun _ ixfun) = Safe.linearWithOffset ixfun

data SymSet = forall n . SymSet (SNat n) (SymSet.SymSet n)

minusPlusEqR :: (m :<<= n) ~ True =>
                SNat n -> SNat m -> (m :+: (n :-: m)) :=: n
minusPlusEqR n m =
  plusMinusCommutes m n m `trans` plusMinusEqR n m

instance FreeIn IxFun where
  freeIn (IxFun _ ixfun) = freeIn ixfun
