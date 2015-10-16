{-# LANGUAGE ScopedTypeVariables, DataKinds, FlexibleContexts, ExistentialQuantification, TypeOperators, TypeFamilies #-}
module Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe
       (
         IxFun
       , Indices
       , Shape
       , ShapeChange
       , rank
       , index
       , iota
       , offsetIndex
       , permute
       , reshape
       , stripe
       , unstripe
       , applyInd
       , base
       , rebase
       , codomain
       , linearWithOffset
       , rearrangeWithOffset
       , isDirect
         -- * Utility
       , shapeFromInts
       , shapeFromSubExps
       )
       where

import Control.Applicative

import Data.List (sort)
import Data.Singletons.Prelude
import Data.Type.Monomorphic
import Data.Type.Natural hiding (n1, n2)
import Data.Type.Ordinal
import qualified Data.Vector.Sized as Vec
import Proof.Equational
import Data.Type.Equality hiding (outer)
import qualified Text.PrettyPrint.Mainland as PP

import Prelude

import Futhark.Analysis.ScalExp
import Futhark.Representation.AST.Syntax (SubExp(..), DimChange)
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Util.Truths
import Futhark.Representation.AST.Attributes.Names
import qualified Futhark.Representation.ExplicitMemory.Permutation as Perm
import Futhark.Representation.ExplicitMemory.Permutation
  (Swap (..), Permutation (..))
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as Safe
import qualified Futhark.Representation.ExplicitMemory.SymSet as SymSet
import Language.Futhark.Core
import Futhark.Representation.AST.Pretty (pretty)

data IxFun = forall c n .
             IxFun
             (SNat ('S c))
             (SNat ('S n))
             (Safe.IxFun ScalExp ('S c) ('S n))

instance Show IxFun where
  show (IxFun _ _ fun) = show fun

instance PP.Pretty IxFun where
  ppr (IxFun _ _ fun) = PP.ppr fun

instance Eq IxFun where
  IxFun
    (c1 :: SNat ('S c1))
    (n1 :: SNat ('S n1)) fun1 == IxFun
    (c2 :: SNat ('S c2))
    (n2 :: SNat ('S n2)) fun2 =
    case (testEquality n1 n2, testEquality c1 c2) of
      (Just Refl, Just Refl) -> fun1 == fun2
      _                      -> False

instance Substitute IxFun where
  substituteNames subst (IxFun c n ixfun) =
    IxFun c n $ substituteNames subst ixfun

instance Rename IxFun where
  rename (IxFun c n ixfun) =
    IxFun c n <$> rename ixfun

type Indices     = [ScalExp]
type Shape       = [ScalExp]
type ShapeChange = [DimChange SubExp]

shapeFromInts :: [Int] -> Shape
shapeFromInts = map fromIntegral

shapeFromSubExps :: [SubExp] -> Shape
shapeFromSubExps = map intSubExpToScalExp

rank :: IxFun -> Int
rank (IxFun _ n _) = sNatToInt n

index :: IxFun -> Indices -> ScalExp -> ScalExp
index f is element_size = case f of
  IxFun _ n f'
    | length is == sNatToInt n ->
        Safe.index f' (Vec.unsafeFromList n is) element_size
    | otherwise ->
        error $
        "Index list " ++ pretty is ++
        " incompatible with index function " ++ pretty f'

iota :: Shape -> IxFun
iota shape = case toSing (intToNat $ n-1) of
  SomeSing (sb::SNat n) ->
    IxFun (SS sb) (SS sb) $ Safe.iota $ Vec.unsafeFromList (SS sb) shape
  where n = Prelude.length shape

offsetIndex :: IxFun -> ScalExp -> IxFun
offsetIndex (IxFun c n f) se =
  IxFun c n $ Safe.offsetIndex f se

permute :: IxFun -> [Int] -> IxFun
permute (IxFun (c::SNat ('S c)) (n::SNat ('S n)) f) perm
  | sort perm /= [0..n'-1] =
    error $ "IndexFunction.Unsafe.permute: " ++ show perm ++
    " is an invalid permutation for index function of rank " ++
    show n'
  | otherwise =
    IxFun c n $ Safe.permute f $
    Prelude.foldl (flip (:>>:)) Identity $
    fst $ Prelude.foldl buildPermutation ([], [0..n'-1]) $
    Prelude.zip [0..] perm
    -- This is fairly hacky - a better way would be to find the cycles
    -- in the permutation.
  where buildPermutation (perm', dims) (at, wants) =
          let wants' = dims Prelude.!! wants
              has = dims Prelude.!! at
              sw :: Swap ('S n)
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

reshape :: IxFun -> ShapeChange -> IxFun
reshape (IxFun c _ ixfun) newshape =
  case toSing $ intToNat $ Prelude.length newshape-1 of
    SomeSing (sn::SNat n) ->
      IxFun c (SS sn) $
      Safe.reshape ixfun $ Vec.unsafeFromList (SS sn) $
      map (fmap intSubExpToScalExp) newshape

stripe :: IxFun -> ScalExp -> IxFun
stripe (IxFun c n ixfun) stride =
  IxFun c n $
  Safe.stripe ixfun stride

unstripe :: IxFun -> ScalExp -> IxFun
unstripe (IxFun c n ixfun) stride =
  IxFun c n $
  Safe.unstripe ixfun stride

applyInd :: IxFun -> Indices -> IxFun
applyInd ixfun@(IxFun (scnat::SNat ('S c)) (snnat::SNat ('S n))
                (f::Safe.IxFun ScalExp ('S c) ('S n))) is =
  case promote (Prelude.length is) :: Monomorphic (Sing :: Nat -> *) of
    Monomorphic (mnat::SNat m) ->
      case mnat %:<<= nnat of
        STrue ->
          let k = SS $ nnat %- mnat
              nmnat :: SNat (n :- m)
              nmnat = nnat %- mnat
              is' :: Safe.Indices ScalExp m
              is' = Vec.unsafeFromList mnat is
              proof :: 'S n :=: (m :+: 'S (n :- m))
              proof = sym $
                      trans (succPlusR mnat nmnat)
                      (succCongEq (minusPlusEqR nnat mnat))
              f' :: Safe.IxFun ScalExp ('S c) (m :+: 'S (n :- m))
              f' = coerce proof f
          in IxFun scnat k $ Safe.applyInd k f' is'
        SFalse ->
          error $
          unlines ["IndexFunction.Unsafe.applyInd: Too many indices given.",
                   "  Index function: " ++ pretty ixfun,
                   "  Indices" ++ pretty is]
  where nnat :: SNat n
        nnat = snnat %- sOne

base :: IxFun -> Shape
base (IxFun _ _ ixfun) =
  Vec.toList $ Safe.base ixfun

rebase :: IxFun -> IxFun -> IxFun
rebase (IxFun (sc0nat::SNat ('S c0)) (sn0nat::SNat ('S n0)) new_base)
       (IxFun (sc1nat::SNat ('S c1)) (sn1nat::SNat ('S n1)) f)
  | Just Refl <- sc1nat `testEquality` sn0nat =
      IxFun sc0nat sn1nat $ Safe.rebase new_base f
  | otherwise =
      error $
      unlines ["IndexFunction.Unsafe.rebase: the index function",
               "  " ++ pretty new_base,
               "cannot be used as base for index function",
               "  " ++ pretty f]

codomain :: IxFun -> SymSet
codomain (IxFun _ n f) =
  SymSet n $ Safe.codomain f

isDirect :: IxFun -> Bool
isDirect =
  maybe False (==zeroscal) . flip linearWithOffset onescal
  where zeroscal = Val (IntVal 0)
        onescal = Val (IntVal 1)

linearWithOffset :: IxFun -> ScalExp -> Maybe ScalExp
linearWithOffset (IxFun _ _ ixfun) =
  Safe.linearWithOffset ixfun

rearrangeWithOffset :: IxFun -> Maybe (ScalExp, [Int])
rearrangeWithOffset (IxFun _ n ixfun) = do
  (offset, perm) <- Safe.rearrangeWithOffset ixfun
  return (offset, Vec.toList $ Perm.apply perm $ Vec.unsafeFromList n [0..])

data SymSet = forall n . SymSet (SNat n) (SymSet.SymSet n)

instance FreeIn IxFun where
  freeIn (IxFun _ _ ixfun) = freeIn ixfun
