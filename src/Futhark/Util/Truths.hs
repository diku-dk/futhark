{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeOperators, GADTs #-}
-- | Truths about the universe that may come in handy while doing
-- compiler hacking.
module Futhark.Util.Truths
       (
         plusMinusCommutes
       , plusMinusSwapR
       , plusMinusSwapL
       )
where

import Data.Constraint (Dict(..))
import Data.Type.Natural
import Proof.Equational

-- | @(x + (y - z)) = ((x + y) - z)@.
plusMinusCommutes :: forall x y z.(z :<<= y) ~ True =>
                     SNat x -> SNat y -> SNat z
                  -> (x :+: (y :-: z)) :=: ((x :+: y) :-: z)
plusMinusCommutes _ _ SZ = Refl `trans` Refl
plusMinusCommutes SZ _ _ = Refl `trans` Refl
plusMinusCommutes _ SZ z = case boolToPropLeq z SZ of
  ZeroLeq _ -> Refl
plusMinusCommutes (SS (x' :: SNat x')) y z =
  case propToBoolLeq prop of
    Dict ->
      let p1 :: (S x' :+: (y :-: z)) :=: S (x' :+: (y :-: z))
          p1 = succPlusL x' (y %- z)
          p2 :: S (x' :+: (y :-: z)) :=: S ((x' :+: y) :-: z)
          p2 = eqPreservesS $ plusMinusCommutes x' y z
          p3 :: S ((x' :+: y) :-: z) :=: (S (x' :+: y) :-: z)
          p3 = sym $ eqSuccMinus (x' %+ y) z
          p4 :: (S (x' :+: y) :-: z) :=: ((S x' :+: y) :-: z)
          p4 = minusCongEq (sym $ succPlusL x' y) z
      in p1 `trans` p2 `trans` p3 `trans` p4
  where prop :: Leq z (x' :+: y)
        prop = plusMonotone (ZeroLeq x') (boolToPropLeq z y)

-- | @((n + m) - k) = (n + (m - k))@.
plusMinusSwapR :: (k :<<= m) ~ True =>
                  SNat n -> SNat m -> SNat k
               -> ((n :+: m) :-: k) :=: (n :+: (m :-: k))
plusMinusSwapR n m k = sym $ plusMinusCommutes n m k

-- | @((n + m) - k) = ((n - k) + m)@.
plusMinusSwapL :: forall n m k.
                  (k :<<= n) ~ True =>
                  SNat n -> SNat m -> SNat k
               -> ((n :+: m) :-: k) :=: ((n :-: k) :+: m)
plusMinusSwapL SZ _ k = case boolToPropLeq k SZ of
  ZeroLeq _ -> Refl
plusMinusSwapL _ _ SZ = Refl
plusMinusSwapL n SZ k = p1 `trans` p2
  where p1 :: ((n :+: m) :-: k) :=: (n :-: k)
        p1 = minusCongEq (plusZR n) k
        p2 :: (n :-: k) :=: ((n :-: k) :+: m)
        p2 = sym $ plusZR (n %- k)
plusMinusSwapL n (SS (m' :: SNat m')) k =
  case propToBoolLeq prop of
    Dict ->
      let p0 :: ((n :+: m) :-: k) :=: (S (n :+: m') :-: k)
          p0 = minusCongEq (succPlusR n m') k
          p1 :: (S (n :+: m') :-: k) :=: S ((n :+: m') :-: k)
          p1 = eqSuccMinus (n %+ m') k
          p2 :: S ((n :+: m') :-: k) :=: S ((n :-: k) :+: m')
          p2 = eqPreservesS $ plusMinusSwapL n m' k
          p3 :: S ((n :-: k) :+: m') :=: ((n :-: k) :+: m)
          p3 = sym $ succPlusR (n %- k) m'
      in p0 `trans` p1 `trans` p2 `trans` p3
  where prop :: Leq k (n :+: m')
        prop = plusLeqL k sZero `leqTrans`
               plusMonotone (boolToPropLeq k n) (ZeroLeq m')
