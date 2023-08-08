-- | Perform index function operations in both algebraic and LMAD
-- representations.
module Futhark.IR.Mem.IxFunWrapper
  ( IxFun,
    iota,
    permute,
    reshape,
    coerce,
    slice,
    flatSlice,
    expand,
  )
where

import Control.Monad (join)
import Futhark.IR.Mem.IxFun qualified as I
import Futhark.IR.Mem.IxFun.Alg qualified as IA
import Futhark.IR.Syntax (FlatSlice, Slice)
import Futhark.Util.IntegralExp

type Shape num = [num]

type Permutation = [Int]

type IxFun num = (Maybe (I.IxFun num), IA.IxFun num)

iota ::
  IntegralExp num =>
  Shape num ->
  IxFun num
iota x = (Just $ I.iota x, IA.iota x)

permute ::
  IntegralExp num =>
  IxFun num ->
  Permutation ->
  IxFun num
permute (l, a) x = (I.permute <$> l <*> pure x, IA.permute a x)

reshape ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  IxFun num
reshape (l, a) x = (join (I.reshape <$> l <*> pure x), IA.reshape a x)

coerce ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  IxFun num
coerce (l, a) x = (I.coerce <$> l <*> pure x, IA.coerce a x)

slice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Slice num ->
  IxFun num
slice (l, a) x = (I.slice <$> l <*> pure x, IA.slice a x)

flatSlice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  FlatSlice num ->
  IxFun num
flatSlice (l, a) x = (I.flatSlice <$> l <*> pure x, IA.flatSlice a x)

expand ::
  (Eq num, IntegralExp num) =>
  num ->
  num ->
  IxFun num ->
  IxFun num
expand o p (lf, af) = (I.expand o p =<< lf, IA.expand o p af)
