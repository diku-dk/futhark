{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Futhark.Analysis.Properties.Traversals
  ( ASTMapper (..),
    ASTMappable (..),
    identityMapper,
    ASTFolder (..),
    ASTFoldable (..),
  )
where

import Control.Monad (foldM)
import Futhark.Analysis.Properties.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Properties.IndexFn (Cases (..), Domain (..), IndexFn (..), Iterator (..), cases, casesToList)
import Futhark.Analysis.Properties.Property (Predicate (..), Property (..))
import Futhark.Analysis.Properties.Symbol
import Futhark.SoP.SoP (SoP, int2SoP, sopToLists, sym2SoP, (.*.), (.+.))

data ASTMapper a m = ASTMapper
  { mapOnSymbol :: a -> m a,
    mapOnSoP :: SoP a -> m (SoP a)
  }

identityMapper :: (Monad m) => ASTMapper a m
identityMapper =
  ASTMapper
    { mapOnSymbol = pure,
      mapOnSoP = pure
    }

class ASTMappable a b where
  astMap :: (Monad m) => ASTMapper a m -> b -> m b

instance (Ord a, ASTMappable a a) => ASTMappable a (SoP a) where
  astMap m sop = do
    mapOnSoP m . foldl (.+.) (int2SoP 0) =<< mapM g (sopToLists sop)
    where
      g (ts, c) = do
        ts' <- mapM (astMap m) ts
        pure $ foldl (.*.) (int2SoP 1) (int2SoP c : map sym2SoP ts')

instance (Ord a, ASTMappable a a) => ASTMappable a (Predicate a) where
  astMap m (Predicate vn e) = Predicate vn <$> astMap m e

instance (Ord a, ASTMappable a a) => ASTMappable a (Property a) where
  astMap _ Boolean = pure Boolean
  astMap _ (Disjoint vns) = pure (Disjoint vns)
  astMap _ (Monotonic x dir) = pure (Monotonic x dir)
  astMap m (Rng x (a, b)) = curry (Rng x) <$> astMap m a <*> astMap m b
  astMap m (Injective x (Just (a, b))) = curry (Injective x . Just) <$> astMap m a <*> astMap m b
  astMap _ (Injective x Nothing) = pure (Injective x Nothing)
  astMap m (BijectiveRCD x (a, b) (c, d)) = do
    rcd <- (,) <$> astMap m a <*> astMap m b
    img <- (,) <$> astMap m c <*> astMap m d
    pure $ BijectiveRCD x rcd img
  astMap m (FiltPartInv x pf pps) = do
    pf' <- astMap m pf
    pps' <- mapM (astMap m) pps
    pure $ FiltPartInv x pf' pps'
  astMap m (FiltPart x y pf pps) = do
    pf' <- astMap m pf
    pps' <- mapM (astMap m) pps
    pure $ FiltPart x y pf' pps'

instance ASTMappable Symbol Symbol where
  astMap _ Recurrence = pure Recurrence
  astMap m (Var x) = mapOnSymbol m $ Var x
  astMap m (Hole x) = mapOnSymbol m $ Hole x
  astMap m (Sum vn lb ub x) =
    mapOnSymbol m =<< Sum vn <$> astMap m lb <*> astMap m ub <*> astMap m x
  astMap m (Idx xs i) = mapOnSymbol m =<< Idx <$> astMap m xs <*> astMap m i
  astMap m (Apply f xs) =
    mapOnSymbol m =<< Apply <$> astMap m f <*> mapM (astMap m) xs
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = mapOnSymbol m . neg =<< astMap m x
  astMap m (x :== y) = mapOnSymbol m =<< (:==) <$> astMap m x <*> astMap m y
  astMap m (x :< y) = mapOnSymbol m =<< (:<) <$> astMap m x <*> astMap m y
  astMap m (x :> y) = mapOnSymbol m =<< (:>) <$> astMap m x <*> astMap m y
  astMap m (x :/= y) = mapOnSymbol m =<< (:/=) <$> astMap m x <*> astMap m y
  astMap m (x :>= y) = mapOnSymbol m =<< (:>=) <$> astMap m x <*> astMap m y
  astMap m (x :<= y) = mapOnSymbol m =<< (:<=) <$> astMap m x <*> astMap m y
  astMap m (x :&& y) = mapOnSymbol m =<< (:&&) <$> astMap m x <*> astMap m y
  astMap m (x :|| y) = mapOnSymbol m =<< (:||) <$> astMap m x <*> astMap m y
  astMap m (Prop p) = Prop <$> astMap m p

instance ASTMappable Symbol IndexFn where
  astMap m (IndexFn dom body) = IndexFn <$> astMap m dom <*> astMap m body

instance ASTMappable Symbol Iterator where
  astMap m (Forall i dom) = Forall i <$> astMap m dom
  astMap _ Empty = pure Empty

instance ASTMappable Symbol Domain where
  astMap m (Iota n) = Iota <$> astMap m n
  astMap m (Cat k n b) = Cat k <$> astMap m n <*> astMap m b

instance ASTMappable Symbol (Cases Symbol (SoP Symbol)) where
  astMap m cs = do
    let (ps, qs) = unzip $ casesToList cs
    ps' <- mapM (astMap m) ps
    qs' <- mapM (astMap m) qs
    pure . cases $ zip ps' qs'

instance ASTMappable Algebra.Symbol Algebra.Symbol where
  astMap m (Algebra.Var x) = mapOnSymbol m $ Algebra.Var x
  astMap m (Algebra.Idx vn i) = mapOnSymbol m . Algebra.Idx vn =<< astMap m i
  astMap m (Algebra.Mdf dir vn i j) =
    mapOnSymbol m =<< Algebra.Mdf dir vn <$> astMap m i <*> astMap m j
  astMap m (Algebra.Sum vn lb ub) =
    mapOnSymbol m =<< Algebra.Sum vn <$> astMap m lb <*> astMap m ub
  astMap m (Algebra.Pow (c, x)) =
    mapOnSymbol m . curry Algebra.Pow c =<< astMap m x

newtype ASTFolder a b m = ASTFolder
  {foldOnSymbol :: b -> a -> m b}

class ASTFoldable a c where
  astFold :: (Monad m) => ASTFolder a b m -> b -> c -> m b
  astFoldF :: (Monad m) => ASTFolder a b m -> c -> b -> m b
  astFoldF m = flip (astFold m)

instance ASTFoldable Symbol (SoP Symbol) where
  astFold m acc =
    foldM (astFold m) acc . concatMap fst . sopToLists

instance ASTFoldable Symbol (Predicate Symbol) where
  astFold m acc (Predicate _ e) = astFold m acc e

instance ASTFoldable Symbol (Property Symbol) where
  -- astFold _m _acc = undefined
  astFold _ acc Boolean = pure acc
  astFold _ acc Disjoint {} = pure acc
  astFold _ acc Monotonic {} = pure acc
  astFold m acc (Rng _ (a, b)) = astFold m acc a >>= astFoldF m b
  astFold m acc (Injective _ (Just (a, b))) = astFold m acc a >>= astFoldF m b
  astFold _ acc (Injective _ Nothing) = pure acc
  astFold m acc (BijectiveRCD _ (a, b) (c, d)) =
    astFold m acc a >>= astFoldF m b >>= astFoldF m c >>= astFoldF m d
  astFold m acc (FiltPartInv _ pf pps) = do
    acc' <- astFold m acc pf
    foldM (astFold m) acc' pps
  astFold m acc (FiltPart _ _ pf pps) = do
    acc' <- astFold m acc pf
    foldM (astFold m) acc' pps

instance ASTFoldable Symbol Symbol where
  astFold m acc e@(Sum _ lb ub x) =
    astFold m acc lb >>= astFoldF m ub >>= astFoldF m x >>= flip (foldOnSymbol m) e
  astFold m acc e@(Idx xs i) =
    astFold m acc xs >>= astFoldF m i >>= flip (foldOnSymbol m) e
  astFold m acc e@(Apply f xs) =
    astFold m acc f >>= \a -> foldM (astFold m) a xs >>= flip (foldOnSymbol m) e
  astFold m acc e@(Not x) =
    astFold m acc x >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :== y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :/= y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :< y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :> y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :>= y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :<= y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :&& y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc e@(x :|| y) =
    astFold m acc x >>= astFoldF m y >>= flip (foldOnSymbol m) e
  astFold m acc Recurrence = foldOnSymbol m acc Recurrence
  astFold m acc (Var x) = foldOnSymbol m acc (Var x)
  astFold m acc (Hole x) = foldOnSymbol m acc (Hole x)
  astFold m acc (Bool x) = foldOnSymbol m acc (Bool x)
  astFold m acc (Prop p) = astFold m acc p

instance ASTFoldable Symbol IndexFn where
  astFold m acc (IndexFn dom body) = astFold m acc dom >>= astFoldF m body

instance ASTFoldable Symbol Iterator where
  astFold m acc (Forall _ dom) = astFold m acc dom
  astFold _ acc Empty = pure acc

instance ASTFoldable Symbol Domain where
  astFold m acc (Iota n) = astFold m acc n
  astFold m acc (Cat _ n b) = astFold m acc n >>= astFoldF m b

instance ASTFoldable Symbol (Cases Symbol (SoP Symbol)) where
  astFold m acc cs = do
    let (ps, qs) = unzip $ casesToList cs
    acc' <- foldM (astFold m) acc ps
    foldM (astFold m) acc' qs
