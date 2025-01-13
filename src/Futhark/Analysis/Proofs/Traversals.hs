module Futhark.Analysis.Proofs.Traversals
  ( ASTMapper (..),
    ASTMappable (..),
    identityMapper,
    ASTFolder (..),
    ASTFoldable (..),
  )
where

import Control.Monad (foldM)
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Cases (..), Domain (..), IndexFn (..), Iterator (..), cases, casesToList)
import Futhark.Analysis.Proofs.Symbol
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
  { foldOnSymbol :: b -> a -> m b }

class ASTFoldable a c where
  astFold :: (Monad m) => ASTFolder a b m -> b -> c -> m b
  astFoldF :: (Monad m) => ASTFolder a b m -> c -> b -> m b
  astFoldF m = flip (astFold m)

instance ASTFoldable Symbol (SoP Symbol) where
  astFold m acc =
    foldM (astFold m) acc . concatMap fst . sopToLists

instance ASTFoldable Symbol Symbol where
  astFold m acc (Sum _ lb ub x) =
    astFold m acc lb >>= astFoldF m ub >>= astFoldF m x
  astFold m acc (Idx xs i) =
    astFold m acc xs >>= astFoldF m i
  astFold m acc (Apply f xs) =
    astFold m acc f >>= \a -> foldM (astFold m) a xs
  astFold m acc (Not x) =
    astFold m acc x
  astFold m acc (x :== y) =
    astFold m acc x >>= astFoldF m y
  astFold m acc (x :< y) =
    astFold m acc x >>= astFoldF m y
  astFold m acc (x :> y) =
    astFold m acc x >>= astFoldF m y
  astFold m acc (x :>= y) =
    astFold m acc x >>= astFoldF m y
  astFold m acc (x :<= y) =
    astFold m acc x >>= astFoldF m y
  astFold m acc (x :&& y) =
    astFold m acc x >>= astFoldF m y
  astFold m acc (x :|| y) =
    astFold m acc x >>= astFoldF m y
  -- Recurr, Var, Hole
  astFold m acc sym = foldOnSymbol m acc sym
