module Futhark.Analysis.Proofs.Traversals where

import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Cases (..), Domain (..), IndexFn (..), Iterator (..), cases, casesToList)
import Futhark.Analysis.Proofs.Symbol
import Futhark.SoP.SoP (SoP, int2SoP, sopToLists, sym2SoP, (.*.), (.+.))
import Control.Monad (foldM)

data ASTMapper a m = ASTMapper
  { mapOnSymbol :: a -> m a,
    mapOnSoP :: SoP a -> m (SoP a)
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
  astMap m (LinComb vn lb ub x) =
    mapOnSymbol m =<< LinComb vn <$> astMap m lb <*> astMap m ub <*> astMap m x
  astMap m (Idx xs i) = mapOnSymbol m =<< Idx <$> astMap m xs <*> astMap m i
  astMap m (Indicator p) = mapOnSymbol m . Indicator =<< astMap m p
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

instance ASTMappable Algebra.Symbol Algebra.Symbol where
  astMap m (Algebra.Var x) = mapOnSymbol m $ Algebra.Var x
  astMap m (Algebra.Idx vn i) = mapOnSymbol m . Algebra.Idx vn =<< astMap m i
  astMap m (Algebra.Mdf dir vn i j) =
    mapOnSymbol m =<< Algebra.Mdf dir vn <$> astMap m i <*> astMap m j
  astMap m (Algebra.Sum vn lb ub) =
    mapOnSymbol m =<< Algebra.Sum vn <$> astMap m lb <*> astMap m ub
  astMap m (Algebra.Pow (c, x)) =
    mapOnSymbol m . curry Algebra.Pow c =<< astMap m x

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
  astFold m acc sym@(LinComb _ lb ub x) = do
    foldOnSymbol m acc sym >>= astFoldF m lb >>= astFoldF m ub >>= astFoldF m x
  astFold m acc sym@(Idx xs i) = do
    foldOnSymbol m acc sym
    >>= astFoldF m xs
    >>= astFoldF m i
  astFold m acc sym@(Indicator p) = do
    foldOnSymbol m acc sym >>= astFoldF m p
  astFold m acc sym@(Apply f xs) = do
    foldOnSymbol m acc sym
    >>= astFoldF m f
    >>= \a -> foldM (astFold m) a xs
  astFold m acc sym@(Not x) = do
    foldOnSymbol m acc sym
    >>= astFoldF m x
  astFold m acc sym@(x :== y) = do
    foldOnSymbol m acc sym
    >>= astFoldF m x
    >>= astFoldF m y
  astFold m acc sym@(x :< y) = do
    acc <- foldOnSymbol m acc sym
    acc <- astFold m acc x
    astFold m acc y
  astFold m acc sym@(x :> y) = do
    acc <- foldOnSymbol m acc sym
    acc <- astFold m acc x
    astFold m acc y
  astFold m acc sym@(x :>= y) = do
    acc <- foldOnSymbol m acc sym
    acc <- astFold m acc x
    astFold m acc y
  astFold m acc sym@(x :<= y) = do
    acc <- foldOnSymbol m acc sym
    acc <- astFold m acc x
    astFold m acc y
  astFold m acc sym@(x :&& y) = do
    acc <- foldOnSymbol m acc sym
    acc <- astFold m acc x
    astFold m acc y
  astFold m acc sym@(x :|| y) = do
    acc <- foldOnSymbol m acc sym
    acc <- astFold m acc x
    astFold m acc y
  -- Recurr, Var, Hole
  astFold m acc sym = foldOnSymbol m acc sym
