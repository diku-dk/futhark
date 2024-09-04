module Futhark.Analysis.Proofs.Traversals
where

import Futhark.Analysis.Proofs.Symbol
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Futhark.SoP.SoP (SoP, (.+.), int2SoP, sopToLists, (.*.), sym2SoP)

data ASTMapper m = ASTMapper
  { mapOnSymbol :: Symbol -> m Symbol,
    mapOnSoP :: SoP Symbol -> m (SoP Symbol)
  }

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

instance ASTMappable (SoP Symbol) where
  astMap m sop = do
    mapOnSoP m . foldl (.+.) (int2SoP 0) =<< mapM g (sopToLists sop)
    where
      g (ts, c) = do
        ts' <- mapM (mapOnSymbol m) ts
        pure $ foldl (.*.) (int2SoP 1) (int2SoP c : map sym2SoP ts')

instance ASTMappable Symbol where
  astMap _ Recurrence = pure Recurrence
  astMap m (Var x) = mapOnSymbol m $ Var x
  astMap m (LinComb vn lb ub x) =
    mapOnSymbol m =<< LinComb vn <$> astMap m lb <*> astMap m ub <*> astMap m x
  astMap m (Idx xs i) = mapOnSymbol m =<< Idx <$> astMap m xs <*> astMap m i
  astMap m (Indicator p) = mapOnSymbol m . Indicator =<< astMap m p
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = mapOnSymbol m . Not =<< astMap m x
  astMap m (x :== y) = mapOnSymbol m =<< (:==) <$> astMap m x <*> astMap m y
  astMap m (x :< y) = mapOnSymbol m =<< (:<) <$> astMap m x <*> astMap m y
  astMap m (x :> y) = mapOnSymbol m =<< (:>) <$> astMap m x <*> astMap m y
  astMap m (x :/= y) = mapOnSymbol m =<< (:/=) <$> astMap m x <*> astMap m y
  astMap m (x :>= y) = mapOnSymbol m =<< (:>=) <$> astMap m x <*> astMap m y
  astMap m (x :<= y) = mapOnSymbol m =<< (:<=) <$> astMap m x <*> astMap m y
  astMap m (x :&& y) = mapOnSymbol m =<< (:&&) <$> astMap m x <*> astMap m y
  astMap m (x :|| y) = mapOnSymbol m =<< (:||) <$> astMap m x <*> astMap m y

normalize :: Symbol -> IndexFnM Symbol
normalize = astMap m . toNNF
  where
    toNNF :: Symbol -> Symbol
    toNNF (Not (Not x)) = x
    toNNF (Not (Bool True)) = Bool False
    toNNF (Not (Bool False)) = Bool True
    toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
    toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
    toNNF (Not (x :== y)) = x :/= y
    toNNF (Not (x :< y)) = x :>= y
    toNNF (Not (x :> y)) = x :<= y
    toNNF (Not (x :/= y)) = x :== y
    toNNF (Not (x :>= y)) = x :< y
    toNNF (Not (x :<= y)) = x :> y
    toNNF x = x

    m = ASTMapper { mapOnSymbol = norm, mapOnSoP = pure }
    norm symbol = case toNNF symbol of
        (Not x) -> do
          pure $ toNNF (Not x)
        (x :&& y) -> do
          case (x, y) of
            (Bool True, b) -> pure b                         -- Identity.
            (a, Bool True) -> pure a
            (Bool False, _) -> pure (Bool False)             -- Annihilation.
            (_, Bool False) -> pure (Bool False)
            (a, b) | a == b -> pure a                        -- Idempotence.
            (a, b) | a == toNNF (Not b) -> pure (Bool False) -- A contradiction.
            (a, b) -> pure $ a :&& b
        (x :|| y) -> do
          case (x, y) of
            (Bool False, b) -> pure b                       -- Identity.
            (a, Bool False) -> pure a
            (Bool True, _) -> pure (Bool True)              -- Annihilation.
            (_, Bool True) -> pure (Bool True)
            (a, b) | a == b -> pure a                       -- Idempotence.
            (a, b) | a == toNNF (Not b) -> pure (Bool True) -- A tautology.
            (a, b) -> pure $ a :|| b
        v -> pure v
