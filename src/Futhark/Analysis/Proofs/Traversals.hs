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
    mapOnSymbol m =<< LinComb vn <$> astMap m lb <*> astMap m ub <*> mapOnSymbol m x
  astMap m (Idx xs i) = mapOnSymbol m =<< Idx <$> mapOnSymbol m xs <*> astMap m i
  astMap m (Indicator p) = mapOnSymbol m . Indicator =<< mapOnSymbol m p
  astMap _ x@(Bool {}) = pure x
  astMap m (Not x) = mapOnSymbol m . Not =<< mapOnSymbol m x
  astMap m (x :== y) = mapOnSymbol m =<< (:==) <$> astMap m x <*> astMap m y
  astMap m (x :< y) = mapOnSymbol m =<< (:<) <$> astMap m x <*> astMap m y
  astMap m (x :> y) = mapOnSymbol m =<< (:>) <$> astMap m x <*> astMap m y
  astMap m (x :/= y) = mapOnSymbol m =<< (:/=) <$> astMap m x <*> astMap m y
  astMap m (x :>= y) = mapOnSymbol m =<< (:>=) <$> astMap m x <*> astMap m y
  astMap m (x :<= y) = mapOnSymbol m =<< (:<=) <$> astMap m x <*> astMap m y
  astMap m (x :&& y) = mapOnSymbol m =<< (:&&) <$> mapOnSymbol m x <*> mapOnSymbol m y
  astMap m (x :|| y) = mapOnSymbol m =<< (:||) <$> mapOnSymbol m x <*> mapOnSymbol m y

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
    -- TODO ^ astMap also on SoP?
    norm symbol = case toNNF symbol of
        (Not x) -> do
          -- x' <- normalize x
          pure $ toNNF (Not x)
        (x :&& y) -> do
          -- x' <- normalize x
          -- y' <- normalize y
          case (x, y) of
            (Bool True, b) -> pure b                         -- Identity.
            (a, Bool True) -> pure a
            (Bool False, _) -> pure (Bool False)             -- Annihilation.
            (_, Bool False) -> pure (Bool False)
            (a, b) | a == b -> pure a                        -- Idempotence.
            (a, b) | a == toNNF (Not b) -> pure (Bool False) -- A contradiction.
            (a, b) -> pure $ a :&& b
        (x :|| y) -> do
          -- x' <- normalize x
          -- y' <- normalize y
          case (x, y) of
            (Bool False, b) -> pure b                       -- Identity.
            (a, Bool False) -> pure a
            (Bool True, _) -> pure (Bool True)              -- Annihilation.
            (_, Bool True) -> pure (Bool True)
            (a, b) | a == b -> pure a                       -- Idempotence.
            (a, b) | a == toNNF (Not b) -> pure (Bool True) -- A tautology.
            (a, b) -> pure $ a :|| b
        v -> pure v

-- normalize :: Symbol -> IndexFnM Symbol
-- normalize = f
--   where
--     m = ASTMapper { mapOnSymbol = f }
--     f (Not x) = do
--       x' <- f x
--       pure $ toNNF (Not x')
--     f (x :&& y) = do
--       x' <- f x
--       y' <- f y
--       case (x', y') of
--         (Bool True, b) -> pure b
--         (a, Bool True) -> pure a
--         (Bool False, _) -> pure (Bool False)
--         (_, Bool False) -> pure (Bool False)
--         (a, b) | a == b -> pure a
--         (a, b) | a == toNNF (Not b) -> -- A contradiction.
--           pure (Bool False)
--         (a, b) -> pure $ a :&& b
--     f (x :|| y) = do
--       x' <- f x
--       y' <- f y
--       case (x', y') of
--         (Bool True, _) -> pure (Bool True)
--         (_, Bool True) -> pure (Bool True)
--         (Bool False, b) -> pure b
--         (a, Bool False) -> pure a
--         (a, b) | a == toNNF (Not b) -> pure (Bool True) -- A tautology.
--         (a, b) -> pure $ a :|| b
--     f v = astMap m v

--     toNNF :: Symbol -> Symbol
--     toNNF (Not (Not x)) = x
--     toNNF (Not (Bool True)) = Bool False
--     toNNF (Not (Bool False)) = Bool True
--     toNNF (Not (x :|| y)) = toNNF (Not x) :&& toNNF (Not y)
--     toNNF (Not (x :&& y)) = toNNF (Not x) :|| toNNF (Not y)
--     toNNF (Not (x :== y)) = x :/= y
--     toNNF (Not (x :< y)) = x :>= y
--     toNNF (Not (x :> y)) = x :<= y
--     toNNF (Not (x :/= y)) = x :== y
--     toNNF (Not (x :>= y)) = x :< y
--     toNNF (Not (x :<= y)) = x :> y
--     toNNF x = x
