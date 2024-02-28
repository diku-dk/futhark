module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Control.Monad.RWS.Strict hiding (Sum)
import qualified Data.Map as M
import Debug.Trace (trace)
import Futhark.Util.Pretty (prettyString)
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Control.Exception

substituteViews :: View -> ViewM View
substituteViews view = do
  knownViews <- gets views
  pure $ idMap (m knownViews) view
  where
    m vs =
      ASTMapper
        { mapOnExp = onExp vs }
    onExp vs e@(Var x) = subst vs e x
    onExp vs e@(Idx (Var xs) i) = subst vs e xs
    onExp vs v = astMap (m vs) v
    subst vs e vn =
      case M.lookup vn vs of
        -- XXX use iterator lol
        -- XXX check that domains are compatible
        -- XXX use index i (for starts, just support simple indexing only?)
        -- XXX substitute i for j in the transplanted expression
        Just (View _it e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
          pure e2
        _ -> pure e

-- Hoist case expressions to the outermost constructor in the view expression
-- by merging them.
hoistCases :: View -> ViewM View
hoistCases (View it e) =
  pure $ View it (Cases $ NE.fromList $ hoistCases' e)

hoistCases' :: Exp -> [(Exp, Exp)]
hoistCases' e =
  let cs = getConds e
      xs = onExp e
      -- cs' = trace ("\n cs:" <> prettyString cs) cs
      -- xs' = trace ("\n xs:" <> prettyString xs) xs
  in assert (length cs == length xs) $ zip cs xs
  where
    m1 =
      ASTMapper
        { mapOnExp = getConds }
    getConds (Var _) = pure $ Bool True
    getConds (Array xs) = map (foldl1 (:&&)) $ mapM getConds xs
    -- getConds (If c t f) =
    --   -- TODO also handle if-statements inside c
    --   ((:&& c) <$> getConds t) ++ ((:&& Not c) <$> getConds f)
    getConds (SoP sop) = do
      -- traceM ("AAA" <> prettyString sop)
      -- traceM ("BBB" <> prettyString (SoP.sopToLists sop))
      foldl (:&&) (Bool True) <$> mapM g (SoP.sopToLists sop)
      where
        g (ts, _) = do
          foldl (:&&) (Bool True) <$> traverse getConds ts
    getConds (Sum {}) =
      -- I think it's just:
      -- map (foldl1 (:&&)) $ mapM getConds [i, lb, ub, e]
      undefined
    getConds (Idx xs i) =
      -- TODO untested
      (:&&) <$> getConds xs <*> getConds i
    getConds Recurrence = pure $ Bool True
    getConds (Cases cases) =
      -- TODO also handle condition c (getConds on c)
      mconcat $ map (\(c, e') -> (:&& c) <$> getConds e') (NE.toList cases)
    getConds v = astMap m1 v

    m2 =
      ASTMapper
        { mapOnExp = onExp }
    onExp (Var x) = pure $ Var x
    onExp (Array xs) = Array <$> mapM onExp xs
    -- onExp (If c t f) =
    --   -- TODO also handle if-statements inside c
    --   onExp t ++ onExp f
    onExp (Cases cases) =
      -- TODO also handle condition c
      mconcat $ map (\(c, e') -> onExp e') (NE.toList cases)
    onExp v = astMap m2 v

simplifyPredicates :: View -> ViewM View
simplifyPredicates view =
  pure $ idMap m view
  where
    m =
      ASTMapper
        { mapOnExp = onExp }
    onExp (Var x) = pure $ Var x
    onExp (x :&& y) = do
      x' <- onExp x
      y' <- onExp y
      case (x', y') of
        (Bool True, b) -> pure b
        (a, Bool True) -> pure a
        (a, b) -> pure $ a :&& b
    onExp v = astMap m v
