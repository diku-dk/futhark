module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Control.Monad.RWS.Strict hiding (Sum)
import qualified Data.Map as M
import Debug.Trace (trace, traceM)
import Futhark.Util.Pretty (prettyString)
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Control.Exception
-- import Control.Monad.Trans.State.Lazy qualified as S

substituteViews :: View -> ViewM View
substituteViews view = do
  knownViews <- gets views
  pure $ idMap (m knownViews) view
  where
    m vs =
      ASTMapper
        { mapOnExp = onExp vs }
    onExp _ (Var x) = pure $ Var x
    onExp vs e@(Idx (Var xs) i) =
      case M.lookup xs vs of
        -- XXX merge cases
        -- XXX check that domains are compatible
        -- XXX use index i (for starts, just support simple indexing only?)
        -- XXX substitute i for j in the transplanted expression
        Just (View (Forall j d2) e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
          pure e2
        _ -> pure e
    onExp vs v = astMap (m vs) v

-- Hoists case expressions to be the outermost contructor
-- in the view expression by merging cases.
-- 1. seems like a fold where the accumulator is the new Exp
--    that always maintains an outermost Case "invariant"
-- hoistCases :: View -> ViewM View
-- -- hoistCases = pure
-- hoistCases (Forall i dom e) = do
--   traceM ("🎭 hoisting ifs")
--   let cases = hoistCases' e
--   pure $ Forall i dom (Cases $ NE.fromList $ cases)
hoistIf :: View -> ViewM View
hoistIf (View it e) = do
  pure $ View it (Cases $ NE.fromList $ hoistIf' e)

hoistIf' :: Exp -> [(Exp, Exp)]
hoistIf' e =
  let cs = getConds e -- XXX `Not` is a hack to match on outermost if.
      xs = onExp e
      -- cs' = trace ("\n cs:" <> prettyString cs) cs
      -- xs' = trace ("\n xs:" <> prettyString xs) xs
  in assert (length cs /= length (Bool True : xs)) $ zip cs xs
  where
    m1 =
      ASTMapper
        { mapOnExp = getConds }
    getConds (Var x) = pure $ Var x
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
    getConds (Cases cases) =
      -- TODO also handle condition c (getConds on c)
      mconcat $ map (\(c, e) -> (:&& c) <$> getConds e) (NE.toList cases)
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
      mconcat $ map (\(c, e) -> onExp e) (NE.toList cases)
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
