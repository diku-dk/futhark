module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Control.Monad.RWS.Strict hiding (Sum)
import qualified Data.Map as M
import Debug.Trace (trace, traceM)
import Futhark.Util.Pretty (prettyString)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Control.Exception
import Futhark.MonadFreshNames

-- substituteViews :: View -> ViewM View
-- substituteViews view@(View Empty _e) = do
--   knownViews <- gets views
--   astMap (m knownViews) view
--   where
--     m vs =
--       ASTMapper
--         { mapOnExp = onExp vs }
--     onExp :: Views -> Exp -> ViewM Exp
--     onExp vs e@(Var vn) =
--       case M.lookup vn vs of
--         Just (View _ e2) ->
--           trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
--                 pure e2
--         _ -> pure e
--     onExp vs e@(Idx (Var vn) eidx) =
--       case M.lookup vn vs of
--         Just (View Empty e2) ->
--           trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
--                 pure $ Idx e2 eidx
--         Just (View (Forall j _) e2) ->
--           -- TODO should I check some kind of equivalence on eidx and i?
--           trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
--                 substituteName j eidx e2
--         _ -> pure e
--     onExp vs v = astMap (m vs) v
-- substituteViews view@(View (Forall _i _dom ) _e) = do
--   knownViews <- gets views
--   astMap (m knownViews) view
--   where
--     m vs =
--       ASTMapper
--         { mapOnExp = onExp vs }
--     onExp :: Views -> Exp -> ViewM Exp
--     onExp vs e@(Var x) =
--       case M.lookup x vs of
--         -- XXX check that domains are compatible
--         -- XXX substitute i for j in the transplanted expression?
--         Just (View Empty e2) ->
--           trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
--                 pure e2
--         Just (View (Forall _ _) _) -> undefined -- Think about this case later.
--         _ -> pure e
--     onExp vs e@(Idx (Var vn) eidx) = do
--       eidx' <- onExp vs eidx
--       case M.lookup vn vs of
--         -- XXX check that domains are compatible
--         Just (View Empty e2) -> do
--           trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
--                 pure $ Idx e2 eidx'
--         Just (View (Forall j _) e2) ->
--           -- TODO should I check some kind of equivalence on eidx and i?
--           trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
--                 substituteName j eidx' e2
--         _ -> pure e
--     onExp vs v = astMap (m vs) v

-- Convert an Exp (with if statements inside) to Cases Exp.
-- This essentially collects all paths from root to leaf, and'ing the
-- branch conditions.
hoistIf :: Exp -> Cases Exp
hoistIf e =
  let vs = flattenValues e
      cs = flattenConds e
  in trace "🎭 hoisting cases" $
       assert (length cs == length vs) $
         Cases . NE.fromList $ zip cs vs
  where
    -- Traverse expression tree, flattening all paths to a list of values.
    m2 = ASTMapper { mapOnExp = flattenValues }
    flattenValues (Var x) = pure $ Var x
    flattenValues (Array xs) = Array <$> mapM flattenValues xs
    flattenValues (If _c t f) =
      mconcat $ map flattenValues [t, f]
      -- mconcat $ map (\(_c, e') -> flattenValues e') (NE.toList cases)
    flattenValues v = astMap m2 v

    -- Flatten Cases to one list of conditions, and'ing nested conditions.
    -- (Happens in the case for Cases.)
    m1 = ASTMapper { mapOnExp = flattenConds }
    -- Leafs.
    flattenConds (Bool _) = pure $ Bool True
    flattenConds Recurrence = pure $ Bool True
    flattenConds (Var _) = pure $ Bool True
    flattenConds (Not (Bool _)) = pure $ Bool True
    flattenConds (Not Recurrence) = pure $ Bool True
    flattenConds (Not (Var _)) = pure $ Bool True
    -- Nodes.
    flattenConds (Array xs) = map (foldl1 (:&&)) $ mapM flattenConds xs
    flattenConds (SoP sop) = do
      foldl (:&&) (Bool True) <$> mapM g (SoP.sopToLists sop)
      where
        g (ts, _) = do
          foldl (:&&) (Bool True) <$> traverse flattenConds ts
    flattenConds (Sum _i _lb _ub x) =
      -- TODO I don't think there can be conds in i, lb or ub since
      -- Sum is just created from Recurrence.
      flattenConds x
    flattenConds (Idx xs i) =
      -- TODO untested
      (:&&) <$> flattenConds xs <*> flattenConds i
    -- flattenConds (Indicator _) = pure $ Bool True
    -- flattenConds (Not x) =
    --   (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (x :== y) =
      (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (x :> y) =
      (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (x :< y) =
      (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (x :/= y) =
      (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (x :>= y) =
      (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (x :<= y) =
      (:&&) <$> flattenConds x <*> flattenConds y
    flattenConds (If c t f) =
      mconcat [(:&& c) <$> flattenConds t, (:&& toNNF (Not c)) <$> flattenConds f]
      -- TODO also handle condition c (flattenConds on c)
      -- Hm, I think c should be already hoisted here, so nothing to handle.
      -- mconcat $ map (\(c, e') -> (:&& c) <$> flattenConds e') (NE.toList cases)
    flattenConds v = astMap m1 v

-- -- Hoist case expressions to the outermost constructor in the view expression
-- -- by merging them.
-- hoistCases :: View -> ViewM View
-- -- hoistCases (View it (Cases es)) =
-- --   pure $ View it (Cases $ NE.fromList $ hoistCases' (Cases es))
-- -- hoistCases (View _ _) = undefined -- shouldn't happen.
-- hoistCases (View it e) =
--   pure $ View it (Cases $ NE.fromList $ hoistCases' e)

-- hoistCases' :: Exp -> [(Exp, Exp)]
-- hoistCases' e =
--   let xs = flattenValues e
--       cs = flattenConds e
--       cs' = concatMap flattenValues cs
--       -- XXX above flattens views substituted into conditions
--       -- but is wrong since
--       -- 1. it will not consider the conditions of the other
--       --    view. (Those have to be and'ed with cs after flattening values.)
--       -- 2. if the other view actually has multiple cases, the lenghts below
--       --    shouldn't match? (Need to replicate corresponding x; just use map
--       --    instead of concatMap to see replications.)
--   in trace ("cs':" <> prettyString cs')
--        assert (length cs' == length xs) $ zip cs' xs
--   where
--     -- Flatten Cases to one list of values.
--     m2 = ASTMapper { mapOnExp = flattenValues }
--     flattenValues (Var x) = pure $ Var x
--     flattenValues (Array xs) = Array <$> mapM flattenValues xs
--     flattenValues (Cases cases) =
--       -- TODO also handle condition c
--       -- Hm, I don't think there's anything to handle in c here, actually.
--       mconcat $ map (\(_c, e') -> flattenValues e') (NE.toList cases)
--     flattenValues v = astMap m2 v

--     -- Flatten Cases to one list of conditions, and'ing nested conditions.
--     -- (Happens in the case for Cases.)
--     m1 = ASTMapper { mapOnExp = flattenConds }
--     flattenConds Recurrence = pure $ Bool True
--     flattenConds (Var _) = pure $ Bool True
--     flattenConds (Array xs) = map (foldl1 (:&&)) $ mapM flattenConds xs
--     flattenConds (SoP sop) = do
--       foldl (:&&) (Bool True) <$> mapM g (SoP.sopToLists sop)
--       where
--         g (ts, _) = do
--           foldl (:&&) (Bool True) <$> traverse flattenConds ts
--     flattenConds (Sum _i _lb _ub x) = -- XXX can there be conds in i, lb or ub?
--       flattenConds x
--       -- I think it's just:
--       -- map (foldl1 (:&&)) $ mapM flattenConds [i, lb, ub, e]
--       -- undefined
--     flattenConds (Idx xs i) =
--       -- TODO untested
--       (:&&) <$> flattenConds xs <*> flattenConds i
--     -- flattenConds (Indicator _) = pure $ Bool True
--     -- flattenConds (Not x) =
--     --   (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (x :== y) =
--       (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (x :> y) =
--       (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (x :< y) =
--       (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (x :/= y) =
--       (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (x :>= y) =
--       (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (x :<= y) =
--       (:&&) <$> flattenConds x <*> flattenConds y
--     flattenConds (Cases cases) =
--       -- TODO also handle condition c (flattenConds on c)
--       -- Hm, I think c should be already hoisted here, so nothing to handle.
--       mconcat $ map (\(c, e') -> (:&& c) <$> flattenConds e') (NE.toList cases)
--     flattenConds v = astMap m1 v

normalise :: View -> ViewM View
-- normalise view =
--   pure $ idMap m view
normalise view =
  pure $ idMap m view
  where
    m =
      ASTMapper
        { mapOnExp = normExp }
    normExp (Var x) = pure $ Var x
    normExp (x :&& y) = do
      x' <- normExp x
      y' <- normExp y
      case (x', y') of
        (Bool True, b) -> pure b
        (a, Bool True) -> pure a
        (Bool False, _) -> pure (Bool False)
        (_, Bool False) -> pure (Bool False)
        (a, b) -> pure $ a :&& b
    normExp (x :|| y) = do
      x' <- normExp x
      y' <- normExp y
      case (x', y') of
        (Bool True, _) -> pure (Bool True)
        (_, Bool True) -> pure (Bool True)
        (Bool False, b) -> pure b
        (a, Bool False) -> pure a
        (a, b) -> pure $ a :|| b
    normExp x@(SoP _) = do
      x' <- astMap m x
      case x' of
        SoP sop -> pure . SoP . normaliseNegation $ sop
        _ -> pure x'
      where
       -- TODO extend this to find any 1 + -1*[[c]] without them being adjacent
       -- or the only terms.
       normaliseNegation sop -- 1 + -1*[[c]] => [[not c]]
        | [([], 1), ([Indicator c], -1)] <- getSoP sop =
          SoP.sym2SoP $ Indicator (Not c)
       normaliseNegation sop = sop
    normExp v = astMap m v

-- TODO Possible to merge this with simplifyPredicates?
simplify :: View -> View
simplify (View it e)
  | Just e' <- simplify' it e =
  View it e'
simplify view = view

simplify' :: Iterator -> Cases Exp -> Maybe (Cases Exp)
simplify' _it = simplifyRule3

-- TODO Maybe this should only apply to | True => 1 | False => 0
-- (and its negation)?
-- Applies if all case values are integer constants.
simplifyRule3 :: Cases Exp -> Maybe (Cases Exp)
simplifyRule3 (Cases ((Bool True, _) NE.:| [])) = Nothing
simplifyRule3 (Cases cases)
  | Just sops <- mapM (justSoP . snd) cases = 
  let preds = NE.map fst cases
      sumOfIndicators =
        SoP.normalize . foldl1 (SoP..+.) . NE.toList $
          NE.zipWith
            (\p x -> SoP.sym2SoP (Indicator p) SoP..*. SoP.int2SoP x)
            preds
            sops
  in  trace "👀 Using Simplification Rule 3" $
        pure $ Cases $ NE.singleton (Bool True, SoP sumOfIndicators)
  where
    justSoP (SoP sop) = SoP.justConstant sop
    justSoP _ = Nothing
simplifyRule3 _ = Nothing


rewrite :: View -> ViewM View
rewrite (View it@(Forall i'' _) (Cases cases))
  | -- Rule 4 (recursive sum)
    (Var i :== b, x) :| [(Not (Var i' :== b'), y)] <- cases,
    -- XXX with NNF we have to test that second case is (toNNF . Not (fst case))?
    i == i'',
    i == i',
    b == b',
    Just x' <- justRecurrence y,
    x == x' = do
      traceM "👀 Using Rule 4 (recursive sum)"
      j <- Var <$> newNameFromString "j"
      let lb = SoP (SoP.int2SoP 0)
      let ub = Var i
      z <- substituteName i j x
      pure $ View it (Cases $ NE.singleton (Bool True, Sum j lb ub z))
  where
    justRecurrence :: Exp -> Maybe Exp
    justRecurrence (SoP sop)
      | [([x], 1), ([Recurrence], 1)] <- getSoP sop =
          Just x
    justRecurrence _ = Nothing
rewrite view = pure view

getSoP :: SoP.SoP Exp -> [([Exp], Integer)]
getSoP = SoP.sopToLists . SoP.normalize
