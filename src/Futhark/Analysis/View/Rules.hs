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

substituteViews :: View -> ViewM View
substituteViews view@(View Empty _e) = do
  knownViews <- gets views
  astMap (m knownViews) view
  where
    m vs =
      ASTMapper
        { mapOnExp = onExp vs }
    onExp :: Views -> Exp -> ViewM Exp
    onExp vs e@(Var vn) =
      case M.lookup vn vs of
        Just (View _ e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
                pure e2
        _ -> pure e
    onExp vs e@(Idx (Var vn) eidx) =
      case M.lookup vn vs of
        Just (View Empty e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
                pure e2
        Just (View (Forall j _) e2) ->
          -- TODO should I check some kind of equivalence on eidx and i?
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
                substituteName j eidx e2
        _ -> pure e
    onExp vs v = astMap (m vs) v
substituteViews view@(View (Forall _i _dom ) _e) = do
  knownViews <- gets views
  astMap (m knownViews) view
  where
    m vs =
      ASTMapper
        { mapOnExp = onExp vs }
    onExp :: Views -> Exp -> ViewM Exp
    onExp vs e@(Var x) =
      case M.lookup x vs of
        -- XXX check that domains are compatible
        -- XXX substitute i for j in the transplanted expression?
        Just (View Empty e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
                pure e2
        Just (View (Forall _ _) _) -> undefined -- Think about this case later.
        _ -> pure e
    onExp vs e@(Idx (Var vn) eidx) =
      case M.lookup vn vs of
        -- XXX check that domains are compatible
        -- XXX use eidx?
        Just (View Empty e2) ->
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
                pure e2
        Just (View (Forall j _) e2) ->
          -- TODO should I check some kind of equivalence on eidx and i?
          trace ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
                substituteName j eidx e2
        _ -> pure e
    onExp vs v = astMap (m vs) v

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
    getConds Recurrence = pure $ Bool True
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
    getConds (Sum _i _lb _ub x) = -- XXX there shouldn't be conds in i, lb and ub.
      getConds x
      -- I think it's just:
      -- map (foldl1 (:&&)) $ mapM getConds [i, lb, ub, e]
      -- undefined
    getConds (Idx xs i) =
      -- TODO untested
      (:&&) <$> getConds xs <*> getConds i
    getConds (Indicator _) = pure $ Bool True
    getConds (Cases cases) =
      -- TODO also handle condition c (getConds on c)
      mconcat $ map (\(c, e') -> (:&& c) <$> getConds e') (NE.toList cases)
    getConds v = astMap m1 v

    m2 =
      ASTMapper
        { mapOnExp = onExp }
    onExp (Var x) = pure $ Var x
    onExp (Array xs) = Array <$> mapM onExp xs
    onExp (Cases cases) =
      -- TODO also handle condition c
      mconcat $ map (\(_c, e') -> onExp e') (NE.toList cases)
    onExp v = astMap m2 v

normalise :: View -> ViewM View
normalise view =
  pure $ idMap m view
  where
    m =
      ASTMapper
        { mapOnExp = onExp }
    onExp (Var x) = pure $ Var x
    onExp (Not (Bool True)) = pure $ Bool False
    onExp (Not (Bool False)) = pure $ Bool True
    onExp (x :&& y) = do
      x' <- onExp x
      y' <- onExp y
      case (x', y') of
        (Bool True, b) -> pure b
        (a, Bool True) -> pure a
        (a, b) -> pure $ a :&& b
    onExp x@(SoP _) = do
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
    onExp v = astMap m v

-- TODO Possible to merge this with simplifyPredicates?
simplify :: View -> View
simplify (View it e)
  | Just e' <- simplify' it e =
  View it e'
simplify view = view

simplify' :: Iterator -> Exp -> Maybe Exp
simplify' _it = simplifyRule3

-- TODO Maybe this should only apply to | True => 1 | False => 0
-- (and its negation)?
-- Applies if all case values are integer constants.
simplifyRule3 :: Exp -> Maybe Exp
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
