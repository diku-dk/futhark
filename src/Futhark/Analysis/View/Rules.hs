module Futhark.Analysis.View.Rules where

import Futhark.Analysis.View.Representation
import Debug.Trace (trace, traceM)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Futhark.SoP.SoP as SoP
import Futhark.MonadFreshNames

normalise :: View -> ViewM View
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
        (a, b) | a == b ->
          pure a
        (a, b) | a == toNNF (Not b) -> -- A contradiction.
          pure (Bool False)
        (a, b) ->
          pure $ a :&& b
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
simplify (View it e) =
  let e' = simplifyRule3 . removeDeadCases $ e
  in  View it e'

removeDeadCases :: Cases Exp -> Cases Exp
removeDeadCases (Cases cases)
  | xs <- NE.filter f cases,
    not $ null xs,
    length xs /= length cases = -- Something actualy got removed.
  trace "👀 Removing dead cases" $
    Cases $ NE.fromList xs
  where
    f (Bool False, _) = False
    f _ = True
removeDeadCases cs = cs

-- TODO Maybe this should only apply to | True => 1 | False => 0
-- (and its negation)?
-- Applies if all case values are integer constants.
simplifyRule3 :: Cases Exp -> Cases Exp
simplifyRule3 e@(Cases ((Bool True, _) NE.:| [])) = e
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
        Cases $ NE.singleton (Bool True, SoP sumOfIndicators)
  where
    justSoP (SoP sop) = SoP.justConstant sop
    justSoP _ = Nothing
simplifyRule3 e = e


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
