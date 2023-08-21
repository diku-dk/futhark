-- | This module implements facilities for determining whether a
-- reduction or fold can be expressed in a closed form (i.e. not as a
-- SOAC).
--
-- Right now, the module can detect only trivial cases.  In the
-- future, we would like to make it more powerful, as well as possibly
-- also being able to analyse sequential loops.
module Futhark.Optimise.Simplify.Rules.ClosedForm
  ( foldClosedForm,
    loopClosedForm,
  )
where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Construct
import Futhark.IR
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.Simple (VarLookup)
import Futhark.Transform.Rename

{-
Motivation:

  let {*[int,x_size_27] map_computed_shape_1286} = replicate(x_size_27,
                                                             all_equal_shape_1044) in
  let {*[bool,x_size_27] map_size_checks_1292} = replicate(x_size_27, x_1291) in
  let {bool all_equal_checked_1298, int all_equal_shape_1299} =
    reduceT(fn {bool, int} (bool bacc_1293, int nacc_1294, bool belm_1295,
                            int nelm_1296) =>
              let {bool tuplit_elems_1297} = bacc_1293 && belm_1295 in
              {tuplit_elems_1297, nelm_1296},
            {True, 0}, map_size_checks_1292, map_computed_shape_1286)
-}

-- | @foldClosedForm look foldfun accargs arrargs@ determines whether
-- each of the results of @foldfun@ can be expressed in a closed form.
foldClosedForm ::
  (BuilderOps rep) =>
  VarLookup rep ->
  Pat (LetDec rep) ->
  Lambda rep ->
  [SubExp] ->
  [VName] ->
  RuleM rep ()
foldClosedForm look pat lam accs arrs = do
  inputsize <- arraysSize 0 <$> mapM lookupType arrs

  t <- case patTypes pat of
    [Prim t] -> pure t
    _ -> cannotSimplify

  closedBody <-
    checkResults
      (patNames pat)
      inputsize
      mempty
      Int64
      knownBnds
      (map paramName (lambdaParams lam))
      (lambdaBody lam)
      accs
  isEmpty <- newVName "fold_input_is_empty"
  letBindNames [isEmpty] $
    BasicOp $
      CmpOp (CmpEq int64) inputsize (intConst Int64 0)
  letBind pat
    =<< ( Match [Var isEmpty]
            <$> (pure . Case [Just $ BoolValue True] <$> resultBodyM accs)
            <*> renameBody closedBody
            <*> pure (MatchDec [primBodyType t] MatchNormal)
        )
  where
    knownBnds = determineKnownBindings look lam accs arrs

-- | @loopClosedForm pat respat merge bound bodys@ determines whether
-- the do-loop can be expressed in a closed form.
loopClosedForm ::
  (BuilderOps rep) =>
  Pat (LetDec rep) ->
  [(FParam rep, SubExp)] ->
  Names ->
  IntType ->
  SubExp ->
  Body rep ->
  RuleM rep ()
loopClosedForm pat merge i it bound body = do
  t <- case patTypes pat of
    [Prim t] -> pure t
    _ -> cannotSimplify

  closedBody <-
    checkResults
      mergenames
      bound
      i
      it
      knownBnds
      (map identName mergeidents)
      body
      mergeexp
  isEmpty <- newVName "bound_is_zero"
  letBindNames [isEmpty] $
    BasicOp $
      CmpOp (CmpSlt it) bound (intConst it 0)

  letBind pat
    =<< ( Match [Var isEmpty]
            <$> (pure . Case [Just (BoolValue True)] <$> resultBodyM mergeexp)
            <*> renameBody closedBody
            <*> pure (MatchDec [primBodyType t] MatchNormal)
        )
  where
    (mergepat, mergeexp) = unzip merge
    mergeidents = map paramIdent mergepat
    mergenames = map paramName mergepat
    knownBnds = M.fromList $ zip mergenames mergeexp

checkResults ::
  (BuilderOps rep) =>
  [VName] ->
  SubExp ->
  Names ->
  IntType ->
  M.Map VName SubExp ->
  -- | Lambda-bound
  [VName] ->
  Body rep ->
  [SubExp] ->
  RuleM rep (Body rep)
checkResults pat size untouchable it knownBnds params body accs = do
  ((), stms) <-
    collectStms $
      zipWithM_ checkResult (zip pat res) (zip accparams accs)
  mkBodyM stms $ varsRes pat
  where
    stmMap = makeBindMap body
    (accparams, _) = splitAt (length accs) params
    res = bodyResult body

    nonFree = boundInBody body <> namesFromList params <> untouchable

    checkResult (p, SubExpRes _ (Var v)) (accparam, acc)
      | Just (BasicOp (BinOp bop x y)) <- M.lookup v stmMap,
        x /= y = do
          -- One of x,y must be *this* accumulator, and the other must
          -- be something that is free in the body.
          let isThisAccum = (== Var accparam)
          (this, el) <- liftMaybe $
            case ( (asFreeSubExp x, isThisAccum y),
                   (asFreeSubExp y, isThisAccum x)
                 ) of
              ((Just free, True), _) -> Just (acc, free)
              (_, (Just free, True)) -> Just (acc, free)
              _ -> Nothing

          case bop of
            LogAnd ->
              letBindNames [p] $ BasicOp $ BinOp LogAnd this el
            Add t w -> do
              size' <- asIntS t size
              letBindNames [p]
                =<< eBinOp
                  (Add t w)
                  (eSubExp this)
                  (pure $ BasicOp $ BinOp (Mul t w) el size')
            FAdd t | Just properly_typed_size <- properFloatSize t -> do
              size' <- properly_typed_size
              letBindNames [p]
                =<< eBinOp
                  (FAdd t)
                  (eSubExp this)
                  (pure $ BasicOp $ BinOp (FMul t) el size')
            _ -> cannotSimplify -- Um... sorry.
    checkResult _ _ = cannotSimplify

    asFreeSubExp :: SubExp -> Maybe SubExp
    asFreeSubExp (Var v)
      | v `nameIn` nonFree = M.lookup v knownBnds
    asFreeSubExp se = Just se

    properFloatSize t =
      Just $
        letSubExp "converted_size" $
          BasicOp $
            ConvOp (SIToFP it t) size

determineKnownBindings ::
  VarLookup rep ->
  Lambda rep ->
  [SubExp] ->
  [VName] ->
  M.Map VName SubExp
determineKnownBindings look lam accs arrs =
  accBnds <> arrBnds
  where
    (accparams, arrparams) =
      splitAt (length accs) $ lambdaParams lam
    accBnds =
      M.fromList $
        zip (map paramName accparams) accs
    arrBnds =
      M.fromList $
        mapMaybe isReplicate $
          zip (map paramName arrparams) arrs

    isReplicate (p, v)
      | Just (BasicOp (Replicate (Shape (_ : _)) ve), cs) <- look v,
        cs == mempty =
          Just (p, ve)
    isReplicate _ = Nothing

makeBindMap :: Body rep -> M.Map VName (Exp rep)
makeBindMap = M.fromList . mapMaybe isSingletonStm . stmsToList . bodyStms
  where
    isSingletonStm (Let pat _ e) = case patNames pat of
      [v] -> Just (v, e)
      _ -> Nothing
