{-# LANGUAGE FlexibleContexts #-}
-- | This module implements facilities for determining whether a
-- reduction or fold can be expressed in a closed form (i.e. not as a
-- SOAC).
--
-- Right now, the module can detect only trivial cases.  In the
-- future, we would like to make it more powerful, as well as possibly
-- also being able to analyse sequential loops.
module Futhark.Optimise.Simplify.ClosedForm
  ( foldClosedForm
  , loopClosedForm
  )
where

import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict as M

import Futhark.Construct
import Futhark.Representation.AST
import Futhark.Transform.Rename
import Futhark.Optimise.Simplify.Rule

-- | A function that, given a variable name, returns its definition.
type VarLookup lore = VName -> Maybe (Exp lore, Certificates)

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
foldClosedForm :: (Attributes lore, BinderOps lore) =>
                  VarLookup lore
               -> Pattern lore
               -> Lambda lore
               -> [SubExp] -> [VName]
               -> RuleM lore ()

foldClosedForm look pat lam accs arrs = do
  inputsize <- arraysSize 0 <$> mapM lookupType arrs

  t <- case patternTypes pat of [Prim t] -> return t
                                _ -> cannotSimplify

  closedBody <- checkResults (patternNames pat) inputsize mempty knownBnds
                (map paramName (lambdaParams lam))
                (lambdaBody lam) accs
  isEmpty <- newVName "fold_input_is_empty"
  letBindNames_ [isEmpty] $
    BasicOp $ CmpOp (CmpEq int32) inputsize (intConst Int32 0)
  letBind_ pat =<< (If (Var isEmpty)
                    <$> resultBodyM accs
                    <*> renameBody closedBody
                    <*> pure (IfAttr [primBodyType t] IfNormal))
  where knownBnds = determineKnownBindings look lam accs arrs

-- | @loopClosedForm pat respat merge bound bodys@ determines whether
-- the do-loop can be expressed in a closed form.
loopClosedForm :: (Attributes lore, BinderOps lore) =>
                  Pattern lore
               -> [(FParam lore,SubExp)]
               -> Names -> SubExp -> Body lore
               -> RuleM lore ()
loopClosedForm pat merge i bound body = do
  t <- case patternTypes pat of [Prim t] -> return t
                                _ -> cannotSimplify

  closedBody <- checkResults mergenames bound i knownBnds
                (map identName mergeidents) body mergeexp
  isEmpty <- newVName "bound_is_zero"
  letBindNames_ [isEmpty] $
    BasicOp $ CmpOp (CmpSlt Int32) bound (intConst Int32 0)

  letBind_ pat =<< (If (Var isEmpty)
                    <$> resultBodyM mergeexp
                    <*> renameBody closedBody
                    <*> pure (IfAttr [primBodyType t] IfNormal))
  where (mergepat, mergeexp) = unzip merge
        mergeidents = map paramIdent mergepat
        mergenames = map paramName mergepat
        knownBnds = M.fromList $ zip mergenames mergeexp

checkResults :: BinderOps lore =>
                [VName]
             -> SubExp
             -> Names
             -> M.Map VName SubExp
             -> [VName] -- ^ Lambda-bound
             -> Body lore
             -> [SubExp]
             -> RuleM lore (Body lore)
checkResults pat size untouchable knownBnds params body accs = do
  ((), bnds) <- collectStms $
                zipWithM_ checkResult (zip pat res) (zip accparams accs)
  mkBodyM bnds $ map Var pat

  where bndMap = makeBindMap body
        (accparams, _) = splitAt (length accs) params
        res = bodyResult body

        nonFree = boundInBody body <> namesFromList params <> untouchable

        checkResult (p, Var v) (accparam, acc)
          | Just (BasicOp (BinOp bop x y)) <- M.lookup v bndMap = do
          -- One of x,y must be *this* accumulator, and the other must
          -- be something that is free in the body.
          let isThisAccum = (==Var accparam)
          (this, el) <- liftMaybe $
                        case ((asFreeSubExp x, isThisAccum y),
                              (asFreeSubExp y, isThisAccum x)) of
                          ((Just free, True), _) -> Just (acc, free)
                          (_, (Just free, True)) -> Just (acc, free)
                          _                      -> Nothing

          case bop of
              LogAnd ->
                letBindNames_ [p] $ BasicOp $ BinOp LogAnd this el
              Add t | Just properly_typed_size <- properIntSize t -> do
                        size' <- properly_typed_size
                        letBindNames_ [p] =<<
                          eBinOp (Add t) (eSubExp this)
                          (pure $ BasicOp $ BinOp (Mul t) el size')
              FAdd t | Just properly_typed_size <- properFloatSize t -> do
                        size' <- properly_typed_size
                        letBindNames_ [p] =<<
                          eBinOp (FAdd t) (eSubExp this)
                          (pure $ BasicOp $ BinOp (FMul t) el size')
              _ -> cannotSimplify -- Um... sorry.

        checkResult _ _ = cannotSimplify

        asFreeSubExp :: SubExp -> Maybe SubExp
        asFreeSubExp (Var v)
          | v `nameIn` nonFree = M.lookup v knownBnds
        asFreeSubExp se = Just se

        properIntSize Int32 = Just $ return size
        properIntSize t = Just $ letSubExp "converted_size" $
                          BasicOp $ ConvOp (SExt Int32 t) size

        properFloatSize t =
          Just $ letSubExp "converted_size" $
          BasicOp $ ConvOp (SIToFP Int32 t) size

determineKnownBindings :: VarLookup lore -> Lambda lore -> [SubExp] -> [VName]
                       -> M.Map VName SubExp
determineKnownBindings look lam accs arrs =
  accBnds <> arrBnds
  where (accparams, arrparams) =
          splitAt (length accs) $ lambdaParams lam
        accBnds = M.fromList $
                  zip (map paramName accparams) accs
        arrBnds = M.fromList $ mapMaybe isReplicate $
                  zip (map paramName arrparams) arrs

        isReplicate (p, v)
          | Just (BasicOp (Replicate _ ve), cs) <- look v,
            cs == mempty = Just (p, ve)
        isReplicate _ = Nothing

makeBindMap :: Body lore -> M.Map VName (Exp lore)
makeBindMap = M.fromList . mapMaybe isSingletonStm . stmsToList . bodyStms
  where isSingletonStm (Let pat _ e) = case patternNames pat of
          [v] -> Just (v,e)
          _   -> Nothing
