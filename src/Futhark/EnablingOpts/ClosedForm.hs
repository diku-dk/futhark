-- | This module implements facilities for determining whether a
-- reduction or fold can be expressed in a closed form (i.e. not as a
-- SOAC).
--
-- Right now, the module can detect only trivial cases.  In the
-- future, we would like to make it more powerful, as well as possibly
-- also being able to analyse sequential loops.
module Futhark.EnablingOpts.ClosedForm
  ( foldClosedForm
  , loopClosedForm
  )
where

import Control.Applicative
import Control.Monad

import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Loc
import Data.Monoid

import Futhark.InternalRep
import Futhark.InternalRep.Renamer
import Futhark.MonadFreshNames
import Futhark.EnablingOpts.Simplifier.Simplify

-- | A function that, given a variable name, returns its definition.
-- XXX: This duplicates something in Futhark.EnablingOpts.Simplification.
type VarLookup = VName -> Maybe Exp

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
foldClosedForm :: VarLookup -> [Ident] -> Lambda -> [SubExp] -> [SubExp]
               -> Simplify [Binding]

foldClosedForm look pat lam accs arrs =
  case checkResults pat knownBindings
       (map fromParam $ lambdaParams lam) (lambdaBody lam) accs lamloc of
    Nothing -> cannotSimplify
    Just closedBody -> do
      isEmpty <- newIdent "fold_input_is_empty" (Basic Bool) lamloc
      let inputsize = arraysSize 0 $ map subExpType arrs
          isEmptyCheck =
            Let [isEmpty] $ BinOp Equal inputsize (intconst 0 lamloc)
                            (Basic Bool) lamloc
          mkBranch  ifNonEmpty =
            Let pat $ If (Var isEmpty)
                             (resultBody [] accs lamloc)
                             ifNonEmpty
                             (map fromConstType $ lambdaReturnType lam)
                             lamloc
      closedBody' <- renameBody closedBody
      return [isEmptyCheck, mkBranch closedBody']
  where lamloc = srclocOf lam

        knownBindings = determineKnownBindings look lam accs arrs

-- | @loopClosedForm pat respat merge bound bodys@ determines whether
-- the do-loop can be expressed in a closed form.
loopClosedForm :: [Ident] -> [Ident] -> [(Ident,SubExp)]
               -> SubExp -> Body -> Simplify [Binding]
loopClosedForm pat respat merge bound body
  | respat == mergepat,
    Just closedBody <- checkResults respat knownBindings
                       mergepat body mergeexp bodyloc = do
  isEmpty <- newIdent "bound_is_zero" (Basic Bool) bodyloc
  let isEmptyCheck =
        Let [isEmpty] $ BinOp Leq bound (intconst 0 bodyloc)
                       (Basic Bool) bodyloc
      mkBranch ifNonEmpty =
        Let pat $ If (Var isEmpty)
                     (resultBody [] mergeexp bodyloc)
                     ifNonEmpty
                     (bodyType body)
                     bodyloc
  closedBody' <- renameBody closedBody
  return [isEmptyCheck, mkBranch closedBody']
  | otherwise = cannotSimplify
  where (mergepat, mergeexp) = unzip merge
        bodyloc = srclocOf body
        knownBindings = HM.fromList merge

checkResults :: [Ident]
             -> HM.HashMap Ident SubExp
             -> [Ident]
             -> Body
             -> [SubExp]
             -> SrcLoc
             -> Maybe Body
checkResults pat knownBindings params body accs bodyloc =
  liftM (\bnds -> Body bnds $ Result [] (map Var pat) bodyloc) $
  concat <$> zipWithM checkResult
             (zip pat $ resultSubExps res) (zip accparams accs)

  where bndMap = makeBindMap body
        (accparams, _) = splitAt (length accs) params
        res = bodyResult body

        nonFree = boundInBody body <>
                  HS.fromList params

        checkResult (p, e) _
          | Just e' <- asFreeSubExp e =
          Just [Let [p] $ subExp e']
        checkResult (p, Var v) (accparam, acc) = do
          e@(BinOp bop x y rt loc) <- HM.lookup v bndMap
          -- One of x,y must be *this* accumulator, and the other must
          -- be something that is free in the body.
          let isThisAccum = (==Var accparam)
          (this, el) <- case ((asFreeSubExp x, isThisAccum y),
                              (asFreeSubExp y, isThisAccum x)) of
                          ((Just free, True), _) -> Just (acc, free)
                          (_, (Just free, True)) -> Just (acc, free)
                          _                           -> Nothing
          case bop of
              LogAnd ->
                Just [Let [v] e,
                      Let [p] $ BinOp LogAnd this el rt loc]
              _ -> Nothing -- Um... sorry.

        checkResult _ _ = Nothing

        asFreeSubExp :: SubExp -> Maybe SubExp
        asFreeSubExp (Var v)
          | HS.member v nonFree = HM.lookup v knownBindings
        asFreeSubExp se = Just se

determineKnownBindings :: VarLookup -> Lambda -> [SubExp] -> [SubExp]
                       -> HM.HashMap Ident SubExp
determineKnownBindings look lam accs arrs =
  accBindings <> arrBindings
  where (accparams, arrparams) =
          splitAt (length accs) $ map fromParam $ lambdaParams lam
        accBindings = HM.fromList $ zip accparams accs
        arrBindings = HM.fromList $ mapMaybe isReplicate $ zip arrparams arrs

        isReplicate (p, Var v)
          | Just (Replicate _ ve _) <- look $ identName v = Just (p, ve)
        isReplicate _       = Nothing

boundInBody :: Body -> HS.HashSet Ident
boundInBody = mconcat . map bound . bodyBindings
  where bound (Let pat _)            = HS.fromList pat

makeBindMap :: Body -> HM.HashMap Ident Exp
makeBindMap = HM.fromList . mapMaybe isSingletonBinding . bodyBindings
  where isSingletonBinding (Let [v] e) = Just (v,e)
        isSingletonBinding _           = Nothing
