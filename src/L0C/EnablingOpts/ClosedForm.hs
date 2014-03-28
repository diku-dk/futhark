-- | This module implements facilities for determining whether a
-- reduction or fold can be expressed in a closed form (i.e. not as a
-- SOAC).
--
-- Right now, the module can detect only trivial cases.  In the
-- future, we would like to make it more powerful, as well as possibly
-- also being able to analyse sequential loops.
module L0C.EnablingOpts.ClosedForm
  ( foldClosedForm
  )
where

import Control.Applicative
import Control.Monad

import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Loc
import Data.Monoid

import L0C.InternalRep
import L0C.InternalRep.Renamer
import L0C.NeedNames
import L0C.MonadFreshNames

-- | A function that, given a variable name, returns its definition.
-- XXX: This duplicates something in L0C.EnablingOpts.Simplification.
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
               -> NeedNames (Maybe [Binding])

foldClosedForm look pat lam accs arrs =
  case checkResults of
    Nothing -> return Nothing
    Just closedBody -> do
      isEmpty <- newIdent "fold_input_is_empty" (Basic Bool) lamloc
      let inputsize = arraysSize 0 $ map subExpType arrs
          zero      = Constant (BasicVal $ IntVal 0) lamloc
          isEmptyCheck =
            Let [isEmpty] $ BinOp Equal inputsize zero (Basic Bool) lamloc
          mkBranch  ifNonEmpty =
            Let pat $ If (Var isEmpty)
                             (resultBody [] accs lamloc)
                             ifNonEmpty
                             (map fromConstType $ lambdaReturnType lam)
                             lamloc
      closedBody' <- renameBody closedBody
      return $ Just [isEmptyCheck, mkBranch closedBody']
  where lamloc = srclocOf lam
        res = bodyResult $ lambdaBody lam
        bndMap = makeBindMap $ lambdaBody lam
        (accparams, _) = splitAt (length accs) $ lambdaParams lam

        checkResults =
          liftM (\bnds -> Body bnds $ Result [] (map Var pat) lamloc) $
          concat <$> zipWithM checkResult
                     (zip pat $ resultSubExps res) (zip accparams accs)

        checkResult (p, e) _
          | Just e' <- asFreeSubExp e =
          Just [Let [p] $ subExp e']
        checkResult (p, Var v) (accparam, acc) = do
          e@(BinOp bop x y rt loc) <- HM.lookup v bndMap
          -- One of x,y must be *this* accumulator, and the other must
          -- be something that is free in the body.
          let isThisAccum = (==Var (fromParam accparam))
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

        knownBindings = determineKnownBindings look lam accs arrs

        nonFree = boundInBody (lambdaBody lam) <>
                  HS.fromList (map fromParam $ lambdaParams lam)

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
        bound (DoLoop merge _ _ _)     = HS.fromList $ map fst merge
        bound (LetWith _ dest _ _ _) = HS.singleton dest

makeBindMap :: Body -> HM.HashMap Ident Exp
makeBindMap = HM.fromList . mapMaybe isSingletonBinding . bodyBindings
  where isSingletonBinding (Let [v] e) = Just (v,e)
        isSingletonBinding _               = Nothing
