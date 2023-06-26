module Futhark.Analysis.Refinement.Relations where

import Control.Monad.RWS
import Data.Map qualified as M
import Data.Set qualified as S
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Representation
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.SoP (SoP, Substitute (..), substituteOne)
import Futhark.SoP.SoP qualified as SoP
import Futhark.SoP.Util

-- isTrue :: (Monad m) => CNF Prop -> RefineT m Bool
-- isTrue = cnfIsValidM checkProp
--
-- isFalse :: (Monad m) => CNF Prop -> RefineT m Bool
-- isFalse = cnfIsValidM (checkProp . negateProp)
--
-- checkProp :: (Monad m) => Prop -> RefineT m Bool
-- checkProp (x :< y) = x ^<^ y
-- checkProp (x :<= y) = x ^<=^ y
-- checkProp (x :> y) = x ^>^ y
-- checkProp (x :>= y) = x ^>=^ y
-- checkProp (x :== y) = x ^==^ y
-- checkProp _ = pure False
--
(^==^) :: (Monad m) => Term -> Term -> RefineT m Bool
SoP x ^==^ SoP y = x $==$ y
x ^==^ y = pure $ x == y

(^<^) :: (Monad m) => Term -> Term -> RefineT m Bool
x ^<^ y =
  (x ~+~ intToTerm 1) ^<=^ y

(^>=^) :: (Monad m) => Term -> Term -> RefineT m Bool
x ^>=^ y = y ^<=^ x

(^>^) :: (Monad m) => Term -> Term -> RefineT m Bool
x ^>^ y = y ^<^ x

(^<=^) :: (Monad m) => Term -> Term -> RefineT m Bool
x ^<=^ y =
  ifM
    (termToSoP x $<=$ termToSoP y)
    (pure True)
    (x ^==^ y ^|| x ^^<=^^ y)
  where
    -- z ^^<=^^ Idx xs@(Var xs') i = do
    --  let satForAll (ForAll y ys pred) =
    --        case (y, ys) of
    --          (Var y', xs) ->
    --            let pred' = substituteOne (y', Idx xs i) pred
    --             in ((z :<= Idx xs i) == pred')
    --                  || ((Idx xs i :>= z) == pred')
    --          _ -> False
    --      satForAll _ = False
    --  km <- gets known_map
    --  case km M.!? xs' of
    --    Just ps -> pure $ any satForAll ps
    --    _ -> pure False
    SoP x ^^<=^^ Sigma (Var y_i) (Range y_from y_step y_to) y_e
      | Just (1, Sigma (Var x_i) (Range x_from x_step x_to) x_e, -1) <- SoP.justAffine x =
          andM
            [ y_e ^<=^ intToTerm 1,
              y_to ^==^ x_to,
              y_from ^==^ intToTerm 1,
              x_from ^==^ intToTerm 0,
              x_step ^==^ intToTerm 1,
              y_step ^==^ intToTerm 1,
              x_e ^<=^ y_e
            ]
    SoP x ^^<=^^ SoP y = x $<=$ y
    Sigma (Var x_i) (Range x_from x_step x_to) x_e ^^<=^^ Sigma (Var y_i) (Range y_from y_step y_to) y_e = do
      x <- x_e ^<=^ substituteOne (y_i, Var x_i) y_e
      y <- x_from ^<=^ y_from
      z <- x_to ^<=^ y_to
      andM
        [ x_e ^<=^ substituteOne (y_i, Var x_i) y_e,
          y_from ^<=^ x_from,
          x_to ^<=^ y_to,
          x_step ^==^ intToTerm 1,
          y_step ^==^ intToTerm 1
        ]
    x ^^<=^^ Sigma _ (Range from _ to) e =
      orM
        [ andM
            [ x ^<=^ intToTerm 0,
              intToTerm 0 ^<=^ e
            ],
          andM
            [ x ^<=^ e,
              from ^<=^ to
            ],
          andM
            [ x ^<=^ intToTerm 0,
              to ^<^ from
            ]
        ]
    x ^^<=^^ BoolToInt {} =
      x ^<=^ intToTerm 0
    _ ^^<=^^ _ = pure False

-- isEmptySet :: (Monad m) => Term -> RefineT m Bool
-- isEmptySet Set es = pure $ S.null es
-- isEmptySet (Range from _ to) = to ^<^ from
-- isEmptySet _ = pure False
