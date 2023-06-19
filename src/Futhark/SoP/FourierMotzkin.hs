-- | A sum-of-product representation of integral algebraic expressions
--   formed with addition and multiplication.
--
--   This representation is intended to allow to statically querry whether
--   a symbolic expression is less than (or equal to) zero by means of
--   Fourier-Motzkin elimination, i.e., if we know that the range of
--   `i` is `[l, u]`, then the querry `a*i + b <= 0`
--   can be solved (recursively) by solving the four subproblems:
--   `( (a <= 0) && (a*l + b <= 0) ) || ( (a >= 0) && (a*u + b <= 0) )`
--
--   A possible future extension of the representation and algebra is to
--   support min/max operations (as well). This would require (i) adding
--   a min-max (outermost) layer to the current representation and
--   (ii) extending the algebra. For example, translating a multiplication
--   such as `min(e1,e2) * sop` from PrimExp to sum-of-products form
--   would require to use Fourier-Motzkin to determine the sign of `sop`:
--   if positive than this is equivalent with `min(e1 * sop, e2 * sop)` and
--   if negative it is equivalent to `max(e1 * sop, e2 * sop)`, where
--   `e1 * sop` and `e2 * sop` are recursively translated.
module Futhark.SoP.FourierMotzkin
  ( fmSolveLTh0,
    fmSolveLEq0,
    fmSolveGTh0,
    fmSolveGEq0,
    ($<$),
    ($<=$),
    ($>$),
    ($>=$),
    ($==$),
    ($/=$),
  )
where

import Data.Set qualified as S
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty

---------------------------------------------
--- Fourier-Motzkin elimination algorithm ---
--- for solving inequation of the form:   ---
--- `a*i + b <= 0` and `a*i + b < 0`      ---
--- assumming we have an environment of   ---
--- ranges containing `i =[l,u]` inclusive---
---------------------------------------------

-- | Solves the inequation `sop < 0` by reducing it to
--   `sop + 1 <= 0`, where `sop` denotes an expression
--   in  sum-of-product form.
fmSolveLTh0 :: MonadSoP u e m => SoP u -> m Bool
fmSolveLTh0 = fmSolveLEq0 . (.+. int2SoP 1)

-- | Solves the inequation `sop > 0` by reducing it to
--   `(-1)*sop < 0`, where `sop` denotes an expression
--   in  sum-of-product form.
fmSolveGTh0 :: MonadSoP u e m => SoP u -> m Bool
fmSolveGTh0 = fmSolveLTh0 . negSoP

-- | Solves the inequation `sop >= 0` by reducing it to
--   `(-1)*sop <= 0`, where `sop` denotes an expression
--   in  sum-of-product form.
fmSolveGEq0 :: MonadSoP u e m => SoP u -> m Bool
fmSolveGEq0 = fmSolveLEq0 . negSoP

-- | Assuming `sop` an expression in sum-of-products (SoP) form,
--   this solves the inequation `sop <= 0` as follows:
--   1. find `i`, the most dependent variable in `sop`, i.e., whose
--      transitive closure of the symbols appearing in its range is
--      maximal.
--   2. re-write `sop = a*i + b`, where `a` and `b` are in SoP form.
--   3. assumming the range of `i` to be `[l, u]`, we rewrite our
--      problem as below and solve it recursively:
--      `(a <= 0 && a*l + b <= 0) || (a >= 0 && a*u + b <= 0)`
--      If one of the ranges is missing, then we solve only the
--      subrpoblem that does not use the missing range.
--   If the result is
--      (i)   `True`  if the inequality is found to always holds;
--      (ii)  `False` if there is an `i` for which the inequality does
--                    not hold or if the answer is unknown.
fmSolveLEq0 :: MonadSoP u e m => SoP u -> m Bool
fmSolveLEq0 sop = do
  sop' <- substEquivs sop
  let syms = S.toList $ free sop'
  case (justConstant sop', not (null syms)) of
    (Just v, _) -> pure (v <= 0)
    (_, True) -> do
      rs <- getRanges
      -- step 1: find `i`
      let i =
            snd $
              maximum $
                map (\s -> (length $ transClosInRanges rs $ S.singleton s, s)) syms
          -- step 2: re-write `sop = a*i + b`
          (a, b) = factorSoP [i] sop'
      Range lb k ub <- lookupRange i
      a_leq_0 <- fmSolveLEq0 a
      al_leq_0 <-
        anyM
          ( \l ->
              fmSolveLEq0 $
                a .*. l .+. int2SoP k .*. b
          )
          (S.toList lb)
      a_geq_0 <- fmSolveLEq0 $ negSoP a
      au_leq_0 <-
        anyM
          ( \u ->
              fmSolveLEq0 $
                a .*. u .+. int2SoP k .*. b
          )
          (S.toList ub)
      pure (a_leq_0 && al_leq_0 || a_geq_0 && au_leq_0)
    _ -> pure False

($<$) :: MonadSoP u e m => SoP u -> SoP u -> m Bool
x $<$ y = fmSolveLTh0 $ x .-. y

($<=$) :: MonadSoP u e m => SoP u -> SoP u -> m Bool
x $<=$ y = fmSolveLEq0 $ x .-. y

($>$) :: MonadSoP u e m => SoP u -> SoP u -> m Bool
x $>$ y = fmSolveGTh0 $ x .-. y

($>=$) :: MonadSoP u e m => SoP u -> SoP u -> m Bool
x $>=$ y = fmSolveGEq0 $ x .-. y

($==$) :: MonadSoP u e m => SoP u -> SoP u -> m Bool
x $==$ y = (&&) <$> (x $<=$ y) <*> (x $>=$ y)

($/=$) :: MonadSoP u e m => SoP u -> SoP u -> m Bool
x $/=$ y = (||) <$> (x $<$ y) <*> (x $>$ y)
