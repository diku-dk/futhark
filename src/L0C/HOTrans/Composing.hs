-- | Facilities for composing SOAC functions.  Mostly intended for use
-- by the fusion module, but factored into a separate module for ease
-- of testing, debugging and development.  Of course, there is nothing
-- preventing you from using the exported functions whereever you
-- want.
module L0C.HOTrans.Composing
  ( fuseMaps
  , fuseFilters
  , fuseFilterIntoFold
  )
  where

import Data.List
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M

import L0C.L0

-- | @fusemap lam1 inp1 out1 lam2 inp2@ fuses the function @lam1@ into
-- @lam2@.  Both functions must be mapping functions, although @lam2@
-- may have leading reduction parameters.  @inp1@ and @inp2@ are the
-- array inputs to the SOACs containing @lam1@ and @lam2@
-- respectively.  @out1@ are the identifiers to which the output of
-- the SOAC containing @lam1@ is bound.  It is nonsensical to call
-- this function unless the intersection of @out1@ and @inp2@ is
-- non-empty.
--
-- If @lam2@ accepts more parameters than there are
-- elements in @inp2@, it is assumed that the surplus (which are
-- positioned at the beginning of the parameter list) are reduction
-- parameters, that do not correspond to array elements, and they are
-- thus not modified.
--
-- The result is the fused function, and a list of the array inputs
-- expected by the SOAC containing the fused function.
fuseMaps :: TupleLambda -> [Exp] -> [Ident]
         -> TupleLambda -> [Exp]
         -> (TupleLambda, [Exp])
fuseMaps lam1 inp1 out1 lam2 inp2 = (lam2', HM.elems inputmap)
  where lam2' =
          lam2 { tupleLambdaParams = lam2redparams ++ HM.keys inputmap
               , tupleLambdaBody = makeCopies $
                                   LetPat pat (tupleLambdaBody lam1) (tupleLambdaBody lam2) loc
               }
        loc = srclocOf lam2

        (lam2redparams, pat, inputmap, makeCopies) = fuseInputs lam1 inp1 out1 lam2 inp2

fuseFilters :: TupleLambda -> [Exp] -> [Ident]
            -> TupleLambda -> [Exp] -> VName
            -> (TupleLambda, [Exp])
fuseFilters lam1 inp1 out1 lam2 inp2 vname =
  fuseFilterInto lam1 inp1 out1 lam2 inp2 vname false
  where false = Literal (TupVal [LogVal False]) $ srclocOf lam2

fuseFilterIntoFold :: TupleLambda -> [Exp] -> [Ident]
                   -> TupleLambda -> [Exp] -> VName
                   -> (TupleLambda, [Exp])
fuseFilterIntoFold lam1 inp1 out1 lam2 inp2 vname =
  fuseFilterInto lam1 inp1 out1 lam2 inp2 vname identity
  where identity = TupLit (map (Var . fromParam) lam2redparams) $ srclocOf lam2
        lam2redparams = take (length (tupleLambdaParams lam2) - length inp2) $
                        tupleLambdaParams lam2

fuseFilterInto :: TupleLambda -> [Exp] -> [Ident]
               -> TupleLambda -> [Exp]
               -> VName -> Exp
               -> (TupleLambda, [Exp])
fuseFilterInto lam1 inp1 out1 lam2 inp2 vname falsebranch = (lam2', HM.elems inputmap)
  where lam2' =
          lam2 { tupleLambdaParams = lam2redparams ++ HM.keys inputmap
               , tupleLambdaBody = makeCopies bindins
               }
        loc = srclocOf lam2
        checkident = Ident vname (Elem Bool) loc
        branch = If (Var checkident)
                 (tupleLambdaBody lam2)
                 falsebranch
                 (typeOf (tupleLambdaBody lam2) `unifyUniqueness`
                  typeOf falsebranch)
                 loc
        check = LetPat (TupId [Id checkident] loc)
                (tupleLambdaBody lam1) branch loc
        lam1tuple = TupLit (map (Var . fromParam) $ tupleLambdaParams lam1) loc
        bindins = LetPat pat lam1tuple check loc

        (lam2redparams, pat, inputmap, makeCopies) = fuseInputs lam1 inp1 out1 lam2 inp2

fuseInputs :: TupleLambda -> [Exp] -> [Ident]
           -> TupleLambda -> [Exp]
           -> ([Parameter], TupIdent, HM.HashMap Parameter Exp, Exp -> Exp)
fuseInputs lam1 inp1 out1 lam2 inp2 =
  (lam2redparams, pat, inputmap, makeCopies)
  where loc = srclocOf lam2
        pat = TupId (map (either (`Wildcard` loc) Id) outbnds) loc
        (lam2redparams, lam2arrparams) =
          splitAt (length (tupleLambdaParams lam2) - length inp2) $ tupleLambdaParams lam2
        originputmap = HM.fromList $
                       zip (tupleLambdaParams lam1 ++ lam2arrparams) (inp1 ++ inp2)
        (outins, outbnds) = filterOutParams out1 lam2arrparams inp2
        (inputmap, makeCopies) =
          removeDuplicateInputs $ foldr HM.delete originputmap outins

filterOutParams :: [Ident] -> [Parameter] -> [Exp]
                -> ([Parameter], [Either Type Ident])
filterOutParams out1 lam2arrparams inp2 =
  (map fst outParams, snd $ mapAccumL checkUsed outUsage out1)
  where outParams = map snd $ filter fst $ snd $
                    mapAccumL isOutParam out1 $ zip lam2arrparams inp2
        isOutParam outs (p, Var a)
          | a `elem` outs      = (a `delete` outs, (True,  (p, Var a)))
          | otherwise          = (outs,            (False, (p, Var a)))
        isOutParam outs (p, e) = (outs,            (False, (p, e)))
        outUsage = foldl add M.empty outParams
          where add m (p, e) = M.insertWith (++) e [fromParam p] m
        checkUsed m a =
          case M.lookup (Var a) m of
            Just (p:ps) -> (M.insert (Var a) ps m, Right p)
            _           -> (m, Left $ rowType $ identType a)

removeDuplicateInputs :: HM.HashMap Parameter Exp
                      -> (HM.HashMap Parameter Exp, Exp -> Exp)
removeDuplicateInputs = fst . HM.foldlWithKey' comb ((HM.empty, id), M.empty)
  where comb ((parmap, inner), arrmap) par arr =
          case M.lookup arr arrmap of
            Nothing -> ((HM.insert par arr parmap, inner),
                        M.insert arr par arrmap)
            Just par' -> ((parmap, inner . forward par par'),
                          arrmap)
        forward to from e = LetPat (Id $ fromParam to)
                            (Var $ fromParam from) e $ srclocOf e

{-

An example of how I tested this module:

I add this import:

import L0C.Dev

-}

{-
And now I can have top-level bindings the like the following, that explicitly call fuseMaps:

(test1fun, test1ins) = fuseMaps lam1 lam1in out lam2 lam2in
  where lam1in = [Var $ tident "[int] arr_x", Var $ tident "[int] arr_z"]
        lam1 = lambdaToFunction $ tupleLambda "fn {int, int} (int x, int z_b) => {x + z_b, x - z_b}"
        outarr = tident "[int] arr_y"
        outarr2 = tident "[int] arr_unused"
        out  = [outarr2, outarr]
        lam2in = [Var outarr, Var $ tident "[int] arr_z"]
        lam2 = lambdaToFunction $ tupleLambda "fn {int} (int red, int y, int z) => {red + y + z}"


(test2fun, test2ins) = fuseFilterIntoFold lam1 lam1in out lam2 lam2in (name "check")
  where lam1in = [Var $ tident "[int] arr_x", Var $ tident "[int] arr_v"]
        lam1 = tupleLambda "fn {bool} (int x, int v) => x+v < 0"
        outarr = tident "[int] arr_y"
        outarr2 = tident "[int] arr_unused"
        out  = [outarr, outarr2]
        lam2in = [Var outarr]
        lam2 = tupleLambda "fn {int} (int red, int y) => {red + y}"

(test3fun, test3ins) = fuseFilterIntoFold lam1 lam1in out lam2 lam2in (name "check")
  where lam1in = [expr "iota(30)", expr "replicate(30, 1)"]
        lam1 = tupleLambda "fn {bool} (int i, int j) => {i+j < 0}"
        outarr = tident "[int] arr_p"
        outarr2 = tident "[int] arr_unused"
        out  = [outarr, outarr2]
        lam2in = [Var outarr]
        lam2 = tupleLambda "fn {int} (int x, int p) => {x ^ p}"

I can inspect these values directly in GHCi.

The point is to demonstrate that by factoring functionality out of the
huge monad in the fusion module, we get something that's much easier
to work with interactively.

-}
