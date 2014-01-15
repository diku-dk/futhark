-- | Facilities for composing SOAC functions.  Mostly intended for use
-- by the fusion module, but factored into a separate module for ease
-- of testing, debugging and development.  Of course, there is nothing
-- preventing you from using the exported functions whereever you
-- want.
--
-- Important: this module is \"dumb\" in the sense that it does not
-- check the validity of its inputs, and does not have any
-- functionality for massaging SOACs to be fusable.  It is assumed
-- that the given SOACs are immediately compatible.
--
-- The module will, however, remove duplicate inputs after fusion.
module L0C.HOTrans.Composing
  ( fuseMaps
  , fuseFilters
  , fuseFilterIntoFold
  , Input(..)
  )
  where

import Data.List
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Data.Maybe

import qualified L0C.HORepresentation.SOAC as SOAC

import L0C.L0

-- | Something that can be used as a SOAC input.  As far as this
-- module is concerned, this means supporting just a single operation.
class (Ord a, Eq a) => Input a where
  -- | Check whether an arbitrary input corresponds to a plain
  -- variable input.  If so, return that variable.
  isVarInput :: a -> Maybe Ident

instance Input SOAC.Input where
  isVarInput = SOAC.isVarInput

-- | @fuseMaps lam1 inp1 out1 lam2 inp2@ fuses the function @lam1@ into
-- @lam2@.  Both functions must be mapping functions, although @lam2@
-- may have leading reduction parameters.  @inp1@ and @inp2@ are the
-- array inputs to the SOACs containing @lam1@ and @lam2@
-- respectively.  @out1@ are the identifiers to which the output of
-- the SOAC containing @lam1@ is bound.  It is nonsensical to call
-- this function unless the intersection of @out1@ and @inp2@ is
-- non-empty.
--
-- If @lam2@ accepts more parameters than there are elements in
-- @inp2@, it is assumed that the surplus (which are positioned at the
-- beginning of the parameter list) are reduction (accumulator)
-- parameters, that do not correspond to array elements, and they are
-- thus not modified.
--
-- The result is the fused function, and a list of the array inputs
-- expected by the SOAC containing the fused function.
fuseMaps :: Input input =>
            TupleLambda -- ^ Function of SOAC to be fused.
         -> [input] -- ^ Input of SOAC to be fused.
         -> [Ident] -- ^ Output of SOAC to be fused.
         -> TupleLambda -- ^ Function to be fused with.
         -> [input] -- ^ Input of SOAC to be fused with.
         -> (TupleLambda, [input]) -- ^ The fused lambda and the
                                   -- inputs of the resulting SOAC.
fuseMaps lam1 inp1 out1 lam2 inp2 = (lam2', HM.elems inputmap)
  where lam2' =
          lam2 { tupleLambdaParams = lam2redparams ++ HM.keys inputmap
               , tupleLambdaBody = makeCopies $
                                   LetPat pat (tupleLambdaBody lam1)
                                   (makeCopiesInner (tupleLambdaBody lam2)) loc
               }
        loc = srclocOf lam2

        (lam2redparams, pat, inputmap, makeCopies, makeCopiesInner) =
          fuseInputs lam1 inp1 out1 lam2 inp2

-- | Similar to 'fuseMaps', although the two functions must be
-- predicates returning @{bool}@.  Returns a new predicate function.
fuseFilters :: Input input =>
               TupleLambda -- ^ Function of SOAC to be fused.
            -> [input] -- ^ Input of SOAC to be fused.
            -> [Ident] -- ^ Output of SOAC to be fused.
            -> TupleLambda -- ^ Function to be fused with.
            -> [input] -- ^ Input of SOAC to be fused with.
            -> VName -- ^ A fresh name (used internally).
            -> (TupleLambda, [input]) -- ^ The fused lambda and the inputs of the resulting SOAC.
fuseFilters lam1 inp1 out1 lam2 inp2 vname =
  fuseFilterInto lam1 inp1 out1 lam2 inp2 vname false
  where false = Literal (TupVal [LogVal False]) $ srclocOf lam2

-- | Similar to 'fuseFilters', except the second function does not
-- have to return @{bool}@, but must be a folding function taking at
-- least one reduction parameter (that is, the number of parameters
-- accepted by the function must be at least one greater than its
-- number of inputs).  If @f1@ is the to-be-fused function, and @f2@
-- is the function to be fused with, the resulting function will be of
-- roughly following form:
--
-- @
-- fn (acc, args) => if f1(args)
--                   then f2(acc,args)
--                   else acc
-- @
fuseFilterIntoFold :: Input input =>
                      TupleLambda -- ^ Function of SOAC to be fused.
                   -> [input] -- ^ Input of SOAC to be fused.
                   -> [Ident] -- ^ Output of SOAC to be fused.
                   -> TupleLambda -- ^ Function to be fused with.
                   -> [input] -- ^ Input of SOAC to be fused with.
                   -> VName -- ^ A fresh name (used internally).
                   -> (TupleLambda, [input]) -- ^ The fused lambda and the inputs of the resulting SOAC.
fuseFilterIntoFold lam1 inp1 out1 lam2 inp2 vname =
  fuseFilterInto lam1 inp1 out1 lam2 inp2 vname identity
  where identity = TupLit (map (Var . fromParam) lam2redparams) $ srclocOf lam2
        lam2redparams = take (length (tupleLambdaParams lam2) - length inp2) $
                        tupleLambdaParams lam2

fuseFilterInto :: Input input =>
                  TupleLambda -> [input] -> [Ident]
               -> TupleLambda -> [input]
               -> VName -> Exp
               -> (TupleLambda, [input])
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
                (tupleLambdaBody lam1) (makeCopiesInner branch) loc
        lam1tuple = TupLit (map (Var . fromParam) $ tupleLambdaParams lam1) loc
        bindins = LetPat pat lam1tuple check loc

        (lam2redparams, pat, inputmap, makeCopies, makeCopiesInner) =
          fuseInputs lam1 inp1 out1 lam2 inp2

fuseInputs :: Input input =>
              TupleLambda -> [input] -> [Ident]
           -> TupleLambda -> [input]
           -> ([Parameter],
               TupIdent,
               HM.HashMap Parameter input,
               Exp -> Exp, Exp -> Exp)
fuseInputs lam1 inp1 out1 lam2 inp2 =
  (lam2redparams, pat, inputmap, makeCopies, makeCopiesInner)
  where loc = srclocOf lam2
        pat = TupId (map (either (`Wildcard` loc) Id) outbnds) loc
        (lam2redparams, lam2arrparams) =
          splitAt (length (tupleLambdaParams lam2) - length inp2) $ tupleLambdaParams lam2
        lam1inputmap = HM.fromList $ zip (tupleLambdaParams lam1) inp1
        lam2inputmap = HM.fromList $ zip lam2arrparams            inp2
        (lam2inputmap', makeCopiesInner) = removeDuplicateInputs lam2inputmap
        originputmap = lam1inputmap `HM.union` lam2inputmap'
        outins = uncurry (outParams out1) $ unzip $ HM.toList lam2inputmap'
        outbnds = filterOutParams out1 outins
        (inputmap, makeCopies) =
          removeDuplicateInputs $ originputmap `HM.difference` outins

outParams :: Input input =>
             [Ident] -> [Parameter] -> [input]
          -> HM.HashMap Parameter input
outParams out1 lam2arrparams inp2 =
  HM.fromList $ mapMaybe isOutParam $ zip lam2arrparams inp2
  where isOutParam (p, inp)
          | Just a <- isVarInput inp,
            a `elem` out1 = Just (p, inp)
        isOutParam _      = Nothing

filterOutParams :: Input input =>
                   [Ident]
                -> HM.HashMap Parameter input
                -> [Either Type Ident]
filterOutParams out1 outins =
  snd $ mapAccumL checkUsed outUsage out1
  where outUsage = HM.foldlWithKey' add M.empty outins
          where add m p inp =
                  case isVarInput inp of
                    Just v  -> M.insertWith (++) v [fromParam p] m
                    Nothing -> m

        checkUsed m a =
          case M.lookup a m of
            Just (p:ps) -> (M.insert a ps m, Right p)
            _           -> (m, Left $ rowType $ identType a)

removeDuplicateInputs :: Input input =>
                         HM.HashMap Parameter input
                      -> (HM.HashMap Parameter input, Exp -> Exp)
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
And now I can have top-level bindings like the following, that explicitly call fuseMaps:

(test1fun, test1ins) = fuseMaps lam1 lam1in out lam2 lam2in
  where lam1in = [SOAC.varInput $ tident "[int] arr_x", SOAC.varInput $ tident "[int] arr_z"]
        lam1 = lambdaToFunction $ tupleLambda "fn {int, int} (int x, int z_b) => {x + z_b, x - z_b}"
        outarr = tident "[int] arr_y"
        outarr2 = tident "[int] arr_unused"
        out  = [outarr2, outarr]
        lam2in = [Var outarr, Var $ tident "[int] arr_z"]
        lam2 = lambdaToFunction $ tupleLambda "fn {int} (int red, int y, int z) => {red + y + z}"


(test2fun, test2ins) = fuseFilterIntoFold lam1 lam1in out lam2 lam2in (name "check")
  where lam1in = [SOAC.varInput $ tident "[int] arr_x", SOAC.varInput $ tident "[int] arr_v"]
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
        lam2in = [SOAC.varInput outarr]
        lam2 = tupleLambda "fn {int} (int x, int p) => {x ^ p}"

I can inspect these values directly in GHCi.

The point is to demonstrate that by factoring functionality out of the
huge monad in the fusion module, we get something that's much easier
to work with interactively.

-}
