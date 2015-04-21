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
module Futhark.Optimise.Fusion.Composing
  ( fuseMaps
  , Input(..)
  )
  where

import Data.List
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Data.Maybe

import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC

import Futhark.Representation.AST
import Futhark.Binder
  (Bindable(..), insertBinding, insertBindings, mkLet')
import Futhark.Tools (mapResult)

-- | Something that can be used as a SOAC input.  As far as this
-- module is concerned, this means supporting just a single operation.
class (Ord a, Eq a) => Input a where
  -- | Check whether an arbitrary input corresponds to a plain
  -- variable input.  If so, return that variable.
  isVarInput :: a -> Maybe VName

instance Input SOAC.Input where
  isVarInput = SOAC.isVarInput

instance (Show a, Ord a, Input inp) => Input (a, inp) where
  isVarInput = isVarInput . snd

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
fuseMaps :: (Input input, Bindable lore) =>
            Lambda lore -- ^ Function of SOAC to be fused.
         -> [input] -- ^ Input of SOAC to be fused.
         -> [(VName,Ident)] -- ^ Output of SOAC to be fused.  The
                            -- first identifier is the name of the
                            -- actual output, where the second output
                            -- is an identifier that can be used to
                            -- bind a single element of that output.
         -> Lambda lore -- ^ Function to be fused with.
         -> [input] -- ^ Input of SOAC to be fused with.
         -> (Lambda lore, [input]) -- ^ The fused lambda and the inputs of
                              -- the resulting SOAC.
fuseMaps lam1 inp1 out1 lam2 inp2 = (lam2', HM.elems inputmap)
  where lam2' =
          lam2 { lambdaParams = lam2redparams ++ HM.keys inputmap
               , lambdaBody =
                 let bnds res = [ mkLet' [] [p] $ PrimOp $ SubExp e
                                | (p,e) <- zip pat $ resultSubExps res]
                     bindLambda res =
                       bnds res `insertBindings` makeCopiesInner (lambdaBody lam2)
                 in makeCopies $ mapResult bindLambda $ lambdaBody lam1
               }

        (lam2redparams, pat, inputmap, makeCopies, makeCopiesInner) =
          fuseInputs lam1 inp1 out1 lam2 inp2

fuseInputs :: (Input input, Bindable lore) =>
              Lambda lore -> [input] -> [(VName,Ident)]
           -> Lambda lore -> [input]
           -> ([Param],
               [Ident],
               HM.HashMap Param input,
               Body lore -> Body lore, Body lore -> Body lore)
fuseInputs lam1 inp1 out1 lam2 inp2 =
  (lam2redparams, outbnds, inputmap, makeCopies, makeCopiesInner)
  where (lam2redparams, lam2arrparams) =
          splitAt (length (lambdaParams lam2) - length inp2) $ lambdaParams lam2
        lam1inputmap = HM.fromList $ zip (lambdaParams lam1) inp1
        lam2inputmap = HM.fromList $ zip lam2arrparams            inp2
        (lam2inputmap', makeCopiesInner) = removeDuplicateInputs lam2inputmap
        originputmap = lam1inputmap `HM.union` lam2inputmap'
        outins = uncurry (outParams $ map fst out1) $
                 unzip $ HM.toList lam2inputmap'
        outbnds = filterOutParams out1 outins
        (inputmap, makeCopies) =
          removeDuplicateInputs $ originputmap `HM.difference` outins

outParams :: Input input =>
             [VName] -> [Param] -> [input]
          -> HM.HashMap Param input
outParams out1 lam2arrparams inp2 =
  HM.fromList $ mapMaybe isOutParam $ zip lam2arrparams inp2
  where isOutParam (p, inp)
          | Just a <- isVarInput inp,
            a `elem` out1 = Just (p, inp)
        isOutParam _      = Nothing

filterOutParams :: Input input =>
                   [(VName,Ident)]
                -> HM.HashMap Param input
                -> [Ident]
filterOutParams out1 outins =
  snd $ mapAccumL checkUsed outUsage out1
  where outUsage = HM.foldlWithKey' add M.empty outins
          where add m p inp =
                  case isVarInput inp of
                    Just v  -> M.insertWith (++) v [p] m
                    Nothing -> m

        checkUsed m (a,ra) =
          case M.lookup a m of
            Just (p:ps) -> (M.insert a ps m, p)
            _           -> (m, ra)

removeDuplicateInputs :: (Input input, Bindable lore) =>
                         HM.HashMap Param input
                      -> (HM.HashMap Param input, Body lore -> Body lore)
removeDuplicateInputs = fst . HM.foldlWithKey' comb ((HM.empty, id), M.empty)
  where comb ((parmap, inner), arrmap) par arr =
          case M.lookup arr arrmap of
            Nothing -> ((HM.insert par arr parmap, inner),
                        M.insert arr (identName par) arrmap)
            Just par' -> ((parmap, inner . forward par par'),
                          arrmap)
        forward to from b =
          mkLet' [] [to] (PrimOp $ SubExp $ Var from)
          `insertBinding` b
