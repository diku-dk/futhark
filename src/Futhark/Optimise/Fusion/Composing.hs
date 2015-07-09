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
  , fuseRedomap
  , mergeReduceOps
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
import Futhark.Construct (mapResult)

--import Debug.Trace

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
            [VName]     -- ^ The producer var names that still need to be returned
         -> Lambda lore -- ^ Function of SOAC to be fused.
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
fuseMaps unfus_nms lam1 inp1 out1 lam2 inp2 = (lam2', HM.elems inputmap)
  where lam2' =
          lam2 { lambdaParams = map (`Param` ()) $ lam2redparams ++ HM.keys inputmap
               , lambdaBody   = new_body2'
               }
        let_i_j = mkLet' [] [Ident (lambdaIndex lam1) $ Basic Int] $
                  PrimOp $ SubExp $ Var $ lambdaIndex lam2
        new_body2 = let bnds res = [ mkLet' [] [p] $ PrimOp $ SubExp e
                                   | (p,e) <- zip pat res]
                        bindLambda res =
                            bnds res `insertBindings` makeCopiesInner (lambdaBody lam2)
                    in makeCopies $ let_i_j `insertBinding` mapResult bindLambda (lambdaBody lam1)
        new_body2_rses = bodyResult new_body2
        new_body2'= new_body2 { bodyResult = new_body2_rses ++
                                             map (Var . identName) unfus_pat  }
        -- unfusable variables are added at the end of the result/pattern/type
        (lam2redparams, unfus_pat, pat, inputmap, makeCopies, makeCopiesInner) =
          fuseInputs unfus_nms lam1 inp1 out1 lam2 inp2
        --(unfus_accpat, unfus_arrpat) = splitAt (length unfus_accs) unfus_pat

fuseInputs :: (Input input, Bindable lore) =>
              [VName]
           -> Lambda lore -> [input] -> [(VName,Ident)]
           -> Lambda lore -> [input]
           -> ([Ident], [Ident], [Ident],
               HM.HashMap Ident input,
               Body lore -> Body lore, Body lore -> Body lore)
fuseInputs unfus_nms lam1 inp1 out1 lam2 inp2 =
  (lam2redparams, unfus_vars, outbnds, inputmap, makeCopies, makeCopiesInner)
  where (lam2redparams, lam2arrparams) =
          splitAt (length lam2params - length inp2) lam2params
        lam1params = map paramIdent $ lambdaParams lam1
        lam2params = map paramIdent $ lambdaParams lam2
        lam1inputmap = HM.fromList $ zip lam1params inp1
        lam2inputmap = HM.fromList $ zip lam2arrparams            inp2
        (lam2inputmap', makeCopiesInner) = removeDuplicateInputs lam2inputmap
        originputmap = lam1inputmap `HM.union` lam2inputmap'
        outins = uncurry (outParams $ map fst out1) $
                 unzip $ HM.toList lam2inputmap'
        outbnds= filterOutParams out1 outins
        (inputmap, makeCopies) =
          removeDuplicateInputs $ originputmap `HM.difference` outins
        -- Cosmin: @unfus_vars@ is supposed to be the lam2 vars corresponding to unfus_nms (?)
        getVarParPair x = case isVarInput (snd x) of
                            Just nm -> Just (nm, fst x)
                            Nothing -> Nothing --should not be reached!
        outinsrev = HM.fromList $ mapMaybe getVarParPair $ HM.toList outins
        unfus_vars= mapMaybe (`HM.lookup` (HM.union outinsrev $ HM.fromList out1)) unfus_nms

outParams :: Input input =>
             [VName] -> [Ident] -> [input]
          -> HM.HashMap Ident input
outParams out1 lam2arrparams inp2 =
  HM.fromList $ mapMaybe isOutParam $ zip lam2arrparams inp2
  where isOutParam (p, inp)
          | Just a <- isVarInput inp,
            a `elem` out1 = Just (p, inp)
        isOutParam _      = Nothing

filterOutParams :: Input input =>
                   [(VName,Ident)]
                -> HM.HashMap Ident input
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
                         HM.HashMap Ident input
                      -> (HM.HashMap Ident input, Body lore -> Body lore)
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

fuseRedomap :: (Input input, Bindable lore) =>
               [VName]  -> [VName]
            -> [SubExp] -> Lambda lore -> [input] -> [(VName,Ident)]
            -> Lambda lore -> [input]
            -> (Lambda lore, [input])
fuseRedomap unfus_nms outVars p_nes p_lam p_inparr outPairs c_lam c_inparr =
  -- We hack the implementation of map o redomap to handle this case:
  --   (i) we remove the accumulator formal paramter and corresponding
  --       (body) result from from redomap's fold-lambda body
  let acc_len     = length p_nes
      unfus_accs  = take acc_len outVars
      unfus_arrs  = unfus_nms \\ unfus_accs
      lam1_body   = lambdaBody p_lam
      lam1_accres = take acc_len $ bodyResult lam1_body
      lam1_arrres = drop acc_len $ bodyResult lam1_body
      lam1_hacked = p_lam { lambdaParams = drop acc_len $ lambdaParams p_lam
                          , lambdaBody   = lam1_body { bodyResult = lam1_arrres }
                          , lambdaReturnType = drop acc_len $ lambdaReturnType p_lam }
  --  (ii) we remove the accumulator's (global) output result from
  --       @outPairs@, then ``map o redomap'' fuse the two lambdas
  --       (in the usual way), and construct the extra return types
  --       for the arrays that fall through.
      (res_lam, new_inp) = fuseMaps unfus_arrs lam1_hacked p_inparr
                                    (drop acc_len outPairs) c_lam c_inparr
      (_,extra_rtps) = unzip $ filter (\(nm,_)->elem nm unfus_arrs) $
                       zip (drop acc_len outVars) $ drop acc_len $
                       lambdaReturnType p_lam
  -- (iii) Finally, we put back the accumulator's formal parameter and
  --       (body) result in the first position of the obtained lambda.
      (accrtps, accpars)  = ( take acc_len $ lambdaReturnType p_lam
                            , take acc_len $ lambdaParams p_lam )
      res_body = lambdaBody res_lam
      res_rses = bodyResult res_body
      res_body'= res_body { bodyResult = lam1_accres ++ res_rses }
      res_lam' = res_lam { lambdaParams     = accpars ++ lambdaParams res_lam
                         , lambdaBody       = res_body'
                         , lambdaReturnType = accrtps ++ lambdaReturnType res_lam ++ extra_rtps
                         }
  in  (res_lam', new_inp)

mergeReduceOps :: Bindable lore => Lambda lore -> Lambda lore -> Lambda lore
mergeReduceOps (Lambda i par1 bdy1 rtp1) (Lambda j par2 bdy2 rtp2) =
  let let_i_j = mkLet' [] [Ident i $ Basic Int] $ PrimOp $ SubExp $ Var j
      body' = Body (bodyLore bdy1)
                   (let_i_j : bodyBindings bdy1 ++ bodyBindings bdy2)
                   (bodyResult   bdy1 ++ bodyResult   bdy2)
      (len1, len2) = (length rtp1, length rtp2)
      par'  = take len1 par1 ++ take len2 par2 ++ drop len1 par1 ++ drop len2 par2
  in  Lambda j par' body' (rtp1++rtp2)
