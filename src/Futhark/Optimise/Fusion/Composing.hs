-- | Facilities for composing SOAC functions.  Mostly intended for use
-- by the fusion module, but factored into a separate module for ease
-- of testing, debugging and development.  Of course, there is nothing
-- preventing you from using the exported functions whereever you
-- want.
--
-- Important: this module is \"dumb\" in the sense that it does not
-- check the validity of its inputs, and does not have any
-- functionality for massaging SOACs to be fusible.  It is assumed
-- that the given SOACs are immediately compatible.
--
-- The module will, however, remove duplicate inputs after fusion.
module Futhark.Optimise.Fusion.Composing
  ( fuseMaps
  , fuseRedomap
  , mergeReduceOps
  )
  where

import Data.List
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.Maybe

import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC

import Futhark.Representation.AST
import Futhark.Binder
  (Bindable(..), insertBinding, insertBindings, mkLet')
import Futhark.Construct (mapResult)

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
fuseMaps :: Bindable lore =>
            Names     -- ^ The producer var names that still need to be returned
         -> Lambda lore -- ^ Function of SOAC to be fused.
         -> [SOAC.Input] -- ^ Input of SOAC to be fused.
         -> [(VName,Ident)] -- ^ Output of SOAC to be fused.  The
                            -- first identifier is the name of the
                            -- actual output, where the second output
                            -- is an identifier that can be used to
                            -- bind a single element of that output.
         -> Lambda lore -- ^ Function to be fused with.
         -> [SOAC.Input] -- ^ Input of SOAC to be fused with.
         -> (Lambda lore, [SOAC.Input]) -- ^ The fused lambda and the inputs of
                                   -- the resulting SOAC.
fuseMaps unfus_nms lam1 inp1 out1 lam2 inp2 = (lam2', HM.elems inputmap)
  where lam2' =
          lam2 { lambdaParams = [ Param name t
                                | Ident name t <- lam2redparams ++ HM.keys inputmap ]
               , lambdaBody   = new_body2'
               }
        new_body2 = let bnds res = [ mkLet' [] [p] $ PrimOp $ SubExp e
                                   | (p,e) <- zip pat res]
                        bindLambda res =
                            bnds res `insertBindings` makeCopiesInner (lambdaBody lam2)
                    in makeCopies $ mapResult bindLambda (lambdaBody lam1)
        new_body2_rses = bodyResult new_body2
        new_body2'= new_body2 { bodyResult = new_body2_rses ++
                                             map (Var . identName) unfus_pat  }
        -- infusible variables are added at the end of the result/pattern/type
        (lam2redparams, unfus_pat, pat, inputmap, makeCopies, makeCopiesInner) =
          fuseInputs unfus_nms lam1 inp1 out1 lam2 inp2
        --(unfus_accpat, unfus_arrpat) = splitAt (length unfus_accs) unfus_pat

fuseInputs :: Bindable lore =>
              Names
           -> Lambda lore -> [SOAC.Input] -> [(VName,Ident)]
           -> Lambda lore -> [SOAC.Input]
           -> ([Ident], [Ident], [Ident],
               HM.HashMap Ident SOAC.Input,
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
        getVarParPair x = case SOAC.isVarInput (snd x) of
                            Just nm -> Just (nm, fst x)
                            Nothing -> Nothing --should not be reached!
        outinsrev = HM.fromList $ mapMaybe getVarParPair $ HM.toList outins
        unfusible outname
          | outname `HS.member` unfus_nms =
            outname `HM.lookup` HM.union outinsrev (HM.fromList out1)
        unfusible _ = Nothing
        unfus_vars= mapMaybe (unfusible . fst) out1

outParams :: [VName] -> [Ident] -> [SOAC.Input]
          -> HM.HashMap Ident SOAC.Input
outParams out1 lam2arrparams inp2 =
  HM.fromList $ mapMaybe isOutParam $ zip lam2arrparams inp2
  where isOutParam (p, inp)
          | Just a <- SOAC.isVarInput inp,
            a `elem` out1 = Just (p, inp)
        isOutParam _      = Nothing

filterOutParams :: [(VName,Ident)]
                -> HM.HashMap Ident SOAC.Input
                -> [Ident]
filterOutParams out1 outins =
  snd $ mapAccumL checkUsed outUsage out1
  where outUsage = HM.foldlWithKey' add M.empty outins
          where add m p inp =
                  case SOAC.isVarInput inp of
                    Just v  -> M.insertWith (++) v [p] m
                    Nothing -> m

        checkUsed m (a,ra) =
          case M.lookup a m of
            Just (p:ps) -> (M.insert a ps m, p)
            _           -> (m, ra)

removeDuplicateInputs :: Bindable lore =>
                         HM.HashMap Ident SOAC.Input
                      -> (HM.HashMap Ident SOAC.Input, Body lore -> Body lore)
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

fuseRedomap :: Bindable lore =>
               Names  -> [VName]
            -> [SubExp] -> Lambda lore -> [SOAC.Input] -> [(VName,Ident)]
            -> Lambda lore -> [SOAC.Input]
            -> (Lambda lore, [SOAC.Input])
fuseRedomap unfus_nms outVars p_nes p_lam p_inparr outPairs c_lam c_inparr =
  -- We hack the implementation of map o redomap to handle this case:
  --   (i) we remove the accumulator formal paramter and corresponding
  --       (body) result from from redomap's fold-lambda body
  let acc_len     = length p_nes
      unfus_arrs  = filter (`HS.member` unfus_nms) outVars
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
      (res_lam, new_inp) = fuseMaps (HS.fromList unfus_arrs) lam1_hacked p_inparr
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
mergeReduceOps (Lambda par1 bdy1 rtp1) (Lambda par2 bdy2 rtp2) =
  let body' = Body (bodyLore bdy1)
                   (bodyBindings bdy1 ++ bodyBindings bdy2)
                   (bodyResult   bdy1 ++ bodyResult   bdy2)
      (len1, len2) = (length rtp1, length rtp2)
      par'  = take len1 par1 ++ take len2 par2 ++ drop len1 par1 ++ drop len2 par2
  in  Lambda par' body' (rtp1++rtp2)
