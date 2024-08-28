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
  ( fuseMaps,
    fuseRedomap,
  )
where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Builder (Buildable (..), insertStm, insertStms, mkLet)
import Futhark.Construct (mapResult)
import Futhark.IR
import Futhark.Util (dropLast, splitAt3, takeLast)

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
fuseMaps ::
  (Buildable rep) =>
  -- | The producer var names that still need to be returned
  Names ->
  -- | Function of SOAC to be fused.
  Lambda rep ->
  -- | Input of SOAC to be fused.
  [SOAC.Input] ->
  -- | Output of SOAC to be fused.  The
  -- first identifier is the name of the
  -- actual output, where the second output
  -- is an identifier that can be used to
  -- bind a single element of that output.
  [(VName, Ident)] ->
  -- | Function to be fused with.
  Lambda rep ->
  -- | Input of SOAC to be fused with.
  [SOAC.Input] ->
  -- | The fused lambda and the inputs of
  -- the resulting SOAC.
  (Lambda rep, [SOAC.Input])
fuseMaps unfus_nms lam1 inp1 out1 lam2 inp2 = (lam2', M.elems inputmap)
  where
    lam2' =
      lam2
        { lambdaParams =
            [ Param mempty name t
              | Ident name t <- lam2redparams ++ M.keys inputmap
            ],
          lambdaBody = new_body2'
        }
    new_body2 =
      let stms res =
            [ certify cs $ mkLet [p] $ BasicOp $ SubExp e
              | (p, SubExpRes cs e) <- zip pat res
            ]
          bindLambda res =
            stmsFromList (stms res) `insertStms` makeCopiesInner (lambdaBody lam2)
       in makeCopies $ mapResult bindLambda (lambdaBody lam1)
    new_body2_rses = bodyResult new_body2
    new_body2' =
      new_body2 {bodyResult = new_body2_rses ++ map (varRes . identName) unfus_pat}
    -- infusible variables are added at the end of the result/pattern/type
    (lam2redparams, unfus_pat, pat, inputmap, makeCopies, makeCopiesInner) =
      fuseInputs unfus_nms lam1 inp1 out1 lam2 inp2

-- (unfus_accpat, unfus_arrpat) = splitAt (length unfus_accs) unfus_pat

fuseInputs ::
  (Buildable rep) =>
  Names ->
  Lambda rep ->
  [SOAC.Input] ->
  [(VName, Ident)] ->
  Lambda rep ->
  [SOAC.Input] ->
  ( [Ident],
    [Ident],
    [Ident],
    M.Map Ident SOAC.Input,
    Body rep -> Body rep,
    Body rep -> Body rep
  )
fuseInputs unfus_nms lam1 inp1 out1 lam2 inp2 =
  (lam2redparams, unfus_vars, outstms, inputmap, makeCopies, makeCopiesInner)
  where
    (lam2redparams, lam2arrparams) =
      splitAt (length lam2params - length inp2) lam2params
    lam1params = map paramIdent $ lambdaParams lam1
    lam2params = map paramIdent $ lambdaParams lam2
    lam1inputmap = M.fromList $ zip lam1params inp1
    lam2inputmap = M.fromList $ zip lam2arrparams inp2
    (lam2inputmap', makeCopiesInner) = removeDuplicateInputs lam2inputmap
    originputmap = lam1inputmap `M.union` lam2inputmap'
    outins =
      uncurry (outParams $ map fst out1) $
        unzip $
          M.toList lam2inputmap'
    outstms = filterOutParams out1 outins
    (inputmap, makeCopies) =
      removeDuplicateInputs $ originputmap `M.difference` outins
    -- Cosmin: @unfus_vars@ is supposed to be the lam2 vars corresponding to unfus_nms (?)
    getVarParPair x = case SOAC.isVarInput (snd x) of
      Just nm -> Just (nm, fst x)
      Nothing -> Nothing -- should not be reached!
    outinsrev = M.fromList $ mapMaybe getVarParPair $ M.toList outins
    unfusible outname
      | outname `nameIn` unfus_nms =
          outname `M.lookup` M.union outinsrev (M.fromList out1)
    unfusible _ = Nothing
    unfus_vars = mapMaybe (unfusible . fst) out1

outParams ::
  [VName] ->
  [Ident] ->
  [SOAC.Input] ->
  M.Map Ident SOAC.Input
outParams out1 lam2arrparams inp2 =
  M.fromList $ mapMaybe isOutParam $ zip lam2arrparams inp2
  where
    isOutParam (p, inp)
      | Just a <- SOAC.isVarInput inp,
        a `elem` out1 =
          Just (p, inp)
    isOutParam _ = Nothing

filterOutParams ::
  [(VName, Ident)] ->
  M.Map Ident SOAC.Input ->
  [Ident]
filterOutParams out1 outins =
  snd $ mapAccumL checkUsed outUsage out1
  where
    outUsage = M.foldlWithKey' add M.empty outins
      where
        add m p inp =
          case SOAC.isVarInput inp of
            Just v -> M.insertWith (++) v [p] m
            Nothing -> m

    checkUsed m (a, ra) =
      case M.lookup a m of
        Just (p : ps) -> (M.insert a ps m, p)
        _ -> (m, ra)

removeDuplicateInputs ::
  (Buildable rep) =>
  M.Map Ident SOAC.Input ->
  (M.Map Ident SOAC.Input, Body rep -> Body rep)
removeDuplicateInputs = fst . M.foldlWithKey' comb ((M.empty, id), M.empty)
  where
    comb ((parmap, inner), arrmap) par arr =
      case M.lookup arr arrmap of
        Nothing ->
          ( (M.insert par arr parmap, inner),
            M.insert arr (identName par) arrmap
          )
        Just par' ->
          ( (parmap, inner . forward par par'),
            arrmap
          )
    forward to from b =
      mkLet [to] (BasicOp $ SubExp $ Var from) `insertStm` b

fuseRedomap ::
  (Buildable rep) =>
  Names ->
  [VName] ->
  Lambda rep ->
  [SubExp] ->
  [SubExp] ->
  [SOAC.Input] ->
  [(VName, Ident)] ->
  Lambda rep ->
  [SubExp] ->
  [SubExp] ->
  [SOAC.Input] ->
  (Lambda rep, [SOAC.Input])
fuseRedomap
  unfus_nms
  outVars
  p_lam
  p_scan_nes
  p_red_nes
  p_inparr
  outPairs
  c_lam
  c_scan_nes
  c_red_nes
  c_inparr =
    -- We hack the implementation of map o redomap to handle this case:
    --   (i) we remove the accumulator formal paramter and corresponding
    --       (body) result from from redomap's fold-lambda body
    let p_num_nes = length p_scan_nes + length p_red_nes
        unfus_arrs = filter (`nameIn` unfus_nms) outVars
        p_lam_body = lambdaBody p_lam
        (p_lam_scan_ts, p_lam_red_ts, p_lam_map_ts) =
          splitAt3 (length p_scan_nes) (length p_red_nes) $ lambdaReturnType p_lam
        (p_lam_scan_res, p_lam_red_res, p_lam_map_res) =
          splitAt3 (length p_scan_nes) (length p_red_nes) $ bodyResult p_lam_body
        p_lam_hacked =
          p_lam
            { lambdaParams = takeLast (length p_inparr) $ lambdaParams p_lam,
              lambdaBody = p_lam_body {bodyResult = p_lam_map_res},
              lambdaReturnType = p_lam_map_ts
            }

        --  (ii) we remove the accumulator's (global) output result from
        --       @outPairs@, then ``map o redomap'' fuse the two lambdas
        --       (in the usual way), and construct the extra return types
        --       for the arrays that fall through.
        (res_lam, new_inp) =
          fuseMaps
            (namesFromList unfus_arrs)
            p_lam_hacked
            p_inparr
            (drop p_num_nes outPairs)
            c_lam
            c_inparr
        (res_lam_scan_ts, res_lam_red_ts, res_lam_map_ts) =
          splitAt3 (length c_scan_nes) (length c_red_nes) $ lambdaReturnType res_lam
        (_, extra_map_ts) =
          unzip $
            filter (\(nm, _) -> nm `elem` unfus_arrs) $
              zip (drop p_num_nes outVars) $
                drop p_num_nes $
                  lambdaReturnType p_lam

        -- (iii) Finally, we put back the accumulator's formal parameter and
        --       (body) result in the first position of the obtained lambda.
        accpars = dropLast (length p_inparr) $ lambdaParams p_lam
        res_body = lambdaBody res_lam
        (res_lam_scan_res, res_lam_red_res, res_lam_map_res) =
          splitAt3 (length c_scan_nes) (length c_red_nes) $ bodyResult res_body
        res_body' =
          res_body
            { bodyResult =
                p_lam_scan_res
                  ++ res_lam_scan_res
                  ++ p_lam_red_res
                  ++ res_lam_red_res
                  ++ res_lam_map_res
            }
        res_lam' =
          res_lam
            { lambdaParams = accpars ++ lambdaParams res_lam,
              lambdaBody = res_body',
              lambdaReturnType =
                p_lam_scan_ts
                  ++ res_lam_scan_ts
                  ++ p_lam_red_ts
                  ++ res_lam_red_ts
                  ++ res_lam_map_ts
                  ++ extra_map_ts
            }
     in (res_lam', new_inp)
