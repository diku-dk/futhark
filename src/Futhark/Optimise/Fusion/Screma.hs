module Futhark.Optimise.Fusion.Screma
  ( fuseLambda,
  )
where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Builder (Buildable (..), insertStm, insertStms, mkLet)
import Futhark.Construct
import Futhark.Construct (mapResult)
import Futhark.IR
import Futhark.IR.SOACS
import Futhark.Util (dropLast, splitAt3, takeLast)

fuseLambda ::
  Lambda SOACS ->
  [SOAC.Input] ->
  [VName] ->
  Lambda SOACS ->
  [VName] ->
  Maybe ([SOAC.Input], Lambda SOACS, [VName])
fuseLambda lam_c inp_c out_c lam_p out_p =
  if all (isJust . SOAC.isVarishInput . snd) $
    mapMaybe (`M.lookup` inp_c_map) out_p
    then
      Just
        ( extra_inp,
          Lambda
            { lambdaBody = new_body,
              lambdaParams = new_params,
              lambdaReturnType = new_ts
            },
          new_out
        )
    else Nothing
  where
    new_params = params_p <> extra_params
    inp_c_map =
      M.fromList $
        zip
          (SOAC.inputArray <$> inp_c)
          (zip (paramName <$> params_c) inp_c)

    bindResToPar (out, res, t) =
      case M.lookup out inp_c_map of
        Just (name, _) ->
          Just $ certify cs $ mkLet [Ident name t] $ BasicOp $ SubExp e
          where
            SubExpRes cs e = res
        Nothing -> Nothing

    binds =
      stmsFromList $
        mapMaybe bindResToPar $
          zip3 out_p res_p ts_p

    (new_out, new_res, new_ts) =
      (out_p <> out_c, res_p <> res_c, ts_p <> ts_c)

    (extra_params, extra_inp) =
      unzip $
        filter
          ((`notElem` out_p) . SOAC.inputArray . snd)
          (zip params_c inp_c)

    ts_c = lambdaReturnType lam_c
    ts_p = lambdaReturnType lam_p
    res_c = bodyResult $ lambdaBody lam_c
    res_p = bodyResult $ lambdaBody lam_p
    body_stms_c = bodyStms $ lambdaBody lam_c
    body_stms_p = bodyStms $ lambdaBody lam_p
    new_body_stms =
      body_stms_p
        <> binds
        <> body_stms_c
    new_body =
      (lambdaBody lam_c)
        { bodyStms = new_body_stms,
          bodyResult = new_res
        }
    params_c = lambdaParams lam_c
    params_p = lambdaParams lam_p

isFusable :: [SOAC.Input] -> [VName] -> [Bool]
isFusable inp_c out_p = (`S.member` inp_c_set) <$> out_p
  where
    inp_c_set = S.fromList $ SOAC.inputArray <$> inp_c

fuseScrema ::
  (MonadFreshNames m) =>
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  m (Maybe ([SOAC.Input], ScremaForm SOACS, [VName]))
fuseScrema inp_c form_c out_c inp_p form_p out_p
  | Just (extra_inp, post_lam', out) <-
      fuseLambda
        lam_c
        inp_c
        out_c
        post_p
        out_p =
      pure Nothing
  | otherwise = pure Nothing
  where
    lam_c = scremaLambda form_c
    post_p = scremaPostLambda form_p
    is_fusable = isFusable inp_c out_p
    reds_c = scremaReduces form_c
    reds_p = scremaReduces form_p
    reds_f = reds_c <> reds_p
    (reds_out_c, post_out_c) = splitAt (redResults reds_c) out_c
    (reds_out_p, post_out_p) = splitAt (redResults reds_p) out_p
    reds_out_f = reds_out_c <> reds_out_p
