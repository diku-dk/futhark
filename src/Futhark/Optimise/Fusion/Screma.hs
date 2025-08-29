module Futhark.Optimise.Fusion.Screma
  ( fuseLambda,
    splitLambdaByPar,
    splitLambdaByRes,
  )
where

import Data.Bifunctor
import Data.Function (on)
import Data.List (mapAccumL)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Builder (Buildable (..), insertStm, insertStms, mkLet)
import Futhark.Construct (mapResult)
import Futhark.IR
import Futhark.IR.Prop.Names
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
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

isVerticallyFusable :: [SOAC.Input] -> [VName] -> [Bool]
isVerticallyFusable inp_c out_p = (`S.member` inp_c_set) <$> out_p
  where
    inp_c_set = S.fromList $ SOAC.inputArray <$> inp_c

-- | Given a set of names which are interpreted as the resulting
-- variables found from the given Stms, eliminate all Stms which are
-- not use to compute the value of the names.
eliminate :: Names -> Stms SOACS -> Stms SOACS
eliminate = auxiliary (stmsFromList [])
  where
    auxiliary stms' deps stms
      | Just (stms'', stm@(Let v _ e)) <- stmsLast stms =
          if namesIntersect deps $ namesFromList $ patNames v
            then
              auxiliary (oneStm stm <> stms') (freeIn e <> deps) stms''
            else
              auxiliary stms' deps stms''
      | otherwise = stms'

-- | Given a list of result values eliminate Stms which are not used
-- to compute these result values.
eliminateByRes :: [SubExpRes] -> Stms SOACS -> Stms SOACS
eliminateByRes = eliminate . namesFromList . mapMaybe subExpResVName

-- | Given a list of parameter variable names and a lambda, split the
-- lambda function into two where the first function will dependt on
-- the paramter variables given if they exist plus additional
-- parameters if they are need for some computation. The other lambda
-- will have all the other parameters and do the remaining
-- computational work. The two lambda will overlap in computational
-- work.
splitLambdaByPar :: [VName] -> Lambda SOACS -> (Lambda SOACS, Lambda SOACS)
splitLambdaByPar names lam =
  ( Lambda
      { lambdaParams = new_params,
        lambdaBody =
          Body
            { bodyDec = bodyDec body,
              bodyResult = new_res,
              bodyStms = new_stms
            },
        lambdaReturnType = new_ts
      },
    Lambda
      { lambdaParams = new_params',
        lambdaBody =
          Body
            { bodyDec = bodyDec body,
              bodyResult = new_res',
              bodyStms = new_stms'
            },
        lambdaReturnType = new_ts'
      }
  )
  where
    pars = lambdaParams lam
    par_deps = lambdaDependencies mempty lam (oneName . paramName <$> pars)
    body = lambdaBody lam
    stms = bodyStms body
    new_stms = eliminateByRes new_res stms
    new_stms' = eliminateByRes new_res' stms
    new_params = filter ((`nameIn` deps) . paramName) pars
    new_params' = filter ((`nameIn` deps') . paramName) pars
    auxiliary = (\(a, b, c) -> (mconcat a, b, c)) . unzip3
    ((deps, new_res, new_ts), (deps', new_res', new_ts')) =
      bimap auxiliary auxiliary
        . L.partition (namesIntersect (namesFromList names) . (\(a, _, _) -> a))
        $ zip3 par_deps (bodyResult body) (lambdaReturnType lam)

-- | Given a list of result variable names and a lambda, split the
-- lambda function into two where the first function will compute
-- values from the list of result variables given and the other will
-- compute the remaining result. The two lambda will overlap in
-- computational work.
splitLambdaByRes :: [VName] -> Lambda SOACS -> (Lambda SOACS, Lambda SOACS)
splitLambdaByRes names lam =
  ( Lambda
      { lambdaParams = new_params,
        lambdaBody =
          Body
            { bodyDec = bodyDec body,
              bodyResult = new_res,
              bodyStms = new_stms
            },
        lambdaReturnType = new_ts
      },
    Lambda
      { lambdaParams = new_params',
        lambdaBody =
          Body
            { bodyDec = bodyDec body,
              bodyResult = new_res',
              bodyStms = new_stms'
            },
        lambdaReturnType = new_ts'
      }
  )
  where
    pars = lambdaParams lam
    body = lambdaBody lam
    stms = bodyStms body
    new_stms = eliminateByRes new_res stms
    new_stms' = eliminateByRes new_res' stms
    deps = freeIn new_stms <> namesFromList (mapMaybe subExpResVName new_res)
    deps' = freeIn new_stms' <> namesFromList (mapMaybe subExpResVName new_res')
    new_params = filter ((`nameIn` deps) . paramName) pars
    new_params' = filter ((`nameIn` deps') . paramName) pars
    ((new_res, new_ts), (new_res', new_ts')) =
      bimap unzip unzip
        . L.partition (maybe False (`elem` names) . subExpResVName . fst)
        $ zip (bodyResult body) (lambdaReturnType lam)

resToOut :: [VName] -> Lambda SOACS -> SubExpRes -> VName
resToOut out lam = (m M.!)
  where
    m = M.fromList $ flip zip out $ bodyResult $ lambdaBody lam

parToInp :: [SOAC.Input] -> Lambda SOACS -> VName -> SOAC.Input
parToInp inp lam = (m M.!)
  where
    m = M.fromList $ flip zip inp $ paramName <$> lambdaParams lam

fuseIsVarish :: [SOAC.Input] -> [VName] -> Bool
fuseIsVarish inp_c =
  all (maybe True (isJust . SOAC.isVarishInput) . flip M.lookup name_to_inp_c)
  where
    name_to_inp_c = M.fromList $ zip (SOAC.inputArray <$> inp_c) inp_c

fusable ::
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  ScremaForm SOACS ->
  Maybe (Lambda SOACS, Lambda SOACS)
fusable inp_c form_c out_p form_p =
  if fuseIsVarish inp_c out_p && forbidden_c `namesIntersect` forbidden_p
    then
      Nothing
    else
      Just (post_scan_p, post_map_p)
  where
    pre_pars_c = oneName . paramName <$> lambdaParams pre_c
    (pre_scan_deps_c, pre_red_deps_c, _) =
      splitAt3 num_scan_c num_red_c $
        lambdaDependencies mempty pre_c pre_pars_c
    forbidden_c =
      namesFromList
        . fmap (SOAC.inputArray . parToInp inp_c pre_c)
        . namesToList
        $ mconcat (pre_scan_deps_c <> pre_red_deps_c)
    pre_c = scremaLambda form_c
    post_p = scremaPostLambda form_p
    post_scan_pars_p = take num_scan_p $ paramName <$> lambdaParams post_p
    num_scan_c = scanResults $ scremaScans form_c
    num_red_c = redResults $ scremaReduces form_c
    num_scan_p = scanResults $ scremaScans form_p
    post_scan_res_p = bodyResult $ lambdaBody post_scan_p
    forbidden_p = namesFromList $ resToOut out_p post_p <$> post_scan_res_p
    (post_scan_p, post_map_p) = splitLambdaByPar post_scan_pars_p post_p

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
  | Just (post_scan_p, post_map_p) <- fusable inp_c form_c out_p form_p = pure Nothing
  | otherwise = pure Nothing
