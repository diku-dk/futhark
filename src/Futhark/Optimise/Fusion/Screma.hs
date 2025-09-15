module Futhark.Optimise.Fusion.Screma
  ( fuseLambda,
    splitLambdaByPar,
    splitLambdaByRes,
    fuseScrema,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Function (on)
import Data.List (mapAccumL)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace (traceShow)
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Builder (Buildable (..), insertStm, insertStms, mkLet)
import Futhark.Construct (mapResult)
import Futhark.IR
import Futhark.IR.Prop.Names
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Util (dropLast, splitAt3, takeLast)
import Futhark.Util.Pretty (pretty)
import Numeric.Natural

debug text a = traceShow (text <> show a) a

debugWith text f a = traceShow (text <> show (f a)) a

debugPretty text a = traceShow (text <> pretty a) a

fuseLambda ::
  (Ord a) =>
  Lambda SOACS ->
  [a] ->
  [a] ->
  Lambda SOACS ->
  [a] ->
  ([a], Lambda SOACS, [a])
fuseLambda lam_c inp_c out_c lam_p out_p =
  ( extra_inp,
    Lambda
      { lambdaBody = new_body,
        lambdaParams = new_params,
        lambdaReturnType = new_ts
      },
    new_out
  )
  where
    new_params = params_p <> extra_params
    inp_c_map =
      M.fromList $
        zip inp_c (paramName <$> params_c)

    bindResToPar (out, res, t) =
      case M.lookup out inp_c_map of
        Just name ->
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
          ((`notElem` out_p) . snd)
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

concatLambda ::
  Lambda SOACS ->
  Lambda SOACS ->
  Lambda SOACS
concatLambda lam lam' =
  Lambda
    { lambdaBody = new_body,
      lambdaParams = new_params,
      lambdaReturnType = new_ts
    }
  where
    new_params = params_p <> params_c

    new_res = res_p <> res_c
    new_ts = ts_p <> ts_c

    ts_c = lambdaReturnType lam
    ts_p = lambdaReturnType lam'
    res_c = bodyResult $ lambdaBody lam
    res_p = bodyResult $ lambdaBody lam'
    body_stms_c = bodyStms $ lambdaBody lam
    body_stms_p = bodyStms $ lambdaBody lam'
    new_body_stms =
      body_stms_p
        <> body_stms_c
    new_body =
      (lambdaBody lam)
        { bodyStms = new_body_stms,
          bodyResult = new_res
        }
    params_c = lambdaParams lam
    params_p = lambdaParams lam'

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
splitLambdaByPar ::
  [VName] ->
  [a] ->
  Lambda SOACS ->
  [b] ->
  (([a], Lambda SOACS, [b]), ([a], Lambda SOACS, [b]))
splitLambdaByPar names inps lam outs =
  ( ( new_inps,
      Lambda
        { lambdaParams = new_params,
          lambdaBody =
            Body
              { bodyDec = bodyDec body,
                bodyResult = new_res,
                bodyStms = new_stms
              },
          lambdaReturnType = new_ts
        },
      new_outs
    ),
    ( new_inps',
      Lambda
        { lambdaParams = new_params',
          lambdaBody =
            Body
              { bodyDec = bodyDec body,
                bodyResult = new_res',
                bodyStms = new_stms'
              },
          lambdaReturnType = new_ts'
        },
      new_outs'
    )
  )
  where
    pars = lambdaParams lam
    m = M.fromList $ zip pars inps
    par_deps = lambdaDependencies mempty lam (oneName . paramName <$> pars)
    body = lambdaBody lam
    stms = bodyStms body
    new_stms = eliminateByRes new_res stms
    new_stms' = eliminateByRes new_res' stms
    new_inps = (m M.!) <$> new_params
    new_inps' = (m M.!) <$> new_params'
    new_params = filter ((`nameIn` deps) . paramName) pars
    new_params' = filter ((`nameIn` deps') . paramName) pars
    auxiliary = (\(a, b, c, d) -> (mconcat a, b, c, d)) . L.unzip4
    ((deps, new_res, new_ts, new_outs), (deps', new_res', new_ts', new_outs')) =
      bimap auxiliary auxiliary
        . L.partition (namesIntersect (namesFromList names) . (\(a, _, _, _) -> a))
        $ L.zip4 par_deps (bodyResult body) (lambdaReturnType lam) outs

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

-- | Create a mapping from lambda results expression to their output
-- array.
resToOut :: [VName] -> Lambda SOACS -> SubExpRes -> VName
resToOut out lam = (m M.!)
  where
    m = M.fromList $ flip zip out $ bodyResult $ lambdaBody lam

-- | Create a mapping from lambda parameter names to their input
-- array.
parToInp :: [SOAC.Input] -> Lambda SOACS -> VName -> SOAC.Input
parToInp inp lam = (m M.!)
  where
    m = M.fromList $ flip zip inp $ paramName <$> lambdaParams lam

-- | Check that all inputs and outputs are fusible for a producer and
-- consumer.
fuseIsVarish :: [SOAC.Input] -> [VName] -> Bool
fuseIsVarish inp_c =
  all (maybe True (isJust . SOAC.isVarishInput) . flip M.lookup name_to_inp_c)
  where
    name_to_inp_c = M.fromList $ zip (SOAC.inputArray <$> inp_c) inp_c

fuseInputs :: [SOAC.Input] -> [VName] -> [VName]
fuseInputs inp_c =
  filter (`M.member` name_to_inp_c)
  where
    name_to_inp_c = M.fromList $ zip (SOAC.inputArray <$> inp_c) inp_c

-- | Check that two scremas are fusible if they are give back the
-- producer scremas post lambda that has been split into the scan
-- lambda and map lambda. It is not fusible if inputs and outputs are
-- being transformed and if the producers scan result is used for the
-- consumers scans or reduces.
fusible ::
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  Maybe [VName]
fusible inp_c form_c out_c inp_p form_p out_p =
  if not (fuseIsVarish inp_c out_p) || forbidden_c `namesIntersect` forbidden_p
    then
      Nothing
    else
      Just $ fuseInputs inp_c out_p
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
    ((_, post_scan_p, _), _) =
      splitLambdaByPar post_scan_pars_p inp_p post_p out_c

simplifyPrePost ::
  (MonadFreshNames m) =>
  ScremaForm SOACS ->
  ([InOut], [InOut]) ->
  ([InOut], [InOut]) ->
  m
    ( ([InOut], Lambda SOACS, [InOut]),
      ([InOut], Lambda SOACS, [InOut])
    )
simplifyPrePost form (pre_inp, pre_out) (post_inp, post_out) = do
  !post_map' <- renameLambda post_map

  case fuseLambda post_map' post_map_inp post_map_out pre pre_out of
    ([], pre', pre_out') -> pure ((pre_inp, pre', pre_out'), post_scan)
    _any -> error "No extra inputs should be created for post."
  where
    pre = scremaLambda form
    post = scremaPostLambda form
    num_scan = scanResults $ scremaScans form
    scan_names = fmap paramName . take num_scan $ lambdaParams post
    (post_scan, (post_map_inp, post_map, post_map_out)) =
      splitLambdaByPar scan_names post_inp post post_out

data InOut
  = External !VName
  | Internal !Natural
  deriving (Show, Ord, Eq)

external :: VName -> InOut
external = External

internal :: Natural -> InOut
internal = Internal

fromInternal :: InOut -> Maybe Natural
fromInternal (Internal n) = Just n
fromInternal (External _) = Nothing

fromExternal :: InOut -> Maybe VName
fromExternal (External n) = Just n
fromExternal (Internal _) = Nothing

fromExternalUnsafe :: InOut -> VName
fromExternalUnsafe =
  fromMaybe (error "Can not get VName from Internal.") . fromExternal

prePostInOut ::
  Natural ->
  [SOAC.Input] ->
  ScremaForm rep ->
  [VName] ->
  (([InOut], [InOut]), ([InOut], [InOut]))
prePostInOut start inp form out =
  ((pre_inp, pre_out), (post_inp, post_out))
  where
    pre_inp = external . SOAC.inputArray <$> inp
    (red_out, post_out) = splitAt num_red (external <$> out)
    pre = scremaLambda form
    num_scan = scanResults $ scremaScans form
    num_red = redResults $ scremaReduces form
    num_pre_rets = length $ lambdaReturnType pre
    num_map = num_pre_rets - num_scan - num_red
    nat_num_scan = fromIntegral num_scan
    nat_num_map = fromIntegral num_map
    (scan_out, map_out) =
      splitAt num_scan $
        internal <$> take (nat_num_scan + nat_num_map) [start ..]
    pre_out = scan_out <> red_out <> map_out
    post_inp = scan_out <> map_out

scremaFuseInOut ::
  [SOAC.Input] ->
  ScremaForm rep ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm rep ->
  [VName] ->
  ( ( ([InOut], [InOut]),
      ([InOut], [InOut])
    ),
    ( ([InOut], [InOut]),
      ([InOut], [InOut])
    )
  )
scremaFuseInOut inp_c form_c out_c inp_p form_p out_p =
  ( ((pre_inp_c, pre_out_c), (post_inp_c, post_out_c)),
    ((pre_inp_p, pre_out_p), (post_inp_p, post_out_p))
  )
  where
    ((pre_inp_c, pre_out_c), (post_inp_c, post_out_c)) =
      prePostInOut 0 inp_c form_c out_c
    s = succ $ maximum $ (0 :) $ mapMaybe fromInternal pre_out_c
    ((pre_inp_p, pre_out_p), (post_inp_p, post_out_p)) =
      prePostInOut s inp_p form_p out_p

toScrema ::
  (MonadFreshNames m) =>
  [SOAC.Input] ->
  ([InOut], Lambda SOACS, [InOut]) ->
  ([Scan SOACS], [InOut]) ->
  ([Reduce SOACS], [InOut]) ->
  ([InOut], Lambda SOACS, [InOut]) ->
  m ([SOAC.Input], ScremaForm SOACS, [VName])
toScrema
  soac_inps
  (pre_inp, pre, pre_out)
  (scans, scans_inout)
  (reds, reds_inout)
  (post_inp, post, post_out) = do
    (post', post_out') <-
      alignPrePost (pre', pre_out') (post_inp, post, post_out)
    let out = fmap fromExternalUnsafe reds_inout <> post_out'
        inp = (mapping M.!) . fromExternalUnsafe <$> pre_inp
    pure
      ( inp,
        ScremaForm pre' scans reds post',
        out
      )
    where
      mapping =
        M.fromList $
          zip
            (SOAC.inputArray <$> soac_inps)
            soac_inps
      (pre', pre_out') =
        alignLambdaRes (pre, pre_out) (scans_inout <> reds_inout)

alignPrePost ::
  (MonadFreshNames m) =>
  (Lambda SOACS, [InOut]) ->
  ([InOut], Lambda SOACS, [InOut]) ->
  m (Lambda SOACS, [VName])
alignPrePost (pre, pre_out) (post_inp, post, post_out) = do
  (post_inp', pars', ts') <-
    unzip3 <$> auxiliary (pure []) _is_pars _is_ts
  let (id_out, id_res, id_ts) =
        unzip3
          . mapMaybe (\(i, p, t) -> (,varRes $ paramName p,t) <$> fromExternal i)
          . L.nubBy ((==) `on` fst3)
          . filter ((`notElem` post_out) . fst3)
          $ zip3 post_inp' pars' ts'
  pure $
    ( post
        { lambdaParams = pars <> pars',
          lambdaReturnType = ts <> id_ts,
          lambdaBody =
            body {bodyResult = res <> id_res}
        },
      map fromExternalUnsafe post_out <> id_out
    )
  where
    fst3 (a, _, _) = a
    body = lambdaBody post
    pars = lambdaParams post
    ts = lambdaReturnType post
    res = bodyResult body
    _is_pars = zip post_inp pars
    _is_ts = zip pre_out $ lambdaReturnType pre

    auxiliary as _ [] = as
    auxiliary as is_pars ((i, t) : is_ts) =
      case pop ((i ==) . fst) is_pars of
        Just ((_, par), is_pars') ->
          let as' = ((i, par, t) :) <$> as
           in auxiliary as' is_pars' is_ts
        Nothing ->
          let as' = do
                par <- newParam "x" t
                ((i, par, t) :) <$> as
           in auxiliary as' is_pars is_ts

pop :: (a -> Bool) -> [a] -> Maybe (a, [a])
pop _ [] = Nothing
pop p (a : as)
  | p a = Just (a, as)
  | otherwise = fmap (a :) <$> pop p as

align ::
  (Eq a) =>
  [(a, b)] ->
  [a] ->
  [(a, b)]
align = auxiliary []
  where
    auxiliary xs is_res [] = reverse xs <> is_res
    auxiliary xs is_res (i : is) =
      case pop ((i ==) . fst) is_res of
        Just (i_res, is_res') -> auxiliary (i_res : xs) is_res' is
        Nothing -> error "If this happend then the developer used this function incorrectly."

alignLambdaRes ::
  (Eq a) =>
  (Lambda SOACS, [a]) ->
  [a] ->
  (Lambda SOACS, [a])
alignLambdaRes (lam, inout) inout'' =
  ( lam
      { lambdaBody = body {bodyResult = res'},
        lambdaReturnType = ts'
      },
    inout'
  )
  where
    body = lambdaBody lam
    res = bodyResult body
    ts = lambdaReturnType lam
    (inout', (res', ts')) =
      fmap unzip . unzip $ align (zip inout (zip res ts)) inout''

alignLambdaPar ::
  (Eq a) =>
  ([a], Lambda SOACS) ->
  [a] ->
  ([a], Lambda SOACS)
alignLambdaPar (inout, lam) inout'' =
  ( inout',
    lam {lambdaParams = par'}
  )
  where
    par = lambdaParams lam
    (inout', par') = unzip $ align (zip inout par) inout''

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
  | Just
      post_lam_fuse <-
      fusible inp_c form_c out_p inp_p form_p out_p = do
      ( (pre_inp_c, pre_c, pre_out_c),
        (post_inp_c, post_c, post_out_c)
        ) <-
        simplifyPrePost form_c pre_inout_c post_inout_c
      ( (pre_inp_p, pre_p, pre_out_p),
        (post_inp_p, post_p, post_out_p)
        ) <-
        simplifyPrePost form_p pre_inout_p post_inout_p
      let ( (post_fuse_inp_c, post_fuse_c, post_fuse_out_c),
            (pre_rest_inp_c, pre_rest_c, pre_rest_out_c)
            ) = splitLambdaByPar post_lam_fuse pre_inp_c pre_c pre_out_c
          (extra_pre_inp, pre_f', pre_out_f') =
            fuseLambda pre_rest_c pre_rest_inp_c pre_rest_out_c pre_p pre_out_p
          pre_inp_f' = pre_inp_p <> extra_pre_inp
          (extra_post_inp, post_p', post_out_p') =
            fuseLambda post_fuse_c post_fuse_inp_c post_fuse_out_c post_p post_out_p
          post_inp_p' = post_inp_p <> extra_post_inp
          post_inp_f' = post_inp_c <> post_inp_p'
          post_f' = concatLambda post_c post_p'
          post_out_f' = post_out_c <> post_out_p'
      Just
        <$> toScrema
          (inp_c <> inp_p)
          (pre_inp_f', pre_f', pre_out_f')
          (scans_f, scans_inout)
          (reds_f, reds_inout)
          (post_inp_f', post_f', post_out_f')
  | otherwise = pure Nothing
  where
    ( (pre_inout_c, post_inout_c),
      (pre_inout_p, post_inout_p)
      ) =
        scremaFuseInOut inp_c form_c out_c inp_p form_p out_p
    scans_f = on (<>) scremaScans form_c form_p
    reds_f = on (<>) scremaReduces form_c form_p
    scans_inout = scans_inout_c <> scans_inout_p
    reds_inout = reds_inout_c <> reds_inout_p
    num_scans_c = scanResults $ scremaScans form_c
    num_scans_p = scanResults $ scremaScans form_p
    num_reds_p = redResults $ scremaReduces form_p
    num_reds_c = redResults $ scremaReduces form_c
    (scans_inout_c, reds_inout_c, _) =
      splitAt3 num_scans_c num_reds_c $ snd pre_inout_c
    (scans_inout_p, reds_inout_p, _) =
      splitAt3 num_scans_p num_reds_p $ snd pre_inout_p
