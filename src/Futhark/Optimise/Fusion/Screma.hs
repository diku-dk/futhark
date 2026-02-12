module Futhark.Optimise.Fusion.Screma
  ( fuseLambda,
    splitLambdaByPar,
    splitLambdaByRes,
    fuseScrema,
    fuseSuperScrema,
    SuperScrema (..),
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
import Data.Ord (comparing)
import Data.Set qualified as S
import Debug.Trace (trace, traceM, traceShow)
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
import Futhark.Util.Pretty
import Numeric.Natural

data SuperScrema rep
  = SuperScrema
      SubExp
      [SOAC.Input]
      (Lambda rep)
      [Scan rep]
      [Reduce rep]
      (Lambda rep)
      [Scan rep]
      [Reduce rep]
      (Lambda rep)
  deriving (Eq, Ord, Show)

instance (PrettyRep rep) => Pretty (SuperScrema rep) where
  pretty (SuperScrema w inps lam1 scans1 reds1 lam2 scans2 reds2 lam3) =
    "superscrema"
      <> (parens . align)
        ( (pretty w <> comma)
            </> ppTuple' (map pretty inps)
            </> pretty lam1
            <> comma
              </> p' scans1
            <> comma
              </> p' reds1
            <> comma
              </> pretty lam2
            <> comma
              </> p' scans2
            <> comma
              </> p' reds2
            <> comma
              </> pretty lam3
        )
    where
      p' xs = braces (mconcat $ L.intersperse (comma <> line) $ map pretty xs)

pick :: [Bool] -> [a] -> [a]
pick bs xs = map snd $ filter fst $ zip bs xs

-- | Check that all inputs and outputs are fusible for a producer and
-- consumer.
fuseIsVarish :: [SOAC.Input] -> [VName] -> Bool
fuseIsVarish inp_c =
  all (maybe True (isJust . SOAC.isVarishInput) . flip M.lookup name_to_inp_c)
  where
    name_to_inp_c = M.fromList $ zip (SOAC.inputArray <$> inp_c) inp_c

-- | Given a list of parameter names and a lambda, split the lambda
-- into two where the first function will depend on the given
-- parameters given if they exist plus additional parameters if they
-- are need for some computation. The other lambda will have all the
-- other parameters and do the remaining computational work. Some
-- statements may be present in both lambdas.
--
-- Further, each parameter is associated with some additional
-- information @a@, and each result with some additional information
-- @b@. This is also partitioned and returned appropriately.
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
  Bool
fusible inp_p form_p out_p inp_c form_c out_c =
  fuseIsVarish inp_c out_p && not (forbidden_c `namesIntersect` forbidden_p)
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

-- WIP: NOT FINISHED
fuseSuperScrema ::
  (MonadFreshNames m) =>
  SubExp ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  m (SuperScrema SOACS, [VName])
fuseSuperScrema w inp_p form_p out_p inp_c form_c out_c = do
  let inp_c_real_map = map (not . inputFromOutput) inp_c
      inp_c_real = pick inp_c_real_map inp_c
      inp_r = inp_p <> inp_c_real
      ts_p = lambdaReturnType $ scremaPostLambda form_p
      res_p = bodyResult $ lambdaBody $ scremaPostLambda form_p
      inp_c_map =
        M.fromList
          . zip (SOAC.inputArray <$> inp_c)
          . fmap paramName
          . lambdaParams
          $ scremaLambda form_c

      bindResToPar :: (VName, SubExpRes, Type) -> Maybe (Stm SOACS)
      bindResToPar (out, res, t) =
        case M.lookup out inp_c_map of
          Just name ->
            Just $ certify cs $ mkLet [Ident name t] $ BasicOp $ SubExp e
            where
              SubExpRes cs e = res
          Nothing -> Nothing

      (out_red_p, out_post_p) =
        splitAt (redResults $ scremaReduces form_p) out_p
      (out_red_c, out_post_c) =
        splitAt (redResults $ scremaReduces form_c) out_c
      binds =
        stmsFromList
          . mapMaybe bindResToPar
          $ zip3 out_post_p res_p ts_p

  forward_params <- forM (pick inp_c_real_map (lambdaParams (scremaLambda form_c))) $ \p ->
    newParam (baseName (paramName p)) (paramType p)

  let lam1 =
        Lambda
          { lambdaParams =
              lambdaParams (scremaLambda form_p) <> forward_params,
            lambdaReturnType =
              lambdaReturnType (scremaLambda form_p)
                <> map paramType forward_params,
            lambdaBody =
              mkBody
                (bodyStms (lambdaBody (scremaLambda form_p)))
                ( bodyResult (lambdaBody (scremaLambda form_p))
                    <> varsRes (map paramName forward_params)
                )
          }

  let lam2 =
        Lambda
          { lambdaParams =
              lambdaParams (scremaPostLambda form_p)
                <> pick inp_c_real_map (lambdaParams (scremaLambda form_c)),
            lambdaReturnType =
              lambdaReturnType (scremaPostLambda form_p)
                <> lambdaReturnType (scremaLambda form_c),
            lambdaBody =
              mkBody
                ( bodyStms (lambdaBody (scremaPostLambda form_p))
                    <> binds
                    <> bodyStms (lambdaBody (scremaLambda form_c))
                )
                ( bodyResult (lambdaBody (scremaLambda form_c))
                    <> bodyResult (lambdaBody (scremaPostLambda form_p))
                )
          }

  post_forward_params <- forM (zip (bodyResult (lambdaBody (scremaPostLambda form_p))) (lambdaReturnType (scremaPostLambda form_p))) $ \(res, t) ->
    newParam (fromMaybe (nameFromString "x") (baseName <$> subExpResVName res)) t

  let lam3 =
        Lambda
          { lambdaParams =
              lambdaParams (scremaPostLambda form_c) <> post_forward_params,
            lambdaReturnType =
              lambdaReturnType (scremaPostLambda form_c)
                <> map paramType post_forward_params,
            lambdaBody =
              mkBody
                (bodyStms (lambdaBody (scremaPostLambda form_c)))
                ( bodyResult (lambdaBody (scremaPostLambda form_c))
                    <> varsRes (map paramName post_forward_params)
                )
          }
  pure $
    ( SuperScrema
        w
        inp_r
        lam1
        (scremaScans form_p)
        (scremaReduces form_p)
        lam2
        (scremaScans form_c)
        (scremaReduces form_c)
        lam3,
      out_red_c <> out_red_p <> out_post_c <> out_post_p
    )
  where
    inputFromOutput inp = SOAC.inputArray inp `elem` out_p

moveScanSuperScrema ::
  (MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
moveScanSuperScrema super_screma = do
  pure super_screma
  where
    SuperScrema
      w
      inp
      lam
      scan
      red
      lam'
      scan'
      red'
      lam'' = super_screma

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
concatLambda lam_c lam_p =
  Lambda
    { lambdaBody = mkBody new_body_stms $ res_p <> res_c,
      lambdaParams = params_p <> params_c,
      lambdaReturnType = ts_p <> ts_c
    }
  where
    ts_c = lambdaReturnType lam_c
    ts_p = lambdaReturnType lam_p
    res_c = bodyResult $ lambdaBody lam_c
    res_p = bodyResult $ lambdaBody lam_p
    body_stms_c = bodyStms $ lambdaBody lam_c
    body_stms_p = bodyStms $ lambdaBody lam_p
    new_body_stms = body_stms_p <> body_stms_c
    params_c = lambdaParams lam_c
    params_p = lambdaParams lam_p

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

vnameInpToPar :: [VName] -> Lambda SOACS -> VName -> VName
vnameInpToPar inp lam = (m M.!)
  where
    m =
      M.fromList
        . zip inp
        $ paramName <$> lambdaParams lam

fuseInputs :: [SOAC.Input] -> [VName] -> [VName]
fuseInputs inp_c =
  filter (`M.member` name_to_inp_c)
  where
    name_to_inp_c = M.fromList $ zip (SOAC.inputArray <$> inp_c) inp_c

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

prunePostOut :: Lambda SOACS -> [InOut] -> (Lambda SOACS, [InOut])
prunePostOut lam out =
  ( lam
      { lambdaBody = body {bodyResult = res'},
        lambdaReturnType = ts'
      },
    out'
  )
  where
    ts = lambdaReturnType lam
    body = lambdaBody lam
    res = bodyResult body

    (ts', res', out') =
      unzip3
        . mapMaybe (\(t, r, o) -> (t,r,) . External <$> fromExternal o)
        $ zip3 ts res out

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
  pure
    ( post
        { lambdaParams = pars',
          lambdaReturnType = ts <> id_ts,
          lambdaBody = body {bodyResult = res <> id_res}
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

    auxiliary as _ [] = reverse <$> as
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

alignInOuts ::
  (Eq a) =>
  [(a, b)] ->
  [a] ->
  [(a, b)]
alignInOuts = auxiliary []
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
      fmap unzip . unzip $ alignInOuts (zip inout (zip res ts)) inout''

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
    (inout', par') = unzip $ alignInOuts (zip inout par) inout''

fuseScrema ::
  (MonadFreshNames m) =>
  SubExp ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  m (Maybe ([SOAC.Input], ScremaForm SOACS, [VName]))
fuseScrema w inp_c form_c out_c inp_p form_p out_p = undefined
