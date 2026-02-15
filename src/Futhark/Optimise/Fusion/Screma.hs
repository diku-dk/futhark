module Futhark.Optimise.Fusion.Screma
  ( splitLambdaByPar,
    splitLambdaByRes,
    fuseScrema,
    fuseSuperScrema,
    SuperScrema (..),
    moveRedScanSuperScrema,
    moveLastSuperScrema,
    moveMidSuperScrema,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Builder (Buildable (..), mkLet)
import Futhark.IR
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Util (splitAt3)
import Futhark.Util.Pretty

-- import Debug.Trace

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
          lambdaBody = mkBody new_stms new_res,
          lambdaReturnType = new_ts
        },
      new_outs
    ),
    ( new_inps',
      Lambda
        { lambdaParams = new_params',
          lambdaBody = mkBody new_stms' new_res',
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
fusible inp_c form_c out_c inp_p form_p out_p =
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

fuseBinds ::
  (Buildable rep, Ord a) =>
  Lambda rep ->
  [a] ->
  [a] ->
  Lambda rep ->
  Stms rep
fuseBinds lam_p out_p inp_c lam_c =
  stmsFromList . mapMaybe bindResToPar $ zip3 out_p res_p ts_p
  where
    ts_p = lambdaReturnType lam_p
    res_p = bodyResult $ lambdaBody lam_p

    inp_c_map =
      M.fromList . zip inp_c $ paramName <$> lambdaParams lam_c

    bindResToPar (out, res, t) =
      case M.lookup out inp_c_map of
        Just name ->
          Just $ certify cs $ mkLet [Ident name t] $ BasicOp $ SubExp e
          where
            SubExpRes cs e = res
        Nothing -> Nothing

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

      (out_red_p, out_post_p) =
        splitAt (redResults $ scremaReduces form_p) out_p
      (out_red_c, out_post_c) =
        splitAt (redResults $ scremaReduces form_c) out_c

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
                    <> fuseBinds
                      (scremaPostLambda form_p)
                      out_post_p
                      (SOAC.inputArray <$> inp_c)
                      (scremaLambda form_c)
                    <> bodyStms (lambdaBody (scremaLambda form_c))
                )
                ( bodyResult (lambdaBody (scremaLambda form_c))
                    <> bodyResult (lambdaBody (scremaPostLambda form_p))
                )
          }

  post_forward_params <- forM (zip (bodyResult (lambdaBody (scremaPostLambda form_p))) (lambdaReturnType (scremaPostLambda form_p))) $ \(res, t) ->
    newParam (maybe (nameFromString "x") baseName (subExpResVName res)) t

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
  pure
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

moveRedScanSuperScrema ::
  (MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
moveRedScanSuperScrema super_screma = do
  renamed_lam_scan_red' <- renameLambda lam_scan_red'
  let (scan_res', red_res') =
        splitAt (scanResults scan')
          . bodyResult
          $ lambdaBody renamed_lam_scan_red'
      (scan_ts', red_ts') =
        splitAt (scanResults scan') $
          lambdaReturnType renamed_lam_scan_red'
      binds = fuseBinds lam out_p inp_c renamed_lam_scan_red'
      stms' = bodyStms $ lambdaBody renamed_lam_scan_red'
      -- Ordering might be wrong here.
      new_scan = scan <> scan'
      new_red = red <> red'
      new_ts = scan_ts <> scan_ts' <> red_ts <> red_ts' <> map_ts
      new_pars = lambdaParams lam
      new_res = scan_res <> scan_res' <> red_res <> red_res' <> map_res
      new_body = mkBody (stms <> binds <> stms') new_res
      new_lam = eliminateDeadCode $ Lambda new_pars new_ts new_body
      (scan_pars', map_pars') =
        splitAt (scanResults scan) (lambdaParams lam_map')

  extra_scan_pars' <- mapM (newParam "x") scan_ts'

  let new_pars' = scan_pars' <> extra_scan_pars' <> map_pars'
      new_ts' = scan_ts' <> lambdaReturnType lam_map'
      new_stms' = bodyStms $ lambdaBody lam_map'
      new_res' =
        varsRes (map paramName extra_scan_pars')
          <> bodyResult (lambdaBody lam_map')
      new_body' = mkBody new_stms' new_res'
      new_lam' = Lambda new_pars' new_ts' new_body'

  pure $
    SuperScrema w inp new_lam new_scan new_red new_lam' [] [] lam''
  where
    stms = bodyStms $ lambdaBody lam
    out_p = [0 .. length (bodyResult $ lambdaBody lam) - 1]
    (scan_out, _, map_out) =
      splitAt3
        (scanResults scan)
        (redResults red)
        [0 .. length (bodyResult $ lambdaBody lam) - 1]
    (scan_res, red_res, map_res) =
      splitAt3
        (scanResults scan)
        (redResults red)
        (bodyResult $ lambdaBody lam)
    (scan_ts, red_ts, map_ts) =
      splitAt3
        (scanResults scan)
        (redResults red)
        (lambdaReturnType lam)
    inp_c = scan_out <> map_out

    (lam_scan_red', lam_map') =
      splitAtLambdaByRes (scanResults scan' + redResults red') lam'

    SuperScrema w inp lam scan red lam' scan' red' lam'' =
      super_screma

moveLastSuperScrema ::
  (MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
moveLastSuperScrema (SuperScrema w inp lam scan red lam' [] [] lam'') = do
  temp_lam'' <- renameLambda lam''
  let new_pars = lambdaParams lam'
      new_ts = lambdaReturnType lam''
      out_p = [0 .. length (bodyResult $ lambdaBody lam') - 1]
      inp_c = out_p
      binds = fuseBinds lam' out_p inp_c temp_lam''
      stms' = bodyStms $ lambdaBody lam'
      stms'' = bodyStms $ lambdaBody temp_lam''
      new_stms = stms' <> binds <> stms''
      new_res = bodyResult $ lambdaBody temp_lam''
      new_body = mkBody new_stms new_res
      new_lam' = eliminateDeadCode $ Lambda new_pars new_ts new_body

  new_lam'' <- mkIdentityLambda $ lambdaReturnType lam''
  pure $
    SuperScrema w inp lam scan red new_lam' [] [] new_lam''
moveLastSuperScrema _ =
  error "moveLastSuperScrema must not have any scans or reduce operation in the end."

numRes :: Lambda rep -> Int
numRes = length . bodyResult . lambdaBody

moveMidSuperScrema ::
  (MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
moveMidSuperScrema (SuperScrema w inp lam scan red lam' scan' red' lam'') = do
  let map_pars =
        drop (scanResults scan + redResults red)
          . map paramName
          $ lambdaParams lam'
      ((inp_c, map_lam, _), _) =
        splitLambdaByPar
          map_pars
          (scan_out <> map_out)
          lam'
          (replicate (numRes lam') ())
  map_lam_renamed <- renameLambda map_lam
  let binds = fuseBinds lam out_p inp_c map_lam_renamed
      stms = bodyStms $ lambdaBody lam
      stms' = bodyStms $ lambdaBody map_lam_renamed
      new_stms = stms <> binds <> stms'
      new_res =
        bodyResult (lambdaBody lam)
          <> bodyResult (lambdaBody map_lam_renamed)
      new_body = mkBody new_stms new_res
      new_pars = lambdaParams lam
      new_ts = lambdaReturnType lam <> lambdaReturnType map_lam_renamed
      new_lam = eliminateDeadCode $ Lambda new_pars new_ts new_body

  forward_params <- mapM (newParam "x") $ lambdaReturnType map_lam_renamed

  let res_mapping =
        M.fromList $
          zip
            (bodyResult $ lambdaBody map_lam)
            (bodyResult $ lambdaBody map_lam_renamed)
      new_pars' = lambdaParams lam' <> forward_params
      new_res' =
        map (\r -> fromMaybe r (M.lookup r res_mapping))
          . bodyResult
          $ lambdaBody lam'
      new_ts' = lambdaReturnType lam'
      new_body' = bodyStms $ lambdaBody lam'
      new_lam' = Lambda new_pars' new_ts' (mkBody new_body' new_res')

  pure $
    SuperScrema w inp new_lam scan red new_lam' scan' red' lam''
  where
    out_p = [0 .. numRes lam - 1]
    (scan_out, _, map_out) =
      splitAt3 (scanResults scan) (redResults red) out_p

-- debug text a = traceShow (text <> show a) a
-- debugWith text f a = traceShow (text <> show (f a)) a
-- debugPretty text a = traceShow (text <> pretty a) a

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

eliminateDeadCode :: Lambda SOACS -> Lambda SOACS
eliminateDeadCode lam =
  Lambda (lambdaParams lam) (lambdaReturnType lam) (mkBody stms res)
  where
    res = bodyResult $ lambdaBody lam
    stms =
      eliminateByRes
        res
        . bodyStms
        $ lambdaBody lam

splitAtLambdaByRes :: Int -> Lambda SOACS -> (Lambda SOACS, Lambda SOACS)
splitAtLambdaByRes i lam =
  ( Lambda pars new_ts (mkBody new_stms new_res),
    Lambda pars new_ts' (mkBody new_stms' new_res')
  )
  where
    pars = lambdaParams lam
    stms = bodyStms $ lambdaBody lam
    new_stms = eliminateByRes new_res stms
    new_stms' = eliminateByRes new_res' stms
    (new_res, new_res') = splitAt i $ bodyResult $ lambdaBody lam
    (new_ts, new_ts') = splitAt i $ lambdaReturnType lam

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

toScrema ::
  SuperScrema SOACS ->
  ([SOAC.Input], ScremaForm SOACS)
toScrema (SuperScrema _ inp lam scan red lam' _ _ _) =
  (inp, ScremaForm lam scan red lam')

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
fuseScrema w inp_c form_c out_c inp_p form_p out_p =
  if not $ fusible inp_c form_c out_c inp_p form_p out_p
    then pure Nothing
    else fmap Just $ do
      (super_screma, new_out) <- fuseSuperScrema w inp_c form_c out_c inp_p form_p out_p
      (new_inp, form) <-
        fmap toScrema $
          moveRedScanSuperScrema super_screma
            >>= moveLastSuperScrema
            >>= moveMidSuperScrema -- <- Broken
      pure (new_inp, form, new_out)
