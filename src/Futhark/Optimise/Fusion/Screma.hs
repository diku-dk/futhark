module Futhark.Optimise.Fusion.Screma
  ( splitLambdaByPar,
    fuseScrema,
    fuseSuperScrema,
    SuperScrema (..),
    moveRedScanSuperScrema,
    moveLastSuperScrema,
    fusible,
    toScrema,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.Alias
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util (splitAt3)
import Futhark.Util.Pretty

-- | The fused representation of two scremas.
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

-- | Using a boolean mask pick elements in the
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
  (MonadFail m) =>
  [VName] ->
  [inp] ->
  Lambda SOACS ->
  [out] ->
  m (([inp], Lambda SOACS, [out]), ([inp], Lambda SOACS, [out]))
splitLambdaByPar names inp lam out = do
  when
    (consumedOverlap new_lam new_lam')
    (fail "Can not fuse due to overlap consumption in pre and post.")
  when
    (parAccsOverlap new_lam new_lam')
    (fail "Can not fuse due to overlap in parameter accumalators.")
  when
    (resAccsOverlap new_lam new_lam')
    (fail "Can not fuse due to overlap in result accumalators.")
  pure
    ( (new_inp, new_lam, new_out),
      (new_inp', new_lam', new_out')
    )
  where
    new_lam = eliminateByRes $ Lambda new_params new_ts (mkBody stms new_res)
    new_lam' = eliminateByRes $ Lambda new_params' new_ts' (mkBody stms new_res')
    pars = lambdaParams lam
    m = M.fromList $ zip pars inp
    par_deps = lambdaDependencies mempty lam (oneName . paramName <$> pars)
    body = lambdaBody lam
    stms = bodyStms body
    new_inp = (m M.!) <$> new_params
    new_inp' = (m M.!) <$> new_params'
    new_params = filter ((`nameIn` deps) . paramName) pars
    new_params' = filter ((`nameIn` deps') . paramName) pars
    auxiliary = (\(a, b, c, d) -> (mconcat a, b, c, d)) . L.unzip4
    ((deps, new_res, new_ts, new_out), (deps', new_res', new_ts', new_out')) =
      bimap auxiliary auxiliary
        . L.partition (namesIntersect (namesFromList names) . (\(a, _, _, _) -> a))
        $ L.zip4 par_deps (bodyResult body) (lambdaReturnType lam) out

-- | Check that two scremas are fusible if they are give back the
-- producer scremas post lambda that has been split into the scan
-- lambda and map lambda. It is not fusible if inputs and outputs are
-- being transformed and if the producers scan result is used for the
-- consumers scans or reduces.
fusible ::
  (MonadFail m) =>
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  m ()
fusible inp_p form_p out_p inp_c form_c out_c = do
  ((_, post_scan_p, _), _) <-
    splitLambdaByPar post_scan_pars_p inp_p post_p out_c
  let post_scan_res_p = bodyResult $ lambdaBody post_scan_p
      forbidden_p = namesFromList $ resToOut out_p post_p <$> post_scan_res_p
      is_fusible =
        fuseIsVarish inp_c out_p
          && not (forbidden_c `namesIntersect` forbidden_p)
  unless is_fusible (fail "Scremas are not fusible.")
  where
    pre_pars_c = oneName . paramName <$> lambdaParams pre_c
    (pre_scan_deps_c, pre_red_deps_c, _) =
      splitAt3 num_scan_c num_red_c $
        lambdaDependencies mempty pre_c pre_pars_c
    forbidden_c =
      namesFromList
        . mapMaybe (fmap SOAC.inputArray . parToInp inp_c pre_c)
        . namesToList
        $ mconcat (pre_scan_deps_c <> pre_red_deps_c)
    pre_c = scremaLambda form_c
    post_p = scremaPostLambda form_p
    post_scan_pars_p = take num_scan_p $ paramName <$> lambdaParams post_p
    num_scan_c = scanResults $ scremaScans form_c
    num_red_c = redResults $ scremaReduces form_c
    num_scan_p = scanResults $ scremaScans form_p

-- | Given two scremas that are fusible, fuse them into a super
-- screma. This is fused but work will have to be moved around in the
-- super screma for it to become a screma.
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
fuseSuperScrema w inp_p' form_p' out_p inp_c' form_c' out_c = do
  let (inp_p, form_p) = dedupInput inp_p' form_p'
      (inp_c, form_c) = dedupInput inp_c' form_c'
      inp_c_real_map = map (not . inputFromOutput) inp_c
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
              lambdaReturnType (scremaLambda form_c)
                <> lambdaReturnType (scremaPostLambda form_p),
            lambdaBody =
              mkBody
                ( bodyStms (lambdaBody (scremaPostLambda form_p))
                    <> composeBinds
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

  post_forward_params <- forM
    ( zip
        (bodyResult (lambdaBody (scremaPostLambda form_p)))
        (lambdaReturnType (scremaPostLambda form_p))
    )
    $ \(res, t) ->
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
      out_red_p <> out_red_c <> out_post_c <> out_post_p
    )
  where
    inputFromOutput inp = SOAC.inputArray inp `elem` out_p

-- | Moves the last scan and reduce from the super screma to the top
-- of the super screma.
moveRedScanSuperScrema ::
  (MonadFail m, MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
moveRedScanSuperScrema super_screma = do
  ((scan_red_inp_c, scan_red_lam', _), (_, map_lam', _)) <-
    splitAtLambdaByRes
      (scanResults scan' + redResults red')
      inp_c
      lam'
      (replicate (length $ lambdaReturnType lam') ())
  renamed_scan_red_lam' <- renameLambda scan_red_lam'
  let (scan_res', red_res') =
        splitAt (scanResults scan')
          . bodyResult
          $ lambdaBody renamed_scan_red_lam'
      (scan_ts', red_ts') =
        splitAt (scanResults scan') $
          lambdaReturnType renamed_scan_red_lam'
      binds = composeBinds lam out_p scan_red_inp_c renamed_scan_red_lam'
      stms' = bodyStms $ lambdaBody renamed_scan_red_lam'
      new_scan = scan <> scan'
      new_red = red <> red'
      new_ts = scan_ts <> scan_ts' <> red_ts <> red_ts' <> map_ts
      new_pars = lambdaParams lam
      new_res = scan_res <> scan_res' <> red_res <> red_res' <> map_res
      new_body = mkBody (stms <> binds <> stms') new_res
      new_lam = eliminateByRes $ Lambda new_pars new_ts new_body
      (scan_pars', map_pars') =
        splitAt (scanResults scan) (lambdaParams lam')

  extra_scan_pars' <- mapM (newParam "x") scan_ts'

  let new_pars' = scan_pars' <> extra_scan_pars' <> map_pars'
      new_ts' = scan_ts' <> lambdaReturnType map_lam'
      new_stms' = bodyStms $ lambdaBody map_lam'
      new_res' =
        varsRes (map paramName extra_scan_pars')
          <> bodyResult (lambdaBody map_lam')
      new_body' = mkBody new_stms' new_res'
      new_lam' = eliminateByRes $ Lambda new_pars' new_ts' new_body'

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

    SuperScrema w inp lam scan red lam' scan' red' lam'' =
      super_screma

-- | Moves all work done in the last lambda to middle lambda of the
-- super screma.
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
      binds = composeBinds lam' out_p inp_c temp_lam''
      stms' = bodyStms $ lambdaBody lam'
      stms'' = bodyStms $ lambdaBody temp_lam''
      new_stms = stms' <> binds <> stms''
      new_res = bodyResult $ lambdaBody temp_lam''
      new_body = mkBody new_stms new_res
      new_lam' = eliminateByRes $ Lambda new_pars new_ts new_body

  new_lam'' <- mkIdentityLambda $ lambdaReturnType lam''
  pure $
    SuperScrema w inp lam scan red new_lam' [] [] new_lam''
moveLastSuperScrema _ =
  error "moveLastSuperScrema must not have any scans or reduce operation in the end."

-- | Find all Accumulator parameters.
parAccs :: Lambda SOACS -> [Type]
parAccs = filter isAcc . map typeOf . lambdaParams

-- | Find all Accumulator results.
resAccs :: Lambda SOACS -> [Type]
resAccs = filter isAcc . lambdaReturnType

-- | Check if the lambda parameters have overlapping accumulators.
parAccsOverlap :: Lambda SOACS -> Lambda SOACS -> Bool
parAccsOverlap lam = any (`elem` accs) . parAccs
  where
    accs = parAccs lam

-- | Check if the lambdas result have overlapping accumulators.
resAccsOverlap :: Lambda SOACS -> Lambda SOACS -> Bool
resAccsOverlap lam = any (`elem` accs) . resAccs
  where
    accs = resAccs lam

-- | Check if the lambdas have parameters that overlap due to
-- consumption.
consumedOverlap :: Lambda SOACS -> Lambda SOACS -> Bool
consumedOverlap =
  on namesIntersect (consumedByLambda . analyseLambda mempty)

-- | Split a lambda at some index for the result and construct two
-- lambda which do not have overlapping results but may have
-- overlapping parameters.
splitAtLambdaByRes ::
  (MonadFail m) =>
  Int ->
  [inp] ->
  Lambda SOACS ->
  [out] ->
  m (([inp], Lambda SOACS, [out]), ([inp], Lambda SOACS, [out]))
splitAtLambdaByRes i inp lam out = do
  when
    (consumedOverlap new_lam new_lam')
    (fail "Can not fuse due to overlap consumption in pre and post.")
  when
    (parAccsOverlap new_lam new_lam')
    (fail "Can not fuse due to overlap in parameter accumalators.")
  pure ((new_inp, new_lam, new_out), (new_inp', new_lam', new_out'))
  where
    new_lam = Lambda new_pars new_ts new_body
    new_lam' = Lambda new_pars' new_ts' new_body'
    pars = lambdaParams lam
    stms = bodyStms $ lambdaBody lam
    new_body = mkBody (eliminate (freeIn new_res) stms) new_res
    new_body' = mkBody (eliminate (freeIn new_res') stms) new_res'
    inBody body = (`nameIn` freeIn body) . paramName . fst
    removePars body = unzip $ filter (inBody body) $ zip pars inp
    (new_pars, new_inp) = removePars new_body
    (new_pars', new_inp') = removePars new_body'
    (new_res, new_res') = splitAt i $ bodyResult $ lambdaBody lam
    (new_ts, new_ts') = splitAt i $ lambdaReturnType lam
    (new_out, new_out') = splitAt i out

-- | Create a mapping from lambda results expression to their output
-- array.
resToOut :: [VName] -> Lambda SOACS -> SubExpRes -> VName
resToOut out lam = (m M.!)
  where
    m = M.fromList $ flip zip out $ bodyResult $ lambdaBody lam

-- | Create a mapping from lambda parameter names to their input
-- array.
parToInp :: [SOAC.Input] -> Lambda SOACS -> VName -> Maybe SOAC.Input
parToInp inp lam = flip M.lookup m
  where
    m = M.fromList $ flip zip inp $ paramName <$> lambdaParams lam

-- | Turn a SuperScrema into Screma.
toScrema ::
  SuperScrema SOACS ->
  ([SOAC.Input], ScremaForm SOACS)
toScrema (SuperScrema _ inp lam scan red lam' _ _ _) =
  (inp, ScremaForm lam scan red lam')

-- | Try to force the post lambda to be an indentity lambda.
tryIdentityPost ::
  (MonadFreshNames m) => ScremaForm SOACS -> m (ScremaForm SOACS)
tryIdentityPost (ScremaForm pre_lam [] reds post_lam)
  | not $ isIdentityLambda post_lam = do
      new_post_lam <- mkIdentityLambda $ lambdaReturnType post_lam
      pure (ScremaForm new_pre_lam [] reds new_post_lam)
  where
    out_p = [0 .. length (bodyResult $ lambdaBody pre_lam) - 1]
    inp_c = drop (redResults reds) out_p
    binds = composeBinds pre_lam out_p inp_c post_lam
    pre_stms = bodyStms $ lambdaBody pre_lam
    post_stms = bodyStms $ lambdaBody post_lam
    post_res = bodyResult $ lambdaBody post_lam
    pre_res = bodyResult $ lambdaBody pre_lam
    post_ts = lambdaReturnType post_lam
    pre_ts = lambdaReturnType pre_lam
    res = take (redResults reds) pre_res <> post_res
    ts = take (redResults reds) pre_ts <> post_ts
    new_pre_lam =
      Lambda
        { lambdaParams = lambdaParams pre_lam,
          lambdaReturnType = ts,
          lambdaBody = mkBody (pre_stms <> binds <> post_stms) res
        }
tryIdentityPost form = pure form

-- Tries to fuse two Scremas into one.
fuseScrema ::
  (MonadFail m, MonadFreshNames m, HasScope SOACS m) =>
  SubExp ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  [SOAC.Input] ->
  ScremaForm SOACS ->
  [VName] ->
  m ([SOAC.Input], ScremaForm SOACS, [VName])
fuseScrema w inp_p form_p out_p inp_c form_c out_c = do
  fusible inp_p form_p out_p inp_c form_c out_c
  (super_screma, new_out) <- fuseSuperScrema w inp_p form_p out_p inp_c form_c out_c
  (new_inp, form') <-
    fmap (second prunePreLambdaResults . toScrema) $
      moveRedScanSuperScrema super_screma
        >>= moveLastSuperScrema
  form <- tryIdentityPost form'
  pure (new_inp, form, new_out)
