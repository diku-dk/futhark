module Futhark.Optimise.Fusion.Screma
  ( splitLambdaByPar,
    fuseScrema,
    fuseSuperScrema,
    SuperScrema (..),
    moveRedScanSuperScrema,
    moveLastSuperScrema,
    moveMidSuperScrema,
    simplifySuperScrema,
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

-- | Eliminate statements if it is not an dependency used to form the
-- names given.
eliminate :: (Buildable rep) => Names -> Stms rep -> Stms rep
eliminate = auxiliary (stmsFromList [])
  where
    auxiliary stms' deps stms
      | Just (stms'', stm@(Let v aux e)) <- stmsLast stms =
          if namesIntersect deps $ namesFromList $ patNames v
            then
              auxiliary (oneStm stm <> stms') (freeIn (aux, e) <> deps) stms''
            else
              auxiliary stms' deps stms''
      | otherwise = stms'

-- | Eliminate statements inside a lambda if they are not used to
-- compute the result.
eliminateByRes :: (Buildable rep) => Lambda rep -> Lambda rep
eliminateByRes lam = lam {lambdaBody = mkBody new_stms res}
  where
    res = bodyResult $ lambdaBody lam
    stms = bodyStms $ lambdaBody lam
    new_stms = eliminate (freeIn res) stms

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

dedupInput ::
  [SOAC.Input] ->
  ScremaForm SOACS ->
  ([SOAC.Input], ScremaForm SOACS)
dedupInput inp form =
  (new_inp, form {scremaLambda = new_lam})
  where
    lam = scremaLambda form
    body = lambdaBody lam
    stms = bodyStms body
    res = bodyResult body
    new_body = mkBody (binds <> stms) res
    new_lam = lam {lambdaParams = new_pars, lambdaBody = new_body}
    pars = lambdaParams lam
    auxiliary [] = Nothing
    auxiliary (x : xs) = Just (x, xs)
    pairs = zip inp pars
    (new_inp, new_pars) = unzip $ filter (`elem` keep_pairs) pairs
    keep_pairs = map fst bind_pairs
    bind_pairs =
      mapMaybe auxiliary
        . L.groupBy ((==) `on` fst)
        $ L.sortOn fst pairs
    par_bind_pairs = bimap snd (map snd) <$> bind_pairs
    binds = foldMap mkBinds par_bind_pairs
    mkBinds (par_name, names) =
      stmsFromList $
        map
          ( \name ->
              mkLet [Ident (paramName name) (paramType par_name)]
                . BasicOp
                . SubExp
                . Var
                $ paramName par_name
          )
          names

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
      out_red_p <> out_red_c <> out_post_c <> out_post_p
    )
  where
    inputFromOutput inp = SOAC.inputArray inp `elem` out_p

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
      binds = fuseBinds lam out_p scan_red_inp_c renamed_scan_red_lam'
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
      new_lam' = eliminateByRes $ Lambda new_pars new_ts new_body

  new_lam'' <- mkIdentityLambda $ lambdaReturnType lam''
  pure $
    SuperScrema w inp lam scan red new_lam' [] [] new_lam''
moveLastSuperScrema _ =
  error "moveLastSuperScrema must not have any scans or reduce operation in the end."

numRes :: Lambda rep -> Int
numRes = length . bodyResult . lambdaBody

moveMidSuperScrema ::
  (MonadFail m, MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
moveMidSuperScrema (SuperScrema w inp lam scan red lam' [] [] lam'')
  | not $ isIdentityLambda lam'' = error "moveLastSuperScrema last lambda must be the identity."
  | otherwise = do
      let map_pars =
            drop (scanResults scan + redResults red)
              . map paramName
              $ lambdaParams lam'
      ((inp_c, map_lam, _), _) <-
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
          new_lam = eliminateByRes $ Lambda new_pars new_ts new_body

      forward_params <- mapM (newParam "x") $ lambdaReturnType map_lam_renamed

      let res_mapping =
            M.fromList $
              zip
                (bodyResult $ lambdaBody map_lam)
                (varsRes $ map paramName forward_params)
          new_pars' = lambdaParams lam' <> forward_params
          new_res' =
            map (\r -> fromMaybe r (M.lookup r res_mapping))
              . bodyResult
              $ lambdaBody lam'
          new_ts' = lambdaReturnType lam'
          new_body' = bodyStms $ lambdaBody lam'
          new_lam' = eliminateByRes $ Lambda new_pars' new_ts' (mkBody new_body' new_res')

      pure $
        SuperScrema w inp new_lam scan red new_lam' [] [] lam''
  where
    out_p = [0 .. numRes lam - 1]
    (scan_out, _, map_out) =
      splitAt3 (scanResults scan) (redResults red) out_p
moveMidSuperScrema _ = error "moveMidSuperScrema must not have any scans or reduce operation in the end."

parAccs :: Lambda SOACS -> [Type]
parAccs = filter isAcc . map typeOf . lambdaParams

resAccs :: Lambda SOACS -> [Type]
resAccs = filter isAcc . lambdaReturnType

parAccsOverlap :: Lambda SOACS -> Lambda SOACS -> Bool
parAccsOverlap lam = any (`elem` accs) . parAccs
  where
    accs = parAccs lam

resAccsOverlap :: Lambda SOACS -> Lambda SOACS -> Bool
resAccsOverlap lam = any (`elem` accs) . resAccs
  where
    accs = resAccs lam

consumedOverlap :: Lambda SOACS -> Lambda SOACS -> Bool
consumedOverlap lam lam' =
  on namesIntersect (consumedByLambda . analyseLambda mempty) lam lam'

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

toScrema ::
  SuperScrema SOACS ->
  ([SOAC.Input], ScremaForm SOACS)
toScrema (SuperScrema _ inp lam scan red lam' _ _ _) =
  (inp, ScremaForm lam scan red lam')

simplifySuperScrema ::
  (HasScope SOACS m, MonadFreshNames m) =>
  SuperScrema SOACS ->
  m (SuperScrema SOACS)
simplifySuperScrema (SuperScrema w inp lam scan red lam' scan' red' lam'') =
  SuperScrema w inp
    <$> simplifyLambda lam
    <*> pure scan
    <*> pure red
    <*> simplifyLambda lam'
    <*> pure scan'
    <*> pure red'
    <*> simplifyLambda lam''

tryIdentityPost ::
  (MonadFreshNames m) => ScremaForm SOACS -> m (ScremaForm SOACS)
tryIdentityPost (ScremaForm pre_lam [] [] post_lam)
  | not $ isIdentityLambda post_lam = do
      new_post_lam <- mkIdentityLambda $ lambdaReturnType post_lam
      pure (ScremaForm new_pre_lam [] [] new_post_lam)
  where
    out_p = [0 .. length (bodyResult $ lambdaBody pre_lam) - 1]
    inp_c = out_p
    binds = fuseBinds pre_lam out_p inp_c post_lam
    pre_stms = bodyStms $ lambdaBody pre_lam
    post_stms = bodyStms $ lambdaBody post_lam
    res = bodyResult $ lambdaBody post_lam
    new_pre_lam =
      Lambda
        { lambdaParams = lambdaParams pre_lam,
          lambdaReturnType = lambdaReturnType post_lam,
          lambdaBody = mkBody (pre_stms <> binds <> post_stms) res
        }
tryIdentityPost form = pure form

-- | Remove unused post lambda map parameters as well the
-- corresponding pre lambda results.
removeUnusedMap :: (Buildable rep) => ScremaForm rep -> ScremaForm rep
removeUnusedMap (ScremaForm lam_p scan red lam_c) =
  ScremaForm new_lam_p scan red new_lam_c
  where
    (rest_res_p, map_res_p) =
      splitAt (scanResults scan + redResults red) . bodyResult $ lambdaBody lam_p
    (rest_ts_p, map_ts_p) =
      splitAt (scanResults scan + redResults red) $ lambdaReturnType lam_p
    (scan_pars_c, map_pars_c) =
      splitAt (scanResults scan) $ lambdaParams temp_lam_c
    new_lam_c = temp_lam_c {lambdaParams = scan_pars_c <> new_map_pars_c}
    new_lam_p =
      eliminateByRes $
        lam_p
          { lambdaBody =
              mkBody
                (bodyStms $ lambdaBody lam_p)
                (rest_res_p <> new_map_res_p),
            lambdaReturnType = rest_ts_p <> new_map_ts_p
          }
    (new_map_res_p, new_map_ts_p, new_map_pars_c) =
      unzip3
        . filter (\(_, _, p) -> paramName p `nameIn` deps)
        $ zip3 map_res_p map_ts_p map_pars_c
    temp_lam_c = eliminateByRes lam_c
    deps = freeIn $ lambdaBody temp_lam_c

-- | Remove unused post lambda scan parameters as well the
-- corresponding pre lambda results.
removeUnusedScan :: (Buildable rep) => ScremaForm rep -> ScremaForm rep
removeUnusedScan (ScremaForm lam_p scan red lam_c) =
  ScremaForm new_lam_p new_scan red new_lam_c
  where
    (scan_res_p, rest_res_p) =
      splitAt (scanResults scan) . bodyResult $ lambdaBody lam_p
    (scan_ts_p, rest_ts_p) =
      splitAt (scanResults scan) $ lambdaReturnType lam_p
    (scan_pars_c, map_pars_c) =
      splitAt (scanResults scan) $ lambdaParams temp_lam_c
    new_lam_c = temp_lam_c {lambdaParams = new_scan_pars_c <> map_pars_c}
    new_lam_p =
      eliminateByRes $
        lam_p
          { lambdaBody =
              mkBody
                (bodyStms $ lambdaBody lam_p)
                (new_scan_res_p <> rest_res_p),
            lambdaReturnType = new_scan_ts_p <> rest_ts_p
          }
    (new_scan, new_scan_res_p, new_scan_ts_p, new_scan_pars_c) =
      L.unzip4
        . filter (\(_, _, _, p) -> paramName p `nameIn` deps)
        $ L.zip4 scan scan_res_p scan_ts_p scan_pars_c
    temp_lam_c = eliminateByRes lam_c
    deps = freeIn $ lambdaBody temp_lam_c

removeUnused :: (Buildable rep) => ScremaForm rep -> ScremaForm rep
removeUnused form =
  if form == form' then form' else removeUnused form'
  where
    form' = removeUnusedScan $ removeUnusedMap form

failOnScan :: (MonadFail m) => ScremaForm SOACS -> m ()
failOnScan (ScremaForm _ scan _ _) =
  unless (null scan) (fail "Must be empty scan.")

failOnRed :: (MonadFail m) => ScremaForm SOACS -> m ()
failOnRed (ScremaForm _ _ red _) =
  unless (null red) (fail "Must be empty scan.")

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
  -- failOnScan form_p
  -- failOnScan form_c
  -- failOnRed form_p
  -- failOnRed form_c
  fusible inp_p form_p out_p inp_c form_c out_c
  (super_screma, new_out) <- fuseSuperScrema w inp_p form_p out_p inp_c form_c out_c
  (new_inp, form') <-
    fmap (second removeUnused . toScrema) $
      moveRedScanSuperScrema super_screma
        >>= moveLastSuperScrema
        >>= moveMidSuperScrema
  -- >>= simplifySuperScrema
  form <- tryIdentityPost form'
  pure (new_inp, form, new_out)
