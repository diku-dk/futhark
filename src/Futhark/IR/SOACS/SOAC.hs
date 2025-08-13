{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of /Second-Order Array Combinators/ (SOACs), which are
-- the main form of parallelism in the early stages of the compiler.
module Futhark.IR.SOACS.SOAC
  ( SOAC (..),
    ScremaForm (..),
    HistOp (..),
    Scan (..),
    scanResults,
    singleScan,
    Reduce (..),
    redResults,
    singleReduce,

    -- * Utility
    scremaType,
    soacType,
    typeCheckSOAC,
    mkIdentityLambda,
    isIdentityLambda,
    isNilLambda,
    nilFn,
    maposcanomapSOAC,
    scanomapSOAC,
    redomapSOAC,
    scanSOAC,
    reduceSOAC,
    mapSOAC,
    isScanomapSOAC,
    isRedomapSOAC,
    isScanSOAC,
    isReduceSOAC,
    isMapSOAC,
    ppScrema,
    ppHist,
    ppStream,

    -- * Generic traversal
    SOACMapper (..),
    identitySOACMapper,
    mapSOACM,
    traverseSOACStms,
  )
where

import Control.Category
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.List (intersperse)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.Metrics
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Aliases (Aliases, CanBeAliased (..))
import Futhark.IR.Prop.Aliases
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, maybeNth, splitAt3)
import Futhark.Util.Pretty (Doc, align, comma, commasep, docText, parens, ppTuple', pretty, (<+>), (</>))
import Futhark.Util.Pretty qualified as PP
import Prelude hiding (id, (.))

-- | A second-order array combinator (SOAC).
data SOAC rep
  = Stream SubExp [VName] [SubExp] (Lambda rep)
  | -- | @Hist <length> <input arrays> <dest-arrays-and-ops> <bucket fun>@
    --
    -- The final lambda produces indexes and values for the 'HistOp's.
    Hist SubExp [VName] [HistOp rep] (Lambda rep)
  | -- FIXME: this should not be here
    JVP [SubExp] [SubExp] (Lambda rep)
  | -- FIXME: this should not be here
    VJP [SubExp] [SubExp] (Lambda rep)
  | -- | A combination of scan, reduction, and map.  The first
    -- t'SubExp' is the size of the input arrays.
    Screma SubExp [VName] (ScremaForm rep)
  deriving (Eq, Ord, Show)

-- | Information about computing a single histogram.
data HistOp rep = HistOp
  { histShape :: Shape,
    -- | Race factor @RF@ means that only @1/RF@
    -- bins are used.
    histRaceFactor :: SubExp,
    histDest :: [VName],
    histNeutral :: [SubExp],
    histOp :: Lambda rep
  }
  deriving (Eq, Ord, Show)

-- | The essential parts of a 'Screma' factored out (everything
-- except the input arrays).
data ScremaForm rep = ScremaForm
  { -- | The "main" lambda of the Screma. For a map, this is
    -- equivalent to 'isMapSOAC'. Note that the meaning of the return
    -- value of this lambda depends crucially on exactly which Screma
    -- this is. The parameters will correspond exactly to elements of
    -- the input arrays, however.
    scremaLambda :: Lambda rep,
    scremaScans :: [Scan rep],
    scremaReduces :: [Reduce rep],
    scremaPostLambda :: Lambda rep
  }
  deriving (Eq, Ord, Show)

singleBinOp :: (Buildable rep) => [Lambda rep] -> Lambda rep
singleBinOp lams =
  Lambda
    { lambdaParams = concatMap xParams lams ++ concatMap yParams lams,
      lambdaReturnType = concatMap lambdaReturnType lams,
      lambdaBody =
        mkBody
          (mconcat (map (bodyStms . lambdaBody) lams))
          (concatMap (bodyResult . lambdaBody) lams)
    }
  where
    xParams lam = take (length (lambdaReturnType lam)) (lambdaParams lam)
    yParams lam = drop (length (lambdaReturnType lam)) (lambdaParams lam)

-- | How to compute a single scan result.
data Scan rep = Scan
  { scanLambda :: Lambda rep,
    scanNeutral :: [SubExp]
  }
  deriving (Eq, Ord, Show)

-- | What are the sizes of reduction results produced by these 'Scan's?
scanSizes :: [Scan rep] -> [Int]
scanSizes = map (length . scanNeutral)

-- | How many reduction results are produced by these 'Scan's?
scanResults :: [Scan rep] -> Int
scanResults = sum . scanSizes

-- | Combine multiple scan operators to a single operator.
singleScan :: (Buildable rep) => [Scan rep] -> Scan rep
singleScan scans =
  let scan_nes = concatMap scanNeutral scans
      scan_lam = singleBinOp $ map scanLambda scans
   in Scan scan_lam scan_nes

-- | How to compute a single reduction result.
data Reduce rep = Reduce
  { redComm :: Commutativity,
    redLambda :: Lambda rep,
    redNeutral :: [SubExp]
  }
  deriving (Eq, Ord, Show)

-- | What are the sizes of reduction results produced by these 'Reduce's?
redSizes :: [Reduce rep] -> [Int]
redSizes = map (length . redNeutral)

-- | How many reduction results are produced by these 'Reduce's?
redResults :: [Reduce rep] -> Int
redResults = sum . redSizes

-- | Combine multiple reduction operators to a single operator.
singleReduce :: (Buildable rep) => [Reduce rep] -> Reduce rep
singleReduce reds =
  let red_nes = concatMap redNeutral reds
      red_lam = singleBinOp $ map redLambda reds
   in Reduce (mconcat (map redComm reds)) red_lam red_nes

-- | The types produced by a single 'Screma', given the size of the
-- input array.
scremaType :: SubExp -> ScremaForm rep -> [Type]
scremaType w (ScremaForm _map_lam _scans reds post_lam) =
  red_tps <> fmap (`arrayOfRow` w) (lambdaReturnType post_lam)
  where
    red_tps = concatMap (lambdaReturnType . redLambda) reds

-- | Construct a lambda that takes parameters of the given types and
-- simply returns them unchanged.
mkIdentityLambda ::
  (Buildable rep, MonadFreshNames m) =>
  [Type] ->
  m (Lambda rep)
mkIdentityLambda ts = do
  params <- mapM (newParam "x") ts
  pure
    Lambda
      { lambdaParams = params,
        lambdaBody = mkBody mempty $ varsRes $ map paramName params,
        lambdaReturnType = ts
      }

-- | Is the given lambda an identity lambda?
isIdentityLambda :: Lambda rep -> Bool
isIdentityLambda lam =
  map resSubExp (bodyResult (lambdaBody lam))
    == map (Var . paramName) (lambdaParams lam)

-- | Is the given lambda a nil lambda?
isNilLambda :: Lambda rep -> Bool
isNilLambda lam =
  null (lambdaParams lam)
    && isIdentityLambda lam

-- | A lambda with no parameters that returns no values.
nilFn :: (Buildable rep) => Lambda rep
nilFn = Lambda mempty mempty (mkBody mempty mempty)

-- | Construct a Screma with possibly multiple scans, and
-- the given map function.
scanomapSOAC ::
  (Buildable rep, MonadFreshNames m) =>
  [Scan rep] ->
  Lambda rep ->
  m (ScremaForm rep)
scanomapSOAC scans lam = do
  post_lam <- mkIdentityLambda $ lambdaReturnType lam
  pure $ ScremaForm lam scans [] post_lam

-- | Construct a Screma with possibly multiple scans,
-- the given map function, and a given post lambda.
maposcanomapSOAC ::
  Lambda rep ->
  [Scan rep] ->
  Lambda rep ->
  ScremaForm rep
maposcanomapSOAC post_lam scans lam = ScremaForm lam scans [] post_lam

-- | Construct a Screma with possibly multiple reductions, and
-- the given map function.
redomapSOAC ::
  (Buildable rep) =>
  [Reduce rep] ->
  Lambda rep ->
  ScremaForm rep
redomapSOAC reds lam = ScremaForm lam [] reds nilFn

-- | Construct a Screma with possibly multiple scans, and identity map
-- function.
scanSOAC ::
  (Buildable rep, MonadFreshNames m) =>
  [Scan rep] ->
  m (ScremaForm rep)
scanSOAC scans = scanomapSOAC scans =<< mkIdentityLambda ts
  where
    ts = concatMap (lambdaReturnType . scanLambda) scans

-- | Construct a Screma with possibly multiple reductions, and
-- identity map function.
reduceSOAC ::
  (Buildable rep, MonadFreshNames m) =>
  [Reduce rep] ->
  m (ScremaForm rep)
reduceSOAC reds = redomapSOAC reds <$> mkIdentityLambda ts
  where
    ts = concatMap (lambdaReturnType . redLambda) reds

-- | Construct a Screma corresponding to a map.
mapSOAC ::
  (Buildable rep, MonadFreshNames m) =>
  Lambda rep ->
  m (ScremaForm rep)
mapSOAC lam = do
  post_lam <- mkIdentityLambda $ lambdaReturnType lam
  pure $ ScremaForm lam [] [] post_lam

-- | Does this Screma correspond to a scan-map composition?
isScanomapSOAC :: ScremaForm rep -> Maybe ([Scan rep], Lambda rep)
isScanomapSOAC (ScremaForm map_lam scans reds post_lam) = do
  guard $ null reds
  guard $ not $ null scans
  guard $ isIdentityLambda post_lam
  pure (scans, map_lam)

-- | Does this Screma correspond to pure scan?
isScanSOAC :: ScremaForm rep -> Maybe [Scan rep]
isScanSOAC form = do
  (scans, map_lam) <- isScanomapSOAC form
  guard $ isIdentityLambda map_lam
  pure scans

-- | Does this Screma correspond to a reduce-map composition?
isRedomapSOAC :: ScremaForm rep -> Maybe ([Reduce rep], Lambda rep)
isRedomapSOAC (ScremaForm map_lam scans reds post_lam) = do
  guard $ null scans
  guard $ not $ null reds
  guard $ isNilLambda post_lam
  pure (reds, map_lam)

-- | Does this Screma correspond to a pure reduce?
isReduceSOAC :: ScremaForm rep -> Maybe [Reduce rep]
isReduceSOAC form = do
  (reds, map_lam) <- isRedomapSOAC form
  guard $ isNilLambda map_lam
  pure reds

-- | Does this Screma correspond to a simple map, without any
-- reduction or scan results?
isMapSOAC :: ScremaForm rep -> Maybe (Lambda rep)
isMapSOAC (ScremaForm map_lam scans reds post_lam) = do
  guard $ null scans
  guard $ null reds
  guard $ isIdentityLambda post_lam
  pure map_lam

-- | Like 'Mapper', but just for 'SOAC's.
data SOACMapper frep trep m = SOACMapper
  { mapOnSOACSubExp :: SubExp -> m SubExp,
    mapOnSOACLambda :: Lambda frep -> m (Lambda trep),
    mapOnSOACVName :: VName -> m VName
  }

-- | A mapper that simply returns the SOAC verbatim.
identitySOACMapper :: forall rep m. (Monad m) => SOACMapper rep rep m
identitySOACMapper =
  SOACMapper
    { mapOnSOACSubExp = pure,
      mapOnSOACLambda = pure,
      mapOnSOACVName = pure
    }

-- | Map a monadic action across the immediate children of a
-- SOAC.  The mapping does not descend recursively into subexpressions
-- and is done left-to-right.
mapSOACM ::
  (Monad m) =>
  SOACMapper frep trep m ->
  SOAC frep ->
  m (SOAC trep)
mapSOACM tv (JVP args vec lam) =
  JVP
    <$> mapM (mapOnSOACSubExp tv) args
    <*> mapM (mapOnSOACSubExp tv) vec
    <*> mapOnSOACLambda tv lam
mapSOACM tv (VJP args vec lam) =
  VJP
    <$> mapM (mapOnSOACSubExp tv) args
    <*> mapM (mapOnSOACSubExp tv) vec
    <*> mapOnSOACLambda tv lam
mapSOACM tv (Stream size arrs accs lam) =
  Stream
    <$> mapOnSOACSubExp tv size
    <*> mapM (mapOnSOACVName tv) arrs
    <*> mapM (mapOnSOACSubExp tv) accs
    <*> mapOnSOACLambda tv lam
mapSOACM tv (Hist w arrs ops bucket_fun) =
  Hist
    <$> mapOnSOACSubExp tv w
    <*> mapM (mapOnSOACVName tv) arrs
    <*> mapM
      ( \(HistOp shape rf op_arrs nes op) ->
          HistOp
            <$> mapM (mapOnSOACSubExp tv) shape
            <*> mapOnSOACSubExp tv rf
            <*> mapM (mapOnSOACVName tv) op_arrs
            <*> mapM (mapOnSOACSubExp tv) nes
            <*> mapOnSOACLambda tv op
      )
      ops
    <*> mapOnSOACLambda tv bucket_fun
mapSOACM tv (Screma w arrs (ScremaForm map_lam scans reds post_lam)) =
  Screma
    <$> mapOnSOACSubExp tv w
    <*> mapM (mapOnSOACVName tv) arrs
    <*> ( ScremaForm
            <$> mapOnSOACLambda tv map_lam
            <*> mapM (mapOnSOACScan tv) scans
            <*> mapM (mapOnSOACReduce tv) reds
            <*> mapOnSOACLambda tv post_lam
        )

mapOnSOACScan :: (Monad m) => SOACMapper frep trep m -> Scan frep -> m (Scan trep)
mapOnSOACScan tv (Scan red_lam red_nes) =
  Scan
    <$> mapOnSOACLambda tv red_lam
    <*> mapM (mapOnSOACSubExp tv) red_nes

mapOnSOACReduce :: (Monad m) => SOACMapper frep trep m -> Reduce frep -> m (Reduce trep)
mapOnSOACReduce tv (Reduce comm red_lam red_nes) =
  Reduce comm
    <$> mapOnSOACLambda tv red_lam
    <*> mapM (mapOnSOACSubExp tv) red_nes

-- | A helper for defining 'TraverseOpStms'.
traverseSOACStms :: (Monad m) => OpStmsTraverser m (SOAC rep) rep
traverseSOACStms f = mapSOACM mapper
  where
    mapper = identitySOACMapper {mapOnSOACLambda = traverseLambdaStms f}

instance (ASTRep rep) => FreeIn (Scan rep) where
  freeIn' (Scan lam ne) = freeIn' lam <> freeIn' ne

instance (ASTRep rep) => FreeIn (Reduce rep) where
  freeIn' (Reduce _ lam ne) = freeIn' lam <> freeIn' ne

instance (ASTRep rep) => FreeIn (ScremaForm rep) where
  freeIn' (ScremaForm scans reds lam post_lam) =
    freeIn' scans <> freeIn' reds <> freeIn' lam <> freeIn' post_lam

instance (ASTRep rep) => FreeIn (HistOp rep) where
  freeIn' (HistOp w rf dests nes lam) =
    freeIn' w <> freeIn' rf <> freeIn' dests <> freeIn' nes <> freeIn' lam

instance (ASTRep rep) => FreeIn (SOAC rep) where
  freeIn' = flip execState mempty . mapSOACM free
    where
      walk f x = modify (<> f x) >> pure x
      free =
        SOACMapper
          { mapOnSOACSubExp = walk freeIn',
            mapOnSOACLambda = walk freeIn',
            mapOnSOACVName = walk freeIn'
          }

instance (ASTRep rep) => Substitute (SOAC rep) where
  substituteNames subst =
    runIdentity . mapSOACM substitute
    where
      substitute =
        SOACMapper
          { mapOnSOACSubExp = pure . substituteNames subst,
            mapOnSOACLambda = pure . substituteNames subst,
            mapOnSOACVName = pure . substituteNames subst
          }

instance (ASTRep rep) => Rename (SOAC rep) where
  rename = mapSOACM renamer
    where
      renamer = SOACMapper rename rename rename

-- | The type of a SOAC.
soacType :: (Typed (LParamInfo rep)) => SOAC rep -> [Type]
soacType (JVP _ _ lam) =
  lambdaReturnType lam ++ lambdaReturnType lam
soacType (VJP _ _ lam) =
  lambdaReturnType lam ++ map paramType (lambdaParams lam)
soacType (Stream outersize _ accs lam) =
  map (substNamesInType substs) rtp
  where
    nms = map paramName $ take (1 + length accs) params
    substs = M.fromList $ zip nms (outersize : accs)
    Lambda params rtp _ = lam
soacType (Hist _ _ ops _bucket_fun) = do
  op <- ops
  map (`arrayOfShape` histShape op) (lambdaReturnType $ histOp op)
soacType (Screma w _arrs form) =
  scremaType w form

instance TypedOp SOAC where
  opType = pure . staticShapes . soacType

instance AliasedOp SOAC where
  opAliases = map (const mempty) . soacType

  consumedInOp JVP {} = mempty
  consumedInOp VJP {} = mempty
  -- Only map functions can consume anything.  The operands to scan
  -- and reduce functions are always considered "fresh".
  consumedInOp (Screma _ arrs (ScremaForm map_lam _ _ _)) =
    mapNames consumedArray $ consumedByLambda map_lam
    where
      consumedArray v = fromMaybe v $ lookup v params_to_arrs
      params_to_arrs = zip (map paramName $ lambdaParams map_lam) arrs
  consumedInOp (Stream _ arrs accs lam) =
    namesFromList $ subExpVars $ map consumedArray $ namesToList $ consumedByLambda lam
    where
      consumedArray v = fromMaybe (Var v) $ lookup v paramsToInput
      -- Drop the chunk parameter, which cannot alias anything.
      paramsToInput =
        zip (map paramName $ drop 1 $ lambdaParams lam) (accs ++ map Var arrs)
  consumedInOp (Hist _ _ ops _) =
    namesFromList $ concatMap histDest ops

mapHistOp ::
  (Lambda frep -> Lambda trep) ->
  HistOp frep ->
  HistOp trep
mapHistOp f (HistOp w rf dests nes lam) =
  HistOp w rf dests nes $ f lam

instance CanBeAliased SOAC where
  addOpAliases aliases (JVP args vec lam) =
    JVP args vec (Alias.analyseLambda aliases lam)
  addOpAliases aliases (VJP args vec lam) =
    VJP args vec (Alias.analyseLambda aliases lam)
  addOpAliases aliases (Stream size arr accs lam) =
    Stream size arr accs $ Alias.analyseLambda aliases lam
  addOpAliases aliases (Hist w arrs ops bucket_fun) =
    Hist
      w
      arrs
      (map (mapHistOp (Alias.analyseLambda aliases)) ops)
      (Alias.analyseLambda aliases bucket_fun)
  addOpAliases aliases (Screma w arrs (ScremaForm map_lam scans reds post_lam)) =
    Screma w arrs $
      ScremaForm
        (Alias.analyseLambda aliases map_lam)
        (map onScan scans)
        (map onRed reds)
        (Alias.analyseLambda aliases post_lam)
    where
      onRed red = red {redLambda = Alias.analyseLambda aliases $ redLambda red}
      onScan scan = scan {scanLambda = Alias.analyseLambda aliases $ scanLambda scan}

instance IsOp SOAC where
  safeOp _ = False
  cheapOp _ = False
  opDependencies (Stream w arrs accs lam) =
    let accs_deps = map depsOf' accs
        arrs_deps = depsOfArrays w arrs
     in lambdaDependencies mempty lam (arrs_deps <> accs_deps)
  opDependencies (Hist w arrs ops lam) =
    let bucket_fun_deps' = lambdaDependencies mempty lam (depsOfArrays w arrs)
        -- Bucket function results are indices followed by values.
        -- Reshape this to align with list of histogram operations.
        ranks = map (shapeRank . histShape) ops
        value_lengths = map (length . histNeutral) ops
        (indices, values) = splitAt (sum ranks) bucket_fun_deps'
        bucket_fun_deps =
          zipWith
            concatIndicesToEachValue
            (chunks ranks indices)
            (chunks value_lengths values)
     in mconcat $ zipWith (zipWith (<>)) bucket_fun_deps (map depsOfHistOp ops)
    where
      depsOfHistOp (HistOp dest_shape rf dests nes op) =
        let shape_deps = depsOfShape dest_shape
            in_deps = map (\vn -> oneName vn <> shape_deps <> depsOf' rf) dests
         in reductionDependencies mempty op nes in_deps
      -- A histogram operation may use the same index for multiple values.
      concatIndicesToEachValue is vs =
        let is_flat = mconcat is
         in map (is_flat <>) vs
  opDependencies (JVP args vec lam) =
    mconcat $
      replicate 2 $
        lambdaDependencies mempty lam $
          zipWith (<>) (map depsOf' args) (map depsOf' vec)
  opDependencies (VJP args vec lam) =
    lambdaDependencies
      mempty
      lam
      (zipWith (<>) (map depsOf' args) (map depsOf' vec))
      <> map (const $ freeIn args <> freeIn lam) (lambdaParams lam)
  opDependencies (Screma w arrs (ScremaForm map_lam scans reds post_lam)) =
    let (scans_in, reds_in, map_deps) =
          splitAt3 (scanResults scans) (redResults reds) $
            lambdaDependencies mempty map_lam (depsOfArrays w arrs)
        scans_deps =
          concatMap depsOfScan (zip scans $ chunks (scanSizes scans) scans_in)
        reds_deps =
          concatMap depsOfRed (zip reds $ chunks (redSizes reds) reds_in)
     in lambdaDependencies mempty post_lam (scans_deps <> reds_deps <> map_deps)
    where
      depsOfScan (Scan lam nes, deps_in) =
        reductionDependencies mempty lam nes deps_in
      depsOfRed (Reduce _ lam nes, deps_in) =
        reductionDependencies mempty lam nes deps_in

substNamesInType :: M.Map VName SubExp -> Type -> Type
substNamesInType _ t@Prim {} = t
substNamesInType _ t@Acc {} = t
substNamesInType _ (Mem space) = Mem space
substNamesInType subs (Array btp shp u) =
  let shp' = Shape $ map (substNamesInSubExp subs) (shapeDims shp)
   in Array btp shp' u

substNamesInSubExp :: M.Map VName SubExp -> SubExp -> SubExp
substNamesInSubExp _ e@(Constant _) = e
substNamesInSubExp subs (Var idd) =
  M.findWithDefault (Var idd) idd subs

instance CanBeWise SOAC where
  addOpWisdom = runIdentity . mapSOACM (SOACMapper pure (pure . informLambda) pure)

instance (RepTypes rep) => ST.IndexOp (SOAC rep) where
  indexOp vtable k soac [i] = do
    (lam, se, arr_params, arrs) <- lambdaAndSubExp soac
    let arr_indexes = M.fromList $ catMaybes $ zipWith arrIndex arr_params arrs
        arr_indexes' = foldl expandPrimExpTable arr_indexes $ bodyStms $ lambdaBody lam
    case se of
      SubExpRes _ (Var v) -> uncurry (flip ST.Indexed) <$> M.lookup v arr_indexes'
      _ -> Nothing
    where
      lambdaAndSubExp (Screma _ arrs (ScremaForm map_lam scans reds post_lam)) = do
        -- UNSURE_IF_CORRECT
        guard $ isIdentityLambda post_lam
        nthMapOut (scanResults scans + redResults reds) map_lam arrs
      lambdaAndSubExp _ =
        Nothing

      nthMapOut num_accs lam arrs = do
        se <- maybeNth (num_accs + k) $ bodyResult $ lambdaBody lam
        pure (lam, se, drop num_accs $ lambdaParams lam, arrs)

      arrIndex p arr = do
        ST.Indexed cs pe <- ST.index' arr [i] vtable
        pure (paramName p, (pe, cs))

      expandPrimExpTable table stm
        | [v] <- patNames $ stmPat stm,
          Just (pe, cs) <-
            runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm,
          all (`ST.elem` vtable) (unCerts $ stmCerts stm) =
            M.insert v (pe, stmCerts stm <> cs) table
        | otherwise =
            table

      asPrimExp table v
        | Just (e, cs) <- M.lookup v table = tell cs >> pure e
        | Just (Prim pt) <- ST.lookupType v vtable =
            pure $ LeafExp v pt
        | otherwise = lift Nothing
  indexOp _ _ _ _ = Nothing

-- | Type-check a SOAC.
typeCheckSOAC :: (TC.Checkable rep) => SOAC (Aliases rep) -> TC.TypeM rep ()
typeCheckSOAC (VJP args vec lam) = do
  args' <- mapM TC.checkArg args
  TC.checkLambda lam $ map TC.noArgAliases args'
  vec_ts <- mapM TC.checkSubExp vec
  unless (vec_ts == lambdaReturnType lam) $
    TC.bad . TC.TypeError . docText $
      "Return type"
        </> PP.indent 2 (pretty (lambdaReturnType lam))
        </> "does not match type of seed vector"
        </> PP.indent 2 (pretty vec_ts)
typeCheckSOAC (JVP args vec lam) = do
  args' <- mapM TC.checkArg args
  TC.checkLambda lam $ map TC.noArgAliases args'
  vec_ts <- mapM TC.checkSubExp vec
  unless (vec_ts == map TC.argType args') $
    TC.bad . TC.TypeError . docText $
      "Parameter type"
        </> PP.indent 2 (pretty $ map TC.argType args')
        </> "does not match type of seed vector"
        </> PP.indent 2 (pretty vec_ts)
typeCheckSOAC (Stream size arrexps accexps lam) = do
  TC.require [Prim int64] size
  accargs <- mapM TC.checkArg accexps
  arrargs <- mapM lookupType arrexps
  _ <- TC.checkSOACArrayArgs size arrexps
  chunk <- case lambdaParams lam of
    chunk : _ -> pure chunk
    [] -> TC.bad $ TC.TypeError "Stream lambda without parameters."
  let asArg t = (t, mempty)
      inttp = Prim int64
      lamarrs' = map (`setOuterSize` Var (paramName chunk)) arrargs
      acc_len = length accexps
      lamrtp = take acc_len $ lambdaReturnType lam
  unless (map TC.argType accargs == lamrtp) $
    TC.bad . TC.TypeError $
      "Stream with inconsistent accumulator type in lambda."
  -- just get the dflow of lambda on the fakearg, which does not alias
  -- arr, so we can later check that aliases of arr are not used inside lam.
  let fake_lamarrs' = map asArg lamarrs'
  TC.checkLambda lam $ asArg inttp : accargs ++ fake_lamarrs'
typeCheckSOAC (Hist w arrs ops bucket_fun) = do
  TC.require [Prim int64] w

  -- Check the operators.
  forM_ ops $ \(HistOp dest_shape rf dests nes op) -> do
    nes' <- mapM TC.checkArg nes
    mapM_ (TC.require [Prim int64]) dest_shape
    TC.require [Prim int64] rf

    -- Operator type must match the type of neutral elements.
    TC.checkLambda op $ map TC.noArgAliases $ nes' ++ nes'
    let nes_t = map TC.argType nes'
    unless (nes_t == lambdaReturnType op) $
      TC.bad . TC.TypeError $
        "Operator has return type "
          <> prettyTuple (lambdaReturnType op)
          <> " but neutral element has type "
          <> prettyTuple nes_t

    -- Arrays must have proper type.
    forM_ (zip nes_t dests) $ \(t, dest) -> do
      TC.requireI [t `arrayOfShape` dest_shape] dest
      TC.consume =<< TC.lookupAliases dest

  -- Types of input arrays must equal parameter types for bucket function.
  img' <- TC.checkSOACArrayArgs w arrs
  TC.checkLambda bucket_fun img'

  -- Return type of bucket function must be an index for each
  -- operation followed by the values to write.
  nes_ts <- concat <$> mapM (mapM subExpType . histNeutral) ops
  let bucket_ret_t =
        concatMap ((`replicate` Prim int64) . shapeRank . histShape) ops
          ++ nes_ts
  unless (bucket_ret_t == lambdaReturnType bucket_fun) $
    TC.bad . TC.TypeError $
      "Bucket function has return type "
        <> prettyTuple (lambdaReturnType bucket_fun)
        <> " but should have type "
        <> prettyTuple bucket_ret_t
typeCheckSOAC (Screma w arrs (ScremaForm map_lam scans reds post_lam)) = do
  TC.require [Prim int64] w
  arrs' <- TC.checkSOACArrayArgs w arrs
  TC.checkLambda map_lam arrs'
  scan_nes' <- concat <$> mapM typeCheckScan scans
  red_nes' <- concat <$> mapM typeCheckReduce reds
  let map_lam_ts = lambdaReturnType map_lam
  unless
    ( take (length scan_nes' + length red_nes') map_lam_ts
        == map TC.argType (scan_nes' ++ red_nes')
    )
    . TC.bad
    . TC.TypeError
    $ "Map function return type "
      <> prettyTuple map_lam_ts
      <> " wrong for given scan and reduction functions."
  let (scan_ts, _, map_ts) =
        splitAt3 (length scan_nes') (length red_nes') map_lam_ts
      post_lam_args = (,mempty) <$> (scan_ts <> map_ts)
  TC.checkLambda post_lam post_lam_args

typeCheckScan :: (TC.Checkable rep) => Scan (Aliases rep) -> TC.TypeM rep [(Type, Names)]
typeCheckScan (Scan scan_lam scan_nes) = do
  scan_nes' <- mapM TC.checkArg scan_nes
  let scan_t = map TC.argType scan_nes'
  TC.checkLambda scan_lam $ map TC.noArgAliases $ scan_nes' ++ scan_nes'
  unless (scan_t == lambdaReturnType scan_lam) $
    TC.bad . TC.TypeError $
      "Scan function returns type "
        <> prettyTuple (lambdaReturnType scan_lam)
        <> " but neutral element has type "
        <> prettyTuple scan_t
  pure scan_nes'

typeCheckReduce :: (TC.Checkable rep) => Reduce (Aliases rep) -> TC.TypeM rep [(Type, Names)]
typeCheckReduce (Reduce _ red_lam red_nes) = do
  red_nes' <- mapM TC.checkArg red_nes
  let red_t = map TC.argType red_nes'
  TC.checkLambda red_lam $ map TC.noArgAliases $ red_nes' ++ red_nes'
  unless (red_t == lambdaReturnType red_lam) $
    TC.bad . TC.TypeError $
      "Reduce function returns type "
        <> prettyTuple (lambdaReturnType red_lam)
        <> " but neutral element has type "
        <> prettyTuple red_t
  pure red_nes'

instance RephraseOp SOAC where
  rephraseInOp r (VJP args vec lam) =
    VJP args vec <$> rephraseLambda r lam
  rephraseInOp r (JVP args vec lam) =
    JVP args vec <$> rephraseLambda r lam
  rephraseInOp r (Stream w arrs acc lam) =
    Stream w arrs acc <$> rephraseLambda r lam
  rephraseInOp r (Hist w arrs ops lam) =
    Hist w arrs <$> mapM onOp ops <*> rephraseLambda r lam
    where
      onOp (HistOp dest_shape rf dests nes op) =
        HistOp dest_shape rf dests nes <$> rephraseLambda r op
  rephraseInOp r (Screma w arrs (ScremaForm lam scans red post_lam)) =
    Screma w arrs
      <$> ( ScremaForm
              <$> rephraseLambda r lam
              <*> mapM (rephraseScan r) scans
              <*> mapM (rephraseRed r) red
              <*> rephraseLambda r post_lam
          )

rephraseRed :: (Monad m) => Rephraser m from to -> Reduce from -> m (Reduce to)
rephraseRed r (Reduce comm op nes) =
  Reduce comm <$> rephraseLambda r op <*> pure nes

rephraseScan :: (Monad m) => Rephraser m from to -> Scan from -> m (Scan to)
rephraseScan r (Scan op nes) =
  Scan <$> rephraseLambda r op <*> pure nes

instance (OpMetrics (Op rep)) => OpMetrics (SOAC rep) where
  opMetrics (VJP _ _ lam) =
    inside "VJP" $ lambdaMetrics lam
  opMetrics (JVP _ _ lam) =
    inside "JVP" $ lambdaMetrics lam
  opMetrics (Stream _ _ _ lam) =
    inside "Stream" $ lambdaMetrics lam
  opMetrics (Hist _ _ ops bucket_fun) =
    inside "Hist" $ mapM_ (lambdaMetrics . histOp) ops >> lambdaMetrics bucket_fun
  opMetrics (Screma _ _ (ScremaForm map_lam scans reds post_lam)) =
    inside "Screma" $ do
      lambdaMetrics map_lam
      mapM_ (lambdaMetrics . scanLambda) scans
      mapM_ (lambdaMetrics . redLambda) reds
      lambdaMetrics post_lam

instance (PrettyRep rep) => PP.Pretty (SOAC rep) where
  pretty (VJP args vec lam) =
    "vjp"
      <> parens
        ( PP.align $
            PP.braces (commasep $ map pretty args)
              <> comma </> PP.braces (commasep $ map pretty vec)
              <> comma </> pretty lam
        )
  pretty (JVP args vec lam) =
    "jvp"
      <> parens
        ( PP.align $
            PP.braces (commasep $ map pretty args)
              <> comma </> PP.braces (commasep $ map pretty vec)
              <> comma </> pretty lam
        )
  pretty (Stream size arrs acc lam) =
    ppStream size arrs acc lam
  pretty (Hist w arrs ops bucket_fun) =
    ppHist w arrs ops bucket_fun
  pretty (Screma w arrs screma)
    | Just map_lam <- isMapSOAC screma =
        "map"
          <> (parens . align)
            ( pretty w
                <> comma </> ppTuple' (map pretty arrs)
                <> comma </> pretty map_lam
                <> comma </> pretty (scremaPostLambda screma)
            )
    | Just (reds, map_lam) <- isRedomapSOAC screma =
        "redomap"
          <> (parens . align)
            ( pretty w
                <> comma </> ppTuple' (map pretty arrs)
                <> comma </> pretty map_lam
                <> comma
                  </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map pretty reds)
                <> comma </> pretty (scremaPostLambda screma)
            )
    | Just (scans, map_lam) <- isScanomapSOAC screma =
        "scanomap"
          <> (parens . align)
            ( pretty w
                <> comma </> ppTuple' (map pretty arrs)
                <> comma </> pretty map_lam
                <> comma
                  </> PP.braces
                    (mconcat $ intersperse (comma <> PP.line) $ map pretty scans)
                <> comma </> pretty (scremaPostLambda screma)
            )
  pretty (Screma w arrs form) = ppScrema w arrs form

-- | Prettyprint the given Screma.
ppScrema ::
  (PrettyRep rep, Pretty inp) => SubExp -> [inp] -> ScremaForm rep -> Doc ann
ppScrema w arrs (ScremaForm map_lam scans reds post_lam) =
  "screma"
    <> (parens . align)
      ( pretty w
          <> comma </> ppTuple' (map pretty arrs)
          <> comma </> pretty map_lam
          <> comma
            </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map pretty scans)
          <> comma
            </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map pretty reds)
          <> comma </> pretty post_lam
      )

-- | Prettyprint the given Stream.
ppStream ::
  (PrettyRep rep, Pretty inp) => SubExp -> [inp] -> [SubExp] -> Lambda rep -> Doc ann
ppStream size arrs acc lam =
  "streamSeq"
    <> (parens . align)
      ( pretty size
          <> comma
            </> ppTuple' (map pretty arrs)
          <> comma
            </> ppTuple' (map pretty acc)
          <> comma
            </> pretty lam
      )

instance (PrettyRep rep) => Pretty (Scan rep) where
  pretty (Scan scan_lam scan_nes) =
    pretty scan_lam <> comma </> PP.braces (commasep $ map pretty scan_nes)

ppComm :: Commutativity -> Doc ann
ppComm Noncommutative = mempty
ppComm Commutative = "commutative "

instance (PrettyRep rep) => Pretty (Reduce rep) where
  pretty (Reduce comm red_lam red_nes) =
    ppComm comm
      <> pretty red_lam
      <> comma
        </> PP.braces (commasep $ map pretty red_nes)

-- | Prettyprint the given histogram operation.
ppHist ::
  (PrettyRep rep, Pretty inp) =>
  SubExp ->
  [inp] ->
  [HistOp rep] ->
  Lambda rep ->
  Doc ann
ppHist w arrs ops bucket_fun =
  "hist"
    <> parens
      ( pretty w
          <> comma
            </> ppTuple' (map pretty arrs)
          <> comma
            </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppOp ops)
          <> comma
            </> pretty bucket_fun
      )
  where
    ppOp (HistOp dest_w rf dests nes op) =
      pretty dest_w
        <> comma
          <+> pretty rf
        <> comma
          <+> PP.braces (commasep $ map pretty dests)
        <> comma
          </> ppTuple' (map pretty nes)
        <> comma
          </> pretty op
