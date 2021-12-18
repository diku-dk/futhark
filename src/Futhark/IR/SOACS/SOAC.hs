{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of /Second-Order Array Combinators/ (SOACs), which are
-- the main form of parallelism in the early stages of the compiler.
module Futhark.IR.SOACS.SOAC
  ( SOAC (..),
    StreamOrd (..),
    StreamForm (..),
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
    nilFn,
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
    scremaLambda,
    ppScrema,
    ppHist,
    groupScatterResults,
    groupScatterResults',
    splitScatterResults,

    -- * Generic traversal
    SOACMapper (..),
    identitySOACMapper,
    mapSOACM,
    traverseSOACStms,
  )
where

import Control.Category
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Function ((&))
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Futhark.Analysis.Alias as Alias
import Futhark.Analysis.Metrics
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Construct
import Futhark.IR
import Futhark.IR.Aliases (Aliases, removeLambdaAliases)
import Futhark.IR.Prop.Aliases
import qualified Futhark.IR.TypeCheck as TC
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (chunks, maybeNth)
import Futhark.Util.Pretty (Doc, Pretty, comma, commasep, parens, ppr, text, (<+>), (</>))
import qualified Futhark.Util.Pretty as PP
import Prelude hiding (id, (.))

-- | A second-order array combinator (SOAC).
data SOAC rep
  = Stream SubExp [VName] (StreamForm rep) [SubExp] (Lambda rep)
  | -- | @Scatter <length> <lambda> <inputs> <outputs>@
    --
    -- Scatter maps values from a set of input arrays to indices and values of a
    -- set of output arrays. It is able to write multiple values to multiple
    -- outputs each of which may have multiple dimensions.
    --
    -- <inputs> is a list of input arrays, all having size <length>, elements of
    -- which are applied to the <lambda> function. For instance, if there are
    -- two arrays, <lambda> will get two values as input, one from each array.
    --
    -- <outputs> specifies the result of the <lambda> and which arrays to write
    -- to. Each element of the list consists of a <VName> specifying which array
    -- to scatter to, a <Shape> describing the shape of that array, and an <Int>
    -- describing how many elements should be written to that array for each
    -- invocation of the <lambda>.
    --
    -- <lambda> is a function that takes inputs from <inputs> and returns values
    -- according to the output-specification in <outputs>. It returns values in
    -- the following manner:
    --
    --     [index_0, index_1, ..., index_n, value_0, value_1, ..., value_m]
    --
    -- For each output in <outputs>, <lambda> returns <i> * <j> index values and
    -- <j> output values, where <i> is the number of dimensions (rank) of the
    -- given output, and <j> is the number of output values written to the given
    -- output.
    --
    -- For example, given the following output specification:
    --
    --     [([x1, y1, z1], 2, arr1), ([x2, y2], 1, arr2)]
    --
    -- <lambda> will produce 6 (3 * 2) index values and 2 output values for
    -- <arr1>, and 2 (2 * 1) index values and 1 output value for
    -- arr2. Additionally, the results are grouped, so the first 6 index values
    -- will correspond to the first two output values, and so on. For this
    -- example, <lambda> should return a total of 11 values, 8 index values and
    -- 3 output values.
    Scatter SubExp [VName] (Lambda rep) [(Shape, Int, VName)]
  | -- | @Hist <length> <dest-arrays-and-ops> <bucket fun> <input arrays>@
    --
    -- The first SubExp is the length of the input arrays. The first
    -- list describes the operations to perform.  The t'Lambda' is the
    -- bucket function.  Finally comes the input images.
    Hist SubExp [VName] [HistOp rep] (Lambda rep)
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

-- | Is the stream chunk required to correspond to a contiguous
-- subsequence of the original input ('InOrder') or not?  'Disorder'
-- streams can be more efficient, but not all algorithms work with
-- this.
data StreamOrd = InOrder | Disorder
  deriving (Eq, Ord, Show)

-- | What kind of stream is this?
data StreamForm rep
  = Parallel StreamOrd Commutativity (Lambda rep)
  | Sequential
  deriving (Eq, Ord, Show)

-- | The essential parts of a 'Screma' factored out (everything
-- except the input arrays).
data ScremaForm rep
  = ScremaForm
      [Scan rep]
      [Reduce rep]
      (Lambda rep)
  deriving (Eq, Ord, Show)

singleBinOp :: Buildable rep => [Lambda rep] -> Lambda rep
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

-- | How many reduction results are produced by these 'Scan's?
scanResults :: [Scan rep] -> Int
scanResults = sum . map (length . scanNeutral)

-- | Combine multiple scan operators to a single operator.
singleScan :: Buildable rep => [Scan rep] -> Scan rep
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

-- | How many reduction results are produced by these 'Reduce's?
redResults :: [Reduce rep] -> Int
redResults = sum . map (length . redNeutral)

-- | Combine multiple reduction operators to a single operator.
singleReduce :: Buildable rep => [Reduce rep] -> Reduce rep
singleReduce reds =
  let red_nes = concatMap redNeutral reds
      red_lam = singleBinOp $ map redLambda reds
   in Reduce (mconcat (map redComm reds)) red_lam red_nes

-- | The types produced by a single 'Screma', given the size of the
-- input array.
scremaType :: SubExp -> ScremaForm rep -> [Type]
scremaType w (ScremaForm scans reds map_lam) =
  scan_tps ++ red_tps ++ map (`arrayOfRow` w) map_tps
  where
    scan_tps =
      map (`arrayOfRow` w) $
        concatMap (lambdaReturnType . scanLambda) scans
    red_tps = concatMap (lambdaReturnType . redLambda) reds
    map_tps = drop (length scan_tps + length red_tps) $ lambdaReturnType map_lam

-- | Construct a lambda that takes parameters of the given types and
-- simply returns them unchanged.
mkIdentityLambda ::
  (Buildable rep, MonadFreshNames m) =>
  [Type] ->
  m (Lambda rep)
mkIdentityLambda ts = do
  params <- mapM (newParam "x") ts
  return
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

-- | A lambda with no parameters that returns no values.
nilFn :: Buildable rep => Lambda rep
nilFn = Lambda mempty (mkBody mempty mempty) mempty

-- | Construct a Screma with possibly multiple scans, and
-- the given map function.
scanomapSOAC :: [Scan rep] -> Lambda rep -> ScremaForm rep
scanomapSOAC scans = ScremaForm scans []

-- | Construct a Screma with possibly multiple reductions, and
-- the given map function.
redomapSOAC :: [Reduce rep] -> Lambda rep -> ScremaForm rep
redomapSOAC = ScremaForm []

-- | Construct a Screma with possibly multiple scans, and identity map
-- function.
scanSOAC ::
  (Buildable rep, MonadFreshNames m) =>
  [Scan rep] ->
  m (ScremaForm rep)
scanSOAC scans = scanomapSOAC scans <$> mkIdentityLambda ts
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
mapSOAC :: Lambda rep -> ScremaForm rep
mapSOAC = ScremaForm [] []

-- | Does this Screma correspond to a scan-map composition?
isScanomapSOAC :: ScremaForm rep -> Maybe ([Scan rep], Lambda rep)
isScanomapSOAC (ScremaForm scans reds map_lam) = do
  guard $ null reds
  guard $ not $ null scans
  return (scans, map_lam)

-- | Does this Screma correspond to pure scan?
isScanSOAC :: ScremaForm rep -> Maybe [Scan rep]
isScanSOAC form = do
  (scans, map_lam) <- isScanomapSOAC form
  guard $ isIdentityLambda map_lam
  return scans

-- | Does this Screma correspond to a reduce-map composition?
isRedomapSOAC :: ScremaForm rep -> Maybe ([Reduce rep], Lambda rep)
isRedomapSOAC (ScremaForm scans reds map_lam) = do
  guard $ null scans
  guard $ not $ null reds
  return (reds, map_lam)

-- | Does this Screma correspond to a pure reduce?
isReduceSOAC :: ScremaForm rep -> Maybe [Reduce rep]
isReduceSOAC form = do
  (reds, map_lam) <- isRedomapSOAC form
  guard $ isIdentityLambda map_lam
  return reds

-- | Does this Screma correspond to a simple map, without any
-- reduction or scan results?
isMapSOAC :: ScremaForm rep -> Maybe (Lambda rep)
isMapSOAC (ScremaForm scans reds map_lam) = do
  guard $ null scans
  guard $ null reds
  return map_lam

-- | Return the "main" lambda of the Screma.  For a map, this is
-- equivalent to 'isMapSOAC'.  Note that the meaning of the return
-- value of this lambda depends crucially on exactly which Screma this
-- is.  The parameters will correspond exactly to elements of the
-- input arrays, however.
scremaLambda :: ScremaForm rep -> Lambda rep
scremaLambda (ScremaForm _ _ map_lam) = map_lam

-- | @groupScatterResults <output specification> <results>@
--
-- Groups the index values and result values of <results> according to the
-- <output specification>.
--
-- This function is used for extracting and grouping the results of a
-- scatter. In the SOAC representation, the lambda inside a 'Scatter' returns
-- all indices and values as one big list. This function groups each value with
-- its corresponding indices (as determined by the t'Shape' of the output array).
--
-- The elements of the resulting list correspond to the shape and name of the
-- output parameters, in addition to a list of values written to that output
-- parameter, along with the array indices marking where to write them to.
--
-- See 'Scatter' for more information.
groupScatterResults :: [(Shape, Int, array)] -> [a] -> [(Shape, array, [([a], a)])]
groupScatterResults output_spec results =
  let (shapes, ns, arrays) = unzip3 output_spec
   in groupScatterResults' output_spec results
        & chunks ns
        & zip3 shapes arrays

-- | @groupScatterResults' <output specification> <results>@
--
-- Groups the index values and result values of <results> according to the
-- output specification. This is the simpler version of @groupScatterResults@,
-- which doesn't return any information about shapes or output arrays.
--
-- See 'groupScatterResults' for more information,
groupScatterResults' :: [(Shape, Int, array)] -> [a] -> [([a], a)]
groupScatterResults' output_spec results =
  let (indices, values) = splitScatterResults output_spec results
      (shapes, ns, _) = unzip3 output_spec
      chunk_sizes =
        concat $ zipWith (\shp n -> replicate n $ length shp) shapes ns
   in zip (chunks chunk_sizes indices) values

-- | @splitScatterResults <output specification> <results>@
--
-- Splits the results array into indices and values according to the output
-- specification.
--
-- See 'groupScatterResults' for more information.
splitScatterResults :: [(Shape, Int, array)] -> [a] -> ([a], [a])
splitScatterResults output_spec results =
  let (shapes, ns, _) = unzip3 output_spec
      num_indices = sum $ zipWith (*) ns $ map length shapes
   in splitAt num_indices results

-- | Like 'Mapper', but just for 'SOAC's.
data SOACMapper frep trep m = SOACMapper
  { mapOnSOACSubExp :: SubExp -> m SubExp,
    mapOnSOACLambda :: Lambda frep -> m (Lambda trep),
    mapOnSOACVName :: VName -> m VName
  }

-- | A mapper that simply returns the SOAC verbatim.
identitySOACMapper :: Monad m => SOACMapper rep rep m
identitySOACMapper =
  SOACMapper
    { mapOnSOACSubExp = return,
      mapOnSOACLambda = return,
      mapOnSOACVName = return
    }

-- | Map a monadic action across the immediate children of a
-- SOAC.  The mapping does not descend recursively into subexpressions
-- and is done left-to-right.
mapSOACM ::
  (Applicative m, Monad m) =>
  SOACMapper frep trep m ->
  SOAC frep ->
  m (SOAC trep)
mapSOACM tv (Stream size arrs form accs lam) =
  Stream <$> mapOnSOACSubExp tv size
    <*> mapM (mapOnSOACVName tv) arrs
    <*> mapOnStreamForm form
    <*> mapM (mapOnSOACSubExp tv) accs
    <*> mapOnSOACLambda tv lam
  where
    mapOnStreamForm (Parallel o comm lam0) =
      Parallel o comm <$> mapOnSOACLambda tv lam0
    mapOnStreamForm Sequential =
      pure Sequential
mapSOACM tv (Scatter w ivs lam as) =
  Scatter
    <$> mapOnSOACSubExp tv w
    <*> mapM (mapOnSOACVName tv) ivs
    <*> mapOnSOACLambda tv lam
    <*> mapM
      ( \(aw, an, a) ->
          (,,) <$> mapM (mapOnSOACSubExp tv) aw
            <*> pure an
            <*> mapOnSOACVName tv a
      )
      as
mapSOACM tv (Hist w arrs ops bucket_fun) =
  Hist
    <$> mapOnSOACSubExp tv w
    <*> mapM (mapOnSOACVName tv) arrs
    <*> mapM
      ( \(HistOp shape rf op_arrs nes op) ->
          HistOp <$> mapM (mapOnSOACSubExp tv) shape
            <*> mapOnSOACSubExp tv rf
            <*> mapM (mapOnSOACVName tv) op_arrs
            <*> mapM (mapOnSOACSubExp tv) nes
            <*> mapOnSOACLambda tv op
      )
      ops
    <*> mapOnSOACLambda tv bucket_fun
mapSOACM tv (Screma w arrs (ScremaForm scans reds map_lam)) =
  Screma <$> mapOnSOACSubExp tv w
    <*> mapM (mapOnSOACVName tv) arrs
    <*> ( ScremaForm
            <$> forM
              scans
              ( \(Scan red_lam red_nes) ->
                  Scan <$> mapOnSOACLambda tv red_lam
                    <*> mapM (mapOnSOACSubExp tv) red_nes
              )
            <*> forM
              reds
              ( \(Reduce comm red_lam red_nes) ->
                  Reduce comm <$> mapOnSOACLambda tv red_lam
                    <*> mapM (mapOnSOACSubExp tv) red_nes
              )
            <*> mapOnSOACLambda tv map_lam
        )

-- | A helper for defining 'TraverseOpStms'.
traverseSOACStms :: Monad m => OpStmsTraverser m (SOAC rep) rep
traverseSOACStms f = mapSOACM mapper
  where
    mapper = identitySOACMapper {mapOnSOACLambda = traverseLambdaStms f}

instance ASTRep rep => FreeIn (SOAC rep) where
  freeIn' = flip execState mempty . mapSOACM free
    where
      walk f x = modify (<> f x) >> return x
      free =
        SOACMapper
          { mapOnSOACSubExp = walk freeIn',
            mapOnSOACLambda = walk freeIn',
            mapOnSOACVName = walk freeIn'
          }

instance ASTRep rep => Substitute (SOAC rep) where
  substituteNames subst =
    runIdentity . mapSOACM substitute
    where
      substitute =
        SOACMapper
          { mapOnSOACSubExp = return . substituteNames subst,
            mapOnSOACLambda = return . substituteNames subst,
            mapOnSOACVName = return . substituteNames subst
          }

instance ASTRep rep => Rename (SOAC rep) where
  rename = mapSOACM renamer
    where
      renamer = SOACMapper rename rename rename

-- | The type of a SOAC.
soacType :: SOAC rep -> [Type]
soacType (Stream outersize _ _ accs lam) =
  map (substNamesInType substs) rtp
  where
    nms = map paramName $ take (1 + length accs) params
    substs = M.fromList $ zip nms (outersize : accs)
    Lambda params _ rtp = lam
soacType (Scatter _w _ivs lam dests) =
  zipWith arrayOfShape val_ts ws
  where
    indexes = sum $ zipWith (*) ns $ map length ws
    val_ts = drop indexes $ lambdaReturnType lam
    (ws, ns, _) = unzip3 dests
soacType (Hist _ _ ops _bucket_fun) = do
  op <- ops
  map (`arrayOfShape` histShape op) (lambdaReturnType $ histOp op)
soacType (Screma w _arrs form) =
  scremaType w form

instance TypedOp (SOAC rep) where
  opType = pure . staticShapes . soacType

instance (ASTRep rep, Aliased rep) => AliasedOp (SOAC rep) where
  opAliases = map (const mempty) . soacType

  -- Only map functions can consume anything.  The operands to scan
  -- and reduce functions are always considered "fresh".
  consumedInOp (Screma _ arrs (ScremaForm _ _ map_lam)) =
    mapNames consumedArray $ consumedByLambda map_lam
    where
      consumedArray v = fromMaybe v $ lookup v params_to_arrs
      params_to_arrs = zip (map paramName $ lambdaParams map_lam) arrs
  consumedInOp (Stream _ arrs form accs lam) =
    namesFromList $
      subExpVars $
        case form of
          Sequential ->
            map consumedArray $ namesToList $ consumedByLambda lam
          Parallel {} ->
            map consumedArray $ namesToList $ consumedByLambda lam
    where
      consumedArray v = fromMaybe (Var v) $ lookup v paramsToInput
      -- Drop the chunk parameter, which cannot alias anything.
      paramsToInput =
        zip (map paramName $ drop 1 $ lambdaParams lam) (accs ++ map Var arrs)
  consumedInOp (Scatter _ _ _ as) =
    namesFromList $ map (\(_, _, a) -> a) as
  consumedInOp (Hist _ _ ops _) =
    namesFromList $ concatMap histDest ops

mapHistOp ::
  (Lambda frep -> Lambda trep) ->
  HistOp frep ->
  HistOp trep
mapHistOp f (HistOp w rf dests nes lam) =
  HistOp w rf dests nes $ f lam

instance
  ( ASTRep rep,
    ASTRep (Aliases rep),
    CanBeAliased (Op rep)
  ) =>
  CanBeAliased (SOAC rep)
  where
  type OpWithAliases (SOAC rep) = SOAC (Aliases rep)

  addOpAliases aliases (Stream size arr form accs lam) =
    Stream size arr (analyseStreamForm form) accs $
      Alias.analyseLambda aliases lam
    where
      analyseStreamForm (Parallel o comm lam0) =
        Parallel o comm (Alias.analyseLambda aliases lam0)
      analyseStreamForm Sequential = Sequential
  addOpAliases aliases (Scatter len arrs lam dests) =
    Scatter len arrs (Alias.analyseLambda aliases lam) dests
  addOpAliases aliases (Hist w arrs ops bucket_fun) =
    Hist
      w
      arrs
      (map (mapHistOp (Alias.analyseLambda aliases)) ops)
      (Alias.analyseLambda aliases bucket_fun)
  addOpAliases aliases (Screma w arrs (ScremaForm scans reds map_lam)) =
    Screma w arrs $
      ScremaForm
        (map onScan scans)
        (map onRed reds)
        (Alias.analyseLambda aliases map_lam)
    where
      onRed red = red {redLambda = Alias.analyseLambda aliases $ redLambda red}
      onScan scan = scan {scanLambda = Alias.analyseLambda aliases $ scanLambda scan}

  removeOpAliases = runIdentity . mapSOACM remove
    where
      remove = SOACMapper return (return . removeLambdaAliases) return

instance ASTRep rep => IsOp (SOAC rep) where
  safeOp _ = False
  cheapOp _ = True

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

instance (ASTRep rep, CanBeWise (Op rep)) => CanBeWise (SOAC rep) where
  type OpWithWisdom (SOAC rep) = SOAC (Wise rep)

  removeOpWisdom = runIdentity . mapSOACM (SOACMapper pure (pure . removeLambdaWisdom) pure)
  addOpWisdom = runIdentity . mapSOACM (SOACMapper pure (pure . informLambda) pure)

instance RepTypes rep => ST.IndexOp (SOAC rep) where
  indexOp vtable k soac [i] = do
    (lam, se, arr_params, arrs) <- lambdaAndSubExp soac
    let arr_indexes = M.fromList $ catMaybes $ zipWith arrIndex arr_params arrs
        arr_indexes' = foldl expandPrimExpTable arr_indexes $ bodyStms $ lambdaBody lam
    case se of
      SubExpRes _ (Var v) -> uncurry (flip ST.Indexed) <$> M.lookup v arr_indexes'
      _ -> Nothing
    where
      lambdaAndSubExp (Screma _ arrs (ScremaForm scans reds map_lam)) =
        nthMapOut (scanResults scans + redResults reds) map_lam arrs
      lambdaAndSubExp _ =
        Nothing

      nthMapOut num_accs lam arrs = do
        se <- maybeNth (num_accs + k) $ bodyResult $ lambdaBody lam
        return (lam, se, drop num_accs $ lambdaParams lam, arrs)

      arrIndex p arr = do
        ST.Indexed cs pe <- ST.index' arr [i] vtable
        return (paramName p, (pe, cs))

      expandPrimExpTable table stm
        | [v] <- patNames $ stmPat stm,
          Just (pe, cs) <-
            runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm,
          all (`ST.elem` vtable) (unCerts $ stmCerts stm) =
          M.insert v (pe, stmCerts stm <> cs) table
        | otherwise =
          table

      asPrimExp table v
        | Just (e, cs) <- M.lookup v table = tell cs >> return e
        | Just (Prim pt) <- ST.lookupType v vtable =
          return $ LeafExp v pt
        | otherwise = lift Nothing
  indexOp _ _ _ _ = Nothing

-- | Type-check a SOAC.
typeCheckSOAC :: TC.Checkable rep => SOAC (Aliases rep) -> TC.TypeM rep ()
typeCheckSOAC (Stream size arrexps form accexps lam) = do
  TC.require [Prim int64] size
  accargs <- mapM TC.checkArg accexps
  arrargs <- mapM lookupType arrexps
  _ <- TC.checkSOACArrayArgs size arrexps
  let chunk = head $ lambdaParams lam
  let asArg t = (t, mempty)
      inttp = Prim int64
      lamarrs' = map (`setOuterSize` Var (paramName chunk)) arrargs
  let acc_len = length accexps
  let lamrtp = take acc_len $ lambdaReturnType lam
  unless (map TC.argType accargs == lamrtp) $
    TC.bad $ TC.TypeError "Stream with inconsistent accumulator type in lambda."
  -- check reduce's lambda, if any
  _ <- case form of
    Parallel _ _ lam0 -> do
      let acct = map TC.argType accargs
          outerRetType = lambdaReturnType lam0
      TC.checkLambda lam0 $ map TC.noArgAliases $ accargs ++ accargs
      unless (acct == outerRetType) $
        TC.bad $
          TC.TypeError $
            "Initial value is of type " ++ prettyTuple acct
              ++ ", but stream's reduce lambda returns type "
              ++ prettyTuple outerRetType
              ++ "."
    Sequential -> return ()
  -- just get the dflow of lambda on the fakearg, which does not alias
  -- arr, so we can later check that aliases of arr are not used inside lam.
  let fake_lamarrs' = map asArg lamarrs'
  TC.checkLambda lam $ asArg inttp : accargs ++ fake_lamarrs'
typeCheckSOAC (Scatter w arrs lam as) = do
  -- Requirements:
  --
  --   0. @lambdaReturnType@ of @lam@ must be a list
  --      [index types..., value types, ...].
  --
  --   1. The number of index types and value types must be equal to the number
  --      of return values from @lam@.
  --
  --   2. Each index type must have the type i64.
  --
  --   3. Each array in @as@ and the value types must have the same type
  --
  --   4. Each array in @as@ is consumed.  This is not really a check, but more
  --      of a requirement, so that e.g. the source is not hoisted out of a
  --      loop, which will mean it cannot be consumed.
  --
  --   5. Each of arrs must be an array matching a corresponding lambda
  --      parameters.
  --
  -- Code:

  -- First check the input size.
  TC.require [Prim int64] w

  -- 0.
  let (as_ws, as_ns, _as_vs) = unzip3 as
      indexes = sum $ zipWith (*) as_ns $ map length as_ws
      rts = lambdaReturnType lam
      rtsI = take indexes rts
      rtsV = drop indexes rts

  -- 1.
  unless (length rts == sum as_ns + sum (zipWith (*) as_ns $ map length as_ws)) $
    TC.bad $ TC.TypeError "Scatter: number of index types, value types and array outputs do not match."

  -- 2.
  forM_ rtsI $ \rtI ->
    unless (Prim int64 == rtI) $
      TC.bad $ TC.TypeError "Scatter: Index return type must be i64."

  forM_ (zip (chunks as_ns rtsV) as) $ \(rtVs, (aw, _, a)) -> do
    -- All lengths must have type i64.
    mapM_ (TC.require [Prim int64]) aw

    -- 3.
    forM_ rtVs $ \rtV -> TC.requireI [arrayOfShape rtV aw] a

    -- 4.
    TC.consume =<< TC.lookupAliases a

  -- 5.
  arrargs <- TC.checkSOACArrayArgs w arrs
  TC.checkLambda lam arrargs
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
      TC.bad $
        TC.TypeError $
          "Operator has return type "
            ++ prettyTuple (lambdaReturnType op)
            ++ " but neutral element has type "
            ++ prettyTuple nes_t

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
    TC.bad $
      TC.TypeError $
        "Bucket function has return type "
          ++ prettyTuple (lambdaReturnType bucket_fun)
          ++ " but should have type "
          ++ prettyTuple bucket_ret_t
typeCheckSOAC (Screma w arrs (ScremaForm scans reds map_lam)) = do
  TC.require [Prim int64] w
  arrs' <- TC.checkSOACArrayArgs w arrs
  TC.checkLambda map_lam arrs'

  scan_nes' <- fmap concat $
    forM scans $ \(Scan scan_lam scan_nes) -> do
      scan_nes' <- mapM TC.checkArg scan_nes
      let scan_t = map TC.argType scan_nes'
      TC.checkLambda scan_lam $ map TC.noArgAliases $ scan_nes' ++ scan_nes'
      unless (scan_t == lambdaReturnType scan_lam) $
        TC.bad $
          TC.TypeError $
            "Scan function returns type "
              ++ prettyTuple (lambdaReturnType scan_lam)
              ++ " but neutral element has type "
              ++ prettyTuple scan_t
      return scan_nes'

  red_nes' <- fmap concat $
    forM reds $ \(Reduce _ red_lam red_nes) -> do
      red_nes' <- mapM TC.checkArg red_nes
      let red_t = map TC.argType red_nes'
      TC.checkLambda red_lam $ map TC.noArgAliases $ red_nes' ++ red_nes'
      unless (red_t == lambdaReturnType red_lam) $
        TC.bad $
          TC.TypeError $
            "Reduce function returns type "
              ++ prettyTuple (lambdaReturnType red_lam)
              ++ " but neutral element has type "
              ++ prettyTuple red_t
      return red_nes'

  let map_lam_ts = lambdaReturnType map_lam

  unless
    ( take (length scan_nes' + length red_nes') map_lam_ts
        == map TC.argType (scan_nes' ++ red_nes')
    )
    $ TC.bad $
      TC.TypeError $
        "Map function return type " ++ prettyTuple map_lam_ts
          ++ " wrong for given scan and reduction functions."

instance OpMetrics (Op rep) => OpMetrics (SOAC rep) where
  opMetrics (Stream _ _ _ _ lam) =
    inside "Stream" $ lambdaMetrics lam
  opMetrics (Scatter _len _ lam _) =
    inside "Scatter" $ lambdaMetrics lam
  opMetrics (Hist _ _ ops bucket_fun) =
    inside "Hist" $ mapM_ (lambdaMetrics . histOp) ops >> lambdaMetrics bucket_fun
  opMetrics (Screma _ _ (ScremaForm scans reds map_lam)) =
    inside "Screma" $ do
      mapM_ (lambdaMetrics . scanLambda) scans
      mapM_ (lambdaMetrics . redLambda) reds
      lambdaMetrics map_lam

instance PrettyRep rep => PP.Pretty (SOAC rep) where
  ppr (Stream size arrs form acc lam) =
    case form of
      Parallel o comm lam0 ->
        let ord_str = if o == Disorder then "Per" else ""
            comm_str = case comm of
              Commutative -> "Comm"
              Noncommutative -> ""
         in text ("streamPar" ++ ord_str ++ comm_str)
              <> parens
                ( ppr size <> comma
                    </> ppTuple' arrs <> comma
                    </> ppr lam0 <> comma
                    </> ppTuple' acc <> comma
                    </> ppr lam
                )
      Sequential ->
        text "streamSeq"
          <> parens
            ( ppr size <> comma
                </> ppTuple' arrs <> comma
                </> ppTuple' acc <> comma
                </> ppr lam
            )
  ppr (Scatter w arrs lam dests) =
    "scatter"
      <> parens
        ( ppr w <> comma
            </> ppTuple' arrs <> comma
            </> ppr lam <> comma
            </> commasep (map ppr dests)
        )
  ppr (Hist w arrs ops bucket_fun) =
    ppHist w arrs ops bucket_fun
  ppr (Screma w arrs (ScremaForm scans reds map_lam))
    | null scans,
      null reds =
      text "map"
        <> parens
          ( ppr w <> comma
              </> ppTuple' arrs <> comma
              </> ppr map_lam
          )
    | null scans =
      text "redomap"
        <> parens
          ( ppr w <> comma
              </> ppTuple' arrs <> comma
              </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr reds) <> comma
              </> ppr map_lam
          )
    | null reds =
      text "scanomap"
        <> parens
          ( ppr w <> comma
              </> ppTuple' arrs <> comma
              </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr scans) <> comma
              </> ppr map_lam
          )
  ppr (Screma w arrs form) = ppScrema w arrs form

-- | Prettyprint the given Screma.
ppScrema ::
  (PrettyRep rep, Pretty inp) => SubExp -> [inp] -> ScremaForm rep -> Doc
ppScrema w arrs (ScremaForm scans reds map_lam) =
  text "screma"
    <> parens
      ( ppr w <> comma
          </> ppTuple' arrs <> comma
          </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr scans) <> comma
          </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr reds) <> comma
          </> ppr map_lam
      )

instance PrettyRep rep => Pretty (Scan rep) where
  ppr (Scan scan_lam scan_nes) =
    ppr scan_lam <> comma </> PP.braces (commasep $ map ppr scan_nes)

ppComm :: Commutativity -> Doc
ppComm Noncommutative = mempty
ppComm Commutative = text "commutative "

instance PrettyRep rep => Pretty (Reduce rep) where
  ppr (Reduce comm red_lam red_nes) =
    ppComm comm <> ppr red_lam <> comma
      </> PP.braces (commasep $ map ppr red_nes)

-- | Prettyprint the given histogram operation.
ppHist ::
  (PrettyRep rep, Pretty inp) =>
  SubExp ->
  [inp] ->
  [HistOp rep] ->
  Lambda rep ->
  Doc
ppHist w arrs ops bucket_fun =
  text "hist"
    <> parens
      ( ppr w <> comma
          </> ppTuple' arrs <> comma
          </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppOp ops) <> comma
          </> ppr bucket_fun
      )
  where
    ppOp (HistOp dest_w rf dests nes op) =
      ppr dest_w <> comma <+> ppr rf <> comma <+> PP.braces (commasep $ map ppr dests) <> comma
        </> ppTuple' nes <> comma
        </> ppr op
