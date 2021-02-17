{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    getStreamAccums,
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
    ppScrema,
    ppHist,

    -- * Generic traversal
    SOACMapper (..),
    identitySOACMapper,
    mapSOACM,
  )
where

import Control.Category
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer
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
import Futhark.Optimise.Simplify.Lore
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import qualified Futhark.TypeCheck as TC
import Futhark.Util (chunks, maybeNth)
import Futhark.Util.Pretty (Doc, Pretty, comma, commasep, parens, ppr, text, (<+>), (</>))
import qualified Futhark.Util.Pretty as PP
import GHC.Generics (Generic)
import Language.SexpGrammar as Sexp
import Language.SexpGrammar.Generic
import Prelude hiding (id, (.))

-- | A second-order array combinator (SOAC).
data SOAC lore
  = Stream SubExp (StreamForm lore) (Lambda lore) [VName]
  | -- | @Scatter <cs> <length> <lambda> <original index and value arrays>@
    --
    -- <input/output arrays along with their sizes and number of
    -- values to write for that array>
    --
    -- <length> is the length of each index array and value array, since they
    -- all must be the same length for any fusion to make sense.  If you have a
    -- list of index-value array pairs of different sizes, you need to use
    -- multiple writes instead.
    --
    -- The lambda body returns the output in this manner:
    --
    --     [index_0, index_1, ..., index_n, value_0, value_1, ..., value_n]
    --
    -- This must be consistent along all Scatter-related optimisations.
    Scatter SubExp (Lambda lore) [VName] [(SubExp, Int, VName)]
  | -- | @Hist <length> <dest-arrays-and-ops> <bucket fun> <input arrays>@
    --
    -- The first SubExp is the length of the input arrays. The first
    -- list describes the operations to perform.  The t'Lambda' is the
    -- bucket function.  Finally comes the input images.
    Hist SubExp [HistOp lore] (Lambda lore) [VName]
  | -- | A combination of scan, reduction, and map.  The first
    -- t'SubExp' is the size of the input arrays.
    Screma SubExp (ScremaForm lore) [VName]
  deriving (Eq, Ord, Show, Generic)

instance Decorations lore => SexpIso (SOAC lore) where
  sexpIso =
    match $
      With (. Sexp.list (Sexp.el (Sexp.sym "stream") >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
        With (. Sexp.list (Sexp.el (Sexp.sym "scatter") >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
          With (. Sexp.list (Sexp.el (Sexp.sym "hist") >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
            With
              (. Sexp.list (Sexp.el (Sexp.sym "screma") >>> Sexp.el sexpIso >>> Sexp.el sexpIso >>> Sexp.el sexpIso))
              End

-- | Information about computing a single histogram.
data HistOp lore = HistOp
  { histWidth :: SubExp,
    -- | Race factor @RF@ means that only @1/RF@
    -- bins are used.
    histRaceFactor :: SubExp,
    histDest :: [VName],
    histNeutral :: [SubExp],
    histOp :: Lambda lore
  }
  deriving (Eq, Ord, Show, Generic)

instance Decorations lore => SexpIso (HistOp lore) where
  sexpIso = with $ \histop ->
    Sexp.list
      ( Sexp.el sexpIso
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
      )
      >>> histop

-- | Is the stream chunk required to correspond to a contiguous
-- subsequence of the original input ('InOrder') or not?  'Disorder'
-- streams can be more efficient, but not all algorithms work with
-- this.
data StreamOrd = InOrder | Disorder
  deriving (Eq, Ord, Show, Generic)

instance SexpIso StreamOrd where
  sexpIso =
    match $
      With (. Sexp.sym "in-order") $
        With
          (. Sexp.sym "disorder")
          End

-- | What kind of stream is this?
data StreamForm lore
  = Parallel StreamOrd Commutativity (Lambda lore) [SubExp]
  | Sequential [SubExp]
  deriving (Eq, Ord, Show, Generic)

instance Decorations lore => SexpIso (StreamForm lore) where
  sexpIso =
    match $
      With
        ( .
            Sexp.list
              ( Sexp.el (Sexp.sym "parallel")
                  >>> Sexp.el sexpIso
                  >>> Sexp.el sexpIso
                  >>> Sexp.el sexpIso
                  >>> Sexp.rest sexpIso
              )
        )
        $ With
          ( .
              Sexp.list
                ( Sexp.el (Sexp.sym "sequential")
                    >>> Sexp.rest sexpIso
                )
          )
          End

-- | The essential parts of a 'Screma' factored out (everything
-- except the input arrays).
data ScremaForm lore
  = ScremaForm
      [Scan lore]
      [Reduce lore]
      (Lambda lore)
  deriving (Eq, Ord, Show, Generic)

instance Decorations lore => SexpIso (ScremaForm lore) where
  sexpIso = with $ \scremaform ->
    Sexp.list
      ( Sexp.el sexpIso
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
      )
      >>> scremaform

singleBinOp :: Bindable lore => [Lambda lore] -> Lambda lore
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
data Scan lore = Scan
  { scanLambda :: Lambda lore,
    scanNeutral :: [SubExp]
  }
  deriving (Eq, Ord, Show, Generic)

instance Decorations lore => SexpIso (Scan lore) where
  sexpIso = with $ \scan ->
    Sexp.list
      ( Sexp.el (Sexp.sym "scan")
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
      )
      >>> scan

-- | How many reduction results are produced by these 'Scan's?
scanResults :: [Scan lore] -> Int
scanResults = sum . map (length . scanNeutral)

-- | Combine multiple scan operators to a single operator.
singleScan :: Bindable lore => [Scan lore] -> Scan lore
singleScan scans =
  let scan_nes = concatMap scanNeutral scans
      scan_lam = singleBinOp $ map scanLambda scans
   in Scan scan_lam scan_nes

-- | How to compute a single reduction result.
data Reduce lore = Reduce
  { redComm :: Commutativity,
    redLambda :: Lambda lore,
    redNeutral :: [SubExp]
  }
  deriving (Eq, Ord, Show, Generic)

instance Decorations lore => SexpIso (Reduce lore) where
  sexpIso = with $ \red ->
    Sexp.list
      ( Sexp.el (Sexp.sym "reduce")
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
          >>> Sexp.el sexpIso
      )
      >>> red

-- | How many reduction results are produced by these 'Reduce's?
redResults :: [Reduce lore] -> Int
redResults = sum . map (length . redNeutral)

-- | Combine multiple reduction operators to a single operator.
singleReduce :: Bindable lore => [Reduce lore] -> Reduce lore
singleReduce reds =
  let red_nes = concatMap redNeutral reds
      red_lam = singleBinOp $ map redLambda reds
   in Reduce (mconcat (map redComm reds)) red_lam red_nes

-- | The types produced by a single 'Screma', given the size of the
-- input array.
scremaType :: SubExp -> ScremaForm lore -> [Type]
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
  (Bindable lore, MonadFreshNames m) =>
  [Type] ->
  m (Lambda lore)
mkIdentityLambda ts = do
  params <- mapM (newParam "x") ts
  return
    Lambda
      { lambdaParams = params,
        lambdaBody = mkBody mempty $ map (Var . paramName) params,
        lambdaReturnType = ts
      }

-- | Is the given lambda an identity lambda?
isIdentityLambda :: Lambda lore -> Bool
isIdentityLambda lam =
  bodyResult (lambdaBody lam)
    == map (Var . paramName) (lambdaParams lam)

-- | A lambda with no parameters that returns no values.
nilFn :: Bindable lore => Lambda lore
nilFn = Lambda mempty (mkBody mempty mempty) mempty

-- | Construct a Screma with possibly multiple scans, and
-- the given map function.
scanomapSOAC :: [Scan lore] -> Lambda lore -> ScremaForm lore
scanomapSOAC scans = ScremaForm scans []

-- | Construct a Screma with possibly multiple reductions, and
-- the given map function.
redomapSOAC :: [Reduce lore] -> Lambda lore -> ScremaForm lore
redomapSOAC = ScremaForm []

-- | Construct a Screma with possibly multiple scans, and identity map
-- function.
scanSOAC ::
  (Bindable lore, MonadFreshNames m) =>
  [Scan lore] ->
  m (ScremaForm lore)
scanSOAC scans = scanomapSOAC scans <$> mkIdentityLambda ts
  where
    ts = concatMap (lambdaReturnType . scanLambda) scans

-- | Construct a Screma with possibly multiple reductions, and
-- identity map function.
reduceSOAC ::
  (Bindable lore, MonadFreshNames m) =>
  [Reduce lore] ->
  m (ScremaForm lore)
reduceSOAC reds = redomapSOAC reds <$> mkIdentityLambda ts
  where
    ts = concatMap (lambdaReturnType . redLambda) reds

-- | Construct a Screma corresponding to a map.
mapSOAC :: Lambda lore -> ScremaForm lore
mapSOAC = ScremaForm [] []

-- | Does this Screma correspond to a scan-map composition?
isScanomapSOAC :: ScremaForm lore -> Maybe ([Scan lore], Lambda lore)
isScanomapSOAC (ScremaForm scans reds map_lam) = do
  guard $ null reds
  guard $ not $ null scans
  return (scans, map_lam)

-- | Does this Screma correspond to pure scan?
isScanSOAC :: ScremaForm lore -> Maybe [Scan lore]
isScanSOAC form = do
  (scans, map_lam) <- isScanomapSOAC form
  guard $ isIdentityLambda map_lam
  return scans

-- | Does this Screma correspond to a reduce-map composition?
isRedomapSOAC :: ScremaForm lore -> Maybe ([Reduce lore], Lambda lore)
isRedomapSOAC (ScremaForm scans reds map_lam) = do
  guard $ null scans
  guard $ not $ null reds
  return (reds, map_lam)

-- | Does this Screma correspond to a pure reduce?
isReduceSOAC :: ScremaForm lore -> Maybe [Reduce lore]
isReduceSOAC form = do
  (reds, map_lam) <- isRedomapSOAC form
  guard $ isIdentityLambda map_lam
  return reds

-- | Does this Screma correspond to a simple map, without any
-- reduction or scan results?
isMapSOAC :: ScremaForm lore -> Maybe (Lambda lore)
isMapSOAC (ScremaForm scans reds map_lam) = do
  guard $ null scans
  guard $ null reds
  return map_lam

-- | Like 'Mapper', but just for 'SOAC's.
data SOACMapper flore tlore m = SOACMapper
  { mapOnSOACSubExp :: SubExp -> m SubExp,
    mapOnSOACLambda :: Lambda flore -> m (Lambda tlore),
    mapOnSOACVName :: VName -> m VName
  }

-- | A mapper that simply returns the SOAC verbatim.
identitySOACMapper :: Monad m => SOACMapper lore lore m
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
  SOACMapper flore tlore m ->
  SOAC flore ->
  m (SOAC tlore)
mapSOACM tv (Stream size form lam arrs) =
  Stream <$> mapOnSOACSubExp tv size
    <*> mapOnStreamForm form
    <*> mapOnSOACLambda tv lam
    <*> mapM (mapOnSOACVName tv) arrs
  where
    mapOnStreamForm (Parallel o comm lam0 acc) =
      Parallel o comm
        <$> mapOnSOACLambda tv lam0
        <*> mapM (mapOnSOACSubExp tv) acc
    mapOnStreamForm (Sequential acc) =
      Sequential <$> mapM (mapOnSOACSubExp tv) acc
mapSOACM tv (Scatter len lam ivs as) =
  Scatter
    <$> mapOnSOACSubExp tv len
    <*> mapOnSOACLambda tv lam
    <*> mapM (mapOnSOACVName tv) ivs
    <*> mapM
      ( \(aw, an, a) ->
          (,,) <$> mapOnSOACSubExp tv aw
            <*> pure an
            <*> mapOnSOACVName tv a
      )
      as
mapSOACM tv (Hist len ops bucket_fun imgs) =
  Hist
    <$> mapOnSOACSubExp tv len
    <*> mapM
      ( \(HistOp e rf arrs nes op) ->
          HistOp <$> mapOnSOACSubExp tv e
            <*> mapOnSOACSubExp tv rf
            <*> mapM (mapOnSOACVName tv) arrs
            <*> mapM (mapOnSOACSubExp tv) nes
            <*> mapOnSOACLambda tv op
      )
      ops
    <*> mapOnSOACLambda tv bucket_fun
    <*> mapM (mapOnSOACVName tv) imgs
mapSOACM tv (Screma w (ScremaForm scans reds map_lam) arrs) =
  Screma <$> mapOnSOACSubExp tv w
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
    <*> mapM (mapOnSOACVName tv) arrs

instance ASTLore lore => FreeIn (SOAC lore) where
  freeIn' = flip execState mempty . mapSOACM free
    where
      walk f x = modify (<> f x) >> return x
      free =
        SOACMapper
          { mapOnSOACSubExp = walk freeIn',
            mapOnSOACLambda = walk freeIn',
            mapOnSOACVName = walk freeIn'
          }

instance ASTLore lore => Substitute (SOAC lore) where
  substituteNames subst =
    runIdentity . mapSOACM substitute
    where
      substitute =
        SOACMapper
          { mapOnSOACSubExp = return . substituteNames subst,
            mapOnSOACLambda = return . substituteNames subst,
            mapOnSOACVName = return . substituteNames subst
          }

instance ASTLore lore => Rename (SOAC lore) where
  rename = mapSOACM renamer
    where
      renamer = SOACMapper rename rename rename

-- | The type of a SOAC.
soacType :: SOAC lore -> [Type]
soacType (Stream outersize form lam _) =
  map (substNamesInType substs) rtp
  where
    nms = map paramName $ take (1 + length accs) params
    substs = M.fromList $ zip nms (outersize : accs)
    Lambda params _ rtp = lam
    accs = case form of
      Parallel _ _ _ acc -> acc
      Sequential acc -> acc
soacType (Scatter _w lam _ivs as) =
  zipWith arrayOfRow val_ts ws
  where
    val_ts =
      concatMap (take 1) $
        chunks ns $
          drop (sum ns) $ lambdaReturnType lam
    (ws, ns, _) = unzip3 as
soacType (Hist _len ops _bucket_fun _imgs) = do
  op <- ops
  map (`arrayOfRow` histWidth op) (lambdaReturnType $ histOp op)
soacType (Screma w form _arrs) =
  scremaType w form

instance TypedOp (SOAC lore) where
  opType = pure . staticShapes . soacType

instance (ASTLore lore, Aliased lore) => AliasedOp (SOAC lore) where
  opAliases = map (const mempty) . soacType

  -- Only map functions can consume anything.  The operands to scan
  -- and reduce functions are always considered "fresh".
  consumedInOp (Screma _ (ScremaForm _ _ map_lam) arrs) =
    mapNames consumedArray $ consumedByLambda map_lam
    where
      consumedArray v = fromMaybe v $ lookup v params_to_arrs
      params_to_arrs = zip (map paramName $ lambdaParams map_lam) arrs
  consumedInOp (Stream _ form lam arrs) =
    namesFromList $
      subExpVars $
        case form of
          Sequential accs ->
            map (consumedArray accs) $ namesToList $ consumedByLambda lam
          Parallel _ _ _ accs ->
            map (consumedArray accs) $ namesToList $ consumedByLambda lam
    where
      consumedArray accs v = fromMaybe (Var v) $ lookup v $ paramsToInput accs
      -- Drop the chunk parameter, which cannot alias anything.
      paramsToInput accs =
        zip
          (map paramName $ drop 1 $ lambdaParams lam)
          (accs ++ map Var arrs)
  consumedInOp (Scatter _ _ _ as) =
    namesFromList $ map (\(_, _, a) -> a) as
  consumedInOp (Hist _ ops _ _) =
    namesFromList $ concatMap histDest ops

mapHistOp ::
  (Lambda flore -> Lambda tlore) ->
  HistOp flore ->
  HistOp tlore
mapHistOp f (HistOp w rf dests nes lam) =
  HistOp w rf dests nes $ f lam

instance
  ( ASTLore lore,
    ASTLore (Aliases lore),
    CanBeAliased (Op lore)
  ) =>
  CanBeAliased (SOAC lore)
  where
  type OpWithAliases (SOAC lore) = SOAC (Aliases lore)

  addOpAliases aliases (Stream size form lam arr) =
    Stream
      size
      (analyseStreamForm form)
      (Alias.analyseLambda aliases lam)
      arr
    where
      analyseStreamForm (Parallel o comm lam0 acc) =
        Parallel o comm (Alias.analyseLambda aliases lam0) acc
      analyseStreamForm (Sequential acc) = Sequential acc
  addOpAliases aliases (Scatter len lam ivs as) =
    Scatter len (Alias.analyseLambda aliases lam) ivs as
  addOpAliases aliases (Hist len ops bucket_fun imgs) =
    Hist
      len
      (map (mapHistOp (Alias.analyseLambda aliases)) ops)
      (Alias.analyseLambda aliases bucket_fun)
      imgs
  addOpAliases aliases (Screma w (ScremaForm scans reds map_lam) arrs) =
    Screma
      w
      ( ScremaForm
          (map onScan scans)
          (map onRed reds)
          (Alias.analyseLambda aliases map_lam)
      )
      arrs
    where
      onRed red = red {redLambda = Alias.analyseLambda aliases $ redLambda red}
      onScan scan = scan {scanLambda = Alias.analyseLambda aliases $ scanLambda scan}

  removeOpAliases = runIdentity . mapSOACM remove
    where
      remove = SOACMapper return (return . removeLambdaAliases) return

instance ASTLore lore => IsOp (SOAC lore) where
  safeOp _ = False
  cheapOp _ = True

substNamesInType :: M.Map VName SubExp -> Type -> Type
substNamesInType _ tp@(Prim _) = tp
substNamesInType _ (Mem space) = Mem space
substNamesInType subs (Array btp shp u) =
  let shp' = Shape $ map (substNamesInSubExp subs) (shapeDims shp)
   in Array btp shp' u

substNamesInSubExp :: M.Map VName SubExp -> SubExp -> SubExp
substNamesInSubExp _ e@(Constant _) = e
substNamesInSubExp subs (Var idd) =
  M.findWithDefault (Var idd) idd subs

instance (ASTLore lore, CanBeWise (Op lore)) => CanBeWise (SOAC lore) where
  type OpWithWisdom (SOAC lore) = SOAC (Wise lore)

  removeOpWisdom = runIdentity . mapSOACM remove
    where
      remove = SOACMapper return (return . removeLambdaWisdom) return

instance Decorations lore => ST.IndexOp (SOAC lore) where
  indexOp vtable k soac [i] = do
    (lam, se, arr_params, arrs) <- lambdaAndSubExp soac
    let arr_indexes = M.fromList $ catMaybes $ zipWith arrIndex arr_params arrs
        arr_indexes' = foldl expandPrimExpTable arr_indexes $ bodyStms $ lambdaBody lam
    case se of
      Var v -> uncurry (flip ST.Indexed) <$> M.lookup v arr_indexes'
      _ -> Nothing
    where
      lambdaAndSubExp (Screma _ (ScremaForm scans reds map_lam) arrs) =
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
        | [v] <- patternNames $ stmPattern stm,
          Just (pe, cs) <-
            runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm,
          all (`ST.elem` vtable) (unCertificates $ stmCerts stm) =
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
typeCheckSOAC :: TC.Checkable lore => SOAC (Aliases lore) -> TC.TypeM lore ()
typeCheckSOAC (Stream size form lam arrexps) = do
  let accexps = getStreamAccums form
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
    Parallel _ _ lam0 _ -> do
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
    _ -> return ()
  -- just get the dflow of lambda on the fakearg, which does not alias
  -- arr, so we can later check that aliases of arr are not used inside lam.
  let fake_lamarrs' = map asArg lamarrs'
  TC.checkLambda lam $ asArg inttp : accargs ++ fake_lamarrs'
typeCheckSOAC (Scatter w lam ivs as) = do
  -- Requirements:
  --
  --   0. @lambdaReturnType@ of @lam@ must be a list
  --      [index types..., value types].
  --
  --   1. The number of index types must be equal to the number of value types
  --      and the number of writes to arrays in @as@.
  --
  --   2. Each index type must have the type i64.
  --
  --   3. Each array in @as@ and the value types must have the same type
  --
  --   4. Each array in @as@ is consumed.  This is not really a check, but more
  --      of a requirement, so that e.g. the source is not hoisted out of a
  --      loop, which will mean it cannot be consumed.
  --
  --   5. Each of ivs must be an array matching a corresponding lambda
  --      parameters.
  --
  -- Code:

  -- First check the input size.
  TC.require [Prim int64] w

  -- 0.
  let (_as_ws, as_ns, _as_vs) = unzip3 as
      rts = lambdaReturnType lam
      rtsLen = length rts `div` 2
      rtsI = take rtsLen rts
      rtsV = drop rtsLen rts

  -- 1.
  unless (rtsLen == sum as_ns) $
    TC.bad $ TC.TypeError "Scatter: Uneven number of index types, value types, and arrays outputs."

  -- 2.
  forM_ rtsI $ \rtI ->
    unless (Prim int64 == rtI) $
      TC.bad $ TC.TypeError "Scatter: Index return type must be i64."

  forM_ (zip (chunks as_ns rtsV) as) $ \(rtVs, (aw, _, a)) -> do
    -- All lengths must have type i64.
    TC.require [Prim int64] aw

    -- 3.
    forM_ rtVs $ \rtV -> TC.requireI [rtV `arrayOfRow` aw] a

    -- 4.
    TC.consume =<< TC.lookupAliases a

  -- 5.
  arrargs <- TC.checkSOACArrayArgs w ivs
  TC.checkLambda lam arrargs
typeCheckSOAC (Hist len ops bucket_fun imgs) = do
  TC.require [Prim int64] len

  -- Check the operators.
  forM_ ops $ \(HistOp dest_w rf dests nes op) -> do
    nes' <- mapM TC.checkArg nes
    TC.require [Prim int64] dest_w
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
      TC.requireI [t `arrayOfRow` dest_w] dest
      TC.consume =<< TC.lookupAliases dest

  -- Types of input arrays must equal parameter types for bucket function.
  img' <- TC.checkSOACArrayArgs len imgs
  TC.checkLambda bucket_fun img'

  -- Return type of bucket function must be an index for each
  -- operation followed by the values to write.
  nes_ts <- concat <$> mapM (mapM subExpType . histNeutral) ops
  let bucket_ret_t = replicate (length ops) (Prim int64) ++ nes_ts
  unless (bucket_ret_t == lambdaReturnType bucket_fun) $
    TC.bad $
      TC.TypeError $
        "Bucket function has return type "
          ++ prettyTuple (lambdaReturnType bucket_fun)
          ++ " but should have type "
          ++ prettyTuple bucket_ret_t
typeCheckSOAC (Screma w (ScremaForm scans reds map_lam) arrs) = do
  TC.require [Prim int64] w
  arrs' <- TC.checkSOACArrayArgs w arrs
  TC.checkLambda map_lam $ map TC.noArgAliases arrs'

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

-- | Get Stream's accumulators as a sub-expression list
getStreamAccums :: StreamForm lore -> [SubExp]
getStreamAccums (Parallel _ _ _ accs) = accs
getStreamAccums (Sequential accs) = accs

instance OpMetrics (Op lore) => OpMetrics (SOAC lore) where
  opMetrics (Stream _ _ lam _) =
    inside "Stream" $ lambdaMetrics lam
  opMetrics (Scatter _len lam _ivs _as) =
    inside "Scatter" $ lambdaMetrics lam
  opMetrics (Hist _len ops bucket_fun _imgs) =
    inside "Hist" $ mapM_ (lambdaMetrics . histOp) ops >> lambdaMetrics bucket_fun
  opMetrics (Screma _ (ScremaForm scans reds map_lam) _) =
    inside "Screma" $ do
      mapM_ (lambdaMetrics . scanLambda) scans
      mapM_ (lambdaMetrics . redLambda) reds
      lambdaMetrics map_lam

instance PrettyLore lore => PP.Pretty (SOAC lore) where
  ppr (Stream size form lam arrs) =
    case form of
      Parallel o comm lam0 acc ->
        let ord_str = if o == Disorder then "Per" else ""
            comm_str = case comm of
              Commutative -> "Comm"
              Noncommutative -> ""
         in text ("streamPar" ++ ord_str ++ comm_str)
              <> parens
                ( ppr size <> comma </> ppr lam0 </> comma </> ppr lam
                    </> commasep (PP.braces (commasep $ map ppr acc) : map ppr arrs)
                )
      Sequential acc ->
        text "streamSeq"
          <> parens
            ( ppr size <> comma </> ppr lam <> comma
                </> commasep (PP.braces (commasep $ map ppr acc) : map ppr arrs)
            )
  ppr (Scatter len lam ivs as) =
    ppSOAC "scatter" len [lam] (Just (map Var ivs)) (map (\(_, n, a) -> (n, a)) as)
  ppr (Hist len ops bucket_fun imgs) =
    ppHist len ops bucket_fun imgs
  ppr (Screma w (ScremaForm scans reds map_lam) arrs)
    | null scans,
      null reds =
      text "map"
        <> parens
          ( ppr w <> comma
              </> ppr map_lam <> comma
              </> commasep (map ppr arrs)
          )
    | null scans =
      text "redomap"
        <> parens
          ( ppr w <> comma
              </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr reds) <> comma
              </> ppr map_lam <> comma
              </> commasep (map ppr arrs)
          )
    | null reds =
      text "scanomap"
        <> parens
          ( ppr w <> comma
              </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr scans) <> comma
              </> ppr map_lam <> comma
              </> commasep (map ppr arrs)
          )
  ppr (Screma w form arrs) = ppScrema w form arrs

-- | Prettyprint the given Screma.
ppScrema ::
  (PrettyLore lore, Pretty inp) =>
  SubExp ->
  ScremaForm lore ->
  [inp] ->
  Doc
ppScrema w (ScremaForm scans reds map_lam) arrs =
  text "screma"
    <> parens
      ( ppr w <> comma
          </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr scans) <> comma
          </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppr reds) <> comma
          </> ppr map_lam <> comma
          </> commasep (map ppr arrs)
      )

instance PrettyLore lore => Pretty (Scan lore) where
  ppr (Scan scan_lam scan_nes) =
    ppr scan_lam <> comma </> PP.braces (commasep $ map ppr scan_nes)

ppComm :: Commutativity -> Doc
ppComm Noncommutative = mempty
ppComm Commutative = text "commutative "

instance PrettyLore lore => Pretty (Reduce lore) where
  ppr (Reduce comm red_lam red_nes) =
    ppComm comm <> ppr red_lam <> comma
      </> PP.braces (commasep $ map ppr red_nes)

-- | Prettyprint the given histogram operation.
ppHist ::
  (PrettyLore lore, Pretty inp) =>
  SubExp ->
  [HistOp lore] ->
  Lambda lore ->
  [inp] ->
  Doc
ppHist len ops bucket_fun imgs =
  text "hist"
    <> parens
      ( ppr len <> comma
          </> PP.braces (mconcat $ intersperse (comma <> PP.line) $ map ppOp ops) <> comma
          </> ppr bucket_fun <> comma
          </> commasep (map ppr imgs)
      )
  where
    ppOp (HistOp w rf dests nes op) =
      ppr w <> comma <+> ppr rf <> comma <+> PP.braces (commasep $ map ppr dests) <> comma
        </> PP.braces (commasep $ map ppr nes) <> comma
        </> ppr op

ppSOAC ::
  (Pretty fn, Pretty v) =>
  String ->
  SubExp ->
  [fn] ->
  Maybe [SubExp] ->
  [v] ->
  Doc
ppSOAC name size funs es as =
  text name
    <> parens
      ( ppr size <> comma
          </> ppList funs
          </> commasep (es' ++ map ppr as)
      )
  where
    es' = maybe [] ((: []) . ppTuple') es

ppList :: Pretty a => [a] -> Doc
ppList as = case map ppr as of
  [] -> mempty
  a' : as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'
