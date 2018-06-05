{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.SOACS.SOAC
       ( SOAC(..)
       , StreamForm(..)
       , ScremaForm(..)
       , Scan
       , Reduce

       , typeCheckSOAC

         -- * Utility
       , getStreamOrder
       , getStreamAccums
       , scremaType
       , soacType

       , mkIdentityLambda
       , isIdentityLambda
       , composeLambda
       , nilFn
       , scanomapSOAC
       , redomapSOAC
       , scanSOAC
       , reduceSOAC
       , mapSOAC
       , isScanomapSOAC
       , isRedomapSOAC
       , isScanSOAC
       , isReduceSOAC
       , isMapSOAC

       , ppScrema

         -- * Generic traversal
       , SOACMapper(..)
       , identitySOACMapper
       , mapSOACM
       )
       where

import Control.Monad.Writer
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

import Futhark.Representation.AST
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty (ppr, Doc, Pretty, parens, comma, (</>), commasep, text)
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplify.Lore
import Futhark.Representation.Ranges (Ranges, removeLambdaRanges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.Aliases (Aliases, removeLambdaAliases)
import Futhark.Analysis.Usage
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.Range as Range
import Futhark.Construct
import Futhark.Util (maybeNth, chunks, splitAt3)

data SOAC lore =
    Stream SubExp (StreamForm lore) (LambdaT lore) [VName]
  | Scatter SubExp (LambdaT lore) [VName] [(SubExp, Int, VName)]
    -- Scatter <cs> <length> <lambda> <original index and value arrays>
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
  | Screma SubExp (ScremaForm lore) [VName]
    -- ^ A combination of scan, reduction, and map.  The first
    -- 'SubExp' is the size of the input arrays.  The first
    -- 'Lambda'/'SubExp' pair is for scan and its neutral elements.
    -- The second is for the reduction.  The final lambda is for the
    -- map part, and finally comes the input arrays.
  | CmpThreshold SubExp String
    deriving (Eq, Ord, Show)

data StreamForm lore  =
    Parallel StreamOrd Commutativity (LambdaT lore) [SubExp]
  | Sequential [SubExp]
  deriving (Eq, Ord, Show)

-- | The essential parts of a 'Screma' factored out (everything
-- except the input arrays).
data ScremaForm lore = ScremaForm
                         (Scan lore)
                         (Reduce lore)
                         (LambdaT lore)
  deriving (Eq, Ord, Show)

type Scan lore = (LambdaT lore, [SubExp])
type Reduce lore = (Commutativity, LambdaT lore, [SubExp])

scremaType :: SubExp -> ScremaForm lore -> [Type]
scremaType w (ScremaForm (scan_lam, _scan_nes) (_, red_lam, _red_nes) map_lam) =
  map (`arrayOfRow` w) scan_tps ++ red_tps ++ map (`arrayOfRow` w) map_tps
  where scan_tps = lambdaReturnType scan_lam
        red_tps  = lambdaReturnType red_lam
        map_tps  = drop (length scan_tps + length red_tps) $ lambdaReturnType map_lam

-- | Construct a lambda that takes parameters of the given types and
-- simply returns them unchanged.
mkIdentityLambda :: (Bindable lore, MonadFreshNames m) =>
                    [Type] -> m (Lambda lore)
mkIdentityLambda ts = do
  params <- mapM (newParam "x") ts
  return Lambda { lambdaParams = params
                , lambdaBody = mkBody mempty $ map (Var . paramName) params
                , lambdaReturnType = ts }

-- | Is the given lambda an identity lambda?
isIdentityLambda :: Lambda lore -> Bool
isIdentityLambda lam = bodyResult (lambdaBody lam) ==
                       map (Var . paramName) (lambdaParams lam)

composeLambda :: (Bindable lore, BinderOps lore, MonadFreshNames m,
                  HasScope somelore m, SameScope somelore lore) =>
                 Lambda lore
              -> Lambda lore
              -> Lambda lore
              -> m (Lambda lore)
composeLambda scan_fun red_fun map_fun = do
  body <- runBodyBinder $ inScopeOf scan_fun $ inScopeOf red_fun $ inScopeOf map_fun $ do
    mapM_ addStm $ bodyStms $ lambdaBody map_fun
    let (scan_res, red_res, map_res) = splitAt3 n m $ bodyResult $ lambdaBody map_fun

    forM_ (zip scan_y_params scan_res) $ \(p,se) ->
      letBindNames_ [paramName p] $ BasicOp $ SubExp se
    forM_ (zip red_y_params red_res) $ \(p,se) ->
      letBindNames_ [paramName p] $ BasicOp $ SubExp se
    mapM_ addStm $ bodyStms $ lambdaBody scan_fun
    mapM_ addStm $ bodyStms $ lambdaBody red_fun

    resultBodyM $
      bodyResult (lambdaBody scan_fun) ++
      bodyResult (lambdaBody red_fun) ++
      map_res

  return Lambda { lambdaParams = scan_x_params ++ red_x_params ++ lambdaParams map_fun
                , lambdaBody = body
                , lambdaReturnType = lambdaReturnType map_fun }
  where n = length $ lambdaReturnType scan_fun
        m = length $ lambdaReturnType red_fun
        (scan_x_params, scan_y_params) = splitAt n $ lambdaParams scan_fun
        (red_x_params, red_y_params) = splitAt m $ lambdaParams red_fun

-- | A lambda with no parameters that returns no values.
nilFn :: Bindable lore => LambdaT lore
nilFn = Lambda mempty (mkBody mempty mempty) mempty

isNilFn :: LambdaT lore -> Bool
isNilFn (Lambda ps body ts) =
  null ps && null ts &&
  null (bodyStms body) && null (bodyResult body)

scanomapSOAC :: Bindable lore =>
                Lambda lore -> [SubExp] -> Lambda lore -> ScremaForm lore
scanomapSOAC lam nes = ScremaForm (lam, nes) (mempty, nilFn, mempty)

redomapSOAC :: Bindable lore =>
               Commutativity -> Lambda lore -> [SubExp] -> Lambda lore -> ScremaForm lore
redomapSOAC comm lam nes = ScremaForm (nilFn, mempty) (comm, lam, nes)

scanSOAC :: (Bindable lore, MonadFreshNames m) =>
            Lambda lore -> [SubExp] -> m (ScremaForm lore)
scanSOAC lam nes = scanomapSOAC lam nes <$> mkIdentityLambda (lambdaReturnType lam)

reduceSOAC :: (Bindable lore, MonadFreshNames m) =>
              Commutativity -> Lambda lore -> [SubExp] -> m (ScremaForm lore)
reduceSOAC comm lam nes = redomapSOAC comm lam nes <$> mkIdentityLambda (lambdaReturnType lam)

mapSOAC :: Bindable lore => Lambda lore -> ScremaForm lore
mapSOAC = ScremaForm (nilFn, mempty) (mempty, nilFn, mempty)

isScanomapSOAC :: ScremaForm lore -> Maybe (Lambda lore, [SubExp], Lambda lore)
isScanomapSOAC (ScremaForm (scan_lam, scan_nes) (_, _, red_nes) map_lam) = do
  guard $ null red_nes
  guard $ not $ null scan_nes
  return (scan_lam, scan_nes, map_lam)

isScanSOAC :: ScremaForm lore -> Maybe (Lambda lore, [SubExp])
isScanSOAC form = do (scan_lam, scan_nes, map_lam) <- isScanomapSOAC form
                     guard $ isIdentityLambda map_lam
                     return (scan_lam, scan_nes)

isRedomapSOAC :: ScremaForm lore -> Maybe (Commutativity, Lambda lore, [SubExp], Lambda lore)
isRedomapSOAC (ScremaForm (_, scan_nes) (comm, red_lam, red_nes) map_lam) = do
  guard $ null scan_nes
  guard $ not $ null red_nes
  return (comm, red_lam, red_nes, map_lam)

isReduceSOAC :: ScremaForm lore -> Maybe (Commutativity, Lambda lore, [SubExp])
isReduceSOAC form = do (comm, red_lam, red_nes, map_lam) <- isRedomapSOAC form
                       guard $ isIdentityLambda map_lam
                       return (comm, red_lam, red_nes)

isMapSOAC :: ScremaForm lore -> Maybe (Lambda lore)
isMapSOAC (ScremaForm (_, scan_nes) (_, _, red_nes) map_lam) = do
  guard $ null scan_nes
  guard $ null red_nes
  return map_lam

-- | Like 'Mapper', but just for 'SOAC's.
data SOACMapper flore tlore m = SOACMapper {
    mapOnSOACSubExp :: SubExp -> m SubExp
  , mapOnSOACLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnSOACVName :: VName -> m VName
  }

-- | A mapper that simply returns the SOAC verbatim.
identitySOACMapper :: Monad m => SOACMapper lore lore m
identitySOACMapper = SOACMapper { mapOnSOACSubExp = return
                                , mapOnSOACLambda = return
                                , mapOnSOACVName = return
                                }

-- | Map a monadic action across the immediate children of a
-- SOAC.  The mapping does not descend recursively into subexpressions
-- and is done left-to-right.
mapSOACM :: (Applicative m, Monad m) =>
            SOACMapper flore tlore m -> SOAC flore -> m (SOAC tlore)
mapSOACM tv (Stream size form lam arrs) =
  Stream <$> mapOnSOACSubExp tv size <*>
  mapOnStreamForm form <*> mapOnSOACLambda tv lam <*>
  mapM (mapOnSOACVName tv) arrs
  where mapOnStreamForm (Parallel o comm lam0 acc) =
            Parallel <$> pure o  <*> pure comm <*>
            mapOnSOACLambda tv lam0 <*>
            mapM (mapOnSOACSubExp tv) acc
        mapOnStreamForm (Sequential acc) =
            Sequential <$> mapM (mapOnSOACSubExp tv) acc
mapSOACM tv (Scatter len lam ivs as) =
  Scatter
  <$> mapOnSOACSubExp tv len
  <*> mapOnSOACLambda tv lam
  <*> mapM (mapOnSOACVName tv) ivs
  <*> mapM (\(aw,an,a) -> (,,) <$> mapOnSOACSubExp tv aw <*>
                          pure an <*> mapOnSOACVName tv a) as
mapSOACM tv (Screma w (ScremaForm (scan_lam, scan_nes) (comm, red_lam, red_nes) map_lam) arrs) =
  Screma <$> mapOnSOACSubExp tv w <*>
  (ScremaForm <$>
   ((,) <$> mapOnSOACLambda tv scan_lam <*> mapM (mapOnSOACSubExp tv) scan_nes) <*>
   ((,,) comm <$> mapOnSOACLambda tv red_lam <*> mapM (mapOnSOACSubExp tv) red_nes) <*>
   mapOnSOACLambda tv map_lam)
  <*> mapM (mapOnSOACVName tv) arrs
mapSOACM tv (CmpThreshold what s) = CmpThreshold <$> mapOnSOACSubExp tv what <*> pure s

instance Attributes lore => FreeIn (SOAC lore) where
  freeIn = execWriter . mapSOACM free
    where walk f x = tell (f x) >> return x
          free = SOACMapper { mapOnSOACSubExp = walk freeIn
                            , mapOnSOACLambda = walk freeInLambda
                            , mapOnSOACVName = walk freeIn
                            }

instance Attributes lore => Substitute (SOAC lore) where
  substituteNames subst =
    runIdentity . mapSOACM substitute
    where substitute =
            SOACMapper { mapOnSOACSubExp = return . substituteNames subst
                       , mapOnSOACLambda = return . substituteNames subst
                       , mapOnSOACVName = return . substituteNames subst
                       }

instance Attributes lore => Rename (SOAC lore) where
  rename = mapSOACM renamer
    where renamer = SOACMapper rename rename rename

soacType :: SOAC lore -> [Type]
soacType (Stream outersize form lam _) =
  map (substNamesInType substs) rtp
  where nms = map paramName $ take (1 + length accs) params
        substs = M.fromList $ zip nms (outersize:accs)
        Lambda params _ rtp = lam
        accs = case form of
                Parallel _ _ _ acc -> acc
                Sequential  acc -> acc
soacType (Scatter _w lam _ivs as) =
  zipWith arrayOfRow val_ts ws
  where val_ts = concatMap (take 1) $ chunks ns $
                 drop (sum ns) $ lambdaReturnType lam
        (ws, ns, _) = unzip3 as
soacType (Screma w form _arrs) =
  scremaType w form
soacType CmpThreshold{} = [Prim Bool]

instance TypedOp (SOAC lore) where
  opType = pure . staticShapes . soacType

instance (Attributes lore, Aliased lore) => AliasedOp (SOAC lore) where
  opAliases = map (const mempty) . soacType

  -- Only map functions can consume anything.  The operands to scan
  -- and reduce functions are always considered "fresh".
  consumedInOp (Screma _ (ScremaForm _ _ map_lam) arrs) =
    S.map consumedArray $ consumedByLambda map_lam
    where consumedArray v = fromMaybe v $ lookup v params_to_arrs
          params_to_arrs = zip (map paramName $ lambdaParams map_lam) arrs
  consumedInOp (Stream _ form lam arrs) =
    S.fromList $ subExpVars $
    case form of Sequential accs ->
                   map (consumedArray accs) $ S.toList $ consumedByLambda lam
                 Parallel _ _ _ accs ->
                   map (consumedArray accs) $ S.toList $ consumedByLambda lam
    where consumedArray accs v = fromMaybe (Var v) $ lookup v $ paramsToInput accs
          -- Drop the chunk parameter, which cannot alias anything.
          paramsToInput accs = zip
                               (map paramName $ drop 1 $ lambdaParams lam)
                               (accs++map Var arrs)
  consumedInOp (Scatter _ _ _ as) =
    S.fromList $ map (\(_, _, a) -> a) as
  consumedInOp CmpThreshold{} = mempty

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (SOAC lore) where
  type OpWithAliases (SOAC lore) = SOAC (Aliases lore)

  addOpAliases (Stream size form lam arr) =
    Stream size (analyseStreamForm form)
    (Alias.analyseLambda lam) arr
    where analyseStreamForm (Parallel o comm lam0 acc) =
              Parallel o comm (Alias.analyseLambda lam0) acc
          analyseStreamForm (Sequential acc) = Sequential acc
  addOpAliases (Scatter len lam ivs as) =
    Scatter len (Alias.analyseLambda lam) ivs as
  addOpAliases (Screma w (ScremaForm (scan_lam, scan_nes) (comm, red_lam, red_nes) map_lam) arrs) =
    Screma w (ScremaForm
                (Alias.analyseLambda scan_lam, scan_nes)
                (comm, Alias.analyseLambda red_lam, red_nes)
                (Alias.analyseLambda map_lam))
               arrs
  addOpAliases (CmpThreshold what s) = CmpThreshold what s

  removeOpAliases = runIdentity . mapSOACM remove
    where remove = SOACMapper return (return . removeLambdaAliases) return

instance Attributes lore => IsOp (SOAC lore) where
  safeOp CmpThreshold{} = True
  safeOp _ = False
  cheapOp _ = True

substNamesInType :: M.Map VName SubExp -> Type -> Type
substNamesInType _ tp@(Prim _) = tp
substNamesInType subs (Mem se space) =
  Mem (substNamesInSubExp subs se) space
substNamesInType subs (Array btp shp u) =
  let shp' = Shape $ map (substNamesInSubExp subs) (shapeDims shp)
  in  Array btp shp' u

substNamesInSubExp :: M.Map VName SubExp -> SubExp -> SubExp
substNamesInSubExp _ e@(Constant _) = e
substNamesInSubExp subs (Var idd) =
  M.findWithDefault (Var idd) idd subs

instance (Ranged inner) => RangedOp (SOAC inner) where
  opRanges op = replicate (length $ soacType op) unknownRange

instance (Attributes lore, CanBeRanged (Op lore)) => CanBeRanged (SOAC lore) where
  type OpWithRanges (SOAC lore) = SOAC (Ranges lore)

  removeOpRanges = runIdentity . mapSOACM remove
    where remove = SOACMapper return (return . removeLambdaRanges) return
  addOpRanges (Stream w form lam arr) =
    Stream w
    (Range.runRangeM $ analyseStreamForm form)
    (Range.runRangeM $ Range.analyseLambda lam)
    arr
    where analyseStreamForm (Sequential acc) =
            return $ Sequential acc
          analyseStreamForm (Parallel o comm lam0 acc) = do
              lam0' <- Range.analyseLambda lam0
              return $ Parallel o comm lam0' acc
  addOpRanges (Scatter len lam ivs as) =
    Scatter len (Range.runRangeM $ Range.analyseLambda lam) ivs as
  addOpRanges (Screma w (ScremaForm (scan_lam, scan_nes) (comm, red_lam, red_nes) map_lam) arrs) =
    Screma w (ScremaForm
                (Range.runRangeM $ Range.analyseLambda scan_lam, scan_nes)
                (comm, Range.runRangeM $ Range.analyseLambda red_lam, red_nes)
                (Range.runRangeM $ Range.analyseLambda map_lam))
               arrs
  addOpRanges (CmpThreshold what s) = CmpThreshold what s

instance (Attributes lore, CanBeWise (Op lore)) => CanBeWise (SOAC lore) where
  type OpWithWisdom (SOAC lore) = SOAC (Wise lore)

  removeOpWisdom = runIdentity . mapSOACM remove
    where remove = SOACMapper return (return . removeLambdaWisdom) return

instance Annotations lore => ST.IndexOp (SOAC lore) where
  indexOp vtable k soac [i] = do
    (lam,se,arr_params,arrs) <- lambdaAndSubExp soac
    let arr_indexes = M.fromList $ catMaybes $ zipWith arrIndex arr_params arrs
        arr_indexes' = foldl expandPrimExpTable arr_indexes $ bodyStms $ lambdaBody lam
    case se of
      Var v -> M.lookup v arr_indexes'
      _ -> Nothing
      where lambdaAndSubExp (Screma _ (ScremaForm (_, scan_nes) (_, _, red_nes) map_lam) arrs) =
              nthMapOut (length scan_nes + length red_nes) map_lam arrs
            lambdaAndSubExp _ =
              Nothing

            nthMapOut num_accs lam arrs = do
              se <- maybeNth (num_accs+k) $ bodyResult $ lambdaBody lam
              return (lam, se, drop num_accs $ lambdaParams lam, arrs)

            arrIndex p arr = do
              (pe,cs) <- ST.index' arr [i] vtable
              return (paramName p, (pe,cs))

            expandPrimExpTable table stm
              | [v] <- patternNames $ stmPattern stm,
                Just (pe,cs) <-
                  runWriterT $ primExpFromExp (asPrimExp table) $ stmExp stm,
                all (`ST.elem` vtable) (unCertificates $ stmCerts stm) =
                  M.insert v (pe, stmCerts stm <> cs) table
              | otherwise =
                  table

            asPrimExp table (BasicOp (SubExp (Var v)))
              | Just (e,cs) <- M.lookup v table = tell cs >> return e
              | Just (Prim pt) <- ST.lookupType v vtable =
                  return $ LeafExp v pt
            asPrimExp _ (BasicOp (SubExp (Constant v))) =
              return $ ValueExp v
            asPrimExp _ _ = lift Nothing
  indexOp _ _ _ _ = Nothing

instance Aliased lore => UsageInOp (SOAC lore) where
  usageInOp (Screma _ (ScremaForm _ _ f) arrs) = usageInLambda f arrs
  usageInOp _ = mempty

typeCheckSOAC :: TC.Checkable lore => SOAC (Aliases lore) -> TC.TypeM lore ()
typeCheckSOAC (CmpThreshold what _) = TC.require [Prim int32] what
typeCheckSOAC (Stream size form lam arrexps) = do
  let accexps = getStreamAccums form
  TC.require [Prim int32] size
  accargs <- mapM TC.checkArg accexps
  arrargs <- mapM lookupType arrexps
  _ <- TC.checkSOACArrayArgs size arrexps
  let chunk = head $ lambdaParams lam
  let asArg t = (t, mempty)
      inttp   = Prim int32
      lamarrs'= map (`setOuterSize` Var (paramName chunk)) arrargs
  let acc_len= length accexps
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
                TC.bad $ TC.TypeError $
                "Initial value is of type " ++ prettyTuple acct ++
                ", but stream's reduce lambda returns type " ++ prettyTuple outerRetType ++ "."
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
  --   2. Each index type must have the type i32.
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
  TC.require [Prim int32] w

  -- 0.
  let (_as_ws, as_ns, _as_vs) = unzip3 as
      rts = lambdaReturnType lam
      rtsLen = length rts `div` 2
      rtsI = take rtsLen rts
      rtsV = drop rtsLen rts

  -- 1.
  unless (rtsLen == sum as_ns)
    $ TC.bad $ TC.TypeError "Scatter: Uneven number of index types, value types, and arrays outputs."

  -- 2.
  forM_ rtsI $ \rtI -> unless (Prim int32 == rtI) $
    TC.bad $ TC.TypeError "Scatter: Index return type must be i32."

  forM_ (zip (chunks as_ns rtsV) as) $ \(rtVs, (aw, _, a)) -> do
    -- All lengths must have type i32.
    TC.require [Prim int32] aw

    -- 3.
    forM_ rtVs $ \rtV -> TC.requireI [rtV `arrayOfRow` aw] a

    -- 4.
    TC.consume =<< TC.lookupAliases a

  -- 5.
  arrargs <- TC.checkSOACArrayArgs w ivs
  TC.checkLambda lam arrargs


typeCheckSOAC (Screma w (ScremaForm (scan_lam, scan_nes) (_, red_lam, red_nes) map_lam) arrs) = do
  TC.require [Prim int32] w
  arrs' <- TC.checkSOACArrayArgs w arrs
  scan_nes' <- mapM TC.checkArg scan_nes
  red_nes' <- mapM TC.checkArg red_nes
  TC.checkLambda map_lam $ map TC.noArgAliases arrs'
  TC.checkLambda scan_lam $ map TC.noArgAliases $ scan_nes' ++ scan_nes'
  TC.checkLambda red_lam $ map TC.noArgAliases $ red_nes' ++ red_nes'
  let scan_t = map TC.argType scan_nes'
      red_t = map TC.argType red_nes'
      map_lam_ts = lambdaReturnType map_lam

  unless (scan_t == lambdaReturnType scan_lam) $
    TC.bad $ TC.TypeError $ "Scan function returns type " ++
    prettyTuple (lambdaReturnType scan_lam) ++ " but neutral element has type " ++
    prettyTuple scan_t

  unless (red_t == lambdaReturnType red_lam) $
    TC.bad $ TC.TypeError $ "Reduce function returns type " ++
    prettyTuple (lambdaReturnType red_lam) ++ " but neutral element has type " ++
    prettyTuple red_t

  unless (take (length scan_nes + length red_nes) map_lam_ts ==
          map TC.argType (scan_nes'++ red_nes')) $
    TC.bad $ TC.TypeError $ "Map function return type " ++ prettyTuple map_lam_ts ++
    " wrong for given scan and reduction functions."

-- | Get Stream's accumulators as a sub-expression list
getStreamAccums :: StreamForm lore -> [SubExp]
getStreamAccums (Parallel _ _ _ accs) = accs
getStreamAccums (Sequential  accs) = accs

getStreamOrder :: StreamForm lore -> StreamOrd
getStreamOrder (Parallel o _ _ _) = o
getStreamOrder (Sequential  _) = InOrder

instance OpMetrics (Op lore) => OpMetrics (SOAC lore) where
  opMetrics (Stream _ _ lam _) =
    inside "Stream" $ lambdaMetrics lam
  opMetrics (Scatter _len lam _ivs _as) =
    inside "Scatter" $ lambdaMetrics lam
  opMetrics (Screma _ (ScremaForm (scan_lam, _) (_, red_lam, _) map_lam) _) =
    inside "Screma" $
    lambdaMetrics scan_lam >> lambdaMetrics red_lam >> lambdaMetrics map_lam
  opMetrics CmpThreshold{} = seen "CmpThreshold"

instance PrettyLore lore => PP.Pretty (SOAC lore) where
  ppr (Stream size form lam arrs) =
    case form of
       Parallel o comm lam0 acc ->
         let ord_str = if o == Disorder then "Per" else ""
             comm_str = case comm of Commutative -> "Comm"
                                     Noncommutative -> ""
         in  text ("streamPar"++ord_str++comm_str) <>
             parens (ppr size <> comma </> ppr lam0 </> comma </> ppr lam </>
                        commasep ( PP.braces (commasep $ map ppr acc) : map ppr arrs ))
       Sequential acc ->
             text "streamSeq" <>
             parens (ppr size <> comma </> ppr lam <> comma </>
                        commasep ( PP.braces (commasep $ map ppr acc) : map ppr arrs ))
  ppr (Scatter len lam ivs as) =
    ppSOAC "scatter" len [lam] (Just (map Var ivs)) (map (\(_,n,a) -> (n,a)) as)

  ppr (Screma w (ScremaForm (scan_lam, scan_nes) (_, red_lam, red_nes) map_lam) arrs)
    | isNilFn scan_lam, null scan_nes,
      isNilFn red_lam, null red_nes =
        text "map" <> parens (ppr w <> comma </>
                              ppr map_lam <> comma </>
                              commasep (map ppr arrs))

    | isNilFn scan_lam, null scan_nes =
        text "redomap" <> parens (ppr w <> comma </>
                                  ppr red_lam <> comma </>
                                  commasep (map ppr red_nes) <> comma </>
                                  ppr map_lam <> comma </>
                                  commasep (map ppr arrs))

    | isNilFn red_lam, null red_nes =
        text "scanomap" <> parens (ppr w <> comma </>
                                   ppr scan_lam <> comma </>
                                   commasep (map ppr scan_nes) <> comma </>
                                   ppr map_lam <> comma </>
                                   commasep (map ppr arrs))

  ppr (Screma w form arrs) = ppScrema w form arrs
  ppr (CmpThreshold what s) = text "cmpThreshold(" <> ppr what <> comma PP.<+> text (show s) <> text ")"

ppScrema :: (PrettyLore lore, Pretty inp) =>
              SubExp -> ScremaForm lore -> [inp] -> Doc
ppScrema w (ScremaForm (scan_lam, scan_nes) (comm, red_lam, red_nes) map_lam) arrs =
  text s <> parens (ppr w <> comma </>
                      ppr scan_lam <> comma </>
                      PP.braces (commasep $ map ppr scan_nes) </>
                      ppr red_lam <> comma </>
                      PP.braces (commasep $ map ppr red_nes) </>
                      ppr map_lam <> comma </>
                      commasep (map ppr arrs))
    where s = case comm of Noncommutative -> "screma"
                           Commutative -> "scremaComm"

ppSOAC :: (Pretty fn, Pretty v) =>
          String -> SubExp -> [fn] -> Maybe [SubExp] -> [v] -> Doc
ppSOAC name size funs es as =
  text name <> parens (ppr size <> comma </>
                       ppList funs </>
                       commasep (es' ++ map ppr as))
  where es' = maybe [] ((:[]) . ppTuple') es

ppList :: Pretty a => [a] -> Doc
ppList as = case map ppr as of
              []     -> mempty
              a':as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'
