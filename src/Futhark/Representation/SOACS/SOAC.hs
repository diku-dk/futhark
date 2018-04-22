{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.SOACS.SOAC
       ( SOAC(..)
       , StreamForm(..)

       , typeCheckSOAC

         -- * Utility
       , getStreamOrder
       , getStreamAccums

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
import Futhark.Util.Pretty
  ((</>), ppr, comma, commasep, Doc, Pretty, parens, text)
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
import Futhark.Util (maybeNth, chunks)

data SOAC lore =
    Map SubExp (LambdaT lore) [VName]
  | Reduce SubExp Commutativity (LambdaT lore) [(SubExp, VName)]
  | Scan SubExp (LambdaT lore) [(SubExp, VName)]
  -- ^ @Scan cs w lam input@ where @(nes, arrs) = unzip input@.
  --
  -- Performs an inclusive scan on the input arrays @arrs@ (that must all have
  -- outer length @w@), using the binary operator defined in @lam@ and the
  -- neutral elements @nes@.
  --
  -- Inclusive scan means the result from scanning array @xs@ with operator @op@
  -- will be @[xs[0], xs[0] op xs[1], ..., x0 op x1 op ... op x[w-1] ]@
  | Redomap SubExp Commutativity (LambdaT lore) (LambdaT lore) [SubExp] [VName]
  | Scanomap SubExp (LambdaT lore) (LambdaT lore) [SubExp] [VName]
  | Stream SubExp (StreamForm lore) (LambdaT lore) [VName]
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
    deriving (Eq, Ord, Show)

data StreamForm lore  =
    Parallel StreamOrd Commutativity (LambdaT lore) [SubExp]
  | Sequential [SubExp]
  deriving (Eq, Ord, Show)

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
mapSOACM tv (Map w lam arrs) =
  Map <$> mapOnSOACSubExp tv w <*>
  mapOnSOACLambda tv lam <*> mapM (mapOnSOACVName tv) arrs
mapSOACM tv (Reduce w comm lam input) =
  Reduce <$> mapOnSOACSubExp tv w <*>
  pure comm <*> mapOnSOACLambda tv lam <*>
  (zip <$> mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs)
  where (nes, arrs) = unzip input
mapSOACM tv (Scan w lam input) =
  Scan <$> mapOnSOACSubExp tv w <*> mapOnSOACLambda tv lam <*>
  (zip <$> mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs)
  where (nes, arrs) = unzip input
mapSOACM tv (Redomap w comm lam0 lam1 nes arrs) =
  Redomap <$> mapOnSOACSubExp tv w <*> pure comm <*>
  mapOnSOACLambda tv lam0 <*> mapOnSOACLambda tv lam1 <*>
  mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs
mapSOACM tv (Scanomap w lam0 lam1 nes arrs) =
  Scanomap <$> mapOnSOACSubExp tv w <*>
  mapOnSOACLambda tv lam0 <*> mapOnSOACLambda tv lam1 <*>
  mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs
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

soacType :: SOAC lore -> [ExtType]
soacType (Map size f _) =
  staticShapes $ mapType size f
soacType (Reduce _ _ fun _) =
  staticShapes $ lambdaReturnType fun
soacType (Scan width lam _) =
  staticShapes $ map (`arrayOfRow` width) $ lambdaReturnType lam
soacType (Redomap outersize _ outerfun innerfun _ _) =
  staticShapes $
  let acc_tp    = lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
  in  case res_el_tp of
        [] -> acc_tp
        _  -> acc_tp ++ map (`arrayOfRow` outersize) res_el_tp
soacType (Scanomap outersize outerfun innerfun _ _) =
  staticShapes $
  let acc_tp    = map (`arrayOfRow` outersize) $ lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
  in  case res_el_tp of
        [] -> acc_tp
        _  -> acc_tp ++ map (`arrayOfRow` outersize) res_el_tp
soacType (Stream outersize form lam _) =
  staticShapes $ map (substNamesInType substs) rtp
  where nms = map paramName $ take (1 + length accs) params
        substs = M.fromList $ zip nms (outersize:accs)
        Lambda params _ rtp = lam
        accs = case form of
                Parallel _ _ _ acc -> acc
                Sequential  acc -> acc
soacType (Scatter _w lam _ivs as) =
  staticShapes $ zipWith arrayOfRow val_ts ws
  where val_ts = concatMap (take 1) $ chunks ns $
                 drop (sum ns) $ lambdaReturnType lam
        (ws, ns, _) = unzip3 as

instance TypedOp (SOAC lore) where
  opType = pure . soacType

instance (Attributes lore, Aliased lore) => AliasedOp (SOAC lore) where
  opAliases (Map _ f _) =
    map (const mempty) $ lambdaReturnType f
  opAliases (Reduce _ _ f _) =
    map (const mempty) $ lambdaReturnType f
  opAliases (Scan _ f _) =
    map (const mempty) $ lambdaReturnType f
  opAliases (Redomap _ _ _ innerfun _ _) =
    map (const mempty) $ lambdaReturnType innerfun
  opAliases (Scanomap _ _ innerfun _ _)  =
    map (const mempty) $ lambdaReturnType innerfun
  opAliases (Stream _ form lam _) =
    let a1 = case form of
               Parallel _ _ lam0 _ -> map (const mempty) $ lambdaReturnType lam0
               Sequential _       -> []
    in a1 ++ map (const mempty) (lambdaReturnType lam)
  opAliases (Scatter _len lam _ivs _as) =
    map (const mempty) $ lambdaReturnType lam

  -- Only Map, Redomap and Stream can consume anything.  The operands
  -- to Scan and Reduce functions are always considered "fresh".
  consumedInOp (Map _ lam arrs) =
    S.map consumedArray $ consumedByLambda lam
    where consumedArray v = fromMaybe v $ lookup v params_to_arrs
          params_to_arrs = zip (map paramName (lambdaParams lam)) arrs
  consumedInOp (Redomap _ _ foldlam _ nes arrs) =
    S.map consumedArray $ consumedByLambda foldlam
    where consumedArray v = fromMaybe v $ lookup v params_to_arrs
          params_to_arrs = zip (map paramName $ drop (length nes) (lambdaParams foldlam)) arrs
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
  consumedInOp _ =
    mempty

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (SOAC lore) where
  type OpWithAliases (SOAC lore) = SOAC (Aliases lore)

  addOpAliases (Map size lam args) =
    Map size (Alias.analyseLambda lam) args
  addOpAliases (Reduce size comm lam input) =
    Reduce size comm (Alias.analyseLambda lam) input
  addOpAliases (Scan size lam input) =
    Scan size (Alias.analyseLambda lam) input
  addOpAliases (Redomap size comm outerlam innerlam acc arr) =
    Redomap size
     comm (Alias.analyseLambda outerlam)
     (Alias.analyseLambda innerlam)
     acc arr
  addOpAliases (Scanomap size outerlam innerlam acc arr) =
    Scanomap size (Alias.analyseLambda outerlam)
     (Alias.analyseLambda innerlam)
     acc arr
  addOpAliases (Stream size form lam arr) =
    Stream size (analyseStreamForm form)
    (Alias.analyseLambda lam) arr
    where analyseStreamForm (Parallel o comm lam0 acc) =
              Parallel o comm (Alias.analyseLambda lam0) acc
          analyseStreamForm (Sequential acc) = Sequential acc
  addOpAliases (Scatter len lam ivs as) =
    Scatter len (Alias.analyseLambda lam) ivs as

  removeOpAliases = runIdentity . mapSOACM remove
    where remove = SOACMapper return (return . removeLambdaAliases) return

instance Attributes lore => IsOp (SOAC lore) where
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
  addOpRanges (Map w lam args) =
    Map w (Range.runRangeM $ Range.analyseLambda lam) args
  addOpRanges (Reduce w comm lam input) =
    Reduce w comm (Range.runRangeM $ Range.analyseLambda lam) input
  addOpRanges (Scan w lam input) =
    Scan w (Range.runRangeM $ Range.analyseLambda lam) input
  addOpRanges (Redomap w comm outerlam innerlam acc arr) =
    Redomap w comm
     (Range.runRangeM $ Range.analyseLambda outerlam)
     (Range.runRangeM $ Range.analyseLambda innerlam)
     acc arr
  addOpRanges (Scanomap w outerlam innerlam acc arr) =
    Scanomap w
     (Range.runRangeM $ Range.analyseLambda outerlam)
     (Range.runRangeM $ Range.analyseLambda innerlam)
     acc arr
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
      where lambdaAndSubExp (Map _ lam arrs) =
              nthMapOut 0 lam arrs
            lambdaAndSubExp (Redomap _ _ _ lam nes arrs) =
              nthMapOut (length nes) lam arrs
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
  usageInOp (Map _ f arrs) = usageInLambda f arrs
  usageInOp (Redomap _ _ _ f _ arrs) = usageInLambda f arrs
  usageInOp _ = mempty

typeCheckSOAC :: TC.Checkable lore => SOAC (Aliases lore) -> TC.TypeM lore ()
typeCheckSOAC (Map size fun arrexps) = do
  TC.require [Prim int32] size
  arrargs <- TC.checkSOACArrayArgs size arrexps
  TC.checkLambda fun arrargs

typeCheckSOAC (Redomap size _ outerfun innerfun accexps arrexps) =
  typeCheckScanomapRedomap size outerfun innerfun accexps arrexps
typeCheckSOAC (Scanomap size outerfun innerfun accexps arrexps) =
  typeCheckScanomapRedomap size outerfun innerfun accexps arrexps
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


typeCheckSOAC (Reduce size _ fun inputs) =
  typeCheckScanReduce size fun inputs

typeCheckSOAC (Scan size fun inputs) =
  typeCheckScanReduce size fun inputs

typeCheckScanReduce :: TC.Checkable lore =>
                       SubExp
                    -> Lambda (Aliases lore)
                    -> [(SubExp, VName)]
                    -> TC.TypeM lore ()
typeCheckScanReduce size fun inputs = do
  let (startexps, arrexps) = unzip inputs
  TC.require [Prim int32] size
  startargs <- mapM TC.checkArg startexps
  arrargs   <- TC.checkSOACArrayArgs size arrexps
  TC.checkLambda fun $ map TC.noArgAliases $ startargs ++ arrargs
  let startt      = map TC.argType startargs
      intupletype = map TC.argType arrargs
      funret      = lambdaReturnType fun
  unless (startt == funret) $
    TC.bad $ TC.TypeError $
    "Initial value is of type " ++ prettyTuple startt ++
    ", but function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype == funret) $
    TC.bad $ TC.TypeError $
    "Array element value is of type " ++ prettyTuple intupletype ++
    ", but function returns type " ++ prettyTuple funret ++ "."

typeCheckScanomapRedomap :: TC.Checkable lore =>
                            SubExp
                         -> Lambda (Aliases lore)
                         -> Lambda (Aliases lore)
                         -> [SubExp]
                         -> [VName]
                         -> TC.TypeM lore ()
typeCheckScanomapRedomap size outerfun innerfun accexps arrexps = do
  TC.require [Prim int32] size
  arrargs <- TC.checkSOACArrayArgs size arrexps
  accargs <- mapM TC.checkArg accexps
  TC.checkLambda innerfun arrargs
  let innerRetType = lambdaReturnType innerfun
      innerAccType = take (length accexps) innerRetType
      asArg t = (t, mempty)
  TC.checkLambda outerfun $ map asArg $ innerAccType ++ innerAccType
  let acct = map TC.argType accargs
      outerRetType = lambdaReturnType outerfun
  unless (acct == innerAccType ) $
    TC.bad $ TC.TypeError $ "Initial value is of type " ++ prettyTuple acct ++
          ", but reduction function returns type " ++ prettyTuple innerRetType ++ "."
  unless (acct == outerRetType) $
    TC.bad $ TC.TypeError $ "Initial value is of type " ++ prettyTuple acct ++
          ", but fold function returns type " ++ prettyTuple outerRetType ++ "."

-- | Get Stream's accumulators as a sub-expression list
getStreamAccums :: StreamForm lore -> [SubExp]
getStreamAccums (Parallel _ _ _ accs) = accs
getStreamAccums (Sequential  accs) = accs

getStreamOrder :: StreamForm lore -> StreamOrd
getStreamOrder (Parallel o _ _ _) = o
getStreamOrder (Sequential  _) = InOrder

instance OpMetrics (Op lore) => OpMetrics (SOAC lore) where
  opMetrics (Map _ fun _) =
    inside "Map" $ lambdaMetrics fun
  opMetrics (Reduce _ _ fun _) =
    inside "Reduce" $ lambdaMetrics fun
  opMetrics (Scan _ fun _) =
    inside "Scan" $ lambdaMetrics fun
  opMetrics (Redomap _ _ fun1 fun2 _ _) =
    inside "Redomap" $ lambdaMetrics fun1 >> lambdaMetrics fun2
  opMetrics (Scanomap _ fun1 fun2 _ _) =
    inside "Scanomap" $ lambdaMetrics fun1 >> lambdaMetrics fun2
  opMetrics (Stream _ _ lam _) =
    inside "Stream" $ lambdaMetrics lam
  opMetrics (Scatter _len lam _ivs _as) =
    inside "Scatter" $ lambdaMetrics lam

instance PrettyLore lore => PP.Pretty (SOAC lore) where
  ppr (Map size lam as) =
    ppSOAC "map" size [lam] Nothing as
  ppr (Reduce size comm lam inputs) =
    ppSOAC s size [lam] (Just es) as
    where (es, as) = unzip inputs
          s = case comm of Noncommutative -> "reduce"
                           Commutative -> "reduceComm"
  ppr (Redomap size comm outer inner es as) =
    text s <>
    parens (ppr size <> comma </>
               ppr outer <> comma </>
               ppr inner <> comma </>
               commasep (PP.braces (commasep $ map ppr es) : map ppr as))
    where s = case comm of Noncommutative -> "redomap"
                           Commutative -> "redomapComm"
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
  ppr (Scan size lam inputs) =
    ppSOAC "scan" size [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Scanomap size outer inner es as) =
    text "scanomap" <>
    parens (ppr size <> comma </>
               ppr outer <> comma </>
               ppr inner <> comma </>
               commasep (PP.braces (commasep $ map ppr es) : map ppr as))
  ppr (Scatter len lam ivs as) =
    ppSOAC "scatter" len [lam] (Just (map Var ivs)) (map (\(_,n,a) -> (n,a)) as)

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
