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

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import Prelude

import Futhark.Representation.AST
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Util.Pretty as PP
import Futhark.Util.Pretty
  ((</>), ppr, comma, commasep, Doc, Pretty, parens, text)
import qualified Futhark.Representation.AST.Pretty as PP
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Optimise.Simplifier.Lore
import Futhark.Representation.Ranges
  (Ranges, removeLambdaRanges, removeExtLambdaRanges)
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.Aliases
  (Aliases, removeLambdaAliases, removeExtLambdaAliases)
import Futhark.Analysis.Usage
import qualified Futhark.TypeCheck as TC
import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.Range as Range

data SOAC lore =
    Map Certificates SubExp (LambdaT lore) [VName]
  | Reduce Certificates SubExp Commutativity (LambdaT lore) [(SubExp, VName)]
  | Scan Certificates SubExp (LambdaT lore) [(SubExp, VName)]
  -- ^ @Scan cs w lam input@ where @(nes, arrs) = unzip input@.
  --
  -- Performs an inclusive scan on the input arrays @arrs@ (that must all have
  -- outer length @w@), using the binary operator defined in @lam@ and the
  -- neutral elements @nes@.
  --
  -- Inclusive scan means the result from scanning array @xs@ with operator @op@
  -- will be @[xs[0], xs[0] op xs[1], ..., x0 op x1 op ... op x[w-1] ]@
  | Redomap Certificates SubExp Commutativity (LambdaT lore) (LambdaT lore) [SubExp] [VName]
  | Scanomap Certificates SubExp (LambdaT lore) (LambdaT lore) [SubExp] [VName]
  | Stream Certificates SubExp (StreamForm lore) (ExtLambdaT lore) [VName]
  | Write Certificates SubExp (LambdaT lore) [VName] [(SubExp, VName)]
    -- Write <cs> <length> <lambda> <original index and value arrays>
    -- <input/output arrays along with their sizes>
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
    -- This must be consistent along all Write-related optimisations.
    --
    -- The original index arrays and value arrays are concatenated.
    deriving (Eq, Ord, Show)

data StreamForm lore  = MapLike    StreamOrd
  | RedLike    StreamOrd Commutativity (LambdaT lore) [SubExp]
  | Sequential [SubExp]
  deriving (Eq, Ord, Show)

-- | Like 'Mapper', but just for 'SOAC's.
data SOACMapper flore tlore m = SOACMapper {
    mapOnSOACSubExp :: SubExp -> m SubExp
  , mapOnSOACLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnSOACExtLambda :: ExtLambda flore -> m (ExtLambda tlore)
  , mapOnSOACVName :: VName -> m VName
  , mapOnSOACCertificates :: Certificates -> m Certificates
  }

-- | A mapper that simply returns the SOAC verbatim.
identitySOACMapper :: Monad m => SOACMapper lore lore m
identitySOACMapper = SOACMapper { mapOnSOACSubExp = return
                                , mapOnSOACLambda = return
                                , mapOnSOACExtLambda = return
                                , mapOnSOACVName = return
                                , mapOnSOACCertificates = return
                                }

-- | Map a monadic action across the immediate children of a
-- SOAC.  The mapping does not descend recursively into subexpressions
-- and is done left-to-right.
mapSOACM :: (Applicative m, Monad m) =>
            SOACMapper flore tlore m -> SOAC flore -> m (SOAC tlore)
mapSOACM tv (Map cs w lam arrs) =
  Map <$>
  mapOnSOACCertificates tv cs <*> mapOnSOACSubExp tv w <*>
  mapOnSOACLambda tv lam <*> mapM (mapOnSOACVName tv) arrs
mapSOACM tv (Reduce cs w comm lam input) =
  Reduce <$>
  mapOnSOACCertificates tv cs <*> mapOnSOACSubExp tv w <*>
  pure comm <*> mapOnSOACLambda tv lam <*>
  (zip <$> mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs)
  where (nes, arrs) = unzip input
mapSOACM tv (Scan cs w lam input) =
  Scan <$>
  mapOnSOACCertificates tv cs <*> mapOnSOACSubExp tv w <*>
  mapOnSOACLambda tv lam <*>
  (zip <$> mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs)
  where (nes, arrs) = unzip input
mapSOACM tv (Redomap cs w comm lam0 lam1 nes arrs) =
  Redomap <$>
  mapOnSOACCertificates tv cs <*> mapOnSOACSubExp tv w <*>
  pure comm <*>
  mapOnSOACLambda tv lam0 <*> mapOnSOACLambda tv lam1 <*>
  mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs
mapSOACM tv (Scanomap cs w lam0 lam1 nes arrs) =
  Scanomap <$>
  mapOnSOACCertificates tv cs <*> mapOnSOACSubExp tv w <*>
  mapOnSOACLambda tv lam0 <*> mapOnSOACLambda tv lam1 <*>
  mapM (mapOnSOACSubExp tv) nes <*> mapM (mapOnSOACVName tv) arrs
mapSOACM tv (Stream cs size form lam arrs) =
  Stream <$>
  mapOnSOACCertificates tv cs <*> mapOnSOACSubExp tv size <*>
  mapOnStreamForm form <*> mapOnSOACExtLambda tv lam <*>
  mapM (mapOnSOACVName tv) arrs
  where mapOnStreamForm (MapLike o) = pure $ MapLike o
        mapOnStreamForm (RedLike o comm lam0 acc) =
            RedLike <$> pure o  <*> pure comm <*>
            mapOnSOACLambda tv lam0 <*>
            mapM (mapOnSOACSubExp tv) acc
        mapOnStreamForm (Sequential acc) =
            Sequential <$> mapM (mapOnSOACSubExp tv) acc
mapSOACM tv (Write cs len lam ivs as) =
  Write
  <$> mapOnSOACCertificates tv cs
  <*> mapOnSOACSubExp tv len
  <*> mapOnSOACLambda tv lam
  <*> mapM (mapOnSOACVName tv) ivs
  <*> mapM (\(aw,a) -> (,) <$> mapOnSOACSubExp tv aw <*> mapOnSOACVName tv a) as

instance Attributes lore => FreeIn (SOAC lore) where
  freeIn = execWriter . mapSOACM free
    where walk f x = tell (f x) >> return x
          free = SOACMapper { mapOnSOACSubExp = walk freeIn
                            , mapOnSOACLambda = walk freeInLambda
                            , mapOnSOACExtLambda = walk freeInExtLambda
                            , mapOnSOACVName = walk freeIn
                            , mapOnSOACCertificates = walk freeIn
                            }

instance Attributes lore => Substitute (SOAC lore) where
  substituteNames subst =
    runIdentity . mapSOACM substitute
    where substitute =
            SOACMapper { mapOnSOACSubExp = return . substituteNames subst
                       , mapOnSOACLambda = return . substituteNames subst
                       , mapOnSOACExtLambda = return . substituteNames subst
                       , mapOnSOACVName = return . substituteNames subst
                       , mapOnSOACCertificates = return . substituteNames subst
                       }

instance Attributes lore => Rename (SOAC lore) where
  rename = mapSOACM renamer
    where renamer = SOACMapper rename rename rename rename rename

soacType :: SOAC lore -> [ExtType]
soacType (Map _ size f _) =
  staticShapes $ mapType size f
soacType (Reduce _ _ _ fun _) =
  staticShapes $ lambdaReturnType fun
soacType (Scan _ width lam _) =
  staticShapes $ map (`arrayOfRow` width) $ lambdaReturnType lam
soacType (Redomap _ outersize _ outerfun innerfun _ _) =
  staticShapes $
  let acc_tp    = lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
  in  case res_el_tp of
        [] -> acc_tp
        _  -> acc_tp ++ map (`arrayOfRow` outersize) res_el_tp
soacType (Scanomap _ outersize outerfun innerfun _ _) =
  staticShapes $
  let acc_tp    = map (`arrayOfRow` outersize) $ lambdaReturnType outerfun
      acc_el_tp = lambdaReturnType innerfun
      res_el_tp = drop (length acc_tp) acc_el_tp
  in  case res_el_tp of
        [] -> acc_tp
        _  -> acc_tp ++ map (`arrayOfRow` outersize) res_el_tp
soacType (Stream _ outersize form lam _) =
  map (substNamesInExtType substs) rtp
  where nms = map paramName $ take (1 + length accs) params
        substs = HM.fromList $ zip nms (outersize:accs)
        ExtLambda params _ rtp = lam
        accs = case form of
                MapLike _ -> []
                RedLike _ _ _ acc -> acc
                Sequential  acc -> acc
soacType (Write _cs _w lam _ivs as) =
  staticShapes $ zipWith arrayOfRow (snd $ splitAt (n `div` 2) lam_ts) ws
  where lam_ts = lambdaReturnType lam
        n = length lam_ts
        ws = map fst as

instance TypedOp (SOAC lore) where
  opType = pure . soacType

instance (Attributes lore, Aliased lore) => AliasedOp (SOAC lore) where
  opAliases (Map _ _ f _) =
    map (const mempty) $ lambdaReturnType f
  opAliases (Reduce _ _ _ f _) =
    map (const mempty) $ lambdaReturnType f
  opAliases (Scan _ _ f _) =
    map (const mempty) $ lambdaReturnType f
  opAliases (Redomap _ _ _ _ innerfun _ _) =
    map (const mempty) $ lambdaReturnType innerfun
  opAliases (Scanomap _ _ _ innerfun _ _)  =
    map (const mempty) $ lambdaReturnType innerfun
  opAliases (Stream _ _ form lam _) =
    let a1 = case form of
               MapLike _          -> []
               RedLike _ _ lam0 _ -> map (const mempty) $ lambdaReturnType lam0
               Sequential _       -> []
    in a1 ++ map (const mempty) (extLambdaReturnType lam)
  opAliases (Write _cs _len lam _ivs _as) =
    map (const mempty) $ lambdaReturnType lam

  -- Only Map, Redomap and Stream can consume anything.  The operands
  -- to Scan and Reduce functions are always considered "fresh".
  consumedInOp (Map _ _ lam arrs) =
    HS.map consumedArray $ consumedByLambda lam
    where consumedArray v = fromMaybe v $ lookup v params_to_arrs
          params_to_arrs = zip (map paramName (lambdaParams lam)) arrs
  consumedInOp (Redomap _ _ _ foldlam _ nes arrs) =
    HS.map consumedArray $ consumedByLambda foldlam
    where consumedArray v = fromMaybe v $ lookup v params_to_arrs
          params_to_arrs = zip (map paramName $ drop (length nes) (lambdaParams foldlam)) arrs
  consumedInOp (Stream _ _ form lam arrs) =
    HS.fromList $ subExpVars $
    case form of MapLike{} ->
                   map (consumedArray []) $ HS.toList $ consumedByExtLambda lam
                 Sequential accs ->
                   map (consumedArray accs) $ HS.toList $ consumedByExtLambda lam
                 RedLike _ _ _ accs ->
                   map (consumedArray accs) $ HS.toList $ consumedByExtLambda lam
    where consumedArray accs v = fromMaybe (Var v) $ lookup v $ paramsToInput accs
          -- Drop the chunk parameter, which cannot alias anything.
          paramsToInput accs = zip
                               (map paramName $ drop 1 $ extLambdaParams lam)
                               (accs++map Var arrs)
  consumedInOp (Write _ _ _ _ as) =
    HS.fromList $ map snd as
  consumedInOp _ =
    mempty

instance (Attributes lore,
          Attributes (Aliases lore),
          CanBeAliased (Op lore)) => CanBeAliased (SOAC lore) where
  type OpWithAliases (SOAC lore) = SOAC (Aliases lore)

  addOpAliases (Map cs size lam args) =
    Map cs size (Alias.analyseLambda lam) args
  addOpAliases (Reduce cs size comm lam input) =
    Reduce cs size comm (Alias.analyseLambda lam) input
  addOpAliases (Scan cs size lam input) =
    Scan cs size (Alias.analyseLambda lam) input
  addOpAliases (Redomap cs size comm outerlam innerlam acc arr) =
    Redomap cs size
     comm (Alias.analyseLambda outerlam)
     (Alias.analyseLambda innerlam)
     acc arr
  addOpAliases (Scanomap cs size outerlam innerlam acc arr) =
    Scanomap cs size (Alias.analyseLambda outerlam)
     (Alias.analyseLambda innerlam)
     acc arr
  addOpAliases (Stream cs size form lam arr) =
    Stream cs size (analyseStreamForm form)
    (Alias.analyseExtLambda lam) arr
    where analyseStreamForm (RedLike o comm lam0 acc) =
              RedLike o comm (Alias.analyseLambda lam0) acc
          analyseStreamForm (Sequential acc) = Sequential acc
          analyseStreamForm (MapLike    o  ) = MapLike    o
  addOpAliases (Write cs len lam ivs as) =
    Write cs len (Alias.analyseLambda lam) ivs as

  removeOpAliases = runIdentity . mapSOACM remove
    where remove = SOACMapper return (return . removeLambdaAliases)
                   (return . removeExtLambdaAliases) return return

instance Attributes lore => IsOp (SOAC lore) where
  safeOp _ = False
  cheapOp _ = True

substNamesInExtType :: HM.HashMap VName SubExp -> ExtType -> ExtType
substNamesInExtType _ tp@(Prim _) = tp
substNamesInExtType subs (Mem se space) =
  Mem (substNamesInSubExp subs se) space
substNamesInExtType subs (Array btp shp u) =
  let shp' = ExtShape $ map (substNamesInExtDimSize subs) (extShapeDims shp)
  in  Array btp shp' u
substNamesInSubExp :: HM.HashMap VName SubExp -> SubExp -> SubExp
substNamesInSubExp _ e@(Constant _) = e
substNamesInSubExp subs (Var idd) =
  HM.lookupDefault (Var idd) idd subs
substNamesInExtDimSize :: HM.HashMap VName SubExp -> ExtDimSize -> ExtDimSize
substNamesInExtDimSize _ (Ext o) = Ext o
substNamesInExtDimSize subs (Free o) = Free $ substNamesInSubExp subs o

instance (Ranged inner) => RangedOp (SOAC inner) where
  opRanges op = replicate (length $ soacType op) unknownRange

instance (Attributes lore, CanBeRanged (Op lore)) => CanBeRanged (SOAC lore) where
  type OpWithRanges (SOAC lore) = SOAC (Ranges lore)

  removeOpRanges = runIdentity . mapSOACM remove
    where remove = SOACMapper return (return . removeLambdaRanges)
                   (return . removeExtLambdaRanges) return return
  addOpRanges (Map cs w lam args) =
    Map cs w (Range.runRangeM $ Range.analyseLambda lam) args
  addOpRanges (Reduce cs w comm lam input) =
    Reduce cs w comm (Range.runRangeM $ Range.analyseLambda lam) input
  addOpRanges (Scan cs w lam input) =
    Scan cs w (Range.runRangeM $ Range.analyseLambda lam) input
  addOpRanges (Redomap cs w comm outerlam innerlam acc arr) =
    Redomap cs w comm
     (Range.runRangeM $ Range.analyseLambda outerlam)
     (Range.runRangeM $ Range.analyseLambda innerlam)
     acc arr
  addOpRanges (Scanomap cs w outerlam innerlam acc arr) =
    Scanomap cs w
     (Range.runRangeM $ Range.analyseLambda outerlam)
     (Range.runRangeM $ Range.analyseLambda innerlam)
     acc arr
  addOpRanges (Stream cs w form lam arr) =
    Stream cs w
    (Range.runRangeM $ analyseStreamForm form)
    (Range.runRangeM $ Range.analyseExtLambda lam)
    arr
    where analyseStreamForm (MapLike    o  ) =
            return $ MapLike o
          analyseStreamForm (Sequential acc) =
            return $ Sequential acc
          analyseStreamForm (RedLike o comm lam0 acc) = do
              lam0' <- Range.analyseLambda lam0
              return $ RedLike o comm lam0' acc
  addOpRanges (Write cs len lam ivs as) =
    Write cs len (Range.runRangeM $ Range.analyseLambda lam) ivs as

instance (Attributes lore, CanBeWise (Op lore)) => CanBeWise (SOAC lore) where
  type OpWithWisdom (SOAC lore) = SOAC (Wise lore)

  removeOpWisdom = runIdentity . mapSOACM remove
    where remove = SOACMapper return
                   (return . removeLambdaWisdom)
                   (return . removeExtLambdaWisdom)
                   return return

instance Aliased lore => UsageInOp (SOAC lore) where
  usageInOp (Map _ _ f arrs) = usageInLambda f arrs
  usageInOp (Redomap _ _ _ _ f _ arrs) = usageInLambda f arrs
  usageInOp _ = mempty

typeCheckSOAC :: TC.Checkable lore => SOAC (Aliases lore) -> TC.TypeM lore ()
typeCheckSOAC (Map cs size fun arrexps) = do
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] size
  arrargs <- TC.checkSOACArrayArgs size arrexps
  TC.checkLambda fun arrargs

typeCheckSOAC (Redomap ass size _ outerfun innerfun accexps arrexps) =
  typeCheckScanomapRedomap ass size outerfun innerfun accexps arrexps
typeCheckSOAC (Scanomap ass size outerfun innerfun accexps arrexps) =
  typeCheckScanomapRedomap ass size outerfun innerfun accexps arrexps
typeCheckSOAC (Stream ass size form lam arrexps) = do
  let accexps = getStreamAccums form
  mapM_ (TC.requireI [Prim Cert]) ass
  TC.require [Prim int32] size
  accargs <- mapM TC.checkArg accexps
  arrargs <- mapM lookupType arrexps
  _ <- TC.checkSOACArrayArgs size arrexps
  let chunk = head $ extLambdaParams lam
  let asArg t = (t, mempty)
      inttp   = Prim int32
      lamarrs'= map (`setOuterSize` Var (paramName chunk)) arrargs
  let acc_len= length accexps
  let lamrtp = take acc_len $ extLambdaReturnType lam
  unless (staticShapes (map TC.argType accargs) == lamrtp) $
    TC.bad $ TC.TypeError "Stream with inconsistent accumulator type in lambda."
  -- check reduce's lambda, if any
  _ <- case form of
        RedLike _ _ lam0 _ -> do
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
  (_,occurs) <- TC.collectOccurences $
                TC.checkExtLambda lam $ asArg inttp : accargs ++ fake_lamarrs'
  let usages = TC.usageMap occurs
  arr_aliases <- mapM TC.lookupAliases arrexps
  let aliased_syms = HS.toList $ HS.fromList $ concatMap HS.toList arr_aliases
  when (any (`HM.member` usages) aliased_syms) $
     TC.bad $ TC.TypeError "Stream with input array used inside lambda."
  -- check outerdim of Lambda's streamed-in array params are NOT specified,
  -- and that return type inner dimens are all specified but not as other
  -- lambda parameters!
  let lamarr_rtp = drop acc_len $ extLambdaReturnType lam
      lamarr_ptp = map paramType $ drop (acc_len+1) $ extLambdaParams lam
      names_lamparams = HS.fromList $ map paramName $ extLambdaParams lam
  _ <- mapM (checkOuterDim (paramName chunk) . head .    shapeDims . arrayShape) lamarr_ptp
  _ <- mapM (checkInnerDim names_lamparams   . tail . extShapeDims . arrayShape) lamarr_rtp
  return ()
    where checkOuterDim chunknm outdim = do
            let chunk_str = textual chunknm
            case outdim of
                    Constant _ ->
                      TC.bad $ TC.TypeError
                      ("Stream: outer dimension of stream should NOT"++
                       " be specified since it is "++chunk_str++"by default.")
                    Var idd    ->
                      unless (idd == chunknm) $
                      TC.bad $ TC.TypeError
                      ("Stream: outer dimension of stream should NOT"++
                       " be specified since it is "++chunk_str++"by default.")
          boundDim (Free (Var idd)) = return $ Just idd
          boundDim (Free _        ) = return Nothing
          boundDim (Ext  _        ) =
            TC.bad $ TC.TypeError $
            "Stream's lambda: inner dimensions of the"++
            " streamed-out arrays MUST be specified!"
          checkInnerDim lamparnms innerdims = do
            rtp_iner_syms <- catMaybes <$> mapM boundDim innerdims
            case find (`HS.member` lamparnms) rtp_iner_syms of
                Just name -> TC.bad $ TC.TypeError $
                             "Stream's lambda: " ++ pretty name ++
                             " cannot specify an inner result shape"
                _ -> return True

typeCheckSOAC (Write cs w lam ivs as) = do
  -- Requirements:
  --
  --   0. @lambdaReturnType@ of @lam@ must be a list
  --      [index types..., value types].
  --
  --   1. The number of index types must be equal to the number of value types
  --      and the number of arrays in @as@.
  --
  --   2. Each index type must have the type i32.
  --
  --   3. Each array pair in @as@ and the value types must have the same type
  --      (though not necessarily the same length).
  --
  --   4. Each array in @as@ is consumed.  This is not really a check, but more
  --      of a requirement, so that e.g. the source is not hoisted out of a
  --      loop, which will mean it cannot be consumed.
  --
  --   5. Each of ivs must be an array matching a corresponding lambda
  --      parameters.
  --
  -- Code:

  -- First check the certificates and input size.
  mapM_ (TC.requireI [Prim Cert]) cs
  TC.require [Prim int32] w

  -- 0.
  let rts = lambdaReturnType lam
      rtsLen = length rts `div` 2
      rtsI = take rtsLen rts
      rtsV = drop rtsLen rts

  -- 1.
  unless (rtsLen == length as)
    $ TC.bad $ TC.TypeError "Write: Uneven number of index types, value types, and I/O arrays."

  -- 2.
  forM_ rtsI $ \rtI -> unless (Prim int32 == rtI)
                       $ TC.bad $ TC.TypeError "Write: Index return type must be i32."

  forM_ (zip rtsV as) $ \(rtV, (aw, a)) -> do
    -- All lengths must have type i32.
    TC.require [Prim int32] aw

    -- 3.
    aType <- lookupType a
    case (rtV, rowType aType) of
      (Prim pt0, Prim pt1) | pt0 == pt1 ->
        return ()
      (Array pt0 _ _, Array pt1 _ _) | pt0 == pt1 ->
        return ()
      _ ->
        TC.bad $ TC.TypeError
        "Write values and input arrays do not have the same primitive type"

    -- 4.
    TC.consume =<< TC.lookupAliases a

  -- 5.
  arrargs <- TC.checkSOACArrayArgs w ivs
  TC.checkLambda lam arrargs


typeCheckSOAC (Reduce ass size _ fun inputs) =
  typeCheckScanReduce ass size fun inputs

typeCheckSOAC (Scan ass size fun inputs) =
  typeCheckScanReduce ass size fun inputs

typeCheckScanReduce :: TC.Checkable lore =>
                       Certificates
                    -> SubExp
                    -> Lambda (Aliases lore)
                    -> [(SubExp, VName)]
                    -> TC.TypeM lore ()
typeCheckScanReduce cs size fun inputs = do
  let (startexps, arrexps) = unzip inputs
  mapM_ (TC.requireI [Prim Cert]) cs
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
                            Certificates
                         -> SubExp
                         -> Lambda (Aliases lore)
                         -> Lambda (Aliases lore)
                         -> [SubExp]
                         -> [VName]
                         -> TC.TypeM lore ()
typeCheckScanomapRedomap ass size outerfun innerfun accexps arrexps = do
  mapM_ (TC.requireI [Prim Cert]) ass
  TC.require [Prim int32] size
  arrargs <- TC.checkSOACArrayArgs size arrexps
  accargs <- mapM TC.checkArg accexps
  TC.checkLambda innerfun $ map TC.noArgAliases accargs ++ arrargs
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
getStreamAccums (MapLike _       ) = []
getStreamAccums (RedLike _ _ _ accs) = accs
getStreamAccums (Sequential  accs) = accs

getStreamOrder :: StreamForm lore -> StreamOrd
getStreamOrder (MapLike o    ) = o
getStreamOrder (RedLike o _ _ _) = o
getStreamOrder (Sequential  _) = InOrder

instance OpMetrics (Op lore) => OpMetrics (SOAC lore) where
  opMetrics (Map _ _ fun _) =
    inside "Map" $ lambdaMetrics fun
  opMetrics (Reduce _ _ _ fun _) =
    inside "Reduce" $ lambdaMetrics fun
  opMetrics (Scan _ _ fun _) =
    inside "Scan" $ lambdaMetrics fun
  opMetrics (Redomap _ _ _ fun1 fun2 _ _) =
    inside "Redomap" $ lambdaMetrics fun1 >> lambdaMetrics fun2
  opMetrics (Scanomap _ _ fun1 fun2 _ _) =
    inside "Scanomap" $ lambdaMetrics fun1 >> lambdaMetrics fun2
  opMetrics (Stream _ _ _ lam _) =
    inside "Stream" $ extLambdaMetrics lam
  opMetrics (Write _cs _len lam _ivs _as) =
    inside "Write" $ lambdaMetrics lam

extLambdaMetrics :: OpMetrics (Op lore) => ExtLambda lore -> MetricsM ()
extLambdaMetrics = bodyMetrics . extLambdaBody

instance PrettyLore lore => PP.Pretty (SOAC lore) where
  ppr (Map cs size lam as) =
    PP.ppCertificates' cs <> ppSOAC "map" size [lam] Nothing as
  ppr (Reduce cs size comm lam inputs) =
    PP.ppCertificates' cs <> ppSOAC s size [lam] (Just es) as
    where (es, as) = unzip inputs
          s = case comm of Noncommutative -> "reduce"
                           Commutative -> "reduceComm"
  ppr (Redomap cs size comm outer inner es as) =
    PP.ppCertificates' cs <> text s <>
    parens (ppr size <> comma </>
               ppr outer <> comma </>
               ppr inner <> comma </>
               commasep (PP.braces (commasep $ map ppr es) : map ppr as))
    where s = case comm of Noncommutative -> "redomap"
                           Commutative -> "redomapComm"
  ppr (Stream cs size form lam arrs) =
    PP.ppCertificates' cs <> case form of
       MapLike o ->
         let ord_str = if o == Disorder then "Per" else ""
         in  text ("streamMap"++ord_str) <>
             parens (ppr size <> comma </> ppr lam <> comma </>
                        commasep (map ppr arrs) )
       RedLike o comm lam0 acc ->
         let ord_str = if o == Disorder then "Per" else ""
             comm_str = case comm of Commutative -> "Comm"
                                     Noncommutative -> ""
         in  text ("streamRed"++ord_str++comm_str) <>
             parens (ppr size <> comma </> ppr lam0 </> comma </> ppr lam </>
                        commasep ( PP.braces (commasep $ map ppr acc) : map ppr arrs ))
       Sequential acc ->
             text "streamSeq" <>
             parens (ppr size <> comma </> ppr lam <> comma </>
                        commasep ( PP.braces (commasep $ map ppr acc) : map ppr arrs ))
  ppr (Scan cs size lam inputs) =
    PP.ppCertificates' cs <> ppSOAC "scan" size [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Scanomap cs size outer inner es as) =
    PP.ppCertificates' cs <> text "scanomap" <>
    parens (ppr size <> comma </>
               ppr outer <> comma </>
               ppr inner <> comma </>
               commasep (PP.braces (commasep $ map ppr es) : map ppr as))
  ppr (Write cs len lam ivs as) =
    PP.ppCertificates' cs <> ppSOAC "write" len [lam] (Just (map Var ivs)) (map snd as)

ppSOAC :: Pretty fn => String -> SubExp -> [fn] -> Maybe [SubExp] -> [VName] -> Doc
ppSOAC name size funs es as =
  text name <> parens (ppr size <> comma </>
                       ppList funs </>
                       commasep (es' ++ map ppr as))
  where es' = maybe [] ((:[]) . ppTuple') es

ppList :: Pretty a => [a] -> Doc
ppList as = case map ppr as of
              []     -> mempty
              a':as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'
