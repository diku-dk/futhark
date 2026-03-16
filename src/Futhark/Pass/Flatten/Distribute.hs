module Futhark.Pass.Flatten.Distribute
  ( distributeMap,
    distributeBody,
    MapArray (..),
    mapArrayRowType,
    DistResults (..),
    DistRep,
    ResMap,
    Distributed (..),
    DistStm (..),
    DistBody (..),
    DistInput (..),
    DistInputs,
    DistType (..),
    distInputType,
    DistResult (..),
    ResTag (..),
  )
where

import Data.Bifunctor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.IR.SOACS
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty

newtype ResTag = ResTag Int
  deriving (Eq, Ord, Show)

-- | Something that is mapped.
data DistInput
  = -- | A value bound outside the original map nest.  By necessity
    -- regular.  The type is the parameter type.
    DistInputFree VName Type
  | -- | A value constructed inside the original map nest.  May be
    -- irregular.
    DistInput ResTag Type
  deriving (Eq, Ord, Show)

type DistInputs = [(VName, DistInput)]

nubInputs :: DistInputs -> DistInputs
nubInputs = L.nubBy (\a b -> fst a == fst b)

-- | The type of a 'DistInput'.  This corresponds to the parameter
-- type of the original map nest.
distInputType :: DistInput -> Type
distInputType (DistInputFree _ t) = t
distInputType (DistInput _ t) = t

data DistType
  = DistType
      -- | Outer regular size.
      SubExp
      -- | Irregular dimensions on top (but after the leading regular
      -- size).
      Rank
      -- | The regular "element type" - in the worst case, at least a
      -- scalar.
      Type
  deriving (Eq, Ord, Show)

data DistResult = DistResult {distResTag :: ResTag, distResType :: DistType, distResName :: VName}
  deriving (Eq, Ord, Show)

-- | The body of a distributed statement.
data DistBody
  = -- | A single statement that may involve parallel operations.
    SingleStm (Stm SOACS)
  | -- | Multiple scalar operations grouped into a single traversal
    ScalarBatch [Stm SOACS]
  deriving (Eq, Ord, Show)

distBodyStms :: DistBody -> [Stm SOACS]
distBodyStms (SingleStm stm) = [stm]
distBodyStms (ScalarBatch stms) = stms

data DistStm = DistStm
  { distStmInputs :: DistInputs,
    distStmResult :: [DistResult],
    distStmBody :: DistBody
  }
  deriving (Eq, Ord, Show)

distStmStms :: DistStm -> [Stm SOACS]
distStmStms = distBodyStms . distStmBody

-- | First element of tuple are certificates for this result.
--
-- Second is the name to which is should be bound.
--
-- Third is the element type (i.e. excluding shape of segments).
type ResMap = M.Map ResTag [([DistInput], VName, Type)]

-- | The results of a map-distribution that were free or identity
-- mapped in the original map function.  These correspond to plain
-- replicated arrays.
type DistRep = (VName, Either SubExp DistInput)

data DistResults = DistResults ResMap [DistRep]
  deriving (Eq, Ord, Show)

data Distributed = Distributed [DistStm] DistResults
  deriving (Eq, Ord, Show)

instance Pretty ResTag where
  pretty (ResTag x) = "r" <> pretty x

instance Pretty DistInput where
  pretty (DistInputFree v _) = pretty v
  pretty (DistInput rt _) = pretty rt

instance Pretty DistType where
  pretty (DistType w r t) =
    brackets (pretty w) <> pretty r <> pretty t

instance Pretty DistResult where
  pretty (DistResult rt t _) =
    pretty rt <> colon <+> pretty t

instance Pretty DistStm where
  pretty (DistStm inputs res stms) =
    "let" <+> ppTuple' (map pretty res) <+> "=" </> indent 2 stm'
    where
      stm' =
        "map"
          <+> nestedBlock
            ( stack $
                map onInput inputs
                  ++ map pretty (distBodyStms stms)
                  ++ [ "return" <+> ppTuple' (map pretty res)
                     ]
            )
      onInput (v, inp) =
        "for"
          <+> parens (pretty v <> colon <+> pretty (distInputType inp))
          <+> "<-"
          <+> pretty inp

instance Pretty Distributed where
  pretty (Distributed stms (DistResults resmap reps)) =
    stms' </> res'
    where
      res' = stack $ map onRes (M.toList resmap) <> map onRep reps
      stms' = stack $ map pretty stms
      onRes (rt, binds) =
        stack ["let" <+> pretty v <+> "=" <+> pretty rt | v <- binds]
      onRep (v, Left se) =
        "let" <+> pretty v <+> "=" <+> "rep" <> parens (pretty se)
      onRep (v, Right tag) =
        "let" <+> pretty v <+> "=" <+> "rep" <> parens (pretty tag)

resultMap :: [(VName, DistInput)] -> [DistStm] -> Pat Type -> Result -> ResMap
resultMap avail_inputs stms pat res = foldMap f $ concatMap distStmResult stms
  where
    pes = M.fromList [(patElemName pe, pe) | stm <- stms, pe <- concatMap (patElems . stmPat) (distStmStms stm)]
    f (DistResult rt _ v) =
      case maybe [] findRess $ M.lookup v pes of
        [] -> mempty
        binds -> M.singleton rt binds
    findRess (PatElem v v_t) = do
      (SubExpRes cs se, pv) <- zip res (patNames pat)
      if se == Var v
        then pure (map findCert (unCerts cs), pv, v_t)
        else []
    findCert v = fromMaybe (DistInputFree v (Prim Unit)) $ lookup v avail_inputs

splitIrregDims :: Names -> Type -> (Rank, Type)
splitIrregDims bound_outside (Array pt shape u) =
  let (reg, irreg) =
        first reverse $ span regDim $ reverse $ shapeDims shape
   in (Rank $ length irreg, Array pt (Shape reg) u)
  where
    regDim (Var v) = v `nameIn` bound_outside
    regDim Constant {} = True
splitIrregDims _ t = (mempty, t)

freeInput :: [(VName, DistInput)] -> VName -> Maybe (VName, DistInput)
freeInput avail_inputs v =
  (v,) <$> lookup v avail_inputs

patInput :: ResTag -> PatElem Type -> (VName, DistInput)
patInput tag pe =
  (patElemName pe, DistInput tag $ patElemType pe)

distributeBody ::
  Scope rep ->
  SubExp ->
  DistInputs ->
  Body SOACS ->
  (DistInputs, [DistStm])
distributeBody outer_scope w param_inputs body =
  let ((_, avail_inputs), stms) =
        L.mapAccumL distributeStm (ResTag (length param_inputs), param_inputs) $
          stmsToList $
            bodyStms body
   in (avail_inputs, groupScalarStms (bodyResult body) stms)
  where
    bound_outside = namesFromList $ M.keys outer_scope
    distType t = uncurry (DistType w) $ splitIrregDims bound_outside t
    distributeStm (ResTag tag, avail_inputs) stm =
      let pat = stmPat stm
          new_tags = map ResTag $ take (patSize pat) [tag ..]
          avail_inputs' =
            avail_inputs <> zipWith patInput new_tags (patElems pat)
          free_in_stm = freeIn stm
          used_free = mapMaybe (freeInput avail_inputs) $ namesToList free_in_stm
          used_free_types =
            mapMaybe (freeInput avail_inputs)
              . namesToList
              . foldMap (freeIn . distInputType . snd)
              $ used_free
          stm' =
            DistStm
              (nubInputs $ used_free_types <> used_free)
              (zipWith3 DistResult new_tags (map distType $ patTypes pat) (patNames pat))
              (SingleStm stm)
       in ((ResTag $ tag + length new_tags, avail_inputs'), stm')

-- | Is this a scalar operation that can be grouped with other scalar
-- operations into a single parallel traversal?
isScalarDistStm :: DistStm -> Bool
isScalarDistStm ds@(DistStm _ _ (SingleStm stm)) =
  not (isParallelStm stm) && hasRegularResults ds
isScalarDistStm _ = False

isParallelStm :: Stm SOACS -> Bool
isParallelStm stm = isMap (stmExp stm) && not ("sequential" `inAttrs` stmAuxAttrs (stmAux stm))
  where
    isMap BasicOp {} = False
    -- TODO: do better
    isMap Apply {} = True
    isMap Match {} = False
    isMap (Loop _ _ body) = (any isParallelStm . bodyStms) body
    isMap (WithAcc _ lam) = (any isParallelStm . bodyStms) $ lambdaBody lam
    isMap Op {} = True

hasRegularResults :: DistStm -> Bool
hasRegularResults (DistStm _ res _) =
  all isRegularRes res
  where
    isRegularRes (DistResult _ (DistType _ (Rank r) _) _) = r == 0

-- isUniformDistStm :: Names -> DistStm -> Bool
-- isUniformDistStm bound_outside (DistStm inps res _) =
--   all isUnifromRes res && all isUniformInp inps
--   where
--     isUnifromRes (DistResult _ (DistType _ (Rank r) _) _) = r == 0
--     isUniformInp (_, DistInputFree _ _) = True
--     isUniformInp (_, DistInput _ t) = fst (splitIrregDims bound_outside t) == mempty

groupScalarStms :: Result -> [DistStm] -> [DistStm]
groupScalarStms _ [] = []
groupScalarStms bodyRes (d : ds)
  | isScalarDistStm d =
      let (moreScalars, rest) = span isScalarDistStm ds
          scalars = d : moreScalars
       in mergeGroup bodyRes scalars rest : groupScalarStms bodyRes rest
  | otherwise = d : groupScalarStms bodyRes ds

-- | Merge a group of scalar 'DistStm's into a single one.
mergeGroup :: Result -> [DistStm] -> [DistStm] -> DistStm
mergeGroup _ [DistStm inp res body] _ = DistStm inp res body
mergeGroup bodyRes ds rest =
  let resTags =
        S.fromList $ concatMap (map distResTag . distStmResult) ds
      isInternal (_, DistInput rt _) = rt `S.member` resTags
      isInternal _ = False
      externalInputs =
        nubInputs $
          filter (not . isInternal) $
            concatMap distStmInputs ds
      externalResults =
        nubOrd $
          filter (isExternal bodyRes rest) $
            concatMap distStmResult ds
      allStms = concatMap distStmStms ds
   in DistStm externalInputs externalResults (ScalarBatch allStms)

-- | A result is external if it is used by a subsequent 'DistStm' or
-- by the body result.
isExternal :: Result -> [DistStm] -> DistResult -> Bool
isExternal bodyRes rest (DistResult rt _ rn) =
  rt `S.member` usedByRest || rn `S.member` bodyResVars || rn `S.member` bodyResCerts
  where
    usedByRest =
      S.fromList
        [rt' | (_, DistInput rt' _) <- concatMap distStmInputs rest]
    bodyResVars =
      S.fromList $
        mapMaybe
          ( \(SubExpRes _ se) -> case se of
              Var v -> Just v
              _ -> Nothing
          )
          bodyRes
    bodyResCerts =
      S.fromList $
        concatMap (\(SubExpRes cs _) -> unCerts cs) bodyRes

-- | The input we are mapping over in 'distributeMap'.
data MapArray t
  = -- | A straightforward array passed in to a
    -- top-level map.
    MapArray VName Type
  | -- | Something more exotic - distribution will assign it a
    -- 'ResTag', but not do anything else.  This is used to
    -- distributed nested maps whose inputs are produced in the outer
    -- nests.
    MapOther t Type

mapArrayRowType :: MapArray t -> Type
mapArrayRowType (MapArray _ t) = t
mapArrayRowType (MapOther _ t) = t

-- This is used to handle those results that are constants or lambda
-- parameters.
findReps :: [(VName, DistInput)] -> Pat Type -> Lambda SOACS -> [DistRep]
findReps avail_inputs map_pat lam =
  mapMaybe f $ zip (patElems map_pat) (bodyResult (lambdaBody lam))
  where
    f (pe, SubExpRes _ (Var v)) =
      case lookup v avail_inputs of
        Nothing -> Just (patElemName pe, Left $ Var v)
        Just inp
          | v `elem` map paramName (lambdaParams lam) ->
              Just (patElemName pe, Right inp)
          | otherwise -> Nothing
    f (pe, SubExpRes _ (Constant v)) = do
      Just (patElemName pe, Left $ Constant v)

distributeMap ::
  Scope rep ->
  Pat Type ->
  SubExp ->
  [MapArray t] ->
  Lambda SOACS ->
  (Distributed, M.Map ResTag t)
distributeMap outer_scope map_pat w arrs lam =
  let ((_, arrmap), param_inputs) =
        L.mapAccumL paramInput (ResTag 0, mempty) $
          zip (lambdaParams lam) arrs
      (avail_inputs, stms) =
        distributeBody outer_scope w param_inputs $ lambdaBody lam
      resmap =
        resultMap avail_inputs stms map_pat $
          bodyResult (lambdaBody lam)
      reps = findReps avail_inputs map_pat lam
   in ( Distributed stms $ DistResults resmap reps,
        arrmap
      )
  where
    paramInput (ResTag i, m) (p, MapArray arr _) =
      ( (ResTag i, m),
        (paramName p, DistInputFree arr $ paramType p)
      )
    paramInput (ResTag i, m) (p, MapOther x _) =
      ( (ResTag (i + 1), M.insert (ResTag i) x m),
        (paramName p, DistInput (ResTag i) $ paramType p)
      )
