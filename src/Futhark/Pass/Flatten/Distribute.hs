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
    DistStms,
    DistBody (..),
    DistInput (..),
    DistInputs,
    DistType (..),
    distInputType,
    DistResult (..),
    ResTag (..),
    FunHasParallelism,
    isRegularDistResult,
    isParallelStm,
    stmHasMeaningfulParallelism,

    -- * Segments
    Segments,
    segmentsShape,
    segmentsRank,
    segmentCount,
  )
where

import Data.Bifunctor
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.SOACS
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty

type Segments = NE.NonEmpty SubExp

type FunHasParallelism = Name -> Bool

segmentsShape :: Segments -> Shape
segmentsShape = Shape . NE.toList

segmentsRank :: Segments -> Int
segmentsRank = shapeRank . segmentsShape

segmentCount :: Segments -> TPrimExp Int64 VName
segmentCount = product . map pe64 . shapeDims . segmentsShape

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
      Segments
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
  = -- | A single statement That may involve parallel operations or produces non unifrom array.
    ParallelStm (Stm SOACS)
  | -- | Single or Multiple scalar operations grouped into a single traversal
    ScalarStm (Stms SOACS)
  deriving (Eq, Ord, Show)

distBodyStms :: DistBody -> Stms SOACS
distBodyStms (ParallelStm stm) = oneStm stm
distBodyStms (ScalarStm stms) = stms

data DistStm = DistStm
  { distStmInputs :: DistInputs,
    distStmResult :: [DistResult],
    distStmBody :: DistBody
  }
  deriving (Eq, Ord, Show)

distStmStms :: DistStm -> Stms SOACS
distStmStms = distBodyStms . distStmBody

-- | An efficient sequence of 'DistStm's.
type DistStms = Seq.Seq DistStm

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

data Distributed = Distributed DistStms DistResults
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
                  ++ map pretty (toList (distBodyStms stms))
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
      stms' = stack $ map pretty $ toList stms
      onRes (rt, binds) =
        stack ["let" <+> pretty v <+> "=" <+> pretty rt | v <- binds]
      onRep (v, Left se) =
        "let" <+> pretty v <+> "=" <+> "rep" <> parens (pretty se)
      onRep (v, Right tag) =
        "let" <+> pretty v <+> "=" <+> "rep" <> parens (pretty tag)

resultMap :: [(VName, DistInput)] -> DistStms -> Pat Type -> Result -> ResMap
resultMap avail_inputs stms pat res = foldMap (foldMap f . distStmResult) stms
  where
    pes = M.fromList $ do
      stm <- toList stms
      pe <- concatMap (patElems . stmPat) (distStmStms stm)
      pure (patElemName pe, pe)
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

nextResTag :: DistInputs -> ResTag
nextResTag = foldl' step (ResTag 0)
  where
    step next (_, DistInputFree _ _) =
      next
    step next (_, DistInput (ResTag i) _) =
      max next (ResTag (i + 1))

distributeBody ::
  FunHasParallelism ->
  Scope rep ->
  Segments ->
  DistInputs ->
  Body SOACS ->
  (DistInputs, DistStms)
distributeBody funHasParallelism outer_scope w param_inputs body = do
  let ((_, avail_inputs), stms) =
        L.mapAccumL distributeStm (nextResTag param_inputs, param_inputs) $
          bodyStms body
   in (avail_inputs, classifyStms funHasParallelism (bodyResult body) stms)
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
              (ParallelStm stm)
       in ((ResTag $ tag + length new_tags, avail_inputs'), stm')

isParallelDistStm :: (Stm SOACS -> Bool) -> DistStm -> Bool
isParallelDistStm stm_is_parallel (DistStm _ res (ParallelStm stm)) =
  stm_is_parallel stm || not (all isRegularDistResult res)
isParallelDistStm _ _ = False

noSequentialAttr :: Stm SOACS -> Bool
noSequentialAttr stm =
  not ("sequential" `inAttrs` stmAuxAttrs (stmAux stm))

-- | Does the statement contain meaningful parallelism - a SOAC or a call to
-- a parallel function, possibly nested inside sequential control flow?
-- Basic operations such as 'Iota' or 'Replicate' do not count. See Note
-- [Meaningful Parallelism].
stmHasMeaningfulParallelism :: FunHasParallelism -> Stm SOACS -> Bool
stmHasMeaningfulParallelism funHasParallelism = hasParallelism
  where
    hasParallelism stm' =
      noSequentialAttr stm'
        && case stmExp stm' of
          BasicOp _ -> False
          Apply fname _ _ _ -> funHasParallelism fname
          Match _ cases def_case _ ->
            any hasParallelism $
              bodyStms def_case
                <> mconcat (map (bodyStms . caseBody) cases)
          Loop _ _ body -> any hasParallelism (bodyStms body)
          WithAcc _ lam -> any hasParallelism (bodyStms (lambdaBody lam))
          Op op -> isParallelOp op

    isParallelOp JVP {} = error "stmHasMeaningfulParallelism: JVP"
    isParallelOp VJP {} = error "stmHasMeaningfulParallelism: VJP"
    isParallelOp _ = True

isParallelStm :: FunHasParallelism -> Stm SOACS -> Bool
isParallelStm funHasParallelism stm =
  noSequentialAttr stm
    && (parallelBasicOp (stmExp stm) || stmHasMeaningfulParallelism funHasParallelism stm)
  where
    parallelBasicOp (BasicOp op) = isParallelBasicOp op
    parallelBasicOp _ = False

    -- TODO: Check other operations
    isParallelBasicOp (Update _ _ slice _) = not $ null $ sliceDims slice
    isParallelBasicOp Concat {} = True
    isParallelBasicOp Iota {} = True
    isParallelBasicOp Replicate {} = True
    isParallelBasicOp ArrayLit {} = True
    isParallelBasicOp ArrayVal {} = True
    isParallelBasicOp (FlatUpdate _ flat_slice _) = not $ null $ flatSliceDims flat_slice
    isParallelBasicOp Manifest {} = True
    isParallelBasicOp Rearrange {} = True
    isParallelBasicOp Reshape {} = True
    isParallelBasicOp (FlatIndex _ flat_slice) = not $ null $ flatSliceDims flat_slice
    isParallelBasicOp (Index _ slice) = not $ null $ sliceDims slice
    isParallelBasicOp _ = False

isRegularDistResult :: DistResult -> Bool
isRegularDistResult (DistResult _ (DistType _ (Rank r) _) _) = r == 0

--  we should probably sort the DistStms first and we should assume they are sorted
-- and then given to this function.
classifyStms :: FunHasParallelism -> Result -> DistStms -> DistStms
classifyStms funHasParallelism bodyRes = classify
  where
    -- If no statement contains meaningful parallelism, treat the
    -- trivially parallel statements as sequential as well; statements
    -- with irregular results are still distributed, as
    -- 'isParallelDistStm' checks that regardless of the predicate.
    -- See Note [Meaningful Parallelism].
    classify ds
      | any meaningfulDistStm ds = groupStms (isParallelStm funHasParallelism) ds
      | otherwise = groupStms (const False) ds
    meaningfulDistStm (DistStm _ _ (ParallelStm stm)) =
      stmHasMeaningfulParallelism funHasParallelism stm
    meaningfulDistStm _ = False
    groupStms _ Seq.Empty = mempty
    groupStms stm_is_parallel ds' =
      let (scalars, rest) = Seq.breakl (isParallelDistStm stm_is_parallel) ds'
          scalar_grouped
            | not $ null scalars =
                Seq.singleton $ mergeGroup bodyRes scalars rest
            | otherwise = mempty
       in case rest of
            Seq.Empty -> scalar_grouped
            p Seq.:<| ps ->
              scalar_grouped <> (p Seq.<| groupStms stm_is_parallel ps)

-- | Merge a group of scalar 'DistStm's into a single one.
mergeGroup :: Result -> DistStms -> DistStms -> DistStm
mergeGroup bodyRes ds rest =
  let resTags =
        S.fromList $ concatMap (map distResTag . distStmResult) ds
      isInternal (_, DistInput rt _) = rt `S.member` resTags
      isInternal _ = False
      externalInputs =
        nubInputs $
          concatMap (filter (not . isInternal) . distStmInputs) ds
      externalResults =
        nubOrd $
          concatMap (filter (isExternal bodyRes rest) . distStmResult) ds
      allStms = foldMap distStmStms ds
   in DistStm externalInputs externalResults (ScalarStm allStms)

-- | A result is external if it is used by a subsequent 'DistStm' or
-- by the body result.
isExternal :: Result -> DistStms -> DistResult -> Bool
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
  FunHasParallelism ->
  Scope rep ->
  Pat Type ->
  Segments ->
  [MapArray t] ->
  Lambda SOACS ->
  (Distributed, M.Map ResTag t)
distributeMap funHasParallelism outer_scope map_pat w arrs lam =
  let ((_, arrmap), param_inputs) =
        L.mapAccumL paramInput (ResTag 0, mempty) $
          zip (lambdaParams lam) arrs
      (avail_inputs, stms) =
        distributeBody funHasParallelism outer_scope w param_inputs $ lambdaBody lam
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

-- Note [Meaningful Parallelism]
--
-- Many basic operations (Iota, Replicate, Concat, nontrivial slicing, and so
-- on) are parallel in principle, but distributing one on its own gains us
-- nothing: it performs the same work as a sequential per-thread traversal, at
-- the cost of manifesting intermediate arrays and launching extra kernels. Our
-- rule of thumb is that code contains *meaningful* parallelism only when it
-- contains at least a Screma, Hist, or a call to a parallel function - possibly
-- nested inside sequential control flow. This is what
-- 'stmHasMeaningfulParallelism' checks. It is intended to avoid "parallelising"
-- map bodies that are essentially sequential loops, but happen to use some
-- basic operations to construct arrays they operate on. This is a somewhat
-- crude classification, and it is possible to imagine a more sophisticated one
-- based on a cost-model. Note that incremental flattening (and auto-tuning)
-- might allow us to generate and pick the sequentialised versions anyway, but
-- it is more efficient to not generate them in the first place.
--
-- The notion is used in two places:
--
-- (1) When classifying the statements of a distributed body as parallel or
--     sequential ('classifyStms'): if no statement contains meaningful
--     parallelism, then the trivially parallel statements are also grouped
--     as sequential, so that ideally the entire body becomes a single
--     segmented operation.
--
-- (2) When deciding whether to multi-version a Screma
--     ('factorScremaForParallelism' in Futhark.Pass.Flatten.Incremental): a
--     fully flattened code version is worth generating only when the lambda
--     contains meaningful parallelism.
--
-- The exception is irregularity. A statement whose result shape varies
-- across the surrounding map nest (e.g. 'iota x' for a mapped 'x') cannot
-- simply be traversed sequentially per thread, as kernels cannot perform
-- irregular allocations - handling it is exactly what flattening is
-- for. Hence irregular results still count as meaningful: in (1) they force
-- distribution regardless of the classification (see 'isParallelDistStm'),
-- and in (2) they make versioning worthwhile
-- ('lambdaHasMeaningfulParallelism' in Futhark.Pass.Flatten.Incremental).
