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
    DistIrregularity (..),
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
import Data.Map qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.SOACS
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty

-- | Widths of the enclosing map-nest, outermost first. For top-level parallel
-- constructs, this is empty, which should be treated as an implicit
-- single-element segment (see 'segmentCount'). Generally, the empty-segments
-- case must be treated specially in some places, which is unfortunate, but it
-- helps unify code between the nested and top level cases.
type Segments = [SubExp]

type FunHasParallelism = Name -> Bool

-- | How to treat irregularity when classifying the statements of a distributed
-- body. This is mainly used to sequentialise nonuniform nested parallelism
-- instead of actually exploiting the parallelism, as the overhead of doing so
-- can sometimes be ruinous.
data DistIrregularity
  = -- | Distribute statements involving irregularity, relying on the machinery
    -- for flattening irregular arrays to handle them.
    DistributeIrregular
  | -- | Sequentialise BasicOps that involve only internal nonuniformity instead
    -- of distributing them. Used when generating intrablock code, where the
    -- machinery for flattening nonuniform nested parallelism would produce
    -- SegOps whose sizes are bound inside the kernel body, which makes the
    -- enclosing intrablock kernel infeasible ('noNonuniformPar' would reject
    -- it). Irregularity that escapes the enclosing map must still be
    -- distributed; if it occurs, the intrablock version is correctly rejected.
    SequentialiseIrregularBasicOps
  | -- | Sequentialise /any/ statement whose nonuniformity stays internal.
    SequentialiseIrregularAll
  deriving (Eq, Show)

segmentsShape :: Segments -> Shape
segmentsShape = Shape

segmentsRank :: Segments -> Int
segmentsRank = shapeRank . segmentsShape

segmentCount :: Segments -> TPrimExp Int64 VName
segmentCount = product . map pe64 . shapeDims . segmentsShape

newtype ResTag = ResTag Int
  deriving (Eq, Ord, Show)

-- | Something that is mapped.
data DistInput
  = -- | A value bound outside the original map-nest.  By necessity
    -- regular.  The type is the parameter type.
    DistInputFree VName Type
  | -- | A value constructed inside the original map-nest.  May be
    -- irregular.
    DistInput ResTag Type
  deriving (Eq, Ord, Show)

type DistInputs = [(VName, DistInput)]

nubInputs :: DistInputs -> DistInputs
nubInputs = L.nubBy (\a b -> fst a == fst b)

-- | The type of a 'DistInput'.  This corresponds to the parameter
-- type of the original map-nest.
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
  = -- | A single statement that may involve parallel operations or produce an
    -- irregular array.
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
  DistIrregularity ->
  FunHasParallelism ->
  Scope rep ->
  Segments ->
  DistInputs ->
  Body SOACS ->
  (DistInputs, DistStms)
distributeBody irreg_mode funHasParallelism outer_scope w param_inputs body = do
  let ((_, avail_inputs), stms) =
        L.mapAccumL distributeStm (nextResTag param_inputs, param_inputs) $
          bodyStms body
   in ( avail_inputs,
        classifyStms irreg_mode funHasParallelism (bodyResult body) stms
      )
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

    isParallelBasicOp (Update _ _ slice _) = not $ null $ sliceDims slice
    isParallelBasicOp Concat {} = True
    isParallelBasicOp Iota {} = True
    isParallelBasicOp Replicate {} = True
    isParallelBasicOp (FlatUpdate _ flat_slice _) = not $ null $ flatSliceDims flat_slice
    isParallelBasicOp Manifest {} = True
    isParallelBasicOp Rearrange {} = True
    isParallelBasicOp Reshape {} = True
    isParallelBasicOp (FlatIndex _ flat_slice) = not $ null $ flatSliceDims flat_slice
    isParallelBasicOp (Index _ slice) = not $ null $ sliceDims slice
    -- The work done by an 'ArrayLit' is its number of rows times the size of a
    -- row. Since the size is syntactic, we consider a literal of scalars to be
    -- always cheap enough for a single thread to construct.
    isParallelBasicOp (ArrayLit _ row_t) = arrayRank row_t > 0
    -- Now the sequential ones - we handle them explicitly so we will notice if
    -- we ever add a new one.
    isParallelBasicOp ArrayVal {} = False
    isParallelBasicOp Scratch {} = False
    isParallelBasicOp SubExp {} = False
    isParallelBasicOp Opaque {} = False
    isParallelBasicOp UnOp {} = False
    isParallelBasicOp BinOp {} = False
    isParallelBasicOp CmpOp {} = False
    isParallelBasicOp ConvOp {} = False
    isParallelBasicOp Assert {} = False
    isParallelBasicOp UpdateAcc {} = False
    isParallelBasicOp UserParam {} = False

isRegularDistResult :: DistResult -> Bool
isRegularDistResult (DistResult _ (DistType _ (Rank r) _) _) = r == 0

-- | Does the statement contain, inside a 'Loop' or 'Match', a statement whose
-- result is sized by a name bound within the enclosing statement itself? Such a
-- statement can never be executed sequentially inside a GPU kernel, as it
-- implies an allocation whose size cannot be computed before the kernel is
-- launched; only the machinery for flattening irregular arrays can handle it.
-- In contrast, sizes that are nonuniform merely by being variant to the
-- enclosing map-nest are fine, as memory expansion can compute those via
-- slicing. This is essentially a heuristic where we bet that slicing is
-- efficient; the fully principled stance would be to not allow any
-- nonuniformity. This is one of the criteria of 'mustDistribute' in
-- 'classifyStms'. See Note [Meaningful Parallelism].
stmHasNonuniformInside :: Stm SOACS -> Bool
stmHasNonuniformInside = inExp mempty . stmExp
  where
    nonuniform bound = any (`nameIn` bound) . subExpVars . arrayDims
    inExp bound e =
      case e of
        Loop merge form body ->
          inBody
            ( bound
                <> namesFromList (map (paramName . fst) merge)
                <> namesFromList (M.keys (scopeOfLoopForm form))
            )
            body
        Match _ cases def_body _ ->
          any (inBody bound) (def_body : map caseBody cases)
        WithAcc _ lam -> inBody bound (lambdaBody lam)
        _ -> False
    inBody bound body =
      any (inStm (bound <> boundInBody body)) $ bodyStms body
    inStm bound stm =
      any (nonuniform bound) (patTypes (stmPat stm))
        || inExp bound (stmExp stm)

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

groupStms ::
  (DistStm -> Bool) ->
  Result ->
  Seq.Seq DistStm ->
  Seq.Seq DistStm
groupStms _ _ Seq.Empty = mempty
groupStms dist_stm_is_parallel body_res ds' =
  let (scalars, rest) = Seq.breakl dist_stm_is_parallel ds'
      scalar_grouped
        | not $ null scalars =
            Seq.singleton $ mergeGroup body_res scalars rest
        | otherwise = mempty
   in case rest of
        Seq.Empty -> scalar_grouped
        p Seq.:<| ps ->
          scalar_grouped <> (p Seq.<| groupStms dist_stm_is_parallel body_res ps)

--  we should probably sort the DistStms first and we should assume they are sorted
-- and then given to this function.
classifyStms :: DistIrregularity -> FunHasParallelism -> Result -> DistStms -> DistStms
classifyStms irreg_mode funHasParallelism body_res = classify
  where
    -- Distribute the statements that are parallel, plus those that
    -- 'mustDistribute' regardless of parallelism. If no statement contains
    -- meaningful parallelism, no statement counts as parallel, so that the
    -- trivially parallel statements are treated as sequential as well. See Note
    -- [Meaningful Parallelism].
    --
    -- With 'SequentialiseIrregularBasicOps', nonuniform basic operations
    -- further count as parallel only when their irregular arrays escape the
    -- scalar group. With 'SequentialiseIrregularAll', this goes for any
    -- statement, not just basic operations.
    classify ds =
      let -- Which statements are candidates for sequentialising when their
          -- nonuniformity does not escape the scalar group.
          sequentialisable = case irreg_mode of
            DistributeIrregular -> const False
            SequentialiseIrregularBasicOps -> isBasicOpDistStm
            SequentialiseIrregularAll -> const True
          parallel
            | any meaningfulDistStm ds =
                \d ->
                  isParallelDistStm (isParallelStm funHasParallelism) d
                    && not (sequentialisable d && involvesIrregularity ds d)
            | otherwise = const False
          forced = mustDistribute (S.fromList $ filter parallel $ toList ds) ds
       in groupStms (\d -> parallel d || d `S.member` forced) body_res ds

    meaningfulDistStm d@(DistStm _ _ (ParallelStm stm)) =
      (isBasicOpDistStm d && any (`nameIn` freeIn body_res) (patNames (stmPat stm)))
        || stmHasMeaningfulParallelism funHasParallelism stm
    meaningfulDistStm _ = False

    isBasicOpDistStm (DistStm _ _ (ParallelStm (Let _ _ BasicOp {}))) = True
    isBasicOpDistStm _ = False

    distStmHasNonuniformInside (DistStm _ _ (ParallelStm stm)) =
      stmHasNonuniformInside stm
    distStmHasNonuniformInside _ = False

    -- Whether a statement produces an irregular array, or consumes
    -- one produced elsewhere in the body.
    involvesIrregularity ds d =
      not (all isRegularDistResult (distStmResult d))
        || any consumesIrregular (distStmInputs d)
      where
        consumesIrregular (_, DistInput rt _) = rt `S.member` irregular_tags
        consumesIrregular _ = False
        irregular_tags =
          S.fromList $
            map distResTag $
              filter (not . isRegularDistResult) $
                foldMap distStmResult ds

    -- The statements that require distribution regardless of whether they
    -- contain profitable parallelism, because a sequentially executed scalar
    -- group cannot handle their sizes:
    --
    -- (1) Statements whose irregular results are used outside the scalar
    --     group (by the body result, or transitively by another distributed
    --     statement), as arrays produced by sequentially executed groups
    --     must be regular.
    --
    -- (2) Compound statements (e.g. loops) with irregular results, even
    --     internally used ones: they contain allocations of nonuniform size
    --     that only flattening can handle, while a basic operation can
    --     reasonably be executed sequentially by a single thread when its
    --     result stays internal. This criterion does not apply under
    --     'SequentialiseIrregularAll', which is only imposed on user request,
    --     and may result in non-compileable code due to impossible memory
    --     expansion.
    --
    -- (3) Statements with sizes bound inside their own sequential control
    --     flow ('stmHasNonuniformInside').
    --
    -- The initial set of forced statements may be seeded with the statements
    -- already known to be distributed, such that irregular arrays they consume
    -- are also distributed.
    mustDistribute seed ds = fixpoint seed
      where
        body_res_names = freeIn body_res
        irregularResults stm =
          filter (not . isRegularDistResult) $ distStmResult stm
        forcedTags forced =
          S.fromList
            [rt | stm <- S.toList forced, (_, DistInput rt _) <- distStmInputs stm]
        compoundIrregular stm =
          irreg_mode /= SequentialiseIrregularAll
            && not (isBasicOpDistStm stm)
            && not (null (irregularResults stm))
        isForced forced stm =
          compoundIrregular stm
            || distStmHasNonuniformInside stm
            || any
              ( \r ->
                  distResName r `nameIn` body_res_names
                    || distResTag r `S.member` forcedTags forced
              )
              (irregularResults stm)
        fixpoint forced =
          let forced' = seed <> S.fromList (filter (isForced forced) $ toList ds)
           in if forced' == forced then forced else fixpoint forced'

-- | A result is external if it is used by a subsequent 'DistStm' or by the body
-- result.
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
  = -- | A straightforward array passed in to a top-level map.
    MapArray VName Type
  | -- | Something more exotic - distribution will assign it a 'ResTag', but not
    -- do anything else. This is used to distributed nested maps whose inputs
    -- are produced in the outer nests.
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
  DistIrregularity ->
  FunHasParallelism ->
  Scope rep ->
  Pat Type ->
  Segments ->
  [MapArray t] ->
  Lambda SOACS ->
  (Distributed, M.Map ResTag t)
distributeMap irreg_mode funHasParallelism outer_scope map_pat w arrs lam =
  let ((_, arrmap), param_inputs) =
        L.mapAccumL paramInput (ResTag 0, mempty) $
          zip (lambdaParams lam) arrs
      (avail_inputs, stms) =
        distributeBody irreg_mode funHasParallelism outer_scope w param_inputs $ lambdaBody lam
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
-- One exception is nonuniformity. A statement whose result shape varies
-- across the surrounding map-nest (e.g. 'iota x' for a mapped 'x') cannot
-- in general be traversed sequentially per thread, as arrays produced as
-- results of sequentially executed groups must be regular - handling it
-- is exactly what flattening is for. Hence irregular results still count
-- as meaningful, with one refinement in (1): if the irregular arrays are
-- used only *within* the scalar group (e.g. slices feeding a concatenation
-- whose size is ultimately uniform), the statements can be executed
-- sequentially after all - see 'mustDistribute' in 'classifyStms'. In (2)
-- irregular results make versioning worthwhile
-- ('lambdaHasMeaningfulParallelism' in Futhark.Pass.Flatten.Incremental).
--
-- The refinement above does not extend to nonuniformity arising *inside* a
-- sequential Loop or Match: an array sized by a loop-variant name implies an
-- allocation whose size cannot be computed before a kernel is launched, so a
-- statement containing one can never be part of a sequentially executed
-- scalar group, even if nothing irregular escapes it. Such statements are
-- unconditionally forced by 'mustDistribute' - see 'stmHasNonuniformInside'.
--
-- Another exception is when a trivial statement (a basic operation) produces
-- the result of a body - in that case we also treat it as meaningful, because
-- we have to manifest it. This does not extend to compound statements (e.g. a
-- sequential loop) producing a body result: those contain no parallelism worth
-- distributing on their own, and treating them as meaningful would needlessly
-- split any surrounding sequential statements into multiple kernels. This is
-- the part that would benefit from a cost model, or simply from being more
-- principled and ignoring the small inefficiencies.
