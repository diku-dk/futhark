{-# LANGUAGE FlexibleContexts #-}
module L0C.HOTrans.LoopKernel
  ( FusedKer(..)
  , newKernel
  , inputs
  , setInputs
  , arrInputs
  , OutputTransform(..)
  , applyTransform
  , attemptFusion
  , TryFusion
  , tryFusion
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad

import qualified Data.HashSet as HS

import Data.Maybe
import Data.List
import Data.Loc

import L0C.L0
import L0C.MonadFreshNames
import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest)
import qualified L0C.HORepresentation.SOACNest as Nest
import L0C.HORepresentation.MapNest (MapNest)
import qualified L0C.HORepresentation.MapNest as MapNest
import L0C.HOTrans.TryFusion
import L0C.HOTrans.Composing

data OutputTransform = OTranspose Certificates Int Int
                     | OReshape Certificates [Exp]
                     | OReshapeOuter Certificates [Exp]
                       deriving (Eq, Ord, Show)

applyTransform :: OutputTransform -> Ident -> SrcLoc -> Exp
applyTransform (OTranspose cs k n) v =
  Transpose cs k n $ Var v
applyTransform (OReshape cs shape) v =
  Reshape cs shape $ Var v
applyTransform (OReshapeOuter cs shape) v =
  Reshape cs (reshapeOuter shape 1 (Var v)) $ Var v

outputTransformToInputTransform :: OutputTransform -> SOAC.InputTransform
outputTransformToInputTransform (OTranspose cs k n) =
  SOAC.Transpose cs k n
outputTransformToInputTransform (OReshape cs shape) =
  SOAC.Reshape cs shape
outputTransformToInputTransform (OReshapeOuter cs shape) =
  SOAC.ReshapeOuter cs shape

outputsToInput :: [OutputTransform] -> SOAC.Input -> SOAC.Input
outputsToInput ots =
  SOAC.addTransforms $ map outputTransformToInputTransform ots

inputToOutput :: SOAC.Input -> Maybe (OutputTransform, SOAC.Input)
inputToOutput (SOAC.Input (SOAC.Transpose cs k n:ts) ia) =
  Just (OTranspose cs k n, SOAC.Input ts ia)
inputToOutput (SOAC.Input (SOAC.Reshape cs shape:ts) ia) =
  Just (OReshape cs shape, SOAC.Input ts ia)
inputToOutput (SOAC.Input (SOAC.ReshapeOuter cs shape:ts) ia) =
  Just (OReshapeOuter cs shape, SOAC.Input ts ia)
inputToOutput _                                          =
  Nothing

data FusedKer = FusedKer {
    fsoac      :: SOAC
  -- ^ the SOAC expression, e.g., mapT( f(a,b), x, y )

  , outputs    :: [Ident]
  -- ^ The names bound to the outputs of the SOAC.

  , inplace    :: HS.HashSet VName
  -- ^ every kernel maintains a set of variables
  -- that alias vars used in in-place updates,
  -- such that fusion is prevented to move
  -- a use of an

  , fusedVars :: [Ident]
  -- ^ whether at least a fusion has been performed.

  , outputTransform :: [OutputTransform]
  }
                deriving (Show)

newKernel :: [Ident] -> SOAC -> FusedKer
newKernel idds soac =
  FusedKer { fsoac = soac
           , outputs = idds
           , inplace = HS.empty
           , fusedVars = []
           , outputTransform = []
           }

arrInputs :: FusedKer -> HS.HashSet Ident
arrInputs = HS.fromList . mapMaybe SOAC.inputArray . inputs

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker { fsoac = inps `SOAC.setInputs` fsoac ker }

tryOptimizeSOAC :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
tryOptimizeSOAC outIds soac ker = do
  (soac', ots) <- liftMaybe $ optimizeSOAC Nothing soac
  let ker' = map (outputsToInput ots) (inputs ker) `setInputs` ker
  applyFusionRules outIds soac' ker'

tryOptimizeKernel :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
tryOptimizeKernel outIds soac ker = do
  ker' <- liftMaybe $ optimizeKernel (Just outIds) ker
  applyFusionRules outIds soac ker'

tryExposeInputs :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
tryExposeInputs outIds soac ker = do
  (ker', ots) <- exposeInputs outIds ker
  case ots of
    [] -> fuseSOACwithKer outIds soac ker'
    _  -> do
      soac' <- pullOutputTransforms soac ots
      applyFusionRules outIds soac' ker'

applyFusionRules :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
applyFusionRules outIds soac ker =
  tryOptimizeSOAC outIds soac ker <|>
  tryOptimizeKernel outIds soac ker <|>
  tryExposeInputs outIds soac ker <|>
  fuseSOACwithKer outIds soac ker

attemptFusion :: MonadFreshNames VName m =>
                 [Ident] -> SOAC -> FusedKer -> m (Maybe FusedKer)
attemptFusion outIds soac ker =
  liftM removeUnusedParamsFromKer <$>
  tryFusion (applyFusionRules outIds soac ker)

removeUnusedParamsFromKer :: FusedKer -> FusedKer
removeUnusedParamsFromKer ker =
  case soac of
    SOAC.MapT {}     -> ker { fsoac = soac' }
    SOAC.RedomapT {} -> ker { fsoac = soac' }
    _                -> ker
  where soac = fsoac ker
        l = SOAC.lambda soac
        inps = SOAC.inputs soac
        (l', inps') = removeUnusedParams l inps
        soac' = l' `SOAC.setLambda`
                (inps' `SOAC.setInputs` soac)

removeUnusedParams :: TupleLambda -> [SOAC.Input] -> (TupleLambda, [SOAC.Input])
removeUnusedParams l inps =
  (l { tupleLambdaParams = accParams ++ ps' }, inps')
  where allParams = tupleLambdaParams l
        (accParams, arrParams) =
          splitAt (length allParams - length inps) allParams
        pInps = zip arrParams inps
        (ps', inps') = case (unzip $ filter (used . fst) pInps, pInps) of
                         (([], []), (p,inp):_) -> ([p], [inp])
                         ((ps_, inps_), _)     -> (ps_, inps_)
        used p = identName p `HS.member` freeInBody
        freeInBody = freeNamesInExp $ tupleLambdaBody l

-- | Check that the consumer uses at least one output of the producer
-- unmodified.
mapFusionOK :: [Ident] -> FusedKer -> Bool
mapFusionOK outIds ker = any (`elem` inputs ker) (map SOAC.varInput outIds)

-- | Check that the input-array set of consumer is equal to the
-- output-array set of producer.  That is, a filter-producer can only
-- be fused with a filter or reduce-consumer if the consumer accepts
-- input from no other source, and consumes everything by the
-- producer.
filterFusionOK :: [Ident] -> FusedKer -> Bool
filterFusionOK outIds ker =
  case mapM SOAC.isVarInput $ inputs ker of
    Nothing       -> False
    Just inputIds -> all (`elem` outIds) inputIds &&
                     all (`elem` inputIds) outIds

-- | Check that the input-array set of consumer is contained in the
-- output-array set of producer.  That is, a filter-producer can only
-- be fused if the consumer accepts input from no other source.
filterFoldFusionOK :: [Ident] -> FusedKer -> Bool
filterFoldFusionOK outIds ker =
  case mapM SOAC.isVarInput $ inputs ker of
    Nothing       -> False
    Just inputIds -> all (`elem` outIds) inputIds

mapScanFusionOK :: [Ident] -> [SOAC.Input] -> FusedKer -> Bool
mapScanFusionOK outIds inp1 ker =
  length inp1 == length replacing &&
  and (zipWith subtypeOf
       (sort $ map (rowType . identType) replacing)
       (sort $ map (rowType . SOAC.inputType) inp1))
  where replacing = filter (`elem` outIds) $
                    mapMaybe SOAC.isVarInput $
                    inputs ker

mapOrFilter :: SOAC -> Bool
mapOrFilter (SOAC.FilterT {}) = True
mapOrFilter (SOAC.MapT {})    = True
mapOrFilter _                 = False

fuseSOACwithKer :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
fuseSOACwithKer outIds soac1 ker = do
  -- We are fusing soac1 into soac2, i.e, the output of soac1 is going
  -- into soac2.
  let soac2 = fsoac ker
      out_ids2 = outputs ker
      cs1      = SOAC.certificates soac1
      cs2      = SOAC.certificates soac2
      inp1_arr = SOAC.inputs soac1
      inp2_arr = SOAC.inputs soac2
      lam1     = SOAC.lambda soac1
      lam2     = SOAC.lambda soac2
      success res_soac = do
        let fusedVars_new = fusedVars ker++outIds
        return $ ker { fsoac = res_soac
                     , outputs = out_ids2
                     , fusedVars = fusedVars_new
                     }
  case (soac2, soac1) of
    -- The Fusions that are semantically map fusions:
    (SOAC.MapT _ _ _ pos, SOAC.MapT    {})
      | mapFusionOK outIds ker -> do
      let (res_lam, new_inp) = fuseMaps lam1 inp1_arr outIds lam2 inp2_arr
      success $ SOAC.MapT (cs1++cs2) res_lam new_inp pos

    (SOAC.RedomapT _ lam21 _ ne _ pos, SOAC.MapT {})
      | mapFusionOK outIds ker -> do
      let (res_lam, new_inp) = fuseMaps lam1 inp1_arr outIds lam2 inp2_arr
      success $ SOAC.RedomapT (cs1++cs2) lam21 res_lam ne new_inp pos

    (SOAC.ScanT _ _ inps2 loc, SOAC.MapT _ _ inps1 _)
      | mapScanFusionOK outIds inp1_arr ker -> do
      let (res_lam, new_inps) =
            fuseMaps lam1 (zip (map fst inps2) inps1) outIds lam2 inps2
      success $ SOAC.ScanT (cs1++cs2) res_lam new_inps loc

    -- The Fusions that are semantically filter fusions:
    (SOAC.ReduceT _ _ args pos, SOAC.FilterT {})
      | filterFusionOK outIds ker -> do
      let ne = map fst args
      name <- newVName "check"
      let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr outIds lam2 inp2_arr name
      success $ SOAC.ReduceT (cs1++cs2) res_lam (zip ne new_inp) pos

    (SOAC.RedomapT _ lam21 _ nes _ pos, SOAC.FilterT {})
      | filterFoldFusionOK outIds ker-> do
      name <- newVName "check"
      let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr outIds lam2 inp2_arr name
      success $ SOAC.RedomapT (cs1++cs2) lam21 res_lam nes new_inp pos

    (SOAC.FilterT _ _ _ pos, SOAC.FilterT {})
      | filterFusionOK outIds ker -> do
      name <- newVName "check"
      let (res_lam, new_inp) = fuseFilters lam1 inp1_arr outIds lam2 inp2_arr name
      success $ SOAC.FilterT (cs1++cs2) res_lam new_inp pos

    -- Nothing else worked, so let's try rewriting to redomap if
    -- possible.
    (SOAC.ReduceT _ lam args loc, _) | mapOrFilter soac1 -> do
       let (ne, arrs) = unzip args
           soac2' = SOAC.RedomapT (cs1++cs2) lam lam ne arrs loc
           ker'   = ker { fsoac = soac2'
                        , outputs = out_ids2 }
       fuseSOACwithKer outIds soac1 ker'

    _ -> fail "Cannot fuse"

-- Here follows optimizations and transforms to expose fusability.

optimizeKernel :: Maybe [Ident] -> FusedKer -> Maybe FusedKer
optimizeKernel inp ker = do
  (resNest, resTrans) <- optimizeSOACNest inp startNest startTrans
  Just ker { fsoac = Nest.toSOAC resNest
           , outputTransform = resTrans
           }
  where startNest = Nest.fromSOAC $ fsoac ker
        startTrans = outputTransform ker

optimizeSOAC :: Maybe [Ident] -> SOAC -> Maybe (SOAC, [OutputTransform])
optimizeSOAC inp soac =
  first Nest.toSOAC <$> optimizeSOACNest inp (Nest.fromSOAC soac) []

optimizeSOACNest :: Maybe [Ident] -> SOACNest -> [OutputTransform]
                 -> Maybe (SOACNest, [OutputTransform])
optimizeSOACNest inp soac os = case foldr comb (False, (soac, os)) optimizations of
                             (False, _)           -> Nothing
                             (True, (soac', os')) -> Just (soac', os')
  where comb f (changed, (soac', os')) =
          case f inp soac' os of
            Nothing             -> (changed, (soac',  os'))
            Just (soac'', os'') -> (True,    (soac'', os''))

type Optimization = Maybe [Ident] -> SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])

optimizations :: [Optimization]
optimizations = [iswim]

iswim :: Maybe [Ident] -> SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
iswim _ nest ots
  | Nest.ScanT cs1 (Nest.NewNest lvl nn) [] es@[_] loc1 <- Nest.operation nest,
    Nest.MapT cs2 mb [] loc2 <- nn,
    Just es' <- mapM SOAC.inputFromExp es,
    Nest.Nesting paramIds mapArrs bndIds postExp retTypes <- lvl,
    mapArrs == map SOAC.varInput paramIds =
    let toInnerAccParam idd = idd { identType = rowType $ identType idd }
        innerAccParams = map toInnerAccParam $ take (length es) paramIds
        innerArrParams = drop (length es) paramIds
        innerScan = ScanT cs2 (Nest.bodyToLambda mb)
                          (zip (map Var innerAccParams) (map Var innerArrParams))
                          loc1
        lam = TupleLambda {
                tupleLambdaParams = map toParam $ innerAccParams ++ innerArrParams
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody = Nest.letPattern bndIds innerScan postExp
              , tupleLambdaSrcLoc = loc2
              }
        transposeInput (SOAC.Input [SOAC.Transpose _ 0 1] ia) =
          SOAC.Input [] ia
        transposeInput (SOAC.Input ts                     ia) =
          SOAC.Input (ts++[SOAC.Transpose cs2 0 1]) ia
    in Just (Nest.SOACNest
               (es' ++ map transposeInput (Nest.inputs nest))
               (Nest.MapT cs1 (Nest.Lambda lam) [] loc2),
             ots ++ [OTranspose cs2 0 1])
iswim _ _ _ = Nothing

-- Now for fiddling with transpositions...

commonTransforms :: [Ident] -> [SOAC.Input]
                 -> ([OutputTransform], [SOAC.Input])
commonTransforms interesting inps = commonTransforms' inps'
  where inps' = [ (maybe False (`elem` interesting) $ SOAC.inputArray inp, inp)
                    | inp <- inps ]

commonTransforms' :: [(Bool, SOAC.Input)] -> ([OutputTransform], [SOAC.Input])
commonTransforms' inps =
  case foldM inspect (Nothing, []) inps of
    Just (Just mot, inps') -> first (mot:) $ commonTransforms' $ reverse inps'
    _                      -> ([], map snd inps)
  where inspect (mot, prev) (True, inp) =
          case (mot, inputToOutput inp) of
           (Nothing,  Just (ot, inp'))  -> Just (Just ot, (True, inp') : prev)
           (Just ot1, Just (ot2, inp'))
             | ot1 == ot2 -> Just (Just ot2, (True, inp') : prev)
           _              -> Nothing
        inspect (mot, prev) inp = Just (mot,inp:prev)

mapDepth :: MapNest -> Int
mapDepth (MapNest.MapNest _ _ levels _ _) =
  length (takeWhile MapNest.pureNest levels) + 1

pullTranspose :: SOACNest -> [OutputTransform] -> TryFusion (SOACNest, [OutputTransform])
pullTranspose nest ots = do
  nest' <- liftMaybeNeedNames $ MapNest.fromSOACNest nest
  OTranspose cs k n:ots' <- return ots
  if transposeDepth (k,n) < mapDepth nest' then
    let inputs' = map (SOAC.addTransform $ SOAC.Transpose cs k n) $ MapNest.inputs nest'
    in return (inputs' `Nest.setInputs` MapNest.toSOACNest nest',
               ots')
  else fail "Cannot pull transpose"

pushTranspose :: [Ident] -> SOACNest -> [OutputTransform]
              -> TryFusion (SOACNest, [OutputTransform])
pushTranspose inpIds nest ots = do
  nest' <- liftMaybeNeedNames $ MapNest.fromSOACNest nest
  (ts, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest'
  if transposeReach ts < mapDepth nest' then
    let outInvTrns = uncurry (OTranspose []) . uncurry transposeInverse
    in return (inputs' `Nest.setInputs` MapNest.toSOACNest nest',
               ots ++ map outInvTrns (reverse ts))
  else fail "Cannot push transpose"

transposeReach :: [(Int,Int)] -> Int
transposeReach = foldr (max . transposeDepth) 0

transposeDepth :: (Int, Int) -> Int
transposeDepth (k,n) = max k (k+n)

fixupInputs :: [Ident] -> [SOAC.Input] -> Maybe ([(Int,Int)], [SOAC.Input])
fixupInputs inpIds inps =
  case filter (not . null) $
       map (snd . SOAC.inputTransposes) $
       filter exposable inps of
    ts:_ -> do inps' <- mapM (fixupInput (transposeReach ts) ts) inps
               return (ts, inps')
    _    -> Nothing
  where exposable = maybe False (`elem` inpIds) . SOAC.inputArray

        fixupInput d ts inp
          | exposable inp = case SOAC.inputTransposes inp of
                              (inp', ts') | ts == ts' -> Just inp'
                                          | otherwise -> Nothing
          | arrayRank (SOAC.inputType inp) > d =
              Just $ transposes inp $ map (uncurry transposeInverse) ts
          | otherwise = Nothing

transposes :: SOAC.Input -> [(Int, Int)] -> SOAC.Input
transposes (SOAC.Input ts ia) kns =
  SOAC.Input (ts++[SOAC.Transpose [] k n | (k,n) <- kns]) ia

pullReshape :: SOACNest -> [OutputTransform] -> TryFusion (SOACNest, [OutputTransform])
pullReshape nest (OReshape cs shape:ots)
  | op@Nest.MapT {} <- Nest.operation nest,
    all basicType $ Nest.returnType op = do
      let loc = srclocOf nest
          inputs' = [ SOAC.Input (ts ++ [SOAC.ReshapeOuter cs shape]) ia
                      | SOAC.Input ts ia <- Nest.inputs nest ]
          outernest i = do
            let j  = length shape - 1 - i
                addDims t = arrayOf t (replicate j Nothing) $ uniqueness t
                retTypes = map addDims $ Nest.returnType op

            ps <- forM (zip (Nest.params op) inputs') $ \(p, inp) -> do
                    let t = rowType (stripArray i $ SOAC.inputType inp)
                            `setUniqueness` uniqueness (identType p)
                    newIdent "pullReshape_param" t $ srclocOf p

            bnds <- forM retTypes $ \t ->
                      newIdent "pullReshape_bnd" (fromDecl t) loc

            return Nest.Nesting {
                          Nest.nestingParams = ps
                        , Nest.nestingInputs = map SOAC.varInput ps
                        , Nest.nestingResult = bnds
                        , Nest.nestingPostExp = TupLit (map Var bnds) loc
                        , Nest.nestingReturnType = retTypes
                        }
      outerNests <- mapM outernest [0..length shape - 2]
      let nesting' = outerNests ++ Nest.nesting op
          nest'   = Nest.SOACNest {
                      Nest.inputs    = inputs'
                    , Nest.operation = nesting' `Nest.setNesting` op
                    }
      return (nest',ots)
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [Ident] -> FusedKer
             -> TryFusion (FusedKer, [OutputTransform])
exposeInputs inpIds ker =
  (exposeInputs' =<< pushTranspose') <|>
  (exposeInputs' =<< pullTranspose') <|>
  exposeInputs' ker
  where nest = Nest.fromSOAC $ fsoac ker
        ot = outputTransform ker

        pushTranspose' = do
          (nest', ot') <- pushTranspose inpIds nest ot
          return ker { fsoac = Nest.toSOAC nest', outputTransform = ot' }

        pullTranspose' = do
          (nest',[]) <- pullTranspose nest ot
          return ker { fsoac = Nest.toSOAC nest', outputTransform = [] }

        exposeInputs' ker' =
          case commonTransforms inpIds $ inputs ker' of
            (ot', inps') | all exposed inps' ->
              return (ker' { fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
            _ -> fail "Cannot expose"

        exposed (SOAC.Input [] (SOAC.Var _)) = True
        exposed inp = maybe True (`notElem` inpIds) $ SOAC.inputArray inp

outputTransformPullers :: [SOACNest -> [OutputTransform] -> TryFusion (SOACNest, [OutputTransform])]
outputTransformPullers = [pullTranspose, pullReshape]

pullOutputTransforms :: SOAC -> [OutputTransform] -> TryFusion SOAC
pullOutputTransforms soac origOts =
  Nest.toSOAC <$> attemptAll (Nest.fromSOAC soac) origOts
  where attemptAll nest ots = attempt nest ots outputTransformPullers
        attempt _ _ [] = fail "Cannot pull anything"
        attempt nest ots (p:ps) = do
          (nest',ots') <- p nest ots
          case ots of
            []  -> return nest'
            _   -> attemptAll nest' ots' <|> return nest'
          <|> attempt nest ots ps
