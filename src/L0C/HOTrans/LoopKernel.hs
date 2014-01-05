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
  )
  where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad

import qualified Data.HashSet as HS

import Data.Maybe
import Data.Loc

import L0C.L0
import L0C.MonadFreshNames
import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest)
import qualified L0C.HORepresentation.SOACNest as Nest
import L0C.HOTrans.Composing

data OutputTransform = OTranspose Certificates Int Int
                     | OReshape Certificates [Exp]
                       deriving (Eq, Ord, Show)

applyTransform :: OutputTransform -> Ident -> SrcLoc -> Exp
applyTransform (OTranspose cs k n) = Transpose cs k n . Var
applyTransform (OReshape cs shape) = Reshape cs shape . Var

outputToInput :: OutputTransform -> SOAC.Input -> SOAC.Input
outputToInput (OTranspose cs k n) = SOAC.Transpose cs k n
outputToInput (OReshape cs shape) = SOAC.Reshape cs shape

outputsToInput :: [OutputTransform] -> SOAC.Input -> SOAC.Input
outputsToInput = foldr ((.) . outputToInput) id

inputToOutput :: SOAC.Input -> Maybe (OutputTransform, SOAC.Input)
inputToOutput (SOAC.Transpose cs k n (SOAC.Var v)) =
  Just (OTranspose cs k n, SOAC.Var v)
inputToOutput (SOAC.Transpose cs k n inp) =
  second (SOAC.Transpose cs k n) <$> inputToOutput inp
inputToOutput (SOAC.Reshape cs shape (SOAC.Var v)) =
  Just (OReshape cs shape, SOAC.Var v)
inputToOutput (SOAC.Reshape cs shape inp) =
  second (SOAC.Reshape cs shape) <$> inputToOutput inp
inputToOutput _                           = Nothing

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

attemptFusion :: MonadFreshNames VName m => [Ident] -> SOAC -> FusedKer -> m (Maybe FusedKer)
attemptFusion outIds soac ker
  | Just (soac', ots) <- optimizeSOAC Nothing soac =
      let ker' = map (outputsToInput ots) (inputs ker) `setInputs` ker
      in attemptFusion outIds soac' ker'
  | Just ker' <- optimizeKernel (Just outIds) ker =
      attemptFusion outIds soac ker'
  | Just (ker', ots) <- exposeInputs outIds ker = do
      msoac' <- pullOutputTransforms soac ots
      case msoac' of
        Just soac' -> attemptFusion outIds soac' ker'
        Nothing    -> fuseSOACwithKer (outIds, soac) ker
  | otherwise =
      fuseSOACwithKer (outIds, soac) ker

-- | Check that the consumer uses at least one output of the producer
-- unmodified.
mapFusionOK :: [Ident] -> FusedKer -> Bool
mapFusionOK outIds ker = any (`elem` inputs ker) (map SOAC.Var outIds)

-- | Check that the input-array set of consumer is equal to the
-- output-array set of producer.  That is, a filter-producer can only
-- be fused with a filter or reduce-consumer if the consumer accepts
-- input from no other source, and consumes everything by the
-- producer.
filterFusionOK :: [Ident] -> FusedKer -> Bool
filterFusionOK outIds ker =
  case mapM SOAC.inputArray $ inputs ker of
    Nothing       -> False
    Just inputIds -> all (`elem` outIds) inputIds &&
                     all (`elem` inputIds) outIds

-- | Check that the input-array set of consumer is contained in the
-- output-array set of producer.  That is, a filter-producer can only
-- be fused if the consumer accepts input from no other source.
filterFoldFusionOK :: [Ident] -> FusedKer -> Bool
filterFoldFusionOK outIds ker =
  case mapM SOAC.inputArray $ inputs ker of
    Nothing       -> False
    Just inputIds -> all (`elem` outIds) inputIds

mapOrFilter :: SOAC -> Bool
mapOrFilter (SOAC.FilterT {}) = True
mapOrFilter (SOAC.MapT {})    = True
mapOrFilter _                 = False

fuseSOACwithKer :: MonadFreshNames VName m => ([Ident], SOAC) -> FusedKer -> m (Maybe FusedKer)
fuseSOACwithKer (outIds, soac1) ker = do
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
        return $ Just ker { fsoac = res_soac
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

    -- The Fusions that are semantically filter fusions:
    (SOAC.ReduceT _ _ ne _ pos, SOAC.FilterT {})
      | filterFusionOK outIds ker -> do
      name <- newVName "check"
      let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr outIds lam2 inp2_arr name
      success $ SOAC.ReduceT (cs1++cs2) res_lam ne new_inp pos

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
    (SOAC.ReduceT _ lam ne arrs loc, _) | mapOrFilter soac1 -> do
       let soac2' = SOAC.RedomapT (cs1++cs2) lam lam ne arrs loc
           ker'   = ker { fsoac = soac2'
                        , outputs = out_ids2 }
       fuseSOACwithKer (outIds, soac1) ker'

    _ -> return Nothing

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
    Nest.Nesting paramIds mapArrs bndIds retTypes <- lvl,
    mapArrs == map SOAC.Var paramIds =
    let toInnerAccParam idd = idd { identType = rowType $ identType idd }
        innerAccParams = map toInnerAccParam $ take (length es) paramIds
        innerArrParams = drop (length es) paramIds
        innerScan = ScanT cs2 (Nest.bodyToLambda mb)
                          (map Var innerAccParams) (map Var innerArrParams)
                          loc1
        lam = TupleLambda {
                tupleLambdaParams = map toParam $ innerAccParams ++ innerArrParams
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody = Nest.letPattern bndIds innerScan
              , tupleLambdaSrcLoc = loc2
              }
        transposeInput (SOAC.Transpose _ 0 1 inp) = inp
        transposeInput inp                        = SOAC.Transpose cs2 0 1 inp
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

mapDepth :: SOACNest -> Int
mapDepth nest = case Nest.operation nest of
                  Nest.MapT _ _ levels _ -> length levels + 1
                  _                      -> 0

pullTranspose :: SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform])
pullTranspose nest (OTranspose cs k n:ots')
  | n+k < mapDepth nest =
      let inputs' = map (SOAC.Transpose cs k n) $ Nest.inputs nest
      in Just (inputs' `Nest.setInputs` nest, ots')
pullTranspose _ _ = Nothing

pushTranspose :: Maybe [Ident] -> SOACNest -> [OutputTransform]
              -> Maybe (SOACNest, [OutputTransform])
pushTranspose _ nest ots
  | Just (n, k, cs, inputs') <- transposedInputs $ Nest.inputs nest,
    n+k < mapDepth nest = Just (inputs' `Nest.setInputs` nest,
                                ots ++ [OTranspose cs n k])
  where transposedInputs (SOAC.Transpose cs n k input:args) =
          foldM comb (n, k, cs, [input]) args
          where comb (n1, k1, cs1, idds) (SOAC.Transpose cs2 n2 k2 input')
                  | n1==n2, k1==k2   = Just (n1,k1,cs1++cs2,idds++[input'])
                comb _             _ = Nothing
        transposedInputs _ = Nothing
pushTranspose (Just inpIds) nest ots
  | Just (ts, inputs') <- fixupInputs inpIds (Nest.inputs nest),
    transposeReach ts < mapDepth nest =
      let outInvTrns = uncurry (OTranspose []) . uncurry transposeInverse
      in Just (inputs' `Nest.setInputs` nest,
               ots ++ map outInvTrns (reverse ts))
pushTranspose _ _ _ = Nothing

transposeReach :: [(Int,Int)] -> Int
transposeReach = foldr (max . transDepth) 0
  where transDepth (k,n) = max k (k+n)

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
          | arrayDims (SOAC.inputType inp) > d =
              Just $ transposes inp $ map (uncurry transposeInverse) ts
          | otherwise = Nothing

transposes :: SOAC.Input -> [(Int, Int)] -> SOAC.Input
transposes = foldr inverseTrans'
  where inverseTrans' (k,n) = SOAC.Transpose [] k n

pullReshape :: MonadFreshNames VName m => SOACNest -> [OutputTransform] -> m (Maybe (SOACNest, [OutputTransform]))
pullReshape nest (OReshape cs shape:ots)
  | op@Nest.MapT {} <- Nest.operation nest,
    all basicType $ Nest.returnType op = do
      let shapeForParam inp = reshapeOuter shape 1 (SOAC.inputToExp inp)
          inputs' = [ SOAC.Reshape cs (shapeForParam inp) inp
                      | inp <- Nest.inputs nest ]
          outernest i = do
            let j  = length shape - 1 - i
                addDims t = arrayOf t (replicate j Nothing) $ uniqueness t
                retTypes = map addDims $ Nest.returnType op

            ps <- forM (zip (Nest.params op) inputs') $ \(p, inp) -> do
                    let t = rowType $ stripArray i $ SOAC.inputType inp
                    newIdent "pullReshape_param" t $ srclocOf p

            bnds <- forM retTypes $ \t ->
                      newIdent "pullReshape_bnd" (fromDecl t) $ srclocOf nest

            return Nest.Nesting {
                          Nest.nestingParams = ps
                        , Nest.nestingInputs = map SOAC.Var ps
                        , Nest.nestingResult = bnds
                        , Nest.nestingReturnType = retTypes
                        }
      outerNests <- mapM outernest [0..length shape - 2]
      let nesting' = outerNests ++ Nest.nesting op
          nest'   = Nest.SOACNest {
                      Nest.inputs    = inputs'
                    , Nest.operation = nesting' `Nest.setNesting` op
                    }
      return $ Just (nest',ots)
pullReshape _ _ = return Nothing

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [Ident] -> FusedKer
             -> Maybe (FusedKer, [OutputTransform])
exposeInputs inpIds ker =
  exposeInputs' ker                  <|>
  (exposeInputs' =<< pushTranspose') <|>
  (exposeInputs' =<< pullTranspose')
  where nest = Nest.fromSOAC $ fsoac ker
        ot = outputTransform ker

        pushTranspose' = do
          (nest', ot') <- pushTranspose (Just inpIds) nest ot
          return ker { fsoac = Nest.toSOAC nest', outputTransform = ot' }

        pullTranspose' = do
          (nest',[]) <- pullTranspose nest ot
          return ker { fsoac = Nest.toSOAC nest', outputTransform = [] }

        exposeInputs' ker' =
          case commonTransforms inpIds $ inputs ker' of
            (ot', inps') | all exposed inps' ->
              Just (ker' { fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
            _ -> Nothing

        exposed (SOAC.Var _) = True
        exposed inp = maybe True (`notElem` inpIds) $ SOAC.inputArray inp

purePuller :: Monad m =>
              (SOACNest -> [OutputTransform] -> Maybe (SOACNest, [OutputTransform]))
           -> SOACNest -> [OutputTransform] -> m (Maybe (SOACNest, [OutputTransform]))
purePuller f soac ots = return $ f soac ots

outputTransformPullers :: MonadFreshNames VName m =>
                          [SOACNest -> [OutputTransform] -> m (Maybe (SOACNest, [OutputTransform]))]
outputTransformPullers = [purePuller pullTranspose, pullReshape]

pullOutputTransforms :: MonadFreshNames VName m =>
                        SOAC -> [OutputTransform] -> m (Maybe SOAC)
pullOutputTransforms soac origOts =
  liftM Nest.toSOAC <$> attemptAll (Nest.fromSOAC soac) origOts
  where attemptAll nest ots = attempt nest ots outputTransformPullers
        attempt _ _ [] = return Nothing
        attempt nest ots (p:ps) = do
          x <- p nest ots
          case x of
            Nothing            -> attempt nest ots ps
            Just (nest', [])   -> return $ Just nest'
            Just (nest', ots') -> attemptAll nest' ots'
