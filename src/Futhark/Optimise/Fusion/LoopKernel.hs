{-# LANGUAGE TypeFamilies #-}
module Futhark.Optimise.Fusion.LoopKernel
  ( FusedKer(..)
  , newKernel
  , inputs
  , setInputs
  , arrInputs
  , kernelType
  , transformOutput
  , attemptFusion
  , SOAC
  , SOACNest
  , MapNest
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import Prelude

import Futhark.Representation.Basic
import Futhark.Renamer (renameLambda)
import Futhark.MonadFreshNames
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import qualified Futhark.Analysis.HORepresentation.SOACNest as Nest
import qualified Futhark.Analysis.HORepresentation.MapNest as MapNest
import Futhark.Optimise.Fusion.TryFusion
import Futhark.Optimise.Fusion.Composing
import Futhark.Tools

type SOAC = SOAC.SOAC Basic
type SOACNest = Nest.SOACNest Basic
type MapNest = MapNest.MapNest Basic

-- XXX: This function is very gross.
transformOutput :: SOAC.ArrayTransforms -> [VName] -> SOAC
                -> Binder Basic ()
transformOutput ts names soac = do
  validents <- zipWithM newIdent (map baseString names) $ SOAC.typeOf soac
  e <- SOAC.toExp soac
  letBind_ (basicPattern' [] validents) e
  descend ts validents
  where descend ts' validents =
          case SOAC.viewf ts' of
            SOAC.EmptyF ->
              forM_ (zip names validents) $ \(k, valident) ->
              letBindNames' [k] $ PrimOp $ SubExp $ Var $ identName valident
            t SOAC.:< ts'' -> do
              let es = map (applyTransform t) validents
                  mkPat ident = Pattern [] [PatElem ident BindVar ()]
              opts <- concat <$> mapM primOpType es
              newIds <- forM (zip names opts) $ \(k, opt) ->
                newIdent (baseString k) opt
              zipWithM_ letBind (map mkPat newIds) $ map PrimOp es
              descend ts'' newIds

applyTransform :: SOAC.ArrayTransform -> Ident -> PrimOp
applyTransform (SOAC.Rearrange cs perm) v =
  Rearrange cs perm $ identName v
applyTransform (SOAC.Reshape cs shape) v =
  Reshape cs shape $ identName v
applyTransform (SOAC.ReshapeOuter cs shape) v =
  let shapes = reshapeOuter shape 1 $ arrayShape $ identType v
  in Reshape cs shapes $ identName v
applyTransform (SOAC.ReshapeInner cs shape) v =
  let shapes = reshapeInner shape 1 $ arrayShape $ identType v
  in Reshape cs shapes $ identName v
applyTransform (SOAC.Replicate n) v =
  Replicate n $ Var $ identName v

inputToOutput :: SOAC.Input -> Maybe (SOAC.ArrayTransform, SOAC.Input)
inputToOutput (SOAC.Input ts ia) =
  case SOAC.viewf ts of
    t SOAC.:< ts' -> Just (t, SOAC.Input ts' ia)
    SOAC.EmptyF   -> Nothing

data FusedKer = FusedKer {
    fsoac      :: SOAC
  -- ^ the SOAC expression, e.g., mapT( f(a,b), x, y )

  , inplace    :: Names
  -- ^ every kernel maintains a set of variables
  -- that alias vars used in in-place updates,
  -- such that fusion is prevented to move
  -- a use of an

  , fusedVars :: [VName]
  -- ^ whether at least a fusion has been performed.

  , outputTransform :: SOAC.ArrayTransforms
  , outNames :: [VName]
  -- ^ the names of the kernel's results
  }
                deriving (Show)

newKernel :: SOAC -> [VName] -> FusedKer
newKernel soac out_nms =
  FusedKer { fsoac = soac
           , inplace = HS.empty
           , fusedVars = []
           , outputTransform = SOAC.noTransforms
           , outNames = out_nms
           }

arrInputs :: FusedKer -> HS.HashSet VName
arrInputs = HS.fromList . mapMaybe SOAC.inputArray . inputs

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker { fsoac = inps `SOAC.setInputs` fsoac ker }

kernelType :: FusedKer -> [Type]
kernelType = SOAC.typeOf . fsoac

tryOptimizeSOAC :: Names -> [VName] -> SOAC -> FusedKer
                -> TryFusion FusedKer
tryOptimizeSOAC unfus_nms outVars soac ker = do
  (soac', ots) <- optimizeSOAC Nothing soac
  let ker' = map (SOAC.addTransforms ots) (inputs ker) `setInputs` ker
      outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
      ker'' = fixInputTypes outIdents ker'
  applyFusionRules unfus_nms outVars soac' ker''

tryOptimizeKernel :: Names -> [VName] -> SOAC -> FusedKer
                  -> TryFusion FusedKer
tryOptimizeKernel unfus_nms outVars soac ker = do
  ker' <- optimizeKernel (Just outVars) ker
  applyFusionRules unfus_nms outVars soac ker'

tryExposeInputs :: Names -> [VName] -> SOAC -> FusedKer
                -> TryFusion FusedKer
tryExposeInputs unfus_nms outVars soac ker = do
  (ker', ots) <- exposeInputs outVars ker
  if SOAC.nullTransforms ots
  then fuseSOACwithKer unfus_nms outVars soac ker'
  else do
    (soac', ots') <- pullOutputTransforms soac ots
    let outIdents = zipWith Ident outVars $ SOAC.typeOf soac'
        ker'' = fixInputTypes outIdents ker'
    if SOAC.nullTransforms ots'
    then applyFusionRules unfus_nms outVars soac' ker''
    else fail "tryExposeInputs could not pull SOAC transforms"

fixInputTypes :: [Ident] -> FusedKer -> FusedKer
fixInputTypes outIdents ker =
  ker { fsoac = fixInputTypes' $ fsoac ker }
  where fixInputTypes' soac =
          map fixInputType (SOAC.inputs soac) `SOAC.setInputs` soac
        fixInputType (SOAC.Input ts (SOAC.Var v _))
          | Just v' <- find ((==v) . identName) outIdents =
            SOAC.Input ts $ SOAC.Var v $ identType v'
        fixInputType inp = inp

applyFusionRules :: Names -> [VName] -> SOAC -> FusedKer
                 -> TryFusion FusedKer
applyFusionRules    unfus_nms outVars soac ker =
  tryOptimizeSOAC   unfus_nms outVars soac ker <|>
  tryOptimizeKernel unfus_nms outVars soac ker <|>
  tryExposeInputs   unfus_nms outVars soac ker <|>
  fuseSOACwithKer   unfus_nms outVars soac ker

attemptFusion :: (MonadFreshNames m, HasTypeEnv m) =>
                 Names -> [VName] -> SOAC -> FusedKer
              -> m (Maybe FusedKer)
attemptFusion unfus_nms outVars soac ker = do
  types <- askTypeEnv
  liftM removeUnusedParamsFromKer <$>
    tryFusion (applyFusionRules unfus_nms outVars soac ker) types

removeUnusedParamsFromKer :: FusedKer -> FusedKer
removeUnusedParamsFromKer ker =
  case soac of
    SOAC.Map {}     -> ker { fsoac = soac' }
    SOAC.Redomap {} -> ker { fsoac = soac' }
    _                -> ker
  where soac = fsoac ker
        l = SOAC.lambda soac
        inps = SOAC.inputs soac
        (l', inps') = removeUnusedParams l inps
        soac' = l' `SOAC.setLambda`
                (inps' `SOAC.setInputs` soac)

removeUnusedParams :: Lambda -> [SOAC.Input] -> (Lambda, [SOAC.Input])
removeUnusedParams l inps =
  (l { lambdaParams = accParams ++ ps' }, inps')
  where allParams = lambdaParams l
        (accParams, arrParams) =
          splitAt (length allParams - length inps) allParams
        pInps = zip arrParams inps
        (ps', inps') = case (unzip $ filter (used . fst) pInps, pInps) of
                         (([], []), (p,inp):_) -> ([p], [inp])
                         ((ps_, inps_), _)     -> (ps_, inps_)
        used p = identName p `HS.member` freeVars
        freeVars = freeInBody $ lambdaBody l

-- | Check that the consumer uses at least one output of the producer
-- unmodified.
mapFusionOK :: [VName] -> FusedKer -> Bool
mapFusionOK outVars ker = any (`elem` inpIds) outVars
  where inpIds = mapMaybe SOAC.isVarInput (inputs ker)

mapOrFilter :: SOAC -> Bool
mapOrFilter (SOAC.Map {}) = True
mapOrFilter _             = False

fuseSOACwithKer :: Names -> [VName] -> SOAC -> FusedKer
                -> TryFusion FusedKer
fuseSOACwithKer unfus_set outVars soac1 ker = do
  -- We are fusing soac1 into soac2, i.e, the output of soac1 is going
  -- into soac2.
  let soac2    = fsoac ker
      cs1      = SOAC.certificates soac1
      cs2      = SOAC.certificates soac2
      inp1_arr = SOAC.inputs soac1
      inp1_idds= mapMaybe SOAC.isVarInput inp1_arr
      inp2_arr = SOAC.inputs soac2
      lam1     = SOAC.lambda soac1
      lam2     = SOAC.lambda soac2
      unfus_nms= HS.toList unfus_set
      success res_outnms res_soac = do
        let fusedVars_new = fusedVars ker++outVars
        -- Avoid name duplication, because the producer lambda is not
        -- removed from the program until much later.
        uniq_lam <- renameLambda $ SOAC.lambda res_soac
        return $ ker { fsoac = uniq_lam `SOAC.setLambda` res_soac
                     , fusedVars = fusedVars_new
                     , outNames = res_outnms
                     }
  outPairs <- forM (zip outVars $ SOAC.typeOf soac1) $ \(outVar, t) -> do
                outVar' <- newVName $ baseString outVar ++ "_elem"
                return (outVar, Ident outVar' t)
  case (soac2, soac1) of
    -- The Fusions that are semantically map fusions:
    (SOAC.Map {}, SOAC.Map    {})
      | mapFusionOK (outVars++inp1_idds) ker -> do
      let (res_lam, new_inp) = fuseMaps unfus_nms lam1 inp1_arr outPairs lam2 inp2_arr
          (_,extra_rtps) = unzip $ filter (\(nm,_)->elem nm unfus_nms) $
                           zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
      success (outNames ker ++ unfus_nms) $
              SOAC.Map (cs1++cs2) res_lam' new_inp

    -- Cosmin added case: CURRENTLY IMPLEMENTING THIS
    (SOAC.Map {}, SOAC.Redomap _ lam11 _ nes _)
      | mapFusionOK (outVars++inp1_idds) ker -> do
      let acc_len     = length nes
          unfus_acc   = take acc_len outVars
          unfus_nms'  = unfus_nms \\ unfus_acc
          lam1_body   = lambdaBody lam1
          lam1_accres = take acc_len $ resultSubExps $ bodyResult lam1_body
          lam1_arrres = drop acc_len $ resultSubExps $ bodyResult lam1_body
          lam1_hacked = lam1 { lambdaParams = drop acc_len $ lambdaParams lam1
                             , lambdaBody   = lam1_body { bodyResult = Result lam1_arrres } }
          (res_lam, new_inp) = fuseMaps unfus_nms' lam1_hacked inp1_arr (tail outPairs) lam2 inp2_arr
          (_,extra_rtps) = unzip $ filter (\(nm,_)->elem nm unfus_nms') $
                           zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          (accrtps, accpars)  = (lambdaReturnType lam11, take acc_len $ lambdaParams lam1)
          res_body    = lambdaBody res_lam
          res_bodyrses= resultSubExps (bodyResult res_body)
          res_body'   = res_body { bodyResult = Result $ lam1_accres ++ res_bodyrses }
          res_lam' = res_lam { lambdaParams     = accpars ++ lambdaParams res_lam
                             , lambdaBody       = res_body'
                             , lambdaReturnType = accrtps ++ lambdaReturnType res_lam ++ extra_rtps
                             }
      success (unfus_acc ++ outNames ker ++ unfus_nms') $
              SOAC.Redomap (cs1++cs2) lam11 res_lam' nes new_inp

    (SOAC.Redomap _ lam21 _ nes _, SOAC.Map {})
      | mapFusionOK (outVars++inp1_idds) ker -> do
      let (res_lam, new_inp) = fuseMaps unfus_nms lam1 inp1_arr outPairs lam2 inp2_arr
          (_,extra_rtps) = unzip $ filter (\(nm,_)->elem nm unfus_nms) $
                           zip outVars $ map (stripArray 1) $ SOAC.typeOf soac1
          res_lam' = res_lam { lambdaReturnType = lambdaReturnType res_lam ++ extra_rtps }
      success (outNames ker ++ unfus_nms) $
              SOAC.Redomap (cs1++cs2) lam21 res_lam' nes new_inp
{-
    (SOAC.Redomap _ lam21 _ ne _, SOAC.Map {})
      | mapFusionOK outVars ker -> do
      let (res_lam, new_inp) = fuseMaps lam1 inp1_arr outPairs lam2 inp2_arr
      success $ SOAC.Redomap (cs1++cs2) lam21 res_lam ne new_inp
-}
    -- Nothing else worked, so mkLets try rewriting to redomap if
    -- possible. (Should not be reached anymore ... remove when safe)
    (SOAC.Reduce _ lam args, _) | mapOrFilter soac1 -> do
       let (ne, arrs) = unzip args
           soac2' = SOAC.Redomap (cs1++cs2) lam lam ne arrs
           ker'   = ker { fsoac = soac2'
                        }
       fuseSOACwithKer unfus_set outVars soac1 ker'

    _ -> fail "Cannot fuse"

-- Here follows optimizations and transforms to expose fusability.

optimizeKernel :: Maybe [VName] -> FusedKer -> TryFusion FusedKer
optimizeKernel inp ker = do
  startNest <- Nest.fromSOAC $ fsoac ker
  (resNest, resTrans) <- optimizeSOACNest inp startNest startTrans
  soac <- Nest.toSOAC resNest
  return $ ker { fsoac = soac
               , outputTransform = resTrans
               }
  where startTrans = outputTransform ker

optimizeSOAC :: Maybe [VName] -> SOAC -> TryFusion (SOAC, SOAC.ArrayTransforms)
optimizeSOAC inp soac = do
  nest <- Nest.fromSOAC soac
  (nest', ots) <- optimizeSOACNest inp nest SOAC.noTransforms
  soac' <- Nest.toSOAC nest'
  return (soac', ots)

optimizeSOACNest :: Maybe [VName] -> SOACNest -> SOAC.ArrayTransforms
                 -> TryFusion (SOACNest, SOAC.ArrayTransforms)
optimizeSOACNest inp soac os = do
  res <- foldM comb (False, soac, os) $ reverse optimizations
  case res of
    (False, _, _)      -> fail "No optimisation applied"
    (True, soac', os') -> return (soac', os')
  where comb (changed, soac', os') f = do
          (soac'', os'') <- f inp soac' os
          return (True, soac'', os'')
          <|> return (changed, soac', os')

type Optimization = Maybe [VName]
                    -> SOACNest
                    -> SOAC.ArrayTransforms
                    -> TryFusion (SOACNest, SOAC.ArrayTransforms)

optimizations :: [Optimization]
optimizations = [iswim]

iswim :: Maybe [VName] -> SOACNest -> SOAC.ArrayTransforms
      -> TryFusion (SOACNest, SOAC.ArrayTransforms)
iswim _ nest ots
  | Nest.Scan cs1 (Nest.NewNest lvl nn) es <- Nest.operation nest,
    Nest.Map cs2 mb <- nn,
    Just es' <- mapM Nest.inputFromTypedSubExp es,
    Nest.Nesting paramIds mapArrs bndIds retTypes <- lvl,
    mapM isVarInput mapArrs == Just paramIds = do
    let newInputs :: [SOAC.Input]
        newInputs = es' ++ map (SOAC.transposeInput 0 1) (Nest.inputs nest)
        inputTypes = map SOAC.inputType newInputs
        (accsizes, arrsizes) =
          splitAt (length es) $ map rowType inputTypes
        innerAccParams = zipWith Nest.TypedSubExp
                         (map Var $ take (length es) paramIds) accsizes
        innerArrParams = zipWith Ident (drop (length es) paramIds) arrsizes
    let innerScan = Nest.Scan cs2 mb innerAccParams
        scanNest = Nest.Nesting {
                     Nest.nestingInputs = map SOAC.identInput innerArrParams
                   , Nest.nestingReturnType = zipWith setOuterSize retTypes $
                                              map (arraySize 0) arrsizes
                   , Nest.nestingResult = bndIds
                   , Nest.nestingParamNames = paramIds
                   }
        perm = case retTypes of []  -> []
                                t:_ -> transposeIndex 0 1 [0..arrayRank t]
        nest' = Nest.SOACNest
                newInputs
                (Nest.Map cs1 (Nest.NewNest scanNest innerScan))
    return (nest',
            ots SOAC.|> SOAC.Rearrange cs2 perm)
iswim _ _ _ = fail "ISWIM does not apply"

-- Now for fiddling with transpositions...

commonTransforms :: [VName] -> [SOAC.Input]
                 -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms interesting inps = commonTransforms' inps'
  where inps' = [ (maybe False (`elem` interesting) $ SOAC.inputArray inp, inp)
                    | inp <- inps ]

commonTransforms' :: [(Bool, SOAC.Input)] -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms' inps =
  case foldM inspect (Nothing, []) inps of
    Just (Just mot, inps') -> first (mot SOAC.<|) $ commonTransforms' $ reverse inps'
    _                      -> (SOAC.noTransforms, map snd inps)
  where inspect (mot, prev) (True, inp) =
          case (mot, inputToOutput inp) of
           (Nothing,  Just (ot, inp'))  -> Just (Just ot, (True, inp') : prev)
           (Just ot1, Just (ot2, inp'))
             | ot1 == ot2 -> Just (Just ot2, (True, inp') : prev)
           _              -> Nothing
        inspect (mot, prev) inp = Just (mot,inp:prev)

mapDepth :: MapNest -> Int
mapDepth (MapNest.MapNest _ body levels _) =
  -- XXX: The restriction to pure nests is conservative, but we cannot
  -- know whether an arbitrary postbody is dependent on the exact size
  -- of the nesting result.
  min resDims (length levels) + 1
  where resDims = minDim $ case levels of
                    [] -> case body of Nest.Fun lam ->
                                         lambdaReturnType lam
                                       Nest.NewNest nest _ ->
                                         Nest.nestingReturnType nest
                    nest:_ -> MapNest.nestingReturnType nest
        minDim [] = 0
        minDim (t:ts) = foldl min (arrayRank t) $ map arrayRank ts

pullRearrange :: SOACNest -> SOAC.ArrayTransforms
              -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pullRearrange nest ots = do
  nest' <- join $ liftMaybe <$> MapNest.fromSOACNest nest
  SOAC.Rearrange cs perm SOAC.:< ots' <- return $ SOAC.viewf ots
  if permuteReach perm <= mapDepth nest' then
    let -- Expand perm to cover the full extent of the input dimensionality
        perm' inp = perm ++ [length perm..SOAC.inputRank inp-1]
        addPerm inp = SOAC.addTransform (SOAC.Rearrange cs $ perm' inp) inp
        inputs' = map addPerm $ MapNest.inputs nest'
    in return (MapNest.toSOACNest $ inputs' `MapNest.setInputs` nest',
               ots')
  else fail "Cannot pull transpose"

pushRearrange :: [VName] -> SOACNest -> SOAC.ArrayTransforms
              -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pushRearrange inpIds nest ots = do
  nest' <- join $ liftMaybe <$> MapNest.fromSOACNest nest
  (perm, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest'
  if permuteReach perm <= mapDepth nest' then
    let invertRearrange = SOAC.Rearrange [] $ permuteInverse perm
    in return (MapNest.toSOACNest $
               inputs' `MapNest.setInputs`
               rearrangeReturnTypes nest' perm,
               ots SOAC.|> invertRearrange)
  else fail "Cannot push transpose"

rearrangeReturnTypes :: MapNest -> [Int] -> MapNest
rearrangeReturnTypes nest@(MapNest.MapNest cs body nestings inps) perm =
  MapNest.MapNest cs body
  (zipWith setReturnType nestings $
   drop 1 $ iterate (map rowType) ts)
  inps
  where rearrange t = setArrayDims t $
                      permuteShape (perm ++ [length perm..arrayRank t-1]) $
                      arrayDims t
        origts = MapNest.typeOf nest
        ts =  map rearrange origts
        setReturnType nesting t' =
          nesting { MapNest.nestingReturnType = t' }

fixupInputs :: [VName] -> [SOAC.Input] -> Maybe ([Int], [SOAC.Input])
fixupInputs inpIds inps =
  case mapMaybe inputRearrange $ filter exposable inps of
    perm:_ -> do inps' <- mapM (fixupInput (permuteReach perm) perm) inps
                 return (perm, inps')
    _    -> Nothing
  where exposable = maybe False (`elem` inpIds) . SOAC.inputArray

        inputRearrange (SOAC.Input ts _)
          | _ SOAC.:> SOAC.Rearrange _ perm <- SOAC.viewl ts = Just perm
        inputRearrange _                                     = Nothing

        fixupInput d perm inp
          | SOAC.inputRank inp >= d =
              Just $ SOAC.addTransform (SOAC.Rearrange [] $ permuteInverse perm) inp
          | otherwise = Nothing

pullReshape :: SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pullReshape nest ots
  | SOAC.Reshape cs shape SOAC.:< ots' <- SOAC.viewf ots,
    op@Nest.Map {} <- Nest.operation nest,
    all basicType $ Nest.returnType op = do
  let inputs' = map (SOAC.addTransform $ SOAC.ReshapeOuter cs shape) $
                Nest.inputs nest
      inputTypes = map SOAC.inputType inputs'
      outernest inner outershape = do
        let addDims t = arrayOf t (Shape outershape) $ uniqueness t
            retTypes = map addDims $ Nest.returnType op

        ps <- forM (zip (Nest.params op) inputTypes) $
              \(p, inpt) -> do
                let t = rowType (stripArray (length outershape-1) inpt)
                        `setUniqueness` uniqueness (identType p)
                newIdent "pullReshape_param" t

        bnds <- forM retTypes $ \_ ->
                  newNameFromString "pullReshape_bnd"

        let nesting = Nest.Nesting {
                        Nest.nestingParamNames = map identName ps
                      , Nest.nestingInputs = map SOAC.identInput ps
                      , Nest.nestingResult = bnds
                      , Nest.nestingReturnType = retTypes
                      }
        return $ Nest.Map [] (Nest.NewNest nesting inner)
  -- Only have the certificates on the outermost loop nest.  This
  -- only has the significance of making the generated code look
  -- very slightly neater.
  op' <- foldM outernest ([] `Nest.setCombCertificates` op) $
         drop 1 $ reverse $ drop 1 $ tails shape
  let nest'   = Nest.SOACNest {
                  Nest.inputs    = inputs'
                , Nest.operation =
                  Nest.combCertificates op `Nest.setCombCertificates` op'
                }
  return (nest', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [VName] -> FusedKer
             -> TryFusion (FusedKer, SOAC.ArrayTransforms)
exposeInputs inpIds ker = do
  nest <- Nest.fromSOAC $ fsoac ker
  (exposeInputs' =<< pushRearrange' nest) <|>
    (exposeInputs' =<< pullRearrange' nest) <|>
    exposeInputs' ker
  where ot = outputTransform ker

        pushRearrange' nest = do
          (nest', ot') <- pushRearrange inpIds nest ot
          soac         <- Nest.toSOAC nest'
          return ker { fsoac = soac, outputTransform = ot' }

        pullRearrange' nest = do
          (nest',ot') <- pullRearrange nest ot
          unless (SOAC.nullTransforms ot') $
            fail "pullRearrange was not enough"
          soac        <- Nest.toSOAC nest'
          return ker { fsoac = soac,
                       outputTransform = SOAC.noTransforms
                     }

        exposeInputs' ker' =
          case commonTransforms inpIds $ inputs ker' of
            (ot', inps') | all exposed inps' ->
              return (ker' { fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
            _ -> fail "Cannot expose"

        exposed (SOAC.Input ts (SOAC.Var {}))
          | SOAC.nullTransforms ts = True
        exposed inp = maybe True (`notElem` inpIds) $ SOAC.inputArray inp

outputTransformPullers :: [SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)]
outputTransformPullers = [pullRearrange, pullReshape]

pullOutputTransforms :: SOAC -> SOAC.ArrayTransforms
                     -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullOutputTransforms soac origOts = do
  nest <- Nest.fromSOAC soac
  (nest', ots') <- attemptAll nest origOts
  soac' <- Nest.toSOAC nest'
  return (soac', ots')
  where attemptAll nest ots = attempt nest ots outputTransformPullers
        attempt _ _ [] = fail "Cannot pull anything"
        attempt nest ots (p:ps) = do
          (nest',ots') <- p nest ots
          if SOAC.nullTransforms ots' then return (nest', SOAC.noTransforms)
          else attemptAll nest' ots' <|> return (nest', ots')
          <|> attempt nest ots ps
