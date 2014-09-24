module Futhark.Analysis.HORepresentation.MapNest
  ( Nesting (..)
  , MapNest (..)
  , params
  , inputs
  , setInputs
  , fromSOACNest
  , toSOACNest
  )
where

import Control.Applicative
import Control.Monad

import Data.List
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Futhark.NeedNames
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import Futhark.Analysis.HORepresentation.SOACNest (SOACNest)
import qualified Futhark.Analysis.HORepresentation.SOACNest as Nest
import Futhark.Substitute
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder (Bindable)

data Nesting lore = Nesting {
    nestingParamNames   :: [VName]
  , nestingResult       :: [VName]
  , nestingReturnType   :: [Type]
  , nestingCertificates :: Certificates
  } deriving (Eq, Ord, Show)

data MapNest lore = MapNest Certificates (Nest.NestBody lore) [Nesting lore] [SOAC.Input] SrcLoc
                  deriving (Show)

params :: MapNest lore -> [VName]
params (MapNest _ body [] _ _)       =
  map identName $ Nest.nestBodyParams body
params (MapNest _ _    (nest:_) _ _) =
  nestingParamNames nest

inputs :: MapNest lore -> [SOAC.Input]
inputs (MapNest _ _ _ inps _) = inps

setInputs :: [SOAC.Input] -> MapNest lore -> MapNest lore
setInputs inps (MapNest cs body ns _ loc) = MapNest cs body ns inps loc

fromSOACNest :: Bindable lore => SOACNest lore -> NeedNames (Maybe (MapNest lore))
fromSOACNest = fromSOACNest' HS.empty

fromSOACNest' :: Bindable lore =>
                 HS.HashSet VName
              -> SOACNest lore
              -> NeedNames (Maybe (MapNest lore))

fromSOACNest' bound (Nest.SOACNest inps
                     (Nest.Map cs (Nest.NewNest n body@Nest.Map{}) loc)) = do
  Just mn@(MapNest cs' body' ns' inps' _) <-
    fromSOACNest' bound' (Nest.SOACNest (Nest.nestingInputs n) body)
  (ps, inps'') <-
    unzip <$> fixInputs (zip (Nest.nestingParamNames n) inps)
                        (zip (params mn) inps')
  let n' = Nesting {
             nestingParamNames   = ps
           , nestingResult       = Nest.nestingResult n
           , nestingReturnType   = Nest.nestingReturnType n
           , nestingCertificates = Nest.nestingCertificates n
           }
  return $ Just $ MapNest (cs++cs') body' (n':ns') inps'' loc
  where bound' = bound `HS.union` HS.fromList (Nest.nestingParamNames n)

fromSOACNest' bound (Nest.SOACNest inps (Nest.Map cs body loc)) = do
  lam <- lambdaBody <$> Nest.bodyToLambda (map SOAC.inputType inps) body
  let boundUsedInBody =
        HS.toList $ HS.filter (flip HS.member bound . identName) $ freeInBody lam
  newParams <- mapM (newIdent' (++"_wasfree")) boundUsedInBody
  let subst = HM.fromList $ zip (map identName boundUsedInBody) (map identName newParams)
      size  = arraysSize 0 $ map SOAC.inputType inps
      inps' = map (substituteNames subst) inps ++
              map (SOAC.addTransform (SOAC.Replicate size) . SOAC.varInput)
              boundUsedInBody
      body' =
        case body of
          Nest.NewNest n comb ->
            let n'    = substituteNames subst
                        n { Nest.nestingParamNames =
                               Nest.nestingParamNames n' ++
                               map identName newParams
                          }
                comb' = substituteNames subst comb
            in Nest.NewNest n' comb'
          Nest.Fun l ->
            Nest.Fun l { lambdaBody =
                           substituteNames subst $ lambdaBody l
                       , lambdaParams =
                         lambdaParams l ++ newParams
                       }
  return $ Just $
         if HM.null subst
         then MapNest cs body [] inps loc
         else MapNest cs body' [] inps' loc

fromSOACNest' _ _ = return Nothing

toSOACNest :: MapNest lore -> SOACNest lore
toSOACNest (MapNest cs body ns inps loc) =
  Nest.SOACNest inps $ toSOACNest' cs body ns (map SOAC.inputType inps) loc

toSOACNest' :: Certificates
            -> Nest.NestBody lore
            -> [Nesting lore]
            -> [TypeBase Shape]
            -> SrcLoc
            -> Nest.Combinator lore
toSOACNest' cs body [] _ loc =
  Nest.Map cs body loc
toSOACNest' cs body (nest:ns) inpts loc =
  let body' = toSOACNest' cs body ns (map rowType inpts) loc
  in Nest.Map cs (Nest.NewNest nest' body') loc
  where mkParam name t = Ident name t loc
        nest' = Nest.Nesting {
                  Nest.nestingParamNames = nestingParamNames nest
                , Nest.nestingResult = nestingResult nest
                , Nest.nestingReturnType = nestingReturnType nest
                , Nest.nestingInputs =
                  map SOAC.varInput $
                  zipWith mkParam (nestingParamNames nest) $ map rowType inpts
                , Nest.nestingCertificates = nestingCertificates nest
                }

fixInputs :: [(VName, SOAC.Input)] -> [(VName, SOAC.Input)]
          -> NeedNames [(VName, SOAC.Input)]
fixInputs ourInps childInps =
  reverse . snd <$> foldM inspect (ourInps, []) childInps
  where
    isParam x (y, _) = identName x == y

    ourSize = arraysSize 0 $ map (SOAC.inputType . snd) ourInps

    findParam :: [(VName, SOAC.Input)]
              -> Param
              -> Maybe ((VName, SOAC.Input), [(VName, SOAC.Input)])
    findParam remPs v
      | ([ourP], remPs') <- partition (isParam v) remPs = Just (ourP, remPs')
      | otherwise                                       = Nothing

    inspect :: ([(VName, SOAC.Input)], [(VName, SOAC.Input)])
            -> (VName, SOAC.Input)
            -> NeedNames ([(VName, SOAC.Input)], [(VName, SOAC.Input)])
    inspect (remPs, newInps) (_, SOAC.Input ts (SOAC.Var v))
      | SOAC.nullTransforms ts,
        Just (ourP, remPs') <- findParam remPs v =
          return (remPs', ourP:newInps)

    inspect (remPs, newInps) (param, SOAC.Input ts ia) =
      case ia of
        SOAC.Var v
          | Just ((p,pInp), remPs') <- findParam remPs v ->
          let pInp'  = SOAC.transformRows ts pInp
          in return (remPs',
                     (p, pInp') : newInps)
          | Just ((p,pInp), _) <- findParam newInps  v -> do
          -- The input corresponds to a variable that has already
          -- been used.
          p' <- newNameFromString $ baseString p
          return (remPs, (p', pInp) : newInps)
        _ -> do
          newParam <- newNameFromString (baseString param ++ "_rep")
          return (remPs, (newParam,
                          SOAC.Input (ts SOAC.|> SOAC.Replicate ourSize) ia) : newInps)
