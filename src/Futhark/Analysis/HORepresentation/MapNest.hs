module Futhark.Analysis.HORepresentation.MapNest
  ( Nesting (..)
  , MapNest (..)
  , typeOf
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
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Prelude

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
  } deriving (Eq, Ord, Show)

data MapNest lore = MapNest Certificates (Nest.NestBody lore) [Nesting lore] [SOAC.Input]
                  deriving (Show)

typeOf :: MapNest lore -> [Type]
typeOf (MapNest _ body nests inps) =
  [ arrayOf t (Shape [outersize]) (uniqueness t) | t <- innersizes ]
  where innersizes = case nests of []     -> Nest.nestBodyReturnType body
                                   nest:_ -> nestingReturnType nest
        outersize = arraysSize 0 $ map SOAC.inputType inps


params :: MapNest lore -> [VName]
params (MapNest _ body [] _)       =
  map identName $ Nest.nestBodyParams body
params (MapNest _ _    (nest:_) _) =
  nestingParamNames nest

inputs :: MapNest lore -> [SOAC.Input]
inputs (MapNest _ _ _ inps) = inps

setInputs :: [SOAC.Input] -> MapNest lore -> MapNest lore
setInputs inps (MapNest cs body ns _) = MapNest cs body ns inps

fromSOACNest :: (Bindable lore, MonadFreshNames m, HasTypeEnv m) =>
                SOACNest lore -> m (Maybe (MapNest lore))
fromSOACNest = fromSOACNest' mempty

fromSOACNest' :: (Bindable lore, MonadFreshNames m, HasTypeEnv m) =>
                 [Ident]
              -> SOACNest lore
              -> m (Maybe (MapNest lore))

fromSOACNest' bound (Nest.SOACNest inps
                     (Nest.Map cs (Nest.NewNest n body@Nest.Map{}))) = do
  Just mn@(MapNest cs' body' ns' inps') <-
    fromSOACNest' bound' (Nest.SOACNest (Nest.nestingInputs n) body)
  (ps, inps'') <-
    unzip <$> fixInputs (zip (Nest.nestingParamNames n) inps)
                        (zip (params mn) inps')
  let n' = Nesting {
             nestingParamNames   = ps
           , nestingResult       = Nest.nestingResult n
           , nestingReturnType   = Nest.nestingReturnType n
           }
  return $ Just $ MapNest (cs++cs') body' (n':ns') inps''
  where bound' = bound <> Nest.nestingParams n

fromSOACNest' bound (Nest.SOACNest inps (Nest.Map cs body)) = do
  lam <- lambdaBody <$> Nest.bodyToLambda (map SOAC.inputType inps) body
  let isBound name
        | Just param <- find ((name==) . identName) bound =
          Just param
        | otherwise =
          Nothing
      boundUsedInBody =
        mapMaybe isBound $ HS.toList $ freeInBody lam
  newParams <- mapM (newIdent' (++"_wasfree")) boundUsedInBody
  let subst = HM.fromList $ zip (map identName boundUsedInBody) (map identName newParams)
      size  = arraysSize 0 $ map SOAC.inputType inps
      inps' = map (substituteNames subst) inps ++
              map (SOAC.addTransform (SOAC.Replicate size) . SOAC.identInput)
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
         then MapNest cs body [] inps
         else MapNest cs body' [] inps'

fromSOACNest' _ _ = return Nothing

toSOACNest :: MapNest lore -> SOACNest lore
toSOACNest (MapNest cs body ns inps) =
  Nest.SOACNest inps $ toSOACNest' cs body ns (map SOAC.inputType inps)

toSOACNest' :: Certificates
            -> Nest.NestBody lore
            -> [Nesting lore]
            -> [TypeBase Shape]
            -> Nest.Combinator lore
toSOACNest' cs body [] _ =
  Nest.Map cs body
toSOACNest' cs body (nest:ns) inpts =
  let body' = toSOACNest' cs body ns (map rowType inpts)
  in Nest.Map cs (Nest.NewNest nest' body')
  where nest' = Nest.Nesting {
                  Nest.nestingParamNames = nestingParamNames nest
                , Nest.nestingResult = nestingResult nest
                , Nest.nestingReturnType = nestingReturnType nest
                , Nest.nestingInputs = map SOAC.identInput newparams
                }
        newparams = zipWith Ident (nestingParamNames nest) $
                    map rowType inpts

fixInputs :: MonadFreshNames m =>
             [(VName, SOAC.Input)] -> [(VName, SOAC.Input)]
          -> m [(VName, SOAC.Input)]
fixInputs ourInps childInps =
  reverse . snd <$> foldM inspect (ourInps, []) childInps
  where
    isParam x (y, _) = x == y

    ourSize = arraysSize 0 $ map (SOAC.inputType . snd) ourInps

    findParam :: [(VName, SOAC.Input)]
              -> VName
              -> Maybe ((VName, SOAC.Input), [(VName, SOAC.Input)])
    findParam remPs v
      | ([ourP], remPs') <- partition (isParam v) remPs = Just (ourP, remPs')
      | otherwise                                       = Nothing

    inspect :: MonadFreshNames m =>
               ([(VName, SOAC.Input)], [(VName, SOAC.Input)])
            -> (VName, SOAC.Input)
            -> m ([(VName, SOAC.Input)], [(VName, SOAC.Input)])
    inspect (remPs, newInps) (_, SOAC.Input ts (SOAC.Var v _))
      | SOAC.nullTransforms ts,
        Just (ourP, remPs') <- findParam remPs v =
          return (remPs', ourP:newInps)

    inspect (remPs, newInps) (param, SOAC.Input ts ia) =
      case ia of
        SOAC.Var v _
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
