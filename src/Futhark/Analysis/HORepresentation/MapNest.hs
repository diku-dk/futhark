{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Futhark.Representation.SOACS.SOAC as Futhark
import Futhark.Transform.Substitute
import Futhark.Representation.AST hiding (typeOf)
import Futhark.MonadFreshNames
import Futhark.Binder (Bindable)

data Nesting lore = Nesting {
    nestingParamNames   :: [VName]
  , nestingResult       :: [VName]
  , nestingReturnType   :: [Type]
  , nestingWidth        :: SubExp
  } deriving (Eq, Ord, Show)

data MapNest lore = MapNest Certificates SubExp (Nest.NestBody lore) [Nesting lore] [SOAC.Input]
                  deriving (Show)

typeOf :: MapNest lore -> [Type]
typeOf (MapNest _ w body nests _) =
  map (`arrayOfRow` w) innersizes
  where innersizes = case nests of []     -> Nest.nestBodyReturnType body
                                   nest:_ -> nestingReturnType nest

params :: Annotations lore => MapNest lore -> [VName]
params (MapNest _ _ body [] _)       =
  map identName $ Nest.nestBodyParams body
params (MapNest _ _ _ (nest:_) _) =
  nestingParamNames nest

inputs :: MapNest lore -> [SOAC.Input]
inputs (MapNest _ _ _ _ inps) = inps

setInputs :: [SOAC.Input] -> MapNest lore -> MapNest lore
setInputs [] (MapNest cs w body ns _) = MapNest cs w body ns []
setInputs (inp:inps) (MapNest cs _ body ns _) = MapNest cs w body ns' (inp:inps)
  where w = arraySize 0 $ SOAC.inputType inp
        ws = drop 1 $ arrayDims $ SOAC.inputType inp
        ns' = zipWith setDepth ns ws
        setDepth n nw = n { nestingWidth = nw }

fromSOACNest :: (Bindable lore, MonadFreshNames m,
                 LocalScope lore m,
                 Op lore ~ Futhark.SOAC lore) =>
                SOACNest lore -> m (Maybe (MapNest lore))
fromSOACNest = fromSOACNest' mempty

fromSOACNest' :: (Bindable lore, MonadFreshNames m,
                  LocalScope lore m,
                 Op lore ~ Futhark.SOAC lore) =>
                 [Ident]
              -> SOACNest lore
              -> m (Maybe (MapNest lore))

fromSOACNest' bound (Nest.SOACNest inps
                     (Nest.Map cs w (Nest.NewNest n body@Nest.Map{}))) = do
  Just mn@(MapNest cs' inner_w body' ns' inps') <-
    fromSOACNest' bound' (Nest.SOACNest (Nest.nestingInputs n) body)
  (ps, inps'') <-
    unzip <$> fixInputs (zip (Nest.nestingParamNames n) inps)
                        (zip (params mn) inps')
  let n' = Nesting {
             nestingParamNames   = ps
           , nestingResult       = Nest.nestingResult n
           , nestingReturnType   = Nest.nestingReturnType n
           , nestingWidth        = inner_w
           }
  return $ Just $ MapNest (cs++cs') w body' (n':ns') inps''
  where bound' = bound <>
                 zipWith Ident
                 (Nest.nestingParamNames n)
                 (map (rowType . SOAC.inputType) inps)

fromSOACNest' bound (Nest.SOACNest inps (Nest.Map cs w body)) = do
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
      inps' = map (substituteNames subst) inps ++
              map (SOAC.addTransform (SOAC.Replicate w) . SOAC.identInput)
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
                         lambdaParams l ++ [ Param name t
                                           | Ident name t <- newParams ]
                       }
  return $ Just $
         if HM.null subst
         then MapNest cs w body [] inps
         else MapNest cs w body' [] inps'

fromSOACNest' _ _ = return Nothing

toSOACNest :: MapNest lore -> SOACNest lore
toSOACNest (MapNest cs w body ns inps) =
  Nest.SOACNest inps $ toSOACNest' cs w body ns (map SOAC.inputType inps)

toSOACNest' :: Certificates
            -> SubExp
            -> Nest.NestBody lore
            -> [Nesting lore]
            -> [Type]
            -> Nest.Combinator lore
toSOACNest' cs w body [] _ =
  Nest.Map cs w body
toSOACNest' cs w body (nest:ns) inpts =
  let body' = toSOACNest' cs (nestingWidth nest) body ns (map rowType inpts)
  in Nest.Map cs w (Nest.NewNest nest' body')
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

    ourWidth = arraysSize 0 $ map (SOAC.inputType . snd) ourInps

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
    inspect (remPs, newInps) (_, SOAC.Input ts v _)
      | Just ((p,pInp), remPs') <- findParam remPs v =
          let pInp' = SOAC.transformRows ts pInp
          in return (remPs',
                     (p, pInp') : newInps)

      | Just ((p,pInp), _) <- findParam newInps v = do
          -- The input corresponds to a variable that has already
          -- been used.
          p' <- newNameFromString $ baseString p
          return (remPs, (p', pInp) : newInps)

    inspect (remPs, newInps) (param, SOAC.Input ts a t) = do
      param' <- newNameFromString (baseString param ++ "_rep")
      return (remPs, (param',
                      SOAC.Input (ts SOAC.|> SOAC.Replicate ourWidth) a t) : newInps)
