{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Futhark.Analysis.HORepresentation.MapNest
  ( Nesting (..)
  , MapNest (..)
  , typeOf
  , params
  , inputs
  , setInputs
  , fromSOAC
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
import Futhark.Analysis.HORepresentation.SOAC (SOAC)
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

fromSOAC :: (Bindable lore, MonadFreshNames m,
             LocalScope lore m,
             Op lore ~ Futhark.SOAC lore) =>
            SOAC lore -> m (Maybe (MapNest lore))
fromSOAC = fromSOAC' mempty

fromSOAC' :: (Bindable lore, MonadFreshNames m,
              LocalScope lore m,
              Op lore ~ Futhark.SOAC lore) =>
             [Ident]
          -> SOAC lore
          -> m (Maybe (MapNest lore))

fromSOAC' bound (SOAC.Map cs w lam inps) = do

  maybenest <- case lambdaBody lam of
    Body _ [Let pat _ e] res | res == map Var (patternNames pat) ->
      either (return . Left) (fmap (Right . fmap (pat,)) . fromSOAC' bound') =<< SOAC.fromExp e
    _ ->
      return $ Right Nothing
  case maybenest of
    -- Do we have a nested MapNest?
    Right (Just (pat, mn@(MapNest cs' inner_w body' ns' inps'))) -> do
      (ps, inps'') <-
        unzip <$>
        fixInputs (zip (map paramName $ lambdaParams lam) inps)
        (zip (params mn) inps')
      let n' = Nesting {
            nestingParamNames   = ps
            , nestingResult     = patternNames pat
            , nestingReturnType = lambdaReturnType lam
            , nestingWidth      = inner_w
            }
      return $ Just $ MapNest (cs++cs') w body' (n':ns') inps''
    -- No nested MapNest it seems.
    _ -> do
      let isBound name
            | Just param <- find ((name==) . identName) bound =
              Just param
            | otherwise =
              Nothing
          boundUsedInBody =
            mapMaybe isBound $ HS.toList $ freeInLambda lam
      newParams <- mapM (newIdent' (++"_wasfree")) boundUsedInBody
      let subst = HM.fromList $
                  zip (map identName boundUsedInBody) (map identName newParams)
          inps' = map (substituteNames subst) inps ++
                  map (SOAC.addTransform (SOAC.Replicate w) . SOAC.identInput)
                  boundUsedInBody
          body =
            Nest.Fun lam { lambdaBody =
                           substituteNames subst $ lambdaBody lam
                         , lambdaParams =
                           lambdaParams lam ++ [ Param name t
                                               | Ident name t <- newParams ]
                         }
      return $ Just $ MapNest cs w body [] inps'
  where bound' = bound <> map paramIdent (lambdaParams lam)

fromSOAC' _ _ = return Nothing

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
