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
  , toSOAC
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

import qualified Futhark.Representation.SOACS.SOAC as Futhark
import Futhark.Transform.Substitute
import Futhark.Representation.AST hiding (typeOf)
import Futhark.MonadFreshNames
import Futhark.Construct

data Nesting lore = Nesting {
    nestingParamNames   :: [VName]
  , nestingResult       :: [VName]
  , nestingReturnType   :: [Type]
  , nestingWidth        :: SubExp
  } deriving (Eq, Ord, Show)

data MapNest lore = MapNest Certificates SubExp (Lambda lore) [Nesting lore] [SOAC.Input]
                  deriving (Show)

typeOf :: MapNest lore -> [Type]
typeOf (MapNest _ w lam [] _) =
  map (`arrayOfRow` w) $ lambdaReturnType lam
typeOf (MapNest _ w _ (nest:_) _) =
  map (`arrayOfRow` w) $ nestingReturnType nest

params :: Annotations lore => MapNest lore -> [VName]
params (MapNest _ _ lam [] _)       =
  map paramName $ lambdaParams lam
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
        fixInputs w (zip (map paramName $ lambdaParams lam) inps)
        (zip (params mn) inps')
      let n' = Nesting {
            nestingParamNames   = ps
            , nestingResult     = patternNames pat
            , nestingReturnType = typeOf mn
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
                  map (SOAC.addTransform (SOAC.Replicate $ Shape [w]) . SOAC.identInput)
                  boundUsedInBody
          lam' =
            lam { lambdaBody =
                    substituteNames subst $ lambdaBody lam
                , lambdaParams =
                    lambdaParams lam ++ [ Param name t
                                        | Ident name t <- newParams ]
                }
      return $ Just $ MapNest cs w lam' [] inps'
  where bound' = bound <> map paramIdent (lambdaParams lam)

fromSOAC' _ _ = return Nothing

toSOAC :: (MonadFreshNames m, HasScope lore m,
           Bindable lore, Op lore ~ Futhark.SOAC lore) =>
          MapNest lore -> m (SOAC lore)
toSOAC (MapNest cs w lam [] inps) =
  return $ SOAC.Map cs w lam inps
toSOAC (MapNest cs w lam (Nesting npnames nres nrettype nw:ns) inps) = do
  let nparams = zipWith Param npnames $ map SOAC.inputRowType inps
  (e,bnds) <- runBinder $ localScope (scopeOfLParams nparams) $ SOAC.toExp =<<
    toSOAC (MapNest [] nw lam ns $ map (SOAC.identInput . paramIdent) nparams)
  bnd <- mkLetNames' nres e
  let outerlam = Lambda { lambdaParams = nparams
                        , lambdaBody = mkBody (bnds++[bnd]) $ map Var nres
                        , lambdaReturnType = nrettype
                        }
  return $ SOAC.Map cs w outerlam inps

fixInputs :: MonadFreshNames m =>
             SubExp -> [(VName, SOAC.Input)] -> [(VName, SOAC.Input)]
          -> m [(VName, SOAC.Input)]
fixInputs w ourInps childInps =
  reverse . snd <$> foldM inspect (ourInps, []) childInps
  where
    isParam x (y, _) = x == y

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
                      SOAC.Input (ts SOAC.|> SOAC.Replicate (Shape [w])) a t) : newInps)
