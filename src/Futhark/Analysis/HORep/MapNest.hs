{-# LANGUAGE TypeFamilies #-}

module Futhark.Analysis.HORep.MapNest
  ( Nesting (..),
    MapNest (..),
    typeOf,
    params,
    inputs,
    setInputs,
    fromSOAC,
    toSOAC,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC (SOAC)
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Construct
import Futhark.IR hiding (typeOf)
import Futhark.IR.SOACS.SOAC qualified as Futhark
import Futhark.Transform.Substitute

data Nesting rep = Nesting
  { nestingParamNames :: [VName],
    nestingResult :: [VName],
    nestingReturnType :: [Type],
    nestingWidth :: SubExp
  }
  deriving (Eq, Ord, Show)

data MapNest rep = MapNest SubExp (Lambda rep) [Nesting rep] [SOAC.Input]
  deriving (Show)

typeOf :: MapNest rep -> [Type]
typeOf (MapNest w lam [] _) =
  map (`arrayOfRow` w) $ lambdaReturnType lam
typeOf (MapNest w _ (nest : _) _) =
  map (`arrayOfRow` w) $ nestingReturnType nest

params :: MapNest rep -> [VName]
params (MapNest _ lam [] _) =
  map paramName $ lambdaParams lam
params (MapNest _ _ (nest : _) _) =
  nestingParamNames nest

inputs :: MapNest rep -> [SOAC.Input]
inputs (MapNest _ _ _ inps) = inps

setInputs :: [SOAC.Input] -> MapNest rep -> MapNest rep
setInputs [] (MapNest w body ns _) = MapNest w body ns []
setInputs (inp : inps) (MapNest _ body ns _) = MapNest w body ns' (inp : inps)
  where
    w = arraySize 0 $ SOAC.inputType inp
    ws = drop 1 $ arrayDims $ SOAC.inputType inp
    ns' = zipWith setDepth ns ws
    setDepth n nw = n {nestingWidth = nw}

fromSOAC ::
  ( Buildable rep,
    MonadFreshNames m,
    LocalScope rep m,
    Op rep ~ Futhark.SOAC rep
  ) =>
  SOAC rep ->
  m (Maybe (MapNest rep))
fromSOAC = fromSOAC' mempty

fromSOAC' ::
  ( Buildable rep,
    MonadFreshNames m,
    LocalScope rep m,
    Op rep ~ Futhark.SOAC rep
  ) =>
  [Ident] ->
  SOAC rep ->
  m (Maybe (MapNest rep))
fromSOAC' bound (SOAC.Screma w inps (SOAC.ScremaForm lam [] [])) = do
  maybenest <- case ( stmsToList $ bodyStms $ lambdaBody lam,
                      bodyResult $ lambdaBody lam
                    ) of
    ([Let pat _ e], res)
      | map resSubExp res == map Var (patNames pat) ->
          localScope (scopeOfLParams $ lambdaParams lam) $
            SOAC.fromExp e
              >>= either (pure . Left) (fmap (Right . fmap (pat,)) . fromSOAC' bound')
    _ ->
      pure $ Right Nothing

  case maybenest of
    -- Do we have a nested MapNest?
    Right (Just (pat, mn@(MapNest inner_w body' ns' inps'))) -> do
      (ps, inps'') <-
        unzip
          <$> fixInputs
            w
            (zip (map paramName $ lambdaParams lam) inps)
            (zip (params mn) inps')
      let n' =
            Nesting
              { nestingParamNames = ps,
                nestingResult = patNames pat,
                nestingReturnType = typeOf mn,
                nestingWidth = inner_w
              }
      pure $ Just $ MapNest w body' (n' : ns') inps''
    -- No nested MapNest it seems.
    _ -> do
      let isBound name
            | Just param <- find ((name ==) . identName) bound =
                Just param
            | otherwise =
                Nothing
          boundUsedInBody =
            mapMaybe isBound $ namesToList $ freeIn lam
      newParams <- mapM (newIdent' (++ "_wasfree")) boundUsedInBody
      let subst =
            M.fromList $
              zip (map identName boundUsedInBody) (map identName newParams)
          inps' =
            inps
              ++ map
                (SOAC.addTransform (SOAC.Replicate mempty $ Shape [w]) . SOAC.identInput)
                boundUsedInBody
          lam' =
            lam
              { lambdaBody =
                  substituteNames subst $ lambdaBody lam,
                lambdaParams =
                  lambdaParams lam
                    ++ [Param mempty name t | Ident name t <- newParams]
              }
      pure $ Just $ MapNest w lam' [] inps'
  where
    bound' = bound <> map paramIdent (lambdaParams lam)
fromSOAC' _ _ = pure Nothing

toSOAC ::
  ( MonadFreshNames m,
    HasScope rep m,
    Buildable rep,
    BuilderOps rep,
    Op rep ~ Futhark.SOAC rep
  ) =>
  MapNest rep ->
  m (SOAC rep)
toSOAC (MapNest w lam [] inps) =
  pure $ SOAC.Screma w inps (Futhark.mapSOAC lam)
toSOAC (MapNest w lam (Nesting npnames nres nrettype nw : ns) inps) = do
  let nparams = zipWith (Param mempty) npnames $ map SOAC.inputRowType inps
  body <- runBodyBuilder $
    localScope (scopeOfLParams nparams) $ do
      letBindNames nres
        =<< SOAC.toExp
        =<< toSOAC (MapNest nw lam ns $ map (SOAC.identInput . paramIdent) nparams)
      pure $ varsRes nres
  let outerlam =
        Lambda
          { lambdaParams = nparams,
            lambdaBody = body,
            lambdaReturnType = nrettype
          }
  pure $ SOAC.Screma w inps (Futhark.mapSOAC outerlam)

fixInputs ::
  (MonadFreshNames m) =>
  SubExp ->
  [(VName, SOAC.Input)] ->
  [(VName, SOAC.Input)] ->
  m [(VName, SOAC.Input)]
fixInputs w ourInps = mapM inspect
  where
    isParam x (y, _) = x == y

    inspect (_, SOAC.Input ts v _)
      | Just (p, pInp) <- find (isParam v) ourInps = do
          let pInp' = SOAC.transformRows ts pInp
          p' <- newNameFromString $ baseString p
          pure (p', pInp')
    inspect (param, SOAC.Input ts a t) = do
      param' <- newNameFromString (baseString param ++ "_rep")
      pure (param', SOAC.Input (ts SOAC.|> SOAC.Replicate mempty (Shape [w])) a t)
