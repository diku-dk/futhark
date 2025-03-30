{-# LANGUAGE TypeFamilies #-}

module Futhark.Analysis.HORep.MapNest
  ( Nesting (..),
    MapNest (..),
    depth,
    typeOf,
    params,
    inputs,
    setInputs,
    fromSOAC,
    toSOAC,
    reshape,
  )
where

import Control.Monad (replicateM)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC (SOAC)
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Construct
import Futhark.IR hiding (typeOf)
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.SOACS.SOAC qualified as Futhark
import Futhark.Transform.Substitute

data Nesting = Nesting
  { nestingParamNames :: [VName],
    nestingResult :: [VName],
    nestingReturnType :: [Type],
    nestingWidth :: SubExp
  }
  deriving (Eq, Ord, Show)

data MapNest = MapNest
  { mapNestWidth :: SubExp,
    mapNestLambda :: Lambda SOACS,
    mapNestNestings :: [Nesting],
    mapNestInput :: [SOAC.Input]
  }
  deriving (Show)

depth :: MapNest -> Int
depth (MapNest _ _ nests _) = 1 + length nests

typeOf :: MapNest -> [Type]
typeOf (MapNest w lam [] _) =
  map (`arrayOfRow` w) $ lambdaReturnType lam
typeOf (MapNest w _ (nest : _) _) =
  map (`arrayOfRow` w) $ nestingReturnType nest

params :: MapNest -> [VName]
params (MapNest _ lam [] _) =
  map paramName $ lambdaParams lam
params (MapNest _ _ (nest : _) _) =
  nestingParamNames nest

inputs :: MapNest -> [SOAC.Input]
inputs (MapNest _ _ _ inps) = inps

setInputs :: [SOAC.Input] -> MapNest -> MapNest
setInputs [] (MapNest w body ns _) = MapNest w body ns []
setInputs (inp : inps) (MapNest _ body ns _) = MapNest w body ns' (inp : inps)
  where
    w = arraySize 0 $ SOAC.inputType inp
    ws = drop 1 $ arrayDims $ SOAC.inputType inp
    ns' = zipWith setDepth ns ws
    setDepth n nw = n {nestingWidth = nw}

pushIntoMapLambda ::
  Stms SOACS ->
  Stm SOACS ->
  Maybe (Stm SOACS)
pushIntoMapLambda stms (Let pat aux (Op (Futhark.Screma w inps form)))
  | Just map_lam <- Futhark.isMapSOAC form,
    not $ any ((`namesIntersect` bound_by_stms) . freeIn) inps =
      let lam_body = lambdaBody map_lam
          map_lam' =
            map_lam {lambdaBody = lam_body {bodyStms = stms <> bodyStms lam_body}}
          form' = Futhark.mapSOAC map_lam'
       in Just $ Let pat aux (Op (Futhark.Screma w inps form'))
  where
    bound_by_stms = namesFromList $ foldMap (patNames . stmPat) stms
pushIntoMapLambda _ _ = Nothing

massage :: SOAC SOACS -> SOAC SOACS
massage (SOAC.Screma w inps form)
  | Just lam <- Futhark.isMapSOAC form,
    Just (init_stms, last_stm) <- stmsLast $ bodyStms $ lambdaBody lam,
    all (cheap . stmExp) init_stms,
    all (`notNameIn` freeIn (bodyResult (lambdaBody lam))) $
      foldMap (patNames . stmPat) init_stms,
    Just last_stm' <- pushIntoMapLambda init_stms last_stm =
      let lam' =
            lam {lambdaBody = (lambdaBody lam) {bodyStms = oneStm last_stm'}}
       in SOAC.Screma w inps (Futhark.mapSOAC lam')
  where
    cheap (BasicOp BinOp {}) = True
    cheap (BasicOp SubExp {}) = True
    cheap (BasicOp CmpOp {}) = True
    cheap (BasicOp ConvOp {}) = True
    cheap (BasicOp UnOp {}) = True
    cheap _ = False
massage soac = soac

fromSOAC' ::
  (MonadFreshNames m, LocalScope SOACS m) =>
  [Ident] ->
  SOAC SOACS ->
  m (Maybe MapNest)
fromSOAC' bound soac
  | SOAC.Screma w inps (SOAC.ScremaForm lam [] []) <- massage soac = do
      let bound' = bound <> map paramIdent (lambdaParams lam)

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
fromSOAC' _ _ = pure Nothing

fromSOAC :: (MonadFreshNames m, LocalScope SOACS m) => SOAC SOACS -> m (Maybe MapNest)
fromSOAC = fromSOAC' mempty

toSOAC :: (MonadFreshNames m, HasScope SOACS m) => MapNest -> m (SOAC SOACS)
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

-- | Reshape a map nest. It is assumed that any validity tests have
-- already been done. Will automatically reshape the inputs
-- appropriately.
reshape :: (MonadFreshNames m) => Certs -> Shape -> MapNest -> m MapNest
reshape cs shape (MapNest _ map_lam _ inps) =
  descend [] $ stripDims 1 shape
  where
    w = shapeSize 0 shape
    transform p inp =
      let shape' = shape <> arrayShape p
          tr = SOAC.Reshape cs ReshapeArbitrary shape'
       in SOAC.addTransform tr inp
    inps' = zipWith transform (map paramType $ lambdaParams map_lam) inps

    descend nests nest_shape
      | shapeRank nest_shape == 0 =
          pure $ MapNest w map_lam nests inps'
      | otherwise = do
          nest_params <-
            mapM (newVName . baseString . paramName) $
              lambdaParams map_lam
          res <-
            replicateM
              (length $ lambdaReturnType map_lam)
              (newVName "mapnest_res")
          let types =
                map (`arrayOfShape` nest_shape) $ lambdaReturnType map_lam
              nest = Nesting nest_params res types (shapeSize 0 nest_shape)
          descend (nests ++ [nest]) $ stripDims 1 nest_shape
