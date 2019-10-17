{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive,
-- and - it's certainly worth considering when we can express such
-- transformations in-place.
module Futhark.Transform.FirstOrderTransform
  ( transformFunDef

  , Transformer
  , transformStmRecursively
  , transformLambda
  , transformSOAC
  , transformBody
  )
  where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.List (zip4)

import qualified Futhark.Representation.AST as AST
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Util (chunks, splitAt3)

transformFunDef :: (MonadFreshNames m, Bindable tolore, BinderOps tolore,
                    LetAttr SOACS ~ LetAttr tolore,
                    CanBeAliased (Op tolore)) =>
                   FunDef SOACS -> m (AST.FunDef tolore)
transformFunDef (FunDef entry fname rettype params body) = do
  (body',_) <- modifyNameSource $ runState $ runBinderT m mempty
  return $ FunDef entry fname rettype params body'
  where m = localScope (scopeOfFParams params) $ insertStmsM $ transformBody body

-- | The constraints that a monad must uphold in order to be used for
-- first-order transformation.
type Transformer m = (MonadBinder m,
                      Bindable (Lore m), BinderOps (Lore m),
                      LocalScope (Lore m) m,
                      LParamAttr SOACS ~ LParamAttr (Lore m),
                      CanBeAliased (Op (Lore m)))

transformBody :: (Transformer m, LetAttr (Lore m) ~ LetAttr SOACS) =>
                 Body -> m (AST.Body (Lore m))
transformBody (Body () bnds res) = insertStmsM $ do
  mapM_ transformStmRecursively bnds
  return $ resultBody res

-- | First transform any nested 'Body' or 'Lambda' elements, then
-- apply 'transformSOAC' if the expression is a SOAC.
transformStmRecursively :: (Transformer m, LetAttr (Lore m) ~ LetAttr SOACS) =>
                           Stm -> m ()

transformStmRecursively (Let pat aux (Op soac)) =
  certifying (stmAuxCerts aux) $
  transformSOAC pat =<< mapSOACM soacTransform soac
  where soacTransform = identitySOACMapper { mapOnSOACLambda = transformLambda }

transformStmRecursively (Let pat aux e) =
  certifying (stmAuxCerts aux) $
  letBind_ pat =<< mapExpM transform e
  where transform = identityMapper { mapOnBody = \scope -> localScope scope . transformBody
                                   , mapOnRetType = return
                                   , mapOnBranchType = return
                                   , mapOnFParam = return
                                   , mapOnLParam = return
                                   , mapOnOp = error "Unhandled Op in first order transform"
                                   }

-- | Transform a single 'SOAC' into a do-loop.  The body of the lambda
-- is untouched, and may or may not contain further 'SOAC's depending
-- on the given lore.
transformSOAC :: Transformer m =>
                 AST.Pattern (Lore m)
              -> SOAC (Lore m)
              -> m ()

transformSOAC pat CmpThreshold{} =
  letBind_ pat $ BasicOp $ SubExp $ constant False -- close enough

transformSOAC pat (Screma w form@(ScremaForm (scan_lam, scan_nes) reds map_lam) arrs) = do
  -- Start by combining all the reduction parts into a single operator
  let (Reduce _ red_lam red_nes) = singleReduce reds
      (scan_arr_ts, _red_ts, map_arr_ts) =
        splitAt3 (length scan_nes) (length red_nes) $ scremaType w form
  scan_arrs <- resultArray scan_arr_ts
  map_arrs <- resultArray map_arr_ts

  -- We construct a loop that contains several groups of merge
  -- parameters:
  --
  -- (0) scan accumulator.
  -- (1) scan results.
  -- (2) reduce results (and accumulator).
  -- (3) map results.
  --
  -- Inside the loop, the parameters to map_lam become for-in
  -- parameters.

  scanacc_params <- mapM (newParam "scanacc" . flip toDecl Nonunique) $ lambdaReturnType scan_lam
  scanout_params <- mapM (newParam "scanout" . flip toDecl Unique) scan_arr_ts
  redout_params <- mapM (newParam "redout" . flip toDecl Nonunique) $ lambdaReturnType red_lam
  mapout_params <- mapM (newParam "mapout" . flip toDecl Unique) map_arr_ts

  let merge = concat [zip scanacc_params scan_nes,
                      zip scanout_params $ map Var scan_arrs,
                      zip redout_params red_nes,
                      zip mapout_params $ map Var map_arrs]
  i <- newVName "i"
  let loopform = ForLoop i Int32 w []

  loop_body <- runBodyBinder $
               localScope (scopeOfFParams $ map fst merge) $
               inScopeOf loopform $ do

    forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $ BasicOp $ Index arr $
        fullSlice arr_t [DimFix $ Var i]

    -- Insert the statements of the lambda.  We have taken care to
    -- ensure that the parameters are bound at this point.
    mapM_ addStm $ bodyStms $ lambdaBody map_lam
    -- Split into scan results, reduce results, and map results.
    let (scan_res, red_res, map_res) =
          splitAt3 (length scan_nes) (length red_nes) $
          bodyResult $ lambdaBody map_lam

    scan_res' <- eLambda scan_lam $ map (pure . BasicOp . SubExp) $
                 map (Var . paramName) scanacc_params ++ scan_res
    red_res' <- eLambda red_lam $ map (pure . BasicOp . SubExp) $
                map (Var . paramName) redout_params ++ red_res

    -- Write the scan accumulator to the scan result arrays.
    scan_outarrs <- letwith (map paramName scanout_params) (pexp (Var i)) $
                    map (BasicOp . SubExp) scan_res'

    -- Write the map results to the map result arrays.
    map_outarrs <- letwith (map paramName mapout_params) (pexp (Var i)) $
                   map (BasicOp . SubExp) map_res

    return $ resultBody $ concat [scan_res',
                                  map Var scan_outarrs,
                                  red_res',
                                  map Var map_outarrs]

  -- We need to discard the final scan accumulators, as they are not
  -- bound in the original pattern.
  names <- (++patternNames pat)
           <$> replicateM (length scanacc_params) (newVName "discard")
  letBindNames_ names $ DoLoop [] merge loopform loop_body

transformSOAC pat (Stream w form lam arrs) =
  sequentialStreamWholeArray pat w nes lam arrs
  where nes = getStreamAccums form

transformSOAC pat (Scatter len lam ivs as) = do
  iter <- newVName "write_iter"

  let (_as_ws, as_ns, as_vs) = unzip3 as
  ts <- mapM lookupType as_vs
  asOuts <- mapM (newIdent "write_out") ts

  let ivsLen = length (lambdaReturnType lam) `div` 2

  -- Scatter is in-place, so we use the input array as the output array.
  let merge = loopMerge asOuts $ map Var as_vs
  loopBody <- runBodyBinder $
    localScope (M.insert iter (IndexInfo Int32) $
                scopeOfFParams $ map fst merge) $ do
    ivs' <- forM ivs $ \iv -> do
      iv_t <- lookupType iv
      letSubExp "write_iv" $ BasicOp $ Index iv $ fullSlice iv_t [DimFix $ Var iter]
    ivs'' <- bindLambda lam (map (BasicOp . SubExp) ivs')

    let indexes = chunks as_ns $ take ivsLen ivs''
        values = chunks as_ns $ drop ivsLen ivs''

    ress <- forM (zip3 indexes values (map identName asOuts)) $ \(indexes', values', arr) -> do
      let saveInArray arr' (indexCur, valueCur) =
            letExp "write_out" =<< eWriteArray arr' [eSubExp indexCur] (eSubExp valueCur)

      foldM saveInArray arr $ zip indexes' values'
    return $ resultBody (map Var ress)
  letBind_ pat $ DoLoop [] merge (ForLoop iter Int32 len []) loopBody

transformSOAC pat (Hist len ops bucket_fun imgs) = do
  iter <- newVName "iter"

  -- Bind arguments to parameters for the merge-variables.
  hists_ts  <- mapM lookupType $ concatMap histDest ops
  hists_out <- mapM (newIdent "dests") hists_ts
  let merge = loopMerge hists_out $ concatMap (map Var . histDest) ops

  -- Bind lambda-bodies for operators.
  loopBody <- runBodyBinder $
    localScope (M.insert iter (IndexInfo Int32) $
                scopeOfFParams $ map fst merge) $ do

    -- Bind images to parameters of bucket function.
    imgs' <- forM imgs $ \img -> do
      img_t <- lookupType img
      letSubExp "pixel" $ BasicOp $ Index img $ fullSlice img_t [DimFix $ Var iter]
    imgs'' <- bindLambda bucket_fun $ map (BasicOp . SubExp) imgs'

    -- Split out values from bucket function.
    let lens = length ops
        inds = take lens imgs''
        vals = chunks (map (length . lambdaReturnType . histOp) ops) $ drop lens imgs''
        hists_out' = chunks (map (length . lambdaReturnType . histOp) ops) $
                     map identName hists_out

    hists_out'' <- forM (zip4 hists_out' ops inds vals) $ \(hist, op, idx, val) -> do
      -- Check whether the indexes are in-bound.  If they are not, we
      -- return the histograms unchanged.
      let outside_bounds_branch = insertStmsM $ resultBodyM $ map Var hist
          oob = case hist of [] -> eSubExp $ constant True
                             arr:_ -> eOutOfBounds arr [eSubExp idx]

      letTupExp "new_histo" <=<
        eIf oob outside_bounds_branch $ do
        -- Read values from histogram.
        h_val <- forM hist $ \arr -> do
          arr_t <- lookupType arr
          letSubExp "read_hist" $ BasicOp $ Index arr $ fullSlice arr_t [DimFix idx]

        -- Apply operator.
        h_val' <- bindLambda (histOp op) $
                  map (BasicOp . SubExp) $ h_val ++ val

        -- Write values back to histograms.
        hist' <- forM (zip hist h_val') $  \(arr, v) -> do
          arr_t <- lookupType arr
          letInPlace "hist_out" arr (fullSlice arr_t [DimFix idx]) $
            BasicOp $ SubExp v

        return $ resultBody $ map Var hist'

    return $ resultBody $ map Var $ concat hists_out''

  -- Wrap up the above into a for-loop.
  letBind_ pat $ DoLoop [] merge (ForLoop iter Int32 len []) loopBody

-- | Recursively first-order-transform a lambda.
transformLambda :: (MonadFreshNames m,
                    Bindable lore, BinderOps lore,
                    LocalScope somelore m,
                    SameScope somelore lore,
                    LetAttr lore ~ LetAttr SOACS,
                    CanBeAliased (Op lore)) =>
                   Lambda -> m (AST.Lambda lore)
transformLambda (Lambda params body rettype) = do
  body' <- runBodyBinder $
           localScope (scopeOfLParams params) $
           transformBody body
  return $ Lambda params body' rettype

resultArray :: Transformer m => [Type] -> m [VName]
resultArray = mapM oneArray
  where oneArray t = letExp "result" $ BasicOp $ Scratch (elemType t) (arrayDims t)

letwith :: Transformer m =>
           [VName] -> m (AST.Exp (Lore m)) -> [AST.Exp (Lore m)]
        -> m [VName]
letwith ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  let update k v = do
        k_t <- lookupType k
        letInPlace "lw_dest" k (fullSlice k_t [DimFix i']) $ BasicOp $ SubExp v
  zipWithM update ks vs'

pexp :: Applicative f => SubExp -> f (AST.Exp lore)
pexp = pure . BasicOp . SubExp

bindLambda :: Transformer m =>
              AST.Lambda (Lore m) -> [AST.Exp (Lore m)]
           -> m [SubExp]
bindLambda (Lambda params body _) args = do
  forM_ (zip params args) $ \(param, arg) ->
    if primType $ paramType param
    then letBindNames [paramName param] arg
    else letBindNames [paramName param] =<< eCopy (pure arg)
  bodyBind body

loopMerge :: [Ident] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge vars = loopMerge' $ zip vars $ repeat Unique

loopMerge' :: [(Ident,Uniqueness)] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge' vars vals = [ (Param pname $ toDecl ptype u, val)
                       | ((Ident pname ptype, u),val) <- zip vars vals ]
