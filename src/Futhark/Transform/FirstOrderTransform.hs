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

  -- * Utility
  , doLoopMapAccumL
  , doLoopMapAccumL'
  )
  where

import Control.Monad.Except
import Control.Monad.State
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Futhark.Representation.AST as AST
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Representation.Aliases (Aliases, removeLambdaAliases)
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Util (chunks, splitAt3)

transformFunDef :: (MonadFreshNames m, Bindable tolore, BinderOps tolore,
                    LetAttr SOACS ~ LetAttr tolore,
                    CanBeAliased (Op tolore)) =>
                   FunDef -> m (AST.FunDef tolore)
transformFunDef (FunDef entry fname rettype params body) = do
  (body',_) <- modifyNameSource $ runState $ runBinderT m mempty
  return $ FunDef entry fname rettype params body'
  where m = localScope (scopeOfFParams params) $ insertStmsM $ transformBody body

-- | The constraints that a monad must uphold in order to be used for
-- first-order transformation.
type Transformer m = (MonadBinder m,
                      Bindable (Lore m), BinderOps (Lore m),
                      LocalScope (Lore m) m,
                      LetAttr SOACS ~ LetAttr (Lore m),
                      LParamAttr SOACS ~ LParamAttr (Lore m),
                      CanBeAliased (Op (Lore m)))

transformBody :: Transformer m =>
                 Body -> m (AST.Body (Lore m))
transformBody (Body () bnds res) = insertStmsM $ do
  mapM_ transformStmRecursively bnds
  return $ resultBody res

-- | First transform any nested 'Body' or 'Lambda' elements, then
-- apply 'transformSOAC' if the expression is a SOAC.
transformStmRecursively :: Transformer m =>
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
                                   , mapOnOp = fail "Unhandled Op in first order transform"
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

transformSOAC pat (Screma w form@(ScremaForm (scan_lam, scan_nes)
                                                 (_, red_lam, red_nes)
                                                 map_lam) arrs) = do
  let (scan_arr_ts, _red_ts, map_arr_ts) =
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
  pat' <- discardPattern (map paramType scanacc_params) pat
  letBind_ pat' $ DoLoop [] merge loopform loop_body

transformSOAC pat (Stream w form lam arrs) = do
  -- We just set the chunksize to w and inline the lambda body.  There
  -- is no difference between parallel and sequential streams here.
  let nes = getStreamAccums form
      (chunk_size_param, fold_params, arr_params) =
        partitionChunkedFoldParameters (length nes) $ lambdaParams lam

  -- The chunk size is the full size of the array.
  letBindNames_ [paramName chunk_size_param] $ BasicOp $ SubExp w

  -- The accumulator parameters are initialised to the neutral element.
  forM_ (zip fold_params nes) $ \(p, ne) ->
    letBindNames [paramName p] $ BasicOp $ SubExp ne

  -- Finally, the array parameters are set to the arrays (but reshaped
  -- to make the types work out; this will be simplified rapidly).
  forM_ (zip arr_params arrs) $ \(p, arr) ->
    letBindNames [paramName p] $ BasicOp $
      Reshape (map DimCoercion $ arrayDims $ paramType p) arr

  -- Then we just inline the lambda body.
  mapM_ addStm $ bodyStms $ lambdaBody lam

  -- The number of results in the body matches exactly the size (and
  -- order) of 'pat', so we bind them up here, again with a reshape to
  -- make the types work out.  We also do a copy to ensure that the
  -- result does not have any aliases (as the semantics of Stream
  -- require).
  forM_ (zip (patternElements pat) $ bodyResult $ lambdaBody lam) $ \(pe, se) ->
    case (arrayDims $ patElemType pe, se) of
      (dims, Var v)
        | not $ null dims -> do
            v_reshaped <- letExp (baseString v <> "_reshaped") $
                          BasicOp $ Reshape (map DimCoercion dims) v
            letBindNames_ [patElemName pe] $ BasicOp $ Copy v_reshaped
      _ -> letBindNames_ [patElemName pe] $ BasicOp $ SubExp se

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

transformSOAC pat (GenReduce len ops bucket_fun imgs) = do
  iter <- newVName "iter"

  -- Bind arguments to parameters for the merge-variables.
  hists_ts  <- mapM lookupType $ concatMap genReduceDest ops
  hists_out <- mapM (newIdent "dests") hists_ts
  let merge = loopMerge hists_out $ concatMap (map Var . genReduceDest) ops

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
        vals = chunks (map (length . lambdaReturnType . genReduceOp) ops) $ drop lens imgs''
        hists_out' = chunks (map (length . lambdaReturnType . genReduceOp) ops) $
                     map identName hists_out

    -- Read values from histograms.
    h_vals <- forM (zip inds hists_out') $ \(idx, hist) ->
      forM hist $ \arr -> do
        arr_t <- lookupType arr
        letSubExp "read_hist" $ BasicOp $ Index arr $ fullSlice arr_t [DimFix idx]

    -- Apply operators.
    h_vals' <- forM (zip3 (map genReduceOp ops) vals h_vals) $ \(op, ne_val, h_val) ->
      bindLambda op $ map (BasicOp . SubExp) $ ne_val ++ h_val

    -- Write values back to histograms.
    ress <- forM (zip3 inds h_vals' hists_out') $ \(idx, val, hist) ->
      forM (zip val hist) $  \(v, arr) ->
        letExp "write_hist" =<< eWriteArray arr [eSubExp idx] (eSubExp v)

    return $ resultBody $ map Var $ concat ress
  -- Wrap up the above into a for-loop.
  letBind_ pat $ DoLoop [] merge (ForLoop iter Int32 len []) loopBody

-- | Recursively first-order-transform a lambda.
transformLambda :: (MonadFreshNames m,
                    Bindable lore, BinderOps lore,
                    LocalScope somelore m,
                    SameScope somelore lore,
                    LetAttr SOACS ~ LetAttr lore,
                    CanBeAliased (Op lore)) =>
                   Lambda -> m (AST.Lambda lore)
transformLambda (Lambda params body rettype) = do
  body' <- runBodyBinder $
           localScope (scopeOfLParams params) $
           transformBody body
  return $ Lambda params body' rettype

newFold :: Transformer m =>
           String -> [(SubExp,Type)] -> [VName]
        -> m ([Ident], [SubExp], [Ident])
newFold what accexps_and_types arrexps = do
  initacc <- mapM copyIfArray acc_exps
  acc <- mapM (newIdent "acc") acc_types
  arrts <- mapM lookupType arrexps
  inarrs <- mapM (newIdent $ what ++ "_inarr") arrts
  return (acc, initacc, inarrs)
  where (acc_exps, acc_types) = unzip accexps_and_types

copyIfArray :: Transformer m =>
               SubExp -> m SubExp
copyIfArray (Constant v) = return $ Constant v
copyIfArray (Var v) = Var <$> copyIfArrayName v

copyIfArrayName :: Transformer m =>
                   VName -> m VName
copyIfArrayName v = do
  t <- lookupType v
  case t of
   Array {} -> letExp (baseString v ++ "_first_order_copy") $ BasicOp $ Copy v
   _        -> return v

index :: (HasScope lore m, Monad m) =>
         [VName] -> SubExp -> m [AST.Exp lore]
index arrs i = forM arrs $ \arr -> do
  arr_t <- lookupType arr
  return $ BasicOp $ Index arr $ fullSlice arr_t [DimFix i]

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

discardPattern :: (MonadFreshNames m, LetAttr (Lore m) ~ LetAttr SOACS) =>
                  [Type] -> AST.Pattern (Lore m) -> m (AST.Pattern (Lore m))
discardPattern discard pat = do
  discard_pat <- basicPattern [] <$> mapM (newIdent "discard") discard
  return $ discard_pat <> pat

-- | Turn a Haskell-style mapAccumL into a sequential do-loop.  This
-- is the guts of transforming a 'Redomap'.
doLoopMapAccumL :: (LocalScope (Lore m) m, MonadBinder m,
                    Bindable (Lore m), BinderOps (Lore m),
                    LetAttr (Lore m) ~ Type,
                    CanBeAliased (Op (Lore m))) =>
                   SubExp
                -> AST.Lambda (Aliases (Lore m))
                -> [SubExp]
                -> [VName]
                -> [VName]
                -> m (AST.Exp (Lore m))
doLoopMapAccumL width innerfun accexps arrexps mapout_arrs = do
  (merge, i, loopbody) <-
    doLoopMapAccumL' width innerfun accexps arrexps mapout_arrs
  return $ DoLoop [] merge (ForLoop i Int32 width []) loopbody

doLoopMapAccumL' :: (LocalScope (Lore m) m, MonadBinder m,
                     Bindable (Lore m), BinderOps (Lore m),
                    LetAttr (Lore m) ~ Type,
                    CanBeAliased (Op (Lore m))) =>
                   SubExp
                -> AST.Lambda (Aliases (Lore m))
                -> [SubExp]
                -> [VName]
                -> [VName]
                -> m ([(AST.FParam (Lore m), SubExp)], VName, AST.Body (Lore m))
doLoopMapAccumL' width innerfun accexps arrexps mapout_arrs = do
  i <- newVName "i"
  -- for the MAP    part
  let acc_num     = length accexps
  let res_tps     = lambdaReturnType innerfun
  let map_arr_tps = drop acc_num res_tps
  let res_ts = [ arrayOf t (Shape [width]) NoUniqueness
               | t <- map_arr_tps ]
  let accts = map paramType $ fst $ splitAt acc_num $ lambdaParams innerfun
  outarrs <- mapM (newIdent "mapaccum_outarr") res_ts
  -- for the REDUCE part
  (acc, initacc, inarrs) <- newFold "mapaccum" (zip accexps accts) arrexps
  let consumed = consumedInBody $ lambdaBody innerfun
      withUniqueness p | identName p `S.member` consumed = (p, Unique)
                       | p `elem` outarrs = (p, Unique)
                       | otherwise = (p, Nonunique)
      merge = loopMerge' (map withUniqueness $ inarrs++acc++outarrs)
              (map Var arrexps++initacc++map Var mapout_arrs)
  loopbody <- runBodyBinder $ localScope (scopeOfFParams $ map fst merge) $ do
    accxis<- bindLambda (removeLambdaAliases innerfun) .
             (map (BasicOp . SubExp . Var . identName) acc ++) =<<
              index (map identName inarrs) (Var i)
    let (acc', xis) = splitAt acc_num accxis
    dests <- letwith (map identName outarrs) (pexp (Var i)) $
             map (BasicOp . SubExp) xis
    return $ resultBody (map (Var . identName) inarrs ++ acc' ++ map Var dests)
  return (merge, i, loopbody)
