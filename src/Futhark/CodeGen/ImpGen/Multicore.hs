{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  )
  where

import Control.Monad
import Data.Maybe
import Data.Foldable (foldl')
import Data.List


import Futhark.Error
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen.Kernels.Base

import Futhark.Util.IntegralExp (quotRoundingUp)
import Futhark.Util (chunks, splitFromEnd, takeLast, maybeNth)
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.MonadFreshNames
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Construct (fullSliceNum)



compileProg :: MonadFreshNames m => Prog ExplicitMemory
            -> m (Either InternalError Imp.Program)
compileProg = Futhark.CodeGen.ImpGen.compileProg ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler :: OpCompiler ExplicitMemory Imp.Multicore
        opCompiler dest (Alloc e space) =
          compileAlloc dest e space
        opCompiler dest (Inner (SegOp op)) =
          compileSegOp dest op
        opCompiler _ (Inner SizeOp{}) =
          -- FIXME: we will have to do something here at some point.
          return ()
        opCompiler _ (Inner (OtherOp ())) =
          return ()

getParam :: VName -> TypeBase shape u -> Imp.Param
getParam name t = case t of
                    Prim pt      -> Imp.ScalarParam name pt
                    Mem space'   -> Imp.MemParam name space'
                    Array pt _ _ -> Imp.ScalarParam name pt -- TODO: Fix this!

-- |
-- These are copied from SegRed.hs but the module doesn't export them
-- so I copied them for now, Should fix this
myGroupResultArrays :: Traversable t =>
                        Space
                     -> Count u SubExp
                     -> t (SegRedOp lore1)
                     -> ImpM lore2 op (t [VName])
myGroupResultArrays space (Count size) reds =
  forM reds $ \(SegRedOp _ lam _ shape) ->
    forM (lambdaReturnType lam) $ \t -> do
    let pt = elemType t
        full_shape = Shape [size] <> shape <> arrayShape t
        -- Move the groupsize dimension last to ensure coalesced
        -- memory access.
        perm = [1..shapeRank full_shape-1] ++ [0]
    sAllocArrayPerm "group_res_arr" pt full_shape space perm

myGroupResultArraysScan :: Space
                     -> Count u SubExp
                     -> LambdaT lore1
                     -> ImpM lore2 op [VName]
myGroupResultArraysScan space (Count size) (Lambda _ _ retype)  =
  forM retype $ \t -> do
    let pt = elemType t
        full_shape = Shape [size] <> arrayShape t
        -- Move the groupsize dimension last to ensure coalesced
        -- memory access.
        perm = [1..shapeRank full_shape-1] ++ [0]
    sAllocArrayPerm "group_res_arr" pt full_shape space perm



-- | A SegRedOp with auxiliary information.
data MySegRedOpSlug =
  MySegRedOpSlug
  { slugOp :: SegRedOp ExplicitMemory
  , slugArrs :: [VName]
    -- ^ The arrays used for computing the intra-group reduction
    -- (either local or global memory).
  , slugAccs :: [(VName, [Imp.Exp])]
    -- ^ Places to store accumulator in stage 1 reduction.
  }

mySlugAccums :: MySegRedOpSlug -> [VName]
mySlugAccums = map paramName . myAccParams

mySlugBody :: MySegRedOpSlug -> Body ExplicitMemory
mySlugBody = lambdaBody . segRedLambda . slugOp

mySlugParams :: MySegRedOpSlug -> [LParam ExplicitMemory]
mySlugParams = lambdaParams . segRedLambda . slugOp

mySlugNeutral :: MySegRedOpSlug -> [SubExp]
mySlugNeutral = segRedNeutral . slugOp

mySlugShape :: MySegRedOpSlug -> Shape
mySlugShape = segRedShape . slugOp

mySlugsComm :: [MySegRedOpSlug] -> Commutativity
mySlugsComm = mconcat . map (segRedComm . slugOp)

myAccParams, myNextParams :: MySegRedOpSlug -> [LParam ExplicitMemory]
myAccParams slug = take (length (mySlugNeutral slug)) $ mySlugParams slug
myNextParams slug = drop (length (mySlugNeutral slug)) $ mySlugParams slug

mySegRedOpSlug :: (SegRedOp ExplicitMemory, [VName])
                        -> ImpM lore op MySegRedOpSlug
mySegRedOpSlug (op, group_res_arrs) =
  MySegRedOpSlug op group_res_arrs <$>
  mapM mkAcc (lambdaParams (segRedLambda op))
  where mkAcc p
          | Prim t <- paramType p,
            shapeRank (segRedShape op) == 0 = do
              acc <- dPrim (baseString (paramName p) <> "_acc") t
              return (acc, [])
          | otherwise =
              error "couldn't determine type"



data MySegHistSlug = MySegHistSlug
                   { mySlugOp :: HistOp ExplicitMemory
                   }

computeHistoUsage :: HistOp ExplicitMemory
                  -> ImpM ExplicitMemory Imp.Multicore (MySegHistSlug)
computeHistoUsage op =
  return $ MySegHistSlug op


type InitLocalHistograms = [(SubExp ->
                             ImpM ExplicitMemory Imp.Multicore ([VName],
                                                 [Imp.Exp] -> ImpM ExplicitMemory Imp.Multicore ()))]

prepareIntermediateArraysLocal :: VName
                               -> SegSpace -> [MySegHistSlug]
                               -> ImpM ExplicitMemory Imp.Multicore InitLocalHistograms
prepareIntermediateArraysLocal num_subhistos_per_group _space =
  mapM onOp
  where
    onOp (MySegHistSlug op) = do
      mk_op <- return $ \arrs bucket -> do
        let op' =  histOp op
        let (acc_params, _arr_params) = splitAt (length arrs) $ lambdaParams op'
            bind_acc_params =
              sComment "bind lhs" $
                forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
                  copyDWIMFix (paramName acc_p) [] (Var arr) bucket
        let op_body = sComment "execute operation" $
                      compileBody' acc_params $ lambdaBody op'
            do_hist =
              sComment "update sub hist result" $
              zipWithM_ (writeArray bucket) arrs $ map (Var . paramName) acc_params

        sComment "Start of body" $ do
          dLParams acc_params
          bind_acc_params
          op_body
          do_hist

      -- Initialise local-memory sub-histograms.  These are
      -- represented as two-dimensional arrays.
      let init_local_subhistos hist_H_chk = do
            local_subhistos <-
              forM (zip (histType op) (histNeutral op)) $ \(t, ne) -> do
                let sub_local_shape =
                      Shape [Var num_subhistos_per_group] <>
                      (arrayShape t `setOuterDim` hist_H_chk)
                name <- (sAllocArray "subhistogram_local"
                        (elemType t) sub_local_shape DefaultSpace)
                ne' <- toExp ne
                size <- mapM toExp $ shapeDims sub_local_shape
                memSet name (elemType t) (product size) ne'
                return name

            return (local_subhistos, mk_op local_subhistos)

      return init_local_subhistos

    writeArray bucket arr val = copyDWIMFix arr bucket val []


-- TODOs
-- 0. Clean-up (write wrapper and remove code duplications)
-- 1. Make segMap, segRed and segScan parallel
-- 2. What does 2nd arg to compileStms do?
-- 3. tests/soacs/scan2.fut fail (doesn't compile)
compileSegOp :: Pattern ExplicitMemory -> SegOp ExplicitMemory
             -> ImpM ExplicitMemory Imp.Multicore ()

compileSegOp pat  (SegHist _ space histops _ kbody) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (_all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  slugs <- mapM computeHistoUsage histops

  -- Maximum group size (or actual, in this case).
  hist_B' <- toExp $ Constant $ IntValue $ Int32Value 6 -- just to be sure
  -- variable for how many subhistograms to allocate
  hist_M <- dPrimV "hist_M" hist_B'

  init_histograms <-
    prepareIntermediateArraysLocal hist_M space slugs

  hist_H_chks <- forM (map (histWidth . mySlugOp) slugs) $ \w -> do
    w' <- toExp w
    dPrimV "hist_H_chk" $ w'

  thread_id <- dPrim "thread_id" $ IntType Int32
  tid_exp <- toExp $ Var thread_id

  -- Actually allocate subhistograms
  histograms <- forM (zip init_histograms hist_H_chks) $
                \(init_local_subhistos, hist_H_chk) -> do
    (local_subhistos, do_op) <- init_local_subhistos (Var hist_H_chk)
    return (local_subhistos, hist_H_chk, do_op)

  -- Generate body of parallel function
  body' <- collect $ do
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
     compileStms mempty (kernelBodyStms kbody) $ do
       let (red_res, _map_res) = splitFromEnd (length map_pes) $
                            map kernelResultSubExp $ kernelBodyResult kbody
           (buckets, vs) = splitAt (length slugs) red_res
           perOp = chunks $ map (length . histDest . mySlugOp) slugs


       forM_ (zip4 (map mySlugOp slugs) histograms buckets (perOp vs)) $
         \(HistOp dest_w _ _ _ shape lam,
           (_, _hist_H_chk, do_op), bucket, vs') -> do

           let bucket' = toExp' int32 bucket
               dest_w' = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
               vs_params = takeLast (length vs') $ lambdaParams lam
               bucket_is = [tid_exp, bucket']

           sComment "perform updates" $
             sWhen bucket_in_bounds $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params vs') $ \(p, v) ->
                 copyDWIMFix (paramName p) [] v is'
               do_op (bucket_is ++ is')


  let paramsNames = namesToList (freeIn body' `namesSubtract`
                                (namesFromList $ thread_id : [segFlat space]))
  ts <- mapM lookupType paramsNames
  let params = zipWith getParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
                              (Imp.MulticoreFunc params mempty body' thread_id)

  -- second stage
  reduce_body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    let (red_res, _map_res) = splitFromEnd (length map_pes) $
                          map kernelResultSubExp $ kernelBodyResult kbody
        (buckets, vs) = splitAt (length slugs) red_res
        perOp = chunks $ map (length . histDest . mySlugOp) slugs

    sFor "i" tid_exp $ \i -> do
      forM_ (zip4 (map mySlugOp slugs) histograms buckets (perOp vs)) $
          \(HistOp dest_w _ _ _ shape lam,
           (hist, _hist_H_chk, _do_op), bucket, vs') -> do

           let bucket' = toExp' int32 bucket
               dest_w' = toExp' int32 dest_w
               bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
               -- Use splitAt
               vs_params = takeLast (length vs') $ lambdaParams lam
               is_params = take (length vs') $ lambdaParams lam
               bucket_is = i : (map Imp.vi32 is)

           sComment "perform updates" $ do
             dLParams $ lambdaParams lam
             sLoopNest shape $ \is' -> do
               forM_ (zip vs_params hist) $ \(p, v) ->
                 copyDWIMFix (paramName p) [] (Var v) (bucket_is ++ is')
               forM_ (zip (patternElements pat) is_params) $ \(pe, p) ->
                 copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 is)
               compileStms mempty (bodyStms $ lambdaBody lam) $
                 forM_ (zip (patternElements pat) $ bodyResult $ lambdaBody lam) $
                 \(pe, se) -> do
                   copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []

  let paramsNames = namesToList (freeIn reduce_body `namesSubtract`
                                (namesFromList $ [segFlat space]))
  ts <- mapM lookupType paramsNames
  let params = zipWith getParam paramsNames ts

  thread_id2 <- dPrim "thread_id" $ IntType Int32
  num_elem <- mapM toExp $ map Var hist_H_chks

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product num_elem)
                              (Imp.MulticoreFunc params mempty reduce_body thread_id2)


compileSegOp pat (SegScan _ space lore subexps _ kbody) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  thread_id <- dPrim "thread_id" $ IntType Int32
  tid_exp <- toExp $ Var thread_id

  stage_one_red_res <- myGroupResultArraysScan DefaultSpace (Count $ Constant $ IntValue $ Int32Value 10) lore

  let body'' red_cont = compileStms mempty (kernelBodyStms kbody) $ do
        let red_res = kernelBodyResult kbody
        red_cont $ zip (map kernelResultSubExp red_res) $ repeat []

  let (scan_x_params, scan_y_params) =
        splitAt (length subexps) $ lambdaParams lore


  prebody <- collect $ do
    dScope Nothing $ scopeOfLParams $ lambdaParams lore
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    sComment "prebody" $
      body'' $ \all_red_res ->
        forM_ (zip (stage_one_red_res) all_red_res) $ \(slug_arr, (res, res_is)) ->
          copyDWIMFix slug_arr [tid_exp] res (res_is)

    emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)


  reduce_body <- collect $ do
    -- Declare y param
    dScope Nothing $ scopeOfLParams scan_y_params
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space

    sComment "Read value" $ compileStms mempty (kernelBodyStms kbody) $ do
      let (scan_res, _map_res) = splitAt (length subexps) $ kernelBodyResult kbody
      -- Load new values
      sComment "load new values " $
        forM_ (zip scan_y_params scan_res) $ \(p, se) ->
          copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
      -- Load reduce accumulator
      sComment "load accumulator " $
        forM_ (zip (scan_x_params) (stage_one_red_res)) $ \(p, slug_arr) ->
          copyDWIMFix (paramName p) [] (Var slug_arr) [tid_exp]
      sComment "combine with carry and write back to res and accum" $
        compileStms mempty (bodyStms $ lambdaBody lore) $
          forM_ (zip stage_one_red_res $ bodyResult $ lambdaBody lore) $ \(res_arr, se) ->
          copyDWIMFix (res_arr) [tid_exp] se []

  let paramsNames = namesToList (freeIn reduce_body `namesIntersection` freeIn prebody `namesSubtract`
                                 (namesFromList $ thread_id : [segFlat space]))
  ts <- mapM lookupType paramsNames
  let params = zipWith getParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
                                 (Imp.MulticoreFunc params prebody reduce_body thread_id)


  -- Begin stage two of scan
  stage_two_red_res <- myGroupResultArraysScan DefaultSpace (Count $ Var thread_id) lore

  -- Set neutral element value
  dScope Nothing $ scopeOfLParams $ lambdaParams lore
  forM_ (zip stage_two_red_res subexps) $ \(two_res, ne) ->
       copyDWIMFix  two_res [] ne []

  -- Let master thread accumulate "neutral elements" for each segment
  sFor "i" (tid_exp-1) $ \i -> do
    forM_ (zip scan_y_params stage_one_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]

    forM_ (zip scan_x_params stage_two_red_res) $ \(p, se) ->
      copyDWIMFix (paramName p) [] (Var se) [i]

    compileStms mempty (bodyStms $ lambdaBody lore) $
      forM_ (zip stage_two_red_res $ bodyResult $ lambdaBody lore) $ \(scan_arr, se) ->
        copyDWIMFix scan_arr [i+1] se [] -- TODO fix this offset



  -- Prepare function body for second scan iteration
  prebody' <- collect $ do
    dScope Nothing $ scopeOfLParams $ lambdaParams lore
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    -- Set neutral element
    forM_ (zip3  (patternElements pat) scan_x_params stage_two_red_res) $ \(pe, p, ne) -> do
       copyDWIMFix  (paramName p) [] (Var ne) [tid_exp]
       copyDWIMFix (patElemName pe) (map Imp.vi32 is) (Var $ paramName p) []

    sComment "Read value" $ compileStms mempty (kernelBodyStms kbody) $ do
       let (scan_res, _map_res) = splitAt (length subexps) $ kernelBodyResult kbody
       forM_ (zip scan_y_params scan_res) $ \(p, se) -> do
         copyDWIMFix (paramName p) [] (kernelResultSubExp se) []
    sComment "combine with carry and write back to res and accum" $
       compileStms mempty (bodyStms $ lambdaBody lore) $
       forM_ (zip3  scan_x_params (patternElements pat) $ bodyResult $ lambdaBody lore) $ \(p, pe, se) -> do
         copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
         copyDWIMFix (paramName p) [] se []

    emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)

  scan_body <- collect $ do
     dScope Nothing $ scopeOfLParams scan_y_params
     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
     sComment "Read value" $ compileStms mempty (kernelBodyStms kbody) $ do
       let (scan_res, _map_res) = splitAt (length subexps) $ kernelBodyResult kbody
       forM_ (zip scan_y_params scan_res) $ \(p, se) ->
         copyDWIMFix (paramName p) [] (kernelResultSubExp se) []

     sComment "combine with carry and write back to res and accum" $
       compileStms mempty (bodyStms $ lambdaBody lore) $
       forM_ (zip3  scan_x_params (patternElements pat) $ bodyResult $ lambdaBody lore) $ \(p, pe, se) -> do
         copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
         copyDWIMFix (paramName p) [] se []

  let paramsNames' = namesToList ((freeIn scan_body <> freeIn prebody') `namesSubtract`
                                 (namesFromList $ thread_id : segFlat space : (map paramName $ lambdaParams lore)))
  ts' <- mapM lookupType paramsNames'
  let params' = zipWith getParam paramsNames' ts'

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
                                 (Imp.MulticoreFunc params' prebody' scan_body thread_id)




-- TODO
-- 1. This can't handle multidimensional reductions (e.g. tests/soacs/reduce4.fut)
-- 2. Need to partition reduction into @num_threads smaller Reductions
-- 2.a and add a accumulator loop
compileSegOp pat (SegRed _ space reds _ body) = do
  let (is, ns) = unzip $ unSegSpace space

  ns' <- mapM toExp ns
  reds_group_res_arrs <- myGroupResultArrays DefaultSpace (Count $ Constant $ IntValue $ Int32Value 10) reds

  thread_id <- dPrim "thread_id" $ IntType Int32
  tid_exp <- toExp $ Var thread_id

  let body'' red_cont = compileStms mempty (kernelBodyStms body) $ do
        let (red_res, _) = splitAt (segRedResults reds) $ kernelBodyResult body
        red_cont $ zip (map kernelResultSubExp red_res) $ repeat []

  slugs <- mapM mySegRedOpSlug $ zip reds reds_group_res_arrs


  prebody <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    dScope Nothing $ scopeOfLParams $ concatMap mySlugParams slugs
    sComment "prebody" $
      body'' $ \all_red_res -> do
      let slugs_res = chunks (map (length . mySlugNeutral) slugs) all_red_res
      forM_ (zip slugs slugs_res) $ \(slug, red_res) ->
        forM_ (zip (slugArrs slug) red_res) $ \(slug_arr, (res, res_is)) ->
          copyDWIMFix slug_arr [tid_exp] res (res_is)
      emit $ Imp.SetScalar (segFlat space) (Imp.vi32 (segFlat space) + 1)


  body' <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    sComment "apply map function" $
      body'' $ \all_red_res -> do
      let slugs_res' = chunks (map (length . mySlugNeutral) slugs) all_red_res
      forM_ (zip slugs slugs_res') $ \(slug, red_res') ->
        sLoopNest (mySlugShape slug) $ \vec_is -> do
        forM_ (zip (myAccParams slug) (slugArrs slug)) $ \(p, slug_arr) ->
          copyDWIMFix (paramName p) [] (Var slug_arr) [tid_exp]
        forM_ (zip (myNextParams slug) red_res') $ \(p, (res, res_is)) ->
          copyDWIMFix (paramName p) [] res (res_is ++ vec_is)
        compileStms mempty (bodyStms $ mySlugBody slug) $
          forM_ (zip (slugArrs slug) (bodyResult $ mySlugBody slug)) $
            \(slug_arr, se') -> copyDWIMFix slug_arr [tid_exp] se' []


  let paramsNames = namesToList ((freeIn body' `namesIntersection` freeIn prebody) `namesSubtract`
                                 (namesFromList $ thread_id : [segFlat space] ))
  ts <- mapM lookupType paramsNames
  let params = zipWith getParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns') (Imp.MulticoreFunc params prebody body' thread_id)


  forM_ slugs $ \slug ->
    forM_ (zip (patternElements pat) (mySlugNeutral slug)) $ \(pe, ne) ->
      copyDWIMFix (patElemName pe) [] ne []

  dScope Nothing $ scopeOfLParams $ concatMap mySlugParams slugs
  sFor "i" tid_exp $ \i' ->
    sComment "apply map function" $
      forM_ slugs $ \slug ->
        sLoopNest (mySlugShape slug) $ \_vec_is -> do
        -- sComment "load accumulator" $
        forM_ (zip (myAccParams slug) (patternElements pat)) $ \(p, pe) ->
          copyDWIMFix (paramName p) [] (Var $ patElemName pe) []
        -- sComment "set new values to func_param" $
        forM_ (zip (myNextParams slug) (slugArrs slug)) $ \(p, acc) ->
          copyDWIMFix (paramName p) [] (Var acc) [i']
        -- sComment "apply reduction operator" $
        compileStms mempty (bodyStms $ mySlugBody slug) $
            forM_ (zip (patternElements pat) (bodyResult $ mySlugBody slug)) $
            \(pe, se') -> copyDWIMFix (patElemName pe) [] se' []

compileSegOp pat (SegMap _ space _ (KernelBody _ kstms kres)) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  thread_id <- dPrim "thread_id" $ IntType Int32

  body' <- collect $ do
   zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
   compileStms (freeIn kres) kstms $ do
     let writeResult pe (Returns _ se) =
           copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
         writeResult _ res =
           error $ "writeResult: cannot handle " ++ pretty res
     zipWithM_ writeResult (patternElements pat) kres

  let paramsNames = namesToList (freeIn body' `namesSubtract` freeIn [segFlat space])
  ts <- mapM lookupType paramsNames
  let params = zipWith getParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns') (Imp.MulticoreFunc params mempty body' thread_id)



-- A different version of segHist
-- Takes sequential version and simpley chunks is
-- Implementation does currently not ensure amotic updates

-- compileSegOp pat  (SegHist _ space histops _ kbody) = do
--   let (is, ns) = unzip $ unSegSpace space
--   ns' <- mapM toExp ns
--   let num_red_res = length histops + sum (map (length . histNeutral) histops)
--       (_all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

--   body' <- collect $ do
--     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
--     compileStms mempty (kernelBodyStms kbody) $ do
--       let (red_res, _map_res) = splitFromEnd (length map_pes) $
--                                map kernelResultSubExp $ kernelBodyResult kbody
--           (buckets, vs) = splitAt (length histops) red_res
--           perOp = chunks $ map (length . histDest) histops

--       forM_ (zip3 histops (perOp vs) buckets) $
--          \(HistOp dest_w _ _ _ shape lam, vs', bucket) -> do

--            let vs_params = takeLast (length vs') $ lambdaParams lam
--                is_params = take (length vs') $ lambdaParams lam
--                bucket'   = toExp' int32 bucket
--                dest_w'   = toExp' int32 dest_w
--                bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

--            sComment "perform non-atomic updates" $
--              sWhen bucket_in_bounds $ do
--              dLParams $ lambdaParams lam
--              sLoopNest shape $ \_is' -> do
--                -- Index
--                buck <- toExp bucket
--                forM_ (zip (patternElements pat) is_params) $ \(pe, p) ->
--                  copyDWIMFix (paramName p) [] (Var $ patElemName pe) [buck]
--                -- Value at index
--                forM_ (zip vs_params vs') $ \(p, v) ->
--                  copyDWIMFix (paramName p) [] v []
--                compileStms mempty (bodyStms $ lambdaBody lam) $
--                  forM_ (zip (patternElements pat)  $ bodyResult $ lambdaBody lam) $
--                  \(pe, se) ->
--                    copyDWIMFix (patElemName pe) [buck] se [] -- TODO fix this offset


--   thread_id <- dPrim "thread_id" $ IntType Int32

--   let paramsNames = namesToList (freeIn body' `namesSubtract`
--                                 (namesFromList $ thread_id : [segFlat space]))
--   ts <- mapM lookupType paramsNames
--   let params = zipWith getParam paramsNames ts

--   emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns')
--                               (Imp.MulticoreFunc params mempty body' thread_id)
