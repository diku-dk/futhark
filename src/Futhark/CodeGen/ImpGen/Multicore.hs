{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  )
  where

import Control.Monad

import Futhark.Error
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp


import Futhark.CodeGen.ImpGen.Kernels.Base

import Futhark.Util (chunks)
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.MonadFreshNames

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


data SubhistosInfo = SubhistosInfo { subhistosArray :: VName
                                   , subhistosAlloc :: CallKernelGen ()
                                   }

data SegHistSlug = SegHistSlug
                   { mySlugOp :: HistOp ExplicitMemory
                   , mySlugNumSubhistos :: VName
                   , mySlugSubhistos :: [SubhistosInfo]
                   , mySlugAtomicUpdate :: AtomicUpdate ExplicitMemory
                   }

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


-- TODOs
-- 0. Clean-up (write wrapper and remove code duplications)
-- 1. Make segMap, segRed and segScan parallel
-- 2. What does 2nd arg to compileStms do?
-- compileSegOp :: Pattern ExplicitMemory -> SegOp ExplicitMemory
--              -> ImpM ExplicitMemory Imp.Multicore ()
-- compileSegOp pat (SegScan _ space lore subexps _ kbody) = do

--   emit $ Imp.DebugPrint "\n# SegScan -- start"  Nothing
--   let (is, ns) = unzip $ unSegSpace space
--   ns' <- mapM toExp ns

--   let (scan_x_params, scan_y_params) =
--         splitAt (length subexps) $ lambdaParams lore

--   -- Set neutral element value
--   dScope Nothing $ scopeOfLParams scan_x_params
--   forM_ (zip scan_x_params subexps) $ \(p, ne) ->
--        copyDWIMFix (paramName p) [] ne []

--   body' <- collect $ do
--     -- Declare y param
--     dScope Nothing $ scopeOfLParams scan_y_params

--     zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
--     sComment "Read value" $ compileStms mempty (kernelBodyStms kbody) $ do
--       let (scan_res, _map_res) = splitAt (length subexps) $ kernelBodyResult kbody
--       forM_ (zip scan_y_params scan_res) $ \(p, se) ->
--         copyDWIMFix (paramName p) [] (kernelResultSubExp se) []

--     sComment "combine with carry and write back to res and accum" $
--       compileStms mempty (bodyStms $ lambdaBody lore) $
--       forM_ (zip3  scan_x_params (patternElements pat) $ bodyResult $ lambdaBody lore) $ \(p, pe, se) -> do
--         copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
--         copyDWIMFix (paramName p) [] se []

--   let paramsNames = namesToList (freeIn body' `namesSubtract` freeIn [segFlat space])
--   ts <- mapM lookupType paramsNames

--   emit $ Imp.Op $ Imp.ParLoopAcc (segFlat space) (product ns') (Imp.MulticoreFunc paramsNames (map getType ts) body')



-- TODO
-- 1. This can't handle multidimensional reductions (e.g. tests/soacs/reduce4.fut)
-- 2. Need to partition reduction into @num_threads smaller Reductions
-- 2.a and add a accumulator loop
compileSegOp pat (SegRed lvl space reds _ body) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  reds_group_res_arrs <- myGroupResultArrays DefaultSpace (Count $ Constant $ IntValue $ Int32Value 10) reds

  thread_id <- dPrim "thread_id" $ IntType Int32
  tid_exp <- toExp $ Var thread_id

  let body'' red_cont = compileStms mempty (kernelBodyStms body) $ do
        let (red_res, _) = splitAt (segRedResults reds) $ kernelBodyResult body
        red_cont $ zip (map kernelResultSubExp red_res) $ repeat []

  -- Creates accumulator variables
  slugs <- mapM mySegRedOpSlug $ zip reds reds_group_res_arrs

  prebody <- collect $ do
    dScope Nothing $ scopeOfLParams $ concatMap mySlugParams slugs
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    sComment "prebody" $
      body'' $ \all_red_res -> do
      let slugs_res = chunks (map (length . mySlugNeutral) slugs) all_red_res
      forM_ (zip slugs slugs_res) $ \(slug, red_res) ->
        forM_ (zip (slugArrs slug) red_res) $ \(slug_arr, (res, res_is)) ->
          copyDWIMFix slug_arr [tid_exp] res (res_is)

  body' <- collect $ do
    -- Intialize function params
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


  let paramsNames = namesToList (freeIn body' `namesIntersection` freeIn prebody `namesSubtract`
                                 (namesFromList $ thread_id : [segFlat space]))
  ts <- mapM lookupType paramsNames
  let params = zipWith getParam paramsNames ts

  emit $ Imp.Op $ Imp.ParLoopAcc (segFlat space) (product ns') (Imp.MulticoreFunc params prebody body' thread_id)


  sComment "neutral-initialise the accumulators" $
    forM_ slugs $ \slug ->
      forM_ (zip (patternElements pat) (mySlugNeutral slug)) $ \(pe, ne) ->
        copyDWIMFix (patElemName pe) [] ne []

  dScope Nothing $ scopeOfLParams $ concatMap mySlugParams slugs
  sFor "i" tid_exp $ \i ->
    sComment "apply map function" $
      forM_ slugs $ \slug ->
        sLoopNest (mySlugShape slug) $ \_vec_is -> do
        -- sComment "load accumulator" $
        forM_ (zip (myAccParams slug) (patternElements pat)) $ \(p, pe) ->
          copyDWIMFix (paramName p) [] (Var $ patElemName pe) []
        -- sComment "set new values to func_param" $
        forM_ (zip (myNextParams slug) (slugArrs slug)) $ \(p, acc) ->
          copyDWIMFix (paramName p) [] (Var acc) ([i])
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


compileSegOp _ op =
  error $ "compileSegOp: unhandled: " ++ pretty op
