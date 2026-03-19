{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.GPU.Simplify
  ( simplifyGPU,
    simplifyLambda,
    GPU,

    -- * Building blocks
    simplifyKernelOp,
  )
where

import Data.List qualified as L
import Futhark.Analysis.DataDependencies
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.IR.GPU
import Futhark.IR.SOACS.Simplify qualified as SOAC
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify qualified as Simplify
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Util (focusNth)

simpleGPU :: Simplify.SimpleOps GPU
simpleGPU = Simplify.bindableSimpleOps $ simplifyKernelOp SOAC.simplifySOAC

simplifyGPU :: Prog GPU -> PassM (Prog GPU)
simplifyGPU =
  Simplify.simplifyProg simpleGPU kernelRules Simplify.noExtraHoistBlockers

simplifyLambda ::
  (HasScope GPU m, MonadFreshNames m) =>
  Lambda GPU ->
  m (Lambda GPU)
simplifyLambda =
  Simplify.simplifyLambda simpleGPU kernelRules Engine.noExtraHoistBlockers

simplifyKernelOp ::
  ( Engine.SimplifiableRep rep,
    BodyDec rep ~ ()
  ) =>
  Simplify.SimplifyOp rep (op (Wise rep)) ->
  HostOp op (Wise rep) ->
  Engine.SimpleM rep (HostOp op (Wise rep), Stms (Wise rep))
simplifyKernelOp f (OtherOp op) = do
  (op', stms) <- f op
  pure (OtherOp op', stms)
simplifyKernelOp _ (SegOp op) = do
  (op', hoisted) <- simplifySegOp op
  pure (SegOp op', hoisted)
simplifyKernelOp _ (SizeOp (GetSize key size_class)) =
  pure (SizeOp $ GetSize key size_class, mempty)
simplifyKernelOp _ (SizeOp (GetSizeMax size_class)) =
  pure (SizeOp $ GetSizeMax size_class, mempty)
simplifyKernelOp _ (SizeOp (CmpSizeLe key size_class x)) = do
  x' <- Engine.simplify x
  pure (SizeOp $ CmpSizeLe key size_class x', mempty)
simplifyKernelOp _ (SizeOp (CalcNumBlocks w max_num_tblocks tblock_size)) = do
  w' <- Engine.simplify w
  pure (SizeOp $ CalcNumBlocks w' max_num_tblocks tblock_size, mempty)
simplifyKernelOp _ (GPUBody ts body) = do
  ts' <- Engine.simplify ts
  (hoisted, body') <-
    Engine.simplifyBody keepOnGPU mempty (map (const mempty) ts) body
  pure (GPUBody ts' body', hoisted)
  where
    keepOnGPU _ _ = keepExpOnGPU . stmExp
    keepExpOnGPU (BasicOp Index {}) = True
    keepExpOnGPU (BasicOp (ArrayLit _ t)) | primType t = True
    keepExpOnGPU Loop {} = True
    keepExpOnGPU _ = False

instance TraverseOpStms (Wise GPU) where
  traverseOpStms = traverseHostOpStms traverseSOACStms

instance BuilderOps (Wise GPU)

instance HasSegOp (Wise GPU) where
  type SegOpLevel (Wise GPU) = SegLevel
  asSegOp (SegOp op) = Just op
  asSegOp _ = Nothing
  segOp = SegOp

instance SOAC.HasSOAC (Wise GPU) where
  asSOAC (OtherOp soac) = Just soac
  asSOAC _ = Nothing
  soacOp = OtherOp

kernelRules :: RuleBook (Wise GPU)
kernelRules =
  standardRules
    <> segOpRules
    <> ruleBook
      [ RuleOp SOAC.simplifyKnownIterationSOAC,
        RuleOp SOAC.removeReplicateMapping,
        RuleOp SOAC.liftIdentityMapping,
        RuleOp SOAC.simplifyMapIota,
        RuleOp SOAC.removeUnusedSOACInput,
        RuleOp removeUnusedKernelBodyResultInSegScan,
        RuleBasicOp removeScalarCopy
      ]
      [ RuleBasicOp removeUnnecessaryCopy,
        RuleOp removeDeadGPUBodyResult
      ]

-- | Remove the unused return values of a GPUBody.
removeDeadGPUBodyResult :: BottomUpRuleOp (Wise GPU)
removeDeadGPUBodyResult (_, used) pat aux (GPUBody types body)
  | -- Figure out which of the names in 'pat' are used...
    pat_used <- map (`UT.isUsedDirectly` used) $ patNames pat,
    -- If they are not all used, then this rule applies.
    not (and pat_used) =
      -- Remove the parts of the GPUBody results that correspond to dead
      -- return value bindings.  Note that this leaves dead code in the
      -- kernel, but that will be removed later.
      let pick :: [a] -> [a]
          pick = map snd . filter fst . zip pat_used
          pat' = pick (patElems pat)
          types' = pick types
          body' = body {bodyResult = pick (bodyResult body)}
       in Simplify $ auxing aux $ letBind (Pat pat') $ Op $ GPUBody types' body'
  | otherwise = Skip
removeDeadGPUBodyResult _ _ _ _ = Skip

-- | Only handle returns cases.
depsOfRes :: Dependencies -> KernelResult -> Names
depsOfRes deps (Returns _ cs se) = depsOf deps se <> depsOfNames deps (freeIn cs)
depsOfRes _ _ = mempty

kernelBodyDependencies :: (ASTRep rep) => Dependencies -> KernelBody rep -> [Names]
kernelBodyDependencies deps kbody =
  let names_in_scope = freeIn kbody
      deps' = dataDependencies' deps kbody
   in map
        (flip namesSubtract names_in_scope . depsOfRes deps')
        (bodyResult kbody)

removeUnusedKernelBodyResultInSegScan ::
  (Buildable rep, BuilderOps rep, HasSegOp rep) =>
  TopDownRuleOp rep
removeUnusedKernelBodyResultInSegScan _ pat aux op
  | -- Figure out which of the names in 'pat' are used...
    Just (SegScan lvl space ts kbody seg_op post_op) <- asSegOp op,
    Just (new_kbody, new_ts, m_new_post_op) <-
      newKbodyPostOp kbody ts seg_op post_op = Simplify $ do
      new_post_op <- m_new_post_op
      auxing aux
        . letBind pat
        . Op
        . segOp
        $ SegScan lvl space new_ts new_kbody seg_op new_post_op
  | otherwise = Skip
  where
    newKbodyPostOp kbody ts seg_op post_op =
      if null sub_map_res_ts_pars
        then Nothing
        else Just (new_kbody, new_ts, new_post_op)
      where
        res = bodyResult kbody
        post_lam = segPostOpLambda post_op
        pars = lambdaParams post_lam

        mkBind t p r =
          mkLet
            [Ident (paramName p) t]
            (BasicOp $ SubExp $ kernelResultSubExp r)

        new_kbody = kbody {bodyResult = new_res}
        new_post_op = do
          let sub_res = map (\(r, _, _, _) -> r) sub_map_res_ts_pars
          temp_body <- renameBody $ kbody {bodyResult = sub_res}
          let new_binds =
                stmsFromList
                  $ zipWith
                    (\(_, t, p, _) r -> mkBind t p r)
                    sub_map_res_ts_pars
                  $ bodyResult temp_body
          pure $
            SegPostOp $
              Lambda
                { lambdaParams = new_pars,
                  lambdaBody =
                    mkBody
                      (bodyStms temp_body <> new_binds <> bodyStms (lambdaBody post_lam))
                      (bodyResult (lambdaBody post_lam)),
                  lambdaReturnType = lambdaReturnType post_lam
                }

        scan_deps = mconcat $ (\(_, _, _, d) -> d) <$> scan_res_ts_pars
        deps = kernelBodyDependencies mempty kbody

        isReturns (Returns {}) = True
        isReturns _ = False

        (new_res, new_ts, new_pars, _) =
          L.unzip4 $ scan_res_ts_pars <> new_map_res_ts_pars
        (new_map_res_ts_pars, sub_map_res_ts_pars) =
          L.partition
            ( \(r, t, _, d) ->
                isReturns r && (d `namesIntersect` scan_deps || isAcc t)
            )
            map_res_ts_pars
        (scan_res_ts_pars, map_res_ts_pars) =
          splitAt (segBinOpResults seg_op) $ L.zip4 res ts pars deps

-- If we see an Update with a scalar where the value to be written is
-- the result of indexing some other array, then we convert it into an
-- Update with a slice of that array.  This matters when the arrays
-- are far away (on the GPU, say), because it avoids a copy of the
-- scalar to and from the host.
removeScalarCopy :: (BuilderOps rep) => TopDownRuleBasicOp rep
removeScalarCopy vtable pat aux (Update safety arr_x (Slice slice_x) (Var v))
  | Just _ <- sliceIndices (Slice slice_x),
    Just (Index arr_y (Slice slice_y), cs_y) <- ST.lookupBasicOp v vtable,
    ST.available arr_y vtable,
    not $ ST.aliases arr_x arr_y vtable,
    Just (slice_x_bef, DimFix i, []) <- focusNth (length slice_x - 1) slice_x,
    Just (slice_y_bef, DimFix j, []) <- focusNth (length slice_y - 1) slice_y = Simplify $ do
      let slice_x' = Slice $ slice_x_bef ++ [DimSlice i (intConst Int64 1) (intConst Int64 1)]
          slice_y' = Slice $ slice_y_bef ++ [DimSlice j (intConst Int64 1) (intConst Int64 1)]
      v' <- letExp (baseName v <> "_slice") $ BasicOp $ Index arr_y slice_y'
      certifying cs_y . auxing aux $
        letBind pat $
          BasicOp $
            Update safety arr_x slice_x' $
              Var v'
removeScalarCopy _ _ _ _ =
  Skip
