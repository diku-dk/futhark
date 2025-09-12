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
