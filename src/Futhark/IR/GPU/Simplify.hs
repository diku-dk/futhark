{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import qualified Futhark.Analysis.UsageTable as UT
import Futhark.IR.GPU
import qualified Futhark.IR.SOACS.Simplify as SOAC
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import Futhark.Tools

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
  Simplify.SimplifyOp rep op ->
  HostOp (Wise rep) op ->
  Engine.SimpleM rep (HostOp (Wise rep) op, Stms (Wise rep))
simplifyKernelOp f (OtherOp op) = do
  (op', stms) <- f op
  pure (OtherOp op', stms)
simplifyKernelOp _ (SegOp op) = do
  (op', hoisted) <- simplifySegOp op
  pure (SegOp op', hoisted)
simplifyKernelOp _ (SizeOp (SplitSpace o w i elems_per_thread)) =
  (,)
    <$> ( SizeOp
            <$> ( SplitSpace <$> Engine.simplify o <*> Engine.simplify w
                    <*> Engine.simplify i
                    <*> Engine.simplify elems_per_thread
                )
        )
    <*> pure mempty
simplifyKernelOp _ (SizeOp (GetSize key size_class)) =
  pure (SizeOp $ GetSize key size_class, mempty)
simplifyKernelOp _ (SizeOp (GetSizeMax size_class)) =
  pure (SizeOp $ GetSizeMax size_class, mempty)
simplifyKernelOp _ (SizeOp (CmpSizeLe key size_class x)) = do
  x' <- Engine.simplify x
  pure (SizeOp $ CmpSizeLe key size_class x', mempty)
simplifyKernelOp _ (SizeOp (CalcNumGroups w max_num_groups group_size)) = do
  w' <- Engine.simplify w
  pure (SizeOp $ CalcNumGroups w' max_num_groups group_size, mempty)
simplifyKernelOp _ (GPUBody ts body) = do
  ts' <- Engine.simplify ts
  (hoisted, body') <-
    Engine.simplifyBody keepOnGPU mempty (map (const mempty) ts) body
  pure (GPUBody ts' body', hoisted)
  where
    keepOnGPU _ _ = keepExpOnGPU . stmExp
    keepExpOnGPU (BasicOp Index {}) = True
    keepExpOnGPU (BasicOp (ArrayLit _ t)) | primType t = True
    keepExpOnGPU DoLoop {} = True
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
  standardRules <> segOpRules
    <> ruleBook
      [ RuleOp SOAC.simplifyKnownIterationSOAC,
        RuleOp SOAC.removeReplicateMapping,
        RuleOp SOAC.liftIdentityMapping,
        RuleOp SOAC.simplifyMapIota,
        RuleOp SOAC.removeUnusedSOACInput
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
