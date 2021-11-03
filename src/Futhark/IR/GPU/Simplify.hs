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

import qualified Futhark.Analysis.SymbolTable as ST
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
import qualified Futhark.Transform.FirstOrderTransform as FOT

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
  return (OtherOp op', stms)
simplifyKernelOp _ (SegOp op) = do
  (op', hoisted) <- simplifySegOp op
  return (SegOp op', hoisted)
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
  return (SizeOp $ GetSize key size_class, mempty)
simplifyKernelOp _ (SizeOp (GetSizeMax size_class)) =
  return (SizeOp $ GetSizeMax size_class, mempty)
simplifyKernelOp _ (SizeOp (CmpSizeLe key size_class x)) = do
  x' <- Engine.simplify x
  return (SizeOp $ CmpSizeLe key size_class x', mempty)
simplifyKernelOp _ (SizeOp (CalcNumGroups w max_num_groups group_size)) = do
  w' <- Engine.simplify w
  return (SizeOp $ CalcNumGroups w' max_num_groups group_size, mempty)

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
      [ --RuleOp redomapIotaToLoop,
        RuleOp SOAC.simplifyMapIota,
        RuleOp SOAC.removeUnusedSOACInput,
        RuleOp SOAC.simplifyKnownIterationSOAC,
        RuleOp SOAC.removeReplicateMapping,
        RuleOp SOAC.liftIdentityMapping
      ]
      [ RuleBasicOp removeUnnecessaryCopy
      ]

-- We turn reductions over (solely) iotas into do-loops, because there
-- is no useful structure here anyway.  This is mostly a hack to work
-- around the fact that loop tiling would otherwise pointlessly tile
-- them.
redomapIotaToLoop :: TopDownRuleOp (Wise GPU)
redomapIotaToLoop vtable pat aux (OtherOp soac@(Screma _ [arr] form))
  | Just _ <- isRedomapSOAC form,
    Just (Iota {}, _) <- ST.lookupBasicOp arr vtable =
    Simplify $ certifying (stmAuxCerts aux) $ FOT.transformSOAC pat soac
redomapIotaToLoop _ _ _ _ =
  Skip
