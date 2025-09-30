-- | Multicore code generation for SegScan. Uses a fairly naive multipass
-- algorithm, with no particular locality optimisations.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Futhark.CodeGen.ImpGen.Multicore.SegScan
  ( compileSegScan,
  )
where

import Control.Monad
import Data.List (zip4)
import Futhark.CodeGen.ImpCode.Multicore qualified as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.IR.MCMem
import Futhark.Util.IntegralExp (quot, rem)
import Prelude hiding (quot, rem)
import Futhark.Util.Pretty (prettyString)


xParams, yParams :: SegBinOp MCMem -> [LParam MCMem]
xParams scan =
  take (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))
yParams scan =
  drop (length (segBinOpNeutral scan)) (lambdaParams (segBinOpLambda scan))

lamBody :: SegBinOp MCMem -> Body MCMem
lamBody = lambdaBody . segBinOpLambda

genBinOpParams :: [SegBinOp MCMem] -> MulticoreGen ()
genBinOpParams scan_ops =
  dScope Nothing $
    scopeOfLParams $
      concatMap (lambdaParams . segBinOpLambda) scan_ops

-- | Compile a SegScan construct.
compileSegScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
compileSegScan pat space reds kbody nsubtasks
  | [_] <- unSegSpace space =
      nonsegmentedScan pat space reds kbody nsubtasks
  | otherwise =
      error "only nonsegmented scans for now"

nonsegmentedScan ::
  Pat LetDecMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen ()
nonsegmentedScan
  (Pat [pe])
  (SegSpace _ [(i, n)])
  [scan_op]
  (Body _ kstms [Returns _ _ res])
  _nsubtasks = do
    emit $ Imp.DebugPrint "nonsegmented segScan" Nothing

    genBinOpParams [scan_op]

    forM_ (zip (xParams scan_op) (segBinOpNeutral scan_op)) $ \(p, ne) ->
      copyDWIMFix (paramName p) [] ne []

    emit $ Imp.DebugPrint ("KERNEL BODY:\n" ++ prettyString kstms) Nothing
    emit $ Imp.DebugPrint ("KERNEL Res:\n" ++ prettyString res) Nothing
    emit $ Imp.DebugPrint ("Lambda BODY:\n" ++ prettyString (bodyStms $ lamBody scan_op)) Nothing

    sFor "j" (pe64 n) $ \j -> do
      dPrimV_ i j
      compileStms mempty kstms $ do
        forM_ (zip (yParams scan_op) [res]) $ \(p, se) ->
          copyDWIMFix (paramName p) [] se []

      compileStms mempty (bodyStms $ lamBody scan_op) $ do
        forM_ (zip (map resSubExp $ bodyResult $ lamBody scan_op) (xParams scan_op))  $ \(se,px) -> do 
          copyDWIMFix (patElemName pe) [j] se []
          copyDWIMFix (paramName px) [] se [] 
          