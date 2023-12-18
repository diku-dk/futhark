{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.ExtractKernels.ToGPU
  ( getSize,
    segThread,
    soacsLambdaToGPU,
    soacsStmToGPU,
    scopeForGPU,
    scopeForSOACs,
    injectSOACS,
  )
where

import Control.Monad.Identity
import Data.List ()
import Futhark.IR
import Futhark.IR.GPU
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.SOACS.SOAC qualified as SOAC
import Futhark.Tools

getSize ::
  (MonadBuilder m, Op (Rep m) ~ HostOp inner (Rep m)) =>
  String ->
  SizeClass ->
  m SubExp
getSize desc size_class = do
  size_key <- nameFromString . prettyString <$> newVName desc
  letSubExp desc $ Op $ SizeOp $ GetSize size_key size_class

segThread ::
  (MonadBuilder m, Op (Rep m) ~ HostOp inner (Rep m)) =>
  String ->
  m SegLevel
segThread desc =
  SegThread SegVirt <$> (Just <$> kernelGrid)
  where
    kernelGrid =
      KernelGrid
        <$> (Count <$> getSize (desc ++ "_num_tblocks") SizeGrid)
        <*> (Count <$> getSize (desc ++ "_tblock_size") SizeThreadBlock)

injectSOACS ::
  ( Monad m,
    SameScope from to,
    ExpDec from ~ ExpDec to,
    BodyDec from ~ BodyDec to,
    RetType from ~ RetType to,
    BranchType from ~ BranchType to,
    Op from ~ SOAC from
  ) =>
  (SOAC to -> Op to) ->
  Rephraser m from to
injectSOACS f =
  Rephraser
    { rephraseExpDec = pure,
      rephraseBodyDec = pure,
      rephraseLetBoundDec = pure,
      rephraseFParamDec = pure,
      rephraseLParamDec = pure,
      rephraseOp = fmap f . onSOAC,
      rephraseRetType = pure,
      rephraseBranchType = pure
    }
  where
    onSOAC = SOAC.mapSOACM mapper
    mapper =
      SOAC.SOACMapper
        { SOAC.mapOnSOACSubExp = pure,
          SOAC.mapOnSOACVName = pure,
          SOAC.mapOnSOACLambda = rephraseLambda $ injectSOACS f
        }

soacsStmToGPU :: Stm SOACS -> Stm GPU
soacsStmToGPU = runIdentity . rephraseStm (injectSOACS OtherOp)

soacsLambdaToGPU :: Lambda SOACS -> Lambda GPU
soacsLambdaToGPU = runIdentity . rephraseLambda (injectSOACS OtherOp)

scopeForSOACs :: Scope GPU -> Scope SOACS
scopeForSOACs = castScope

scopeForGPU :: Scope SOACS -> Scope GPU
scopeForGPU = castScope
