{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Futhark.Analysis.Rephrase
import Futhark.IR
import Futhark.IR.GPU
import Futhark.IR.SOACS (SOACS)
import qualified Futhark.IR.SOACS.SOAC as SOAC
import Futhark.Tools

getSize ::
  (MonadBuilder m, Op (Rep m) ~ HostOp (Rep m) inner) =>
  String ->
  SizeClass ->
  m SubExp
getSize desc size_class = do
  size_key <- nameFromString . pretty <$> newVName desc
  letSubExp desc $ Op $ SizeOp $ GetSize size_key size_class

segThread ::
  (MonadBuilder m, Op (Rep m) ~ HostOp (Rep m) inner) =>
  String ->
  m SegLevel
segThread desc =
  SegThread
    <$> (Count <$> getSize (desc ++ "_num_groups") SizeNumGroups)
    <*> (Count <$> getSize (desc ++ "_group_size") SizeGroup)
    <*> pure SegVirt

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
    { rephraseExpDec = return,
      rephraseBodyDec = return,
      rephraseLetBoundDec = return,
      rephraseFParamDec = return,
      rephraseLParamDec = return,
      rephraseOp = fmap f . onSOAC,
      rephraseRetType = return,
      rephraseBranchType = return
    }
  where
    onSOAC = SOAC.mapSOACM mapper
    mapper =
      SOAC.SOACMapper
        { SOAC.mapOnSOACSubExp = return,
          SOAC.mapOnSOACVName = return,
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
