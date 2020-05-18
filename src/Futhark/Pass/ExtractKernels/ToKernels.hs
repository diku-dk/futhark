{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Pass.ExtractKernels.ToKernels
       ( getSize
       , segThread

       , soacsLambdaToKernels
       , soacsStmToKernels
       , scopeForKernels
       , scopeForSOACs
       )
       where

import Control.Monad.Identity
import Data.List ()

import Futhark.Analysis.Rephrase
import Futhark.Representation.AST
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.SOACS.SOAC as SOAC
import Futhark.Representation.Kernels
import Futhark.Tools

getSize :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
           String -> SizeClass -> m SubExp
getSize desc size_class = do
  size_key <- nameFromString . pretty <$> newVName desc
  letSubExp desc $ Op $ SizeOp $ GetSize size_key size_class

segThread :: (MonadBinder m, Op (Lore m) ~ HostOp (Lore m) inner) =>
             String -> m SegLevel
segThread desc =
  SegThread
    <$> (Count <$> getSize (desc ++ "_num_groups") SizeNumGroups)
    <*> (Count <$> getSize (desc ++ "_group_size") SizeGroup)
    <*> pure SegVirt

injectSOACS :: (Monad m,
                SameScope from to,
                ExpDec from ~ ExpDec to,
                BodyDec from ~ BodyDec to,
                RetType from ~ RetType to,
                BranchType from ~ BranchType to,
                Op from ~ SOAC from) =>
               (SOAC to -> Op to) -> Rephraser m from to
injectSOACS f = Rephraser { rephraseExpLore = return
                          , rephraseBodyLore = return
                          , rephraseLetBoundLore = return
                          , rephraseFParamLore = return
                          , rephraseLParamLore = return
                          , rephraseOp = fmap f . onSOAC
                          , rephraseRetType = return
                          , rephraseBranchType = return
                          }
  where onSOAC = SOAC.mapSOACM mapper
        mapper = SOAC.SOACMapper { SOAC.mapOnSOACSubExp = return
                                 , SOAC.mapOnSOACVName = return
                                 , SOAC.mapOnSOACLambda = rephraseLambda $ injectSOACS f
                                 }

soacsStmToKernels :: Stm SOACS -> Stm Kernels
soacsStmToKernels = runIdentity . rephraseStm (injectSOACS OtherOp)

soacsLambdaToKernels :: Lambda SOACS -> Lambda Kernels
soacsLambdaToKernels = runIdentity . rephraseLambda (injectSOACS OtherOp)

scopeForSOACs :: Scope Kernels -> Scope SOACS
scopeForSOACs = castScope

scopeForKernels :: Scope SOACS -> Scope Kernels
scopeForKernels = castScope
