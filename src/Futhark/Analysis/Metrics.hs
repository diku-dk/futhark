{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Abstract Syntax Tree metrics.  This is used in the @futhark-test@ program.
module Futhark.Analysis.Metrics
       ( AstMetrics
       , progMetrics

         -- * Extensibility
       , OpMetrics(..)
       , seen
       , inside
       , MetricsM
       , bodyMetrics
       ) where

import Control.Applicative
import Control.Monad.Writer
import Data.Text (Text)
import Data.String
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST

type AstMetrics = HM.HashMap Text Int

class OpMetrics op where
  opMetrics :: op -> MetricsM ()

instance OpMetrics () where
  opMetrics () = return ()

-- | This wrapper is just to define a 'Monoid' instance.
newtype CountMetrics = CountMetrics { actualMetrics :: AstMetrics }

instance Monoid CountMetrics where
  mempty = CountMetrics mempty

  mappend (CountMetrics x) (CountMetrics y) = CountMetrics $ HM.unionWith (+) x y

newtype MetricsM a = MetricsM { runMetricsM :: Writer CountMetrics a }
                   deriving (Monad, Applicative, Functor, MonadWriter CountMetrics)

seen :: Text -> MetricsM ()
seen k = tell $ CountMetrics $ HM.singleton k 1

inside :: Text -> MetricsM () -> MetricsM ()
inside what m = seen what >> censor addWhat m
  where addWhat (CountMetrics metrics) =
          CountMetrics metrics <>
          CountMetrics (HM.foldlWithKey' addWhat' mempty metrics)
        addWhat' new_metrics k v =
          HM.insert (what <> "/" <> k) v new_metrics

progMetrics :: OpMetrics (Op lore) => Prog lore -> AstMetrics
progMetrics = actualMetrics . execWriter . runMetricsM . mapM_ funDecMetrics . progFunctions

funDecMetrics :: OpMetrics (Op lore) => FunDec lore -> MetricsM ()
funDecMetrics = bodyMetrics . funDecBody

bodyMetrics :: OpMetrics (Op lore) => Body lore -> MetricsM ()
bodyMetrics = mapM_ bindingMetrics . bodyBindings

bindingMetrics :: OpMetrics (Op lore) => Binding lore -> MetricsM ()
bindingMetrics = expMetrics . bindingExp

expMetrics :: OpMetrics (Op lore) => Exp lore -> MetricsM ()
expMetrics (PrimOp op) =
  seen "PrimOp" >> primOpMetrics op
expMetrics (LoopOp op) =
  seen "LoopOp" >> loopOpMetrics op
expMetrics (SegOp op) =
  seen "SegOp" >> segOpMetrics op
expMetrics (If _ tb fb _) =
  inside "If" $ bodyMetrics tb >> bodyMetrics fb
expMetrics (Apply fname _ _) =
  seen $ "Apply" <> fromString (nameToString fname)
expMetrics (Op op) =
  opMetrics op

primOpMetrics :: PrimOp lore -> MetricsM ()
primOpMetrics (SubExp _) = seen "SubExp"
primOpMetrics ArrayLit{} = seen "ArrayLit"
primOpMetrics BinOp{} = seen "BinOp"
primOpMetrics Not{} = seen "Not"
primOpMetrics Negate{} = seen "Negate"
primOpMetrics Complement{} = seen "Complement"
primOpMetrics Abs{} = seen "Abs"
primOpMetrics Signum{} = seen "Signum"
primOpMetrics Assert{} = seen "Assert"
primOpMetrics Index{} = seen "Index"
primOpMetrics Split{} = seen "Split"
primOpMetrics Concat{} = seen "Concat"
primOpMetrics Copy{} = seen "Copy"
primOpMetrics Iota{} = seen "Iota"
primOpMetrics Replicate{} = seen "Replicate"
primOpMetrics Scratch{} = seen "Scratch"
primOpMetrics Reshape{} = seen "Reshape"
primOpMetrics Rearrange{} = seen "Rearrange"
primOpMetrics Stripe{} = seen "Stripe"
primOpMetrics Unstripe{} = seen "Unstripe"
primOpMetrics Partition{} = seen "Partition"

loopOpMetrics :: OpMetrics (Op lore) => LoopOp lore -> MetricsM ()
loopOpMetrics (DoLoop _ _ ForLoop{} body) =
  inside "DoLoop" $ seen "ForLoop" >> bodyMetrics body
loopOpMetrics (DoLoop _ _ WhileLoop{} body) =
  inside "DoLoop" $ seen "WhileLoop" >> bodyMetrics body
loopOpMetrics (MapKernel _ _ _ _ _ _ body) =
  inside "MapKernel" $ bodyMetrics body
loopOpMetrics (ReduceKernel _ _ _ lam1 lam2 _ _) =
  inside "ReduceKernel" $ lambdaMetrics lam1 >> lambdaMetrics lam2
loopOpMetrics (ScanKernel _ _ _ _ lam _) =
  inside "ScanKernel" $ lambdaMetrics lam

segOpMetrics :: OpMetrics (Op lore) => SegOp lore -> MetricsM ()
segOpMetrics (SegReduce _ _ fun _ _) =
  inside "SegReduce" $ lambdaMetrics fun
segOpMetrics (SegScan _ _ ScanInclusive fun _ _) =
  inside "SegScanInclusive" $ lambdaMetrics fun
segOpMetrics (SegScan _ _ ScanExclusive fun _ _) =
  inside "SegScanExclusive" $ lambdaMetrics fun
segOpMetrics SegReplicate{} =
  seen "SegReplicate"

lambdaMetrics :: OpMetrics (Op lore) => Lambda lore -> MetricsM ()
lambdaMetrics = bodyMetrics . lambdaBody
