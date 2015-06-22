{-# LANGUAGE OverloadedStrings #-}
-- | Abstract Syntax Tree metrics.  This is used in the @futhark-test@ program.
module Futhark.Metrics
       ( AstMetrics
       , progMetrics
       ) where

import Control.Monad.Writer
import Data.Text (Text)
import Data.String
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST

type AstMetrics = HM.HashMap Text Int

-- | This wrapper is just to define a 'Monoid' instance.
newtype CountMetrics = CountMetrics { actualMetrics :: AstMetrics }

instance Monoid CountMetrics where
  mempty = CountMetrics mempty

  mappend (CountMetrics x) (CountMetrics y) = CountMetrics $ HM.unionWith (+) x y

type MetricsM = Writer CountMetrics

seen :: Text -> MetricsM ()
seen k = tell $ CountMetrics $ HM.singleton k 1

progMetrics :: Prog lore -> AstMetrics
progMetrics = actualMetrics . execWriter . mapM_ funDecMetrics . progFunctions

funDecMetrics :: FunDec lore -> MetricsM ()
funDecMetrics = bodyMetrics . funDecBody

bodyMetrics :: Body lore -> MetricsM ()
bodyMetrics = mapM_ bindingMetrics . bodyBindings

bindingMetrics :: Binding lore -> MetricsM ()
bindingMetrics = expMetrics . bindingExp

expMetrics :: Exp lore -> MetricsM ()
expMetrics (PrimOp op) =
  seen "PrimOp" >> primOpMetrics op
expMetrics (LoopOp op) =
  seen "LoopOp" >> loopOpMetrics op
expMetrics (SegOp op) =
  seen "SegOp" >> segOpMetrics op
expMetrics (If _ tb fb _) =
  seen "If" >> bodyMetrics tb >> bodyMetrics fb
expMetrics (Apply fname _ _) =
  seen $ "Apply" <> fromString (nameToString fname)

primOpMetrics :: PrimOp lore -> MetricsM ()
primOpMetrics (SubExp _) = seen "SubExp"
primOpMetrics (ArrayLit {}) = seen "ArrayLit"
primOpMetrics (BinOp {}) = seen "BinOp"
primOpMetrics (Not {}) = seen "Not"
primOpMetrics (Negate {}) = seen "Negate"
primOpMetrics (Complement {}) = seen "Complement"
primOpMetrics (Abs {}) = seen "Abs"
primOpMetrics (Signum {}) = seen "Signum"
primOpMetrics (Assert {}) = seen "Assert"
primOpMetrics (Index {}) = seen "Index"
primOpMetrics (Split {}) = seen "Split"
primOpMetrics (Concat {}) = seen "Concat"
primOpMetrics (Copy {}) = seen "Copy"
primOpMetrics (Iota {}) = seen "Iota"
primOpMetrics (Replicate {}) = seen "Replicate"
primOpMetrics (Scratch {}) = seen "Scratch"
primOpMetrics (Reshape {}) = seen "Reshape"
primOpMetrics (Rearrange {}) = seen "Rearrange"
primOpMetrics (Partition {}) = seen "Partition"
primOpMetrics (Alloc {}) = seen "Alloc"

loopOpMetrics :: LoopOp lore -> MetricsM ()
loopOpMetrics (DoLoop _ _ (ForLoop {}) body) =
  seen "DoLoop" >> seen "ForLoop" >> bodyMetrics body
loopOpMetrics (DoLoop _ _ (WhileLoop {}) body) =
  seen "DoLoop" >> seen "WhileLoop" >> bodyMetrics body
loopOpMetrics (Map _ fun _) =
  seen "Map" >> lambdaMetrics fun
loopOpMetrics (Reduce _ fun _) =
  seen "Reduce" >> lambdaMetrics fun
loopOpMetrics (Scan _ fun _) =
  seen "Scan" >> lambdaMetrics fun
loopOpMetrics (ConcatMap _ fun _) =
  seen "ConcatMap" >> lambdaMetrics fun
loopOpMetrics (Redomap _ fun1 fun2 _ _) =
  seen "Redomap" >> lambdaMetrics fun1 >> lambdaMetrics fun2
loopOpMetrics (Stream _ _ lam _ _) =
  seen "Stream" >> extLambdaMetrics lam

segOpMetrics :: SegOp lore -> MetricsM ()
segOpMetrics (SegReduce _ fun _ _) =
  seen "SegReduce" >> lambdaMetrics fun
segOpMetrics (SegScan _ ScanInclusive fun _ _) =
  seen "SegScanInclusive" >> lambdaMetrics fun
segOpMetrics (SegScan _ ScanExclusive fun _ _) =
  seen "SegScanExclusive" >> lambdaMetrics fun
segOpMetrics (SegReplicate{}) =
  seen "SegReplicate"

lambdaMetrics :: Lambda lore -> MetricsM ()
lambdaMetrics = bodyMetrics . lambdaBody

extLambdaMetrics :: ExtLambda lore -> MetricsM ()
extLambdaMetrics = bodyMetrics . extLambdaBody
