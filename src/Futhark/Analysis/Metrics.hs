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
       , bindingMetrics
       , lambdaMetrics
       ) where

import Control.Applicative
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.List
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.AST

type AstMetrics = HM.HashMap Text Int

class OpMetrics op where
  opMetrics :: op -> MetricsM ()

instance OpMetrics () where
  opMetrics () = return ()

newtype CountMetrics = CountMetrics [([Text], Text)]

instance Monoid CountMetrics where
  mempty = CountMetrics mempty

  mappend (CountMetrics x) (CountMetrics y) = CountMetrics $ x <> y

actualMetrics :: CountMetrics -> AstMetrics
actualMetrics (CountMetrics metrics) =
  HM.fromListWith (+) $ concatMap expand metrics
  where expand (ctx, k) =
          [ (T.intercalate "/" (ctx' ++ [k]), 1)
          | ctx' <- tails $ "" : ctx ]

newtype MetricsM a = MetricsM { runMetricsM :: Writer CountMetrics a }
                   deriving (Monad, Applicative, Functor, MonadWriter CountMetrics)

seen :: Text -> MetricsM ()
seen k = tell $ CountMetrics [([], k)]

inside :: Text -> MetricsM () -> MetricsM ()
inside what m = seen what >> censor addWhat m
  where addWhat (CountMetrics metrics) =
          CountMetrics (map addWhat' metrics)
        addWhat' (ctx, k) = (what : ctx, k)

progMetrics :: OpMetrics (Op lore) => Prog lore -> AstMetrics
progMetrics = actualMetrics . execWriter . runMetricsM . mapM_ funDefMetrics . progFunctions

funDefMetrics :: OpMetrics (Op lore) => FunDef lore -> MetricsM ()
funDefMetrics = bodyMetrics . funDefBody

bodyMetrics :: OpMetrics (Op lore) => Body lore -> MetricsM ()
bodyMetrics = mapM_ bindingMetrics . bodyBindings

bindingMetrics :: OpMetrics (Op lore) => Binding lore -> MetricsM ()
bindingMetrics = expMetrics . bindingExp

expMetrics :: OpMetrics (Op lore) => Exp lore -> MetricsM ()
expMetrics (PrimOp op) =
  seen "PrimOp" >> primOpMetrics op
expMetrics (DoLoop _ _ ForLoop{} body) =
  inside "DoLoop" $ seen "ForLoop" >> bodyMetrics body
expMetrics (DoLoop _ _ WhileLoop{} body) =
  inside "DoLoop" $ seen "WhileLoop" >> bodyMetrics body
expMetrics (If _ tb fb _) =
  inside "If" $ do
    inside "True" $ bodyMetrics tb
    inside "False" $ bodyMetrics fb
expMetrics (Apply fname _ _) =
  seen $ "Apply" <> fromString (nameToString fname)
expMetrics (Op op) =
  opMetrics op

primOpMetrics :: PrimOp lore -> MetricsM ()
primOpMetrics (SubExp _) = seen "SubExp"
primOpMetrics ArrayLit{} = seen "ArrayLit"
primOpMetrics BinOp{} = seen "BinOp"
primOpMetrics UnOp{} = seen "UnOp"
primOpMetrics ConvOp{} = seen "ConvOp"
primOpMetrics CmpOp{} = seen "ConvOp"
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
primOpMetrics Rotate{} = seen "Rotate"
primOpMetrics Partition{} = seen "Partition"

lambdaMetrics :: OpMetrics (Op lore) => Lambda lore -> MetricsM ()
lambdaMetrics = bodyMetrics . lambdaBody
