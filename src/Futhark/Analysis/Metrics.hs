{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- | Abstract Syntax Tree metrics.  This is used in the @futhark test@
-- program, for the @structure@ stanzas.
module Futhark.Analysis.Metrics
  ( AstMetrics (..),
    progMetrics,

    -- * Extensibility
    OpMetrics (..),
    seen,
    inside,
    MetricsM,
    stmMetrics,
    lambdaMetrics,
  )
where

import Control.Monad.Writer
import Data.List (tails)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Futhark.Analysis.Metrics.Type
import Futhark.IR

-- | Compute the metrics for some operation.
class OpMetrics op where
  opMetrics :: op -> MetricsM ()

instance OpMetrics a => OpMetrics (Maybe a) where
  opMetrics Nothing = return ()
  opMetrics (Just x) = opMetrics x

instance OpMetrics () where
  opMetrics () = return ()

newtype CountMetrics = CountMetrics [([Text], Text)]

instance Semigroup CountMetrics where
  CountMetrics x <> CountMetrics y = CountMetrics $ x <> y

instance Monoid CountMetrics where
  mempty = CountMetrics mempty

actualMetrics :: CountMetrics -> AstMetrics
actualMetrics (CountMetrics metrics) =
  AstMetrics $ M.fromListWith (+) $ concatMap expand metrics
  where
    expand (ctx, k) =
      [ (T.intercalate "/" (ctx' ++ [k]), 1)
        | ctx' <- tails $ "" : ctx
      ]

-- | This monad is used for computing metrics.  It internally keeps
-- track of what we've seen so far.  Use 'seen' to add more stuff.
newtype MetricsM a = MetricsM {runMetricsM :: Writer CountMetrics a}
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadWriter CountMetrics
    )

-- | Add this node to the current tally.
seen :: Text -> MetricsM ()
seen k = tell $ CountMetrics [([], k)]

-- | Enclose a metrics counting operation.  Most importantly, this
-- prefixes the name of the context to all the metrics computed in the
-- enclosed operation.
inside :: Text -> MetricsM () -> MetricsM ()
inside what m = seen what >> censor addWhat m
  where
    addWhat (CountMetrics metrics) =
      CountMetrics (map addWhat' metrics)
    addWhat' (ctx, k) = (what : ctx, k)

-- | Compute the metrics for a program.
progMetrics :: OpMetrics (Op rep) => Prog rep -> AstMetrics
progMetrics prog =
  actualMetrics $
    execWriter $
      runMetricsM $ do
        mapM_ funDefMetrics $ progFuns prog
        mapM_ stmMetrics $ progConsts prog

funDefMetrics :: OpMetrics (Op rep) => FunDef rep -> MetricsM ()
funDefMetrics = bodyMetrics . funDefBody

bodyMetrics :: OpMetrics (Op rep) => Body rep -> MetricsM ()
bodyMetrics = mapM_ stmMetrics . bodyStms

-- | Compute metrics for this statement.
stmMetrics :: OpMetrics (Op rep) => Stm rep -> MetricsM ()
stmMetrics = expMetrics . stmExp

expMetrics :: OpMetrics (Op rep) => Exp rep -> MetricsM ()
expMetrics (BasicOp op) =
  seen "BasicOp" >> primOpMetrics op
expMetrics (DoLoop _ ForLoop {} body) =
  inside "DoLoop" $ seen "ForLoop" >> bodyMetrics body
expMetrics (DoLoop _ WhileLoop {} body) =
  inside "DoLoop" $ seen "WhileLoop" >> bodyMetrics body
expMetrics (If _ tb fb _) =
  inside "If" $ do
    inside "True" $ bodyMetrics tb
    inside "False" $ bodyMetrics fb
expMetrics Apply {} =
  seen "Apply"
expMetrics (WithAcc _ lam) =
  inside "MkAcc" $ lambdaMetrics lam
expMetrics (Op op) =
  opMetrics op

primOpMetrics :: BasicOp -> MetricsM ()
primOpMetrics (SubExp _) = seen "SubExp"
primOpMetrics (Opaque _ _) = seen "Opaque"
primOpMetrics ArrayLit {} = seen "ArrayLit"
primOpMetrics BinOp {} = seen "BinOp"
primOpMetrics UnOp {} = seen "UnOp"
primOpMetrics ConvOp {} = seen "ConvOp"
primOpMetrics CmpOp {} = seen "ConvOp"
primOpMetrics Assert {} = seen "Assert"
primOpMetrics Index {} = seen "Index"
primOpMetrics Update {} = seen "Update"
primOpMetrics Concat {} = seen "Concat"
primOpMetrics Copy {} = seen "Copy"
primOpMetrics Manifest {} = seen "Manifest"
primOpMetrics Iota {} = seen "Iota"
primOpMetrics Replicate {} = seen "Replicate"
primOpMetrics Scratch {} = seen "Scratch"
primOpMetrics Reshape {} = seen "Reshape"
primOpMetrics Rearrange {} = seen "Rearrange"
primOpMetrics Rotate {} = seen "Rotate"
primOpMetrics UpdateAcc {} = seen "UpdateAcc"

-- | Compute metrics for this lambda.
lambdaMetrics :: OpMetrics (Op rep) => Lambda rep -> MetricsM ()
lambdaMetrics = bodyMetrics . lambdaBody
