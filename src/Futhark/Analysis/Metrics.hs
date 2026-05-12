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
    bodyMetrics,
  )
where

import Control.Monad
import Control.Monad.Writer
import Data.List (tails)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Futhark.Analysis.Metrics.Type
import Futhark.IR
import Futhark.Util (showText)

-- | Compute the metrics for some operation.
class OpMetrics op where
  opMetrics :: op -> MetricsM ()

instance (OpMetrics a) => OpMetrics (Maybe a) where
  opMetrics Nothing = pure ()
  opMetrics (Just x) = opMetrics x

instance OpMetrics (NoOp rep) where
  opMetrics NoOp = pure ()

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
progMetrics :: (OpMetrics (Op rep)) => Prog rep -> AstMetrics
progMetrics prog =
  actualMetrics $
    execWriter $
      runMetricsM $ do
        mapM_ funDefMetrics $ progFuns prog
        mapM_ stmMetrics $ progConsts prog

funDefMetrics :: (OpMetrics (Op rep)) => FunDef rep -> MetricsM ()
funDefMetrics = bodyMetrics . funDefBody

-- | Compute metrics for this body.
bodyMetrics :: (OpMetrics (Op rep)) => GBody rep res -> MetricsM ()
bodyMetrics = mapM_ stmMetrics . bodyStms

-- | Compute metrics for this statement.
stmMetrics :: (OpMetrics (Op rep)) => Stm rep -> MetricsM ()
stmMetrics = expMetrics . stmExp

expMetrics :: (OpMetrics (Op rep)) => Exp rep -> MetricsM ()
expMetrics (BasicOp op) =
  seen "BasicOp" >> basicOpMetrics op
expMetrics (Loop _ ForLoop {} body) =
  inside "Loop" $ seen "ForLoop" >> bodyMetrics body
expMetrics (Loop _ WhileLoop {} body) =
  inside "Loop" $ seen "WhileLoop" >> bodyMetrics body
expMetrics (Match _ [Case [Just (BoolValue True)] tb] fb _) =
  inside "If" $ do
    inside "True" $ bodyMetrics tb
    inside "False" $ bodyMetrics fb
expMetrics (Match _ cases defbody _) =
  inside "Match" $ do
    forM_ (zip [0 ..] cases) $ \(i, c) ->
      inside (showText (i :: Int)) $ bodyMetrics $ caseBody c
    inside "default" $ bodyMetrics defbody
expMetrics Apply {} =
  seen "Apply"
expMetrics (WithAcc _ lam) =
  inside "WithAcc" $ lambdaMetrics lam
expMetrics (Op op) =
  opMetrics op

basicOpMetrics :: BasicOp -> MetricsM ()
basicOpMetrics (SubExp _) = seen "SubExp"
basicOpMetrics (Opaque _ _) = seen "Opaque"
basicOpMetrics ArrayVal {} = seen "ArrayVal"
basicOpMetrics ArrayLit {} = seen "ArrayLit"
basicOpMetrics BinOp {} = seen "BinOp"
basicOpMetrics UnOp {} = seen "UnOp"
basicOpMetrics ConvOp {} = seen "ConvOp"
basicOpMetrics CmpOp {} = seen "CmpOp"
basicOpMetrics Assert {} = seen "Assert"
basicOpMetrics Index {} = seen "Index"
basicOpMetrics Update {} = seen "Update"
basicOpMetrics FlatIndex {} = seen "FlatIndex"
basicOpMetrics FlatUpdate {} = seen "FlatUpdate"
basicOpMetrics Concat {} = seen "Concat"
basicOpMetrics Manifest {} = seen "Manifest"
basicOpMetrics Iota {} = seen "Iota"
basicOpMetrics Replicate {} = seen "Replicate"
basicOpMetrics Scratch {} = seen "Scratch"
basicOpMetrics Reshape {} = seen "Reshape"
basicOpMetrics Rearrange {} = seen "Rearrange"
basicOpMetrics UpdateAcc {} = seen "UpdateAcc"
basicOpMetrics UserParam {} = seen "UserParam"

-- | Compute metrics for this lambda.
lambdaMetrics :: (OpMetrics (Op rep)) => Lambda rep -> MetricsM ()
lambdaMetrics = bodyMetrics . lambdaBody
