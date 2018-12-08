{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Abstract Syntax Tree metrics.  This is used in the @futhark-test@ program.
module Futhark.Analysis.Metrics
       ( AstMetrics(..)
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

import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Semigroup as Sem

import Futhark.Representation.AST

newtype AstMetrics = AstMetrics (M.Map Text Int)

instance Show AstMetrics where
  show (AstMetrics m) = unlines $ map metric $ M.toList m
    where metric (k, v) = pretty k ++ " " ++ pretty v

instance Read AstMetrics where
  readsPrec _ s =
    maybe [] success $ mapM onLine $ lines s
    where onLine l = case words l of
                       [k, x] | [(n, "")] <- reads x -> Just (T.pack k, n)
                       _ -> Nothing
          success m = [(AstMetrics $ M.fromList m, "")]

class OpMetrics op where
  opMetrics :: op -> MetricsM ()

instance OpMetrics () where
  opMetrics () = return ()

newtype CountMetrics = CountMetrics [([Text], Text)]

instance Sem.Semigroup CountMetrics where
  CountMetrics x <> CountMetrics y = CountMetrics $ x <> y

instance Monoid CountMetrics where
  mempty = CountMetrics mempty
  mappend = (Sem.<>)

actualMetrics :: CountMetrics -> AstMetrics
actualMetrics (CountMetrics metrics) =
  AstMetrics $ M.fromListWith (+) $ concatMap expand metrics
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
bodyMetrics = mapM_ bindingMetrics . bodyStms

bindingMetrics :: OpMetrics (Op lore) => Stm lore -> MetricsM ()
bindingMetrics = expMetrics . stmExp

expMetrics :: OpMetrics (Op lore) => Exp lore -> MetricsM ()
expMetrics (BasicOp op) =
  seen "BasicOp" >> primOpMetrics op
expMetrics (DoLoop _ _ ForLoop{} body) =
  inside "DoLoop" $ seen "ForLoop" >> bodyMetrics body
expMetrics (DoLoop _ _ WhileLoop{} body) =
  inside "DoLoop" $ seen "WhileLoop" >> bodyMetrics body
expMetrics (If _ tb fb _) =
  inside "If" $ do
    inside "True" $ bodyMetrics tb
    inside "False" $ bodyMetrics fb
expMetrics (Apply fname _ _ _) =
  seen $ "Apply" <> fromString (nameToString fname)
expMetrics (Op op) =
  opMetrics op

primOpMetrics :: BasicOp lore -> MetricsM ()
primOpMetrics (SubExp _) = seen "SubExp"
primOpMetrics (Opaque _) = seen "Opaque"
primOpMetrics ArrayLit{} = seen "ArrayLit"
primOpMetrics BinOp{} = seen "BinOp"
primOpMetrics UnOp{} = seen "UnOp"
primOpMetrics ConvOp{} = seen "ConvOp"
primOpMetrics CmpOp{} = seen "ConvOp"
primOpMetrics Assert{} = seen "Assert"
primOpMetrics Index{} = seen "Index"
primOpMetrics Update{} = seen "Update"
primOpMetrics Concat{} = seen "Concat"
primOpMetrics Copy{} = seen "Copy"
primOpMetrics Manifest{} = seen "Manifest"
primOpMetrics Iota{} = seen "Iota"
primOpMetrics Replicate{} = seen "Replicate"
primOpMetrics Repeat{} = seen "Repeat"
primOpMetrics Scratch{} = seen "Scratch"
primOpMetrics Reshape{} = seen "Reshape"
primOpMetrics Rearrange{} = seen "Rearrange"
primOpMetrics Rotate{} = seen "Rotate"

lambdaMetrics :: OpMetrics (Op lore) => Lambda lore -> MetricsM ()
lambdaMetrics = bodyMetrics . lambdaBody
