{-# LANGUAGE FlexibleContexts #-}
-- | Alias analysis of a full Futhark program.  Takes as input a
-- program with an arbitrary lore and produces one with aliases.  This
-- module does not implement the aliasing logic itself, and derives
-- its information from definitions in
-- "Futhark.Representation.AST.Attributes.Aliases" and
-- "Futhark.Representation.Aliases".
module Futhark.Analysis.Alias
       ( aliasAnalysis
         -- * Ad-hoc utilities
       , analyseFun
       , analyseBinding
       , analyseExp
       , analyseBody
       , analyseLambda
       , analyseExtLambda
       )
       where

import Data.Monoid

import Futhark.Representation.AST.Attributes (Attributes)
import qualified Futhark.Representation.AST.Syntax as In
import qualified Futhark.Representation.Aliases as Out

import Prelude

-- | Perform alias analysis on a Futhark program.
aliasAnalysis :: (Attributes lore, Out.CanBeAliased (In.Op lore)) =>
                 In.Prog lore -> Out.Prog lore
aliasAnalysis = Out.Prog . map analyseFun . In.progFunctions

analyseFun :: (Attributes lore, Out.CanBeAliased (In.Op lore)) =>
              In.FunDef lore -> Out.FunDef lore
analyseFun (In.FunDef entry fname restype params body) =
  Out.FunDef entry fname restype params body'
  where body' = analyseBody body

analyseBody :: (Attributes lore,
                Out.CanBeAliased (In.Op lore)) =>
               In.Body lore -> Out.Body lore
analyseBody (In.Body lore origbnds result) =
  let bnds' = map analyseBinding origbnds
  in Out.mkAliasedBody lore bnds' result

analyseBinding :: (Attributes lore,
                   Out.CanBeAliased (In.Op lore)) =>
                  In.Binding lore -> Out.Binding lore
analyseBinding (In.Let pat lore e) =
  let e' = analyseExp e
      pat' = Out.addAliasesToPattern pat e'
      lore' = (Out.Names' $ Out.consumedInPattern pat' <> Out.consumedInExp e',
               lore)
  in Out.Let pat' lore' e'

analyseExp :: (Attributes lore, Out.CanBeAliased (In.Op lore)) =>
              In.Exp lore -> Out.Exp lore
analyseExp = Out.mapExp analyse
  where analyse =
          Out.Mapper { Out.mapOnSubExp = return
                     , Out.mapOnCertificates = return
                     , Out.mapOnVName = return
                     , Out.mapOnBody = return . analyseBody
                     , Out.mapOnRetType = return
                     , Out.mapOnFParam = return
                     , Out.mapOnOp = return . Out.addOpAliases
                     }

analyseLambda :: (Attributes lore, Out.CanBeAliased (In.Op lore)) =>
                 In.Lambda lore -> Out.Lambda lore
analyseLambda lam =
  let body = analyseBody $ In.lambdaBody lam
  in lam { Out.lambdaBody = body
         , Out.lambdaParams = In.lambdaParams lam
         }
analyseExtLambda :: (Attributes lore, Out.CanBeAliased (In.Op lore)) =>
                    In.ExtLambda lore -> Out.ExtLambda lore
analyseExtLambda lam =
  let body = analyseBody $ In.extLambdaBody lam
  in lam { Out.extLambdaBody = body
         , Out.extLambdaParams = In.extLambdaParams lam
         }
