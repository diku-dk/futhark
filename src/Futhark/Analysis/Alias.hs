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
       , analyseStm
       , analyseExp
       , analyseBody
       , analyseLambda
       )
       where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.Aliases

-- | Perform alias analysis on a Futhark program.
aliasAnalysis :: (Attributes lore, CanBeAliased (Op lore)) =>
                 Prog lore -> Prog (Aliases lore)
aliasAnalysis = Prog . map analyseFun . progFunctions

analyseFun :: (Attributes lore, CanBeAliased (Op lore)) =>
              FunDef lore -> FunDef (Aliases lore)
analyseFun (FunDef entry fname restype params body) =
  FunDef entry fname restype params body'
  where body' = analyseBody body

analyseBody :: (Attributes lore,
                CanBeAliased (Op lore)) =>
               Body lore -> Body (Aliases lore)
analyseBody (Body lore origbnds result) =
  let bnds' = fmap analyseStm origbnds
  in mkAliasedBody lore bnds' result

analyseStm :: (Attributes lore, CanBeAliased (Op lore)) =>
              Stm lore -> Stm (Aliases lore)
analyseStm (Let pat (StmAux cs attr) e) =
  let e' = analyseExp e
      pat' = addAliasesToPattern pat e'
      lore' = (Names' $ consumedInExp e', attr)
  in Let pat' (StmAux cs lore') e'

analyseExp :: (Attributes lore, CanBeAliased (Op lore)) =>
              Exp lore -> Exp (Aliases lore)
analyseExp = mapExp analyse
  where analyse =
          Mapper { mapOnSubExp = return
                 , mapOnVName = return
                 , mapOnBody = const $ return . analyseBody
                 , mapOnRetType = return
                 , mapOnBranchType = return
                 , mapOnFParam = return
                 , mapOnLParam = return
                 , mapOnOp = return . addOpAliases
                 }

analyseLambda :: (Attributes lore, CanBeAliased (Op lore)) =>
                 Lambda lore -> Lambda (Aliases lore)
analyseLambda lam =
  let body = analyseBody $ lambdaBody lam
  in lam { lambdaBody = body
         , lambdaParams = lambdaParams lam
         }
