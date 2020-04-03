{-# LANGUAGE FlexibleContexts #-}
-- | Alias analysis of a full Futhark program.  Takes as input a
-- program with an arbitrary lore and produces one with aliases.  This
-- module does not implement the aliasing logic itself, and derives
-- its information from definitions in
-- "Futhark.Representation.AST.Attributes.Aliases" and
-- "Futhark.Representation.Aliases".  The alias information computed
-- here will include transitive aliases (note that this is not what
-- the building blocks do).
module Futhark.Analysis.Alias
       ( aliasAnalysis
         -- * Ad-hoc utilities
       , AliasTable
       , analyseFun
       , analyseStm
       , analyseExp
       , analyseBody
       , analyseLambda
       )
       where

import Data.List (foldl')
import qualified Data.Map as M

import Futhark.Representation.AST.Syntax
import Futhark.Representation.Aliases

-- | Perform alias analysis on a Futhark program.
aliasAnalysis :: (Attributes lore, CanBeAliased (Op lore)) =>
                 Prog lore -> Prog (Aliases lore)
aliasAnalysis = Prog . map analyseFun . progFuns

analyseFun :: (Attributes lore, CanBeAliased (Op lore)) =>
              FunDef lore -> FunDef (Aliases lore)
analyseFun (FunDef entry fname restype params body) =
  FunDef entry fname restype params body'
  where body' = analyseBody mempty body

-- | Pre-existing aliases for variables.  Used to add transitive
-- aliases.
type AliasTable = M.Map VName Names

analyseBody :: (Attributes lore,
                CanBeAliased (Op lore)) =>
               AliasTable -> Body lore -> Body (Aliases lore)
analyseBody atable (Body lore stms result) =
  let (stms', _atable') = analyseStms atable stms
  in mkAliasedBody lore stms' result

analyseStms :: (Attributes lore, CanBeAliased (Op lore)) =>
               AliasTable -> Stms lore -> (Stms (Aliases lore), AliasesAndConsumed)
analyseStms orig_aliases =
  foldl' f (mempty, (orig_aliases, mempty)) . stmsToList
  where f (stms, aliases) stm =
          let stm' = analyseStm (fst aliases) stm
              atable' = trackAliases aliases stm'
          in (stms<>oneStm stm', atable')

analyseStm :: (Attributes lore, CanBeAliased (Op lore)) =>
              AliasTable -> Stm lore -> Stm (Aliases lore)
analyseStm aliases (Let pat (StmAux cs attr) e) =
  let e' = analyseExp aliases e
      pat' = addAliasesToPattern pat e'
      lore' = (Names' $ consumedInExp e', attr)
  in Let pat' (StmAux cs lore') e'

analyseExp :: (Attributes lore, CanBeAliased (Op lore)) =>
              AliasTable -> Exp lore -> Exp (Aliases lore)

-- Would be better to put this in a BranchType annotation, but that
-- requires a lot of other work.
analyseExp aliases (If cond tb fb attr) =
  let Body ((tb_als, tb_cons), tb_attr) tb_stms tb_res = analyseBody aliases tb
      Body ((fb_als, fb_cons), fb_attr) fb_stms fb_res = analyseBody aliases fb
      cons = tb_cons <> fb_cons
      isConsumed v = any (`nameIn` unNames cons) $
                     v : namesToList (M.findWithDefault mempty v aliases)
      notConsumed = Names' . namesFromList .
                    filter (not . isConsumed) .
                    namesToList . unNames
      tb_als' = map notConsumed tb_als
      fb_als' = map notConsumed fb_als
      tb' = Body ((tb_als', tb_cons), tb_attr) tb_stms tb_res
      fb' = Body ((fb_als', fb_cons), fb_attr) fb_stms fb_res
  in If cond tb' fb' attr

analyseExp aliases e = mapExp analyse e
  where analyse =
          Mapper { mapOnSubExp = return
                 , mapOnVName = return
                 , mapOnBody = const $ return . analyseBody aliases
                 , mapOnRetType = return
                 , mapOnBranchType = return
                 , mapOnFParam = return
                 , mapOnLParam = return
                 , mapOnOp = return . addOpAliases
                 }

analyseLambda :: (Attributes lore, CanBeAliased (Op lore)) =>
                 Lambda lore -> Lambda (Aliases lore)
analyseLambda lam =
  -- XXX: it may cause trouble that we pass mempty to analyseBody
  -- here.  However, fixing this generally involves adding an
  -- AliasTable argument to addOpAliases.
  let body = analyseBody mempty $ lambdaBody lam
  in lam { lambdaBody = body
         , lambdaParams = lambdaParams lam
         }
