module Futhark.Analysis.Usage
       ( usageInBinding
       , usageInExp
       )
       where

import Data.Monoid
import qualified Data.HashSet as HS

import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Binder (Proper)

usageInBinding :: (Proper lore, Aliased lore) => Binding lore -> UT.UsageTable
usageInBinding (Let pat lore e) =
  mconcat [usageInPat pat,
           usageInExpLore,
           usageInExp e,
           UT.usages (freeNamesInExp e)]
  where usageInPat =
          UT.usages . mconcat . map bindeeUsage . patternBindees
        usageInExpLore =
          UT.usages $ freeNamesIn lore
        bindeeUsage bindee = bindeeName bindee `HS.delete`
                             freeNamesIn bindee

usageInExp :: Aliased lore => Exp lore -> UT.UsageTable
usageInExp (PrimOp (Update _ src _ _)) =
  UT.consumedUsage $ identName src
usageInExp (Apply _ args _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases arg
          | (arg,d) <- args, d == Consume ]
usageInExp (LoopOp (DoLoop _ merge _ _ _)) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases se
          | (v,se) <- merge, unique $ bindeeType v ]
usageInExp (LoopOp (Map _ f args)) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ identAliases se
          | (v,se) <- zip (lambdaParams f) args,
            unique $ identType v ]
usageInExp (LoopOp (Reduce _ f args)) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map subExpAliases acc ++
                       map identAliases arr,
            unique $ identType v ]
  where (acc, arr) = unzip args
usageInExp (LoopOp (Scan _ f args)) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map subExpAliases acc ++
                       map identAliases arr,
            unique $ identType v ]
  where (acc, arr) = unzip args
usageInExp (LoopOp (Redomap _ _ f acc arr)) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map subExpAliases acc ++
                       map identAliases arr,
            unique $ identType v ]
usageInExp _ = UT.empty
