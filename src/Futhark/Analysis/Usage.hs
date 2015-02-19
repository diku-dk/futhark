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
  mconcat [usageInPat,
           usageInExpLore,
           usageInExp e,
           UT.usages (freeNamesInExp e)]
  where usageInPat =
          UT.usages (HS.fromList (patternNames pat) `HS.difference`
                     mconcat (map freeNamesIn $ patternElements pat))
          <> mconcat (map consumptionInPatElem $ patternElements pat)
        usageInExpLore =
          UT.usages $ freeNamesIn lore
        consumptionInPatElem (PatElem _ (BindInPlace _ src _) _) =
          UT.consumedUsage $ identName src
        consumptionInPatElem _ =
          mempty

usageInExp :: Aliased lore => Exp lore -> UT.UsageTable
usageInExp (Apply _ args _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases arg
          | (arg,d) <- args, d == Consume ]
usageInExp (LoopOp (DoLoop _ merge _ _ _)) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases se
          | (v,se) <- merge, unique $ fparamType v ]
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
