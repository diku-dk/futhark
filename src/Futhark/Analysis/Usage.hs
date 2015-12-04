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
           UT.usages (freeInExp e)]
  where usageInPat =
          UT.usages (mconcat (map freeIn $ patternElements pat)
                     `HS.difference`
                     HS.fromList (patternNames pat))
          <> mconcat (map consumptionInPatElem $ patternElements pat)
        usageInExpLore =
          UT.usages $ freeIn lore
        consumptionInPatElem (PatElem _ (BindInPlace _ src _) _) =
          UT.consumedUsage src
        consumptionInPatElem _ =
          mempty

usageInExp :: Aliased lore => Exp lore -> UT.UsageTable
usageInExp (Apply _ args _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases arg
          | (arg,d) <- args, d == Consume ]
usageInExp (LoopOp (DoLoop _ merge _ _)) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases se
          | (v,se) <- merge, unique $ paramDeclType v ]
usageInExp (LoopOp (Map _ _ f _)) =
  usageInLambda f
usageInExp (LoopOp (Reduce _ _ f _)) =
  usageInLambda f
usageInExp (LoopOp (Scan _ _ f _)) =
  usageInLambda f
usageInExp (LoopOp (Redomap _ _ _ f _ _)) =
  usageInLambda f
usageInExp _ = UT.empty

usageInLambda :: Aliased lore => Lambda lore -> UT.UsageTable
usageInLambda =
  mconcat .
  map UT.consumedUsage .
  HS.toList . consumedInBody . lambdaBody
