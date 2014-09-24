module Futhark.Analysis.Usage
       ( usageInBinding
       , usageInExp
       )
       where

import Data.Maybe
import Data.Monoid
import qualified Data.HashSet as HS

import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Binder (Proper)

usageInBinding :: (Proper lore, Aliased lore) => Binding lore -> UT.UsageTable
usageInBinding (Let pat _ e) =
  usageInPat pat <> usageInPatLore pat <> usageInExp e <> UT.usages (freeNamesInExp e)
  where usageInPat =
          UT.usages . HS.fromList . mapMaybe subExpUsage .
          concatMap (arrayDims . identType) . patternIdents
        usageInPatLore = UT.usages . mconcat . map (freeNamesIn . bindeeLore) . patternBindees
        subExpUsage (Var v)       = Just $ identName v
        subExpUsage (Constant {}) = Nothing

usageInExp :: Aliased lore => Exp lore -> UT.UsageTable
usageInExp (PrimOp (Assert (Var v) _)) =
  UT.predicateUsage $ identName v
usageInExp (PrimOp (Update _ src _ _ _)) =
  UT.consumedUsage $ identName src
usageInExp (Apply _ args _ _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases arg
          | (arg,d) <- args, d == Consume ]
usageInExp (LoopOp (DoLoop _ merge _ _ _ _)) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases se
          | (v,se) <- merge, unique $ identType v ]
usageInExp (LoopOp (Map _ f args _)) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases se
          | (v,se) <- zip (lambdaParams f) args,
            unique $ identType v ]
usageInExp (LoopOp (Reduce _ f args _)) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map subExpAliases $ acc ++ arr,
            unique $ identType v ]
  where (acc, arr) = unzip args
usageInExp (LoopOp (Scan _ f args _)) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map subExpAliases $ acc ++ arr,
            unique $ identType v ]
  where (acc, arr) = unzip args
usageInExp (LoopOp (Redomap _ _ f acc arr _)) =
  mconcat [ mconcat $ map UT.consumedUsage $ HS.toList als
          | (v,als) <- zip (lambdaParams f) $
                       map subExpAliases $ acc ++ arr,
            unique $ identType v ]
usageInExp _ = UT.empty
