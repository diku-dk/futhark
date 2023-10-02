{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Analysis.Pretty
  ( prettyTuple,
    prettyTupleLines,
    prettyString,
  )
where

import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.Util.Pretty

instance Pretty (IndexTable rep) where
  pretty = stack . map f . M.toList :: IndexTable rep -> Doc ann
    where
      f (segop, arrayNameToIdxExprMap) = pretty segop <+> colon <+> g arrayNameToIdxExprMap

      g maps = lbrace </> indent 4 (mapprintArray $ M.toList maps) </> rbrace

      mapprintArray :: [(ArrayName, M.Map IndexExprName (MemoryEntry rep))] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap (name, maps) =
        "(arr)"
          <+> pretty name
          <+> colon
          <+> lbrace
          </> indent 4 (mapprintIdxExpr (M.toList maps))
          </> rbrace

      mapprintIdxExpr :: [(IndexExprName, MemoryEntry rep)] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty name <+> ":" </> indent 4 (printMemoryEntry mems)

      printMemoryEntry :: MemoryEntry rep -> Doc ann
      printMemoryEntry (MemoryEntry dims _) =
        stack $ zipWith printDim [0 .. (length dims)] dims

      printDim idx m = pretty idx <+> ":" <+> indent 0 (pretty m)

instance Pretty (DimIdxPat rep) where
  pretty (DimIdxPat dependencies) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "dependencies" <+> equals <+> align (prettyDeps dependencies)
    where
      prettyDeps = braces . commasep . map (printPair . snd) . S.toList
      printPair (name, lvl, itertype) = pretty name <+> pretty lvl <+> pretty itertype

instance Pretty SegOpName where
  pretty (SegmentedMap (_, name)) = "(segmap)" <+> pretty name
  pretty (SegmentedRed (_, name)) = "(segred)" <+> pretty name
  pretty (SegmentedScan (_, name)) = "(segscan)" <+> pretty name
  pretty (SegmentedHist (_, name)) = "(seghist)" <+> pretty name

instance Pretty BodyType where
  pretty (SegOpName (SegmentedMap (_, name))) = pretty name <+> colon <+> "segmap"
  pretty (SegOpName (SegmentedRed (_, name))) = pretty name <+> colon <+> "segred"
  pretty (SegOpName (SegmentedScan (_, name))) = pretty name <+> colon <+> "segscan"
  pretty (SegOpName (SegmentedHist (_, name))) = pretty name <+> colon <+> "seghist"
  pretty (LoopBodyName (_, name)) = pretty name <+> colon <+> "loop"
  pretty (CondBodyName (_, name)) = pretty name <+> colon <+> "cond"

instance Pretty (IterationType rep) where
  pretty Sequential = "seq"
  pretty Parallel = "par"
