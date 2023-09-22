{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Analysis.Pretty
  ( prettyTuple,
    prettyTupleLines,
    prettyString,
  ) where

import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.Util.Pretty

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      f ((_, name), maps) =
        "(segmap)" <+> pretty name <+> colon <+> lbrace </> indent 4 (mapprintArray $ M.toList maps) </> rbrace

      mapprintArray :: [(ArrayName, M.Map IndexExprName [MemoryEntry])] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap (name, maps) = "(arr)" <+> pretty name <+> colon <+> lbrace </> indent 4 (mapprintIdxExpr (M.toList maps)) </> rbrace

      mapprintIdxExpr :: [(IndexExprName, [MemoryEntry])] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty name <+> ":" </> indent 4 (printMemoryEntryList mems)

      printMemoryEntryList :: [MemoryEntry] -> Doc ann
      printMemoryEntryList [] = ""
      printMemoryEntryList [m] = printMemoryEntry m 0
      printMemoryEntryList (m : mm) = printMemoryEntry m 0 </> printMemoryEntryList mm

      printMemoryEntry :: MemoryEntry -> Int -> Doc ann
      printMemoryEntry [] _ = ""
      printMemoryEntry [m] idx = printDim m idx
      printMemoryEntry (m : mm) idx = printDim m idx </> printMemoryEntry mm (idx + 1)

      printDim m idx = pretty idx <+> ":" <+> indent 0 (pretty m)


instance Pretty DimIdxPat where
  pretty (DimIdxPat dependencies) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
      "dependencies" <+> equals <+> align (prettyDeps dependencies)
    where
      prettyDeps = encloseSep "[ " " ]" " | " . map (printPair . snd) . S.toList
      printPair (name, lvl, itertype) = pretty name <+> pretty lvl <+> pretty itertype

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"
