{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Diagnostic
  ( publishWarningDiagnostics,
    publishErrorDiagnostics,
  )
where

import Control.Lens ((^.))
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.Map (assocs, empty, insertWith)
import qualified Data.Text as T
import Futhark.Compiler.Program (ProgError (ProgError))
import Futhark.Util (debug)
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos), SrcLoc, locOf)
import Futhark.Util.Pretty (Doc, pretty)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (LspT, getVersionedTextDoc, publishDiagnostics)
import Language.LSP.Types
  ( Diagnostic (Diagnostic),
    DiagnosticSeverity (DsError, DsWarning),
    Position (Position),
    Range (Range),
    TextDocumentIdentifier (TextDocumentIdentifier),
    Uri,
    filePathToUri,
    toNormalizedUri,
  )
import Language.LSP.Types.Lens (HasVersion (version))

mkDiagnostic :: Range -> DiagnosticSeverity -> T.Text -> Diagnostic
mkDiagnostic range severity msg = Diagnostic range (Just severity) Nothing (Just "futhark") msg Nothing Nothing

-- | Publish diagnostics from a Uri to Diagnostics mapping.
publish :: [(Uri, [Diagnostic])] -> LspT () IO ()
publish uri_diags_map = for_ uri_diags_map $ \(uri, diags) -> do
  doc <- getVersionedTextDoc $ TextDocumentIdentifier uri
  debug $ "Publishing diagnostics for " ++ show uri ++ " Verion: " ++ show (doc ^. version)
  publishDiagnostics 100 (toNormalizedUri uri) (doc ^. version) (partitionBySource diags)

publishWarningDiagnostics :: [(SrcLoc, Doc)] -> LspT () IO ()
publishWarningDiagnostics warnings = do
  let diags_map =
        foldr
          ( \(srcloc, msg) acc -> do
              let (Loc (Pos file _ _ _) _) = locOf srcloc
                  uri = filePathToUri file
                  diag = mkDiagnostic (rangeFromSrcLoc srcloc) DsWarning (T.pack $ pretty msg)
              insertWith (++) uri [diag] acc
          )
          empty
          warnings
  publish $ assocs diags_map

publishErrorDiagnostics :: NE.NonEmpty ProgError -> LspT () IO ()
publishErrorDiagnostics errors = do
  let diags_map =
        foldr
          ( \(ProgError loc msg) acc -> do
              let (Loc (Pos file _ _ _) _) = loc
                  uri = filePathToUri file
                  diag = mkDiagnostic (rangeFromLoc loc) DsError (T.pack $ pretty msg)
              insertWith (++) uri [diag] acc
          )
          empty
          errors
  publish $ assocs diags_map

-- the ending appears to be one col too short
rangeFromSrcLoc :: SrcLoc -> Range
rangeFromSrcLoc srcloc = do
  let Loc start end = locOf srcloc
  Range (getPosition start) (getPosition end)

rangeFromLoc :: Loc -> Range
rangeFromLoc (Loc start end) = Range (getPosition start) (getPosition end)
rangeFromLoc NoLoc = Range (Position 0 0) (Position 0 5) -- only when file not found, throw error after moving to vfs

getPosition :: Pos -> Position
getPosition pos = do
  let Pos _ line col _ = pos
  Position (toEnum line - 1) (toEnum col - 1)
