{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Diagnostic
  ( sendDiagnostics,
    warningsToDiagnostics,
    errorToDiagnostics,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Futhark.Compiler.Program (ProgramError (ProgramError))
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos), SrcLoc, locOf)
import Futhark.Util.Pretty (Doc, pretty)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (LspT, publishDiagnostics)
import Language.LSP.Types
  ( Diagnostic (Diagnostic),
    DiagnosticSeverity (DsError, DsWarning),
    NormalizedUri,
    Position (Position),
    Range (Range),
    TextDocumentVersion,
  )

sendDiagnostics :: NormalizedUri -> [Diagnostic] -> TextDocumentVersion -> LspT () IO ()
sendDiagnostics uri diags version = publishDiagnostics 100 uri version (partitionBySource diags)

mkDiagnostic :: Range -> DiagnosticSeverity -> T.Text -> Diagnostic
mkDiagnostic range severity msg = Diagnostic range (Just severity) Nothing (Just "futhark") msg Nothing Nothing

warningsToDiagnostics :: [(SrcLoc, Doc)] -> [Diagnostic]
warningsToDiagnostics =
  map
    ( \(srcloc, msg) ->
        mkDiagnostic (rangeFromSrcLoc srcloc) DsWarning (T.pack $ pretty msg)
    )

errorToDiagnostics :: NE.NonEmpty ProgramError -> [Diagnostic]
errorToDiagnostics prog_error = map onError (NE.toList prog_error)
  where
    onError (ProgramError loc msg) = mkDiagnostic (rangeFromLoc loc) DsError (T.pack $ pretty msg)

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
