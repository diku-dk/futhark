{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Diagnostic
  ( sendDiagnostics,
    warningsToDiagnostics,
    errorToDiagnostics,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Futhark.Compiler.Program (ProgError (ProgError))
import Futhark.LSP.Tool (rangeFromLoc, rangeFromSrcLoc)
import Futhark.Util.Loc (SrcLoc)
import Futhark.Util.Pretty (Doc, pretty)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (LspT, publishDiagnostics)
import Language.LSP.Types
  ( Diagnostic (Diagnostic),
    DiagnosticSeverity (DsError, DsWarning),
    NormalizedUri,
    Range,
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

errorToDiagnostics :: NE.NonEmpty ProgError -> [Diagnostic]
errorToDiagnostics prog_error = map onError (NE.toList prog_error)
  where
    onError (ProgError loc msg) = mkDiagnostic (rangeFromLoc loc) DsError (T.pack $ pretty msg)
