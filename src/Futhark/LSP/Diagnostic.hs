{-# LANGUAGE OverloadedStrings #-}

-- | Handling of diagnostics in the language server - things like
-- warnings and errors.
module Futhark.LSP.Diagnostic
  ( publishWarningDiagnostics,
    publishErrorDiagnostics,
    diagnosticSource,
    maxDiagnostic,
  )
where

import Control.Lens ((^.))
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.Map (assocs, empty, insertWith)
import qualified Data.Text as T
import Futhark.Compiler.Program (ProgError (ProgError))
import Futhark.LSP.Tool (posToUri, rangeFromLoc, rangeFromSrcLoc)
import Futhark.Util (debug)
import Futhark.Util.Loc (Loc (..), SrcLoc, locOf)
import Futhark.Util.Pretty (Doc, prettyText)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (LspT, getVersionedTextDoc, publishDiagnostics)
import Language.LSP.Types
  ( Diagnostic (Diagnostic),
    DiagnosticSeverity (DsError, DsWarning),
    Range,
    TextDocumentIdentifier (TextDocumentIdentifier),
    Uri,
    toNormalizedUri,
  )
import Language.LSP.Types.Lens (HasVersion (version))

mkDiagnostic :: Range -> DiagnosticSeverity -> T.Text -> Diagnostic
mkDiagnostic range severity msg = Diagnostic range (Just severity) Nothing diagnosticSource msg Nothing Nothing

-- | Publish diagnostics from a Uri to Diagnostics mapping.
publish :: [(Uri, [Diagnostic])] -> LspT () IO ()
publish uri_diags_map = for_ uri_diags_map $ \(uri, diags) -> do
  doc <- getVersionedTextDoc $ TextDocumentIdentifier uri
  debug $ "Publishing diagnostics for " ++ show uri ++ " Verion: " ++ show (doc ^. version)
  publishDiagnostics maxDiagnostic (toNormalizedUri uri) (doc ^. version) (partitionBySource diags)

-- | Send warning diagnostics to the client.
publishWarningDiagnostics :: [(SrcLoc, Doc)] -> LspT () IO ()
publishWarningDiagnostics warnings = do
  let diags_map =
        foldr
          ( \(srcloc, msg) acc ->
              let diag = mkDiagnostic (rangeFromSrcLoc srcloc) DsWarning (prettyText msg)
               in case locOf srcloc of
                    NoLoc -> acc
                    Loc pos _ -> insertWith (++) (posToUri pos) [diag] acc
          )
          empty
          warnings
  publish $ assocs diags_map

-- | Send error diagnostics to the client.
publishErrorDiagnostics :: NE.NonEmpty ProgError -> LspT () IO ()
publishErrorDiagnostics errors = do
  let diags_map =
        foldr
          ( \(ProgError loc msg) acc ->
              let diag = mkDiagnostic (rangeFromLoc loc) DsError (prettyText msg)
               in case loc of
                    NoLoc -> acc
                    Loc pos _ -> insertWith (++) (posToUri pos) [diag] acc
          )
          empty
          errors
  publish $ assocs diags_map

-- | The maximum number of diagnostics to report.
maxDiagnostic :: Int
maxDiagnostic = 100

-- | The source of the diagnostics.  (That is, the Futhark compiler,
-- but apparently the client must be told such things...)
diagnosticSource :: Maybe T.Text
diagnosticSource = Just "futhark"
