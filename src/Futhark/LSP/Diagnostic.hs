-- | Handling of diagnostics in the language server - things like
-- warnings and errors.
module Futhark.LSP.Diagnostic
  ( publishWarningDiagnostics,
    publishErrorDiagnostics,
    diagnosticSource,
    maxDiagnostic,
  )
where

import Colog.Core (logStringStderr, (<&))
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Compiler.Program (ProgError (..))
import Futhark.LSP.Tool (posToUri, rangeFromLoc)
import Futhark.Util.Loc (Loc (..))
import Futhark.Util.Pretty (Doc, docText)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens (HasVersion (version))
import Language.LSP.Protocol.Types
import Language.LSP.Server (LspT, getVersionedTextDoc, publishDiagnostics)

mkDiagnostic :: Range -> DiagnosticSeverity -> T.Text -> Diagnostic
mkDiagnostic range severity msg =
  Diagnostic
    range
    (Just severity)
    Nothing
    Nothing
    diagnosticSource
    msg
    Nothing
    Nothing
    Nothing

-- | Publish diagnostics from a Uri to Diagnostics mapping.
publish :: [(Uri, [Diagnostic])] -> LspT () IO ()
publish uri_diags_map = for_ uri_diags_map $ \(uri, diags) -> do
  doc <- getVersionedTextDoc $ TextDocumentIdentifier uri
  logStringStderr
    <& ("Publishing diagnostics for " ++ show uri ++ " Version: " ++ show (doc ^. version))
  publishDiagnostics
    maxDiagnostic
    (toNormalizedUri uri)
    (Just $ doc ^. version)
    (partitionBySource diags)

-- | Send warning diagnostics to the client.
publishWarningDiagnostics :: [(Loc, Doc a)] -> LspT () IO ()
publishWarningDiagnostics warnings = do
  publish $ M.assocs $ M.unionsWith (++) $ map onWarn warnings
  where
    onWarn (NoLoc, _) = mempty
    onWarn (loc@(Loc pos _), msg) =
      M.singleton
        (posToUri pos)
        [ mkDiagnostic
            (rangeFromLoc loc)
            DiagnosticSeverity_Warning
            (docText msg)
        ]

-- | Send error diagnostics to the client.
publishErrorDiagnostics :: NE.NonEmpty ProgError -> LspT () IO ()
publishErrorDiagnostics errors =
  publish $ M.assocs $ M.unionsWith (++) $ map onDiag $ NE.toList errors
  where
    onDiag (ProgError NoLoc _) = mempty
    onDiag (ProgError loc@(Loc pos _) msg) =
      M.singleton
        (posToUri pos)
        [ mkDiagnostic
            (rangeFromLoc loc)
            DiagnosticSeverity_Error
            (docText msg)
        ]
    onDiag (ProgWarning NoLoc _) = mempty
    onDiag (ProgWarning loc@(Loc pos _) msg) =
      M.singleton
        (posToUri pos)
        [ mkDiagnostic
            (rangeFromLoc loc)
            DiagnosticSeverity_Error
            (docText msg)
        ]

-- | The maximum number of diagnostics to report.
maxDiagnostic :: Int
maxDiagnostic = 100

-- | The source of the diagnostics.  (That is, the Futhark compiler,
-- but apparently the client must be told such things...)
diagnosticSource :: Maybe T.Text
diagnosticSource = Just "futhark"
