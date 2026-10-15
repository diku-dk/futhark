-- | Handling of diagnostics in the language server - things like
-- warnings and errors.
module Futhark.LSP.Diagnostic
  ( publishWarningDiagnostics,
    publishErrorDiagnostics,
    diagnosticSource,
    maxDiagnostic,
  )
where

import Colog.Core (Severity (Debug), (<&))
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Compiler.Program (ProgError (..))
import Futhark.LSP.Tool (logWithSeverity, posToUri, rangeFromLoc)
import Futhark.Util (showText)
import Futhark.Util.Loc (Loc (..))
import Futhark.Util.Pretty (Doc, docText)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens (HasVersion (version))
import Language.LSP.Protocol.Message (SMethod (SMethod_WindowShowMessage))
import Language.LSP.Protocol.Types
import Language.LSP.Server (LspT, getVersionedTextDoc, publishDiagnostics, sendNotification)

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
  logWithSeverity Debug
    <& ("Publishing diagnostics for " <> showText uri <> " Version: " <> showText (doc ^. version))
  publishDiagnostics
    maxDiagnostic
    (toNormalizedUri uri)
    (Just $ doc ^. version)
    (partitionBySource diags)

-- | A diagnostic for the file containing the given location, if it
-- has one.
diagnosticAt :: Loc -> DiagnosticSeverity -> Doc a -> M.Map Uri [Diagnostic]
diagnosticAt NoLoc _ _ = mempty
diagnosticAt loc@(Loc pos _) severity msg =
  M.singleton (posToUri pos) [mkDiagnostic (rangeFromLoc loc) severity (docText msg)]

-- | Send warning diagnostics to the client.
publishWarningDiagnostics :: [(Loc, Doc a)] -> LspT () IO ()
publishWarningDiagnostics warnings =
  publish $ M.assocs $ M.unionsWith (++) $ map onWarn warnings
  where
    onWarn (loc, msg) = diagnosticAt loc DiagnosticSeverity_Warning msg

-- | Send error diagnostics to the client.  Errors without a location
-- cannot be attached to a file, so they are shown as messages.
publishErrorDiagnostics :: NE.NonEmpty ProgError -> LspT () IO ()
publishErrorDiagnostics errors = do
  publish $ M.assocs $ M.unionsWith (++) $ map onDiag $ NE.toList errors
  for_ errors showUnlocated
  where
    onDiag (ProgError loc msg) = diagnosticAt loc DiagnosticSeverity_Error msg
    onDiag (ProgWarning loc msg) = diagnosticAt loc DiagnosticSeverity_Warning msg

    showUnlocated (ProgError NoLoc msg) = showMessage MessageType_Error msg
    showUnlocated (ProgWarning NoLoc msg) = showMessage MessageType_Warning msg
    showUnlocated _ = pure ()

    showMessage message_type msg =
      sendNotification SMethod_WindowShowMessage $
        ShowMessageParams message_type (docText msg)

-- | The maximum number of diagnostics to report.
maxDiagnostic :: Int
maxDiagnostic = 100

-- | The source of the diagnostics.  (That is, the Futhark compiler,
-- but apparently the client must be told such things...)
diagnosticSource :: Maybe T.Text
diagnosticSource = Just "futhark"
