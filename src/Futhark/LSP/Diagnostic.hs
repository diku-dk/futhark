-- | Handling of diagnostics in the language server - things like
-- warnings and errors.
--
-- Every compilation produces the diagnostics for the entire program,
-- so publishing them replaces everything previously published.  We
-- do not use the incremental diagnostic store of the lsp library, as
-- its notion of replacement depends on document versions recorded
-- at earlier publications.
module Futhark.LSP.Diagnostic
  ( Diagnostics,
    warningDiagnostics,
    errorDiagnostics,
    showUnlocatedErrors,
    publishDiagnostics,
  )
where

import Colog.Core (Severity (Debug), (<&))
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Compiler.Program (ProgError (..))
import Futhark.LSP.Tool (logWithSeverity, posToUri, rangeFromLoc)
import Futhark.Util (showText)
import Futhark.Util.Loc (Loc (..))
import Futhark.Util.Pretty (Doc, docText)
import Language.LSP.Protocol.Message (SMethod (SMethod_TextDocumentPublishDiagnostics, SMethod_WindowShowMessage))
import Language.LSP.Protocol.Types
import Language.LSP.Server (LspT, sendNotification)
import Language.LSP.VFS (VFS, VirtualFileEntry (..), vfsMap, virtualFileVersion)

-- | The diagnostics of every file that has any.
type Diagnostics = M.Map NormalizedUri [Diagnostic]

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

-- | A diagnostic for the file containing the given location, if it
-- has one.
diagnosticAt :: Loc -> DiagnosticSeverity -> Doc a -> Diagnostics
diagnosticAt NoLoc _ _ = mempty
diagnosticAt loc@(Loc pos _) severity msg =
  M.singleton
    (toNormalizedUri $ posToUri pos)
    [mkDiagnostic (rangeFromLoc loc) severity (docText msg)]

-- | Diagnostics for the warnings of a successful compilation.
warningDiagnostics :: [(Loc, Doc a)] -> Diagnostics
warningDiagnostics = M.unionsWith (++) . map onWarn
  where
    onWarn (loc, msg) = diagnosticAt loc DiagnosticSeverity_Warning msg

-- | Diagnostics for the problems of a failed compilation.
errorDiagnostics :: NE.NonEmpty ProgError -> Diagnostics
errorDiagnostics = M.unionsWith (++) . map onDiag . NE.toList
  where
    onDiag (ProgError loc msg) = diagnosticAt loc DiagnosticSeverity_Error msg
    onDiag (ProgWarning loc msg) = diagnosticAt loc DiagnosticSeverity_Warning msg

-- | Errors without a location cannot be attached to a file, so they
-- are shown as messages.
showUnlocatedErrors :: NE.NonEmpty ProgError -> LspT () IO ()
showUnlocatedErrors = mapM_ showUnlocated
  where
    showUnlocated (ProgError NoLoc msg) = showMessage MessageType_Error msg
    showUnlocated (ProgWarning NoLoc msg) = showMessage MessageType_Warning msg
    showUnlocated _ = pure ()

    showMessage message_type msg =
      sendNotification SMethod_WindowShowMessage $
        ShowMessageParams message_type (docText msg)

-- | Publish the diagnostics of a compilation of the given VFS, for
-- every open file and every file in the given set of files that were
-- previously published for.  Returns the files that now have
-- diagnostics.  A client may discard a publication for a document it
-- has since changed, so open files are published for even when they
-- have no diagnostics.
publishDiagnostics :: VFS -> S.Set NormalizedUri -> Diagnostics -> LspT () IO (S.Set NormalizedUri)
publishDiagnostics vfs previous diagnostics = do
  let versions = M.mapMaybe openVersion $ vfs ^. vfsMap
      targets = previous <> M.keysSet versions
  for_ (M.toList $ diagnostics <> M.fromSet (const []) targets) $ \(uri, diags) -> do
    let version = M.lookup uri versions
    logWithSeverity Debug
      <& ("Publishing diagnostics for " <> showText uri <> " Version: " <> showText version)
    sendNotification SMethod_TextDocumentPublishDiagnostics $
      PublishDiagnosticsParams (fromNormalizedUri uri) version (take maxDiagnostic diags)
  pure $ M.keysSet diagnostics
  where
    openVersion (Open file) = Just $ virtualFileVersion file
    openVersion (Closed _) = Nothing

-- | The maximum number of diagnostics to report for a file.
maxDiagnostic :: Int
maxDiagnostic = 100

-- | The source of the diagnostics.  (That is, the Futhark compiler,
-- but apparently the client must be told such things...)
diagnosticSource :: Maybe T.Text
diagnosticSource = Just "futhark"
