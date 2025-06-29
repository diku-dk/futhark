-- | Generally useful definition used in various places in the
-- language server implementation.
module Futhark.LSP.Tool
  ( getHoverInfoFromState,
    findDefinitionRange,
    rangeFromLoc,
    posToUri,
    computeMapping,
  )
where

import Data.Text qualified as T
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.PositionMapping
  ( PositionMapping,
    mappingFromDiff,
    toCurrentLoc,
    toStalePos,
  )
import Futhark.LSP.State (State (..), getStaleContent, getStaleMapping)
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos))
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Core (isBuiltinLoc)
import Language.Futhark.Query
  ( AtPos (AtName),
    BoundTo (..),
    atPos,
    boundLoc,
  )
import Language.LSP.Protocol.Types
import Language.LSP.Server (LspM, getVirtualFile)
import Language.LSP.VFS (VirtualFile, virtualFileText, virtualFileVersion)

-- | Retrieve hover info for the definition referenced at the given
-- file at the given line and column number (the two 'Int's).
getHoverInfoFromState :: State -> Maybe FilePath -> Int -> Int -> Maybe Hover
getHoverInfoFromState state (Just path) l c = do
  AtName _ (Just def) loc <- queryAtPos state $ Pos path l c 0
  let msg =
        case def of
          BoundTerm t _ -> prettyText t
          BoundModule {} -> "module"
          BoundModuleType {} -> "module type"
          BoundType {} -> "type"
      ms = MarkupContent MarkupKind_PlainText msg
  Just $ Hover (InL ms) (Just (rangeFromLoc loc))
getHoverInfoFromState _ _ _ _ = Nothing

-- | Find the location of the definition referenced at the given file
-- at the given line and column number (the two 'Int's).
findDefinitionRange :: State -> Maybe FilePath -> Int -> Int -> Maybe Location
findDefinitionRange state (Just path) l c = do
  -- some unnessecary operations inside `queryAtPos` for this function
  -- but shouldn't affect performance much since "Go to definition" is called less frequently
  AtName _qn (Just bound) _loc <- queryAtPos state $ Pos path l c 0
  let loc = boundLoc bound
      Loc (Pos file_path _ _ _) _ = loc
  if isBuiltinLoc loc
    then Nothing
    else Just $ Location (filePathToUri file_path) (rangeFromLoc loc)
findDefinitionRange _ _ _ _ = Nothing

-- | Query the AST for information at certain Pos.
queryAtPos :: State -> Pos -> Maybe AtPos
queryAtPos state pos = do
  let Pos path _ _ _ = pos
      mapping = getStaleMapping state path
  loaded_prog <- stateProgram state
  stale_pos <- toStalePos mapping pos
  query_result <- atPos (lpImports loaded_prog) stale_pos
  updateAtPos mapping query_result
  where
    -- Update the 'AtPos' with the current mapping.
    updateAtPos :: Maybe PositionMapping -> AtPos -> Maybe AtPos
    updateAtPos mapping (AtName qn (Just def) loc) = do
      let def_loc = boundLoc def
          Loc (Pos def_file _ _ _) _ = def_loc
          Pos current_file _ _ _ = pos
      current_loc <- toCurrentLoc mapping loc
      if def_file == current_file
        then do
          current_def_loc <- toCurrentLoc mapping def_loc
          Just $ AtName qn (Just (updateBoundLoc def current_def_loc)) current_loc
        else do
          -- Defined in another file, get the corresponding PositionMapping.
          let def_mapping = getStaleMapping state def_file
          current_def_loc <- toCurrentLoc def_mapping def_loc
          Just $ AtName qn (Just (updateBoundLoc def current_def_loc)) current_loc
    updateAtPos _ _ = Nothing

    updateBoundLoc :: BoundTo -> Loc -> BoundTo
    updateBoundLoc (BoundTerm t _loc) current_loc = BoundTerm t current_loc
    updateBoundLoc (BoundModule _loc) current_loc = BoundModule current_loc
    updateBoundLoc (BoundModuleType _loc) current_loc = BoundModuleType current_loc
    updateBoundLoc (BoundType _loc) current_loc = BoundType current_loc

-- | Entry point for computing PositionMapping.
computeMapping :: State -> Maybe FilePath -> LspM () (Maybe PositionMapping)
computeMapping state (Just file_path) = do
  virtual_file <- getVirtualFile $ toNormalizedUri $ filePathToUri file_path
  pure $ getMapping (getStaleContent state file_path) virtual_file
  where
    getMapping :: Maybe VirtualFile -> Maybe VirtualFile -> Maybe PositionMapping
    getMapping (Just stale_file) (Just current_file) =
      if virtualFileVersion stale_file == virtualFileVersion current_file
        then Nothing -- Happens when other files (e.g. dependencies) fail to type-check.
        else Just $ mappingFromDiff (T.lines $ virtualFileText stale_file) (T.lines $ virtualFileText current_file)
    getMapping _ _ = Nothing
computeMapping _ _ = pure Nothing

-- | Convert a Futhark 'Pos' to an LSP 'Uri'.
posToUri :: Pos -> Uri
posToUri (Pos file _ _ _) = filePathToUri file

-- Futhark's parser has a slightly different notion of locations than
-- LSP; so we tweak the positions here.
getStartPos :: Pos -> Position
getStartPos (Pos _ l c _) =
  Position (toEnum l - 1) (toEnum c - 1)

getEndPos :: Pos -> Position
getEndPos (Pos _ l c _) =
  Position (toEnum l - 1) (toEnum c)

-- | Create an LSP 'Range' from a Futhark 'Loc'.
rangeFromLoc :: Loc -> Range
rangeFromLoc (Loc start end) = Range (getStartPos start) (getEndPos end)
rangeFromLoc NoLoc = Range (Position 0 0) (Position 0 5) -- only when file not found, throw error after moving to vfs
