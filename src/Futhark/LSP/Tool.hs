{-# LANGUAGE OverloadedStrings #-}

-- | Generally useful definition used in various places in the
-- language server implementation.
module Futhark.LSP.Tool
  ( getHoverInfoFromState,
    findDefinitionRange,
    rangeFromSrcLoc,
    rangeFromLoc,
    posToUri,
    getMapping,
  )
where

import qualified Data.Text as T
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.PositionMapping
  ( PositionMapping,
    mappingFromDiff,
    toCurrentLoc,
    toStalePos,
  )
import Futhark.LSP.State (State (..), getStaleContent)
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos), SrcLoc, locOf)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Prop (isBuiltinLoc)
import Language.Futhark.Query
  ( AtPos (AtName),
    BoundTo (..),
    atPos,
    boundLoc,
  )
import Language.LSP.Server (LspM, getVirtualFile)
import Language.LSP.Types
import Language.LSP.VFS (VirtualFile, virtualFileText)

-- | Retrieve hover info for the definition referenced at the given
-- file at the given line and column number (the two 'Int's).
getHoverInfoFromState :: State -> Maybe FilePath -> Maybe PositionMapping -> Int -> Int -> Maybe Hover
getHoverInfoFromState state (Just path) mapping l c = do
  AtName _ (Just def) loc <- queryAtPos state mapping $ Pos path l c 0
  let msg =
        case def of
          BoundTerm t _ -> prettyText t
          BoundModule {} -> "module"
          BoundModuleType {} -> "module type"
          BoundType {} -> "type"
      ms = HoverContents $ MarkupContent MkPlainText msg
  Just $ Hover ms (Just (rangeFromLoc loc))
getHoverInfoFromState _ _ _ _ _ = Nothing

-- | Find the location of the definition referenced at the given file
-- at the given line and column number (the two 'Int's).
findDefinitionRange :: State -> Maybe FilePath -> Maybe PositionMapping -> Int -> Int -> Maybe Location
findDefinitionRange state (Just path) mapping l c = do
  -- some unnessecary operations inside `queryAtPos` for this function
  -- but shouldn't affect performance much since "Go to definition" is called less frequently
  AtName _qn (Just bound) _loc <- queryAtPos state mapping $ Pos path l c 0
  let loc = boundLoc bound
      Loc (Pos file_path _ _ _) _ = loc
  if isBuiltinLoc loc
    then Nothing
    else Just $ Location (filePathToUri file_path) (rangeFromLoc loc)
findDefinitionRange _ _ _ _ _ = Nothing

queryAtPos :: State -> Maybe PositionMapping -> Pos -> Maybe AtPos
queryAtPos state mapping pos = do
  loaded_prog <- stateProgram state
  stale_pos <- toStalePos mapping pos
  query_result <- atPos (lpImports loaded_prog) stale_pos
  updateAtPos query_result
  where
    updateAtPos :: AtPos -> Maybe AtPos
    updateAtPos (AtName qn (Just def) loc) = do
      current_loc <- toCurrentLoc mapping loc
      -- TODO: update boundloc to current_loc
      -- note, boundloc could be in another file, don't change in that case
      Just $ AtName qn (Just def) current_loc
    updateAtPos _ = Nothing

-- | Entry point for create PositionMapping.
-- Nothing if diff is not needed.
getMapping :: State -> Uri -> LspM () (Maybe PositionMapping)
getMapping state uri = do
  virtual_file <- getVirtualFile $ toNormalizedUri uri
  pure $ computeMapping (getStaleContent state $ uriToFilePath uri) virtual_file
  where
    computeMapping :: Maybe T.Text -> Maybe VirtualFile -> Maybe PositionMapping
    computeMapping (Just stale_content) (Just virtual_file) =
      Just $ mappingFromDiff (T.lines stale_content) (T.lines $ virtualFileText virtual_file)
    computeMapping _ _ = Nothing

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

-- | Create an LSP 'Range' from a Futhark 'SrcLoc'.
rangeFromSrcLoc :: SrcLoc -> Range
rangeFromSrcLoc = rangeFromLoc . locOf
