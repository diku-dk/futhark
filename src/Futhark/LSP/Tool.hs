{-# LANGUAGE OverloadedStrings #-}

-- | Generally useful definition used in various places in the
-- language server implementation.
module Futhark.LSP.Tool
  ( getHoverInfoFromState,
    findDefinitionRange,
    rangeFromSrcLoc,
    rangeFromLoc,
    posToUri,
  )
where

import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.State (State (..))
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos), SrcLoc, locOf)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Prop (isBuiltinLoc)
import Language.Futhark.Query
  ( AtPos (AtName),
    BoundTo (..),
    atPos,
    boundLoc,
  )
import Language.LSP.Types

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
      ms = HoverContents $ MarkupContent MkPlainText msg
  Just $ Hover ms (Just (rangeFromLoc loc))
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

queryAtPos :: State -> Pos -> Maybe AtPos
queryAtPos state pos = do
  loaded_prog <- stateProgram state
  atPos (lpImports loaded_prog) pos

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
