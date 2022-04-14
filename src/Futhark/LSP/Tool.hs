{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Tool
  ( getHoverInfoFromState,
    findDefinitionRange,
    rangeFromSrcLoc,
    rangeFromLoc,
    locToUri,
  )
where

import qualified Data.Text as T
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.State (State (..))
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos), SrcLoc, locOf, srclocOf)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Core (locStr)
import Language.Futhark.Prop (isBuiltinLoc)
import Language.Futhark.Query
  ( AtPos (AtName),
    BoundTo (BoundTerm),
    atPos,
    boundLoc,
  )
import Language.LSP.Types
  ( Location (..),
    Position (..),
    Range (..),
    Uri,
    filePathToUri,
  )

getHoverInfoFromState :: State -> Maybe FilePath -> Int -> Int -> Maybe T.Text
getHoverInfoFromState state (Just path) l c = do
  AtName qn (Just def) _loc <- queryAtPos state $ Pos path l c 0
  case def of
    BoundTerm t defloc -> do
      Just $
        (prettyText qn <> " : " <> prettyText t)
          <> if isBuiltinLoc defloc
            then mempty
            else "\n\n**Definition: " <> T.pack (locStr (srclocOf defloc)) <> "**"
    bound
      | isBuiltinLoc (boundLoc bound) -> Just "Builtin definition."
      | otherwise -> Just $ "Definition: " <> T.pack (locStr (boundLoc bound))
getHoverInfoFromState _ _ _ _ = Nothing

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
queryAtPos state pos =
  case stateProgram state of
    Nothing -> Nothing
    Just loaded_prog -> atPos (lpImports loaded_prog) pos

locToUri :: Loc -> Uri
locToUri loc = do
  let (Loc (Pos file _ _ _) _) = loc
  filePathToUri file

-- Futhark's parser has a slightly different notion of locations than
-- LSP; so we tweak the positions here.
getStartPos :: Pos -> Position
getStartPos (Pos _ line col _) =
  Position (toEnum line - 1) (toEnum col - 1)

getEndPos :: Pos -> Position
getEndPos (Pos _ line col _) =
  Position (toEnum line - 1) (toEnum col)

rangeFromLoc :: Loc -> Range
rangeFromLoc (Loc start end) = Range (getStartPos start) (getEndPos end)
rangeFromLoc NoLoc = Range (Position 0 0) (Position 0 5) -- only when file not found, throw error after moving to vfs

rangeFromSrcLoc :: SrcLoc -> Range
rangeFromSrcLoc = rangeFromLoc . locOf
