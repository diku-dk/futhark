{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Tool
  ( getHoverInfoFromState,
    findDefinitionRange,
    rangeFromSrcLoc,
    rangeFromLoc,
  )
where

import qualified Data.Text as T
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.State (State (..))
import Futhark.Util.Loc (Loc (Loc, NoLoc), Pos (Pos), SrcLoc, locOf, srclocOf)
import Futhark.Util.Pretty (pretty)
import Language.Futhark.Core (locStr)
import Language.Futhark.Query
  ( AtPos (AtName),
    BoundTo (BoundModule, BoundModuleType, BoundTerm, BoundType),
    atPos,
    boundLoc,
  )
import Language.LSP.Types (Position (..), Range (..))

getHoverInfoFromState :: State -> Maybe FilePath -> Int -> Int -> Maybe T.Text
getHoverInfoFromState state (Just path) l c = do
  case queryAtPos state $ Pos path l c 0 of
    Nothing -> Nothing
    Just (AtName qn def _loc) -> do
      case def of
        Nothing -> Nothing
        Just (BoundTerm t defloc) -> do
          Just $ T.pack $ pretty qn ++ " : " ++ pretty t ++ "\n\n" ++ "**Definition: " ++ locStr (srclocOf defloc) ++ "**"
        Just (BoundType defloc) ->
          Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModule defloc) ->
          Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModuleType defloc) ->
          Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
getHoverInfoFromState _ _ _ _ = Nothing

findDefinitionRange :: State -> Maybe FilePath -> Int -> Int -> Maybe Range
findDefinitionRange state (Just path) l c = do
  -- some unnessecary operations inside `queryAtPos` for this functionality
  -- but shouldn't affect performance much since "Go to definition" is called less fequently
  case queryAtPos state $ Pos path l c 0 of
    Nothing -> Nothing
    Just (AtName _qn def _loc) -> do
      case def of
        Nothing -> Nothing
        Just def' -> Just $ rangeFromLoc (boundLoc def')
findDefinitionRange _ _ _ _ = Nothing

queryAtPos :: State -> Pos -> Maybe AtPos
queryAtPos state pos =
  case stateProgram state of
    Nothing -> Nothing
    Just loaded_prog -> atPos (lpImports loaded_prog) pos

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
