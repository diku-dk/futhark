{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Tool (getHoverInfoFromState) where

import qualified Data.Text as T
import Futhark.Compiler (lpImports)
import Futhark.LSP.State (State (..))
import Futhark.Util.Loc (srclocOf)
import Futhark.Util.Pretty (pretty)
import Language.Futhark.Query
  ( AtPos (AtName),
    BoundTo (BoundModule, BoundModuleType, BoundTerm, BoundType),
    Pos (Pos),
    atPos,
  )
import Language.Futhark.Syntax (locStr)

getHoverInfoFromState :: State -> Maybe FilePath -> Int -> Int -> IO (Maybe T.Text)
getHoverInfoFromState state (Just path) l c = do
  case stateProgram state of
    Nothing -> pure Nothing
    Just loaded_prog -> do
      let imports = lpImports loaded_prog
      case atPos imports $ Pos path l c 0 of
        Nothing -> pure Nothing
        Just (AtName qn def _loc) -> do
          case def of
            Nothing -> pure Nothing
            Just (BoundTerm t defloc) -> do
              pure $ Just $ T.pack $ pretty qn ++ " :: " ++ pretty t ++ "\n\n" ++ "**Definition: " ++ locStr (srclocOf defloc) ++ "**"
            Just (BoundType defloc) ->
              pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
            Just (BoundModule defloc) ->
              pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
            Just (BoundModuleType defloc) ->
              pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
getHoverInfoFromState _ _ _ _ = pure Nothing
