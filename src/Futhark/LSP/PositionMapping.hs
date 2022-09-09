-- | Provide mapping between position in stale content and current.
module Futhark.LSP.PositionMapping
  ( mappingFromDiff,
    PositionMapping,
    toStalePos,
    toCurrentLoc,
    StaleFile (..),
  )
where

import Data.Algorithm.Diff (Diff, PolyDiff (Both, First, Second), getDiff)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Text qualified as T
import Data.Vector qualified as V
import Futhark.Util.Loc (Loc (Loc), Pos (Pos))
import Language.LSP.VFS (VirtualFile)

-- | A mapping between current file content and the stale (last successful compiled) file content.
-- Currently, only supports entire line mapping,
-- more detailed mapping might be achieved via referring to haskell-language-server@efb4b94
data PositionMapping = PositionMapping
  { -- | The mapping from stale position to current.
    -- e.g. staleToCurrent[2] = 4 means "line 2" in the stale file, corresponds to "line 4" in the current file.
    staleToCurrent :: V.Vector Int,
    -- | The mapping from current position to stale.
    currentToStale :: V.Vector Int
  }
  deriving (Show)

-- | Stale pretty document stored in state.
data StaleFile = StaleFile
  { -- | The last successfully compiled file content.
    -- Using VirtualFile for convenience, we can use anything with {version, content}
    staleContent :: VirtualFile,
    -- | PositionMapping between current and stale file content.
    -- Nothing if last type-check is successful.
    staleMapping :: Maybe PositionMapping
  }
  deriving (Show)

-- | Compute PositionMapping using the diff between two texts.
mappingFromDiff :: [T.Text] -> [T.Text] -> PositionMapping
mappingFromDiff stale current = do
  let (stale_to_current, current_to_stale) = rawMapping (getDiff stale current) 0 0
  PositionMapping (V.fromList stale_to_current) (V.fromList current_to_stale)
  where
    rawMapping :: [Diff T.Text] -> Int -> Int -> ([Int], [Int])
    rawMapping [] _ _ = ([], [])
    rawMapping (Both _ _ : xs) lold lnew = bimap (lnew :) (lold :) $ rawMapping xs (lold + 1) (lnew + 1)
    rawMapping (First _ : xs) lold lnew = first (-1 :) $ rawMapping xs (lold + 1) lnew
    rawMapping (Second _ : xs) lold lnew = second (-1 :) $ rawMapping xs lold (lnew + 1)

-- | Transform current Pos to the stale pos for query
-- Note: line and col in Pos is larger by one
toStalePos :: Maybe PositionMapping -> Pos -> Maybe Pos
toStalePos (Just (PositionMapping _ current_to_stale)) pos =
  if l > Prelude.length current_to_stale
    then Nothing
    else Just $ Pos file (V.unsafeIndex current_to_stale (l - 1) + 1) c o
  where
    Pos file l c o = pos
toStalePos Nothing pos = Just pos

-- some refactoring might be needed, same logic as toStalePos
toCurrentPos :: Maybe PositionMapping -> Pos -> Maybe Pos
toCurrentPos (Just (PositionMapping stale_to_current _)) pos =
  if l > Prelude.length stale_to_current
    then Nothing
    else Just $ Pos file (V.unsafeIndex stale_to_current (l - 1) + 1) c o
  where
    Pos file l c o = pos
toCurrentPos Nothing pos = Just pos

-- | Transform stale Loc gotten from stale AST to current Loc.
toCurrentLoc :: Maybe PositionMapping -> Loc -> Maybe Loc
toCurrentLoc mapping loc = do
  let Loc start end = loc
  current_start <- toCurrentPos mapping start
  current_end <- toCurrentPos mapping end
  Just $ Loc current_start current_end
