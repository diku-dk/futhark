module Futhark.CLI.Fmt.Format (
  Line,
  Fmt,
  FormatState,
  comment,
  isSingleLine,
  fmtDocComment,
  fmtMany,
  emptyComments,
  runFormatState,
  fmtIndent
  ) where

import Data.Text qualified as T
import Futhark.Util.Loc
import Language.Futhark
import Language.Futhark.Parser ( Comment (..) )
import Control.Monad.State.Strict
import Control.Monad

-- | Invariant: Should not contain newline characters.
type Line = T.Text

-- | The result of formatting is a list of lines.  This is useful as
-- we might want to modify every line of a prettyprinted
-- sub-expression, to e.g. prepend indentation.
type Fmt = [Line]

-- | State to keep track of comments and indentation.
data Env = Env
  { comments :: ![Comment]
  , indent :: !Int
  } deriving (Show, Eq, Ord)

-- | State monad for our environment
type FormatState a = State Env a 

-- | Retrieves every comment from the state.
emptyComments :: FormatState [Comment]
emptyComments = do
  cs <- gets comments
  modify (\s -> s { comments = [] })
  pure cs

runFormatState :: FormatState Fmt -> [Comment] -> Fmt
runFormatState f cs = evalState f (Env { indent = 0, comments = cs })

-- | Functions for operating on FormatState monad
comment :: Located a => a -> FormatState Fmt
comment a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s { comments = cs }
      pure [commentText c]
    _ -> pure []

fmtIndent :: FormatState a -> FormatState a
fmtIndent st = do
  modify (\s -> s {indent = indent s + 2})
  a <- st
  modify (\s -> s {indent = indent s - 2})
  pure a

-- | Other Utility Functions
isSingleLine :: Located a => a -> Bool
isSingleLine a =
  case locOf a of
    Loc start end -> posLine start == posLine end
    NoLoc -> undefined -- Huh????? spørg troels.

-- | Documentation comments are always optional, so this takes a 'Maybe'.
fmtDocComment :: Maybe DocComment -> FormatState Fmt
fmtDocComment (Just (DocComment x loc)) =
  (<> prefix (T.lines x)) <$> comment loc
  where
    prefix [] = []
    prefix (l : ls) = ("-- | " <> l) : map ("-- " <>) ls
fmtDocComment Nothing = pure []

fmtMany :: (a -> FormatState Fmt) -> [a] -> FormatState Fmt 
fmtMany f = foldM (\a b -> (a<>) <$> f b) []
