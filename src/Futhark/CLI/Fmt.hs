-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Data.Bifunctor (second)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark
import Language.Futhark.Parser
  ( Comment (..),
    SyntaxError (..),
    parseFutharkWithComments,
  )
import System.Exit
import System.IO
import Text.Printf (printf)
import Control.Monad.State.Strict

-- The formatter implemented here is very crude and incomplete.  The
-- only syntactical construct it can currently handle is type
-- abbreviations without parameters, e.g:
--
--  type t = (bool, i32)
--
-- A *formatting function* is a function of type
--
--  [Comment] -> a -> ([Comment], Fmt)
--
-- where 'a' is some syntactical object, e.g. UncheckedTypeExp.  The
-- [Comment] list is a sorted list of "residual comments" that have
-- yet to be injected into the output.  A formatting function returns
-- a new list of comments (which may or may not be identical to the
-- input), as well as the formatted representation of 'a' as a 'Fmt'
-- value.
--
-- TODO: refactor this entire thing so that the maintenance of the
-- residual comments is encapsulated monadically.  Use a State monad
-- to keep the list.  This will make the code much cleaner.


-- TODO: ensure that doc comments (that start with "-- |" and are
-- connected to definitions) are always separated from other comments
-- with a single newline character.
--
-- TODO: prettyprint in a nicer way than one line per terminal.
--
-- TODO: support all syntactical constructs.

-- | Invariant: Should not contain newline characters.
type Line = T.Text

-- | The result of formatting is a list of lines.  This is useful as
-- we might want to modify every line of a prettyprinted
-- sub-expression, to e.g. prepend indentation.
type Fmt = [Line]

-- State monad to keep track of comments 
data Env = Env 
  { comments :: [Comment]
  , fmt      :: Fmt
  } deriving (Show, Eq, Ord)

type Smth = State Env   

-- Example function to test our new monad
doSomething :: Comment -> Smth ()
doSomething c = modify (\s -> s { comments = [c]})

-- Monad version
monadComment :: Located a => a -> Smth () 
monadComment a =
  modify (\s ->
    case s of
      Env (c : cs) fmts | locOf a > locOf c -> s { comments = cs, fmt = fmts <> [commentText c] }
      _ -> s
  )

comment :: Located a => [Comment] -> a -> ([Comment], Fmt)
comment (c : cs) a
  | locOf a > locOf c =
      (cs, [commentText c])
comment cs _ = (cs, [])
-- | Documentation comments are always optional, so this takes a 'Maybe'.
fmtDocComment :: [Comment] -> Maybe DocComment -> ([Comment], Fmt)
fmtDocComment cs (Just (DocComment x loc)) =
  let (cs', c) = comment cs loc
   in (cs', c <> prefix (T.lines x))
  where
    prefix [] = []
    prefix (l : ls) = ("-- | " <> l) : map ("-- " <>) ls
fmtDocComment cs Nothing = (cs, [])

--fmtMany :: (a -> Smth ()) -> [a] -> Smth ()
--fmtMany f as = foldM f mempty as

fmtMany :: ([Comment] -> a -> ([Comment], Fmt)) -> [Comment] -> [a] -> ([Comment], Fmt)
fmtMany f cs ds = second concat $ L.mapAccumL f cs ds

fmtTupleTypeElems :: [Comment] -> [UncheckedTypeExp] -> ([Comment], Fmt)
fmtTupleTypeElems cs [] = (cs, [])
fmtTupleTypeElems cs [t] = fmtTypeExp cs t
fmtTupleTypeElems cs (t : ts) =
  let (cs', t') = fmtTypeExp cs t
      (cs'', ts') = fmtTupleTypeElems cs' ts
   in (cs'', t' <> [","] <> ts')

-- does comment works for multi-line comments?
fmtTypeExp :: [Comment] -> UncheckedTypeExp -> ([Comment], Fmt)
fmtTypeExp cs (TEVar v loc) =
  let (cs', c) = comment cs loc
   in (cs', c <> [prettyText v])
fmtTypeExp cs (TETuple ts loc) =
  let (cs', c) = comment cs loc
      (cs'', ts') = fmtTupleTypeElems cs' ts
   in (cs'', c <> ["("] <> ts' <> [")"])

fmtTypeParam :: [Comment] -> UncheckedTypeParam -> ([Comment], Fmt)
fmtTypeParam = undefined

fmtTypeBind :: [Comment] -> UncheckedTypeBind -> ([Comment], Fmt)
fmtTypeBind cs (TypeBind name l ps e NoInfo dc _loc) =
  let (cs', dc') = fmtDocComment cs dc
      (cs'', ps') = fmtMany fmtTypeParam cs' ps
      (cs''', e') = fmtTypeExp cs'' e
   in (cs''', dc' <> ["type" <> prettyText l] <> [prettyText name] <> ps' <> e')

fmtDec :: [Comment] -> UncheckedDec -> ([Comment], Fmt)
fmtDec cs (TypeDec tb) = fmtTypeBind cs tb

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: [Comment] -> UncheckedProg -> Fmt
fmtProg cs (Prog dc decs) =
  let (cs', dc') = fmtDocComment cs dc
      (cs'', decs') = fmtMany fmtDec cs' decs
   in dc' <> decs' <> map commentText cs''

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      pres <- parseFutharkWithComments file <$> T.readFile file
      case pres of
        Left (SyntaxError loc err) -> do
          T.hPutStr stderr $ locText loc <> ":\n" <> prettyText err
          exitFailure
        Right (prog, cs) -> do
          let number i l = T.pack $ printf "%4d %s" (i :: Int) l
          T.hPutStr stdout $ T.unlines $ zipWith number [0 ..] $ fmtProg cs prog
    _ -> Nothing
