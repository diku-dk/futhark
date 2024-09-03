-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

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
import Control.Monad
import Debug.Trace

debug :: Show a => a -> a
debug a = traceShow a a

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
-- DONE: refactor this entire thing so that the maintenance of the
-- residual comments is encapsulated monadically.  Use a State monad
-- to keep the list.  This will make the code much cleaner.

-- TODO: Name monad encapsulating comments something better than "Smth"

-- TODO: ensure that doc comments (that start with "-- |" and are
-- connected to definitions) are always separated from other comments
-- with a single newline character. Question: Exactly what is meant by this?
-- Question: 
      -- Test

      -- | This is a type

      -- this is a type
-- type test = (i32, i32)


-- TODO: Fix formatting of several lines of comments 
-- TODO: prettyprint in a nicer way than one line per terminal.
--
-- TODO: support all syntactical constructs.

-- TODO (Question?): Change fmt to be a sequence of lines instead of a list of lines 

-- | Invariant: Should not contain newline characters.
type Line = T.Text

-- | The result of formatting is a list of lines.  This is useful as
-- we might want to modify every line of a prettyprinted
-- sub-expression, to e.g. prepend indentation.
type Fmt = [Line]

-- State monad to keep track of comments 
newtype Env = Env
  { comments :: [Comment]
  } deriving (Show, Eq, Ord)

type Smth a = State Env a 

-- Monad version
comment :: Located a => a -> Smth Fmt
comment a = do
  s <- get
  case s of
    Env (c : cs) | locOf a > locOf c -> do
      put $ s { comments = cs }
      pure [commentText c]
    _ -> pure []

-- | Documentation comments are always optional, so this takes a 'Maybe'.
fmtDocComment :: Maybe DocComment -> Smth Fmt
fmtDocComment (Just (DocComment x loc)) =
  (<> prefix (T.lines x)) <$> comment loc
  where
    prefix [] = []
    prefix (l : ls) = ("-- | " <> l) : map ("-- " <>) ls
fmtDocComment Nothing = pure []

fmtMany :: (a -> Smth Fmt) -> [a] -> Smth Fmt
fmtMany f = foldM (\a b -> (a<>) <$> f b) []

fmtTupleTypeElems :: [UncheckedTypeExp] -> Smth Fmt
fmtTupleTypeElems [] = pure []
fmtTupleTypeElems [t] = fmtTypeExp t
fmtTupleTypeElems (t : ts) = do
  t' <- fmtTypeExp t 
  ts' <- fmtTupleTypeElems ts
  pure $ t' <> [","] <> ts' 

fmtTypeExp :: UncheckedTypeExp -> Smth Fmt
fmtTypeExp (TEVar v loc) = do
  c <- comment loc
  pure $ c <> [prettyText v]
fmtTypeExp (TETuple ts loc) = do
  c <- comment loc
  ts' <- fmtTupleTypeElems ts
  pure $ c <> ["("] <> ts' <> [")"]

fmtTypeParam :: UncheckedTypeParam -> Smth Fmt
fmtTypeParam = undefined

fmtTypeBind :: UncheckedTypeBind -> Smth Fmt
fmtTypeBind (TypeBind name l ps e NoInfo dc _loc) = do
  dc' <- fmtDocComment dc
  ps' <- fmtMany fmtTypeParam ps
  e' <- fmtTypeExp e
  pure $ dc' <> ["type" <> prettyText l] <> [prettyText name] <> ps' <> [" = "] <> e'

fmtDec :: UncheckedDec -> Smth Fmt
fmtDec (TypeDec tb) = fmtTypeBind tb

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: UncheckedProg -> Smth Fmt
fmtProg (Prog dc decs) = do
  dc' <- fmtDocComment dc
  decs' <- fmtMany fmtDec decs
  cs <- gets comments
  put (Env [])
  pure $ dc' <> decs' <> map commentText cs 

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
          let fmt = evalState (fmtProg prog) (Env { comments = cs })
          T.hPutStr stdout $ T.unlines $ zipWith number [0 ..] fmt
    _ -> Nothing
