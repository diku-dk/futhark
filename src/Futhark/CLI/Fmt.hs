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
data Env = Env
  { comments :: ![Comment]
  , indent :: !Int
  } deriving (Show, Eq, Ord)

type Smth a = State Env a 

-- Functions for operating on Smth monad
comment :: Located a => a -> Smth Fmt
comment a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s { comments = cs }
      pure [commentText c]
    _ -> pure []

incrIndent :: Smth () 
incrIndent = modify (\s -> s {indent = indent s + 2})

decrIndent :: Smth ()
decrIndent = modify (\s -> s {indent = indent s - 2})

-- Other Utility Functions
isSingleLine :: Located a => a -> Bool
isSingleLine a =
  case locOf a of
    Loc start end -> posLine start == posLine end
    NoLoc -> undefined -- Huh????? spørg troels.


-- Main format code

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

-- Boolean indicates whether tuple is single line
fmtTupleTypeElems :: [UncheckedTypeExp] -> Bool -> Smth Fmt
fmtTupleTypeElems [] _ = pure []
fmtTupleTypeElems [t] _ = fmtTypeExp t
fmtTupleTypeElems (t : ts) isSingle = do
  t' <- fmtTypeExp t
  ts' <- fmtTupleTypeElems ts isSingle
  if isSingle then pure [T.concat $ t' <> [", "] <> ts'] 
  else pure $ t' <> prependComma ts' -- TO DO: Make the comma not be on its own line  
  where prependComma :: Fmt -> Fmt
        prependComma [] = [] -- comma still on seperate linethat's probably pretty slow, better way to do this?
        prependComma fmt = [T.concat $ [", "] <> fmt] 

-- | Formatting of Futhark type expressions.
fmtTypeExp :: UncheckedTypeExp -> Smth Fmt
fmtTypeExp (TEVar v loc) = do
  c <- comment loc
  pure $ c <> [prettyText v]
fmtTypeExp (TETuple ts loc) | isSingleLine loc = do
  c <- comment loc
  ts' <- fmtTupleTypeElems ts True
  pure $ c <> ["(" <> T.concat ts' <> ")"]
fmtTypeExp (TETuple ts loc) = do
  c <- comment loc
  ts' <- fmtTupleTypeElems ts False
  pure $ c <> ["("] <>  ts' <> [")"]
fmtTypeExp (TEParens type_exp loc) = undefined
fmtTypeExp (TERecord ts loc) = undefined -- Records
fmtTypeExp (TEArray size_exp type_exp loc) = undefined -- A array with an size expression
fmtTypeExp (TEUnique type_exp loc) = undefined -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
fmtTypeExp (TEApply type_exp type_arg_exp loc) = undefined -- I am not sure I guess applying a higher kinded type to some type expression
fmtTypeExp (TEArrow (Just name) type_exp type_exp' loc) = undefined -- is this "->"?
fmtTypeExp (TEArrow Nothing type_exp type_exp' loc) = undefined -- is this "->"?
fmtTypeExp (TESum type_exps loc) = undefined -- This should be "|"
fmtTypeExp (TEDim names exp loc) = undefined -- This this probably [n][m]expression for array dimensions

fmtTypeParam :: UncheckedTypeParam -> Smth Fmt
fmtTypeParam (TypeParamDim name loc) = undefined 
fmtTypeParam (TypeParamType l name loc) = undefined

fmtTypeBind :: UncheckedTypeBind -> Smth Fmt
fmtTypeBind (TypeBind name l ps e NoInfo dc loc) = do
  dc' <- fmtDocComment dc
  ps' <- fmtMany fmtTypeParam ps
  if isSingleLine loc then do
    e' <- fmtTypeExp e
    pure $ dc' <> ["type " <>
                   prettyText l <>
                   prettyText name <>
                   T.intercalate " " ps' <>
                   " = " <>
                   T.concat e']
  else do
    -- incrIndent
    e' <- fmtTypeExp e
    -- decrIndent
    pure $ dc' <> ["type " <>
                  prettyText l <>
                  prettyText name <>
                  T.intercalate " " ps' <>
                  " = "] <> e'

-- | Formatting of Futhark declarations.
fmtDec :: UncheckedDec -> Smth Fmt
fmtDec (ValDec t) = undefined -- A value declaration.
fmtDec (TypeDec tb) = fmtTypeBind tb -- A type declaration.
fmtDec (ModTypeDec tb) = undefined -- A module type declation.
fmtDec (ModDec tb) = undefined -- A module declation.
fmtDec (OpenDec tb loc) = undefined -- I have no clue.
fmtDec (LocalDec tb loc) = undefined -- I have no clue, maybe this just adds the local keyword?
fmtDec (ImportDec path tb loc) = undefined -- Import declarations.

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: UncheckedProg -> Smth Fmt
fmtProg (Prog dc decs) = do
  dc' <- fmtDocComment dc
  decs' <- fmtMany fmtDec decs
  cs <- gets comments
  modify (\s -> s {comments = []})
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
          let fmt = evalState (fmtProg prog) (Env { indent = 0, comments = cs })
          T.hPutStr stdout $ T.unlines $ zipWith number [0 ..] fmt
    _ -> Nothing
