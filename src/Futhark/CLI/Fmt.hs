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

-- DONE: Name monad encapsulating comments something better than "Smth"

-- TODO: ensure that doc comments (that start with "-- |" and are
-- connected to definitions) are always separated from other comments
-- with a single newline character. Question: Exactly what is meant by this?
-- Question: 
      -- Test

      -- | This is a type

      -- this is a type
-- type test = (i32, i32) (Not that interesting left for now)


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
data FmtState = FmtState
  { comments :: ![Comment]
  } deriving (Show, Eq, Ord)

type FmtM a = State FmtState a 

-- Functions for operating on FmtM monad
comment :: Located a => a -> FmtM Fmt
comment a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s { comments = cs }
      cs' <- comment a --fmts remaining comments 
      pure $ commentText c : cs'
    _ -> pure []

-- parses comments infront of a and converts a to fmt using f 
buildFmt :: Located a => (a -> FmtM Fmt) -> a -> FmtM Fmt  
buildFmt f a = do 
  c <- comment a
  a' <-  f a
  pure $ c <> a' 

-- Other Utility Functions

-- Should probably be updated to return a special single/multi-line indicator type
isSingleLine :: Located a => a -> Bool
isSingleLine a =
  case locOf a of
    Loc start end -> posLine start == posLine end
    NoLoc -> undefined -- should throw an error


-- Main format code

-- | Documentation comments are always optional, so this takes a 'Maybe'.
fmtDocComment :: Maybe DocComment -> FmtM Fmt
fmtDocComment (Just (DocComment x loc)) =
  (<> prefix (T.lines x)) <$> comment loc
  where
    prefix [] = []
    prefix (l : ls) = ("-- | " <> l) : map ("-- " <>) ls
fmtDocComment Nothing = pure []

fmtMany :: (a -> FmtM Fmt) -> [a] -> FmtM Fmt 
fmtMany f = foldM (\a b -> (a<>) <$> f b) []

-- Boolean indicates whether tuple is single line
fmtTupleTypeElems :: [UncheckedTypeExp] -> Bool -> FmtM Fmt
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
fmtTypeExp :: UncheckedTypeExp -> FmtM Fmt
fmtTypeExp (TEVar v loc) = buildFmt (const (pure [prettyText v])) loc
  --pure [prettyText v]
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

fmtTypeParam :: UncheckedTypeParam -> FmtM Fmt
fmtTypeParam (TypeParamDim name loc) = undefined 
fmtTypeParam (TypeParamType l name loc) = undefined

fmtTypeBind :: UncheckedTypeBind -> FmtM Fmt
fmtTypeBind (TypeBind name l ps e NoInfo dc loc) = do
  dc' <- fmtDocComment dc
  ps' <- fmtMany fmtTypeParam ps
  if isSingleLine loc then do
    e' <- fmtTypeExp e
    pure $ dc' <> ["type " <>
                   prettyText l <>
                   prettyText name <>
                   T.intercalate " " ps' <>
                   " = " <> T.concat e'] 
  else do
    e' <- fmtTypeExp e
    pure $ dc' <> ["type " <>
                  prettyText l <>
                  prettyText name <>
                  T.intercalate " " ps' <>
                  " = "] <> e'

fmtAttrAtom :: AttrAtom a -> FmtM Fmt
fmtAttrAtom (AtomName name) = pure [prettyText name]
fmtAttrAtom (AtomInt int) = pure [prettyText int]

fmtAttrInfo :: AttrInfo a -> FmtM Fmt
fmtAttrInfo (AttrAtom attr loc) = fmtAttrAtom attr
fmtAttrInfo (AttrComp name attrs loc) = do
  x <- mapM fmtAttrInfo attrs
  pure $ [prettyText name] <> ["("] <> L.intercalate [","] x <> [")"]

-- I've added smth to make the code parse
fmtAttr :: AttrInfo a -> FmtM Fmt
fmtAttr attr = do
  attr_info <- fmtAttrInfo attr
  pure $ ["#["] <> attr_info <> ["]"]

fmtValBind :: UncheckedValBind -> FmtM Fmt
fmtValBind (ValBind entry name retdecl rettype tparams args body doc attrs loc) = do
  fmt_attrs <- concat <$> mapM fmtAttr attrs
  pure $ fmt_attrs <> fun
  where
    fun =
      case entry of
        Just _ -> ["entry"]
        _ -> ["def"]

-- | Formatting of Futhark declarations.
fmtDec :: UncheckedDec -> FmtM Fmt
fmtDec (ValDec t) = fmtValBind t -- A value declaration.
fmtDec (TypeDec tb) = buildFmt fmtTypeBind tb -- A type declaration.
fmtDec (ModTypeDec tb) = undefined -- A module type declation.
fmtDec (ModDec tb) = undefined -- A module declation.
fmtDec (OpenDec tb loc) = undefined -- I have no clue.
fmtDec (LocalDec tb loc) = undefined -- I have no clue, maybe this just adds the local keyword?
fmtDec (ImportDec path tb loc) = undefined -- Import declarations.

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: UncheckedProg -> FmtM Fmt
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
          let fmt = evalState (fmtProg prog) (FmtState { comments = cs })
          T.hPutStr stdout $ T.unlines $ zipWith number [0 ..] fmt
    _ -> Nothing
