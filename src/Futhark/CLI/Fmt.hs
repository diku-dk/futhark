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
--   a -> FmtM Fmt
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

-- Some utility functions 
commasep :: Fmt -> FmtM Fmt
commasep [] = pure []
commasep [x] = pure [x]
commasep (x:xs) = do 
  xs' <- commasep xs
  pure $ (x <> ", ") : xs'

brackets :: Fmt -> FmtM Fmt
brackets fmt = pure $ ["["] <> fmt <> ["]"]
{-
commasepbetween :: Line -> Line -> Fmt -> FmtM Fmt
commasepbetween o c xs = do
  pure $ (zipWidth (<>) (o : repeat ",") xs) <> [c]
-}
-- | Invariant: Should not contain newline characters.
type Line = T.Text


-- | The result of formatting is a list of lines.  This is useful as
-- we might want to modify every line of a prettyprinted
-- sub-expression, to e.g. prepend indentation.
type Fmt = [Line]

-- State monad to keep track of comments 
newtype FmtState = FmtState
  { comments :: [Comment]
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

trailingComment :: Located a => a -> FmtM Line
trailingComment a = do
  s <- get
  case comments s of
    c : cs | isSameLine a c -> do
      put $ s { comments = cs }
      pure $ commentText c
    _ -> pure ""

-- parses comments infront of a and converts a to fmt using formatting function f 
buildFmt :: Located a => (a -> FmtM Fmt) -> a -> FmtM Fmt  
buildFmt f a = do 
  c <- comment a
  a' <-  f a
  c' <- trailingComment a
  pure $ c <> mapLast (<> " " <> c') a' 

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f as = i ++ [f l]
  where
    l = last as
    i = init as


-- Other Utility Functions

-- Should probably be updated to return a special single/multi-line indicator type
isSingleLine :: Located a => a -> Bool
isSingleLine a =
  case locOf a of
    Loc start end -> posLine start == posLine end
    NoLoc -> undefined -- should throw an error

isSameLine :: (Located a, Located b) => a -> b -> Bool
isSameLine a b = 
  case (locOf a, locOf b) of
    (Loc _startA endA, Loc _startB endB) -> posLine endA == posLine endB
    _ -> undefined -- should throw an error

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

fmtRecordTypeFields :: [(Name, UncheckedTypeExp)] -> FmtM Fmt
fmtRecordTypeFields [] = pure []
fmtRecordTypeFields ((name, te):fs) = do
  f' <- fmtTypeExp te
  fs' <- concat <$> mapM fmtField fs
  pure $  [prettyText name <> ": "] <> f' <> fs'  
  where fmtField :: (Name, UncheckedTypeExp) -> FmtM Fmt 
        fmtField (n, t) = do 
          t' <- fmtTypeExp t
          pure $ mapLast (", " <>) t'

-- | Formatting of Futhark type expressions.
fmtTypeExp :: UncheckedTypeExp -> FmtM Fmt
fmtTypeExp (TEVar v loc) = buildFmt (const (pure [prettyText v])) loc
  --pure [prettyText v]
fmtTypeExp (TETuple ts loc) | isSingleLine loc = 
  buildFmt fmtFun loc
  where fmtFun :: a -> FmtM Fmt
        fmtFun _ = do
          ts' <- fmtTupleTypeElems ts True
          pure ["(" <> T.concat ts' <> ")"]
fmtTypeExp (TETuple ts loc) = 
  buildFmt fmtFun loc
  where fmtFun :: a -> FmtM Fmt
        fmtFun _ = do
          ts' <- fmtTupleTypeElems ts False
          pure $ ["("] <>  ts' <> [")"]
fmtTypeExp (TEParens te loc) = buildFmt fmtFun loc
  where fmtFun :: a -> FmtM Fmt
        fmtFun _ = do 
          te' <- fmtTypeExp te
          pure $ ["("] <> te' <> [")"] 
fmtTypeExp (TERecord fs loc) = buildFmt fmtFun loc -- Records
  where fmtFun :: a -> FmtM Fmt
        fmtFun _ = do
          ts <- fmtRecordTypeFields fs
          pure $ ["{"] <> ts <> ["}"]
fmtTypeExp (TEArray se te loc) = buildFmt fmtFun loc -- A array with an size expression 
  where fmtFun :: a -> FmtM Fmt 
        fmtFun _ = do
          se' <- fmtSizeExp se
          te' <- fmtTypeExp te
          pure $  se' <> te'
-- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
fmtTypeExp (TEUnique te loc) = buildFmt fmtFun loc 
  where fmtFun :: a -> FmtM Fmt
        fmtFun _ = do 
          te' <- fmtTypeExp te 
          pure $ ["*"] <> te' 
-- I am not sure I guess applying a higher kinded type to some type expression
-- NOT DONE missing arg formatting 
fmtTypeExp (TEApply te tArgE loc) = buildFmt fmtFun loc 
  where fmtFun :: a -> FmtM Fmt 
        fmtFun _ = do 
          te' <- fmtTypeExp te
          pure $ te'-- undefined 
fmtTypeExp (TEArrow (Just name) type_exp type_exp' loc) = undefined -- is this "->"?
fmtTypeExp (TEArrow Nothing type_exp type_exp' loc) = undefined -- is this "->"?
fmtTypeExp (TESum type_exps loc) = undefined -- This should be "|"
fmtTypeExp (TEDim names exp loc) = undefined -- This this probably [n][m]expression for array dimensions

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

fmtTypeParam :: UncheckedTypeParam -> FmtM Fmt
fmtTypeParam (TypeParamDim name loc) = pure ["[" <> prettyText name <> "]"]
fmtTypeParam (TypeParamType l name loc) =
  pure $ ["'" <> fmtLiftedness l] <> [prettyText name]
  where
    fmtLiftedness Unlifted = ""
    fmtLiftedness SizeLifted = "~"
    fmtLiftedness Lifted = "^"

fmtPat :: UncheckedPat t -> FmtM Fmt
fmtPat (TuplePat pats loc) = do
  fmt <- L.intercalate [", "] <$> mapM fmtPat pats
  pure $ ["("] <> fmt <> [")"]
fmtPat (RecordPat pats loc) = do
  fmts <- L.intercalate [", "] <$> mapM fmtField pats
  pure $ ["{"] <> fmts <> ["}"]
  where
    fmtField (name, t) = do
      t' <- fmtPat t
      pure $ [prettyText name] <> [" = "] <> t'
fmtPat (PatParens pat loc) = do
  fmt <- fmtPat pat
  pure $ ["("] <> fmt <> [")"]
fmtPat (Id name _ loc) = pure [prettyText name]
fmtPat (Wildcard t loc) = pure ["_"]
fmtPat (PatAscription pat t loc) = do
  pat' <- fmtPat pat
  t' <- fmtTypeExp t
  pure $ pat' <> [":"] <> t'
fmtPat (PatLit e _ loc) = pure [prettyText e]
fmtPat (PatConstr n _ pats loc) = do
  pats' <- concat <$> mapM fmtPat pats
  pure $ ["#"] <> [prettyText n] <> pats' 
fmtPat (PatAttr attr pat loc) = do
  attr' <- fmtAttr attr
  pat' <- fmtPat pat
  pure $ attr' <> pat'

fmtValBind :: UncheckedValBind -> FmtM Fmt
fmtValBind (ValBind entry name retdecl _rettype tparams args body doc attrs loc) = do
  fmt_attrs <- concat <$> mapM fmtAttr attrs
  tparams' <- concat <$> mapM fmtTypeParam tparams
  args' <- concat <$> mapM fmtPat args
  retdecl' <-
    case fmtTypeExp <$> retdecl of
    Just a -> fmap ([":"] <>) a
    Nothing -> pure []
  -- exp <- fmtExp body
  pure $ fmt_attrs <>
         fun <>
         [prettyText name] <>
         tparams' <>
         args' <>
         retdecl' <>
         ["="] -- <> exp
  where
    fun =
      case entry of
        Just _ -> ["entry"]
        _ -> ["def"]

fmtSizeExp :: SizeExp d -> FmtM Fmt
fmtSizeExp (SizeExp d loc) = undefined -- cannot fiugre out how to format dimension d
fmtSizeExp (SizeExpAny _loc) = brackets mempty

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
