-- | @futhark fmt@
module Futhark.CLI.Fmt (main, fmtText) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (chr)
import Data.Foldable
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

-- import Debug.Trace
{-
debug :: Show a => a -> a
debug a = traceShow a a
-}

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

-- \| This is a type

-- this is a type
-- type test = (i32, i32) (Not that interesting left for now)

-- TODO: Fix formatting of several lines of comments
-- TODO: prettyprint in a nicer way than one line per terminal.
--
-- DONE: support all syntactical constructs.

-- TODO (Question?): Change fmt to be a sequence of lines instead of a list of lines
{-
-- Some utility functions
commasep :: Fmt -> FmtM Fmt
commasep [] = pure []
commasep [x] = pure [x]
commasep (x:xs) = do
  xs' <- commasep xs
  pure $ (x <> ", ") : xs'
-}

brackets :: Fmt -> Fmt
brackets fmt = ["["] <> fmt <> ["]"]

braces :: Fmt -> Fmt
braces fmt = ["{"] <> fmt <> ["}"]

parens :: Fmt -> Fmt
parens fmt = ["("] <> fmt <> [")"]

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f (x : xs) = f x : xs

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
  }
  deriving (Show, Eq, Ord)

type FmtM a = State FmtState a

-- Functions for operating on FmtM monad
comment :: (Located a) => a -> FmtM Fmt
comment a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s {comments = cs}
      cs' <- comment a -- fmts remaining comments
      pure $ commentText c : cs'
    _ -> pure []

trailingComment :: (Located a) => a -> FmtM Line
trailingComment a = do
  s <- get
  case comments s of
    c : cs | isSameLine a c -> do
      put $ s {comments = cs}
      pure $ commentText c
    _ -> pure ""

-- parses comments infront of a and converts a to fmt using formatting function f
buildFmt :: (Located a) => (a -> FmtM Fmt) -> a -> FmtM Fmt
buildFmt f a = do
  c <- comment a
  a' <- f a
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
isSingleLine :: (Located a) => a -> Bool
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
fmtMany f = foldM (\a b -> (a <>) <$> f b) []

-- Boolean indicates whether tuple is single line
fmtTupleTypeElems :: [UncheckedTypeExp] -> Bool -> FmtM Fmt
fmtTupleTypeElems [] _ = pure []
fmtTupleTypeElems [t] _ = fmtTypeExp t
fmtTupleTypeElems (t : ts) isSingle = do
  t' <- fmtTypeExp t
  ts' <- fmtTupleTypeElems ts isSingle
  if isSingle
    then pure [T.concat $ t' <> [", "] <> ts']
    else pure $ t' <> prependComma ts' -- TO DO: Make the comma not be on its own line
  where
    prependComma :: Fmt -> Fmt
    prependComma [] = [] -- comma still on seperate linethat's probably pretty slow, better way to do this?
    prependComma fmt = [T.concat $ [", "] <> fmt]

fmtRecordTypeFields :: [(Name, UncheckedTypeExp)] -> FmtM Fmt
fmtRecordTypeFields [] = pure []
fmtRecordTypeFields ((name, te) : fs) = do
  f' <- fmtTypeExp te
  fs' <- concat <$> mapM fmtFieldType fs
  pure $ [prettyText name <> ": "] <> f' <> fs'
  where
    fmtFieldType :: (Name, UncheckedTypeExp) -> FmtM Fmt
    fmtFieldType (name', t) = do
      t' <- fmtTypeExp t
      pure $ ["," <> prettyText name' <> ": "] <> t'

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> FmtM Fmt
fmtSumTypeConstr (name, fs) = do
  fs' <- mapM fmtTypeExp fs
  pure $ ["#" <> prettyText name] <> mconcat fs'

-- | Formatting of Futhark type expressions.
fmtTypeExp :: UncheckedTypeExp -> FmtM Fmt
fmtTypeExp (TEVar v loc) = buildFmt (const (pure [prettyText v])) loc
-- pure [prettyText v]
fmtTypeExp (TETuple ts loc)
  | isSingleLine loc =
      buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      ts' <- fmtTupleTypeElems ts True
      pure ["(" <> T.concat ts' <> ")"]
fmtTypeExp (TETuple ts loc) =
  buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      ts' <- fmtTupleTypeElems ts False
      pure $ ["("] <> ts' <> [")"]
fmtTypeExp (TEParens te loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      te' <- fmtTypeExp te
      pure $ ["("] <> te' <> [")"]
fmtTypeExp (TERecord fs loc) = buildFmt fmtFun loc -- Records
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      ts <- fmtRecordTypeFields fs
      pure $ ["{"] <> ts <> ["}"]
fmtTypeExp (TEArray se te loc) = buildFmt fmtFun loc -- A array with an size expression
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      se' <- fmtSizeExp se
      te' <- fmtTypeExp te
      pure $ se' <> te'
-- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
fmtTypeExp (TEUnique te loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      te' <- fmtTypeExp te
      pure $ ["*"] <> te'
-- I am not sure I guess applying a higher kinded type to some type expression
fmtTypeExp (TEApply te tArgE loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      te' <- fmtTypeExp te
      tArgE' <- fmtArgExp tArgE
      pure $ te' <> [" "] <> tArgE'
-- this is "->"
fmtTypeExp (TEArrow (Just name) te0 te1 loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      te0' <- fmtTypeExp te0
      te1' <- fmtTypeExp te1
      pure $ parens (prettyText name : " : " : te0') <> [" -> "] <> te1'
-- this is"->"
fmtTypeExp (TEArrow Nothing te0 te1 loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      te0' <- fmtTypeExp te0
      te1' <- fmtTypeExp te1
      pure $ te0' <> [" -> "] <> te1'
-- This should be "|"
fmtTypeExp (TESum tes loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      tes' <- mapM fmtSumTypeConstr tes
      pure $ L.intercalate ["|"] tes'
fmtTypeExp (TEDim dims te loc) = buildFmt fmtFun loc
  where
    fmtFun :: a -> FmtM Fmt
    fmtFun _ = do
      te' <- fmtTypeExp te
      let dims' = mconcat (map (brackets . \t -> [prettyText t]) dims)
      pure $ ["?"] <> dims' <> ["."] <> te'

fmtArgExp :: TypeArgExp UncheckedExp Name -> FmtM Fmt
fmtArgExp (TypeArgExpSize se) = fmtSizeExp se
fmtArgExp (TypeArgExpType te) = fmtTypeExp te

fmtTypeBind :: UncheckedTypeBind -> FmtM Fmt
fmtTypeBind (TypeBind name l ps e NoInfo dc loc) = do
  l' <- fmtLiftedness l
  dc' <- fmtDocComment dc
  ps' <- fmtMany fmtTypeParam ps
  if isSingleLine loc
    then do
      e' <- fmtTypeExp e
      pure $
        dc'
          <> [ "type"
                 <> l'
                 <> " "
                 <> prettyText name
                 <> (if null ps then "" else " ")
                 <> T.intercalate " " ps'
                 <> " = "
                 <> T.unlines e'
             ]
    else do
      e' <- fmtTypeExp e
      pure $
        dc'
          <> [ "type"
                 <> l'
                 <> " "
                 <> prettyText name
                 <> T.intercalate " " ps'
                 <> " = "
             ]
          <> e'

fmtAttrAtom :: AttrAtom a -> FmtM Fmt
fmtAttrAtom (AtomName name) = pure [prettyText name]
fmtAttrAtom (AtomInt int) = pure [prettyText int]

fmtAttrInfo :: AttrInfo a -> FmtM Fmt
fmtAttrInfo (AttrAtom attr _loc) = fmtAttrAtom attr
fmtAttrInfo (AttrComp name attrs _loc) = do
  x <- mapM fmtAttrInfo attrs
  pure $ [prettyText name] <> ["("] <> L.intercalate [","] x <> [")"]

-- I've added smth to make the code parse
fmtAttr :: AttrInfo a -> FmtM Fmt
fmtAttr attr = do
  attr_info <- fmtAttrInfo attr
  pure $ ["#["] <> attr_info <> ["]"]

fmtLiftedness :: Liftedness -> FmtM Line
fmtLiftedness Unlifted = pure ""
fmtLiftedness SizeLifted = pure "~"
fmtLiftedness Lifted = pure "^"

fmtTypeParam :: UncheckedTypeParam -> FmtM Fmt
fmtTypeParam (TypeParamDim name _loc) = pure ["[" <> prettyText name <> "]"]
fmtTypeParam (TypeParamType l name _loc) = do
  l' <- fmtLiftedness l
  pure ["'" <> l' <> prettyText name]

fmtName :: Name -> Line
fmtName name
  | operatorName name = "(" <> prettyText name <> ")"
  | otherwise = prettyText name

fmtPat :: UncheckedPat t -> FmtM Fmt
fmtPat (TuplePat pats _loc) = do
  fmt <- L.intercalate [", "] <$> mapM fmtPat pats
  pure $ ["("] <> fmt <> [")"]
fmtPat (RecordPat pats _loc) = do
  fmts <- L.intercalate [", "] <$> mapM fmtFieldPat pats
  pure $ ["{"] <> fmts <> ["}"]
  where
    fmtFieldPat (name, t) = do
      t' <- fmtPat t
      pure $ [prettyText name] <> [" = "] <> t' -- Currently it allways adds the fields it seems.
fmtPat (PatParens pat _loc) = do
  fmt <- fmtPat pat
  pure $ ["("] <> fmt <> [")"]
fmtPat (Id name _ _loc) = pure [fmtName name] -- Need to add parenthesis around ++ in tests/uniqueness/uniqueness-error37.fut
fmtPat (Wildcard _t _loc) = pure ["_"]
fmtPat (PatAscription pat t _loc) = do
  pat' <- fmtPat pat
  t' <- fmtTypeExp t
  pure $ pat' <> [":"] <> t'
fmtPat (PatLit e _ _loc) = pure [prettyText e]
fmtPat (PatConstr n _ pats _loc) = do
  pats' <- mconcat <$> mapM fmtPat pats
  pure $ ["#" <> prettyText n] <> pats'
fmtPat (PatAttr attr pat _loc) = do
  attr' <- fmtAttr attr
  pat' <- fmtPat pat
  pure $ attr' <> pat'

fmtField :: FieldBase NoInfo Name -> FmtM Fmt
fmtField (RecordFieldExplicit name e _loc) = do
  e' <- fmtExp e
  pure $ [prettyText name] <> ["="] <> e'
fmtField (RecordFieldImplicit name _ _loc) = pure [prettyText name]

fmtPrimValue :: PrimValue -> FmtM Fmt
fmtPrimValue (UnsignedValue (Int8Value v)) =
  pure [prettyText (show (fromIntegral v :: Word8)) <> "u8"]
fmtPrimValue (UnsignedValue (Int16Value v)) =
  pure [prettyText (show (fromIntegral v :: Word16)) <> "u16"]
fmtPrimValue (UnsignedValue (Int32Value v)) =
  pure [prettyText (show (fromIntegral v :: Word32)) <> "u32"]
fmtPrimValue (UnsignedValue (Int64Value v)) =
  pure [prettyText (show (fromIntegral v :: Word64)) <> "u64"]
fmtPrimValue (SignedValue v) = pure [prettyText v]
fmtPrimValue (BoolValue True) = pure ["true"]
fmtPrimValue (BoolValue False) = pure ["false"]
fmtPrimValue (FloatValue v) = pure [prettyText v]

fmtDimIndex :: UncheckedDimIndex -> FmtM Fmt
fmtDimIndex (DimFix e) = fmtExp e
fmtDimIndex (DimSlice i j (Just s)) = do
  i' <- maybe (pure mempty) fmtExp i
  j' <- maybe (pure mempty) fmtExp j
  s' <- fmtExp s
  pure $ i' <> [":"] <> j' <> [":"] <> s'
fmtDimIndex (DimSlice i (Just j) s) = do
  i' <- maybe (pure mempty) fmtExp i
  j' <- fmtExp j
  s' <- maybe (pure mempty) (fmap ([":"] <>) . fmtExp) s
  pure $ i' <> [":"] <> j' <> s'
fmtDimIndex (DimSlice i Nothing Nothing) =
  (<> [":"]) <$> maybe (pure mempty) fmtExp i

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

fmtExp :: UncheckedExp -> FmtM Fmt
fmtExp (Var name _ _loc) = pure [prettyText name]
fmtExp (Hole _ _loc) = pure ["???"]
fmtExp (Parens e _loc) = parens <$> fmtExp e
fmtExp (QualParens (v, _loc) e _loc') = do
  fmt <- fmtExp e
  pure $ [prettyText v] <> parens fmt
fmtExp (Ascript e t _loc) = do
  e' <- fmtExp e
  t' <- fmtTypeExp t
  pure $ e' <> [":"] <> t'
fmtExp (Coerce e t _ _loc) = do
  e' <- fmtExp e
  t' <- fmtTypeExp t
  pure $ e' <> [":>"] <> t'
fmtExp (Literal v _loc) = pure [prettyText v]
fmtExp (IntLit v _ _loc) = pure [prettyText v]
fmtExp (FloatLit v _ _loc) = pure [prettyText v]
fmtExp (TupLit es _loc) = parens . L.intercalate [", "] <$> mapM fmtExp es
fmtExp (RecordLit fs _loc) = braces . L.intercalate [", "] <$> mapM fmtField fs
fmtExp (ArrayVal vs _ _loc) = brackets . L.intercalate [", "] <$> mapM fmtPrimValue vs
fmtExp (ArrayLit es _ _loc) = brackets . L.intercalate [", "] <$> mapM fmtExp es
fmtExp (StringLit s _loc) = pure [prettyText $ show $ fmap (chr . fromIntegral) s]
fmtExp (Project k e _ _loc) = do
  e' <- fmtExp e
  pure $ e' <> ["."] <> [prettyText k]
fmtExp (Negate e _loc) = do
  e' <- fmtExp e
  pure $ mapFirst ("-" <>) e'
fmtExp (Not e _loc) = do
  e' <- fmtExp e
  pure $ mapFirst ("not " <>) e'
fmtExp (Update src idxs ve _loc) = do
  src' <- fmtExp src
  idxs' <- brackets . L.intercalate [", "] <$> mapM fmtDimIndex idxs
  ve' <- fmtExp ve
  pure $ src' <> ["with"] <> idxs' <> ["="] <> ve'
fmtExp (RecordUpdate src fs ve _ _loc) = do
  src' <- fmtExp src
  let fs' = L.intersperse "." $ prettyText <$> fs
  ve' <- fmtExp ve
  pure $ src' <> ["with"] <> fs' <> ["="] <> ve'
fmtExp (Assert e1 e2 _ _loc) = do
  e1' <- fmtExp e1
  e2' <- fmtExp e2
  pure $ ["assert"] <> e1' <> e2'
fmtExp (Lambda params body rettype _ _) = do
  params' <- L.intercalate [" "] <$> mapM fmtPat params
  body' <- fmtExp body
  ascript <- maybe (pure mempty) (fmap ([":"] <>) . fmtTypeExp) rettype
  pure $ ["\\"] <> params' <> ascript <> [" -> "] <> body'
fmtExp (OpSection binop _ _loc) = pure ["(" <> fmtQualName binop <> ")"]
fmtExp (OpSectionLeft binop _ x _ _ _) =
  pure [mconcat $ parens [prettyText x <> fmtBinOp binop]]
fmtExp (OpSectionRight binop _ x _ _ _) =
  pure [mconcat $ parens [fmtBinOp binop <> prettyText x]]
fmtExp (ProjectSection fields _ _) =
  pure [mconcat $ parens $ p <$> fields]
  where
    p name = "." <> prettyText name
fmtExp (IndexSection idxs _ _) = do
  idxs' <- mconcat . brackets . L.intercalate [", "] <$> mapM fmtDimIndex idxs
  pure ["(" <> "." <> idxs' <> ")"]
fmtExp (Constr n cs _ _) = do
  cs' <- mconcat . L.intercalate [" "] <$> mapM fmtExp cs
  pure ["#" <> prettyText n <> cs']
fmtExp (Attr attr e _) = do
  attr' <- fmtAttr attr
  e' <- fmtExp e
  pure $ attr' <> e'
fmtExp (AppExp e _) = fmtAppExp e

fmtQualName :: QualName Name -> Line
fmtQualName (QualName names name) =
  pre <> prettyText name
  where
    pre =
      if null names
        then ""
        else T.intercalate "." (prettyText <$> names) <> "."

fmtCase :: UncheckedCase -> FmtM Fmt
fmtCase (CasePat p e _) = do
  p' <- fmtPat p
  e' <- fmtExp e
  pure $ ["case"] <> p' <> [" -> "] <> e'

-- Should check if exp match pat
matchPat :: UncheckedPat ParamType -> UncheckedExp -> Bool
matchPat _ _ = False
-- matchPat (TuplePat _pats _paramt) _exp = undefined 
-- matchPat (RecordPat _namePats _loc) _exp = undefined 
-- matchPat (PatParens _pats _loc) _exp = undefined 
-- matchPat (Id _names _fs _loc) _exp = undefined
-- matchPat (Wildcard _f _loc) _exp = False 
-- matchPat (PatAscription _pats _tyype _loc) _exp = undefined  
-- matchPat (PatLit _lit _f _loc) _exp = undefined 
-- matchPat (PatConstr _name _f _pats _loc) _exp = undefined 
-- matchPat (PatAttr _info _pat _loc) _exp = undefined 

fmtAppExp :: AppExpBase NoInfo Name -> FmtM Fmt
fmtAppExp (BinOp (bop, _) _ (x, _) (y, _) _) = do
  x' <- fmtExp x
  y' <- fmtExp y
  pure $ x' <> [fmtBinOp bop] <> y'
fmtAppExp (Match e cs _) = do
  e' <- fmtExp e
  cs' <- mconcat <$> mapM fmtCase (toList cs)
  pure $ ["match"] <> e' <> cs'
-- should omit the initial value expression 
-- need some way to catch when the value expression match the pattern 
fmtAppExp (Loop sizeparams pat initexp form loopbody _) | matchPat pat initexp = do
  let sizeparams' = ("[" <>) . (<> "]") . prettyText . toName <$> sizeparams
  pat' <- fmtPat pat
  form' <- fmtLoopForm form
  loopbody' <- fmtExp loopbody
  pure $
    ["loop"]
      <> sizeparams'
      <> pat'
      <> form'
      <> ["do"]
      <> loopbody'
fmtAppExp (Loop sizeparams pat initexp form loopbody _) = do
  let sizeparams' = ("[" <>) . (<> "]") . prettyText . toName <$> sizeparams
  pat' <- fmtPat pat
  initexp' <- fmtExp initexp
  form' <- fmtLoopForm form
  loopbody' <- fmtExp loopbody
  pure $
    ["loop"]
      <> sizeparams'
      <> pat'
      <> ["="]
      <> initexp' 
      <> form'
      <> ["do"]
      <> loopbody'
fmtAppExp (Index e idxs _) = do
  e' <- fmtExp e
  idxs' <- L.intercalate [","] <$> mapM fmtDimIndex idxs
  pure $ [mconcat e' <> "["] <> idxs' <> ["]"] -- It is important that "[" is connected to its expression.
fmtAppExp (LetPat sizes pat e body _) = do
  let sizes' = mconcat $ fmtSizeBinder <$> sizes
  pat' <- fmtPat pat
  e' <- fmtExp e
  body' <- letBody body
  pure $ ["let"] <> sizes' <> pat' <> ["="] <> e' <> body'
fmtAppExp (LetFun fname (tparams, params, retdecl, _, e) body _) = do
  tparams' <- mconcat <$> mapM fmtTypeParam tparams
  params' <- mconcat <$> mapM fmtPat params
  retdecl' <-
    case fmtTypeExp <$> retdecl of
      Just a -> fmap ([":"] <>) a
      Nothing -> pure []
  e' <- fmtExp e
  body' <- letBody body
  pure $
    ["let"]
      <> [prettyText fname]
      <> tparams'
      <> params'
      <> retdecl'
      <> ["="]
      <> e'
      <> body'
fmtAppExp (LetWith dest src idxs ve body _)
  | dest == src = do
      dest' <- fmtIdent dest
      idxs' <- L.intercalate [","] <$> mapM fmtDimIndex idxs
      ve' <- fmtExp ve
      body' <- letBody body
      pure $
        ["let"]
          <> [mconcat dest' <> "["]
          <> idxs' -- It is important that "[" is connected to its expression.
          <> ["]"]
          <> ["="]
          <> ve'
          <> body'
  | otherwise = do
      dest' <- fmtIdent dest
      src' <- fmtIdent src
      idxs' <- mconcat <$> mapM fmtDimIndex idxs
      ve' <- fmtExp ve
      body' <- letBody body
      pure $
        ["let"]
          <> dest'
          <> ["="]
          <> src'
          <> ["with"]
          <> idxs'
          <> ve'
          <> body'
fmtAppExp (Range start maybe_step end _) = do
  start' <- fmtExp start
  maybe_step' <- maybe (pure mempty) (fmap ([".."] <>) . fmtExp) maybe_step
  end' <-
    case end of
      DownToExclusive end' -> (["..>"] <>) <$> fmtExp end'
      ToInclusive end' -> (["..."] <>) <$> fmtExp end'
      UpToExclusive end' -> (["..<"] <>) <$> fmtExp end'
  pure $ start' <> maybe_step' <> end'
fmtAppExp (If c t f _) = do
  c' <- fmtExp c
  t' <- fmtExp t
  f' <- fmtExp f
  pure $
    ["if"]
      <> c'
      <> ["then"]
      <> t'
      <> ["else"]
      <> f'
fmtAppExp (Apply f args _) = do
  f' <- fmtExp f
  args' <- mconcat <$> mapM (fmtExp . snd) (toList args)
  pure $ f' <> args'

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmtExp body
letBody body@(AppExp LetFun {} _) = fmtExp body
letBody body@(AppExp LetWith {} _) = fmtExp body 
letBody body = do
  body' <- fmtExp body
  pure $ ["in"] <> body'

fmtSizeBinder :: SizeBinder Name -> Fmt
fmtSizeBinder (SizeBinder v _) = brackets [prettyText v]

fmtIdent :: IdentBase NoInfo Name t -> FmtM Fmt
fmtIdent = pure . L.singleton . prettyText . identName

fmtLoopForm :: LoopFormBase NoInfo Name -> FmtM Fmt
fmtLoopForm (For i ubound) = do
  i' <- fmtIdent i
  ubound' <- fmtExp ubound
  pure $ ["for"] <> i' <> ["<"] <> ubound'
fmtLoopForm (ForIn x e) = do
  x' <- fmtPat x
  e' <- fmtExp e
  pure $ ["for"] <> x' <> ["in"] <> e'
fmtLoopForm (While cond) = do
  cond' <- fmtExp cond
  pure $ ["while"] <> cond'

fmtBinOp :: QualName Name -> Line
fmtBinOp bop =
  case leading of
    Backtick -> "`" <> fmtQualName bop <> "`"
    _ -> prettyText bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

fmtValBind :: UncheckedValBind -> FmtM Fmt
fmtValBind (ValBind entry name retdecl _rettype tparams args body _doc attrs _loc) = do
  fmt_attrs <- concat <$> mapM fmtAttr attrs
  tparams' <- concat <$> mapM fmtTypeParam tparams
  args' <- concat <$> mapM fmtPat args
  retdecl' <-
    case fmtTypeExp <$> retdecl of
      Just a -> fmap ([":"] <>) a
      Nothing -> pure []
  body' <- fmtExp body
  pure $
    fmt_attrs
      <> fun
      <> [fmtName name]
      <> tparams'
      <> args'
      <> retdecl'
      <> ["="]
      <> body'
  where
    fun =
      case entry of
        Just _ -> ["entry"]
        _ -> ["def"]

fmtSizeExp :: SizeExp UncheckedExp -> FmtM Fmt
fmtSizeExp (SizeExp d _loc) = do
  d' <- fmtExp d
  pure $ brackets d'
fmtSizeExp (SizeExpAny _loc) = pure $ brackets mempty

fmtSpecBase :: UncheckedSpec -> FmtM Fmt
fmtSpecBase (TypeAbbrSpec tpsig) = fmtTypeBind tpsig
fmtSpecBase (TypeSpec l name ps _doc _loc) = do
  l' <- fmtLiftedness l
  ps' <- mapM fmtTypeParam ps
  pure $ ["type" <> l'] <> mconcat ([prettyText name] : ps')
fmtSpecBase (ValSpec name ps te _ _doc _loc) = do
  ps' <- mapM fmtTypeParam ps
  te' <- fmtTypeExp te
  pure $ ["val"] <> mconcat ([prettyText name] : ps') <> [": "] <> te'
fmtSpecBase (ModSpec name mte _doc _loc) = do
  mte' <- fmtModTypeExp mte
  pure $ ["module " <> prettyText name <> ": "] <> mte'
fmtSpecBase (IncludeSpec mte _loc) = do
  mte' <- fmtModTypeExp mte
  pure $ ["include "] <> mte'

fmtModTypeExp :: UncheckedModTypeExp -> FmtM Fmt
fmtModTypeExp (ModTypeVar v _ _loc) = pure [prettyText v]
fmtModTypeExp (ModTypeParens mte _loc) = do
  mte' <- fmtModTypeExp mte
  pure $ parens mte'
fmtModTypeExp (ModTypeSpecs sbs _loc) = do
  sbs' <- mapM fmtSpecBase sbs
  pure $ braces $ mconcat sbs'
fmtModTypeExp (ModTypeWith mte (TypeRef v ps td _) _loc) = do
  mte' <- fmtModTypeExp mte
  ps' <- mapM fmtTypeParam ps
  td' <- fmtTypeExp td
  pure $ mte' <> ["with " <> prettyText v <> " "] <> mconcat ps' <> ["="] <> td'
fmtModTypeExp (ModTypeArrow (Just v) te0 te1 _loc) = do
  te0' <- fmtModTypeExp te0
  te1' <- fmtModTypeExp te1
  pure $ parens [prettyText v <> ": "] <> te0' <> ["->"] <> te1'
fmtModTypeExp (ModTypeArrow Nothing te0 te1 _loc) = do
  te0' <- fmtModTypeExp te0
  te1' <- fmtModTypeExp te1
  pure $ te0' <> [" -> "] <> te1'

fmtModTypeBind :: UncheckedModTypeBind -> FmtM Fmt
fmtModTypeBind (ModTypeBind pName pSig _ _) = do
  pSig' <- fmtModTypeExp pSig
  pure $ ["module type " <> prettyText pName <> " = "] <> pSig'

fmtModParam :: ModParamBase NoInfo Name -> FmtM Fmt
fmtModParam (ModParam pName pSig _f _loc) = do
  pSig' <- fmtModTypeExp pSig
  pure $ parens $ [prettyText pName <> ": "] <> pSig'

fmtModBind :: UncheckedModBind -> FmtM Fmt
fmtModBind (ModBind name ps sig te _doc _loc) = do
  ps' <- mapM fmtModParam ps
  sig' <- fmtSig sig
  te' <- fmtModExp te
  pure $ ["module "] <> mconcat ([prettyText name] : ps') <> sig' <> [" = "] <> te'
  where
    fmtSig s = case s of
      Nothing -> pure mempty
      Just (s', _f) -> do
        s'' <- fmtModTypeExp s'
        pure $ [" : "] <> s'' <> [" "]

-- All of these should probably be "extra" indented
fmtModExp :: UncheckedModExp -> FmtM Fmt
fmtModExp (ModVar v _loc) = pure [prettyText v]
fmtModExp (ModParens f _loc) = do
  f' <- fmtModExp f
  pure $ parens f'
fmtModExp (ModImport path _f _loc) =
  pure ["import \"" <> prettyText path <> "\""]
-- Should be put inside a nested block
fmtModExp (ModDecs decs _loc) = do
  decs' <- mapM fmtDec decs
  pure $ braces $ mconcat decs'
-- should be put in parens if indentation is above some thresshold?
-- (thresshold = 10)
-- Due note: I do not think so since we do not want to interfere with parenthesis for our formatter.
fmtModExp (ModApply f a _f0 _f1 _loc) = do
  f' <- fmtModExp f
  a' <- fmtModExp a
  pure $ f' <> [" "] <> a'
fmtModExp (ModAscript me se _f _loc) = do
  me' <- fmtModExp me
  se' <- fmtModTypeExp se
  pure $ me' <> [":"] <> se'
fmtModExp (ModLambda param maybe_sig body _loc) = do
  param' <- fmtModParam param
  maybe_sig' <-
    case maybe_sig of
      Nothing -> pure mempty
      Just (sig, _) -> ([":"] <>) <$> fmtModTypeExp sig
  body' <- fmtModExp body
  pure $ ["\\"] <> param' <> maybe_sig' <> ["->"] <> body'

-- | Formatting of Futhark declarations.
fmtDec :: UncheckedDec -> FmtM Fmt
fmtDec (ValDec t) = buildFmt fmtValBind t -- A value declaration.
fmtDec (TypeDec tb) = buildFmt fmtTypeBind tb -- A type declaration.
fmtDec (ModTypeDec tb) = buildFmt fmtModTypeBind tb -- A module type declation.
fmtDec (ModDec tb) = buildFmt fmtModBind tb -- A module declation.
fmtDec (OpenDec tb _loc) = do
  tb' <- fmtModExp tb
  pure $ ["open "] <> tb'
-- Adds the local keyword
fmtDec (LocalDec tb _loc) = do
  tb' <- fmtDec tb
  pure $ ["local "] <> tb'
-- Import declarations.
fmtDec (ImportDec path _tb _loc) =
  pure ["import \"" <> prettyText path <> "\""]

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
          -- let number i l = T.pack $ printf "%4d %s" (i :: Int) l
          let fmt = evalState (fmtProg prog) (FmtState {comments = cs})
          T.hPutStr stdout $ T.unlines fmt
    _ -> Nothing

fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  pure $ T.unlines $ evalState (fmtProg prog) (FmtState {comments = cs})
