module Futhark.CLI.Fmt.Printer (fmtText) where

import Futhark.CLI.Fmt.AST
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Char (chr)
import Data.Foldable
import Data.Text qualified as T
import Futhark.Util.Loc
import Language.Futhark
import Language.Futhark.Parser
  ( Comment (..),
    SyntaxError (..),
    parseFutharkWithComments,
  )
import Prettyprinter.Internal (Pretty)

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

-- State monad to keep track of comments
newtype FmtState =
  FmtState
  { comments :: [Comment] }
  deriving (Show, Eq, Ord)

data Layout = MultiLine | SingleLine deriving (Show, Eq)

type FmtM a = ReaderT Layout (StateT FmtState Identity) a

-- Functions for operating on FmtM monad
fmtComments :: (Located a) => a -> FmtM Fmt
fmtComments a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s {comments = cs}
      cs' <- fmtComments a -- fmts remaining comments
      pure $ comment (commentText c) <> cs'
    _ -> pure nil

{-
trailingComment :: (Located a) => a -> FmtM Fmt
trailingComment a = do
  s <- get
  case comments s of
    c : cs | isSameLine a c -> do
      put $ s {comments = cs}
      pure $ comment (commentText c)
    _ -> pure ""
-}

sepByLayout :: Located a => a -> Fmt -> Fmt -> Fmt
sepByLayout loc a b =
  case lineLayout loc of
    MultiLine -> stdNest (a </> b)
    SingleLine -> a <+> b

fmtName :: Name -> Fmt
fmtName name
  | operatorName name = parens $ fmtPretty name
  | otherwise = fmtPretty name

fmtPretty :: Pretty a => a -> Fmt
fmtPretty = code . prettyText

-- parses comments infront of a and converts a to fmt using formatting function f
buildFmt :: (Located a) => a -> FmtM Fmt -> FmtM Fmt -> FmtM Fmt
buildFmt a single multi = local (const $ lineLayout a) $ do
  c <- fmtComments a
  m <- ask
  a' <- if m == SingleLine then single else multi
  -- c' <- trailingComment a
  pure $ c <> a'

lineLayout :: Located a => a -> Layout
lineLayout a =
  case locOf a of
    Loc start end ->
      if posLine start == posLine end
      then SingleLine
      else MultiLine
    NoLoc -> undefined -- should throw an error

-- | Documentation comments are always optional, so this takes a 'Maybe'.
-- TODO: make special documentation comments in Fmt?
-- TODO: Add "--" and "-- |" in pretty printer
fmtDocComment :: Maybe DocComment -> FmtM Fmt
fmtDocComment (Just (DocComment x _loc)) =
  pure $ mconcat $ prefixes (T.lines x)
  where
    prefixes [] = []
    prefixes (l : ls) = comment ("-- | " <> l) : map (comment . ("-- " <>)) ls
fmtDocComment Nothing = pure nil


fmtFieldType :: (Name, UncheckedTypeExp) -> FmtM Fmt
fmtFieldType (name', t) = do
  t' <- fmtTypeExp t
  pure $ fmtName name' <> code ":" <+> t'
  
fmtParamType :: Maybe Name -> UncheckedTypeExp -> FmtM Fmt
fmtParamType (Just n) te = 
  parens . ((fmtName n <> code ":") <+>) <$> fmtTypeExp te
fmtParamType Nothing te = fmtTypeExp te

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> FmtM Fmt
fmtSumTypeConstr (name, fs) = 
  ((code "#" <> fmtName name) <+>) . sep (code "") <$> mapM fmtTypeExp fs 

-- | Formatting of Futhark type expressions.
fmtTypeExp :: UncheckedTypeExp -> FmtM Fmt
fmtTypeExp (TEVar v loc) = buildFmt loc single multi
  where
    single = pure $ fmtQualNameSingle v
    multi = pure $ fmtQualNameMulti v
fmtTypeExp (TETuple ts loc) = buildFmt loc single multi
  where
    single = parens . sepSpace (code ",") <$> mapM fmtTypeExp ts
    multi = parens . sepLine (code ",") <$> mapM fmtTypeExp ts
fmtTypeExp (TEParens te loc) = buildFmt loc single multi
  where
    single = parens <$> fmtTypeExp te
    multi = single -- not sure this is correct 
fmtTypeExp (TERecord fs loc) = buildFmt loc single multi
  where
    single = braces . sepSpace (code ",") <$> mapM fmtFieldType fs
    multi = braces . sepLine (code ",") <$> mapM fmtFieldType fs
fmtTypeExp (TEArray se te loc) = buildFmt loc single multi -- A array with an size expression
  where
    single = (<>) <$> fmtSizeExp se <*> fmtTypeExp te
    multi = single -- not sure if this can be multi line
-- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
fmtTypeExp (TEUnique te loc) = buildFmt loc single multi
  where
    single = (code "*" <>) <$> fmtTypeExp te
    multi = single -- not sure if this can be multi line
-- I am not sure I guess applying a higher kinded type to some type expression
fmtTypeExp (TEApply te tArgE loc) = buildFmt loc single multi
  where
    single = (<+>) <$> fmtTypeExp te <*> fmtArgExp tArgE
    multi = single -- not sure if this can be multi lin
-- this is "->"
fmtTypeExp (TEArrow name te0 te1 loc) = buildFmt loc single multi
  where
    single = do
      arg <- fmtParamType name te0
      te1' <- fmtTypeExp te1
      pure $ arg <+> code "->" <+> te1'
    multi = do 
      arg <- fmtParamType name te0
      te1' <- fmtTypeExp te1 
      pure $ stdNest $ arg <+> code "->" </> te1'
-- This should be "|"
fmtTypeExp (TESum tes loc) = buildFmt loc single multi
  where
    single = sepSpace (code " |") <$> mapM fmtSumTypeConstr tes
    multi = sepLine (code "|") <$> mapM fmtSumTypeConstr tes
fmtTypeExp (TEDim dims te loc) = buildFmt loc single multi
  where
    single = do
      te' <- fmtTypeExp te
      let dims' = mconcat $ map (brackets . fmtName) dims
      pure $ code "?" <> dims' <> code "." <> te'
    multi = single -- not sure how to format this as multiple lines

fmtArgExp :: TypeArgExp UncheckedExp Name -> FmtM Fmt
fmtArgExp (TypeArgExpSize se) = buildFmt se single multi
  where
    single = fmtSizeExp se
    multi = single
fmtArgExp (TypeArgExpType te) = buildFmt te single multi
  where
    single = fmtTypeExp te
    multi = single

fmtTypeBind :: UncheckedTypeBind -> FmtM Fmt
fmtTypeBind (TypeBind name l ps e NoInfo dc loc) = buildFmt loc single multi
  where
    common = do
      l' <- fmtLiftedness l
      dc' <- fmtDocComment dc
      ps' <- mapM fmtTypeParam ps
      pure (l', dc', ps')
    single = do
      (l', dc', ps') <- common
      e' <- fmtTypeExp e
      pure $
        dc'
        </> (code "type"
             <> l'
             <+> fmtName name
             <> code (if null ps then "" else " ")
             <> sep space ps'
             <+> code "="
             <+> e')
    multi = do
      (l', dc', ps') <- common
      e' <- fmtTypeExp e
      pure $
        dc'
          </> (code "type"
               <> l'
               <+> fmtName name
               <> sep space ps'
               <+> code "=")
          <+> e'

fmtAttrAtom :: AttrAtom a -> FmtM Fmt
fmtAttrAtom (AtomName name) = pure $ fmtName name
fmtAttrAtom (AtomInt int) = pure $ code $ prettyText int

fmtAttrInfo :: AttrInfo a -> FmtM Fmt
fmtAttrInfo (AttrAtom attr _loc) = fmtAttrAtom attr
fmtAttrInfo (AttrComp name attrs _loc) =
  (fmtName name <>) . parens . sep (code ",") <$> mapM fmtAttrInfo attrs

-- I've added smth to make the code parse
fmtAttr :: AttrInfo a -> FmtM Fmt
fmtAttr attr =  (code "#" <>) . brackets <$> fmtAttrInfo attr

fmtLiftedness :: Liftedness -> FmtM Fmt
fmtLiftedness Unlifted = pure $ code ""
fmtLiftedness SizeLifted = pure $ code "~"
fmtLiftedness Lifted = pure $ code "^"

fmtTypeParam :: UncheckedTypeParam -> FmtM Fmt
fmtTypeParam (TypeParamDim name loc) = buildFmt loc single multi
  where
    single = pure $ brackets $ fmtName name
    multi = single
fmtTypeParam (TypeParamType l name loc) = buildFmt loc single multi
  where
    single = do
      l' <- fmtLiftedness l
      pure $ code "'" <> l' <> fmtName name
    multi = single

fmtPat :: UncheckedPat t -> FmtM Fmt
fmtPat (TuplePat pats loc) = buildFmt loc single multi
  where
    single = parens . sepSpace (code ",") <$> mapM fmtPat pats
    multi = parens . sepLine (code ",") <$> mapM fmtPat pats
fmtPat (RecordPat pats loc) = buildFmt loc single multi
  where
    fmtFieldPat (name, t) = do
      t' <- fmtPat t
      pure $ fmtName name <+> code "=" <+> t' -- Currently it allways adds the fields it seems. I think it has to do this.
    single = braces <$> (sepSpace (code ",") <$> mapM fmtFieldPat pats)
    multi = braces <$> (sepLine (code ",") <$> mapM fmtFieldPat pats)
fmtPat (PatParens pat loc) = buildFmt loc single multi
  where
    single = parens <$> fmtPat pat
    multi = (</> code ")") <$> (stdNest . (code "(" </>) <$> fmtPat pat)
fmtPat (Id name _ loc) = buildFmt loc single multi
  where
    single = pure $ fmtName name
    multi = single
fmtPat (Wildcard _t loc) = buildFmt loc single multi
  where
    single = pure $ code "_"
    multi = single
fmtPat (PatAscription pat t loc) = buildFmt loc single multi
  where
    pat' = (<> code ":") <$> fmtPat pat
    single = (<+>) <$> pat' <*> fmtTypeExp t
    multi = single
fmtPat (PatLit e _ loc) = buildFmt loc single multi
  where
    single = pure $ fmtPretty e
    multi = single
fmtPat (PatConstr n _ pats loc) = buildFmt loc single multi
  where
    common s = sep s <$> mapM fmtPat pats
    cons = pure $ code "#" <> fmtName n
    single = (<+>) <$> cons <*> common space
    multi = fmap stdNest $ (</>) <$> cons <*> common line
fmtPat (PatAttr attr pat loc) = buildFmt loc single multi
  where
    single = (<+>) <$> fmtAttr attr <*> fmtPat pat
    multi = single

fmtField :: FieldBase NoInfo Name -> FmtM Fmt
fmtField (RecordFieldExplicit name e loc) = buildFmt loc single multi
  where
    common = fmtName name <+> code "="
    single = (common <+>) <$> fmtExp e
    multi = (common </>) <$> fmtExp e
fmtField (RecordFieldImplicit name _ loc) = buildFmt loc single multi
  where
    single = pure $ fmtName name
    multi = single

fmtPrimValue :: PrimValue -> FmtM Fmt
fmtPrimValue (UnsignedValue (Int8Value v)) =
  pure $ fmtPretty (show (fromIntegral v :: Word8)) <> code "u8"
fmtPrimValue (UnsignedValue (Int16Value v)) =
  pure $ fmtPretty (show (fromIntegral v :: Word16)) <> code "u16"
fmtPrimValue (UnsignedValue (Int32Value v)) =
  pure $ fmtPretty (show (fromIntegral v :: Word32)) <> code "u32"
fmtPrimValue (UnsignedValue (Int64Value v)) =
  pure $ fmtPretty (show (fromIntegral v :: Word64)) <> code "u64"
fmtPrimValue (SignedValue v) = pure $ fmtPretty v
fmtPrimValue (BoolValue True) = pure $ code "true"
fmtPrimValue (BoolValue False) = pure $ code "false"
fmtPrimValue (FloatValue v) = pure $ fmtPretty v

fmtDimIndex :: UncheckedDimIndex -> FmtM Fmt
fmtDimIndex (DimFix e) = fmtExp e
fmtDimIndex (DimSlice i j (Just s)) = do
  i' <- maybe (pure mempty) fmtExp i
  j' <- maybe (pure mempty) fmtExp j
  s' <- fmtExp s
  pure $ i' <> code ":" <> j' <> code ":" <> s'
fmtDimIndex (DimSlice i (Just j) s) = do
  i' <- maybe (pure mempty) fmtExp i
  j' <- fmtExp j
  s' <- maybe (pure mempty) (fmap (code ":" <>) . fmtExp) s
  pure $ i' <> code ":" <> j' <> s'
fmtDimIndex (DimSlice i Nothing Nothing) =
  (<> code ":") <$> maybe (pure mempty) fmtExp i

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

fmtExp :: UncheckedExp -> FmtM Fmt
fmtExp (Var name _ loc) = buildFmt loc single multi
  where
    single = fmtQualName name
    multi = single
fmtExp (Hole _ loc) = buildFmt loc single multi
  where
    single = pure $ code "???"
    multi = single
fmtExp (Parens e loc) = buildFmt loc single multi
  where
    single = parens <$> fmtExp e
    multi = (</> code ")") <$> (stdNest . (code "(" </>) <$> fmtExp e)
fmtExp (QualParens (v, _loc) e loc') = buildFmt loc' single multi
  where
    single = do
      fmt <- fmtExp e
      n <- fmtQualName v
      pure $ n <> parens fmt
    multi = single
fmtExp (Ascript e t loc) = buildFmt loc single multi
  where
    single = do
      e' <- fmtExp e
      t' <- fmtTypeExp t
      pure $ e' <> code ":" <+> t'
    multi = single
fmtExp (Coerce e t _ loc) = buildFmt loc single multi
  where
    single = do
      e' <- fmtExp e
      t' <- fmtTypeExp t
      pure $ e' <+> code ":>" <+> t'
    multi = single
fmtExp (Literal v loc) = buildFmt loc single single
  where
    single = pure $ fmtPretty v -- Not sure how this can be multiline.
fmtExp (IntLit v _ loc) = buildFmt loc single single
  where
    single = pure $ fmtPretty v -- Not sure how this can be multiline.
fmtExp (FloatLit v _ loc) = buildFmt loc single single
  where
    single = pure $ fmtPretty v -- Not sure how this can be multiline.
fmtExp (TupLit es loc) = buildFmt loc single multi
  where
    single = parens . sepSpace (code ",") <$> mapM fmtExp es
    multi = parens . sepLine (code ",") <$> mapM fmtExp es
fmtExp (RecordLit fs loc) = buildFmt loc single multi
  where
    single = braces . sepSpace (code ",") <$> mapM fmtField fs
    multi = braces . sepLine (code ",") <$> mapM fmtField fs
fmtExp (ArrayVal vs _ loc) = buildFmt loc single multi
  where
    single = brackets . sepSpace (code ",") <$> mapM fmtPrimValue vs
    multi = brackets . sepLine (code ",") <$> mapM fmtPrimValue vs
fmtExp (ArrayLit es _ loc) = buildFmt loc single multi
  where
    single = brackets . sepSpace (code ",") <$> mapM fmtExp es
    multi = brackets . sepLine (code ",") <$> mapM fmtExp es
fmtExp (StringLit s loc) = buildFmt loc single multi
  where
    single = pure $ fmtPretty $ show $ fmap (chr . fromIntegral) s
    multi = single
fmtExp (Project k e _ loc) = buildFmt loc single multi
  where
    single = do
      e' <- fmtExp e
      pure $ e' <> code "." <> fmtPretty k
    multi = single
fmtExp (Negate e loc) = buildFmt loc single multi
  where
    single = (code "-" <>) <$> fmtExp e
    multi = single
fmtExp (Not e loc) = buildFmt loc single multi
  where
    single = (code "not" <>) <$> fmtExp e
    multi = single
fmtExp (Update src idxs ve loc) = buildFmt loc single multi
  where
    common = do
      src' <- fmtExp src -- This could account for multiline.
      idxs' <- brackets . sep (code ", ") <$> mapM fmtDimIndex idxs -- This could account for multiline.
      pure $ src' <+> code "with" <+> idxs' <+> code "="
    single = (<+>) <$> common <*> fmtExp ve
    multi = (</>) <$> common <*> fmtExp ve
fmtExp (RecordUpdate src fs ve _ loc) = buildFmt loc single multi
  where
    common = do
      src' <- fmtExp src  -- This could account for multiline.
      let fs' = sep (code ".") $ fmtName <$> fs -- This could account for multiline.
      pure $ src' <+> code "with" <+> fs' <+> code "="
    single = (<+>) <$> fmtExp ve <*> common
    multi = (</>) <$> fmtExp ve <*> common
fmtExp (Assert e1 e2 _ loc) = buildFmt loc single multi
  where
    single = (<+>) <$> ((code "assert" <+>) <$> fmtExp e1) <*> fmtExp e2
    multi = fmap stdNest $ (</>) <$> ((code "assert" </>) <$> fmtExp e1) <*> fmtExp e2
fmtExp (Lambda params body rettype _ loc) = buildFmt loc single multi
  where
    common = do
      params' <- sep space <$> mapM fmtPat params
      ascript <- maybe (pure mempty) (fmap (code ": " <>) . fmtTypeExp) rettype
      pure $ code "\\" <> params' <> ascript <+> code "->"
    single = (<+>) <$> common <*> fmtExp body
    multi = fmap stdNest $ (</>) <$> common <*> fmtExp body
fmtExp (OpSection binop _ loc) = buildFmt loc single multi
  where
    single = parens <$> fmtQualName binop
    multi = single
fmtExp (OpSectionLeft binop _ x _ _ loc) = buildFmt loc single multi
  where
    single = fmap parens $ (<+>) <$> fmtExp x <*> fmtBinOp binop
    multi = single
fmtExp (OpSectionRight binop _ x _ _ loc) = buildFmt loc single multi
  where
    single = fmap parens $ (<+>) <$> fmtBinOp binop <*> fmtExp x
    multi = single
fmtExp (ProjectSection fields _ loc) = buildFmt loc single multi
  where
    single = pure $ parens $ mconcat $ p <$> fields
    multi = single
    p name = code "." <> fmtName name
fmtExp (IndexSection idxs _ loc) = buildFmt loc single multi
  where
    single = do
      idxs' <- brackets . sep (code ", ") <$> mapM fmtDimIndex idxs
      pure $ parens (code "." <> idxs')
    multi = single
fmtExp (Constr n cs _ loc) = buildFmt loc single multi
  where
    cons = pure $ code "#" <> fmtName n
    common s = sep s <$> mapM fmtExp cs
    single = (<+>) <$> cons <*> common space
    multi = fmap stdNest $ (</>) <$> cons <*> common line
fmtExp (Attr attr e loc) = buildFmt loc single multi
  where
    single = (<+>) <$> fmtAttr attr <*> fmtExp e
    multi = (</>) <$> fmtAttr attr <*> fmtExp e
fmtExp (AppExp e _loc) = buildFmt e single multi
  where
    single = fmtAppExp e
    multi = single

fmtQualNameSingle :: QualName Name -> Fmt
fmtQualNameSingle (QualName names name) =
  pre <> fmtName name
  where
    pre =
      if null names
        then nil
        else sep (code ".") (fmtName <$> names) <> code "."

fmtQualNameMulti :: QualName Name -> Fmt
fmtQualNameMulti (QualName names name) =
  pre <> fmtName name
  where
      pre =
        if null names then nil
        else sepLine (code ".") (fmtName <$> names) <> code "."

fmtQualName :: QualName Name -> FmtM Fmt
fmtQualName n = pure $ fmtQualNameSingle n

fmtCase :: UncheckedCase -> FmtM Fmt
fmtCase (CasePat p e loc) = buildFmt loc single multi
  where
    preSpace t = fmap (code t <+>)
    preLine t = fmap (stdNest . (code t <+>))
    single =
      (<+>) <$> preSpace "case" (fmtPat p) <*> preSpace "->" (fmtExp e)
    multi =
      (<+>) <$> preSpace "case" (fmtPat p) <*> preLine "->" (fmtExp e)

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
fmtAppExp (BinOp (bop, _) _ (x, _) (y, _) loc) = buildFmt loc single multi
  where
    f a b c = a <+> b <+> c
    g a b c= a </> b <+> c
    single = f <$> fmtExp x <*> fmtBinOp bop <*> fmtExp y
    multi = g <$> fmtExp x <*> fmtBinOp bop <*> fmtExp y
fmtAppExp (Match e cs loc) = buildFmt loc single multi
  where
    match = (code "match" <+>) <$> fmtExp e
    multiCases = sep line <$> mapM fmtCase (toList cs)
    singleCases = sep space <$> mapM fmtCase (toList cs)
    single = (<+>) <$> match <*> singleCases
    multi = fmap stdNest $ (</>) <$> match <*> multiCases
-- should omit the initial value expression 
-- need some way to catch when the value expression match the pattern 
fmtAppExp (Loop sizeparams pat initexp form loopbody loc) | matchPat pat initexp = buildFmt loc single multi
  where
    op = if null sizeparams then (<>) else (<+>)
    common = do
      let sizeparams' = mconcat $ brackets . fmtName . toName <$> sizeparams
      pat' <- fmtPat pat
      form' <- fmtLoopForm form
      pure $
        sepByLayout pat (
          code "loop" `op` sizeparams'
        ) pat'
        <+> form'
        <+> code "do"
    single = (<+>) <$> common <*> fmtExp loopbody
    multi = fmap stdNest $ (</>) <$> common <*> fmtExp loopbody
fmtAppExp (Loop sizeparams pat initexp form loopbody loc) = buildFmt loc single multi
  where
    op = if null sizeparams then (<>) else (<+>)
    common = do
      let sizeparams' = mconcat $ brackets . fmtName . toName <$> sizeparams
      pat' <- fmtPat pat
      initexp' <- fmtExp initexp
      form' <- fmtLoopForm form
      pure $
        sepByLayout initexp (
          sepByLayout pat (
            code "loop" `op` sizeparams'
          ) pat'
          <+> code "="
        ) initexp'
        <+> form'
        <+> code "do"
    single = (<+>) <$> common <*> fmtExp loopbody
    multi = fmap stdNest $ (</>) <$> common <*> fmtExp loopbody
fmtAppExp (Index e idxs loc) = buildFmt loc single multi
  where
    idxsM = mapM fmtDimIndex idxs
    aux f = brackets . f (code ",") <$> idxsM
    single = (<>) <$> fmtExp e <*> aux sepSpace
    multi = (<>) <$> fmtExp e <*> aux sepLine
fmtAppExp (LetPat sizes pat e body loc) = buildFmt loc single multi
  where
    common = do
      let sizes' = mconcat $ fmtSizeBinder <$> sizes
      pat' <- fmtPat pat
      e' <- fmtExp e
      pure $
        sepByLayout e (
          code "let"
          <+> sepNonEmpty space [sizes', pat']
          <+> code "="
        ) e'
    single = (<+>) <$> common <*> letBody body
    multi = (</>) <$> common <*> letBody body
fmtAppExp (LetFun fname (tparams, params, retdecl, _, e) body loc) = buildFmt loc single multi
  where
    common = do
      tparams' <- sep space <$> mapM fmtTypeParam tparams
      params' <- sep space <$> mapM fmtPat params
      retdecl' <-
        case fmtTypeExp <$> retdecl of
          Just a -> fmap (\b -> code ":" <+> b <> space) a
          Nothing -> pure nil
      let sub = sepNonEmpty space [tparams', params']
      e' <- fmtExp e
      pure $
        sepByLayout e (
          code "let"
          <+> fmtName fname
          <> (if isEmpty sub then nil else space)
          <> sub
          <> retdecl'
          <> code "="
          ) e'
    single = (<+>) <$> common <*> letBody body
    multi = (</>) <$> common <*> letBody body
fmtAppExp (LetWith dest src idxs ve body loc)
  | dest == src = buildFmt loc singleSame multiSame
  | otherwise = buildFmt loc singleDiff multiDiff
    where
      commonSame = do
        dest' <- fmtIdent dest
        idxs' <- brackets . sep (code ", ") <$> mapM fmtDimIndex idxs
        ve' <- fmtExp ve
        pure $
          sepByLayout ve (
            code "let"
              <+> dest'
              <> idxs'
              <+> code "="
            ) ve'
      singleSame = (<+>) <$> commonSame <*> letBody body
      multiSame = (</>) <$> commonSame <*> letBody body
      commonDiff = do
        dest' <- fmtIdent dest
        src' <- fmtIdent src
        idxs' <- brackets . sep (code ",") <$> mapM fmtDimIndex idxs
        ve' <- fmtExp ve
        pure $
          sepByLayout ve (
            code "let"
              <+> dest'
              <+> code "="
              <+> src'
              <+> code "with"
              <+> idxs'
            ) ve'
      singleDiff = (<+>) <$> commonDiff <*> letBody body
      multiDiff = (</>) <$> commonDiff <*> letBody body
fmtAppExp (Range start maybe_step end loc) = buildFmt loc single multi
  where
    single = do
      start' <- fmtExp start
      step <- maybe (pure mempty) (fmap (code ".." <>) . fmtExp) maybe_step
      end' <-
        case end of
          DownToExclusive end' -> (code "..>" <>) <$> fmtExp end'
          ToInclusive end' -> (code "..." <>) <$> fmtExp end'
          UpToExclusive end' -> (code "..<" <>) <$> fmtExp end'
      pure $ start' <> step <> end'
    multi = single
fmtAppExp (If c t f loc) = buildFmt loc single multi
  where
    single = do
      c' <- fmtExp c
      t' <- fmtExp t
      f' <- fmtExp f
      pure $
        code "if"
          <+> c'
          <+> code "then"
          <+> t'
          <+> code "else"
          <+> f'
    multi = do -- This should handle chained if expressions better.
      c' <- fmtExp c
      t' <- fmtExp t
      f' <- fmtExp f
      pure $
        code "if"
          <+> c'
          <+> code "then"
          </> stdNest t'
          </> code "else"
          </> stdNest f'
fmtAppExp (Apply f args loc) = buildFmt loc single multi
  where
    mArgs = sep space <$> mapM (fmtExp . snd) (toList args)
    single = (<+>) <$> fmtExp f <*> mArgs
    multi = fmap stdNest $ (</>) <$> fmtExp f <*> mArgs -- This should format in a pretty way but I am not sure how.

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmtExp body
letBody body@(AppExp LetFun {} _) = fmtExp body
letBody body@(AppExp LetWith {} _) = fmtExp body
letBody body = buildFmt body single multi
  where
    single = (code "in" <+>) <$> fmtExp body
    multi = fmap stdNest $ (code "in" </>) <$> fmtExp body

fmtSizeBinder :: SizeBinder Name -> Fmt
fmtSizeBinder (SizeBinder v _) = brackets $ fmtName v

fmtIdent :: IdentBase NoInfo Name t -> FmtM Fmt
fmtIdent = pure . fmtPretty . identName

fmtLoopForm :: LoopFormBase NoInfo Name -> FmtM Fmt
fmtLoopForm (For i ubound) = do
  i' <- fmtIdent i
  ubound' <- fmtExp ubound
  pure $ code "for" <+> i' <+> code "<" <+> ubound'
fmtLoopForm (ForIn x e) = do
  x' <- fmtPat x
  e' <- fmtExp e
  pure $ code "for" <+> x' <+> code "in" <+> e'
fmtLoopForm (While cond) = do
  cond' <- fmtExp cond
  pure $ code "while" <+> cond'

fmtBinOp :: QualName Name -> FmtM Fmt
fmtBinOp bop =
  case leading of
    Backtick -> (\a -> code "`" <> a <> code "`") <$> fmtQualName bop
    _any -> pure $ fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

fmtValBind :: UncheckedValBind -> FmtM Fmt
fmtValBind (ValBind entry name retdecl _rettype tparams args body doc attrs loc) = buildFmt loc single multi
  where
    common = do
      docs <- fmtDocComment doc
      fmt_attrs <- sep space <$> mapM fmtAttr attrs
      tparams' <- sep space <$> mapM fmtTypeParam tparams
      args' <- sep space <$> mapM fmtPat args
      retdecl' <-
        case fmtTypeExp <$> retdecl of
          Just a -> fmap (\b -> code ":" <+> b <> space) a
          Nothing -> pure space
      let sub = sepNonEmpty space [tparams', args']
      pure $
        docs
        <> (if null attrs then nil else fmt_attrs <> space)
        <> fun
        <+> fmtName name
        <> (if isEmpty sub then nil else space)
        <> sub
        <> retdecl'
        <> code "="
    single = (<+>) <$> common <*> fmtExp body
    multi = fmap (stdNest) $ (</>) <$> common <*> fmtExp body
    fun =
      case entry of
        Just _ -> code "entry"
        _any -> code "def"

fmtSizeExp :: SizeExp UncheckedExp -> FmtM Fmt
fmtSizeExp (SizeExp d loc) = buildFmt loc single multi
  where
    single = brackets <$> fmtExp d
    multi = single
fmtSizeExp (SizeExpAny loc) = buildFmt loc single multi
  where
    single = pure $ brackets mempty
    multi = single

fmtSpecBase :: UncheckedSpec -> FmtM Fmt
fmtSpecBase (TypeAbbrSpec tpsig) = fmtTypeBind tpsig
fmtSpecBase (TypeSpec l name ps doc loc) = buildFmt loc single multi
  where
    single = do
      doc' <- fmtDocComment doc
      l' <- fmtLiftedness l
      ps' <- mapM fmtTypeParam ps
      pure $ doc' <> code "type" <> l' <+> sep space (fmtName name : ps')
    multi = single
fmtSpecBase (ValSpec name ps te _ doc loc) = buildFmt loc single multi
  where
    multi = single
    single = do
      doc' <- fmtDocComment doc
      ps' <- mapM fmtTypeParam ps
      te' <- fmtTypeExp te
      pure $ doc' <> code "val" <+> sep space (fmtName name : ps') <> code ":" <+> te'
fmtSpecBase (ModSpec name mte doc loc) = buildFmt loc single multi
  where
    single = do
      doc' <- fmtDocComment doc
      mte' <- fmtModTypeExp mte
      pure $ doc' <> code "module" <+> fmtName name <> code ":" <+> mte'
    multi = single
fmtSpecBase (IncludeSpec mte loc) = buildFmt loc single multi
  where
    single = do
      mte' <- fmtModTypeExp mte
      pure $ code "include" <+> mte'
    multi = single

fmtModTypeExp :: UncheckedModTypeExp -> FmtM Fmt
fmtModTypeExp (ModTypeVar v _ loc) = buildFmt loc single multi
  where
    single = pure $ fmtPretty v
    multi = single
fmtModTypeExp (ModTypeParens mte loc) = buildFmt loc single multi
  where
    single = do
      mte' <- fmtModTypeExp mte
      pure $ parens mte'
    multi = single
fmtModTypeExp (ModTypeSpecs sbs loc) = buildFmt loc single multi
  where
    single = do
      sbs' <- mapM fmtSpecBase sbs
      pure $ braces $ mconcat sbs'
    multi = single
fmtModTypeExp (ModTypeWith mte (TypeRef v ps td _) loc) = buildFmt loc single multi
  where
    single = do
      mte' <- fmtModTypeExp mte
      ps' <- mapM fmtTypeParam ps
      td' <- fmtTypeExp td
      pure $ mte' <> code "with" <+> fmtPretty v <+> sep space ps' <+> code "=" <+> td'
    multi = single
fmtModTypeExp (ModTypeArrow (Just v) te0 te1 loc) = buildFmt loc single multi
  where
    op a b = parens (fmtName v <> code ":") <+> a <+> code "->" <+> b
    single = op <$> fmtModTypeExp te0 <*> fmtModTypeExp te1
    multi = single
fmtModTypeExp (ModTypeArrow Nothing te0 te1 loc) = buildFmt loc single multi
  where
    op a b = a <+> code "->" <+> b
    single = op <$> fmtModTypeExp te0 <*> fmtModTypeExp te1
    multi = single

fmtModTypeBind :: UncheckedModTypeBind -> FmtM Fmt
fmtModTypeBind (ModTypeBind pName pSig doc loc) = buildFmt loc single multi
  where
    common = do
      doc' <- fmtDocComment doc
      pure $ doc' <> code "module type" <+> fmtName pName <+> code "="
    single = (<+>) <$> common <*> fmtModTypeExp pSig
    multi = (</>) <$> common <*> fmtModTypeExp pSig

fmtModParam :: ModParamBase NoInfo Name -> FmtM Fmt
fmtModParam (ModParam pName pSig _f loc) = buildFmt loc single multi
  where
    single = do
      pSig' <- fmtModTypeExp pSig
      pure $ parens $ fmtName pName <> code ":" <+> pSig'
    multi = single

fmtModBind :: UncheckedModBind -> FmtM Fmt
fmtModBind (ModBind name ps sig te doc loc) = buildFmt loc single multi
  where
    single = do
      doc' <- fmtDocComment doc
      ps' <- mapM fmtModParam ps
      sig' <- fmtSig sig
      te' <- fmtModExp te
      pure $
        doc'
        <> code "module"
        <+> sep space (fmtName name : ps')
        <> sig'
        <+> code "="
        <+> te'
    multi = do
      doc' <- fmtDocComment doc
      ps' <- mapM fmtModParam ps
      sig' <- fmtSig sig
      te' <- fmtModExp te
      pure $
        doc'
        <> code "module"
        <+> stdNest (sep space (fmtName name : ps')) -- This could be better
        <> sig'
        <+> code "="
        <+> te'
    fmtSig s = case s of
      Nothing -> pure mempty
      Just (s', _f) -> do
        s'' <- fmtModTypeExp s'
        pure $ code ":" <+> s'' <> space

-- All of these should probably be "extra" indented
fmtModExp :: UncheckedModExp -> FmtM Fmt
fmtModExp (ModVar v loc) = buildFmt loc single multi
  where
    single = fmtQualName v
    multi = single
fmtModExp (ModParens f loc) = buildFmt loc single multi
  where
    single = braces <$> fmtModExp f
    multi = (</> code ")") <$> (stdNest . (code "()" </>) <$> fmtModExp f)
fmtModExp (ModImport path _f loc) = buildFmt loc single multi
  where
    single = pure $ code "import \"" <> fmtPretty path <> code "\""
    multi = single
-- Should be put inside a nested block
fmtModExp (ModDecs decs loc) = buildFmt loc single multi
  where
    fmtDecs s = sep s <$> mapM fmtDec decs
    single = braces <$> fmtDecs (line <> line)
    multi = (</> code "}") <$> (stdNest . (code "{" </>) <$> fmtDecs line)
fmtModExp (ModApply f a _f0 _f1 loc) = buildFmt loc single multi
  where
    single = (<+>) <$> fmtModExp f <*> fmtModExp a
    multi = fmap stdNest $ (</>) <$> fmtModExp f <*> fmtModExp a
fmtModExp (ModAscript me se _f loc) = buildFmt loc single multi
  where
    single = do
      me' <- fmtModExp me
      se' <- fmtModTypeExp se
      pure $ me' <> code ":" <+> se'
    multi = single
fmtModExp (ModLambda param maybe_sig body loc) = buildFmt loc single multi
  where
    common = do
      param' <- fmtModParam param
      maybe_sig' <-
        case maybe_sig of
          Nothing -> pure mempty
          Just (sig, _) -> (code ":" <+>) <$> fmtModTypeExp sig
      pure $ code "\\" <> param' <> maybe_sig' <+> code "->"
    single = (<+>) <$> common <*> fmtModExp body
    multi = (</>) <$> common <*> fmtModExp body

-- | Formatting of Futhark declarations.
fmtDec :: UncheckedDec -> FmtM Fmt
fmtDec (ValDec t) = buildFmt t single multi
  where
    single = fmtValBind t -- A value declaration.
    multi = single
fmtDec (TypeDec tb) = buildFmt tb single multi
  where
    single = fmtTypeBind tb -- A type declaration.
    multi = single
fmtDec (ModTypeDec tb) = buildFmt tb single multi
  where
    single = fmtModTypeBind tb -- A module type declation.
    multi = single
fmtDec (ModDec tb) = buildFmt tb single multi
  where
    single = fmtModBind tb -- A module declation.
    multi = single
fmtDec (OpenDec tb loc) = buildFmt loc single multi
  where
    single = do
      tb' <- fmtModExp tb
      pure $ code "open" <+> tb'
    multi = single
-- Adds the local keyword
fmtDec (LocalDec tb loc) = buildFmt loc single multi
  where
    single = do
      tb' <- fmtDec tb
      pure $ code "local" <+> tb'
    multi = single
-- Import declarations.
fmtDec (ImportDec path _tb loc) = buildFmt loc single multi
  where
    single = pure $ code "import \"" <> fmtPretty path <> code "\""
    multi = single

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: UncheckedProg -> FmtM Fmt
fmtProg (Prog dc decs) =  do
  dc' <- fmtDocComment dc
  decs' <- sep (line <> line) <$> mapM fmtDec decs
  cs <- gets (mconcat . fmap (comment . commentText) . comments)
  modify (\s -> s {comments = []})
  pure $ dc' <> decs' <> cs

fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let s = FmtState {comments = cs}
  let e = MultiLine
  let fmt = runIdentity $ evalStateT (runReaderT (fmtProg prog) e) s
  pure $ pretty 80 fmt -- The width does nothing currently.
