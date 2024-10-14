module Futhark.CLI.Fmt.Printer (fmtText) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (chr)
import Data.Foldable
import Data.Text qualified as T
import Futhark.CLI.Fmt.AST
import Futhark.Util.Loc
import Language.Futhark
import Language.Futhark.Parser
  ( Comment (..),
    SyntaxError (..),
    parseFutharkWithComments,
  )
import Prettyprinter.Internal (Pretty)

{-
import Debug.Trace

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

fmtName :: Name -> FmtM Fmt
fmtName = code . nameToText

fmtNameParen :: Name -> FmtM Fmt
fmtNameParen name
  | operatorName name = parens $ fmtName name -- (doesn't seem like this needs to always be parenthesized?)
  | otherwise = fmtName name

fmtPretty :: (Pretty a) => a -> FmtM Fmt
fmtPretty = code . prettyText

-- | Documentation comments are always optional, so this takes a 'Maybe'.
-- TODO: make special documentation comments in Fmt?
-- TODO: Add "--" and "-- |" in pretty printer
fmtDocComment :: Maybe DocComment -> FmtM Fmt
fmtDocComment (Just (DocComment x _loc)) =
  sep nil $ prefixes (T.lines x)
  where
    prefixes [] = []
    prefixes (l : ls) = comment ("-- | " <> l) : map (comment . ("-- " <>)) ls
fmtDocComment Nothing = nil

fmtFieldType :: (Name, UncheckedTypeExp) -> FmtM Fmt
fmtFieldType (name', t) = fmtName name' <:> code ":" <+> fmtTypeExp t

fmtParamType :: Maybe Name -> UncheckedTypeExp -> FmtM Fmt
fmtParamType (Just n) te =
  parens $ fmtName n <:> code ":" <+> fmtTypeExp te
fmtParamType Nothing te = fmtTypeExp te

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> FmtM Fmt
fmtSumTypeConstr (name, fs) =
  code "#" <:> fmtName name <+> sep space (map fmtTypeExp fs)

-- | Formatting of Futhark type expressions.
fmtTypeExp :: UncheckedTypeExp -> FmtM Fmt
fmtTypeExp (TEVar v loc) = buildFmt loc single multi
  where
    single = fmtQualNameSingle v
    multi = fmtQualNameMulti v
fmtTypeExp (TETuple ts loc) = buildFmt loc single multi
  where
    single = parens $ sepSpace (code ",") $ map fmtTypeExp ts
    multi = parens $ sepLine (code ",") $ map fmtTypeExp ts
fmtTypeExp (TEParens te loc) = buildFmt loc single multi
  where
    single = parens $ fmtTypeExp te
    multi = single -- not sure this is correct
fmtTypeExp (TERecord fs loc) = buildFmt loc single multi
  where
    single = braces $ sepSpace (code ",") $ map fmtFieldType fs
    multi = braces $ sepLine (code ",") $ map fmtFieldType fs
fmtTypeExp (TEArray se te loc) = buildFmt loc single multi -- A array with an size expression
  where
    single = fmtSizeExp se <:> fmtTypeExp te
    multi = single -- not sure if this can be multi line
    -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
fmtTypeExp (TEUnique te loc) = buildFmt loc single multi
  where
    single = code "*" <:> fmtTypeExp te
    multi = single -- not sure if this can be multi line
    -- I am not sure I guess applying a higher kinded type to some type expression
fmtTypeExp (TEApply te tArgE loc) = buildFmt loc single multi
  where
    single = fmtTypeExp te <+> fmtArgExp tArgE
    multi = single -- not sure if this can be multi lin
    -- this is "->"
fmtTypeExp (TEArrow name te0 te1 loc) = buildFmt loc single multi
  where
    single =
      fmtParamType name te0 <+> code "->" <+> fmtTypeExp te1
    multi =
      stdNest $ fmtParamType name te0 <+> code "->" </>  fmtTypeExp te1 -- Not sure this is correct.
-- This should be "|"
fmtTypeExp (TESum tes loc) = buildFmt loc single multi
  where
    single = sepSpace (code " |") $ map fmtSumTypeConstr tes
    multi = sepLine (code "| ") $ map fmtSumTypeConstr tes
fmtTypeExp (TEDim dims te loc) = buildFmt loc single multi
  where
    dims' = sep nil $ map (brackets . fmtName) dims
    single = code "?" <:> dims' <:> code "." <:> fmtTypeExp te
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
    ps_op = if null ps then (<:>) else (<+>)
    l' = fmtLiftedness l
    dc' = fmtDocComment dc
    ps' = map fmtTypeParam ps
    single =
      dc'
      <:> code "type"
      <:> l' 
      <+> fmtName name
      <:> code (if null ps then "" else " ")
      `ps_op` sep space ps'
      <+> code "="
      <+> fmtTypeExp e
    multi = do
      dc'
      <:> code "type"
      <:> l'
      <+> fmtName name
      `ps_op` sep space ps'
      <+> stdNest (code "=" </> fmtTypeExp e)

fmtAttrAtom :: AttrAtom a -> FmtM Fmt
fmtAttrAtom (AtomName name) = fmtName name
fmtAttrAtom (AtomInt int) = code $ prettyText int

fmtAttrInfo :: AttrInfo a -> FmtM Fmt
fmtAttrInfo (AttrAtom attr _loc) = fmtAttrAtom attr
fmtAttrInfo (AttrComp name attrs _loc) =
  fmtName name <:> parens (sep (code ",") $ map fmtAttrInfo attrs)

-- I've added smth to make the code parse
fmtAttr :: AttrInfo a -> FmtM Fmt
fmtAttr attr = code "#" <:> brackets (fmtAttrInfo attr)

fmtLiftedness :: Liftedness -> FmtM Fmt
fmtLiftedness Unlifted = nil
fmtLiftedness SizeLifted = code "~"
fmtLiftedness Lifted = code "^"

fmtTypeParam :: UncheckedTypeParam -> FmtM Fmt
fmtTypeParam (TypeParamDim name loc) = buildFmt loc single multi
  where
    single = brackets $ fmtName name
    multi = single
fmtTypeParam (TypeParamType l name loc) = buildFmt loc single multi
  where
    single = code "'" <:> fmtLiftedness l <:> fmtName name
    multi = single

fmtPat :: UncheckedPat t -> FmtM Fmt
fmtPat (TuplePat pats loc) = buildFmt loc single multi
  where
    single = parens $ sepSpace (code ",") $ map fmtPat pats
    multi = parens $ sepLine (code ",") $ map fmtPat pats
fmtPat (RecordPat pats loc) = buildFmt loc single multi
  where
    fmtFieldPat (name, t) = fmtName name <+> code "=" <+> fmtPat t -- Currently it allways adds the fields it seems. I think it has to do this.
    single = braces $ sepSpace (code ",") $ map fmtFieldPat pats
    multi = braces $ sepLine (code ",") $ map fmtFieldPat pats
fmtPat (PatParens pat loc) = buildFmt loc single multi
  where
    single = parens $ fmtPat pat
    multi = stdNest (code "(" </> fmtPat pat) </> code ")"
fmtPat (Id name _ loc) = buildFmt loc single multi
  where
    single = fmtNameParen name
    multi = single
fmtPat (Wildcard _t loc) = buildFmt loc single multi
  where
    single = code "_"
    multi = single
fmtPat (PatAscription pat t loc) = buildFmt loc single multi
  where
    single = fmtPat pat <:> code ":" <+> fmtTypeExp t
    multi = single
fmtPat (PatLit e _ loc) = buildFmt loc single multi
  where
    single = fmtPretty e
    multi = single
fmtPat (PatConstr n _ pats loc) = buildFmt loc single multi
  where
    common s = sep s $ map fmtPat pats
    cons = code "#" <:> fmtName n
    single = cons <+> common space
    multi = stdNest $ cons </> common line
fmtPat (PatAttr attr pat loc) = buildFmt loc single multi
  where
    single = fmtAttr attr <+> fmtPat pat
    multi = single

fmtField :: FieldBase NoInfo Name -> FmtM Fmt
fmtField (RecordFieldExplicit name e loc) = buildFmt loc single multi
  where
    single = fmtName name <+> code "=" <+> fmtExp e
    multi = fmtName name <+> stdNest (code "=" </> fmtExp e)
fmtField (RecordFieldImplicit name _ loc) = buildFmt loc single multi
  where
    single = fmtName name
    multi = single

fmtPrimValue :: PrimValue -> FmtM Fmt
fmtPrimValue (UnsignedValue (Int8Value v)) =
  fmtPretty (show (fromIntegral v :: Word8)) <:> code "u8"
fmtPrimValue (UnsignedValue (Int16Value v)) =
  fmtPretty (show (fromIntegral v :: Word16)) <:> code "u16"
fmtPrimValue (UnsignedValue (Int32Value v)) =
  fmtPretty (show (fromIntegral v :: Word32)) <:> code "u32"
fmtPrimValue (UnsignedValue (Int64Value v)) =
  fmtPretty (show (fromIntegral v :: Word64)) <:> code "u64"
fmtPrimValue (SignedValue v) = fmtPretty v
fmtPrimValue (BoolValue True) = code "true"
fmtPrimValue (BoolValue False) = code "false"
fmtPrimValue (FloatValue v) = fmtPretty v

fmtDimIndex :: UncheckedDimIndex -> FmtM Fmt
fmtDimIndex (DimFix e) = fmtExp e
fmtDimIndex (DimSlice i j (Just s)) = do
  maybe (pure mempty) fmtExp i
  <:> code ":"
  <:> maybe (pure mempty) fmtExp j
  <:> code ":"
  <:> fmtExp s
fmtDimIndex (DimSlice i (Just j) s) = do
  maybe (pure mempty) fmtExp i
  <:> code ":"
  <:> fmtExp j
  <:> maybe (pure mempty) ((code ":" <:>) . fmtExp) s
fmtDimIndex (DimSlice i Nothing Nothing) =
  maybe (pure mempty) fmtExp i <:> code ":"

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

fmtExp :: UncheckedExp -> FmtM Fmt
fmtExp (Var name _ loc) = buildFmt loc single multi
  where
    single = fmtQualNameSingle name
    multi = fmtQualNameMulti name
fmtExp (Hole _ loc) = buildFmt loc single multi
  where
    single = code "???"
    multi = single
fmtExp (Parens e loc) = buildFmt loc single multi
  where
    single = parens $ fmtExp e
    multi = stdNest (code "(" </> fmtExp e) </> code ")"
fmtExp (QualParens (v, _loc) e loc') = buildFmt loc' single multi
  where
    single = fmtQualNameSingle v <:> code "." <:> parens (fmtExp e)
    multi = fmtQualNameMulti v <:> code "." <:> stdNest (code "(" </> fmtExp e) </> code ")"
fmtExp (Ascript e t loc) = buildFmt loc single multi
  where
    single = fmtExp e <:> code ":" <+> fmtTypeExp t
    multi = single
fmtExp (Coerce e t _ loc) = buildFmt loc single multi
  where
    single = fmtExp e <+> code ":>" <+> fmtTypeExp t
    multi = single
fmtExp (Literal v loc) = buildFmt loc single single
  where
    single = fmtPretty v -- Not sure how this can be multiline.
fmtExp (IntLit v _ loc) = buildFmt loc single single
  where
    single = fmtPretty v -- Not sure how this can be multiline.
fmtExp (FloatLit v _ loc) = buildFmt loc single single
  where
    single = fmtPretty v -- Not sure how this can be multiline.
fmtExp (TupLit es loc) = buildFmt loc single multi
  where
    single = parens $ sepSpace (code ",") $ map fmtExp es
    multi = parens $ sepLine (code ",") $ map fmtExp es
fmtExp (RecordLit fs loc) = buildFmt loc single multi
  where
    single = braces $ sepSpace (code ",") $ map fmtField fs
    multi = braces $ sepLine (code ",") $ map fmtField fs
fmtExp (ArrayVal vs _ loc) = buildFmt loc single multi
  where
    single = brackets $ sepSpace (code ",") $ map fmtPrimValue vs
    multi = brackets $ sepLine (code ",") $ map fmtPrimValue vs
fmtExp (ArrayLit es _ loc) = buildFmt loc single multi
  where
    single = brackets $ sepSpace (code ",") $ map fmtExp es
    multi = brackets $ sepLine (code ",") $ map fmtExp es
fmtExp (StringLit s loc) = buildFmt loc single multi
  where
    single = fmtPretty $ show $ fmap (chr . fromIntegral) s
    multi = single
fmtExp (Project k e _ loc) = buildFmt loc single multi
  where
    single = fmtExp e <:> code "." <:> fmtPretty k
    multi = single
fmtExp (Negate e loc) = buildFmt loc single multi
  where
    single = code "-" <:> fmtExp e
    multi = single
fmtExp (Not e loc) = buildFmt loc single multi
  where
    single = code "!" <:> fmtExp e
    multi = single
fmtExp (Update src idxs ve loc) = buildFmt loc single multi
  where
    src' = fmtExp src -- This could account for multiline.
    idxs' = brackets $ sepSpace (code ",") $ map fmtDimIndex idxs -- This could account for multiline.
    common = src' <+> code "with" <+> idxs'
    single = common <+> code "=" <+> fmtExp ve
    multi = common <+> stdNest (code "=" </> fmtExp ve)
fmtExp (RecordUpdate src fs ve _ loc) = buildFmt loc single multi
  where
    src' = fmtExp src -- This could account for multiline.
    fs' = sep (code ".") $ fmtName <$> fs -- This could account for multiline.
    common = src' <+> code "with" <+> fs'
    single = common <+> code "=" <+> fmtExp ve
    multi = common <+> stdNest (code "=" </> fmtExp ve)
fmtExp (Assert e1 e2 _ loc) = buildFmt loc single multi
  where
    single = code "assert" <+> fmtExp e1 <+> fmtExp e2
    multi = single -- This needs to be multiline.
fmtExp (Lambda params body rettype _ loc) = buildFmt loc single multi
  where
    params' = sep space $ map fmtPat params
    ascript = maybe (pure mempty) ((code ": " <:>) . fmtTypeExp) rettype
    common = code "\\" <:> params' <:> ascript
    single = common <+> code "->" <+> fmtExp body
    multi = common <+> stdNest (code "->" </> fmtExp body)
fmtExp (OpSection binop _ loc) = buildFmt loc single multi
  where
    single = fmtQualNameSingle binop
    multi = fmtQualNameMulti binop
fmtExp (OpSectionLeft binop _ x _ _ loc) = buildFmt loc single multi
  where
    single = parens $ fmtExp x <+> fmtBinOp binop  
    multi = single
fmtExp (OpSectionRight binop _ x _ _ loc) = buildFmt loc single multi
  where
    single = parens $ fmtBinOp binop <+> fmtExp x
    multi = single
fmtExp (ProjectSection fields _ loc) = buildFmt loc single multi
  where
    single = parens $ code "." <:> sep (code ".") (fmtName <$> fields)
    multi = single
fmtExp (IndexSection idxs _ loc) = buildFmt loc single multi
  where
    idxs' = brackets $ sepSpace (code ",") $ map fmtDimIndex idxs
    single = parens (code "." <:> idxs')
    multi = single
fmtExp (Constr n cs _ loc) = buildFmt loc single multi
  where
    cons = code "#" <:> fmtName n
    common s = sep s $ map fmtExp cs
    single = cons <+> common space
    multi = stdNest $ cons </> common line
fmtExp (Attr attr e loc) = buildFmt loc single multi
  where
    single = fmtAttr attr <+> fmtExp e
    multi = stdNest (fmtAttr attr </> fmtExp e)
fmtExp (AppExp e _loc) = buildFmt e single multi
  where
    single = fmtAppExp e
    multi = single

fmtQualNameSingle :: QualName Name -> FmtM Fmt
fmtQualNameSingle (QualName names name)
  | operatorName name = parens $ pre <:> fmtName name 
  | otherwise = pre <:> fmtName name
    where
      pre =
        if null names
          then nil
          else sep (code ".") (map fmtName names) <:> code "."

fmtQualNameMulti :: QualName Name -> FmtM Fmt
fmtQualNameMulti (QualName names name)
  | operatorName name = parens $ pre <:> fmtName name 
  | otherwise = pre <:> fmtName name
    where
      pre =
        if null names
          then nil
          else sep (code ".") (map fmtName names) <:> code "."

fmtCase :: UncheckedCase -> FmtM Fmt
fmtCase (CasePat p e loc) = buildFmt loc single multi
  where
    single = code "case" <+> fmtPat p <+> code "->" <+> fmtExp e
    multi = code "case" <+> fmtPat p <+> stdNest (code "->" </> fmtExp e)

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
    single = fmtExp x <+> fmtBinOp bop <+> fmtExp y
    multi = fmtExp x </> fmtBinOp bop <+> fmtExp y
fmtAppExp (Match e cs loc) = buildFmt loc single multi
  where
    multiCases = sep line $ map fmtCase (toList cs)
    singleCases = sep space $ map fmtCase (toList cs)
    single = code "match" <+> fmtExp e <+> singleCases
    multi = code "match" <+> fmtExp e </> multiCases
-- should omit the initial value expression
-- need some way to catch when the value expression match the pattern
fmtAppExp (Loop sizeparams pat initexp form loopbody loc) | matchPat pat initexp = buildFmt loc single multi
  where
    op = if null sizeparams then (<:>) else (<+>)
    sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
    common =
      sepByLayout
      pat
      ( code "loop" `op` sizeparams'
      )
      (fmtPat pat)
      <+> fmtLoopForm form
    single = common <+> code "do" <+> fmtExp loopbody
    multi = common <+> stdNest (code "do" </> fmtExp loopbody)
fmtAppExp (Loop sizeparams pat initexp form loopbody loc) = buildFmt loc single multi
  where
    op = if null sizeparams then (<:>) else (<+>)
    sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
    common =
      sepByLayout
      initexp
      ( sepByLayout
        pat
        ( code "loop" `op` sizeparams' )
        (fmtPat pat)
        <+> code "="
      )
      (fmtExp initexp)
      <+> fmtLoopForm form
    single = common <+> code "do" <+> fmtExp loopbody
    multi = common <+> stdNest (code "do" </> fmtExp loopbody)
fmtAppExp (Index e idxs loc) = buildFmt loc single multi
  where
    idxs' = map fmtDimIndex idxs
    aux f = brackets $ f (code ",") idxs'
    single = fmtExp e <:> aux sepSpace
    multi = fmtExp e <:> aux sepLine
fmtAppExp (LetPat sizes pat e body loc) = buildFmt loc single multi
  where
    sizes' = sep nil $ fmtSizeBinder <$> sizes
    common = 
      sepByLayout
      e
      ( code "let"
        <+> sepNonEmpty space [sizes', fmtPat pat]
        <+> code "="
      )
      (fmtExp e)
    single = common <+> letBody body
    multi = common </> letBody body
fmtAppExp (LetFun fname (tparams, params, retdecl, _, e) body loc) = buildFmt loc single multi
  where
    tparams' = sep space $ map fmtTypeParam tparams
    params' = sep space $ map fmtPat params
    retdecl' =
      case fmtTypeExp <$> retdecl of
        Just a -> code ":" <+> a <:> space
        Nothing -> space
    sub = sepNonEmpty space [tparams', params']
    common =
      sepByLayout
      e
      ( code "let"
        <+> fmtName fname
        <:> (if null params && null tparams then nil else space)
        <:> sub
        <:> retdecl'
        <:> code "="
      )
      (fmtExp e)
    single = common <+> letBody body
    multi = common </> letBody body
fmtAppExp (LetWith dest src idxs ve body loc)
  | dest == src = buildFmt loc singleSame multiSame
  | otherwise = buildFmt loc singleDiff multiDiff
  where
    dest' = fmtIdent dest
    src' = fmtIdent src
    idxs' = brackets . sep (code ", ") $ map fmtDimIndex idxs
    ve' = fmtExp ve
    commonSame =
      sepByLayout
      ve
      ( code "let"
        <+> dest'
        <:> idxs'
        <+> code "="
      )
      ve'
    singleSame = commonSame <+> letBody body
    multiSame = commonSame </> letBody body
    commonDiff =
      sepByLayout
      ve
      ( code "let"
        <+> dest'
        <+> code "="
        <+> src'
        <+> code "with"
        <+> idxs'
      )
      ve'
    singleDiff = commonDiff <+> letBody body
    multiDiff = commonDiff </> letBody body
fmtAppExp (Range start maybe_step end loc) = buildFmt loc single multi
  where
    
    end' =
      case end of
        DownToExclusive e -> code "..>" <:> fmtExp e
        ToInclusive e -> code "..." <:> fmtExp e
        UpToExclusive e -> code "..<" <:> fmtExp e
    step = maybe (pure mempty) ((code ".." <:>) . fmtExp) maybe_step
    single = fmtExp start <:> step <:> end'
    multi = single
fmtAppExp (If c t f loc) = buildFmt loc single multi
  where
    single =
      code "if"
      <+> fmtExp c
      <+> code "then"
      <+> fmtExp t
      <+> code "else"
      <+> fmtExp f
    multi = do
      -- This should handle chained if expressions better.
      code "if"
        <+> fmtExp c
        <+> code "then"
        </> fmtExp t
        </> code "else"
        </> fmtExp f
fmtAppExp (Apply f args loc) = buildFmt loc single multi
  where
    mArgs = sep space $ map (fmtExp . snd) (toList args)
    single = fmtExp f <+> mArgs
    multi = stdNest $ fmtExp f </> mArgs -- This should format in a pretty way but I am not sure how.

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmtExp body
letBody body@(AppExp LetFun {} _) = fmtExp body
letBody body@(AppExp LetWith {} _) = fmtExp body
letBody body = buildFmt body single multi
  where
    single = code "in" <+> fmtExp body
    multi = stdNest (code "in" </> fmtExp body)

fmtSizeBinder :: SizeBinder Name -> FmtM Fmt
fmtSizeBinder (SizeBinder v _) = brackets $ fmtName v

fmtIdent :: IdentBase NoInfo Name t -> FmtM Fmt
fmtIdent = fmtPretty . identName

fmtLoopForm :: LoopFormBase NoInfo Name -> FmtM Fmt
fmtLoopForm (For i ubound) =
  code "for" <+> fmtIdent i <+> code "<" <+> fmtExp ubound
fmtLoopForm (ForIn x e) =
  code "for" <+> fmtPat x <+> code "in" <+> fmtExp e
fmtLoopForm (While cond) = do
  code "while" <+> fmtExp cond

fmtBinOp :: QualName Name -> FmtM Fmt
fmtBinOp bop =
  case leading of
    Backtick -> code "`" <:> fmtQualNameSingle bop <:> code "`"
    _any -> fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

fmtValBind :: UncheckedValBind -> FmtM Fmt
fmtValBind (ValBind entry name retdecl _rettype tparams args body docs attrs loc) = buildFmt loc single multi
  where
    docs' = fmtDocComment docs
    attrs' = sep space $ map fmtAttr attrs
    tparams' = sep space $ map fmtTypeParam tparams
    args' = sep space $ map fmtPat args
    retdecl' =
      case fmtTypeExp <$> retdecl of
        Just a -> code ":" <+> a <:> space
        Nothing -> space
    sub = sepNonEmpty space [tparams', args']
    common =
      docs'
      <:> (if null attrs then nil else attrs' <:> space)
      <:> fun
      <+> fmtNameParen name
      <:> (if null tparams && null args then nil else space)
      <:> sub
      <:> retdecl'
    single = common <+> code "=" <+> fmtExp body
    multi = common <+> stdNest (code "=" </> fmtExp body)
    fun =
      case entry of
        Just _ -> code "entry"
        _any -> code "def"

fmtSizeExp :: SizeExp UncheckedExp -> FmtM Fmt
fmtSizeExp (SizeExp d loc) = buildFmt loc single multi
  where
    single = brackets $ fmtExp d
    multi = single
fmtSizeExp (SizeExpAny loc) = buildFmt loc single multi
  where
    single = brackets nil
    multi = single

fmtSpecBase :: UncheckedSpec -> FmtM Fmt
fmtSpecBase (TypeAbbrSpec tpsig) = fmtTypeBind tpsig
fmtSpecBase (TypeSpec l name ps doc loc) = buildFmt loc single multi
  where
    common = fmtDocComment doc <:> code "type" <:> fmtLiftedness l
    ps' = map fmtTypeParam ps
    single = common <+> sep space (fmtName name : ps')
    multi = common <+> stdNest (sep line (fmtName name : ps')) -- This could be prettier.
fmtSpecBase (ValSpec name ps te _ doc loc) = buildFmt loc single multi
  where
    doc' = fmtDocComment doc
    ps' = map fmtTypeParam ps
    te' = fmtTypeExp te
    single = doc' <:> code "val" <+> sep space (fmtName name : ps') <:> code ":" <+> te'
    multi = single
fmtSpecBase (ModSpec name mte doc loc) = buildFmt loc single multi
  where
    single = fmtDocComment doc <:> code "module" <+> fmtName name <:> code ":" <+> fmtModTypeExp mte
    multi = single
fmtSpecBase (IncludeSpec mte loc) = buildFmt loc single multi
  where
    single = code "include" <+> fmtModTypeExp mte
    multi = single

fmtModTypeExp :: UncheckedModTypeExp -> FmtM Fmt
fmtModTypeExp (ModTypeVar v _ loc) = buildFmt loc single multi
  where
    single = fmtPretty v
    multi = single
fmtModTypeExp (ModTypeParens mte loc) = buildFmt loc single multi
  where
    common = fmtModTypeExp mte
    single = parens $ fmtModTypeExp mte
    multi = stdNest (code "(" </> common) </> code ")"
fmtModTypeExp (ModTypeSpecs sbs loc) = buildFmt loc single multi
  where
    common s = sep s $ map fmtSpecBase sbs
    single = braces $ common space
    multi = stdNest (code "{" </> common line) </> code "}"
fmtModTypeExp (ModTypeWith mte (TypeRef v ps td _) loc) = buildFmt loc single multi
  where
    ps_op = if null ps then (<:>) else (<+>)
    ps' = map fmtTypeParam ps
    common = fmtModTypeExp mte <+> code "with" <+> fmtPretty v `ps_op` sep space ps'
    single = common <+> code "=" <+> fmtTypeExp td
    multi = common <+> stdNest (code "=" <+> fmtTypeExp td)
fmtModTypeExp (ModTypeArrow (Just v) te0 te1 loc) = buildFmt loc single multi
  where
    op a b = parens (fmtName v <:> code ":" <+> a) <+> code "->" <+> b
    single = fmtModTypeExp te0 `op` fmtModTypeExp te1
    multi = single
fmtModTypeExp (ModTypeArrow Nothing te0 te1 loc) = buildFmt loc single multi
  where
    single = fmtModTypeExp te0 <+> code "->" <+> fmtModTypeExp te1
    multi = single

fmtModTypeBind :: UncheckedModTypeBind -> FmtM Fmt
fmtModTypeBind (ModTypeBind pName pSig doc loc) = buildFmt loc single multi
  where
    op = if isBracesOrParens pSig then (<+>) else (</>)
    doc' = fmtDocComment doc
    common = doc' <:> code "module type" <+> fmtName pName <+> code "="
    single = common <+> fmtModTypeExp pSig
    multi = common `op` fmtModTypeExp pSig

isBracesOrParens :: UncheckedModTypeExp -> Bool
isBracesOrParens (ModTypeSpecs _ _) = True
isBracesOrParens (ModTypeParens _ _) = True
isBracesOrParens _ = False

fmtModParam :: ModParamBase NoInfo Name -> FmtM Fmt
fmtModParam (ModParam pName pSig _f loc) = buildFmt loc single multi
  where
    single = parens $ fmtName pName <:> code ":" <+> fmtModTypeExp pSig
    multi = single

fmtModBind :: UncheckedModBind -> FmtM Fmt
fmtModBind (ModBind name ps sig te doc loc) = buildFmt loc single multi
  where
    doc' = fmtDocComment doc
    ps' = map fmtModParam ps
    sig' = fmtSig sig
    te' = fmtModExp te
    single =
      doc'
      <:> code "module"
      <+> sep space (fmtName name : ps')
      <:> sig'
      <:> code "="
      <+> te'
    multi = 
      doc'
      <:> code "module"
      <+> stdNest (sep space (fmtName name : ps')) -- This could be better
      <:> sig'
      <:> code "="
      <+> te'
    fmtSig Nothing = space
    fmtSig (Just (s', _f)) = code ":" <+> fmtModTypeExp s' <:> space

-- All of these should probably be "extra" indented
fmtModExp :: UncheckedModExp -> FmtM Fmt
fmtModExp (ModVar v loc) = buildFmt loc single multi
  where
    single = fmtQualNameSingle v
    multi = fmtQualNameMulti v
fmtModExp (ModParens f loc) = buildFmt loc single multi
  where
    single = parens $ fmtModExp f
    multi = stdNest (code "(" </> fmtModExp f) </> code ")"
fmtModExp (ModImport path _f loc) = buildFmt loc single multi
  where
    single = code "import \"" <:> fmtPretty path <:> code "\""
    multi = single
-- Should be put inside a nested block
fmtModExp (ModDecs decs loc) = buildFmt loc single multi
  where
    fmtDecs s = sep s $ map fmtDec decs
    single = braces $ fmtDecs space
    multi = stdNest (code "{" </> fmtDecs (line <:> line)) </> code "}"
fmtModExp (ModApply f a _f0 _f1 loc) = buildFmt loc single multi
  where
    single = fmtModExp f <+> fmtModExp a
    multi = stdNest $ fmtModExp f </> fmtModExp a
fmtModExp (ModAscript me se _f loc) = buildFmt loc single multi
  where
    single = fmtModExp me <:> code ":" <+> fmtModTypeExp se
    multi = single
fmtModExp (ModLambda param maybe_sig body loc) = buildFmt loc single multi
  where
    param' = fmtModParam param
    maybe_sig' =
      case maybe_sig of
        Nothing -> nil
        Just (sig, _) -> code ":" <+> parens (fmtModTypeExp sig)
    common = code "\\" <:> param' <:> maybe_sig'
    single = common <+> code "->" <+> fmtModExp body
    multi = common <+> stdNest (code "->" </> fmtModExp body)

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
    single = code "open" <+> fmtModExp tb
    multi = single
-- Adds the local keyword
fmtDec (LocalDec tb loc) = buildFmt loc single multi
  where
    single = code "local" <+> fmtDec tb
    multi = single
-- Import declarations.
fmtDec (ImportDec path _tb loc) = buildFmt loc single multi
  where
    single = code "import \"" <:> fmtPretty path <:> code "\""
    multi = single

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: UncheckedProg -> FmtM Fmt
fmtProg (Prog dc decs) =
  fmtDocComment dc <:> decs' <:> popComments
  where
    decs' = sep (line <:> line) $ map fmtDec decs
fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmtProg prog
  pure $ pretty $ runFormat m cs
