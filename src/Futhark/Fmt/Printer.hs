{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Fmt.Printer (fmtText) where

import Data.Foldable
import Data.Text qualified as T
import Futhark.Fmt.Monad
import Language.Futhark
import Language.Futhark.Parser
  ( SyntaxError (..),
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

-- DONE: Fix formatting of several lines of comments
-- DONE: prettyprint in a nicer way than one line per terminal.
--
-- DONE: support all syntactical constructs.

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
instance Format (Maybe DocComment) where
  fmt (Just (DocComment x loc)) =
    prependComments loc $ sep nil $ prefixes (T.lines x)
    where
      prefixes [] = []
      prefixes (l : ls) = comment ("-- | " <> l) : map (comment . ("-- " <>)) ls
  fmt Nothing = nil

fmtFieldType :: (Name, UncheckedTypeExp) -> FmtM Fmt
fmtFieldType (name', t) = fmtName name' <:> code ":" <+> t

fmtParamType :: Maybe Name -> UncheckedTypeExp -> FmtM Fmt
fmtParamType (Just n) te =
  parens $ fmtName n <:> code ":" <+> te
fmtParamType Nothing te = fmt te

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> FmtM Fmt
fmtSumTypeConstr (name, fs) =
  code "#" <:> fmtName name <+> sep space fs

-- | Formatting of Futhark type expressions.
instance Format UncheckedTypeExp where
  fmt (TEVar v loc) = prependComments loc $ fmtQualName v
  fmt (TETuple ts loc) =
    prependComments loc $ parens $ sep (code "," <:> space <|> line <:> code ",") ts
  fmt (TEParens te loc) = prependComments loc $ parens te -- not sure this is correct
  fmt (TERecord fs loc) =
    prependComments loc $ braces $ sep (code "," <:> space <|> line <:> code ",") fields
    where
      fields = fmtFieldType <$> fs
  fmt (TEArray se te loc) = prependComments loc $ se <:> te -- not sure if this can be multi line
  -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
  fmt (TEUnique te loc) = prependComments loc $ code "*" <:> te  -- not sure if this can be multi line
  -- I am not sure I guess applying a higher kinded type to some type expression
  fmt (TEApply te tArgE loc) = prependComments loc  $ te <+> tArgE -- not sure if this can be multi lin
  -- this is "->"
  fmt (TEArrow name te0 te1 loc) =
    prependComments loc $ fmtParamType name te0 <+> code "->" <:+/> softStdIndent te1
  -- This should be "|"
  fmt (TESum tes loc) =
    prependComments loc
    $ sep (code " | " <|> line <:> code "| ")
    $ map fmtSumTypeConstr tes
  fmt (TEDim dims te loc) =
    prependComments loc $ code "?" <:> dims' <:> code "." <:> te -- not sure how to format this as multiple lines
    where
      dims' = sep nil $ map (brackets . fmtName) dims

instance Format (TypeArgExp UncheckedExp Name) where
  fmt (TypeArgExpSize se) = fmt se
  fmt (TypeArgExpType te) = fmt te

instance Format UncheckedTypeBind where 
  fmt (TypeBind name l ps e NoInfo dc loc) =
    prependComments loc $
    dc
    <:> code "type"
    <:> l
    <+> fmtName name
    <:> (if null ps then nil else space)
    <:> localLayoutList ps (align $ sep softline ps)
    <+> code "="
    <:+/> softStdIndent e

instance Format (AttrAtom a) where
  fmt (AtomName name) = fmtName name
  fmt (AtomInt int) = code $ prettyText int

instance Format (AttrInfo a) where
  fmt attr = code "#" <:> brackets (fmtAttrInfo attr)
    where
      fmtAttrInfo (AttrAtom attr' loc) = prependComments loc $ fmt attr'
      fmtAttrInfo (AttrComp name attrs loc) =
        prependComments loc $ fmtName name <:> parens (sep (code ",") $ map fmtAttrInfo attrs)

instance Format Liftedness where
  fmt Unlifted = nil
  fmt SizeLifted = code "~"
  fmt Lifted = code "^"

instance Format UncheckedTypeParam where
  fmt (TypeParamDim name loc) = prependComments loc $ brackets $ fmtName name
  fmt (TypeParamType l name loc) = prependComments loc $ code "'" <:> l <:> fmtName name  

instance Format (UncheckedPat t) where
  fmt (TuplePat pats loc) =
    prependComments loc
    $ parens
    $ sep (code "," <:> space <|> line <:> code ",") pats
  fmt (RecordPat pats loc) =
    prependComments loc
    $ braces
    $ sep (code "," <:> space <|> line <:> code ",") $ map fmtFieldPat pats
    where
      fmtFieldPat (name, t) = fmtName name <+> code "=" <+> t -- Currently it allways adds the fields it seems. I think it has to do this.
  fmt (PatParens pat loc) =
    prependComments loc $ code "(" <:> align pat <:/> code ")"
  fmt (Id name _ loc) = prependComments loc $ fmtNameParen name
  fmt (Wildcard _t loc) = prependComments loc $ code "_"
  fmt (PatAscription pat t loc) = prependComments loc $ pat <:> code ":" <+> t
  fmt (PatLit _e _ loc) = prependComments loc $ fmtCopyLoc loc
  fmt (PatConstr n _ pats loc) =
    prependComments loc
    $ code "#" <:> fmtName n <:+/> align (sep softline pats)
  fmt (PatAttr attr pat loc) = prependComments loc $ attr <+> pat

instance Format (FieldBase NoInfo Name) where
  fmt (RecordFieldExplicit name e loc) =
    prependComments loc $ fmtName name <+> code "=" <:+/> softStdIndent e
  fmt (RecordFieldImplicit name _ loc) = prependComments loc $ fmtName name

instance Format PrimValue where
  fmt (UnsignedValue (Int8Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word8)) <:> code "u8"
  fmt (UnsignedValue (Int16Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word16)) <:> code "u16"
  fmt (UnsignedValue (Int32Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word32)) <:> code "u32"
  fmt (UnsignedValue (Int64Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word64)) <:> code "u64"
  fmt (SignedValue v) = fmtPretty v
  fmt (BoolValue True) = fmt $ code "true"
  fmt (BoolValue False) = fmt $ code "false"
  fmt (FloatValue v) = fmtPretty v

instance Format UncheckedDimIndex where
  fmt (DimFix e) = fmt e
  fmt (DimSlice i j (Just s)) =
    maybe (fmt nil) fmt i
    <:> code ":"
    <:> maybe (fmt nil) fmt j
    <:> code ":"
    <:> fmt s
  fmt (DimSlice i (Just j) s) =
    maybe (fmt nil) fmt i
    <:> code ":"
    <:> fmt j
    <:> maybe nil ((code ":" <:>) . fmt) s
  fmt (DimSlice i Nothing Nothing) =
    maybe (fmt nil) fmt i <:> code ":"

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

instance Format UncheckedExp where
  fmt (Var name _ loc) = prependComments loc $ fmtQualName name
  fmt (Hole _ loc) = prependComments loc $ code "???"
  fmt (Parens e loc) =
    prependComments loc $ code "(" <:> stdNest e <:/> code ")"
  fmt (QualParens (v, _loc) e loc) =
    prependComments loc
    $ fmtQualName v <:> code "." <:> code "(" <:>  align e <:/> code ")"
  fmt (Ascript e t loc) = prependComments loc $ e <:> code ":" <+> t
  fmt (Coerce e t _ loc) = prependComments loc $ e <+> code ":>" <+> t
  fmt (Literal _v loc) = prependComments loc $ fmtCopyLoc loc
  fmt (IntLit _v _ loc) = prependComments loc $ fmtCopyLoc loc
  fmt (FloatLit _v _ loc) = prependComments loc $ fmtCopyLoc loc -- fmtPretty _v -- Not sure how this can be multiline.
  fmt (TupLit es loc) =
    prependComments loc
    $ parens
    $ sep (code "," <:> space <|> line <:> code ",") es
  fmt (RecordLit fs loc) =
    prependComments loc
    $ braces
    $ sep (code "," <:> space <|> line <:> code ",") fs
  fmt (ArrayVal vs _ loc) =
    prependComments loc
    $ brackets
    $ sep (code "," <:> space <|> line <:> code ",") vs
  fmt (ArrayLit es _ loc) =
    prependComments loc
    $ brackets
    $ sep (code "," <:> space <|> line <:> code ",") es
  fmt (StringLit _s loc) = fmtCopyLoc loc
  fmt (Project k e _ loc) = prependComments loc $ e <:> code "." <:> fmtPretty k
  fmt (Negate e loc) = prependComments loc $ code "-" <:> e
  fmt (Not e loc) = prependComments loc $ code "!" <:> e
  fmt (Update src idxs ve loc) =
    prependComments loc
    $ src <+> code "with" <+> idxs' <+> stdNest (code "=" <:+/> ve)
    where
      idxs' = brackets $ sep (code "," <:> space) idxs -- This could account for multiline.
  fmt (RecordUpdate src fs ve _ loc) =
    prependComments loc
    $ src <+> code "with" <+> fs' <+> stdNest (code "=" <:+/> ve)
    where
      fs' = sep (code ".") $ fmtName <$> fs -- This could account for multiline.
  fmt (Assert e1 e2 _ loc) =
    prependComments loc $ code "assert" <+> e1 <+> e2
  fmt (Lambda params body rettype _ loc) =
    prependComments loc
    $ code "\\" <:> sep space params <:> ascript <+> stdNest (code "->" <:+/> body)
    where
      ascript = maybe nil (code ": " <:>) rettype
  fmt (OpSection binop _ loc) = prependComments loc $
    if operatorName (qualLeaf binop)
    then fmtQualName binop
    else parens $ code "`" <:> fmtQualName binop <:> code "`"
  fmt (OpSectionLeft binop _ x _ _ loc) =
    prependComments loc $ parens $ x <+> fmtBinOp binop
  fmt (OpSectionRight binop _ x _ _ loc) =
    prependComments loc $ parens $ fmtBinOp binop <+> x
  fmt (ProjectSection fields _ loc) =
    prependComments loc $ parens $ code "." <:> sep (code ".") (fmtName <$> fields)
  fmt (IndexSection idxs _ loc) =
    prependComments loc $ parens (code "." <:> idxs')
    where
      idxs' = brackets $ sep (code "," <:> space) idxs
  fmt (Constr n cs _ loc) =
    prependComments loc $ code "#" <:> fmtName n <+> align (sep softline cs)
  fmt (Attr attr e loc) = prependComments loc $ align (attr <:+/> e)
  fmt (AppExp e _) = fmt e

-- | This should always be simplified by location.
fmtQualName :: QualName Name -> FmtM Fmt
fmtQualName (QualName names name)
  | operatorName name = parens $ pre <:> fmtName name 
  | otherwise = pre <:> fmtName name
    where 
      pre =
        if null names
          then nil
          else sep (code ".") (map fmtName names) <:> code "."

instance Format UncheckedCase where
  fmt (CasePat p e loc) =
    prependComments loc $ code "case" <+> p <+> code "->" <:+/> softStdIndent e

-- matchPat (TuplePat _pats _paramt) _exp = undefined
-- matchPat (RecordPat _namePats _loc) _exp = undefined
-- matchPat (PatParens _pats _loc) _exp = undefined
-- matchPat (Id _names _fs _loc) _exp = undefined
-- matchPat (Wildcard _f _loc) _exp = False
-- matchPat (PatAscription _pats _tyype _loc) _exp = undefined
-- matchPat (PatLit _lit _f _loc) _exp = undefined
-- matchPat (PatConstr _name _f _pats _loc) _exp = undefined
-- matchPat (PatAttr _info _pat _loc) _exp = undefined

instance Format (AppExpBase NoInfo Name) where
  fmt (BinOp (bop, _) _ (x, _) (y, _) loc) =
    prependComments loc $ x <:+/> fmtBinOp bop <+> y
  fmt (Match e cs loc) =
    prependComments loc $ code "match" <+> e <:+/> sep softline (toList cs)
  -- should omit the initial value expression
  -- need some way to catch when the value expression match the pattern
  fmt (Loop sizeparams pat (LoopInitImplicit NoInfo) form loopbody loc) =
    prependComments loc
    $ ( ( code "loop" `op` sizeparams' )
        <+/> pat
      )
    <+> form
    <+> code "do"
    <:+/> softStdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Loop sizeparams pat (LoopInitExplicit initexp) form loopbody loc) =
    prependComments loc
    $ ( ( code "loop" `op` sizeparams' )
        <+/> pat
        <+> code "="
      )
    <+/> initexp
    <+> form
    <+> code "do"
    <:+/> softStdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Index e idxs loc) =
    prependComments loc
    $ (e <:>)
    $ brackets
    $ sep (code "," <:> space <|> line <:> code ",") idxs
  fmt (LetPat sizes pat e body loc) =
    prependComments loc
    $ ( code "let"
        <+> sepFilter [not $ null sizes, True] space [sizes', fmt pat]
        <+> code "="
      )
    <+/> e
    <:+/> letBody body
    where
      sizes' = sep nil sizes
  fmt (LetFun fname (tparams, params, retdecl, _, e) body loc) =
    prependComments loc
    $ ( code "let"
        <+> fmtName fname
        <:> (if null params && null tparams then nil else space)
        <:> sub
        <:> retdecl'
        <:> code "="
      )
    <+/> e
    <:+/> letBody body
    where
      tparams' = sep space tparams
      params' = sep space params
      retdecl' =
        case retdecl of
          Just a -> code ":" <+> a <:> space
          Nothing -> space
      sub = sepFilter [not $ null tparams, not $ null params] space [tparams', params'] 
  fmt (LetWith dest src idxs ve body loc)
    | dest == src =
      prependComments loc
      $ ( code "let"
          <+> dest
          <:> idxs'
          <+> code "="
        )
      <+/> ve
      <:+/> letBody body
    | otherwise =
      prependComments loc
      $ ( code "let"
          <+> dest
          <+> code "="
          <+> src
          <+> code "with"
          <+> idxs'
        )
      <+/> ve
      <:+/> letBody body
    where
      idxs' = brackets $ sep (code ", ") idxs
  fmt (Range start maybe_step end loc) =
    prependComments loc $ start <:> step <:> end'
    where

      end' =
        case end of
          DownToExclusive e -> code "..>" <:> e
          ToInclusive e -> code "..." <:> e
          UpToExclusive e -> code "..<" <:> e
      step = maybe nil (code ".." <:>) maybe_step
  fmt (If c t f loc) = -- This could be prettier.
    prependComments loc
    $ code "if"
    <+> c
    <+> code "then"
    <:+/> softStdIndent t
    <:+/> code "else"
    <:+/> softStdIndent f
  fmt (Apply f args loc) =
    prependComments loc
    $ f <+> align fmt_args
    where
      fmt_args = sepLoc $ map snd (toList args)

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmt body
letBody body@(AppExp LetFun {} _) = fmt body
letBody body@(AppExp LetWith {} _) = fmt body
letBody body = prependComments body $ code "in" <+> align body

instance Format (SizeBinder Name) where
  fmt (SizeBinder v loc) = prependComments loc $ brackets $ fmtName v

instance Format (IdentBase NoInfo Name t) where
  fmt = fmtPretty . identName

instance Format (LoopFormBase NoInfo Name) where
  fmt (For i ubound) =
    code "for" <+> i <+> code "<" <+> ubound
  fmt (ForIn x e) =
    code "for" <+> x <+> code "in" <+> e
  fmt (While cond) = do
    code "while" <+> cond

-- | This should always be simplified by location.
fmtBinOp :: QualName Name -> FmtM Fmt
fmtBinOp bop =
  case leading of
    Backtick -> code "`" <:> fmtQualName bop <:> code "`"
    _any -> fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

instance Format UncheckedValBind where
  fmt (ValBind entry name retdecl _rettype tparams args body docs attrs loc) =
    prependComments loc
    $ docs
    <:> (if null attrs then nil else attrs' <:> space)
    <:> fun
    <+> fmtNameParen name
    <:> (if null tparams && null args then nil else space)
    <:> sub
    <:> retdecl'
    <:> code "="
    <:+/> softStdIndent body
    where
      attrs' = sep space attrs
      tparams' = localLayoutList tparams $ align $ sep softline tparams
      args' = localLayoutList args $ align $ sep softline args
      retdecl' =
        case retdecl of
          Just a -> code ":" <+> a <:> space
          Nothing -> space
      flags = [not $ null tparams, not $ null args]
      sub =
        sepFilter flags space [tparams', args']
      fun =
        case entry of
          Just _ -> code "entry"
          _any -> code "def"

instance Format (SizeExp UncheckedExp) where
  fmt (SizeExp d loc) = prependComments loc $ brackets d
  fmt (SizeExpAny loc) = prependComments loc $ brackets nil

instance Format UncheckedSpec where
  fmt (TypeAbbrSpec tpsig) = fmt tpsig
  fmt (TypeSpec l name ps doc loc) =
    prependComments loc
    $ doc <:> code "type" <+> l <:> fmtName name <+> align (sep softline ps)
  fmt (ValSpec name ps te _ doc loc) =
    prependComments loc
    $ doc <:> code "val" <+> fmtName name <+> align (sep softline ps) <:> code ":" <+> te
  fmt (ModSpec name mte doc loc) =
    prependComments loc $ doc <:> code "module" <+> fmtName name <:> code ":" <+> mte
  fmt (IncludeSpec mte loc) = prependComments loc $ code "include" <+> mte

instance Format UncheckedModTypeExp where
  fmt (ModTypeVar v _ loc) = prependComments loc $ fmtPretty v
  fmt (ModTypeParens mte loc) =
    prependComments loc $ code "(" <:> align mte <:/> code ")"
  fmt (ModTypeSpecs sbs loc) =
    prependComments loc $ code "{" <:/> softStdIndent (sep softline sbs) <:/> code "}"
  fmt (ModTypeWith mte (TypeRef v ps td _) loc) =
    prependComments loc
    $ mte <+> code "with" <+> fmtPretty v `ps_op` sep space ps <+> code "=" <+> td
    where
      ps_op = if null ps then (<:>) else (<+>)
  fmt (ModTypeArrow (Just v) te0 te1 loc) =
    prependComments loc
    $ parens (fmtName v <:> code ":" <+> te0) <+> align (code "->" <:/> te1)
  fmt (ModTypeArrow Nothing te0 te1 loc) =
    prependComments loc $ te0 <+> code "->" <+> te1

instance Format UncheckedModTypeBind where
  fmt (ModTypeBind pName pSig doc loc) =
    prependComments loc
    $ doc <:> code "module type" <+> fmtName pName <+> code "=" <+> pSig

instance Format (ModParamBase NoInfo Name) where 
  fmt :: ModParamBase NoInfo Name -> FmtM Fmt
  fmt (ModParam pName pSig _f loc) =
    prependComments loc $ parens $ fmtName pName <:> code ":" <+> pSig

instance Format UncheckedModBind where
  fmt (ModBind name ps sig te doc loc) =
    prependComments loc
    $ doc
    <:> code "module"
    <+> fmtName name
    <:> ps'
    <:> sig'
    <:> code "="
    <+> te
    where
      sig' = fmtSig sig
      fmtSig Nothing = space
      fmtSig (Just (s', _f)) = code ":" <+> s' <:> space
      ps' =
        case ps of
          [] -> nil
          _any -> space <:> localLayoutList ps (align $ sep softline ps)

-- All of these should probably be "extra" indented
instance Format UncheckedModExp where
  fmt (ModVar v loc) = prependComments loc $ fmtQualName v
  fmt (ModParens f loc) =
    prependComments loc $ code "(" <:/> softStdIndent f <:/> code ")"
  fmt (ModImport path _f loc) =
    prependComments loc $ code "import \"" <:> fmtPretty path <:> code "\""
  -- Should be put inside a nested block
  fmt (ModDecs decs loc) =
    prependComments loc
    $ code "{" <:+/> softStdIndent (sep (softline <:> softline) decs) <:+/> code "}"
  fmt (ModApply f a _f0 _f1 loc) = prependComments loc $ f <+> a
  fmt (ModAscript me se _f loc) = prependComments loc $ me <:> code ":" <+> se
  fmt (ModLambda param maybe_sig body loc) =
    prependComments loc
    $ code "\\" <:> param <:> sig <+> code "->" <:+/> softStdIndent body
    where
      sig =
        case maybe_sig of
          Nothing -> nil
          Just (sig', _) -> code ":" <+> parens sig'

-- | Formatting of Futhark declarations.
instance Format UncheckedDec where
  fmt (ValDec t) = fmt t -- A value declaration.
  fmt (TypeDec tb) = fmt tb -- A type declaration.
  fmt (ModTypeDec tb) = fmt tb -- A module type declation.
  fmt (ModDec tb) = fmt tb -- A module declation.
  fmt (OpenDec tb loc) = prependComments loc $ code "open" <+> tb -- Adds the local keyword
  fmt (LocalDec tb loc) = prependComments loc $ code "local" <+> tb
  -- Import declarations.
  fmt (ImportDec path _tb loc) =
    prependComments loc $ code "import \"" <:> fmtPretty path <:> code "\""

-- | Does not return residual comments, because these are simply
-- inserted at the end.
instance Format UncheckedProg where
  fmt (Prog dc decs) =
    fmt $ dc <:> sep (softline <:> softline) decs <:+/> popComments
  

fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmt prog
  pure $ pretty $ runFormat m cs fContent
