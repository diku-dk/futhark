{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.CLI.Fmt.Printer (fmtText) where

import Data.Foldable
import Data.Text qualified as T
import Futhark.CLI.Fmt.AST
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

-- TODO: Fix formatting of several lines of comments
-- TODO: prettyprint in a nicer way than one line per terminal.
--
-- DONE: support all syntactical constructs.

-- TODO (Question?): Change fmt to be a sequence of lines instead of a list of lines

fmtName :: Name -> FmtM Fmt
fmtName = simplify . code . nameToText

fmtNameParen :: Name -> FmtM Fmt
fmtNameParen name
  | operatorName name = simplify $ parens $ fmtName name -- (doesn't seem like this needs to always be parenthesized?)
  | otherwise = fmtName name

fmtPretty :: (Pretty a) => a -> FmtM Fmt
fmtPretty = simplify . code . prettyText

-- | Documentation comments are always optional, so this takes a 'Maybe'.
-- TODO: make special documentation comments in Fmt?
-- TODO: Add "--" and "-- |" in pretty printer
instance Format (Maybe DocComment) where
  fmt (Just (DocComment x loc)) =
    simplifyByLoc loc $ sep nil $ prefixes (T.lines x)
    where
      prefixes [] = []
      prefixes (l : ls) = comment ("-- | " <> l) : map (comment . ("-- " <>)) ls
  fmt Nothing = nil

fmtFieldType :: (Name, UncheckedTypeExp) -> FmtM Fmt
fmtFieldType (name', t) = simplify $ fmtName name' <:> code ":" <+> t

fmtParamType :: Maybe Name -> UncheckedTypeExp -> FmtM Fmt
fmtParamType (Just n) te =
  simplify $ parens $ fmtName n <:> code ":" <+> te
fmtParamType Nothing te = fmt te

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> FmtM Fmt
fmtSumTypeConstr (name, fs) =
  simplify $ code "#" <:> fmtName name <+> sep space fs

-- | Formatting of Futhark type expressions.
instance Format UncheckedTypeExp where
  fmt (TEVar v loc) = simplifyByLoc loc $ fmtQualName v
  fmt (TETuple ts loc) =
    simplifyByLoc loc $ parens $ sep (code "," <:> space <|> line <:> code ",") ts
  fmt (TEParens te loc) = simplifyByLoc loc (single <|> multi)
    where
      single = parens te
      multi = single -- not sure this is correct
  fmt (TERecord fs loc) =
    simplifyByLoc loc $ braces $ sep (code "," <:> space <|> line <:> code ",") fields
    where
      fields = fmtFieldType <$> fs
  fmt (TEArray se te loc) = simplifyByLoc loc $ se <:> te -- not sure if this can be multi line
  -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
  fmt (TEUnique te loc) = simplifyByLoc loc $ code "*" <:> te  -- not sure if this can be multi line
  -- I am not sure I guess applying a higher kinded type to some type expression
  fmt (TEApply te tArgE loc) = simplifyByLoc loc  $ te <+> tArgE -- not sure if this can be multi lin
  -- this is "->"
  fmt (TEArrow name te0 te1 loc) =
    simplifyByLoc loc $ fmtParamType name te0 <+> code "->" </> stdIndent te1
  -- This should be "|"
  fmt (TESum tes loc) =
    simplifyByLoc loc
    $ sep (code " | " <|> line <:> code "| ")
    $ map fmtSumTypeConstr tes
  fmt (TEDim dims te loc) =
    simplifyByLoc loc $ code "?" <:> dims' <:> code "." <:> te -- not sure how to format this as multiple lines
    where
      dims' = sep nil $ map (brackets . fmtName) dims

instance Format (TypeArgExp UncheckedExp Name) where
  fmt (TypeArgExpSize se) = fmt se
  fmt (TypeArgExpType te) = fmt te

instance Format UncheckedTypeBind where 
  fmt (TypeBind name l ps e NoInfo dc loc) =
    simplifyByLoc loc $
    dc
    <:> code "type"
    <:> l
    <+> fmtName name
    <:> (if null ps then nil else space)
    <:> simplifyByLocList ps (align $ sep line ps)
    <+> code "="
    </> stdIndent e

instance Format (AttrAtom a) where
  fmt (AtomName name) = fmtName name
  fmt (AtomInt int) = simplify $ code $ prettyText int

instance Format (AttrInfo a) where
  fmt attr = simplify $ code "#" <:> brackets (fmtAttrInfo attr)
    where
      fmtAttrInfo (AttrAtom attr' loc) = simplifyByLoc loc $ fmt attr'
      fmtAttrInfo (AttrComp name attrs loc) =
        simplifyByLoc loc $ fmtName name <:> parens (sep (code ",") $ map fmtAttrInfo attrs)

instance Format Liftedness where
  fmt Unlifted = nil
  fmt SizeLifted = code "~"
  fmt Lifted = code "^"

instance Format UncheckedTypeParam where
  fmt (TypeParamDim name loc) = simplifyByLoc loc $ brackets $ fmtName name
  fmt (TypeParamType l name loc) = simplifyByLoc loc $ code "'" <:> l <:> fmtName name  

instance Format (UncheckedPat t) where
  fmt (TuplePat pats loc) =
    simplifyByLoc loc
    $ parens
    $ sep (code "," <:> space <|> line <:> code ",") pats
  fmt (RecordPat pats loc) =
    simplifyByLoc loc
    $ braces
    $ sep (code "," <:> space <|> line <:> code ",") $ map fmtFieldPat pats
    where
      fmtFieldPat (name, t) = fmtName name <+> code "=" <+> t -- Currently it allways adds the fields it seems. I think it has to do this.
  fmt (PatParens pat loc) =
    simplifyByLoc loc $ code "(" <:> align pat <:/> code ")"
  fmt (Id name _ loc) = simplifyByLoc loc $ fmtNameParen name
  fmt (Wildcard _t loc) = simplifyByLoc loc $ code "_"
  fmt (PatAscription pat t loc) = simplifyByLoc loc $ pat <:> code ":" <+> t
  fmt (PatLit _e _ loc) = simplifyByLoc loc $ fmtCopyLoc loc
  fmt (PatConstr n _ pats loc) =
    simplifyByLoc loc
    $ code "#" <:> fmtName n </> align (sep line pats)
  fmt (PatAttr attr pat loc) = simplifyByLoc loc $ attr <+> pat

instance Format (FieldBase NoInfo Name) where
  fmt (RecordFieldExplicit name e loc) =
    simplifyByLoc loc $ fmtName name <+> code "=" </> stdIndent e
  fmt (RecordFieldImplicit name _ loc) = simplifyByLoc loc $ fmtName name

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
    simplify $
    maybe (fmt nil) fmt i
    <:> code ":"
    <:> maybe (fmt nil) fmt j
    <:> code ":"
    <:> fmt s
  fmt (DimSlice i (Just j) s) =
    simplify $
    maybe (fmt nil) fmt i
    <:> code ":"
    <:> fmt j
    <:> maybe nil ((code ":" <:>) . fmt) s
  fmt (DimSlice i Nothing Nothing) =
    simplify $ maybe (fmt nil) fmt i <:> code ":"

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

instance Format UncheckedExp where
  fmt (Var name _ loc) = simplifyByLoc loc $ fmtQualName name
  fmt (Hole _ loc) = simplifyByLoc loc $ code "???"
  fmt (Parens e loc) =
    simplifyByLoc loc $ code "(" <:> stdNest e <:/> code ")"
  fmt (QualParens (v, _loc) e loc) =
    simplifyByLoc loc
    $ fmtQualName v <:> code "." <:> code "(" <:>  align e <:/> code ")"
  fmt (Ascript e t loc) = simplifyByLoc loc $ e <:> code ":" <+> t
  fmt (Coerce e t _ loc) = simplifyByLoc loc $ e <+> code ":>" <+> t
  fmt (Literal _v loc) = simplifyByLoc loc $ fmtCopyLoc loc
  fmt (IntLit _v _ loc) = simplifyByLoc loc $ fmtCopyLoc loc
  fmt (FloatLit _v _ loc) = simplifyByLoc loc $ fmtCopyLoc loc -- fmtPretty _v -- Not sure how this can be multiline.
  fmt (TupLit es loc) =
    simplifyByLoc loc
    $ parens
    $ sep (code "," <:> space <|> line <:> code ",")  es
  fmt (RecordLit fs loc) =
    simplifyByLoc loc
    $ braces
    $ sep (code "," <:> space <|> line <:> code ",")  fs
  fmt (ArrayVal vs _ loc) =
    simplifyByLoc loc
    $ brackets
    $ sep (code "," <:> space <|> line <:> code ",")  vs
  fmt (ArrayLit es _ loc) =
    simplifyByLoc loc
    $ brackets
    $ sep (code "," <:> space <|> line <:> code ",")  es
  fmt (StringLit _s loc) = fmtCopyLoc loc
  fmt (Project k e _ loc) = simplifyByLoc loc $ e <:> code "." <:> fmtPretty k
  fmt (Negate e loc) = simplifyByLoc loc $ code "-" <:> e
  fmt (Not e loc) = simplifyByLoc loc $ code "!" <:> e
  fmt (Update src idxs ve loc) =
    simplifyByLoc loc
    $ src <+> code "with" <+> idxs' <+> stdNest (code "=" </> ve)
    where
      idxs' = brackets $ sep (code "," <:> space) idxs -- This could account for multiline.
  fmt (RecordUpdate src fs ve _ loc) =
    simplifyByLoc loc
    $ src <+> code "with" <+> fs' <+> stdNest (code "=" </> ve)
    where
      fs' = sep (code ".") $ fmtName <$> fs -- This could account for multiline.
  fmt (Assert e1 e2 _ loc) =
    simplifyByLoc loc $ code "assert" <+> e1 <+> e2
  fmt (Lambda params body rettype _ loc) =
    simplifyByLoc loc
    $ code "\\" <:> sep space params <:> ascript <+> stdNest (code "->" </> body)
    where
      ascript = maybe nil (code ": " <:>) rettype
  fmt (OpSection binop _ loc) = simplifyByLoc loc $ fmtQualName binop
  fmt (OpSectionLeft binop _ x _ _ loc) =
    simplifyByLoc loc $ parens $ x <+> fmtBinOp binop
  fmt (OpSectionRight binop _ x _ _ loc) =
    simplifyByLoc loc $ parens $ fmtBinOp binop <+> x
  fmt (ProjectSection fields _ loc) =
    simplifyByLoc loc $ parens $ code "." <:> sep (code ".") (fmtName <$> fields)
  fmt (IndexSection idxs _ loc) =
    simplifyByLoc loc $ parens (code "." <:> idxs')
    where
      idxs' = brackets $ sep (code "," <:> space) idxs
  fmt (Constr n cs _ loc) =
    simplifyByLoc loc $ code "#" <:> fmtName n <+> align (sep line cs)
  fmt (Attr attr e loc) = simplifyByLoc loc $ align (attr </> e)
  fmt (AppExp e _) = simplify $ fmt e

-- | This should always be simplified by location.
fmtQualName :: QualName Name -> FmtM Fmt
fmtQualName (QualName names name)
  | operatorName name = simplify $ parens $ pre <:> fmtName name 
  | otherwise = simplify $ pre <:> fmtName name
    where 
      pre =
        if null names
          then nil
          else sep (code ".") (map fmtName names) <:> code "."

instance Format UncheckedCase where
  fmt (CasePat p e loc) =
    simplifyByLoc loc $ code "case" <+> p <+> code "->" </> stdIndent e

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

instance Format (AppExpBase NoInfo Name) where
  fmt (BinOp (bop, _) _ (x, _) (y, _) loc) =
    simplifyByLoc loc $ x </> fmtBinOp bop <+> y
  fmt (Match e cs loc) =
    simplifyByLoc loc $ code "match" <+> e </> sep line (toList cs)
  -- should omit the initial value expression
  -- need some way to catch when the value expression match the pattern
  fmt (Loop _sizeparams pat initexp _form _loopbody _loc) | matchPat pat initexp = undefined
  fmt (Loop sizeparams pat initexp form loopbody loc) =
    simplifyByLoc loc
    $ ( ( code "loop" `op` sizeparams' )
        <+/> pat
        <+> code "="
      )
    <+/> initexp
    <+> form
    <+> code "do"
    </> stdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Index e idxs loc) =
    simplifyByLoc loc
    $ (e <:>)
    $ brackets
    $ sep (code "," <:> space <|> line <:> code ",") idxs
  fmt (LetPat sizes pat e body loc) =
    simplifyByLoc loc
    $ ( code "let"
        <+> sepFilter [not $ null sizes, True] space [sizes', fmt pat]
        <+> code "="
      )
    <+/> e
    </> letBody body
    where
      sizes' = sep nil sizes
  fmt (LetFun fname (tparams, params, retdecl, _, e) body loc) =
    simplifyByLoc loc
    $ ( code "let"
        <+> fmtName fname
        <:> (if null params && null tparams then nil else space)
        <:> sub
        <:> retdecl'
        <:> code "="
      )
    <+/> e
    </> letBody body
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
      simplifyByLoc loc
      $ ( code "let"
          <+> dest
          <:> idxs'
          <+> code "="
        )
      <+/> ve
      </> letBody body
    | otherwise =
      simplifyByLoc loc
      $ ( code "let"
          <+> dest
          <+> code "="
          <+> src
          <+> code "with"
          <+> idxs'
        )
      <+/> ve
      </> letBody body
    where
      idxs' = brackets $ sep (code ", ") idxs
  fmt (Range start maybe_step end loc) =
    simplifyByLoc loc $ start <:> step <:> end'
    where

      end' =
        case end of
          DownToExclusive e -> code "..>" <:> e
          ToInclusive e -> code "..." <:> e
          UpToExclusive e -> code "..<" <:> e
      step = maybe nil (code ".." <:>) maybe_step
  fmt (If c t f loc) = -- This could be prettier.
    simplifyByLoc loc
    $ code "if"
    <+> c
    <+> code "then"
    </> stdIndent t
    </> code "else"
    </> stdIndent f
  fmt (Apply f args loc) =
    simplifyByLoc loc
    $ f <+> align fmt_args
    where
      fmt_args = sepLoc $ map snd (toList args)

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmt body
letBody body@(AppExp LetFun {} _) = fmt body
letBody body@(AppExp LetWith {} _) = fmt body
letBody body = simplifyByLoc body $ code "in" <+> align body

instance Format (SizeBinder Name) where
  fmt (SizeBinder v loc) = simplifyByLoc loc $ brackets $ fmtName v

instance Format (IdentBase NoInfo Name t) where
  fmt = fmtPretty . identName

instance Format (LoopFormBase NoInfo Name) where
  fmt (For i ubound) =
    simplify $ code "for" <+> i <+> code "<" <+> ubound
  fmt (ForIn x e) =
    simplify $ code "for" <+> x <+> code "in" <+> e
  fmt (While cond) = do
    simplify $ code "while" <+> cond

-- | This should always be simplified by location.
fmtBinOp :: QualName Name -> FmtM Fmt
fmtBinOp bop =
  case leading of
    Backtick -> simplify $ code "`" <:> fmtQualName bop <:> code "`"
    _any -> fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

instance Format UncheckedValBind where
  fmt (ValBind entry name retdecl _rettype tparams args body docs attrs loc) =
    simplifyByLoc loc
    $ docs
    <:> (if null attrs then nil else attrs' <:> space)
    <:> fun
    <+> fmtNameParen name
    <:> (if null tparams && null args then nil else space)
    <:> sub
    <:> retdecl'
    <:> code "="
    </> stdIndent body
    where
      attrs' = sep space attrs
      tparams' = simplifyByLocList tparams $ align $ sep line tparams
      args' = simplifyByLocList args $ align $ sep line args
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
  fmt (SizeExp d loc) = simplifyByLoc loc $ brackets d
  fmt (SizeExpAny loc) = simplifyByLoc loc $ brackets nil

instance Format UncheckedSpec where
  fmt (TypeAbbrSpec tpsig) = fmt tpsig
  fmt (TypeSpec l name ps doc loc) =
    simplifyByLoc loc
    $ doc <:> code "type" <+> l <:> fmtName name <+> align (sep line ps)
  fmt (ValSpec name ps te _ doc loc) =
    simplifyByLoc loc
    $ doc <:> code "val" <+> fmtName name <+> align (sep line ps) <:> code ":" <+> te
  fmt (ModSpec name mte doc loc) =
    simplifyByLoc loc $ doc <:> code "module" <+> fmtName name <:> code ":" <+> mte
  fmt (IncludeSpec mte loc) = simplifyByLoc loc $ code "include" <+> mte

instance Format UncheckedModTypeExp where
  fmt (ModTypeVar v _ loc) = simplifyByLoc loc $ fmtPretty v
  fmt (ModTypeParens mte loc) =
    simplifyByLoc loc $ code "(" <:> align mte <:/> code ")"
  fmt (ModTypeSpecs sbs loc) =
    simplifyByLoc loc $ code "{" <:/> stdIndent (sep line sbs) <:/> code "}"
  fmt (ModTypeWith mte (TypeRef v ps td _) loc) =
    simplifyByLoc loc
    $ mte <+> code "with" <+> fmtPretty v `ps_op` sep space ps <+> code "=" <+> td
    where
      ps_op = if null ps then (<:>) else (<+>)
  fmt (ModTypeArrow (Just v) te0 te1 loc) =
    simplifyByLoc loc
    $ parens (fmtName v <:> code ":" <+> te0) <+> align (code "->" <:/> te1)
  fmt (ModTypeArrow Nothing te0 te1 loc) =
    simplifyByLoc loc $ te0 <+> code "->" <+> te1

instance Format UncheckedModTypeBind where
  fmt (ModTypeBind pName pSig doc loc) =
    simplifyByLoc loc
    $ doc <:> code "module type" <+> fmtName pName <+> code "=" <+> pSig

instance Format (ModParamBase NoInfo Name) where 
  fmt :: ModParamBase NoInfo Name -> FmtM Fmt
  fmt (ModParam pName pSig _f loc) =
    simplifyByLoc loc $ parens $ fmtName pName <:> code ":" <+> pSig

instance Format UncheckedModBind where
  fmt (ModBind name ps sig te doc loc) =
    simplifyByLoc loc
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
          _any -> space <:> simplifyByLocList ps (align $ sep line ps)

-- All of these should probably be "extra" indented
instance Format UncheckedModExp where
  fmt (ModVar v loc) = simplifyByLoc loc $ fmtQualName v
  fmt (ModParens f loc) =
    simplifyByLoc loc $ code "(" <:/> stdIndent f <:/> code ")"
  fmt (ModImport path _f loc) =
    simplifyByLoc loc $ code "import \"" <:> fmtPretty path <:> code "\""
  -- Should be put inside a nested block
  fmt (ModDecs decs loc) =
    simplifyByLoc loc
    $ code "{" </> stdIndent (sep (space <|> line <:> line) decs) </> code "}"
  fmt (ModApply f a _f0 _f1 loc) = simplifyByLoc loc $ f <+> a
  fmt (ModAscript me se _f loc) = simplifyByLoc loc $ me <:> code ":" <+> se
  fmt (ModLambda param maybe_sig body loc) =
    simplifyByLoc loc
    $ code "\\" <:> param <:> sig <+> code "->" </> stdIndent body
    where
      sig =
        case maybe_sig of
          Nothing -> nil
          Just (sig', _) -> code ":" <+> parens sig'

-- | Formatting of Futhark declarations.
instance Format UncheckedDec where
  fmt (ValDec t) = simplify $ fmt t -- A value declaration.
  fmt (TypeDec tb) = simplify $ fmt tb -- A type declaration.
  fmt (ModTypeDec tb) = simplify $ fmt tb -- A module type declation.
  fmt (ModDec tb) = simplify $ fmt tb -- A module declation.
  fmt (OpenDec tb loc) = simplifyByLoc loc $ code "open" <+> tb -- Adds the local keyword
  fmt (LocalDec tb loc) = simplifyByLoc loc $ code "local" <+> tb
  -- Import declarations.
  fmt (ImportDec path _tb loc) =
    simplifyByLoc loc $ code "import \"" <:> fmtPretty path <:> code "\""

-- | Does not return residual comments, because these are simply
-- inserted at the end.
instance Format UncheckedProg where
  fmt (Prog dc decs) =
    fmt $ dc <:> sep (line <:> line) decs </> popComments
  

fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmt prog
  pure $ pretty $ runFormat m cs fContent
