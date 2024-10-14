{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.CLI.Fmt.Printer (fmtText) where

import Data.Char (chr)
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
  fmt (Just (DocComment x _loc)) =
    sep nil $ prefixes (T.lines x)
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
  fmt (TEVar v loc) = buildFmt loc single multi
    where
      single = fmtQualNameSingle v
      multi = fmtQualNameMulti v
  fmt (TETuple ts loc) = buildFmt loc single multi
    where
      single = parens $ sepSpace (code ",") ts
      multi = parens $ sepLine (code ",") ts
  fmt (TEParens te loc) = buildFmt loc single multi
    where
      single = parens te
      multi = single -- not sure this is correct
  fmt (TERecord fs loc) = buildFmt loc single multi
    where
      fields = fmtFieldType <$> fs
      single = braces $ sepSpace (code ",") fields 
      multi = braces $ sepLine (code ",") fields
  fmt (TEArray se te loc) = buildFmt loc single multi -- A array with an size expression
    where
      single = se <:> te
      multi = single -- not sure if this can be multi line
      -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
  fmt (TEUnique te loc) = buildFmt loc single multi
    where
      single = code "*" <:> te
      multi = single -- not sure if this can be multi line
      -- I am not sure I guess applying a higher kinded type to some type expression
  fmt (TEApply te tArgE loc) = buildFmt loc single multi
    where
      single = te <+> tArgE
      multi = single -- not sure if this can be multi lin
      -- this is "->"
  fmt (TEArrow name te0 te1 loc) = buildFmt loc single multi
    where
      single =
        fmtParamType name te0 <+> code "->" <+> te1
      multi =
        fmtParamType name te0 <+> code "->" </> stdIndent te1
        -- This should be "|"
  fmt (TESum tes loc) = buildFmt loc single multi
    where
      single = sepSpace (code " |") $ map fmtSumTypeConstr tes
      multi = sepLine (code "| ") $ map fmtSumTypeConstr tes
  fmt (TEDim dims te loc) = buildFmt loc single multi
    where
      dims' = sep nil $ map (brackets . fmtName) dims
      single = code "?" <:> dims' <:> code "." <:> te
      multi = single -- not sure how to format this as multiple lines

instance Format (TypeArgExp UncheckedExp Name) where
  fmt (TypeArgExpSize se) = buildFmt se single multi
    where
      single = fmt se
      multi = single
  fmt (TypeArgExpType te) = buildFmt te single multi
    where
      single = fmt te
      multi = single

instance Format UncheckedTypeBind where 
  fmt (TypeBind name l ps e NoInfo dc loc) = buildFmt loc single multi
    where
      ps_op = if null ps then (<:>) else (<+>)
      single =
        dc
        <:> code "type"
        <:> l
        <+> fmtName name
        <:> code (if null ps then "" else " ")
        `ps_op` sep space ps
        <+> code "="
        <+> e
      multi = do
        dc
        <:> code "type"
        <:> l
        <+> fmtName name
        `ps_op` sep space ps
        <+> code "="
        </> stdIndent e

instance Format (AttrAtom a) where
  fmt (AtomName name) = fmtName name
  fmt (AtomInt int) = code $ prettyText int

instance Format (AttrInfo a) where
  fmt attr = code "#" <:> brackets (fmtAttrInfo attr)
    where
      fmtAttrInfo (AttrAtom attr' _loc) = fmt attr'
      fmtAttrInfo (AttrComp name attrs _loc) =
        fmtName name <:> parens (sep (code ",") $ map fmtAttrInfo attrs)

instance Format Liftedness where
  fmt Unlifted = nil
  fmt SizeLifted = code "~"
  fmt Lifted = code "^"

instance Format UncheckedTypeParam where
  fmt (TypeParamDim name loc) = buildFmt loc single multi
    where
      single = brackets $ fmtName name
      multi = single
  fmt (TypeParamType l name loc) = buildFmt loc single multi
    where
      single = code "'" <:> l <:> fmtName name
      multi = single  

instance Format (UncheckedPat t) where
  fmt (TuplePat pats loc) = buildFmt loc single multi
    where
      single = parens $ sepSpace (code ",") pats
      multi = parens $ sepLine (code ",") pats
  fmt (RecordPat pats loc) = buildFmt loc single multi
    where
      fmtFieldPat (name, t) = fmtName name <+> code "=" <+> t -- Currently it allways adds the fields it seems. I think it has to do this.
      single = braces $ sepSpace (code ",") $ map fmtFieldPat pats
      multi = braces $ sepLine (code ",") $ map fmtFieldPat pats
  fmt (PatParens pat loc) = buildFmt loc single multi
    where
      single = parens pat
      multi = align (code "(" <:> pat) </> code ")"
  fmt (Id name _ loc) = buildFmt loc single multi
    where
      single = fmtNameParen name
      multi = single
  fmt (Wildcard _t loc) = buildFmt loc single multi
    where
      single = code "_"
      multi = single
  fmt (PatAscription pat t loc) = buildFmt loc single multi
    where
      single = pat <:> code ":" <+> t
      multi = single
  fmt (PatLit e _ loc) = buildFmt loc single multi
    where
      single = fmtPretty e
      multi = single
  fmt (PatConstr n _ pats loc) = buildFmt loc single multi
    where
      common s = sep s pats
      cons = code "#" <:> fmtName n
      single = cons <+> common space
      multi =  cons </> align (common line)
  fmt (PatAttr attr pat loc) = buildFmt loc single multi
    where
      single = attr <+> pat
      multi = single

instance Format (FieldBase NoInfo Name) where
  fmt (RecordFieldExplicit name e loc) = buildFmt loc single multi
    where
      single = fmtName name <+> code "=" <+> e
      multi = fmtName name <+> code "=" </> stdIndent e
  fmt (RecordFieldImplicit name _ loc) = buildFmt loc single multi
    where
      single = fmtName name
      multi = single

instance Format PrimValue where
  fmt (UnsignedValue (Int8Value v)) =
    fmtPretty (show (fromIntegral v :: Word8)) <:> code "u8"
  fmt (UnsignedValue (Int16Value v)) =
    fmtPretty (show (fromIntegral v :: Word16)) <:> code "u16"
  fmt (UnsignedValue (Int32Value v)) =
    fmtPretty (show (fromIntegral v :: Word32)) <:> code "u32"
  fmt (UnsignedValue (Int64Value v)) =
    fmtPretty (show (fromIntegral v :: Word64)) <:> code "u64"
  fmt (SignedValue v) = fmtPretty v
  fmt (BoolValue True) = code "true"
  fmt (BoolValue False) = code "false"
  fmt (FloatValue v) = fmtPretty v

instance Format UncheckedDimIndex where
  fmt (DimFix e) = fmt e
  fmt (DimSlice i j (Just s)) = do
    maybe nil fmt i
    <:> code ":"
    <:> maybe nil fmt j
    <:> code ":"
    <:> s
  fmt (DimSlice i (Just j) s) = do
    maybe nil fmt i
    <:> code ":"
    <:> fmt j
    <:> maybe nil (code ":" <:>) s
  fmt (DimSlice i Nothing Nothing) =
    maybe nil fmt i <:> code ":"

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

instance Format UncheckedExp where
  fmt (Var name _ loc) = buildFmt loc single multi
    where
      single = fmtQualNameSingle name
      multi = fmtQualNameMulti name
  fmt (Hole _ loc) = buildFmt loc single multi
    where
      single = code "???"
      multi = single
  fmt (Parens e loc) = buildFmt loc single multi
    where
      single = parens e
      multi = align (code "(" <:> e) </> code ")"
  fmt (QualParens (v, _loc) e loc') = buildFmt loc' single multi
    where
      single = fmtQualNameSingle v <:> code "." <:> parens e
      multi = fmtQualNameMulti v <:> code "." <:> align (code "(" </> e) </> code ")"
  fmt (Ascript e t loc) = buildFmt loc single multi
    where
      single = e <:> code ":" <+> t
      multi = single
  fmt (Coerce e t _ loc) = buildFmt loc single multi
    where
      single = e <+> code ":>" <+> t
      multi = single
  fmt (Literal v loc) = buildFmt loc single single
    where
      single = fmtPretty v -- Not sure how this can be multiline.
  fmt (IntLit v _ loc) = buildFmt loc single single
    where
      single = fmtPretty v -- Not sure how this can be multiline.
  fmt (FloatLit v _ loc) = buildFmt loc single single
    where
      single = fmtPretty v -- Not sure how this can be multiline.
  fmt (TupLit es loc) = buildFmt loc single multi
    where
      single = parens $ sepSpace (code ",") es
      multi = parens $ sepLine (code ",") es
  fmt (RecordLit fs loc) = buildFmt loc single multi
    where
      single = braces $ sepSpace (code ",") fs
      multi = braces $ sepLine (code ",") fs
  fmt (ArrayVal vs _ loc) = buildFmt loc single multi
    where
      single = brackets $ sepSpace (code ",") vs
      multi = brackets $ sepLine (code ",") vs
  fmt (ArrayLit es _ loc) = buildFmt loc single multi
    where
      single = brackets $ sepSpace (code ",") es
      multi = brackets $ sepLine (code ",") es
  fmt (StringLit s loc) = buildFmt loc single multi
    where
      single = fmtPretty $ show $ fmap (chr . fromIntegral) s
      multi = single
  fmt (Project k e _ loc) = buildFmt loc single multi
    where
      single = e <:> code "." <:> fmtPretty k
      multi = single
  fmt (Negate e loc) = buildFmt loc single multi
    where
      single = code "-" <:> e
      multi = single
  fmt (Not e loc) = buildFmt loc single multi
    where
      single = code "!" <:> e
      multi = single
  fmt (Update src idxs ve loc) = buildFmt loc single multi
    where
      idxs' = brackets $ sepSpace (code ",") idxs -- This could account for multiline.
      common = src <+> code "with" <+> idxs'
      single = common <+> code "=" <+> ve
      multi = common <+> stdNest (code "=" </> ve)
  fmt (RecordUpdate src fs ve _ loc) = buildFmt loc single multi
    where
      fs' = sep (code ".") $ fmtName <$> fs -- This could account for multiline.
      common = src <+> code "with" <+> fs'
      single = common <+> code "=" <+> ve
      multi = common <+> stdNest (code "=" </> ve)
  fmt (Assert e1 e2 _ loc) = buildFmt loc single multi
    where
      single = code "assert" <+> e1 <+> e2
      multi = single -- This needs to be multiline.
  fmt (Lambda params body rettype _ loc) = buildFmt loc single multi
    where
      ascript = maybe nil (code ": " <:>) rettype
      common = code "\\" <:> sep space params <:> ascript
      single = common <+> code "->" <+> body
      multi = common <+> stdNest (code "->" </> body)
  fmt (OpSection binop _ loc) = buildFmt loc single multi
    where
      single = fmtQualNameSingle binop
      multi = fmtQualNameMulti binop
  fmt (OpSectionLeft binop _ x _ _ loc) = buildFmt loc single multi
    where
      single = parens $ x <+> fmtBinOp binop  
      multi = single
  fmt (OpSectionRight binop _ x _ _ loc) = buildFmt loc single multi
    where
      single = parens $ fmtBinOp binop <+> x
      multi = single
  fmt (ProjectSection fields _ loc) = buildFmt loc single multi
    where
      single = parens $ code "." <:> sep (code ".") (fmtName <$> fields)
      multi = single
  fmt (IndexSection idxs _ loc) = buildFmt loc single multi
    where
      idxs' = brackets $ sepSpace (code ",") idxs
      single = parens (code "." <:> idxs')
      multi = single
  fmt (Constr n cs _ loc) = buildFmt loc single multi
    where
      cons = code "#" <:> fmtName n
      common s = sep s cs
      single = cons <+> common space
      multi = stdNest $ cons </> common line
  fmt (Attr attr e loc) = buildFmt loc single multi
    where
      single = attr <+> e
      multi = stdNest (attr </> e)
  fmt (AppExp e _loc) = buildFmt e single multi
    where
      single = fmt e
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

instance Format UncheckedCase where
  fmt (CasePat p e loc) = buildFmt loc single multi
    where
      single = code "case" <+> p <+> code "->" <+> e
      multi = code "case" <+> p <+> stdNest (code "->" </> e)

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
  fmt (BinOp (bop, _) _ (x, _) (y, _) loc) = buildFmt loc single multi
    where
      single = x <+> fmtBinOp bop <+> y
      multi = x </> fmtBinOp bop <+> y
  fmt (Match e cs loc) = buildFmt loc single multi
    where
      multiCases = sep line $ toList cs
      singleCases = sep space $ toList cs
      single = code "match" <+> e <+> singleCases
      multi = code "match" <+> e </> multiCases
  -- should omit the initial value expression
  -- need some way to catch when the value expression match the pattern
  fmt (Loop sizeparams pat initexp form loopbody loc) | matchPat pat initexp = buildFmt loc single multi
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
      common =
        ( code "loop" `op` sizeparams' )
        <+/> pat
        <+> form
      single = common <+> code "do" <+> loopbody
      multi = common <+> stdNest (code "do" </> loopbody)
  fmt (Loop sizeparams pat initexp form loopbody loc) = buildFmt loc single multi
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
      common =
        ( ( code "loop" `op` sizeparams' )
          <+/> pat
          <+> code "="
        )
        <+/> initexp
        <+> form
      single = common <+> code "do" <+> loopbody
      multi = common <+> stdNest (code "do" </> loopbody)
  fmt (Index e idxs loc) = buildFmt loc single multi
    where
      aux f = brackets $ f (code ",") idxs
      single = e <:> aux sepSpace
      multi = e <:> aux sepLine
  fmt (LetPat sizes pat e body loc) = buildFmt loc single multi
    where
      sizes' = sep nil sizes
      common =
        ( code "let"
          <+> sepNonEmpty space [sizes', fmt pat]
          <+> code "="
        )
        <+/> e
      single = common <+> letBody body
      multi = common </> letBody body
  fmt (LetFun fname (tparams, params, retdecl, _, e) body loc) = buildFmt loc single multi
    where
      tparams' = sep space tparams
      params' = sep space params
      retdecl' =
        case retdecl of
          Just a -> code ":" <+> a <:> space
          Nothing -> space
      sub = sepNonEmpty space [tparams', params']
      common =
        ( code "let"
          <+> fmtName fname
          <:> (if null params && null tparams then nil else space)
          <:> sub
          <:> retdecl'
          <:> code "="
        )
        <+/> e
      single = common <+> letBody body
      multi = common </> letBody body
  fmt (LetWith dest src idxs ve body loc)
    | dest == src = buildFmt loc singleSame multiSame
    | otherwise = buildFmt loc singleDiff multiDiff
    where
      idxs' = brackets $ sep (code ", ") idxs
      commonSame =
        ( code "let"
          <+> dest
          <:> idxs'
          <+> code "="
        )
        <+/> ve
      singleSame = commonSame <+> letBody body
      multiSame = commonSame </> letBody body
      commonDiff =
        ( code "let"
          <+> dest
          <+> code "="
          <+> src
          <+> code "with"
          <+> idxs'
        )
        <+/> ve
      singleDiff = commonDiff <+> letBody body
      multiDiff = commonDiff </> letBody body
  fmt (Range start maybe_step end loc) = buildFmt loc single multi
    where

      end' =
        case end of
          DownToExclusive e -> code "..>" <:> e
          ToInclusive e -> code "..." <:> e
          UpToExclusive e -> code "..<" <:> e
      step = maybe nil (code ".." <:>) maybe_step
      single = start <:> step <:> end'
      multi = single
  fmt (If c t f loc) = buildFmt loc single multi
    where
      single =
        code "if"
        <+> c
        <+> code "then"
        <+> t
        <+> code "else"
        <+> f
      multi = do
        -- This should handle chained if expressions better.
        code "if"
          <+> c
          <+> code "then"
          </> t
          </> code "else"
          </> f
  fmt (Apply f args loc) = buildFmt loc single multi
    where
      mArgs s = sep s $ map snd (toList args)
      single = f <+> mArgs space
      multi = f <+> align (mArgs line)

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmt body
letBody body@(AppExp LetFun {} _) = fmt body
letBody body@(AppExp LetWith {} _) = fmt body
letBody body = buildFmt body single multi
  where
    single = code "in" <+> body
    multi = code "in" </> stdIndent body

instance Format (SizeBinder Name) where
  fmt (SizeBinder v _) = brackets $ fmtName v

instance Format (IdentBase NoInfo Name t) where
  fmt = fmtPretty . identName

instance Format (LoopFormBase NoInfo Name) where
  fmt (For i ubound) =
    code "for" <+> i <+> code "<" <+> ubound
  fmt (ForIn x e) =
    code "for" <+> x <+> code "in" <+> e
  fmt (While cond) = do
    code "while" <+> cond

fmtBinOp :: QualName Name -> FmtM Fmt
fmtBinOp bop =
  case leading of
    Backtick -> code "`" <:> fmtQualNameSingle bop <:> code "`"
    _any -> fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

instance Format UncheckedValBind where
  fmt (ValBind entry name retdecl _rettype tparams args body docs attrs loc) = buildFmt loc single multi
    where
      attrs' = sep space attrs
      tparams' = sep space tparams
      args' = sep space args
      retdecl' =
        case retdecl of
          Just a -> code ":" <+> a <:> space
          Nothing -> space
      sub = sepNonEmpty space [tparams', args']
      common =
        docs
        <:> (if null attrs then nil else attrs' <:> space)
        <:> fun
        <+> fmtNameParen name
        <:> (if null tparams && null args then nil else space)
        <:> sub
        <:> retdecl'
      single = common <+> code "=" <+> body
      multi = common <+> code "=" </> stdNest body
      fun =
        case entry of
          Just _ -> code "entry"
          _any -> code "def"

instance Format (SizeExp UncheckedExp) where
  fmt (SizeExp d loc) = buildFmt loc single multi
    where
      single = brackets d
      multi = single
  fmt (SizeExpAny loc) = buildFmt loc single multi
    where
      single = brackets nil
      multi = single

instance Format UncheckedSpec where
  fmt (TypeAbbrSpec tpsig) = fmt tpsig
  fmt (TypeSpec l name ps doc loc) = buildFmt loc single multi
    where
      common = doc <:> code "type" <:> l
      single = common <+> sep space (fmtName name : map fmt ps)
      multi = common <+> fmtName name <:> align (sep line ps) -- This could be prettier.
  fmt (ValSpec name ps te _ doc loc) = buildFmt loc single multi
    where
      doc' = doc
      single = doc' <:> code "val" <+> sep space (fmtName name : map fmt ps) <:> code ":" <+> te
      multi = single
  fmt (ModSpec name mte doc loc) = buildFmt loc single multi
    where
      single = doc <:> code "module" <+> fmtName name <:> code ":" <+> mte
      multi = single
  fmt (IncludeSpec mte loc) = buildFmt loc single multi
    where
      single = code "include" <+> mte
      multi = single

instance Format UncheckedModTypeExp where
  fmt (ModTypeVar v _ loc) = buildFmt loc single multi
    where
      single = fmtPretty v
      multi = single
  fmt (ModTypeParens mte loc) = buildFmt loc single multi
    where
      common = fmt mte
      single = parens mte
      multi = align (code "(" <:> common) </> code ")"
  fmt (ModTypeSpecs sbs loc) = buildFmt loc single multi
    where
      common s = sep s sbs
      single = braces $ common space
      multi = align (code "{" <:> common line) </> code "}"
  fmt (ModTypeWith mte (TypeRef v ps td _) loc) = buildFmt loc single multi
    where
      ps_op = if null ps then (<:>) else (<+>)
      common = mte <+> code "with" <+> fmtPretty v `ps_op` sep space ps
      single = common <+> code "=" <+> td
      multi = common <+> code "=" <+> stdIndent td
  fmt (ModTypeArrow (Just v) te0 te1 loc) = buildFmt loc single multi
    where
      op a b = parens (fmtName v <:> code ":" <+> a) <+> code "->" <+> b
      single = te0 `op` te1
      multi = single
  fmt (ModTypeArrow Nothing te0 te1 loc) = buildFmt loc single multi
    where
      single = te0 <+> code "->" <+> te1
      multi = single

instance Format UncheckedModTypeBind where
  fmt (ModTypeBind pName pSig doc loc) = buildFmt loc single multi
    where
      op = if isBracesOrParens pSig then (<+>) else (</>)
      common = doc <:> code "module type" <+> fmtName pName <+> code "="
      single = common <+> pSig
      multi = common `op` pSig

isBracesOrParens :: UncheckedModTypeExp -> Bool
isBracesOrParens (ModTypeSpecs _ _) = True
isBracesOrParens (ModTypeParens _ _) = True
isBracesOrParens _ = False

instance Format (ModParamBase NoInfo Name) where 
  fmt :: ModParamBase NoInfo Name -> FmtM Fmt
  fmt (ModParam pName pSig _f loc) = buildFmt loc single multi
    where
      single = parens $ fmtName pName <:> code ":" <+> pSig
      multi = single

instance Format UncheckedModBind where
  fmt (ModBind name ps sig te doc loc) = buildFmt loc single multi
    where
      sig' = fmtSig sig
      single =
        doc
        <:> code "module"
        <+> sep space (fmtName name : map fmt ps)
        <:> sig'
        <:> code "="
        <+> te
      multi = 
        doc
        <:> code "module"
        <+> fmtName name
        <+> align (sep line ps) -- This could be better
        <:> sig'
        <:> code "="
        <+> te
      fmtSig Nothing = space
      fmtSig (Just (s', _f)) = code ":" <+> s' <:> space

-- All of these should probably be "extra" indented
instance Format UncheckedModExp where
  fmt (ModVar v loc) = buildFmt loc single multi
    where
      single = fmtQualNameSingle v
      multi = fmtQualNameMulti v
  fmt (ModParens f loc) = buildFmt loc single multi
    where
      single = parens f
      multi = align (code "(" </> f) </> code ")"
  fmt (ModImport path _f loc) = buildFmt loc single multi
    where
      single = code "import \"" <:> fmtPretty path <:> code "\""
      multi = single
  -- Should be put inside a nested block
  fmt (ModDecs decs loc) = buildFmt loc single multi
    where
      fmtDecs s = sep s decs
      single = braces $ fmtDecs space
      multi = code "{" </> stdIndent (fmtDecs (line <:> line)) </> code "}"
  fmt (ModApply f a _f0 _f1 loc) = buildFmt loc single multi
    where
      single = f <+> a
      multi =  f </> stdIndent a
  fmt (ModAscript me se _f loc) = buildFmt loc single multi
    where
      single = me <:> code ":" <+> se
      multi = single
  fmt (ModLambda param maybe_sig body loc) = buildFmt loc single multi
    where
      sig =
        case maybe_sig of
          Nothing -> nil
          Just (sig', _) -> code ":" <+> parens sig'
      common = code "\\" <:> param <:> sig
      single = common <+> code "->" <+> body
      multi = common <+> code "->" </> stdIndent body

-- | Formatting of Futhark declarations.
instance Format UncheckedDec where
  fmt (ValDec t) = buildFmt t single multi
    where
      single = fmt t -- A value declaration.
      multi = single
  fmt (TypeDec tb) = buildFmt tb single multi
    where
      single = fmt tb -- A type declaration.
      multi = single
  fmt (ModTypeDec tb) = buildFmt tb single multi
    where
      single = fmt tb -- A module type declation.
      multi = single
  fmt (ModDec tb) = buildFmt tb single multi
    where
      single = fmt tb -- A module declation.
      multi = single
  fmt (OpenDec tb loc) = buildFmt loc single multi
    where
      single = code "open" <+> tb
      multi = single
  -- Adds the local keyword
  fmt (LocalDec tb loc) = buildFmt loc single multi
    where
      single = code "local" <+> tb
      multi = single
  -- Import declarations.
  fmt (ImportDec path _tb loc) = buildFmt loc single multi
    where
      single = code "import \"" <:> fmtPretty path <:> code "\""
      multi = single

-- | Does not return residual comments, because these are simply
-- inserted at the end.
instance Format UncheckedProg where
  fmt (Prog dc decs) =
    dc <:> sep (line <:> line) decs <:> popComments

fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmt prog
  pure $ pretty $ runFormat m cs
