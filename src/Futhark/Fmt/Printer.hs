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
--
-- A *formatting function* is a function of type
--
--   Format a => a -> FmtM Fmt
--
-- where 'a' is some syntactical object, e.g. UncheckedTypeExp.  The
-- FmtM is state monad with state: [Comment], a list of sorted "residual 
-- comments" that have yet to be injected into the output.  
-- A formatting function returns the formatted representation of 'a'
-- as a 'Fmt' value as well a the formatted representation of any comments
-- before 'a', and updates the FmtM state by removing comments
-- the printed comments.

-- TODO: ensure that doc comments (that start with "-- |" and are
-- connected to definitions) are always separated from other comments
-- with a single newline character. Question: Exactly what is meant by this?

fmtName :: Name -> FmtM Fmt
fmtName = text . nameToText

fmtNameParen :: Name -> FmtM Fmt
fmtNameParen name
  | operatorName name = parens $ fmtName name 
  | otherwise = fmtName name

fmtPretty :: (Pretty a) => a -> FmtM Fmt
fmtPretty = text . prettyText

-- | Documentation comments are always optional, so this takes a 'Maybe'.
instance Format (Maybe DocComment) where
  fmt (Just (DocComment x loc)) =
    prependComments loc $ sep nil $ prefixes (T.lines x)
    where
      prefixes [] = []
      prefixes (l : ls) = comment ("-- | " <> l) : map (comment . ("-- " <>)) ls
  fmt Nothing = nil

fmtFieldType :: (Name, UncheckedTypeExp) -> FmtM Fmt
fmtFieldType (name', t) = fmtName name' <:> text ":" <+> t

fmtParamType :: Maybe Name -> UncheckedTypeExp -> FmtM Fmt
fmtParamType (Just n) te =
  parens $ fmtName n <:> text ":" <+> te
fmtParamType Nothing te = fmt te

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> FmtM Fmt
fmtSumTypeConstr (name, fs) =
  text "#" <:> fmtName name <+> sep space fs

-- | Formatting of Futhark type expressions.
instance Format UncheckedTypeExp where
  fmt (TEVar v loc) = prependComments loc $ fmtQualName v
  fmt (TETuple ts loc) =
    prependComments loc $ parens $ sepLine (text ",") ts
  fmt (TEParens te loc) = prependComments loc $ parens te 
  fmt (TERecord fs loc) =
    prependComments loc $ braces $ sepLine (text ",") fields
    where
      fields = fmtFieldType <$> fs
  fmt (TEArray se te loc) = prependComments loc $ se <:> te 
  -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
  fmt (TEUnique te loc) = prependComments loc $ text "*" <:> te 
  -- I am not sure I guess applying a higher kinded type to some type expression
  fmt (TEApply te tArgE loc) = prependComments loc $ te <+> tArgE 
  -- this is "->"
  fmt (TEArrow name te0 te1 loc) =
    prependComments loc $ fmtParamType name te0 <+> text "->" </> softStdIndent te1
  -- This should be "|"
  fmt (TESum tes loc) =
    prependComments loc $
      sep (line <:> text "|" <:> space) $
        map fmtSumTypeConstr tes
  fmt (TEDim dims te loc) =
    prependComments loc $ text "?" <:> dims' <:> text "." <:> te
    where
      dims' = sep nil $ map (brackets . fmtName) dims

instance Format (TypeArgExp UncheckedExp Name) where
  fmt (TypeArgExpSize se) = fmt se
  fmt (TypeArgExpType te) = fmt te

instance Format UncheckedTypeBind where
  fmt (TypeBind name l ps e NoInfo dc loc) =
    prependComments loc $
      dc
        <:> text "type"
        <:> l
        <+> fmtName name
        <:> (if null ps then nil else space)
        <:> localLayoutList ps (align $ sep line ps)
        <+> text "="
        </> softStdIndent e

instance Format (AttrAtom a) where
  fmt (AtomName name) = fmtName name
  fmt (AtomInt int) = text $ prettyText int

instance Format (AttrInfo a) where
  fmt attr = text "#" <:> brackets (fmtAttrInfo attr)
    where
      fmtAttrInfo (AttrAtom attr' loc) = prependComments loc $ fmt attr'
      fmtAttrInfo (AttrComp name attrs loc) =
        prependComments loc $ fmtName name <:> parens (sep (text ",") $ map fmtAttrInfo attrs)

instance Format Liftedness where
  fmt Unlifted = nil
  fmt SizeLifted = text "~"
  fmt Lifted = text "^"

instance Format UncheckedTypeParam where
  fmt (TypeParamDim name loc) = prependComments loc $ brackets $ fmtName name
  fmt (TypeParamType l name loc) = prependComments loc $ text "'" <:> l <:> fmtName name

instance Format (UncheckedPat t) where
  fmt (TuplePat pats loc) =
    prependComments loc $
      parens $
        sepLine (text ",") pats
  fmt (RecordPat pats loc) =
    prependComments loc $
      braces $
        sepLine (text ",") $
          map fmtFieldPat pats
    where
      -- Currently it always adds the fields it seems. I think it has to do this.
      fmtFieldPat (name, t) = fmtName name <+> text "=" <+> t 
  fmt (PatParens pat loc) =
    prependComments loc $ text "(" <:> align pat <:/> text ")"
  fmt (Id name _ loc) = prependComments loc $ fmtNameParen name
  fmt (Wildcard _t loc) = prependComments loc $ text "_"
  fmt (PatAscription pat t loc) = prependComments loc $ pat <:> text ":" <+> t
  fmt (PatLit _e _ loc) = prependComments loc $ fmtCopyLoc loc
  fmt (PatConstr n _ pats loc) =
    prependComments loc $
      text "#" <:> fmtName n </> align (sep line pats)
  fmt (PatAttr attr pat loc) = prependComments loc $ attr <+> pat

instance Format (FieldBase NoInfo Name) where
  fmt (RecordFieldExplicit name e loc) =
    prependComments loc $ fmtName name <+> text "=" </> softStdIndent e
  fmt (RecordFieldImplicit name _ loc) = prependComments loc $ fmtName name

instance Format PrimValue where
  fmt (UnsignedValue (Int8Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word8)) <:> text "u8"
  fmt (UnsignedValue (Int16Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word16)) <:> text "u16"
  fmt (UnsignedValue (Int32Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word32)) <:> text "u32"
  fmt (UnsignedValue (Int64Value v)) =
    fmt $ fmtPretty (show (fromIntegral v :: Word64)) <:> text "u64"
  fmt (SignedValue v) = fmtPretty v
  fmt (BoolValue True) = fmt $ text "true"
  fmt (BoolValue False) = fmt $ text "false"
  fmt (FloatValue v) = fmtPretty v

instance Format UncheckedDimIndex where
  fmt (DimFix e) = fmt e
  fmt (DimSlice i j (Just s)) =
    maybe (fmt nil) fmt i
      <:> text ":"
      <:> maybe (fmt nil) fmt j
      <:> text ":"
      <:> fmt s
  fmt (DimSlice i (Just j) s) =
    maybe (fmt nil) fmt i
      <:> text ":"
      <:> fmt j
      <:> maybe nil ((text ":" <:>) . fmt) s
  fmt (DimSlice i Nothing Nothing) =
    maybe (fmt nil) fmt i <:> text ":"

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

instance Format UncheckedExp where
  fmt (Var name _ loc) = prependComments loc $ fmtQualName name
  fmt (Hole _ loc) = prependComments loc $ text "???"
  fmt (Parens e loc) =
    prependComments loc $ text "(" <:> stdNest e <:/> text ")"
  fmt (QualParens (v, _loc) e loc) =
    prependComments loc $
      fmtQualName v <:> text "." <:> text "(" <:> align e <:/> text ")"
  fmt (Ascript e t loc) = prependComments loc $ e <:> text ":" <+> t
  fmt (Coerce e t _ loc) = prependComments loc $ e <+> text ":>" <+> t
  fmt (Literal _v loc) = prependComments loc $ fmtCopyLoc loc
  fmt (IntLit _v _ loc) = prependComments loc $ fmtCopyLoc loc
  fmt (FloatLit _v _ loc) = prependComments loc $ fmtCopyLoc loc 
  fmt (TupLit es loc) =
    prependComments loc $
      parens $
        sepLine (text ",") es
  fmt (RecordLit fs loc) =
    prependComments loc $
      braces $
        sepLine (text ",") fs
  fmt (ArrayVal vs _ loc) =
    prependComments loc $
      brackets $
        sepLine (text ",") vs
  fmt (ArrayLit es _ loc) =
    prependComments loc $
      brackets $
        sepLine (text ",") es
  fmt (StringLit _s loc) = fmtCopyLoc loc
  fmt (Project k e _ loc) = prependComments loc $ e <:> text "." <:> fmtPretty k
  fmt (Negate e loc) = prependComments loc $ text "-" <:> e
  fmt (Not e loc) = prependComments loc $ text "!" <:> e
  fmt (Update src idxs ve loc) =
    prependComments loc $
      src <+> text "with" <+> idxs' <+> stdNest (text "=" </> ve)
    where
      idxs' = brackets $ sep (text "," <:> space) idxs
  fmt (RecordUpdate src fs ve _ loc) =
    prependComments loc $
      src <+> text "with" <+> fs' <+> stdNest (text "=" </> ve)
    where
      fs' = sep (text ".") $ fmtName <$> fs 
  fmt (Assert e1 e2 _ loc) =
    prependComments loc $ text "assert" <+> e1 <+> e2
  fmt (Lambda params body rettype _ loc) =
    prependComments loc $
      text "\\" <:> sep space params <:> ascript <+> stdNest (text "->" </> body)
    where
      ascript = maybe nil (text ": " <:>) rettype
  fmt (OpSection binop _ loc) =
    prependComments loc $
      if operatorName (qualLeaf binop)
        then fmtQualName binop
        else parens $ text "`" <:> fmtQualName binop <:> text "`"
  fmt (OpSectionLeft binop _ x _ _ loc) =
    prependComments loc $ parens $ x <+> fmtBinOp binop
  fmt (OpSectionRight binop _ x _ _ loc) =
    prependComments loc $ parens $ fmtBinOp binop <+> x
  fmt (ProjectSection fields _ loc) =
    prependComments loc $ parens $ text "." <:> sep (text ".") (fmtName <$> fields)
  fmt (IndexSection idxs _ loc) =
    prependComments loc $ parens (text "." <:> idxs')
    where
      idxs' = brackets $ sep (text "," <:> space) idxs
  fmt (Constr n cs _ loc) =
    prependComments loc $ text "#" <:> fmtName n <+> align (sep line cs)
  fmt (Attr attr e loc) = prependComments loc $ align (attr </> e)
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
        else sep (text ".") (map fmtName names) <:> text "."

instance Format UncheckedCase where
  fmt (CasePat p e loc) =
    prependComments loc $ text "case" <+> p <+> text "->" </> softStdIndent e

instance Format (AppExpBase NoInfo Name) where
  fmt (BinOp (bop, _) _ (x, _) (y, _) loc) =
    prependComments loc $ x </> fmtBinOp bop <+> y
  fmt (Match e cs loc) =
    prependComments loc $ text "match" <+> e </> sep line (toList cs)
  -- need some way to omit the inital value expression, when this it's trivial
  fmt (Loop sizeparams pat (LoopInitImplicit NoInfo) form loopbody loc) =
    prependComments loc $
      ( (text "loop" `op` sizeparams')
          <+/> pat
      )
        <+> form
        <+> text "do"
        </> softStdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Loop sizeparams pat (LoopInitExplicit initexp) form loopbody loc) =
    prependComments loc $
      ( (text "loop" `op` sizeparams')
          <+/> pat
          <+> text "="
      )
        <+/> initexp
        <+> form
        <+> text "do"
        </> softStdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Index e idxs loc) =
    prependComments loc $
      (e <:>) $
        brackets $
          sepLine (text ",") idxs
  fmt (LetPat sizes pat e body loc) =
    prependComments loc $
      ( text "let"
          <+> sepFilter [not $ null sizes, True] space [sizes', fmt pat]
          <+> text "="
      )
        <+/> e
        </> letBody body
    where
      sizes' = sep nil sizes
  fmt (LetFun fname (tparams, params, retdecl, _, e) body loc) =
    prependComments loc $
      ( text "let"
          <+> fmtName fname
          <:> (if null params && null tparams then nil else space)
          <:> sub
          <:> retdecl'
          <:> text "="
      )
        <+/> e
        </> letBody body
    where
      tparams' = sep space tparams
      params' = sep space params
      retdecl' =
        case retdecl of
          Just a -> text ":" <+> a <:> space
          Nothing -> space
      sub = sepFilter [not $ null tparams, not $ null params] space [tparams', params']
  fmt (LetWith dest src idxs ve body loc)
    | dest == src =
        prependComments loc $
          ( text "let"
              <+> dest
              <:> idxs'
              <+> text "="
          )
            <+/> ve
            </> letBody body
    | otherwise =
        prependComments loc $
          ( text "let"
              <+> dest
              <+> text "="
              <+> src
              <+> text "with"
              <+> idxs'
          )
            <+/> ve
            </> letBody body
    where
      idxs' = brackets $ sep (text ", ") idxs
  fmt (Range start maybe_step end loc) =
    prependComments loc $ start <:> step <:> end'
    where
      end' =
        case end of
          DownToExclusive e -> text "..>" <:> e
          ToInclusive e -> text "..." <:> e
          UpToExclusive e -> text "..<" <:> e
      step = maybe nil (text ".." <:>) maybe_step
  fmt (If c t f loc) =
    prependComments loc $
      text "if"
        <+> c
        <+> text "then"
        </> softStdIndent t
        </> text "else"
        </> softStdIndent f
  fmt (Apply f args loc) =
    prependComments loc $
      f <+> align fmt_args
    where
      fmt_args = sepArgs $ map snd (toList args)

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmt body
letBody body@(AppExp LetFun {} _) = fmt body
letBody body@(AppExp LetWith {} _) = fmt body
letBody body = prependComments body $ text "in" <+> align body

instance Format (SizeBinder Name) where
  fmt (SizeBinder v loc) = prependComments loc $ brackets $ fmtName v

instance Format (IdentBase NoInfo Name t) where
  fmt = fmtPretty . identName

instance Format (LoopFormBase NoInfo Name) where
  fmt (For i ubound) =
    text "for" <+> i <+> text "<" <+> ubound
  fmt (ForIn x e) =
    text "for" <+> x <+> text "in" <+> e
  fmt (While cond) = do
    text "while" <+> cond

-- | This should always be simplified by location.
fmtBinOp :: QualName Name -> FmtM Fmt
fmtBinOp bop =
  case leading of
    Backtick -> text "`" <:> fmtQualName bop <:> text "`"
    _any -> fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

instance Format UncheckedValBind where
  fmt (ValBind entry name retdecl _rettype tparams args body docs attrs loc) =
    prependComments loc $
      docs
        <:> (if null attrs then nil else attrs' <:> space)
        <:> fun
        <+> fmtNameParen name
        <:> (if null tparams && null args then nil else space)
        <:> sub
        <:> retdecl'
        <:> text "="
        </> softStdIndent body
    where
      attrs' = sep space attrs
      tparams' = localLayoutList tparams $ align $ sep line tparams
      args' = localLayoutList args $ align $ sep line args
      retdecl' =
        case retdecl of
          Just a -> text ":" <+> a <:> space
          Nothing -> space
      flags = [not $ null tparams, not $ null args]
      sub =
        sepFilter flags space [tparams', args']
      fun =
        case entry of
          Just _ -> text "entry"
          _any -> text "def"

instance Format (SizeExp UncheckedExp) where
  fmt (SizeExp d loc) = prependComments loc $ brackets d
  fmt (SizeExpAny loc) = prependComments loc $ brackets nil

instance Format UncheckedSpec where
  fmt (TypeAbbrSpec tpsig) = fmt tpsig
  fmt (TypeSpec l name ps doc loc) =
    prependComments loc $
      doc <:> text "type" <+> l <:> sub
    where
      sub = sepFilter [True, not $ null ps] line [fmtName name, align (sep line ps)]
  fmt (ValSpec name ps te _ doc loc) =
    prependComments loc $
      doc
        <:> text "val"
        <+> sub
        <:> text ":"
        <+> te
    where
      sub = sepFilter [True, not $ null ps] line [fmtName name, align (sep line ps)]
  fmt (ModSpec name mte doc loc) =
    prependComments loc $ doc <:> text "module" <+> fmtName name <:> text ":" <+> mte
  fmt (IncludeSpec mte loc) = prependComments loc $ text "include" <+> mte

instance Format UncheckedModTypeExp where
  fmt (ModTypeVar v _ loc) = prependComments loc $ fmtPretty v
  fmt (ModTypeParens mte loc) =
    prependComments loc $ text "(" <:> align mte <:/> text ")"
  fmt (ModTypeSpecs sbs loc) =
    prependComments loc $ text "{" <:/> softStdIndent (sep line sbs) <:/> text "}"
  fmt (ModTypeWith mte (TypeRef v ps td _) loc) =
    prependComments loc $
      mte <+> text "with" <+> fmtPretty v `ps_op` sep space ps <+> text "=" <+> td
    where
      ps_op = if null ps then (<:>) else (<+>)
  fmt (ModTypeArrow (Just v) te0 te1 loc) =
    prependComments loc $
      parens (fmtName v <:> text ":" <+> te0) <+> align (text "->" </> te1)
  fmt (ModTypeArrow Nothing te0 te1 loc) =
    prependComments loc $ te0 <+> text "->" <+> te1

instance Format UncheckedModTypeBind where
  fmt (ModTypeBind pName pSig doc loc) =
    prependComments loc $
      doc <:> text "module type" <+> fmtName pName <+> text "=" <+> pSig

instance Format (ModParamBase NoInfo Name) where
  fmt :: ModParamBase NoInfo Name -> FmtM Fmt
  fmt (ModParam pName pSig _f loc) =
    prependComments loc $ parens $ fmtName pName <:> text ":" <+> pSig

instance Format UncheckedModBind where
  fmt (ModBind name ps sig te doc loc) =
    prependComments loc $
      doc
        <:> text "module"
        <+> fmtName name
        <:> ps'
        <:> sig'
        <:> text "="
        <:> te'
    where
      te' = fmtByLayout te (line <:> softStdIndent te) (space <:> te)
      sig' = fmtSig sig
      fmtSig Nothing = space
      fmtSig (Just (s', _f)) = text ":" <+> s' <:> space
      ps' =
        case ps of
          [] -> nil
          _any -> space <:> localLayoutList ps (align $ sep line ps)

-- All of these should probably be "extra" indented
instance Format UncheckedModExp where
  fmt (ModVar v loc) = prependComments loc $ fmtQualName v
  fmt (ModParens f loc) =
    prependComments loc $ text "(" <:/> softStdIndent f <:/> text ")"
  fmt (ModImport path _f loc) =
    prependComments loc $ text "import \"" <:> fmtPretty path <:> text "\""
  -- Should be put inside a nested block
  fmt (ModDecs decs loc) =
    prependComments loc $
      text "{" <:/> softStdIndent (sepDecs decs) <:/> text "}"
  fmt (ModApply f a _f0 _f1 loc) = prependComments loc $ f <+> a
  fmt (ModAscript me se _f loc) = prependComments loc $ align (me <:> text ":" </> se)
  fmt (ModLambda param maybe_sig body loc) =
    prependComments loc $
      text "\\" <:> param <:> sig <+> text "->" </> softStdIndent body
    where
      sig =
        case maybe_sig of
          Nothing -> nil
          Just (sig', _) -> text ":" <+> parens sig'

-- | Formatting of Futhark declarations.
instance Format UncheckedDec where
  fmt (ValDec t) = fmt t -- A value declaration.
  fmt (TypeDec tb) = fmt tb -- A type declaration.
  fmt (ModTypeDec tb) = fmt tb -- A module type declation.
  fmt (ModDec tb) = fmt tb -- A module declation.
  fmt (OpenDec tb loc) = prependComments loc $ text "open" <+> tb 
  -- Adds the local keyword
  fmt (LocalDec tb loc) = prependComments loc $ text "local" <+> tb
  -- Import declarations.
  fmt (ImportDec path _tb loc) =
    prependComments loc $ text "import \"" <:> fmtPretty path <:> text "\""

-- | Does not return residual comments, because these are simply
-- inserted at the end.
instance Format UncheckedProg where
  fmt (Prog dc decs) =
    fmt $ dc <:> sepDecs decs </> popComments

fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmt prog
  pure $ pretty $ runFormat m cs fContent
