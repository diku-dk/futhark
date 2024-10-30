{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Fmt.Printer (fmtText) where

import Data.Foldable
import Data.Loc (Loc (..))
import Data.Text qualified as T
import Futhark.Fmt.Monad
import Language.Futhark
import Language.Futhark.Parser
  ( SyntaxError (..),
    parseFutharkWithComments,
  )
import Prettyprinter.Internal (Pretty)

fmtName :: Name -> FmtM Fmt
fmtName = text . nameToText

fmtNameParen :: Name -> FmtM Fmt
fmtNameParen name
  | operatorName name = parens $ fmtName name
  | otherwise = fmtName name

fmtPretty :: (Pretty a) => a -> FmtM Fmt
fmtPretty = text . prettyText

instance Format (Maybe DocComment) where
  fmt (Just (DocComment x loc)) =
    addComments loc $ sep nil $ prefixes (T.lines x)
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

instance Format UncheckedTypeExp where
  fmt (TEVar v loc) = addComments loc $ fmtQualName v
  fmt (TETuple ts loc) =
    addComments loc $ parens $ sepLine (text ",") ts
  fmt (TEParens te loc) = addComments loc $ parens te
  fmt (TERecord fs loc) =
    addComments loc $ braces $ sepLine (text ",") fields
    where
      fields = fmtFieldType <$> fs
  fmt (TEArray se te loc) = addComments loc $ se <:> te
  fmt (TEUnique te loc) = addComments loc $ text "*" <:> te
  fmt (TEApply te tArgE loc) = addComments loc $ te <+> tArgE
  fmt (TEArrow name te0 te1 loc) =
    addComments loc $ fmtParamType name te0 <+> text "->" </> stdIndent te1
  fmt (TESum tes loc) =
    addComments loc $
      sep (line <:> text "|" <:> space) $
        map fmtSumTypeConstr tes
  fmt (TEDim dims te loc) =
    addComments loc $ text "?" <:> dims' <:> text "." <:> te
    where
      dims' = sep nil $ map (brackets . fmtName) dims

instance Format (TypeArgExp UncheckedExp Name) where
  fmt (TypeArgExpSize se) = fmt se
  fmt (TypeArgExpType te) = fmt te

instance Format UncheckedTypeBind where
  fmt (TypeBind name l ps e NoInfo dc loc) =
    addComments loc $
      dc
        <:> text "type"
        <:> l
        <+> fmtName name
        <:> (if null ps then nil else space)
        <:> localLayoutList ps (align $ sep line ps)
        <+> text "="
        </> stdIndent e

instance Located (AttrAtom a) where
  locOf _ = NoLoc

instance Format (AttrAtom a) where
  fmt (AtomName name) = fmtName name
  fmt (AtomInt int) = text $ prettyText int

-- Not sure this is correct.
instance Located (AttrInfo a) where
  locOf (AttrAtom _ loc) = locOf loc
  locOf (AttrComp _ _ loc) = locOf loc

instance Format (AttrInfo a) where
  fmt attr = text "#" <:> brackets (fmtAttrInfo attr)
    where
      fmtAttrInfo (AttrAtom attr' loc) = addComments loc $ fmt attr'
      fmtAttrInfo (AttrComp name attrs loc) =
        addComments loc $ fmtName name <:> parens (sep (text ",") $ map fmtAttrInfo attrs)

instance Located Liftedness where
  locOf _ = NoLoc

instance Format Liftedness where
  fmt Unlifted = nil
  fmt SizeLifted = text "~"
  fmt Lifted = text "^"

instance Format UncheckedTypeParam where
  fmt (TypeParamDim name loc) = addComments loc $ brackets $ fmtName name
  fmt (TypeParamType l name loc) = addComments loc $ text "'" <:> l <:> fmtName name

instance Format (UncheckedPat t) where
  fmt (TuplePat pats loc) =
    addComments loc $
      parens $
        sepLine (text ",") pats
  fmt (RecordPat pats loc) =
    addComments loc $
      braces $
        sepLine (text ",") $
          map fmtFieldPat pats
    where
      -- Currently it always adds the fields it seems.
      fmtFieldPat (name, t) = fmtName name <+> text "=" <+> t
  fmt (PatParens pat loc) =
    addComments loc $ text "(" <:> align pat <:/> text ")"
  fmt (Id name _ loc) = addComments loc $ fmtNameParen name
  fmt (Wildcard _t loc) = addComments loc $ text "_"
  fmt (PatAscription pat t loc) = addComments loc $ pat <:> text ":" <+> t
  fmt (PatLit _e _ loc) = addComments loc $ fmtCopyLoc loc
  fmt (PatConstr n _ pats loc) =
    addComments loc $
      text "#" <:> fmtName n </> align (sep line pats)
  fmt (PatAttr attr pat loc) = addComments loc $ attr <+> pat

instance Format (FieldBase NoInfo Name) where
  fmt (RecordFieldExplicit name e loc) =
    addComments loc $ fmtName name <+> text "=" </> stdIndent e
  fmt (RecordFieldImplicit name _ loc) = addComments loc $ fmtName name

instance Located PrimValue where
  locOf _ = NoLoc

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

instance Located UncheckedDimIndex where
  locOf _ = NoLoc

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
  fmt (Var name _ loc) = addComments loc $ fmtQualName name
  fmt (Hole _ loc) = addComments loc $ text "???"
  fmt (Parens e loc) =
    addComments loc $ text "(" <:> stdNest e <:/> text ")"
  fmt (QualParens (v, _qLoc) e loc) =
    addComments loc $
      fmtQualName v <:> text "." <:> text "(" <:> align e <:/> text ")"
  fmt (Ascript e t loc) = addComments loc $ e <:> text ":" <+> t
  fmt (Coerce e t _ loc) = addComments loc $ e <+> text ":>" <+> t
  fmt (Literal _v loc) = addComments loc $ fmtCopyLoc loc
  fmt (IntLit _v _ loc) = addComments loc $ fmtCopyLoc loc
  fmt (FloatLit _v _ loc) = addComments loc $ fmtCopyLoc loc
  fmt (TupLit es loc) =
    addComments loc $
      parens $
        sepLine (text ",") es
  fmt (RecordLit fs loc) =
    addComments loc $
      braces $
        sepLine (text ",") fs
  fmt (ArrayVal vs _ loc) =
    addComments loc $
      brackets $
        sepLine (text ",") vs
  fmt (ArrayLit es _ loc) =
    addComments loc $
      brackets $
        sepLine (text ",") es
  fmt (StringLit _s loc) = addComments loc $ fmtCopyLoc loc
  fmt (Project k e _ loc) = addComments loc $ e <:> text "." <:> fmtPretty k
  fmt (Negate e loc) = addComments loc $ text "-" <:> e
  fmt (Not e loc) = addComments loc $ text "!" <:> e
  fmt (Update src idxs ve loc) =
    addComments loc $
      src <+> text "with" <+> idxs' <+> stdNest (text "=" </> ve)
    where
      idxs' = brackets $ sep (text "," <:> space) idxs
  fmt (RecordUpdate src fs ve _ loc) =
    addComments loc $
      src <+> text "with" <+> fs' <+> stdNest (text "=" </> ve)
    where
      fs' = sep (text ".") $ fmtName <$> fs
  fmt (Assert e1 e2 _ loc) =
    addComments loc $ text "assert" <+> e1 <+> e2
  fmt (Lambda params body rettype _ loc) =
    addComments loc $
      text "\\" <:> sep space params <:> ascript <+> stdNest (text "->" </> body)
    where
      ascript = maybe nil (text ": " <:>) rettype
  fmt (OpSection binop _ loc) =
    addComments loc $
      if operatorName (qualLeaf binop)
        then fmtQualName binop
        else parens $ text "`" <:> fmtQualName binop <:> text "`"
  fmt (OpSectionLeft binop _ x _ _ loc) =
    addComments loc $ parens $ x <+> fmtBinOp binop
  fmt (OpSectionRight binop _ x _ _ loc) =
    addComments loc $ parens $ fmtBinOp binop <+> x
  fmt (ProjectSection fields _ loc) =
    addComments loc $ parens $ text "." <:> sep (text ".") (fmtName <$> fields)
  fmt (IndexSection idxs _ loc) =
    addComments loc $ parens (text "." <:> idxs')
    where
      idxs' = brackets $ sep (text "," <:> space) idxs
  fmt (Constr n cs _ loc) =
    addComments loc $ text "#" <:> fmtName n <+> align (sep line cs)
  fmt (Attr attr e loc) = addComments loc $ align (attr </> e)
  fmt (AppExp e _) = fmt e

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
    addComments loc $ text "case" <+> p <+> text "->" </> stdIndent e

instance Format (AppExpBase NoInfo Name) where
  fmt (BinOp (bop, _) _ (x, _) (y, _) loc) =
    addComments loc $ x </> fmtBinOp bop <+> y
  fmt (Match e cs loc) =
    addComments loc $ text "match" <+> e </> sep line (toList cs)
  -- need some way to omit the inital value expression, when this it's trivial
  fmt (Loop sizeparams pat (LoopInitImplicit NoInfo) form loopbody loc) =
    addComments loc $
      ( (text "loop" `op` sizeparams')
          <+/> pat
      )
        <+> form
        <+> text "do"
        </> stdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Loop sizeparams pat (LoopInitExplicit initexp) form loopbody loc) =
    addComments loc $
      ( (text "loop" `op` sizeparams')
          <+/> pat
          <+> text "="
      )
        <+/> initexp
        <+> form
        <+> text "do"
        </> stdIndent loopbody
    where
      op = if null sizeparams then (<:>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Index e idxs loc) =
    addComments loc $
      (e <:>) $
        brackets $
          sepLine (text ",") idxs
  fmt (LetPat sizes pat e body loc) =
    addComments loc $
      ( text "let"
          <+> sub
          <+> text "="
      )
        <+/> e
        </> letBody body
    where
      sizes' = sep nil sizes
      sub
        | null sizes = fmt pat
        | otherwise = sizes' <+> fmt pat
  fmt (LetFun fname (tparams, params, retdecl, _, e) body loc) =
    addComments loc $
      ( text "let"
          <+> fmtName fname
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
      sub
        | null tparams && null params = nil
        | null tparams = space <:> params'
        | null params = space <:> tparams'
        | otherwise = space <:> tparams' <+> params'
  fmt (LetWith dest src idxs ve body loc)
    | dest == src =
        addComments loc $
          ( text "let"
              <+> dest
              <:> idxs'
              <+> text "="
          )
            <+/> ve
            </> letBody body
    | otherwise =
        addComments loc $
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
    addComments loc $ start <:> step <:> end'
    where
      end' =
        case end of
          DownToExclusive e -> text "..>" <:> e
          ToInclusive e -> text "..." <:> e
          UpToExclusive e -> text "..<" <:> e
      step = maybe nil (text ".." <:>) maybe_step
  fmt (If c t f loc) =
    addComments loc $
      text "if"
        <+> c
        <+> text "then"
        </> stdIndent t
        </> text "else"
        </> stdIndent f
  fmt (Apply f args loc) =
    addComments loc $
      f <+> align fmt_args
    where
      fmt_args = sepArgs $ map snd (toList args)

letBody :: UncheckedExp -> FmtM Fmt
letBody body@(AppExp LetPat {} _) = fmt body
letBody body@(AppExp LetFun {} _) = fmt body
letBody body@(AppExp LetWith {} _) = fmt body
letBody body = addComments body $ text "in" <+> align body

instance Format (SizeBinder Name) where
  fmt (SizeBinder v loc) = addComments loc $ brackets $ fmtName v

instance Format (IdentBase NoInfo Name t) where
  fmt = fmtPretty . identName

instance Located (LoopFormBase NoInfo Name) where
  locOf _ = NoLoc

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
    addComments loc $
      docs
        <:> (if null attrs then nil else attrs' <:> space)
        <:> fun
        <+> fmtNameParen name
        <:> sub
        <:> retdecl'
        <:> text "="
        </> stdIndent body
    where
      attrs' = sep space attrs
      tparams' = localLayoutList tparams $ align $ sep line tparams
      args' = localLayoutList args $ align $ sep line args
      retdecl' =
        case retdecl of
          Just a -> text ":" <+> a <:> space
          Nothing -> space
      sub
        | null tparams && null args = nil
        | null tparams = space <:> args'
        | null args = space <:> tparams'
        | otherwise = space <:> tparams' <+> args'
      fun =
        case entry of
          Just _ -> text "entry"
          _any -> text "def"

instance Format (SizeExp UncheckedExp) where
  fmt (SizeExp d loc) = addComments loc $ brackets d
  fmt (SizeExpAny loc) = addComments loc $ brackets nil

instance Format UncheckedSpec where
  fmt (TypeAbbrSpec tpsig) = fmt tpsig
  fmt (TypeSpec l name ps doc loc) =
    addComments loc $
      doc <:> text "type" <+> l <:> sub
    where
      sub
        | null ps = fmtName name
        | otherwise = fmtName name </> align (sep line ps)
  fmt (ValSpec name ps te _ doc loc) =
    addComments loc $
      doc
        <:> text "val"
        <+> sub
        <:> text ":"
        <+> te
    where
      sub
        | null ps = fmtName name
        | otherwise = fmtName name </> align (sep line ps)
  fmt (ModSpec name mte doc loc) =
    addComments loc $ doc <:> text "module" <+> fmtName name <:> text ":" <+> mte
  fmt (IncludeSpec mte loc) = addComments loc $ text "include" <+> mte

instance Format UncheckedModTypeExp where
  fmt (ModTypeVar v _ loc) = addComments loc $ fmtPretty v
  fmt (ModTypeParens mte loc) =
    addComments loc $ text "(" <:> align mte <:/> text ")"
  fmt (ModTypeSpecs sbs loc) =
    addComments loc $ text "{" <:/> stdIndent (sep line sbs) <:/> text "}"
  fmt (ModTypeWith mte (TypeRef v ps td _) loc) =
    addComments loc $
      mte <+> text "with" <+> fmtPretty v `ps_op` sep space ps <+> text "=" <+> td
    where
      ps_op = if null ps then (<:>) else (<+>)
  fmt (ModTypeArrow (Just v) te0 te1 loc) =
    addComments loc $
      parens (fmtName v <:> text ":" <+> te0) <+> align (text "->" </> te1)
  fmt (ModTypeArrow Nothing te0 te1 loc) =
    addComments loc $ te0 <+> text "->" <+> te1

instance Format UncheckedModTypeBind where
  fmt (ModTypeBind pName pSig doc loc) =
    addComments loc $
      doc <:> text "module type" <+> fmtName pName <+> text "=" <+> pSig

instance Format (ModParamBase NoInfo Name) where
  fmt (ModParam pName pSig _f loc) =
    addComments loc $ parens $ fmtName pName <:> text ":" <+> pSig

instance Format UncheckedModBind where
  fmt (ModBind name ps sig te doc loc) =
    addComments loc $
      doc
        <:> text "module"
        <+> fmtName name
        <:> ps'
        <:> sig'
        <:> text "="
        <:> te'
    where
      te' = fmtByLayout te (line <:> stdIndent te) (space <:> te)
      sig' = fmtSig sig
      fmtSig Nothing = space
      fmtSig (Just (s', _f)) = text ":" <+> s' <:> space
      ps' =
        case ps of
          [] -> nil
          _any -> space <:> localLayoutList ps (align $ sep line ps)

-- All of these should probably be "extra" indented
instance Format UncheckedModExp where
  fmt (ModVar v loc) = addComments loc $ fmtQualName v
  fmt (ModParens f loc) =
    addComments loc $ text "(" <:/> stdIndent f <:/> text ")"
  fmt (ModImport path _f loc) =
    addComments loc $ text "import \"" <:> fmtPretty path <:> text "\""
  fmt (ModDecs decs loc) =
    addComments loc $
      text "{" <:/> stdIndent (sepDecs decs) <:/> text "}"
  fmt (ModApply f a _f0 _f1 loc) = addComments loc $ f <+> a
  fmt (ModAscript me se _f loc) = addComments loc $ align (me <:> text ":" </> se)
  fmt (ModLambda param maybe_sig body loc) =
    addComments loc $
      text "\\" <:> param <:> sig <+> text "->" </> stdIndent body
    where
      sig =
        case maybe_sig of
          Nothing -> nil
          Just (sig', _) -> text ":" <+> parens sig'

instance Format UncheckedDec where
  fmt (ValDec t) = fmt t
  fmt (TypeDec tb) = fmt tb
  fmt (ModTypeDec tb) = fmt tb
  fmt (ModDec tb) = fmt tb
  fmt (OpenDec tb loc) = addComments loc $ text "open" <+> tb
  fmt (LocalDec tb loc) = addComments loc $ text "local" <+> tb
  fmt (ImportDec path _tb loc) =
    addComments loc $ text "import \"" <:> fmtPretty path <:> text "\""

instance Located UncheckedProg where
  locOf _ = NoLoc

instance Format UncheckedProg where
  fmt (Prog dc decs) =
    fmt $ dc <:> sepDecs decs </> popComments

-- | Given a filename and a futhark program, formats the program.
fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmt prog
  pure $ pretty $ runFormat m cs fContent
