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

lineIndent :: (Located a) => a -> Fmt -> Fmt -> Fmt
lineIndent l a b = fmtByLayout l (a <+> b) (a </> hardStdIndent b)

fmtName :: Name -> Fmt
fmtName = text . nameToText

fmtNameParen :: Name -> Fmt
fmtNameParen name
  | operatorName name = parens $ fmtName name
  | otherwise = fmtName name

fmtPretty :: (Pretty a) => a -> Fmt
fmtPretty = text . prettyText

class Format a where
  fmt :: a -> Fmt

instance Format (Maybe DocComment) where
  fmt (Just (DocComment x loc)) =
    addComments loc $ sep nil $ prefixes (T.lines x)
    where
      prefixes [] = []
      prefixes (l : ls) = comment ("-- | " <> l) : map (comment . ("-- " <>)) ls
  fmt Nothing = nil

fmtFieldType :: (Name, UncheckedTypeExp) -> Fmt
fmtFieldType (name', t) = fmtName name' <> ":" <+> fmt t

fmtParamType :: Maybe Name -> UncheckedTypeExp -> Fmt
fmtParamType (Just n) te =
  parens $ fmtName n <> ":" <+> fmt te
fmtParamType Nothing te = fmt te

fmtSumTypeConstr :: (Name, [UncheckedTypeExp]) -> Fmt
fmtSumTypeConstr (name, fs) =
  "#" <> fmtName name <+> sep space (map fmt fs)

instance Format UncheckedTypeExp where
  fmt (TEVar v loc) = addComments loc $ fmtQualName v
  fmt (TETuple ts loc) =
    addComments loc $
      parens $
        sepLineComments locOf fmt "," ts
  fmt (TEParens te loc) = addComments loc $ parens $ fmt te
  fmt (TERecord fs loc) =
    addComments loc $
      braces $
        sepLineComments (locOf . snd) fmtFieldType "," fs
  fmt (TEArray se te loc) = addComments loc $ fmt se <> fmt te
  fmt (TEUnique te loc) = addComments loc $ "*" <> fmt te
  fmt (TEApply te tArgE loc) = addComments loc $ fmt te <+> fmt tArgE
  fmt (TEArrow name te0 te1 loc) =
    addComments loc $ fmtParamType name te0 <+> "->" </> stdIndent (fmt te1)
  fmt (TESum tes loc) =
    addComments loc $
      sep (line <> "|" <> space) $
        map fmtSumTypeConstr tes -- Comments can not be inserted correctly here because names do not have a location.
  fmt (TEDim dims te loc) =
    addComments loc $ "?" <> dims' <> "." <> fmt te
    where
      dims' = sep nil $ map (brackets . fmtName) dims

instance Format (TypeArgExp UncheckedExp Name) where
  fmt (TypeArgExpSize se) = fmt se
  fmt (TypeArgExpType te) = fmt te

instance Format UncheckedTypeBind where
  fmt (TypeBind name l ps e NoInfo dc loc) =
    addComments loc $
      fmt dc
        <> "type"
        <> fmt l
          <+> fmtName name
        <> (if null ps then nil else space)
        <> localLayoutList ps (align $ sep line $ map fmt ps)
          <+> "="
          </> stdIndent (fmt e)

instance Format (AttrAtom a) where
  fmt (AtomName name) = fmtName name
  fmt (AtomInt int) = text $ prettyText int

instance Format (AttrInfo a) where
  fmt attr = "#" <> brackets (fmtAttrInfo attr)
    where
      fmtAttrInfo (AttrAtom attr' loc) = addComments loc $ fmt attr'
      fmtAttrInfo (AttrComp name attrs loc) =
        addComments loc $
          fmtName name
            <> parens (sep "," $ map fmtAttrInfo attrs)

instance Format Liftedness where
  fmt Unlifted = nil
  fmt SizeLifted = "~"
  fmt Lifted = "^"

instance Format UncheckedTypeParam where
  fmt (TypeParamDim name loc) =
    addComments loc $ brackets $ fmtName name
  fmt (TypeParamType l name loc) =
    addComments loc $ "'" <> fmt l <> fmtName name

instance Format (UncheckedPat t) where
  fmt (TuplePat pats loc) =
    addComments loc $
      parens $
        sepLineComments locOf fmt "," pats
  fmt (RecordPat pats loc) =
    addComments loc $
      braces $
        sepLineComments (locOf . snd) fmtFieldPat "," pats
    where
      -- Currently it always adds the fields it seems.
      fmtFieldPat (name, t) = fmtName name <+> "=" <+> fmt t
  fmt (PatParens pat loc) =
    addComments loc $ "(" <> align (fmt pat) <:/> ")"
  fmt (Id name _ loc) = addComments loc $ fmtNameParen name
  fmt (Wildcard _t loc) = addComments loc "_"
  fmt (PatAscription pat t loc) = addComments loc $ fmt pat <> ":" <+> fmt t
  fmt (PatLit _e _ loc) = addComments loc $ fmtCopyLoc loc
  fmt (PatConstr n _ pats loc) =
    addComments loc $
      "#" <> fmtName n </> align (sep line (map fmt pats))
  fmt (PatAttr attr pat loc) = addComments loc $ fmt attr <+> fmt pat

instance Format (FieldBase NoInfo Name) where
  fmt (RecordFieldExplicit name e loc) =
    addComments loc $ fmtName name <+> "=" </> stdIndent (fmt e)
  fmt (RecordFieldImplicit name _ loc) = addComments loc $ fmtName name

instance Format PrimValue where
  fmt (UnsignedValue (Int8Value v)) =
    fmtPretty (show (fromIntegral v :: Word8)) <> "u8"
  fmt (UnsignedValue (Int16Value v)) =
    fmtPretty (show (fromIntegral v :: Word16)) <> "u16"
  fmt (UnsignedValue (Int32Value v)) =
    fmtPretty (show (fromIntegral v :: Word32)) <> "u32"
  fmt (UnsignedValue (Int64Value v)) =
    fmtPretty (show (fromIntegral v :: Word64)) <> "u64"
  fmt (SignedValue v) = fmtPretty v
  fmt (BoolValue True) = "true"
  fmt (BoolValue False) = "false"
  fmt (FloatValue v) = fmtPretty v

instance Format UncheckedDimIndex where
  fmt (DimFix e) = fmt e
  fmt (DimSlice i j (Just s)) =
    maybe nil fmt i
      <> ":"
      <> maybe nil fmt j
      <> ":"
      <> fmt s
  fmt (DimSlice i (Just j) s) =
    maybe nil fmt i
      <> ":"
      <> fmt j
      <> maybe nil ((":" <>) . fmt) s
  fmt (DimSlice i Nothing Nothing) =
    maybe nil fmt i <> ":"

operatorName :: Name -> Bool
operatorName = (`elem` opchars) . T.head . nameToText
  where
    opchars :: String
    opchars = "+-*/%=!><|&^."

instance Format UncheckedExp where
  fmt (Var name _ loc) = addComments loc $ fmtQualName name
  fmt (Hole _ loc) = addComments loc "???"
  fmt (Parens e loc) =
    addComments loc $ "(" <> stdNest (fmt e) <:/> ")"
  fmt (QualParens (v, _qLoc) e loc) =
    addComments loc $
      fmtQualName v <> "." <> "(" <> align (fmt e) <:/> ")"
  fmt (Ascript e t loc) = addComments loc $ fmt e <> ":" <+> fmt t
  fmt (Coerce e t _ loc) = addComments loc $ fmt e <+> ":>" <+> fmt t
  fmt (Literal _v loc) = addComments loc $ fmtCopyLoc loc
  fmt (IntLit _v _ loc) = addComments loc $ fmtCopyLoc loc
  fmt (FloatLit _v _ loc) = addComments loc $ fmtCopyLoc loc
  fmt (TupLit es loc) =
    addComments loc $ parens $ sepLineComments locOf fmt "," es
  fmt (RecordLit fs loc) =
    addComments loc $ braces $ sepLineComments locOf fmt "," fs
  fmt (ArrayVal vs _ loc) =
    addComments loc $ brackets $ sepLine "," $ map fmt vs
  fmt (ArrayLit es _ loc) =
    addComments loc $ brackets $ sepLineComments locOf fmt "," es
  fmt (StringLit _s loc) = addComments loc $ fmtCopyLoc loc
  fmt (Project k e _ loc) = addComments loc $ fmt e <> "." <> fmtPretty k
  fmt (Negate e loc) = addComments loc $ "-" <> fmt e
  fmt (Not e loc) = addComments loc $ "!" <> fmt e
  fmt (Update src idxs ve loc) =
    addComments loc $
      fmt src <+> "with" <+> idxs' <+> stdNest ("=" </> fmt ve)
    where
      idxs' = brackets $ sep ("," <> space) $ map fmt idxs
  fmt (RecordUpdate src fs ve _ loc) =
    addComments loc $
      fmt src <+> "with" <+> fs' <+> stdNest ("=" </> fmt ve)
    where
      fs' = sep "." $ fmtName <$> fs
  fmt (Assert e1 e2 _ loc) =
    addComments loc $ "assert" <+> fmt e1 <+> fmt e2
  fmt (Lambda params body rettype _ loc) =
    addComments loc $
      "\\" <> sep space (map fmt params) <> ascript <+> stdNest ("->" </> fmt body)
    where
      ascript = maybe nil ((": " <>) . fmt) rettype
  fmt (OpSection binop _ loc) =
    addComments loc $
      if operatorName (qualLeaf binop)
        then fmtQualName binop
        else parens $ "`" <> fmtQualName binop <> "`"
  fmt (OpSectionLeft binop _ x _ _ loc) =
    addComments loc $ parens $ fmt x <+> fmtBinOp binop
  fmt (OpSectionRight binop _ x _ _ loc) =
    addComments loc $ parens $ fmtBinOp binop <+> fmt x
  fmt (ProjectSection fields _ loc) =
    addComments loc $ parens $ "." <> sep "." (fmtName <$> fields)
  fmt (IndexSection idxs _ loc) =
    addComments loc $ parens ("." <> idxs')
    where
      idxs' = brackets $ sep ("," <> space) $ map fmt idxs
  fmt (Constr n cs _ loc) =
    addComments loc $ "#" <> fmtName n <+> align (sep line $ map fmt cs)
  fmt (Attr attr e loc) = addComments loc $ align $ fmt attr </> fmt e
  fmt (AppExp e _) = fmt e

fmtQualName :: QualName Name -> Fmt
fmtQualName (QualName names name)
  | operatorName name = parens $ pre <> fmtName name
  | otherwise = pre <> fmtName name
  where
    pre =
      if null names
        then nil
        else sep "." (map fmtName names) <> "."

instance Format UncheckedCase where
  fmt (CasePat p e loc) =
    addComments loc $ "case" <+> fmt p <+> "->" </> stdIndent (fmt e)

instance Format (AppExpBase NoInfo Name) where
  fmt (BinOp (bop, _) _ (x, _) (y, _) loc) =
    addComments loc $ fmt x </> fmtBinOp bop <+> fmt y
  fmt (Match e cs loc) =
    addComments loc $ "match" <+> fmt e </> sep line (map fmt $ toList cs)
  -- need some way to omit the inital value expression, when this it's trivial
  fmt (Loop sizeparams pat (LoopInitImplicit NoInfo) form loopbody loc) =
    addComments loc $
      lineIndent pat ("loop" `op` sizeparams') (fmt pat)
        <+> fmt form
        <+> "do"
        </> stdIndent (fmt loopbody)
    where
      op = if null sizeparams then (<>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Loop sizeparams pat (LoopInitExplicit initexp) form loopbody loc) =
    addComments loc $
      lineIndent
        initexp
        ( lineIndent pat ("loop" `op` sizeparams') (fmt pat)
            <+> "="
        )
        (fmt initexp)
        <+> fmt form
        <+> "do"
        </> stdIndent (fmt loopbody)
    where
      op = if null sizeparams then (<>) else (<+>)
      sizeparams' = sep nil $ brackets . fmtName . toName <$> sizeparams
  fmt (Index e idxs loc) =
    addComments loc $ (fmt e <>) $ brackets $ sepLine "," $ map fmt idxs
  fmt (LetPat sizes pat e body loc) =
    addComments loc $
      lineIndent e ("let" <+> sub <+> "=") (fmt e)
        </> letBody body
    where
      sizes' = sep nil $ map fmt sizes
      sub
        | null sizes = fmt pat
        | otherwise = sizes' <+> fmt pat
  fmt (LetFun fname (tparams, params, retdecl, _, e) body loc) =
    addComments loc $
      lineIndent
        e
        ( "let"
            <+> fmtName fname
            <> sub
            <> retdecl'
            <> "="
        )
        (fmt e)
        </> letBody body
    where
      tparams' = sep space $ map fmt tparams
      params' = sep space $ map fmt params
      retdecl' =
        case retdecl of
          Just a -> ":" <+> fmt a <> space
          Nothing -> space
      sub
        | null tparams && null params = nil
        | null tparams = space <> params'
        | null params = space <> tparams'
        | otherwise = space <> tparams' <+> params'
  fmt (LetWith dest src idxs ve body loc)
    | dest == src =
        addComments loc $
          lineIndent
            ve
            ( "let"
                <+> fmt dest
                <> idxs'
                  <+> "="
            )
            (fmt ve)
            </> letBody body
    | otherwise =
        addComments loc $
          lineIndent
            ve
            ( "let"
                <+> fmt dest
                <+> "="
                <+> fmt src
                <+> "with"
                <+> idxs'
            )
            (fmt ve)
            </> letBody body
    where
      idxs' = brackets $ sep ", " $ map fmt idxs
  fmt (Range start maybe_step end loc) =
    addComments loc $ fmt start <> step <> end'
    where
      end' =
        case end of
          DownToExclusive e -> "..>" <> fmt e
          ToInclusive e -> "..." <> fmt e
          UpToExclusive e -> "..<" <> fmt e
      step = maybe nil ((".." <>) . fmt) maybe_step
  fmt (If c t f loc) =
    addComments loc $
      "if"
        <+> fmt c
        <+> "then"
        </> stdIndent (fmt t)
        </> "else"
        </> stdIndent (fmt f)
  fmt (Apply f args loc) =
    addComments loc $ fmt f <+> align fmt_args
    where
      fmt_args = sepArgs fmt $ map snd $ toList args

letBody :: UncheckedExp -> Fmt
letBody body@(AppExp LetPat {} _) = fmt body
letBody body@(AppExp LetFun {} _) = fmt body
letBody body@(AppExp LetWith {} _) = fmt body
letBody body = addComments body $ "in" <+> align (fmt body)

instance Format (SizeBinder Name) where
  fmt (SizeBinder v loc) = addComments loc $ brackets $ fmtName v

instance Format (IdentBase NoInfo Name t) where
  fmt = fmtPretty . identName

instance Format (LoopFormBase NoInfo Name) where
  fmt (For i ubound) = "for" <+> fmt i <+> "<" <+> fmt ubound
  fmt (ForIn x e) = "for" <+> fmt x <+> "in" <+> fmt e
  fmt (While cond) = "while" <+> fmt cond

-- | This should always be simplified by location.
fmtBinOp :: QualName Name -> Fmt
fmtBinOp bop =
  case leading of
    Backtick -> "`" <> fmtQualName bop <> "`"
    _any -> fmtPretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

instance Format UncheckedValBind where
  fmt (ValBind entry name retdecl _rettype tparams args body docs attrs loc) =
    addComments loc $
      fmt docs
        <> (if null attrs then nil else attrs' <> space)
        <> fun
          <+> fmtNameParen name
        <> sub
        <> retdecl'
        <> "="
          </> stdIndent (fmt body)
    where
      attrs' = sep space $ map fmt attrs
      tparams' = localLayoutList tparams $ align $ sep line $ map fmt tparams
      args' = localLayoutList args $ align $ sep line $ map fmt args
      retdecl' =
        case retdecl of
          Just a -> ":" <+> fmt a <> space
          Nothing -> space
      sub
        | null tparams && null args = nil
        | null tparams = space <> args'
        | null args = space <> tparams'
        | otherwise = space <> tparams' <+> args'
      fun =
        case entry of
          Just _ -> "entry"
          _any -> "def"

instance Format (SizeExp UncheckedExp) where
  fmt (SizeExp d loc) = addComments loc $ brackets $ fmt d
  fmt (SizeExpAny loc) = addComments loc $ brackets nil

instance Format UncheckedSpec where
  fmt (TypeAbbrSpec tpsig) = fmt tpsig
  fmt (TypeSpec l name ps doc loc) =
    addComments loc $
      fmt doc <> "type" <+> fmt l <> sub
    where
      sub
        | null ps = fmtName name
        | otherwise = fmtName name </> align (sep line $ map fmt ps)
  fmt (ValSpec name ps te _ doc loc) =
    addComments loc $
      fmt doc
        <> "val"
          <+> sub
        <> ":"
          <+> fmt te
    where
      sub
        | null ps = fmtName name
        | otherwise = fmtName name </> align (sep line $ map fmt ps)
  fmt (ModSpec name mte doc loc) =
    addComments loc $ fmt doc <> "module" <+> fmtName name <> ":" <+> fmt mte
  fmt (IncludeSpec mte loc) = addComments loc $ "include" <+> fmt mte

instance Format UncheckedModTypeExp where
  fmt (ModTypeVar v _ loc) = addComments loc $ fmtPretty v
  fmt (ModTypeParens mte loc) =
    addComments loc $ "(" <> align (fmt mte) <:/> ")"
  fmt (ModTypeSpecs sbs loc) =
    addComments loc $ "{" <:/> stdIndent (sep line $ map fmt sbs) <:/> "}"
  fmt (ModTypeWith mte (TypeRef v ps td _) loc) =
    addComments loc $
      fmt mte
        <+> "with"
        <+> fmtPretty v
        `ps_op` sep space (map fmt ps)
        <+> "="
        <+> fmt td
    where
      ps_op = if null ps then (<>) else (<+>)
  fmt (ModTypeArrow (Just v) te0 te1 loc) =
    addComments loc $
      parens (fmtName v <> ":" <+> fmt te0) <+> align ("->" </> fmt te1)
  fmt (ModTypeArrow Nothing te0 te1 loc) =
    addComments loc $ fmt te0 <+> "->" <+> fmt te1

instance Format UncheckedModTypeBind where
  fmt (ModTypeBind pName pSig doc loc) =
    addComments loc $
      fmt doc <> "module type" <+> fmtName pName <+> "=" <+> fmt pSig

instance Format (ModParamBase NoInfo Name) where
  fmt (ModParam pName pSig _f loc) =
    addComments loc $ parens $ fmtName pName <> ":" <+> fmt pSig

instance Format UncheckedModBind where
  fmt (ModBind name ps sig te doc loc) =
    addComments loc $
      fmt doc
        <> "module"
          <+> fmtName name
        <> ps'
        <> sig'
        <> "="
        <> te'
    where
      te' = fmtByLayout te (line <> stdIndent (fmt te)) (space <> fmt te)
      sig' = fmtSig sig
      fmtSig Nothing = space
      fmtSig (Just (s', _f)) = ":" <+> fmt s' <> space
      ps' =
        case ps of
          [] -> nil
          _any -> space <> localLayoutList ps (align $ sep line $ map fmt ps)

-- All of these should probably be "extra" indented
instance Format UncheckedModExp where
  fmt (ModVar v loc) = addComments loc $ fmtQualName v
  fmt (ModParens f loc) =
    addComments loc $ "(" <:/> stdIndent (fmt f) <:/> ")"
  fmt (ModImport path _f loc) =
    addComments loc $ "import \"" <> fmtPretty path <> "\""
  fmt (ModDecs decs loc) =
    addComments loc $
      "{" <:/> stdIndent (sepDecs fmt decs) <:/> "}"
  fmt (ModApply f a _f0 _f1 loc) = addComments loc $ fmt f <+> fmt a
  fmt (ModAscript me se _f loc) = addComments loc $ align (fmt me <> ":" </> fmt se)
  fmt (ModLambda param maybe_sig body loc) =
    addComments loc $
      "\\" <> fmt param <> sig <+> "->" </> stdIndent (fmt body)
    where
      sig =
        case maybe_sig of
          Nothing -> nil
          Just (sig', _) -> ":" <+> parens (fmt sig')

instance Format UncheckedDec where
  fmt (ValDec t) = fmt t
  fmt (TypeDec tb) = fmt tb
  fmt (ModTypeDec tb) = fmt tb
  fmt (ModDec tb) = fmt tb
  fmt (OpenDec tb loc) = addComments loc $ "open" <+> fmt tb
  fmt (LocalDec tb loc) = addComments loc $ "local" <+> fmt tb
  fmt (ImportDec path _tb loc) =
    addComments loc $ "import \"" <> fmtPretty path <> "\""

instance Format UncheckedProg where
  fmt (Prog dc decs) =
    fmt dc <> sepDecs fmt decs </> popComments

-- | Given a filename and a futhark program, formats the program.
fmtText :: String -> T.Text -> Either SyntaxError T.Text
fmtText fName fContent = do
  (prog, cs) <- parseFutharkWithComments fName fContent
  let m = fmt prog
  pure $ pretty $ runFormat m cs fContent
